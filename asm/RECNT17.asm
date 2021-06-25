*          DATA SET RECNT17    AT LEVEL 237 AS OF 06/12/12                      
*PHASE T80217A                                                                  
*INCLUDE OUTDAY                                                                 
*INCLUDE REGENPBY                                                               
*INCLUDE REGENBUF                                                               
*INCLUDE REGENTL2                                                               
*INCLUDE UNBOOK                                                                 
*INCLUDE XSORT                                                                  
         TITLE 'RECNT17 - T80217 - REPPAK REMOTE CONTRACT PRINT'                
*                                                                               
*********************************************************************           
*                                                                   *           
*        RECNT17 --- ON-LINE CONTRACT PRINT MODULE                  *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* REFER TO RECNTHIST FOR PAST HISTORY                               *           
*                                                                   *           
*  NOV11  SMY CONDITIONALLY PRINT BUYLINE DEMO VALUES               *           
* 04MAY11 SKU SUPPORT FORCED OCM NON-DISCRIMINATION CLAUSE          *           
* 22APR09 SKU SUPPORT FOR NEW INVENTORY KEY                         *           
* 20MAY05 HQ  SUPPRESS BUY AUTO-COMMENTING                          *           
* 16APR01 SKU FIX PRINTING BLANK PAGE                               *           
* 09FEB01 BU  FIX TRADE BRANCH                                      *           
* 08JAN01 RHV SPORTS BUYS                                           *           
* 21JUL00 BU  TRADE CONTRACT PROCESSING                             *           
* 05JAN99 RHV HANDLE NON-DELETED CANCELLED BUYS                     *           
* 28APR98 RHV COVERSHEET                                            *           
* 18DEC97 JRD CARE OF AGENCIES                                      *           
* 23JUL97 SKU 4K CONTRACT SUPPORT                                   *           
* 07JUL97 RHV AGY COMMENTS ON CONFIRMATION                          *           
* 28MAY97 RHV CFC COMMENTS ON CONFIRMATION                          *           
* 14MAY97 SKU FIX BUG OF SENDID GOING TO WRONG USER                 *           
* 24MAR97 SKU FIX PRTQUE BUG                                        *           
* 02DEC96 RHV REVISED CONTROL OF CONFIRMATION GENERATION            *           
*             FAX ORIGINAL STYLE CONFIRMATION                       *           
*             SEND LOGO FILENAME TO FAXGATE                         *           
* 03OCT96 SKU SUPPORT LOW POWER STATION                             *           
* 28JUN96 SKU FIX COMBO TOTAL PRINT                                 *           
* 03JUN96 WSB DISP NON-DARE HIATUS INFO IF CON NOT LINKED TO DARE   *           
* 07MAY96 RHV CONTYPE RECORD CONTROLLED CONTRACT FORMATTING         *           
* 27MAR96 RHV REVISED DISPLAY OF SAR BOOK NAMES&RENAMES             *           
* 04MAR96 RHV SUPPORT PETRY 34 BYTE AGY ADDR FIELDS                 *           
* 21FEB96 RHV SUPPRESS PRINTING OF EI FLD LABELS WHEN EMPTY         *           
* 06JAN96 SKU PROFILE 24 TO PRINT PTP OVER SALESPERSON FOR TYPE D   *           
* 13DEC95 SKU 2K CONTRACT SUPPORT                                   *           
*                                                                   *           
*********************************************************************           
* MEMORY MAP:                                                       *           
*                                                                   *           
* DBLOCKD USED FOR PRINTING DEMOS IN HEADLINE:                      *           
*           SPOOLAR+0000 ===> SPOOLAR+0255                          *           
* ENTD (FOR SORTED FLIGHTS)                                         *           
*           SPOOLAR+0256 ===> SPOOLAR+0933                          *           
* REGENPBY OUTPUT                                                   *           
*           SPOOLAR+3200 ===> SPOOLAR+5468                          *           
*                                                                   *           
*********************************************************************           
*                                                                               
T80217   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80217,R9,RR=R5                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,R7                                                       
*                                                                               
         CLI   TWACFFLG,0    DO WE HAVE A REQUEST TO PRINT ANYTHING             
         BNE   *+6           YES                                                
         DC    H'0'          NO - SHOULDN'T BE HERE                             
*                                                                               
         XC    WORK5,WORK5         CLEAR COMBO TOTAL AREAS                      
         MVC   WORK5(2),=H'2'                                                   
         XC    WORK6,WORK6                                                      
         MVC   WORK6(2),=H'2'                                                   
*                                                                               
         MVC   DMCB+4(4),=X'D9000A11'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         MVC   UNTIME,DMCB         FIND CORE ADDRESS OF UNTIME                  
*                                                                               
         MVC   DMCB+4(4),=X'D9000AE0'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         MVC   DEMCON,DMCB         FIND CORE ADDRESS OF UNTIME                  
*                                                                               
         L     R1,=V(OUTDAY)       RELOCATE EXTERNAL MODULES                    
         AR    R1,R5                                                            
         ST    R1,OUTDAY                                                        
         L     R1,=V(REGENPBY)                                                  
         AR    R1,R5                                                            
         ST    R1,REGENPBY                                                      
         L     R1,=V(REGENTL2)                                                  
         AR    R1,R5                                                            
         ST    R1,REGENTL2                                                      
         SPACE 1                                                                
* POINT R8 PAST THE BUY RECORD                                                  
* AREA AND OUTPUT BLOCK AND COVER                                               
         L     R8,ASPULAR                                                       
         A     R8,=AL4(256)                                                     
         USING ENTD,R8             WITH ASECT OF FLIGHTED BUY ENTRIES           
         MVI   BYTE,0              ZERO FLIGHTED BUY COUNTER                    
         MVI   BYTE2,0             ZERO CURRENT FLIGHT                          
         MVI   BYTE4,0             ZERO FLIGHT CONTROL INDICATOR                
         MVI   KPRTOPTS,0          ZERO K PRINT OPTIONS                         
         XC    SAVAM,SAVAM                                                      
         SPACE                                                                  
PR10     LA    R2,CONCACTH                                                      
         LA    R3,ACTERR                                                        
         TM    RCONREC+29,X'01'    COMPRESSED                                   
         BO    ERROR                                                            
         SPACE 1                                                                
         LA    R3,196              CONTRACT NOT CONFIRMED                       
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    PR20                                                             
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    6(R6),X'40'                                                      
         BZ    ERROR                                                            
         SPACE 1                                                                
PR20     DS    0H                  ALWAYS READ STA REC FOR AFFL                 
         XC    KEY,KEY             GET AM AND FM STATION                        
         MVI   KEY,2               FROM STATION RECORD                          
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONKSTA                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    PR30                                                             
         DC    H'0',C'STA REC MISSING'                                          
PR30     GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TEMP(3),RSTAAFFL                                                 
*                                                                               
         CLI   RCONKSTA+4,C'C'     IF COMBINED STATIONS                         
         BNE   PR60                                                             
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'0A'                                                     
         BAS   RE,GETEL                                                         
         BE    PR40                                                             
         LA    R3,264              STA REC MISSING COMBO ELEM                   
         B     ERROR                                                            
         USING RSTACSEL,R6                                                      
*                                                                               
PR40     DS    0H                  READ FIRST TWO STATIONS                      
         CLI   RSTACS+4,C'A'                                                    
         BE    PR50                                                             
         MVC   SAVFM,RSTACS                                                     
         BAS   RE,NEXTEL                                                        
         BNE   PR60                                                             
         MVC   SAVAM,RSTACS                                                     
         B     PR60                                                             
*                                                                               
PR50     DS    0H                                                               
         MVC   SAVAM,RSTACS                                                     
         BAS   RE,NEXTEL                                                        
         BNE   PR60                                                             
         MVC   SAVFM,RSTACS                                                     
         DROP  R6                                                               
*                                                                               
PR60     DS    0H                                                               
         LA    R6,IOAREA                                                        
         USING RSTAXXEL,R6                                                      
         MVI   ELCODE,8            GET OPTION                                   
         BAS   RE,GETEL                                                         
         BNE   PR70                                                             
         CLI   RSTAOPT7,C'Y'                                                    
         BNE   PR70                                                             
         OI    SOPTIONS,STAOPT7    SET TO PRINT AGY,ADV AND TRAF#               
         DROP  R6                                                               
*                                                                               
PR70     DS    0H                                                               
         LA    R6,IOAREA                                                        
         USING RSTAELEM,R6                                                      
         MVI   ELCODE,1            GET OPTION                                   
         BAS   RE,GETEL                                                         
         BNE   PR100                                                            
         CLI   RSTAP5,C'Y'                                                      
         BNE   PR100                                                            
         OI    SOPTIONS,STAOPT5    SET TO PRINT BUYLINE DEMO VALUES             
         DROP  R6                                                               
*                                                                               
PR100    DS    0H                                                               
         OC    TWAAFAX,TWAAFAX     DO WE HAVE AN AGENCY FAX NUMBER?             
         BNZ   *+8                 YES - GREAT                                  
         NI    TWACFFLG,X'FF'-X'10' NO - DON'T GEN AGY FAX COPY                 
*                                                                               
* READ POINT PERSON RECORD FOR NAME                                             
*                                                                               
         XC    WPTPEXP,WPTPEXP     INIT POINT PERSON EXPANSION                  
         XC    WPTPPH#,WPTPPH#     INIT POINT PERSON EXPANSION                  
         OC    TWAPDPTP,TWAPDPTP   POINT PERSON CODE PRESENT?                   
         BZ    PR132                                                            
         GOTO1 =A(PPTP),DMCB,(RC),RR=Y                                          
PR132    DS    0H                                                               
*                                                                               
* GOTO FORMAT ROUTINE HERE                                                      
         GOTO1 =A(FMT),DMCB,(RC),RR=YES                                         
*                                                                               
         TM    TWACFFLG,X'80'      PRINT REP CONFIRMATION?                      
         BZ    PR200               NO - SKIP                                    
*                                                                               
*********************************************************************           
*                                                                               
* PRINT CONTRACT AND/OR AGENCY COPY WITH CLASS C                                
*                                                                               
*********************************************************************           
         MVI   MAXLNES,74          MAX LINES                                    
         XC    WORK3,WORK3                                                      
         MVC   WORK3(2),=H'2'                                                   
         XC    WORK4,WORK4                                                      
         MVC   WORK4(2),=H'2'                                                   
         MVI   PG,1                PG COUNT                                     
*                                                                               
* SET 1ST TIME FOR PRTQUE                                                       
*                                                                               
         USING PQPLD,RE            PRINT QUEUE PRINT LNE                        
         LA    RE,MYP-1                                                         
         L     R2,AFACILS                                                       
         L     R2,12(R2)           A(TIA)                                       
         MVI   PLCC,0              OPEN                                         
         XC    MYP,MYP                                                          
         MVC   QLDESC(3),=C'CON'   ID                                           
         ZAP   DUB,=P'0'                                                        
         MVO   DUB+3(5),RCONKCON   K NUMBER                                     
         EDIT  (P5,DUB+3),(8,QLDESC+3),ALIGN=LEFT                               
         MVC   QLSUBID,RCONSAL     SET SUB-KEY                                  
         MVI   QLCLASS,C'C'        CLASS 'C'                                    
         MVI   QLEXTRA,X'FF'       INDICATE NEW STYLE LIST                      
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'2'       KEEP PRINTED CONTRACT 2 HOURS                
         MVI   QLSYS,C'R'          FORMS/COPIES COLUMN                          
         MVC   QLPRG,=C'CO'                                                     
*                                                                               
         CLC   =C'PRI',CONACT                                                   
         BE    PR140                                                            
         OC    SENDID,SENDID       IF SENDID, IT'S ACE/GRAPHNET                 
         BZ    PR140                                                            
         MVC   QLSRCID,SENDID                                                   
         DROP  RE                                                               
*                                                                               
* MAKE SURE TO CLEAR SENDID AT THE END OF THIS ROUTINE                          
*                                                                               
PR140    LA    RE,=C'PRTQUE '                                                   
         ST    RE,DMCB+4                                                        
         GOTO1 DATAMGR,DMCB,=C'DMPRINT',,,MYP-1,(TERMNAL,(R2))                  
         TM    DMCB+8,X'FF'                                                     
         BZ    PR150                                                            
         DC    H'0',C'$PQFULL'                                                  
*                                                                               
PR150    MVI   BYTE3,0                                                          
         CLI   RCONTYPE,C'N'       TYPE N FOR                                   
         BNE   PR170                                                            
         CLC   TWARMAST,=C'IR'     CHECK IF INTEREP                             
         BNE   PR170                                                            
*                                                                               
         MVI   BYTE3,C'S'          STATION COPY FIRST                           
*                                                                               
PR170    BAS   RE,PRTCON           PRINT CONTRACT                               
*                                                                               
* GO TO PRTQUE WITH LAST TIME                                                   
*                                                                               
PR200    DS    0H                                                               
         MVI   MYP-1,X'FF'           LAST TIME                                  
         L     R2,AFACILS                                                       
         L     R2,12(R2)           A(TIA)                                       
         LA    RE,=C'PRTQUE '                                                   
         ST    RE,DMCB+4                                                        
         GOTO1 DATAMGR,DMCB,=C'DMPRINT',,,MYP-1,(TERMNAL,(R2))                  
*                                                                               
         TM    TWACFFLG,X'10'      AGY FAX COPY?                                
         BO    PR205               YES                                          
         XC    SENDID,SENDID       CLEAR SENDID                                 
         B     PR260                                                            
         EJECT                                                                  
*********************************************************************           
*                                                                               
* PRINT AGENCY COPY OF CONTRACT AND SET TO EASYLINK CLASS G                     
*                                                                               
*********************************************************************           
PR205    DS    0H                                                               
         MVI   MAXLNES,74          MAX LINES                                    
         TM    PROFILES+CNTFAXHB,CNTFAXHA  PROF TO FAX ORIGINAL STYLE           
         BZ    *+8                         YES - MAXLNES=60                     
         MVI   MAXLNES,60          MAX LINES                                    
*                                                                               
         XC    WORK3,WORK3                                                      
         MVC   WORK3(2),=H'2'                                                   
         XC    WORK4,WORK4                                                      
         MVC   WORK4(2),=H'2'                                                   
         MVI   PG,1                PG COUNT                                     
*                                                                               
* SET 1ST TIME FOR PRTQUE                                                       
*                                                                               
         USING PQPLD,RE            PRINT QUEUE PRINT LNE                        
         LA    RE,MYP-1                                                         
         L     R2,AFACILS                                                       
         L     R2,12(R2)           A(TIA)                                       
         MVI   PLCC,0              OPEN                                         
         XC    MYP,MYP                                                          
         MVC   QLDESC(3),=C'CON'   ID                                           
         ZAP   DUB,=P'0'                                                        
         MVO   DUB+3(5),RCONKCON   K NUMBER                                     
         EDIT  (P5,DUB+3),(8,QLDESC+3),ALIGN=LEFT                               
         MVC   QLSUBID,RCONSAL     SET SUB-KEY                                  
         MVI   QLCLASS,C'G'        CLASS 'G'                                    
         MVI   QLEXTRA,X'FF'       INDICATE NEW STYLE LIST                      
         MVC   QLSUBID,=C'CON'                                                  
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'26'      PRTD/SENT RETENTION OF 26                    
         MVI   QLSYS,C'R'          FORMS/COPIES COLUMN                          
         MVC   QLPRG,=C'CO'                                                     
*                                                                               
         CLC   =C'PRI',CONACT                                                   
         BE    PR210                                                            
         OC    SENDID,SENDID       IF SENDID, IT'S ACE/GRAPHNET                 
         BZ    PR210                                                            
         MVC   QLSRCID,SENDID                                                   
         XC    SENDID,SENDID       CLEAR SENDID                                 
         DROP  RE                                                               
*                                                                               
PR210    LA    RE,=C'PRTQUE '                                                   
         ST    RE,DMCB+4                                                        
         GOTO1 DATAMGR,DMCB,=C'DMPRINT',,,MYP-1,(TERMNAL,(R2))                  
         TM    DMCB+8,X'FF'                                                     
         BZ    PR220                                                            
         DC    H'0',C'$PQFULL'                                                  
*                                                                               
* PRINT EDICT HEADER INFO                                                       
*                                                                               
PR220    DS    0H                                                               
         MVI   MYP-1,X'89'         SKIP TO TOP OF PAGE                          
         BAS   RE,PRINT                                                         
*                                                                               
         MVI   MYP-1,X'09'                                                      
         MVC   MYP+4(5),=C'*HDR*'                                               
*                                                                               
         MVC   MYP+9(5),=C'FAX '                                                
         MVC   MYP+13(3),TWAAFAX                                                
         MVI   MYP+16,C'-'                                                      
         MVC   MYP+17(3),TWAAFAX+3                                              
         MVI   MYP+20,C'-'                                                      
         MVC   MYP+21(4),TWAAFAX+6                                              
         MVI   MYP+35,C'P'         REPL X'89' IN REP W/ EASYLINK /PAGE          
*                                                                               
         MVC   MYP+54(2),RCONTEM   TEAM, BILLING INFO                           
         BAS   RE,PRINT            SEND SPECIAL PRINT LINE                      
*                                                                               
* PRINT A ++DDS CARD WITH 58 BYTES OF DATA FOR EDICT XACTION PROG               
*                                                                               
         LA    R4,MYP                                                           
         USING EDICTD,R4                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST,=C'RE'      FOR SYSTEM REP                               
         MVC   EDIPROG,=C'CON'     FOR TYPE CON                                 
         MVC   EDIIDEN,=C'TRN'     XACTION FILE DATA                            
*                                                                               
         MVC   EDIRCNRP,RCONKREP   REP CODE                                     
         MVC   EDIRCNOF,RCONKOFF   OFF CODE                                     
         MVC   EDIRCNSP,RCONSAL    SALESPERSON CODE                             
         MVC   EDIRCNAG,RCONKAGY   AGENCY CODE                                  
         MVC   EDIRCNAO,RCONKAOF   CITY CODE                                    
         MVC   EDIRCNAD,RCONKADV   ADVERTISER CODE                              
         MVC   EDIRCNCT,RCONKCON   CONTRACT TYPE                                
* FLIGHT START AND END DATES                                                    
         LA    R3,RCONDATE         DEFAULT TO DISPLAY NORMAL DATES              
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRT225                                                           
         USING RCONRFEL,R6                                                      
         OC    RCONRFLT,RCONRFLT   HAVE REVISED DATE?                           
         BZ    PRT225                                                           
         LA    R3,RCONRFLT         YES - USE IT                                 
         DROP  R6                                                               
PRT225   GOTO1 DATCON,DMCB,(3,0(R3)),(5,EDIRCNFS)                               
         GOTO1 DATCON,DMCB,(3,3(R3)),(5,EDIRCNFE)                               
* LATEST VERSION NUMBER                                                         
         LA    R6,RCONREC                                                       
         USING RCONSEND,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   PR240                                                            
         CLC   RCONSRV,RCONSSV     SHOW LATEST VERSION                          
         BH    PR230                                                            
         EDIT  (1,RCONSSV),(3,EDIRCNVN),ALIGN=LEFT                              
         B     PR240                                                            
PR230    EDIT  (1,RCONSRV),(3,EDIRCNVN),ALIGN=LEFT                              
         DROP  R6                                                               
*                                                                               
PR240    MVC   EDIRCNST,RCONKSTA   STATION CALLS                                
* CONTRACT NUMBER                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         EDIT  (P5,WORK),(8,EDIRCNHN),ALIGN=LEFT                                
         DROP  R4                                                               
         BAS   RE,PRINT            SEND SPECIAL PRINT LINE                      
*                                                                               
*  PRINT A ++DDS FXG RECORD                                                     
*                                                                               
         OC    TWALOGO,TWALOGO     ONLY PRINT CARD IF WE HAVE A LOGO            
         BZ    PR245                                                            
         LA    R4,MYP                                                           
         MVC   0(5,R4),=C'++DDS'                                                
         MVC   11(3,R4),=C'FXG'                                                 
         MVC   15(12,R4),TWALOGO                                                
         MVC   28(12,R4),=C'CONTRACT.TMP'                                       
         BAS   RE,PRINT            SEND SPECIAL LINE                            
*                                                                               
PR245    DS    0H                                                               
         MVI   BYTE3,0             EASYLINK RATE COPY                           
         CLI   RCONTYPE,C'N'       IF CONTRACT TYPE N AND                       
         BNE   PR250                                                            
         CLC   TWARMAST,=C'IR'     IF INTEREP                                   
         BNE   PR250                                                            
         MVI   BYTE3,C'A'          SEND EASYLINK AGENCY NO RATE COPY            
*                                                                               
PR250    DS    0H                                                               
         MVI   PG,1                                                             
         XC    BYTE(2),BYTE        RE-INITIALIZE COUNTERS AND                   
         MVI   BYTE4,0             INDICATORS FOR LAST CALL                     
         TM    PROFILES+CNTFAXHB,CNTFAXHA  PROF TO FAX ORIGINAL STYLE           
         BO    *+8                         YES - TREAT AS NOT FAX               
         OI    KPRTOPTS,KPRTOPFX   PRINT FAX COPY                               
         BAS   RE,PRTCON                                                        
*                                                                               
         MVI   MYP-1,X'FF'           LAST TIME                                  
         L     R2,AFACILS                                                       
         L     R2,12(R2)           A(TIA)                                       
         LA    RE,=C'PRTQUE '                                                   
         ST    RE,DMCB+4                                                        
         GOTO1 DATAMGR,DMCB,=C'DMPRINT',,,MYP-1,(TERMNAL,(R2))                  
         EJECT                                                                  
PR260    LA    R2,CONCACTH         CURSOR                                       
         CLI   TWAACCS,C'$'        IF STATION, IT'S 'ACE'                       
         BE    EXXMOD              DON'T SHOW THIS CONMSG                       
         SPACE 1                                                                
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDISMSG,DMCB,52                                                  
         B     EXXMOD                                                           
         TITLE 'T80217-CONTRACT PRINT'                                          
***********************************************************************         
* CHECKS FOR SPECIAL COMBO CONTRACT PRINT                             *         
***********************************************************************         
PRTCON   NTR1                                                                   
         CLI   TWACOMBO,0          CHECK IF COMBO PRINT                         
         BNE   PRTCOMBO                                                         
         BAS   RE,CONPRINT         NO, GO PRINT CONTRACT                        
         B     PCOMBOX             AND LEAVE                                    
*                                                                               
PRTCOMBO DS    0H                  YES...                                       
         XC    TWATTLAR,TWATTLAR   INITIALIZE GRAND TOTAL STORAGE               
         MVC   SVCONKY2,RCONREC    SAVE FIRST COMBO K KEY                       
         MVC   SVCNUM2,TWACNUM                                                  
         MVI   PRTCMBPT,1          INDEX, SET TO FIRST COMPONENT K              
*                                                                               
         LA    R6,RCONREC          PRINT BUYLINES IN THE ORDER AS               
         USING RCONCBEL,R6         FOUND IN THE COMBO ELEMENT                   
         MVI   ELCODE,X'17'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    SVCOMBO2,SVCOMBO2   SAVE OFF COMPONENTS, CONTRACT IO             
         ZIC   R1,RCONCBLN         AREA GETS USED BY OTHER CONTRACTS            
         SH    R1,=H'3'            OVERHEAD                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVCOMBO2(0),RCONCBST                                             
         DROP  R6                                                               
*                                                                               
         LA    R2,SVCOMBO2+5       GET NEXT COMPONENT K#                        
*                                                                               
PCOMBO10 DS    0H                  LOAD NEXT COMPONENT K INTO RCONREC           
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),0(4,R2)  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+10(5) GET 9'S COMPLEMENT                          
         MVO   WORK(5),WORK+5(5)   CHANGE TO PWOS                               
*                                                                               
         XC    RCONREC(32),RCONREC                                              
         MVC   RCONPCON,WORK       NUMBER                                       
         MVC   RCONPREP,REPALPHA                                                
         MVI   RCONPTYP,X'8C'                                                   
         MVC   KEY,RCONREC                                                      
         GOTO1 VHIGH               GET CONTRACT KEY                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PCOMBO60            CAN'T FIND CONTRACT, SKIP IT                 
*                                                                               
         MVC   TWAKADDR,KEY+28                                                  
         MVC   TWACNUM,KEY+23                                                   
         PACK  TWACNUM(1),KEY+26(1)      REVERSE THE COMPLIMENT                 
         PACK  TWACNUM+1(1),KEY+25(1)                                           
         PACK  TWACNUM+2(1),KEY+24(1)                                           
         PACK  TWACNUM+3(1),KEY+23(1)                                           
*                                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                                                               
         BAS   RE,CONPRINT         GO PRINT COMBO BUYLINES                      
*                                                                               
         TM    RCONMODR+1,X'20'    MON???                                       
         BZ    PCOMBO50                                                         
         GOTO1 =V(REGENBUF),DMCB,RCONREC,WORK3,RR=Y                             
*                                                                               
PCOMBO50 DS    0H                                                               
         CLI   PRTCMBPT,1          IF COMBO FIRST PASS, SKIP GRAND              
         BNE   PCOMBO55            TOTAL ROUTINE                                
*                                  IF COMBO FIRST PASS, STORE FIRST             
         MVC   TWATTLAR,WORK3      TOTAL TO GRAND TOTAL AREA                    
         B     PCOMBO60                                                         
*                                                                               
PCOMBO55 DS    0H                                                               
         GOTO1 =A(TOTLTOTL),DMCB,(RC),RR=Y                                      
*                                                                               
PCOMBO60 ZIC   RF,PRTCMBPT         BUMP COMPONENT K POINTER                     
         LA    RF,1(RF)                                                         
         STC   RF,PRTCMBPT                                                      
*                                                                               
         CLC   PRTCMBPT,TWACOMBO   WE HAVE THIS MANY COMBO K'S TO DO            
         BH    PCOMBO70                                                         
         LA    R2,9(R2)            GET NEXT COMPONENT K#                        
         B     PCOMBO10                                                         
*                                                                               
PCOMBO70 DS    0H                                                               
         GOTO1 =A(GRANDTOT),RR=Y   PRINT THE GRAND TOTAL                        
*                                  ALL DONE, RESTORE ORIGINAL COMBO K           
*                                  TO RCONREC AREA                              
         MVC   KEY(L'RCONKEY),SVCONKY2                                          
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGETREC,DMCB,RCONREC TO RCONREC AREA                             
         TM    RCONCNTL,X'80'                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TWAKADDR,KEY+28     RESTORE K INFO IN TWA, TOO                   
         MVC   TWACNUM,SVCNUM2                                                  
*                                                                               
PCOMBOX  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT HEADER AND BODY OF CONTRACT                                   *         
***********************************************************************         
CONPRINT NTR1                                                                   
         CLI   TWACOMBO,0          IF COMBO,  PRINT HEADER ONLY AT THE          
         BE    CONPRT10              FIRST PASS                                 
*                                                                               
         MVI   BYTE,0              ZERO FLIGHTED BUY COUNTER                    
         MVI   BYTE2,0             ZERO CURRENT FLIGHT                          
         MVI   BYTE4,0             ZERO FLIGHT CONTROL INDICATOR                
*                                                                               
         CLI   PRTCMBPT,1                                                       
         BNE   CONPRT25                                                         
*                                                                               
CONPRT10 DS    0H                                                               
         GOTO1 =A(HEADLINE),DMCB,(RC),RR=Y                                      
         GOTO1 =A(PRTCOV),RR=Y                                                  
         GOTO1 PRTCALL,DMCB,MYP+13 ELSE, PRINT STATIONS' CALL LETTERS           
         BAS   RE,PRTEICDS         PRINT EI CODES                               
*                                                                               
         GOTO1 =A(PRTCFC),DMCB,(RC),RR=Y   PRINT CFC COMMENT                    
*                                                                               
         GOTO1 =A(PRTKCMT),RR=Y    PRINT K COMMENTS                             
*                                                                               
CONPRT20 DS    0H                                                               
         CLI   TWACOMBO,0          IF COMBO,  PRINT BUYLINE INFO                
         BE    CONPRT30                                                         
*                                                                               
CONPRT25 DS    0H                                                               
         BAS   RE,PRINT                                                         
*                                                                               
         ZIC   R3,LNE              MAKE SURE WE CAN AT LEAST FIT                
         LA    R3,8(R3)            A BUY LINE                                   
         ZIC   R4,MAXLNES                                                       
         CR    R3,R4               ENOUGH ROOM?                                 
         BNH   CONPRT28            IF NOT, START NEW PAGE                       
         GOTO1 =A(HEADLINE),DMCB,(RC),RR=Y                                      
*                                                                               
CONPRT28 DS    0H                                                               
         MVC   MYP+10(4),RCONKSTA  PRINT COMPONENT STATIONS                     
         MVI   MYP+14,C'-'                                                      
         MVC   MYP+15(1),RCONKSTA+4                                             
         ZAP   DUB,=P'0'                                                        
         MVO   DUB+3(5),RCONKCON   K NUMBER                                     
         EDIT  (P5,DUB+3),(8,MYP+25),ALIGN=LEFT                                 
         MVI   MYP-1,SPACE2                                                     
         BAS   RE,PRINT                                                         
*                                                                               
CONPRT30 DS    0H                  PRINTS BUYLINES                              
***>     TM    TWACFFLG,X'10'      PRINT FAX HEADER IF FAX COPY                 
***>     BZ    CONPRT40                                                         
***>     TM    PROFILES+CNTFAXHB,CNTFAXHA  PROF TO FAX ORIGINAL STYLE           
***>     BO    CONPRT40                    YES - SKIP FAX HEADER                
         TM    KPRTOPTS,KPRTOPFX   ARE WE FAXING?                               
         BZ    CONPRT40            NO                                           
***      BAS   RE,FAXHEAD                                                       
         GOTO1 =A(FAXHEAD),RR=Y                                                 
                                                                                
CONPRT40 DS    0H                                                               
         XC    WORK3,WORK3                                                      
         MVC   WORK3(2),=H'2'                                                   
         XC    WORK4,WORK4                                                      
         MVC   WORK4(2),=H'2'                                                   
*                                                                               
         XC    TEMP,TEMP                                                        
* BUILD BUY KEY                                                                 
         XC    RBUYKEY,RBUYKEY                                                  
         MVI   RBUYKTYP,11                                                      
         MVC   RBUYKREP,REPALPHA                                                
         MVC   RBUYKCON,TWACNUM                                                 
         MVC   KEY,RBUYKEY                                                      
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VHIGH                                                            
         B     NEXTBUY1                                                         
         SPACE 1                                                                
NEXTBUY  OI    DMINBTS,X'08'                                                    
         GOTO1 VSEQ                                                             
NEXTBUY1 CLC   KEY(22),KEYSAVE                                                  
         BE    NEXTBUY2                                                         
         CLI   BYTE,0              ARE THERE ANY FLIGHTED BUYS                  
         BE    TOTAL               NO-PUT OUT CONTRACT TOTALS                   
         SPACE                                                                  
         BAS   RE,SORTFLT          YES-CALL XSORT                               
         SPACE                                                                  
FLTREAD  DS    0H                                                               
         XCEF  RBUYREC,1000                                                     
         MVC   KEY+28(4),ENTDA     FETCH DISK ADDRESS                           
         OI    DMINBTS,X'08'                                                    
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         CLC   BYTE2,RBUYFLT       HAS FLIGHT NUMBER CHANGED                    
         BE    FLTREAD2            NO                                           
         MVC   BYTE2,RBUYFLT       REPLACE CURRENT FLIGHT                       
         OI    BYTE4,FLTH          TURN ON FLIGHT HEADER INDICATOR              
         TM    BYTE4,FLTWT                                                      
         BZ    *+8                                                              
         BAS   RE,WKTOT            PUT OUT WEEKLY TOTALS FOR LAST FLT           
         SPACE                                                                  
FLTREAD2 BAS   RE,UPWKTOT          UPDATE WEEKLY BUCKETS                        
         B     NEXTBUY3                                                         
         SPACE                                                                  
NEXTBUY2 TM    KEY+27,X'C0'        VOID?                                        
         BO    NEXTBUY                                                          
         SPACE                                                                  
         OI    DMINBTS,X'08'                                                    
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                  CHECK IF COMBO A PLACE HOLDER                
         TM    RBUYCOMB,X'80'      (IE N/A RATE)                                
         BO    NEXTBUY             YES, SKIP IT                                 
*                                                                               
         CLI   RBUYFLT,0           DOES BUY BELONG TO A FLIGHT                  
         BE    NEXTBUY3            NO                                           
         SPACE                                                                  
         BAS   RE,BLDENTRY         YES-BUILD AN ENTRY FOR XSORT                 
         B     NEXTBUY             GO BACK AND READ NEXT BUY                    
         SPACE                                                                  
NEXTBUY3 MVC   WORK(4),OUTDAY                                                   
         MVC   WORK+4(4),UNTIME                                                 
         MVC   WORK+8(4),DATCON                                                 
         MVC   WORK+12(4),ADDAY    EXPLODE ALT WEEKS                            
         MVC   WORK+16(4),VREPFACS                                              
         MVC   WORK+20(4),DEMCON                                                
*                                                                               
         L     R4,ASPULAR                                                       
         A     R4,=AL4(3200)                                                    
         USING PD,R4                                                            
*SMY*    GOTO1 REGENPBY,DMCB,(X'40',RBUYREC),(28,(R4)),WORK,RCONREC,            
*SMY*          PROFILES                                                         
         MVI   SVKVER,X'40'        SUPPRESS AUTO-COMMENT                        
*                                                                               
         TM    SOPTIONS,STAOPT5    PRINT BUYLINE DEMO VALUES?                   
         BNO   NXTBY4              NO                                           
         OI    SVKVER,X'80'                                                     
*                                                                               
*SMY*    BRAS  RE,CKSTPROF         PRINT BUYLINE DEMO VALUES?                   
*SMY*    BE    NXTBY4                                                           
*SMY*    OI    SVKVER,X'02'        NO - WILL OVERRIDE THE X'80'                 
*                                                                               
NXTBY4   EQU   *                                                                
         MVC   SVDEMFLG,SVKVER     SAVE DEMO PRINT OPTION                       
         NI    SVDEMFLG,X'02'                                                   
         GOTO1 REGENPBY,DMCB,(SVKVER,RBUYREC),(32,(R4)),WORK,RCONREC,  >        
               PROFILES,ACOMFACS                                                
         SR    R2,R2                                                            
         IC    R2,DMCB+4           PRINT LINES                                  
         LR    R5,R2                                                            
         LTR   R2,R2                                                            
         BZ    RESET                                                            
*                                                                               
         TM    BYTE4,FLTH          PRINT FLIGHT HEADER BEFORE BUY               
         BZ    *+8                 LNE DATA WHEN THE INDICATOR IS               
         BAS   RE,FLTHEAD          ON.                                          
         SR    R3,R3                                                            
         IC    R3,LNE                                                           
         AR    R2,R3                                                            
         LA    R2,1(R2)                                                         
         IC    R3,MAXLNES                                                       
         CR    R2,R3                                                            
         BNH   NEXTBUY6            WILL FIT ON PG                               
         GOTO1 =A(HEADLINE),DMCB,(RC),RR=Y                                      
         MVC   KEY(27),RBUYKEY     RESTORE KEY SEQUENCE                         
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    NEXTBUY6                                                         
         DC    H'0'                                                             
*                                                                               
NEXTBUY6 EQU   *                                                                
         CLI   BYTE3,C'A'          AGENCY COPY (NO RATES)?                      
         BNE   *+14                                                             
         MVI   PRAT,C'*'                                                        
         MVC   PRAT+1(9),PRAT      ELIM. COST                                   
*                                                                               
PRTLINE  DS    0H                    BUY PRINT LNE                              
         CLC   =C'P=',PDAY+6                                                    
         BNE   PRTLINE5                                                         
         MVC   MYP+14(12),=C'PROGRAMMING='                                      
         MVC   MYP+26(64),PDAY+8                                                
         B     PRTLINE8                                                         
*                                                                               
PRTLINE5 DS    0H                    BUY PRINT LNE                              
         MVC   MYP+1(79),PRTLN                                                  
*                                                                               
PRTLINE8 BAS   RE,PRINT                                                         
         LA    R4,L'PRTLN(R4)                                                   
         BCT   R5,PRTLINE                                                       
         DROP  R4                                                               
*                                                                               
         BAS   RE,PRINT            SPACE LNE AFTER BUY                          
*                                                                               
         TM    RBUYCNTL,X'80'                                                   
         BO    RESET                                                            
*                                                                               
         CLI   RBUYCHGI,C'C'                                                    
         BE    RESET                                                            
*                                                                               
* ADD TO TOTAL BUCKETS                                                          
*                                                                               
         XC    WORK2,WORK2                                                      
*                                                                               
         MVC   WORK(4),VGTBROAD                                                 
         MVC   WORK+4(4),GETDAY                                                 
         MVC   WORK+8(4),ADDAY                                                  
         MVC   WORK+12(4),DATCON                                                
*                                                                               
* GET BUY BUCKETS WITH GRPS                                                     
*                                                                               
         GOTO1 REGENBUC,DMCB,RBUYREC,WORK2,(X'40',WORK)                         
*                                                                               
         CLC   WORK2(2),=H'2'      SKIP IF NO BUCKETS FOUND                     
         BE    RESET                                                            
*                                                                               
         SR    R4,R4                                                            
*                                                                               
* ADD BUY BUCKETS TO TOTAL CONTRACT BUCKETS                                     
*                                                                               
         LA    R3,WORK2+2          FIRST BUCKET                                 
*                                                                               
B50      LA    R5,WORK3+2          TOTAL                                        
*                                                                               
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BNO   B100                NO                                           
*                                                                               
         LA    R5,WORK4+2          YES - TRADE BUCKET                           
*                                                                               
B100     CLI   0(R5),0             LAST?                                        
         BE    B200                                                             
*                                                                               
         CLC   2(2,R3),2(R5)       COMPARE BUCKET DATES                         
         BL    B200                                                             
         BE    B300                                                             
*                                                                               
* GET NEXT TOTAL BUCKET                                                         
*                                                                               
         IC    R4,1(R5)            LEN                                          
         AR    R5,R4                                                            
         B     B100                                                             
*                                                                               
* ADD NEW TOTAL BUCKET                                                          
*                                                                               
B200     EQU   *                                                                
*                                                                               
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BO    B210                YES                                          
*                                                                               
         GOTO1 VRECUP,DMCB,(X'FF',WORK3),(R3),(R5)                              
*                                                                               
         B     B250                                                             
*                                                                               
B210     EQU   *                                                                
         GOTO1 VRECUP,DMCB,(X'FF',WORK4),(R3),(R5)                              
*                                                                               
* GET NEXT BUY BUCKET                                                           
*                                                                               
B250     IC    R4,1(R3)            LENGTH                                       
         AR    R3,R4                                                            
*                                                                               
         CLI   0(R3),0             LAST?                                        
         BE    RESET                                                            
*                                                                               
         B     B50                                                              
*                                                                               
* ADD TO TOTAL BUCKET                                                           
*                                                                               
B300     MVC   DUB(8),6(R3)        DOLLARS AND SPOTS                            
*                                                                               
         LM    RE,RF,DUB                                                        
         MVC   DUB(8),6(R5)                                                     
         A     RE,DUB                                                           
         A     RF,DUB+4                                                         
         STM   RE,RF,DUB                                                        
         MVC   6(8,R5),DUB                                                      
*                                                                               
         CLI   1(R3),14            SKIP IF NO GRPS IN BUCKETS                   
         BNH   B310                                                             
*                                                                               
         ICM   RE,15,14(R3)        BUCKET GRPS                                  
         ICM   RF,15,14(R5)        TOTAL GRPS                                   
         AR    RF,RE               UPDATED TOTAL GRPS                           
         STCM  RF,15,14(R5)                                                     
*                                                                               
B310     DS    0H                                                               
*                                                                               
         B     B250                                                             
*                                                                               
* PREPARE TO BRANCH BACK TO SEQUENTIAL OR FLIGHT READ                           
*                                                                               
RESET    TM    BYTE4,FLT           IS FLIGHT INDICATOR ON                       
         BZ    NEXTBUY             NO-GO TO SEQUENTIAL READ                     
         LA    R8,L'ENTREC(R8)     INCREMENT POINTER                            
         BCT   R6,FLTREAD                                                       
*                                  FOR CANCELLED FLIGHTS                        
         TM    BYTE4,FLTWT         IF FLIGHT HEADER NOT PRINTED                 
         BZ    *+8                 SHOULD NOT PRINT WEEK TOTALS                 
         BAS   RE,WKTOT            PUT OUT TOTALS FOR LAST FLIGHT               
*                                  SKIP A LNE BEFORE CONTRACT                   
         BAS   RE,PRINT            TOTALS                                       
         B     TOTAL                                                            
*                                                                               
TOTAL    TM    RCONCNTL,X'80'      DELETED K?                                   
         BZ    TOT10                                                            
         MVC   MYP+10(24),=CL24'*** CONTRACT DELETED ***'                       
         BAS   RE,PRINT                                                         
         B     XIT                                                              
*                                                                               
TOT10    DS    0H                                                               
         TM    RCONMODR+1,X'20'    MON???                                       
         BZ    TOT100                                                           
*                                                                               
*******************************************************************             
*                                                                 *             
* WORK3X IS THE STARTING BUCKET TOTAL   (BUILT IN DISBUCK)        *             
*    AREA+0(2)=YYMM                                               *             
*    AREA+2(4)=DOLLARS                                            *             
*                                                                 *             
*******************************************************************             
*                                                                               
         BAS   RE,PRINT                                                         
*******************************************************************             
* BUILD TOTAL BUCKETS                                                           
*******************************************************************             
BLDBUCK  DS    0H                                                               
         LA    R5,WORK3                                                         
         XCEF  (R5),240                                                         
         XR    R3,R3               KEEP TOTAL $ COUNTER                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   BBX                 NO BUCKETS YET                               
BB4      MVC   0(2,R5),2(R6)       SAVE BUCK YYMM                               
BB5      L     R1,6(R6)            R1=BUCK AMT                                  
         AR    R3,R1               TOTAL TOTAL (ALL MOS)                        
         A     R1,2(R5)            ADD RUNNING TOTAL                            
         ST    R1,2(R5)                                                         
         BAS   RE,NEXTEL                                                        
         BNE   BBX                                                              
         CLC   0(2,R5),2(R6)       SAME BDCST MON?                              
         BE    BB5                                                              
         LA    R5,6(R5)            NEXT TOTAL BUCKET                            
         B     BB4                                                              
BBX      DS    0H                                                               
         ST    R3,WORK2                                                         
DISBUCK  DS    0H                                                               
*                                                                               
         LA    R3,RCONDATE         DEFAULT TO DISPLAY NORMAL DATES              
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   DB05                                                             
         USING RCONRFEL,R6                                                      
         OC    RCONRFLT,RCONRFLT   HAVE REVISED DATE?                           
         BZ    DB05                                                             
         LA    R3,RCONRFLT         YES - USE IT                                 
         DROP  R6                                                               
DB05     GOTO1 DATCON,DMCB,(3,0(R3)),(0,WORK)      K START DATE                 
         GOTO1 DATCON,DMCB,(3,3(R3)),(0,WORK+6)  K END DATE                     
* WORK HAS EBCDIC START AND WORK+6 HAS END DATE                                 
         GOTO1 VGTBROAD,DMCB,(1,WORK),TEMP,GETDAY,ADDAY                         
         GOTO1 VGTBROAD,DMCB,(1,WORK+6),TEMP+12,GETDAY,ADDAY                    
* TEMP+6 HAS END DATE OF BROADCAST MONTH OF K START DATE                        
* TEMP+18 HAS END DATE OF BROADCAST MONTH OF K END DATE                         
         GOTO1 DATCON,DMCB,(0,TEMP+6),(3,WORK)                                  
         GOTO1 DATCON,DMCB,(0,TEMP+18),(3,WORK+3)                               
* WORK NOW HAS START BROADCAST MONTH & YEAR AND WORK+3 HAS END                  
* BROADCAST MONTH AND YEAR (YMD - BINARY)                                       
*                                                                               
         LA    R5,WORK3                                                         
         MVC   HALF,TEMP+6         K START YEAR                                 
         MVC   BYTE2,WORK          SAVE BIN YEAR                                
         ZIC   R4,WORK+1           START MON                                    
         ZIC   R3,WORK+4           END MON                                      
         CLC   WORK(1),WORK+3      SAME YEAR?                                   
         BE    *+8                                                              
         LA    R3,12(R3)           ADJUST FOR NEXT YEAR                         
         SR    R3,R4                                                            
         LA    R3,1(R3)            NUM MONTHS                                   
         LR    R1,R3               GET NUM OF LINES                             
         SRL   R1,2                DIV BY 4, FUCK THE REMAINDER                 
         LA    R1,1(R1)                                                         
         LR    R0,R1               CAN'T 'LA' R0...                             
         BCTR  R4,0                MONTHS ARE 0 RELATIVE                        
         MH    R4,=Y(L'MONTBL)     R4 NOW AT STARTING MONTH                     
         LA    R4,MONTBL(R4)       IT HELPS TO POINT IT AT THE TABLE            
         L     R2,AIO2                                                          
*                                                                               
         XCEF  (R2),=A(L'IO2)      CLEAR FOR COMBO MULTIPLE USE                 
*                                                                               
DB10     DS    0H                                                               
         MVC   0(3,R2),1(R4)       MMM                                          
         MVC   3(2,R2),HALF        YY                                           
         LA    R2,7(R2)                                                         
*                                                                               
* DISPLAY $ (IF ANY)                                                            
         CLC   0(1,R5),BYTE2       THIS YEAR?                                   
         BNE   DB20                                                             
         CLC   1(1,R5),0(R4)       THIS MONTH?                                  
         BNE   DB20                                                             
         LR    R1,R0               EDIT KILLS R0                                
         EDIT  (4,2(R5)),(10,0(R2)),2                                           
         LR    R0,R1                                                            
         LA    R5,6(R5)            SET NEXT TOTAL BUCKET                        
DB20     LA    R2,13(R2)                                                        
         LA    R4,L'MONTBL(R4)     NEXT MON                                     
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BNE   DB30                NO - CONTINUE                                
         LA    R4,MONTBL           BACK TO START OF TBL                         
         MVC   HALF,TEMP+18        K END YEAR (EBCDIC)                          
         ZIC   R1,BYTE2                                                         
         LA    R1,1(R1)                                                         
         STC   R1,BYTE2                                                         
DB30     BCT   R3,DB10                                                          
         LR    R5,R0               EDIT KILLS R0                                
         MVC   0(05,R2),=C'TOTAL'                                               
         EDIT  (4,WORK2),(10,7(R2)),2,FLOAT=$                                   
         LR    R2,R5                                                            
*        LA    R5,RBUYREC                                                       
*        LA    R5,1000(R5)                                                      
         L     R5,AIO2                                                          
         B     TOT200                                                           
* GET K TOTALS                                                                  
*TOT100   LA    R5,RBUYREC                                                      
*         LA    R5,1000(R5)                                                     
TOT100   L     R5,AIO2                                                          
*                                                                               
         CLI   BYTE3,C'A'          AGY COPY (NO RATES)?                         
         BE    TOTXIT                                                           
         GOTO1 REGENTL2,DMCB,(SVDEMFLG,WORK3),(R5)                              
*                                                                               
         SR    R2,R2                                                            
         IC    R2,DMCB+4           TOTAL LINES                                  
         SR    R3,R3                                                            
         IC    R3,LNE                                                           
         LA    R3,1(R3,R2)                                                      
         CLC   =X'0002',WORK4      ANY TRADE DOLLARS IN ARRAY?                  
         BE    TOT110              NO                                           
         LA    R3,1(R3)            YES - ADD 1 LINE TO COUNT                    
TOT110   EQU   *                                                                
         SR    R4,R4                                                            
         IC    R4,MAXLNES                                                       
         SR    R4,R3               ENOUGH ROOM?                                 
         BZ    TOT200                                                           
         BP    TOT150                                                           
         GOTO1 =A(HEADLINE),DMCB,(RC),RR=Y                                      
         B     TOT200                                                           
*                                                                               
TOT150   CH    R4,=H'2'                                                         
         BNH   *+8                                                              
         LA    R4,2                                                             
*                                                                               
         BAS   RE,PRINT                                                         
         BCT   R4,*-4                                                           
*                                                                               
TOT200   DS    0H                                                               
         CLC   =X'0002',WORK4      ANY TRADE DOLLARS IN ARRAY?                  
         BE    TOT210              NO                                           
*                                  YES - DISPLAY DOLLAR TYPE                    
         CLC   =X'0002',WORK3      ANY CASH  DOLLARS IN ARRAY?                  
         BE    TOT220              NO                                           
         MVC   MYP+1(08),=C'**CASH**'                                           
         BAS   RE,PRINT                                                         
TOT210   DS    0H                                                               
         MVC   MYP+1(79),0(R5)       TOTAL LINES                                
         BAS   RE,PRINT                                                         
         LA    R5,80(R5)                                                        
         BCT   R2,TOT210                                                        
         CLC   =X'0002',WORK4      ANY TRADE DOLLARS IN ARRAY?                  
         BE    TOT340              NO                                           
*                                  YES - ACCUM   DOLLAR TYPE                    
         OC    WORK5,WORK5         ANY VALUE IN 'CASH TOTAL'                    
         BNZ   TOT215              YES - ACCUMULATE, DON'T LOAD                 
         MVC   WORK5,WORK3         NO  - LOAD, DON'T ACCUMULATE                 
         B     TOT220                                                           
TOT215   EQU   *                                                                
         GOTO1 =A(GENLTOTL),DMCB,WORK5,WORK3,RR=Y                               
TOT220   EQU   *                                                                
*                                                                               
         GOTO1 REGENTL2,DMCB,(SVDEMFLG,WORK4),(R5)                              
         SR    R2,R2                                                            
         IC    R2,DMCB+4           TOTAL LINES                                  
         SR    R3,R3                                                            
         IC    R3,LNE                                                           
         LA    R3,1(R3,R2)                                                      
         CLC   =X'0002',WORK4      ANY TRADE DOLLARS IN ARRAY?                  
         BE    TOT230              NO                                           
         LA    R3,1(R3)            ADD 1 LINE TO COUNT FOR TRADE                
*                                     BANNER                                    
TOT230   EQU   *                                                                
         SR    R4,R4                                                            
         IC    R4,MAXLNES                                                       
         SR    R4,R3               ENOUGH ROOM?                                 
         BZ    TOT300                                                           
         BP    TOT250                                                           
         GOTO1 =A(HEADLINE),DMCB,(RC),RR=Y                                      
         B     TOT300                                                           
*                                                                               
TOT250   CH    R4,=H'2'                                                         
         BNH   *+8                                                              
         LA    R4,2                                                             
*                                                                               
         BAS   RE,PRINT                                                         
         BCT   R4,*-4                                                           
*                                                                               
TOT300   DS    0H                                                               
*                                  YES - DISPLAY DOLLAR TYPE                    
         CLC   =X'0002',WORK4      ANY TRADE DOLLARS IN ARRAY?                  
         BE    TOT340              NO  - ONLY CASH FIGURES SHOWN                
*                                  YES - DISPLAY DOLLAR TYPE                    
         MVC   MYP+1(09),=C'**TRADE**'                                          
         BAS   RE,PRINT                                                         
TOT320   DS    0H                                                               
         MVC   MYP+1(79),0(R5)       TOTAL LINES                                
         BAS   RE,PRINT                                                         
         LA    R5,80(R5)                                                        
         BCT   R2,TOT320                                                        
         CLC   =X'0002',WORK4      ANY TRADE DOLLARS IN ARRAY?                  
         BE    TOT340              NO  - ONLY CASH FIGURES SHOWN                
*                                  YES - DISPLAY DOLLAR TYPE                    
         OC    WORK6,WORK6         ANY VALUE IN 'CASH TOTAL'                    
         BNZ   TOT325              YES - ACCUMULATE, DON'T LOAD                 
         MVC   WORK6,WORK4         NO  - LOAD, DON'T ACCUMULATE                 
         B     TOT340                                                           
TOT325   EQU   *                                                                
         GOTO1 =A(GENLTOTL),DMCB,WORK6,WORK4,RR=Y                               
*                                  TRADE FIGURES (WORK4) ADDED IN               
*                                                                               
TOT340   DS    0H                                                               
         CLC   =X'0002',WORK3      ANY CASH  DOLLARS IN ARRAY?                  
         BE    TOTXIT              NO  - TOTALS NOT NEEDED                      
*                                                                               
         CLC   =X'0002',WORK4      YES - ANY TRADE DOLLARS IN ARRAY?            
         BE    TOTXIT              NO  - TOTALS NOT NEEDED                      
*                                                                               
         MVC   TWATTLAR,WORK4      YES - LOAD TRADE TOTALS                      
         GOTO1 =A(TOTLTOTL),DMCB,(RC),RR=Y                                      
*                                  WILL ADD IN CASH TOTALS                      
         GOTO1 REGENTL2,DMCB,(SVDEMFLG,TWATTLAR),(R5)                           
         SR    R2,R2                                                            
         IC    R2,DMCB+4           TOTAL LINES                                  
         SR    R3,R3                                                            
         IC    R3,LNE                                                           
         LA    R3,1(R3,R2)                                                      
         LA    R3,1(R3)            ADD 1 LINE TO COUNT FOR CASH+TRADE           
*                                     BANNER                                    
         SR    R4,R4                                                            
         IC    R4,MAXLNES                                                       
         SR    R4,R3               ENOUGH ROOM?                                 
         BZ    TOT400                                                           
         BP    TOT350                                                           
         GOTO1 =A(HEADLINE),DMCB,(RC),RR=Y                                      
         B     TOT400                                                           
*                                                                               
TOT350   CH    R4,=H'2'                                                         
         BNH   *+8                                                              
         LA    R4,2                                                             
*                                                                               
         BAS   RE,PRINT                                                         
         BCT   R4,*-4                                                           
*                                                                               
TOT400   DS    0H                                                               
*                                  YES - DISPLAY DOLLAR TYPE                    
         MVC   MYP+1(14),=C'**CASH+TRADE**'                                     
         BAS   RE,PRINT                                                         
TOT420   DS    0H                                                               
         MVC   MYP+1(79),0(R5)       TOTAL LINES                                
         BAS   RE,PRINT                                                         
         LA    R5,80(R5)                                                        
         BCT   R2,TOT420                                                        
*                                                                               
*                                                                               
TOTXIT   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
* YES, NO AND XIT                                                               
*********************************************************************           
         PRINT GEN                                                              
         ANSR                                                                   
*                                     YES, NO AND XIT                           
         PRINT NOGEN                                                            
         EJECT                                                                  
*********************************************************************           
* PRINT COMPONENT STATION CALL LETTERS FOR COMBO ORDERS                         
*********************************************************************           
PRTCALL  NTR1                                                                   
         L     R1,0(R1)            POINT TO OUTPUT LINE                         
*                                                                               
         CLI   TWACOMBO,0          IF COMBO ORDER, SHOW ALL COMPONENT           
         BE    PRTCX               STATIONS INVOLVE                             
*                                                                               
         LA    RE,4                AT MOST 4 COMPONENT STATIONS                 
         LA    RF,TWACMBS1         POINT TO FIRST STATION TO PRINT              
*                                                                               
PRTC10   OC    0(L'TWACMBS1,RF),0(RF)                                           
         BZ    PRTC20                                                           
         MVC   0(4,R1),0(RF)       CALL LETTER                                  
         MVI   4(R1),C'-'                                                       
         MVC   5(1,R1),4(RF)       BAND                                         
*                                                                               
         LA    RF,9(RF)                                                         
         LA    R1,8(R1)                                                         
         BCT   RE,PRTC10                                                        
*                                                                               
PRTC20   DS    0H                                                               
         TM    KPRTOPTS,KPRTOPFX   ARE WE FAXING?                               
         BZ    PRTCX               NO                                           
         MVI   0(R1),C'/'                                                       
         MVC   1(20,R1),CONSTAM    MARKET                                       
*                                                                               
PRTCX    B     XIT                                                              
         TITLE 'T80217-PRINT CONTRACT COMMENTS'                                 
*********************************************************************           
* PRINT ROUTINE                                                                 
*********************************************************************           
PRINT    NTR1                                                                   
         L     R2,AFACILS                                                       
         L     R2,12(R2)           A(TIA)                                       
         LA    RE,=C'PRTQUE '                                                   
         ST    RE,DMCB+4                                                        
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMPRINT',,,MYP-1,(TERMNAL,(R2))                  
         TM    DMCB+8,X'FF'        ERROR?                                       
         BZ    PRT10                                                            
         DC    H'0',C'$PQFULL'     PRINT ERROR                                  
PRT10    XC    MYP,MYP                                                          
         IC    RE,LNE                                                           
         LA    RE,1(RE)            BUMP LNE COUNT                               
         CLI   MYP-1,SPACE2        2 LINES                                      
         BNE   *+12                                                             
         LA    RE,1(RE)                                                         
         B     PRTXIT                                                           
         CLI   MYP-1,SPACE3        3 LINES                                      
         BNE   PRTXIT                                                           
         LA    RE,2(RE)                                                         
*                                                                               
PRTXIT   STC   RE,LNE                                                           
         MVI   MYP-1,SPACE1        DEFAULT TO 1 ALWAYS                          
         B     EXXMOD                                                           
         EJECT                                                                  
*********************************************************************           
* SUB-ROUTINES TO SUPPORT THE SORTING OF FLIGHTED BUYS, PRINTING A              
* LABEL ABOVE BUYS OF THE SAME FLIGHT, MAINTAINING BUCKETS OF WEEKLY            
* SPOTS AND DOLLARS FOR THE FLIGHT, AND PRINTING THE TOTALS                     
*********************************************************************           
SORTFLT  EQU   *                   SORT ENTRIES OF FLIGHTED BUYS                
         ST    RE,FULL                                                          
         OI    BYTE4,FLT+FIRST     TURN ON INDICATORS FOR ENTRY TO FLT          
         L     R8,ASPULAR                                                       
         A     R8,=AL4(256)                                                     
         ZIC   R6,BYTE             INSERT COUNTER OF FLIGHTED BUYS              
         XC    DMCB+16(4),DMCB+16                                               
         GOTO1 =V(XSORT),DMCB,(R8),(R6),L'ENTREC,3,,RR=YES                      
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*********************************************************************           
* UPDATE WEEKLY BUCKET TOTALS                                                   
*********************************************************************           
UPWKTOT  ST    RE,FULL                                                          
         TM    RBUYCNTL,X'80'      DO NOT INCLUDE DELETED LNE                   
         BOR   RE                                                               
         CLI   RBUYCHGI,C'C'       OR CANCELLED LINE                            
         BER   RE                                                               
         ZIC   RE,RBUYNW           NUMBER PER WEEK                              
         LR    R0,RE                                                            
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BO    UPWKT10             YES                                          
         AH    R0,HALF2            UPDATE CASH SPOTS/WK BUCKET                  
         STH   R0,HALF2                                                         
         B     UPWKT20                                                          
UPWKT10  AH    R0,TEMP+8           UPDATE TRADE SPOTS/WK BUCKET                 
         STH   R0,TEMP+8                                                        
UPWKT20  ICM   RF,15,RBUYCOS                                                    
         MR    RE,RE               COST/SPOT X SPOTS/WK                         
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BO    UPWKT30             YES                                          
         A     RF,SAVETOT          UPDATE CASH COST/WK BUCKET                   
         ST    RF,SAVETOT                                                       
         B     UPWKT40                                                          
UPWKT30  A     RF,TEMP+4           UPDATE TRADE COST/WK BUCKET                  
         ST    RF,TEMP+4                                                        
UPWKT40  L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO PRINT WEEKLY TOTAL LNE                                             
*********************************************************************           
WKTOT    ST    RE,FULL                                                          
         CLC   LNE,MAXLNES         WILL LNE FIT ON PG                           
         BL    WKTOT1              YES                                          
         GOTO1 =A(HEADLINE),DMCB,(RC),RR=Y                                      
*                                                                               
WKTOT1   MVC   MYP+37(24),=C'**WEEKLY FLIGHT TOTALS**'                          
         SPACE                                                                  
         EDIT  (2,HALF2),(3,MYP+62)    CASH                                     
         EDIT  (4,SAVETOT),(10,MYP+66),2,FLOAT=-                                
         OC    HALF2,HALF2         ANY CASH SPOTS?                              
         BZ    WKTOT4              NO                                           
         CLI   BYTE3,C'A'          AGENCY COPY (NO RATES)                       
         BNE   *+14                                                             
         MVI   MYP+66,ASTER                                                     
         MVC   MYP+67(9),MYP+66        FILL COST WITH ASTERISKS                 
         BAS   RE,PRINT                                                         
*                                                                               
WKTOT4   DS    0H                                                               
         EDIT  (2,TEMP+8),(3,MYP+62)    TRADE                                   
         MVI   MYP+65,C'T'                                                      
         EDIT  (4,TEMP+4),(10,MYP+66),2,FLOAT=-                                 
         OC    TEMP+8(2),TEMP+8    ANY CASH SPOTS?                              
         BZ    WKTOT5              NO                                           
         CLI   BYTE3,C'A'          AGENCY COPY (NO RATES)                       
         BNE   *+14                                                             
         MVI   MYP+66,ASTER                                                     
         MVC   MYP+67(9),MYP+66        FILL COST WITH ASTERISKS                 
         BAS   RE,PRINT                                                         
*                                                                               
WKTOT5   DS    0H                                                               
         XC    HALF2,HALF2         ZERO BUCKETS FOR SPOTS                       
         XC    SAVETOT,SAVETOT     AND DOLLARS                                  
         XC    TEMP+4(6),TEMP+4    AND TRADE BUCKS&SPOTS                        
         SPACE                                                                  
WKTOTEX  NI    BYTE4,X'FF'-FLTWT   RESET WEEK TOTALS                            
         NI    BYTE4,X'FF'-FIRST   TURN OFF INDICATOR                           
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO PRINT HEADER 'FLIGHT N' FOR EACH CHANGE IN FLT NUM                 
*********************************************************************           
FLTHEAD  ST    RE,FULL                                                          
         ZIC   R1,LNE              WILL HEADER FIT ON PG                        
         LA    R1,6(R1)            INCREMENT LNE CNT BY SIZE OF HEAD            
         CLM   R1,1,MAXLNES                                                     
         BNH   FLTHEAD5            IT FITS                                      
         GOTO1 =A(HEADLINE),DMCB,(RC),RR=Y                                      
*                                                                               
FLTHEAD5 MVI   MYP-1,SPACE2          SKIP 2 LINES BEFORE LABEL                  
         BAS   RE,PRINT                                                         
         MVC   MYP+8(7),=C'FLIGHT '                                             
         EDIT  (1,RBUYFLT),(3,MYP+15),ALIGN=LEFT                                
         LR    R1,R0               SWITCH NUMBER LENGTH TO R1                   
         LA    R1,7(R1)            FIND LENGTH OF DASHES                        
         SPACE                                                                  
         BAS   RE,PRINT                                                         
         MVI   MYP+8,DASH                                                       
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP+9(0),MYP+8          PROPAGATE DASHES                         
         BAS   RE,PRINT                                                         
         BAS   RE,PRINT                                                         
         NI    BYTE4,X'FF'-FLTH    TURN OFF INDICATOR                           
         OI    BYTE4,FLTWT                                                      
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO BUILD A FLIGHT ENTRY FOR LATER XSORT CALL                          
*********************************************************************           
BLDENTRY TM    RBUYCNTL,X'80'      BUILD A FLIGHT ENTRY FOR                     
         BZ    *+10                CANCELLED, BUT NOT DELETED BUYS.             
         CLI   RBUYCHGI,C'X'                                                    
         BER   RE                                                               
         MVC   ENTFLT,RBUYFLT      FLIGHT NUMBER                                
         MVC   ENTMAS,RBUYKMLN     MASTER LNE                                   
         MVC   ENTLIN,RBUYKLIN     LNE                                          
         MVC   ENTDA,KEY+28        DISK ADDRESS                                 
         ZIC   R1,BYTE                                                          
         LA    R1,1(R1)            INCREMENT COUNTER                            
         STC   R1,BYTE                                                          
         LA    R8,L'ENTREC(R8)     INCREMENT POINTER TO XSORT ENTRIES           
         BR    RE                                                               
         EJECT                                                                  
*********************************************************************           
* PRINT EI CODES                                                                
*********************************************************************           
PRTEICDS NTR1                                                                   
         LA    R6,RCONREC                                                       
         USING RCONIEL,R6                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRTEICDX                                                         
         MVC   MYP+13(EIHEADLQ),EIHEADER                                        
         MVC   MYP+31(4),RCONIADV                                               
         MVC   MYP+41(4),RCONIPRD                                               
         MVC   MYP+51(4),RCONIPR2                                               
         MVC   MYP+61(10),RCONXEST                                              
         OC    MYP+61(10),MYSPACES                                              
         CLC   MYP+61(10),MYSPACES                                              
         BNE   *+10                                                             
         MVC   MYP+61(4),RCONIEST                                               
                                                                                
PRTEICDX DS    0H                                                               
         MVI   MYP-1,SPACE2                                                     
         BAS   RE,PRINT                                                         
         B     XIT                                                              
         DROP  R6                                                               
                                                                                
EIHEADER DS    0C                                                               
         DC    C'AGENCY CODES  CLT:      PRD:      PTR:      EST:'              
EIHEADLQ EQU   *-EIHEADER                                                       
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO PRINT *** CONTRACT COMMENT ***                                     
*********************************************************************           
PRTHEAD  NTR1                                                                   
         CLI   PUTBYTE,TRUE                                                     
         BE    XIT                                                              
         MVC   MYP+13(24),=CL24'*** CONTRACT COMMENT ***'                       
         MVI   PUTBYTE,TRUE        TURN ON SWITCH                               
         BAS   RE,PRINT                                                         
         BAS   RE,PRINT                                                         
         B     XIT                                                              
         EJECT                                                                  
*  ++INCLUDE REPINTLIST                                                         
       ++INCLUDE REPINTLIST                                                     
         PRINT ON                                                               
*                                                                               
RTGTBL   DS    0CL3                                                             
         DC    C'ARBNSISRCBIRTRCMTDRAM',X'004040',X'FF'                         
*                                                                               
MONTBL   DS    0CL4                                                             
         DC    X'01',C'JAN'                                                     
         DC    X'02',C'FEB'                                                     
         DC    X'03',C'MAR'                                                     
         DC    X'04',C'APR'                                                     
         DC    X'05',C'MAY'                                                     
         DC    X'06',C'JUN'                                                     
         DC    X'07',C'JUL'                                                     
         DC    X'08',C'AUG'                                                     
         DC    X'09',C'SEP'                                                     
         DC    X'0A',C'OCT'                                                     
         DC    X'0B',C'NOV'                                                     
         DC    X'0C',C'DEC'                                                     
         DC    X'FF'                                                            
         EJECT                                                                  
* EQUATES USED TO SUPPORT FLIGHT FEATURES                                       
*                                                                               
ASTER    EQU   C'*'                                                             
DASH     EQU   C'-'                                                             
SPACE1   EQU   X'09'                                                            
SPACE2   EQU   X'11'                                                            
SPACE3   EQU   X'19'                                                            
TRUE     EQU   C'T'                                                             
FALSE    EQU   C'F'                                                             
FLT      EQU   X'80'               READING FLIGHT BUYS                          
FLTH     EQU   X'40'               PRINT FLIGHT HEADER                          
FIRST    EQU   X'20'               FIRST FLIGHT BUY READ                        
FLTWT    EQU   X'10'               PRINT FLIGHT WEEK TOTALS                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*  ++INCLUDE RECNTWR2K                                                          
       ++INCLUDE RECNTWR2K                                                      
         PRINT ON                                                               
         EJECT                                                                  
         ORG   LOCALTWA                                                         
WORK4    DS    CL240                                                            
WORK5    DS    CL240                                                            
WORK6    DS    CL240                                                            
         EJECT                                                                  
* DSECT TO COVER INVENTORY (TEXT) RECORDS                                       
*                                                                               
RINVD    DSECT                                                                  
       ++INCLUDE REGENINV                                                       
         SPACE 2                                                                
* DSECT TO COVER FLIGHTED BUY ENTRIES                                           
*                                                                               
ENTD     DSECT                                                                  
ENTREC   DS    0CL7                                                             
ENTFLT   DS    X                                                                
ENTMAS   DS    X                                                                
ENTLIN   DS    X                                                                
ENTDA    DS    XL4                                                              
         SPACE 2                                                                
         EJECT                                                                  
*  ++INCLUDE DMPRTQL                                                            
*  ++INCLUDE REGENPBYD                                                          
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE REGENPBYD                                                      
         PRINT ON                                                               
*  ++INCLUDE EDIDDSHD                                                           
*  ++INCLUDE EDILINKD                                                           
*  ++INCLUDE REGENCFC                                                           
*  ++INCLUDE REGENCOV                                                           
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
       ++INCLUDE REGENCFC                                                       
       ++INCLUDE REGENCOV                                                       
         PRINT ON                                                               
         TITLE 'T80217- PRINT ROUTINES'                                         
T80217   CSECT                                                                  
*********************************************************************           
* PRINT K & STATION COMMENTS                                                    
*********************************************************************           
PRTKCMT  NTR1  BASE=*,LABEL=*                                                   
* HARD CODED COMMENTS                                                           
         MVI   PUTBYTE,FALSE       SET A SWITCH FOR CON COMM LITERAL            
*                                                                               
PKCMT03  DS    0H                  PRINT STANDARD COMMENT ATTACHED TO           
         CLI   RCONTYPE,0          TYPE, IF ANY                                 
         BE    STACMT                                                           
         GOTO1 =A(TYPECMT),DMCB,(RC),RR=Y                                       
*                                                                               
STACMT   XC    KEY,KEY             BUILD STATION KEY                            
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONKSTA                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    STACMT10                                                         
         DC    H'0',C'MISSING STA REC'                                          
*                                                                               
STACMT10 GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA           GET LIABILITY POSITION                       
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BNE   STACMT20                                                         
         USING RSTAELEM,R6                                                      
         MVC   TWAALIAB,RSTALIAB                                                
         DROP  R6                                                               
*                                                                               
STACMT20 LA    R6,IOAREA                                                        
         MVI   ELCODE,3                                                         
         BAS   RE,GETEL                                                         
         BNE   KCMT                                                             
*                                                                               
         USING RSTACEL,R6                                                       
         CLI   RSTACTYP,C'L'       LIBRARY REFERENCE                            
         BE    STACMT40                                                         
*                                                                               
         BAS   RE,PRTHEAD          PRINT K CMT HEADER LINE                      
*                                                                               
         XC    WORK2(132),WORK2    STATION STORED COMMENTS                      
         ZIC   R1,RSTACLEN                                                      
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),RSTACCMT                                                
*                                                                               
         GOTO1 =A(PSTCMT),DMCB,(3,WORK2),RR=Y PRINT STATION COMMENTS            
*                                                                               
         BAS   RE,PRINT                                                         
         B     KCMT                GO TO CONTRACT COMMENT RTN                   
*                                                                               
STACMT40 DC    0H'0'               LIBRARY REFERENCE LOGIC                      
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RINVKEY,R5                                                       
         MVI   RINVKTYP,RINVKTYQ                                                
         MVC   RINVKREP,REPALPHA                                                
         MVC   RINVKSTA,=CL5'ZZZZZ'                                             
         MVI   RINVKRSR,X'FF'                                                   
         MVC   RINVKTXT,RSTACNUM                                                
         DROP  R5,R6                                                            
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   KCMT                COULD NOT FIND L REFERENCE                   
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         LA    R5,34(R6)           R5 POINTS TO FIRST ELEMENT                   
         USING RINVTEL,R5                                                       
*                                                                               
         BAS   RE,PRTHEAD          PRINT K CMT HEADER LINE                      
*                                                                               
STACMT50 LA    R2,12               COUNTER FOR MAX LINES                        
*                                                                               
STACMT60 DC    0H'0'               LOOP THROUGH TEXT ELS                        
         CLI   0(R5),1                                                          
         BNE   STACMT70                                                         
         ZIC   R1,1(R5)                                                         
         SH    R1,=H'7'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP+13(0),RINVTEXT                                               
         BAS   RE,PRINT                                                         
         LA    R1,7(R1)            RESTORE ELEMENT LENGTH                       
         AR    R5,R1               POINT TO NEXT ELEMENT                        
         BCT   R2,STACMT60                                                      
*                                                                               
STACMT70 DS    0H                                                               
         BAS   RE,PRINT                                                         
         DROP  R5                                                               
         EJECT                                                                  
*********************************************************************           
* CONTRACT COMMENTS                                                             
*********************************************************************           
KCMT     DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'25'        ANY DARE AGENCY HIATUS DATES?                
         BAS   RE,GETEL                                                         
         BE    KCMT20              YES, PRINT HEADER                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'26'        ANY HIATUS COMMENTS?                         
         BAS   RE,GETEL                                                         
         BNE   KCMT50              NO, NO HIATUS INFO                           
                                                                                
KCMT20   DS    0H                                                               
         BAS   RE,PRTHEAD          PRINT K CMT HEADER LINE                      
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        IS DARE AGY ORDER ELEM THERE?                
         BAS   RE,GETEL                                                         
         BNE   KCMT45              NO, CONTINUE                                 
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'      YES, IS CONTR LINKED TO AGY ORDER?           
         BNO   KCMT45              NO, CONTINUE                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 =A(HIATUS),DMCB,(RC),RR=Y    PRINT HIATUS DATES                  
         B     KCMT50                                                           
*                                                                               
KCMT45   DS    0H                                                               
         GOTO1 =A(NODAHIAT),DMCB,(RC),RR=Y  PRINT NON-DARE HIATUS INFO          
                                                                                
KCMT50   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,2            ANY CONTRACT COMMENTS ?                      
         BAS   RE,GETEL                                                         
         BNE   KCMT60                                                           
*                                                                               
         BAS   RE,PRTHEAD          PRINT K CMT HEADER LINE                      
*                                                                               
         GOTO1 =A(PSTCMT),DMCB,(2,0(R6)),RR=Y  1ST COMMENT LINE                 
*                                                                               
         BAS   RE,NEXTEL           BUMP FORWARD TO GET NEXT COMMENT             
         BNE   KCMT60                                                           
*                                                                               
         GOTO1 =A(PSTCMT),DMCB,(2,0(R6)),RR=Y  2ND COMMENT LINE                 
*                                                                               
         BAS   RE,PRINT            SKIP LNE AFTER COMMENTS                      
*                                                                               
KCMT60   DS    0H                                                               
         GOTO1 =A(PSTCMT),DMCB,(3,=C'SC=*'),RR=Y                                
         EJECT                                                                  
*********************************************************************           
* AGENCY COMMENTS                                                               
*********************************************************************           
AGYCMT   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'1A'           RAGY2REC                                     
         MVC   KEY+19(4),RCONKAGY                                               
         MVC   KEY+23(2),RCONKAOF                                               
         MVC   KEY+25(2),RCONKREP                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   SLCMT                                                            
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'40'        AGY CMT ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   SLCMT                                                            
*                                                                               
         BAS   RE,PRTHEAD          PRINT K CMT HEADER LINE                      
*                                                                               
         GOTO1 =A(PSTCMT),DMCB,(2,0(R6)),RR=Y  1ST COMMENT LINE                 
*                                                                               
         BAS   RE,NEXTEL           BUMP FORWARD TO GET NEXT COMMENT             
         BNE   SLCMT                                                            
*                                                                               
         GOTO1 =A(PSTCMT),DMCB,(2,0(R6)),RR=Y  2ND COMMENT LINE                 
*                                                                               
         BAS   RE,PRINT            SKIP LNE AFTER COMMENTS                      
         EJECT                                                                  
***********************************************************************         
* STATION LIABILITY COMMENTS                                                    
***********************************************************************         
SLCMT    CLI   TWAALIAB,0                                                       
         BE    SLCMT10                                                          
*                                                                               
         BAS   RE,PRTHEAD          PRINT K CMT HEADER LINE                      
*                                                                               
         GOTO1 =A(PSTCMT),DMCB,(4,TWAALIAB),RR=Y                                
         BAS   RE,PRINT                                                         
*                                                                               
SLCMT10  DS    0H                                                               
***>     MVI   MYP-1,SPACE2        2 SPACES                                     
***>     BAS   RE,PRINT                                                         
***>>>   B     EXXMOD                                                           
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PRINT THE GRAND TOTAL - COMBO ONLY                                  *         
***********************************************************************         
GRANDTOT NTR1  BASE=*,LABEL=*                                                   
         CLI   BYTE3,C'A'          AGY COPY (NO RATES)?                         
         BE    GTOTX                                                            
*                                                                               
         L     R5,AIO2                                                          
*                                                                               
         CLC   =X'0002',WORK6      ANY TRADE GRAND TOTALS IN ARRAY?             
         BE    GTOT005             NO  - JUST SHOW COMBO TOTALS                 
         CLC   =X'0002',WORK5      YES - ANY CASH GRAND TOTALS?                 
         BE    GTOT0400            NO  - JUST SHOW TRADE                        
         B     GTOT0200            YES - SHOW CASH+TRADE                        
*                                                                               
GTOT005  EQU   *                                                                
         GOTO1 REGENTL2,DMCB,(SVDEMFLG,TWATTLAR),(R5)                           
         SR    R2,R2                                                            
         IC    R2,DMCB+4           TOTAL LINES                                  
         SR    R3,R3                                                            
         IC    R3,LNE                                                           
         LA    R3,1(R3,R2)                                                      
         SR    R4,R4                                                            
         IC    R4,MAXLNES                                                       
         SR    R4,R3               ENOUGH ROOM?                                 
         BZ    GTOT15                                                           
         BP    GTOT10                                                           
         GOTO1 =A(HEADLINE),DMCB,(RC),RR=Y                                      
         B     GTOT15                                                           
*                                                                               
GTOT10   CH    R4,=H'2'                                                         
         BNH   *+8                                                              
         LA    R4,2                                                             
*                                                                               
         BAS   RE,PRINT                                                         
         BCT   R4,*-4                                                           
*                                                                               
GTOT15   MVC   MYP(13),=C'COMBO TOTALS:'                                        
         BAS   RE,PRINT                                                         
*                                                                               
GTOT20   DS    0H                                                               
         MVC   MYP+1(79),0(R5)       TOTAL LINES                                
         BAS   RE,PRINT                                                         
         LA    R5,80(R5)                                                        
         BCT   R2,GTOT20                                                        
         B     GTOTX               EXIT ROUTINE                                 
*                                                                               
GTOT0200 EQU   *                                                                
         GOTO1 REGENTL2,DMCB,(SVDEMFLG,WORK5),(R5)                              
         SR    R2,R2                                                            
         IC    R2,DMCB+4           TOTAL LINES                                  
         SR    R3,R3                                                            
         IC    R3,LNE                                                           
         LA    R3,1(R3,R2)                                                      
         SR    R4,R4                                                            
         IC    R4,MAXLNES                                                       
         SR    R4,R3               ENOUGH ROOM?                                 
         BZ    GTOT0240                                                         
         BP    GTOT0220                                                         
         GOTO1 =A(HEADLINE),DMCB,(RC),RR=Y                                      
         B     GTOT0240                                                         
*                                                                               
GTOT0220 CH    R4,=H'2'                                                         
         BNH   *+8                                                              
         LA    R4,2                                                             
*                                                                               
         BAS   RE,PRINT                                                         
         BCT   R4,*-4                                                           
*                                                                               
GTOT0240 MVC   MYP(15),=C'**CASH TOTALS**'                                      
         BAS   RE,PRINT                                                         
*                                                                               
GTOT0260 DS    0H                                                               
         MVC   MYP+1(79),0(R5)       TOTAL LINES                                
         BAS   RE,PRINT                                                         
         LA    R5,80(R5)                                                        
         BCT   R2,GTOT0260                                                      
*                                                                               
GTOT0400 EQU   *                                                                
         GOTO1 REGENTL2,DMCB,(SVDEMFLG,WORK6),(R5)                              
         SR    R2,R2                                                            
         IC    R2,DMCB+4           TOTAL LINES                                  
         SR    R3,R3                                                            
         IC    R3,LNE                                                           
         LA    R3,1(R3,R2)                                                      
         SR    R4,R4                                                            
         IC    R4,MAXLNES                                                       
         SR    R4,R3               ENOUGH ROOM?                                 
         BZ    GTOT0440                                                         
         BP    GTOT0420                                                         
         GOTO1 =A(HEADLINE),DMCB,(RC),RR=Y                                      
         B     GTOT0440                                                         
*                                                                               
GTOT0420 CH    R4,=H'2'                                                         
         BNH   *+8                                                              
         LA    R4,2                                                             
*                                                                               
         BAS   RE,PRINT                                                         
         BCT   R4,*-4                                                           
*                                                                               
GTOT0440 MVC   MYP(16),=C'**TRADE TOTALS**'                                     
         BAS   RE,PRINT                                                         
*                                                                               
GTOT0460 DS    0H                                                               
         MVC   MYP+1(79),0(R5)       TOTAL LINES                                
         BAS   RE,PRINT                                                         
         LA    R5,80(R5)                                                        
         BCT   R2,GTOT0460                                                      
*                                                                               
         CLC   =X'0002',WORK5      ANY CASH GRAND TOTALS?                       
         BE    GTOTX               YES - SHOW BOTH CASH + TRADE                 
*                                                                               
         GOTO1 =A(GENLTOTL),DMCB,WORK6,WORK5,RR=Y                               
*                                  CASH+TRADE GRAND TOTAL                       
         GOTO1 REGENTL2,DMCB,(SVDEMFLG,WORK6),(R5)                              
         SR    R2,R2                                                            
         IC    R2,DMCB+4           TOTAL LINES                                  
         SR    R3,R3                                                            
         IC    R3,LNE                                                           
         LA    R3,1(R3,R2)                                                      
         SR    R4,R4                                                            
         IC    R4,MAXLNES                                                       
         SR    R4,R3               ENOUGH ROOM?                                 
         BZ    GTOT0540                                                         
         BP    GTOT0520                                                         
         GOTO1 =A(HEADLINE),DMCB,(RC),RR=Y                                      
         B     GTOT0540                                                         
*                                                                               
GTOT0520 CH    R4,=H'2'                                                         
         BNH   *+8                                                              
         LA    R4,2                                                             
*                                                                               
         BAS   RE,PRINT                                                         
         BCT   R4,*-4                                                           
*                                                                               
GTOT0540 MVC   MYP(16),=C'**GRAND TOTALS**'                                     
         BAS   RE,PRINT                                                         
*                                                                               
GTOT0560 DS    0H                                                               
         MVC   MYP+1(79),0(R5)       TOTAL LINES                                
         BAS   RE,PRINT                                                         
         LA    R5,80(R5)                                                        
         BCT   R2,GTOT0560                                                      
*                                                                               
GTOTX    XIT1                                                                   
         EJECT                                                                  
***>>>                                                                          
***********************************************************************         
*  GENLTOTL:  ACCUMULATES HIGHER-LEVEL TOTALS                                   
*    P1            =    ADDRESS OF HIGHER LEVEL TOTALS                          
*    P2            =    TOTALS TO BE ADDED IN                                   
*    IO2           =    TEMPORARY WORK SPACE (THIS IS IO2)                      
***********************************************************************         
*                                                                               
*  EQUATES ARE ESTABLISHED IN ROUTINE TOTLTOTL                                  
*                                                                               
*BCKTDATE EQU   2                                                               
*BCKTVALU EQU   6                                                               
*BCKTSPOT EQU   10                                                              
*                                                                               
GENLTOTL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AIO2                                                          
         L     R6,0(R1)            SET A(HIGHER LEVEL TOTALS)                   
         ST    R6,AHILEVEL         SAVE A(HIGH LEVEL TOTALS)                    
         L     R3,4(R1)            SET A(TOTALS TO BE ADDED IN)                 
*                                    OUTSIDE DSECT ADDRESSABILITY               
         MVC   0(240,R2),0(R6)     LOAD TEMPORARY WORK SPACE                    
         LR    RE,R3               A(LOWER LEVEL TOTALS)                        
         ZICM  R0,0(RE),2          L(ENTRY)                                     
         AR    RE,R0               A(END OF ENTRIES)                            
         LR    RF,R2               A(TEMP WORK SPACE)                           
         ZICM  R0,0(RF),2          L(ENTRY)                                     
         AR    RF,R0               A(END OF ENTRIES)                            
         LA    R1,2(R3)            A(1ST BUCKET STA IN PROG)                    
         LA    R2,2(R2)            A(1ST BUCKET TEMP WORK SPACE)                
         LA    R3,2(R6)            A(1ST BUCKET ACCUMULATOR)                    
GENL0010 EQU   *                                                                
         CR    R1,RE               STA IN PROGRESS AT END?                      
         BNE   GENL0020            NO  - CHECK TEMP WORK SPACE                  
         CR    R2,RF               YES - TEMP WORK SPACE AT END?                
         BNE   GENL0050            NO  - RUN TEMP WORK SPACE                    
         B     GENL0100            YES -                                        
GENL0020 EQU   *                                                                
         CR    R2,RF               TEMP WORK SPACE AT END?                      
         BE    GENL0060            YES - RUN OUT STA IN PROGRESS                
*                                                                               
*   NEITHER ARRAY AT END:  COMPARE DATES                                        
*                                                                               
         CLC   BCKTDATE(2,R1),BCKTDATE(R2)                                      
         BL    GENL0030            STA IN PROG < TEMP WORK SPACE                
         BH    GENL0040            TEMP WORK SPACE < STA IN PROG                
*                                                                               
*   DATES EQUAL:  ACCUMULATE SPOTS, TOTALS AND GRPS                             
*                                                                               
         MVC   DUB(4),BCKTVALU(R1)                                              
         L     R4,DUB                                                           
         MVC   DUB(4),BCKTVALU(R2)                                              
         L     R5,DUB                                                           
         AR    R4,R5                                                            
         ST    R4,DUB                                                           
         MVC   0(14,R3),0(R1)      SET UP ACCUMULATOR ELEMENT                   
         MVC   BCKTVALU(4,R3),DUB  LOAD NEW DOLLARS                             
*                                                                               
* ACCUMULATE TOTAL SPOTS                                                        
*                                                                               
         ZICM  R4,BCKTSPOT(R1),4                                                
         ZICM  R5,BCKTSPOT(R2),4                                                
         AR    R4,R5                                                            
         STCM  R4,15,BCKTSPOT(R3)                                               
*                                                                               
*        ACCUMULATE GRPS                                                        
*                                                                               
         CLI   1(R3),14            SKIP IF NO GRPS IN ELEMENT                   
         BNH   GENL0025                                                         
*                                                                               
         ICM   R4,15,BCKTGRPS(R1)                                               
         ICM   R5,15,BCKTGRPS(R2)                                               
         AR    R4,R5                                                            
         STCM  R4,15,BCKTGRPS(R3)                                               
*                                                                               
GENL0025 DS    0H                                                               
*                                                                               
         LLC   R4,1(R1)            BUMP TO NEXT ELEMENTS                        
         LA    R1,0(R4,R1)                                                      
         LLC   R4,1(R2)                                                         
         LA    R2,0(R4,R2)                                                      
         LLC   R4,1(R3)                                                         
         LA    R3,0(R4,R3)                                                      
*                                                                               
         B     GENL0010                                                         
*                                                                               
GENL0030 EQU   *                                                                
*                                                                               
         LLC   R4,1(R1)                                                         
         BCTR  R4,0                DECREMENT FOR EXECUTE                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)      RUN OUT STA IN PROGRESS                       
*                                                                               
         LLC   R4,1(R1)            BUMP TO NEXT ELEMENTS                        
         LA    R1,0(R4,R1)                                                      
         LLC   R4,1(R3)                                                         
         LA    R3,0(R4,R3)                                                      
*                                                                               
         B     GENL0010                                                         
*                                                                               
GENL0040 EQU   *                                                                
*                                                                               
         LLC   R4,1(R2)                                                         
         BCTR  R4,0                DECREMENT FOR EXECUTE                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)      RUN OUT TEMP WORK SPACE                       
*                                                                               
         LLC   R4,1(R2)                                                         
         LA    R2,0(R4,R2)                                                      
         LLC   R4,1(R3)                                                         
         LA    R3,0(R4,R3)                                                      
*                                                                               
         B     GENL0010                                                         
*                                                                               
GENL0050 EQU   *                   RUN OUT TEMP WORK SPACE                      
*                                                                               
         CR    R2,RF               END OF TEMP WORK SPACE?                      
         BE    GENL0100            YES                                          
*                                                                               
         LLC   R4,1(R2)                                                         
         BCTR  R4,0                DECREMENT FOR EXECUTE                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)      RUN OUT TEMP WORK SPACE                       
*                                                                               
         LLC   R4,1(R2)                                                         
         LA    R2,0(R4,R2)                                                      
         LLC   R4,1(R3)                                                         
         LA    R3,0(R4,R3)                                                      
*                                                                               
         B     GENL0050                                                         
*                                                                               
GENL0060 EQU   *                   RUN OUT STA IN PROGRESS                      
*                                                                               
         CR    R1,RE               END OF STA IN PROGRESS?                      
         BE    GENL0100            YES                                          
*                                                                               
         LLC   R4,1(R1)                                                         
         BCTR  R4,0                DECREMENT FOR EXECUTE                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)                                                    
*                                                                               
         LLC   R4,1(R1)            BUMP TO NEXT ELEMENTS                        
         LA    R1,0(R4,R1)                                                      
         LLC   R4,1(R3)                                                         
         LA    R3,0(R4,R3)                                                      
*                                                                               
         B     GENL0060                                                         
*                                                                               
GENL0100 EQU   *                                                                
*                                                                               
         L     RF,AHILEVEL         RECALCULATE LENGTH                           
         SR    R3,RF                                                            
         STH   R3,DUB                                                           
         MVC   0(2,RF),DUB         INSERT NEW LENGTH                            
         XIT1                                                                   
AHILEVEL DS    A                   ADDR(HIGH LEVEL TOTALS)                      
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
***>>>                                                                          
***********************************************************************         
* PURPOSE:                                                                      
*     PRINT STORED COMMENTS IF ANY, ELSE IT WILL PRINT FREE                     
*     FORM COMMENTS                                                             
*                                                                               
* INPUT: PARAMETER 1: BYTE 1    = MODE                                          
*                     BYTE 2-4  = A(COMMENT CODE)                               
*                                                                               
* OUTPUT: NONE                                                                  
***********************************************************************         
PSTCMT   NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)                                                         
         MVC   STCMODE(1),0(R1)                                                 
*                                                                               
         L     R8,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(STCMODE,0(R3)),(R8),DATAMGR,RCONREC,0             
         BNZ   PSTCMTX             COMMENT NOT FOUND, PRINT NOTHING             
*                                                                               
         CLI   0(R8),X'FF'         IF X'FF', PRINT NOTHING                      
         BE    PSTCMTX                                                          
         CLI   0(R8),0             IF NULL, PRINT FREE FORM COMMENT             
         BE    PSTCMT20                                                         
*                                                                               
PSTCMT10 ZIC   R4,0(R8)            GET LENGTH OF COMMENT                        
         BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   MYP+13(0),1(R8)                                                  
         BAS   RE,PRINT                                                         
*                                                                               
         ZIC   R4,0(R8)            BUMP TO NEXT COMMENT ENTRY                   
         AR    R8,R4               X'FF' MARKS THE END OF BLOCK                 
         CLI   0(R8),X'FF'         IF X'FF', DONE                               
         BE    PSTCMTX                                                          
*                                                                               
         L     R4,AIO4             BOUNDARY CHECK FOR R8                        
         A     R4,=AL4(L'IO4+1)                                                 
         CR    R4,R8                                                            
         BH    PSTCMT10                                                         
         B     PSTCMTX                                                          
*                                                                               
PSTCMT20 DS    0H                  PRINT FREE FORM COMMENT                      
         CLI   STCMODE,2           FROM CONTRACT COMMENT                        
         BNE   PSTCMT30                                                         
         CLI   1(R3),3                                                          
         BL    PSTCMTX                                                          
         ZIC   R4,1(R3)                                                         
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   MYP+13(0),2(R3)                                                  
         B     PSTCMT40                                                         
*                                                                               
PSTCMT30 CLI   STCMODE,3           FROM STATION COMMENT                         
         BNE   PSTCMTX             ANYTHING ELSE DON'T PRINT                    
         LA    R4,59                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   MYP+13(0),0(R3)                                                  
*                                                                               
PSTCMT40 DS    0H                                                               
         BAS   RE,PRINT                                                         
*                                                                               
PSTCMTX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PRINT HEADING FOR FAX/EASYLINK COPIES                                         
***********************************************************************         
FAXHEAD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MYP+1(2),=C'MC'                                                  
         MVC   MYP+5(2),=C'LN'                                                  
         MVC   MYP+8(4),=C'DAYS'                                                
         MVC   MYP+21(5),=C'TIMES'                                              
         MVC   MYP+33(3),=C'LEN'                                                
         MVC   MYP+37(9),=C'EFF DATES'                                          
         MVC   MYP+50(3),=C'CLS'                                                
         MVC   MYP+62(3),=C'NPW'                                                
         MVC   MYP+66(4),=C'RATE'                                               
         MVC   MYP+77(3),=C'TOT'                                                
         BAS   RE,PRINT                                                         
         MVC   MYP+50(3),=C'SEC'                                                
         MVC   MYP+62(3),=C'PLA'                                                
         MVC   MYP+77(3),=C'SPT'                                                
         BAS   RE,PRINT                                                         
         MVC   MYP+1(2),=12C'-'                                                 
         MVC   MYP+5(2),=12C'-'                                                 
         MVC   MYP+8(12),=12C'-'                                                
         MVC   MYP+21(10),=12C'-'                                               
         MVC   MYP+33(3),=12C'-'                                                
         MVC   MYP+37(12),=12C'-'                                               
         MVC   MYP+50(3),=12C'-'                                                
         MVC   MYP+62(3),=12C'-'                                                
         MVC   MYP+66(10),=12C'-'                                               
         MVC   MYP+77(3),=12C'-'                                                
         BAS   RE,PRINT                                                         
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PRTCOV - PRINT COVERSHEET                                                     
***********************************************************************         
PRTCOV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
COVWIDE  EQU   80                                                               
*                                                                               
         MVI   COVERFLG,0                                                       
         LA    R6,RCONREC                                                       
         CLI   RCONREC,X'0C'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,X'A6'                                                     
         BAS   RE,GETEL                                                         
         BNE   COVERX                                                           
*                                                                               
         USING RCONCVEL,R6                                                      
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING RCOVREC,R2                                                       
         GOTOX (RFCONLOW,VREPFACS),DMCB,RCONREC  GET LOWEST K NUMBER            
         MVI   KEY,X'49'                                                        
         MVC   RCOVKREP,RCONKREP                                                
         MVC   RCOVKNAM,RCONCVNM                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY                   
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,AIO4,DMWORK,0         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO4                                                          
         DROP  R6                                                               
*                                                                               
         TM    RCOVFLAG,RCOVFLAF                                                
         BZ    *+8                                                              
         OI    COVERFLG,X'80'      AUTO FORMAT ON                               
*                                                                               
         LA    R3,COVWIDE                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   MYP(0),=132C'*' SECTION HEADER                                   
         LA    R3,1(R3)                                                         
         SH    R3,=Y(L'COVHEAD)                                                 
         SRL   R3,1                                                             
         LA    R4,MYP                                                           
         AR    R4,R3                                                            
         MVC   0(L'COVHEAD,R4),COVHEAD                                          
         BAS   RE,PRINT                                                         
         MVC   MYP,=(L'MYP)C' '                                                 
         BAS   RE,PRINT                                                         
         MVC   MYP,=(L'MYP)C' '                                                 
*                                                                               
         LA    R6,RCOVEL1          POINT TO 1ST ELEM IN RCOVREC                 
         DROP  R2                                                               
*                                                                               
COVER050 DS    0H                                                               
         BAS   RE,GETCOV           GET NEXT COVER TEXT ELEM                     
         LTR   R6,R6               HAVE ANOTHER ELEMENT?                        
         BZ    COVER200            NO -WRAP IT UP                               
*                                                                               
         CLC   =C'SC=',2(R6)       SFM CMT?                                     
         BE    *+14                                                             
         CLC   =C'C=',2(R6)        STD CMT?                                     
         BNE   COVER054                                                         
         BAS   RE,STDCMT                                                        
         B     COVER050                                                         
*                                                                               
COVER054 DS    0H                                                               
         CLI   1(R6),2             PRINTING BLANK LINE?                         
         BH    COVER060            NO, NEXT CASE                                
         TM    COVERFLG,X'80'      AUTO FORMAT ON                               
         BZ    COVER055                                                         
         CLC   MYP,MYSPACES        ANYTHING ON LINE?                            
         BE    COVER055                                                         
         BAS   RE,PRINT                                                         
         MVC   MYP,=(L'MYP)C' '                                                 
COVER055 DS    0H                                                               
         ZIC   R1,LNE                                                           
         LA    R1,1(R1)                                                         
         CLM   R1,1,MAXLNES        ROOM LEFT?                                   
         BL    COVER056                                                         
         GOTO1 =A(HEADLINE),DMCB,(RC),RR=Y                                      
         B     COVER050            NEXT ELEMENT                                 
COVER056 DS    0H                                                               
         BAS   RE,PRINT                                                         
         MVC   MYP,=(L'MYP)C' '                                                 
         B     COVER050            NEXT ELEMENT                                 
*                                                                               
COVER060 DS    0H                                                               
         ZIC   R3,1(R6)                                                         
         SH    R3,=H'2'            LENGTH FOR CURRENT DATA                      
         LA    R5,2(R6)            A(CURRENT DATA)                              
         MVC   FULL,0(R5)                                                       
         OC    FULL,MYSPACES       KEEP THE 1ST 4 CHAR IN CAPS                  
*                                                                               
         CH    R3,=H'2'            CHECK FOR '$P' PAGE BREAK                    
         BNE   COVER070                                                         
         CLC   =C'$P',FULL                                                      
         BNE   COVER070                                                         
         GOTO1 =A(HEADLINE),DMCB,(RC),RR=Y                                      
         B     COVER050                                                         
*                                                                               
COVER070 DS    0H                  CHECK FOR '$ON' CONTROL                      
         CH    R3,=H'3'                                                         
         BNE   COVER080                                                         
         CLC   =C'$ON',FULL                                                     
         BNE   COVER080                                                         
         OI    COVERFLG,X'80'      AUTO FORMAT ON                               
         B     COVER050                                                         
*                                                                               
COVER080 DS    0H                  CHECK FOR '$OFF' CONTROL                     
         CH    R3,=H'4'                                                         
         BNE   COVER090                                                         
         CLC   =C'$OFF',FULL                                                    
         BNE   COVER090                                                         
         NI    COVERFLG,X'FF'-X'80' AUTO FORMAT OFF                             
         CLC   MYP,MYSPACES        ANYTHING ALREADY ON LINE?                    
         BE    COVER090            NO - OK                                      
         ZIC   R1,LNE                                                           
         LA    R1,1(R1)                                                         
         CLM   R1,1,MAXLNES        ROOM LEFT?                                   
         BL    COVER085                                                         
         GOTO1 =A(HEADLINE),DMCB,(RC),RR=Y                                      
COVER085 DS    0H                                                               
         BAS   RE,PRINT                                                         
         MVC   MYP,=(L'MYP)C' '                                                 
         B     COVER050                                                         
*                                                                               
COVER090 DS    0H                  HANDLE LINE TEXT                             
         ZIC   R1,LNE                                                           
         LA    R1,1(R1)                                                         
         CLM   R1,1,MAXLNES        ROOM LEFT?                                   
         BL    COVER095                                                         
         GOTO1 =A(HEADLINE),DMCB,(RC),RR=Y                                      
COVER095 DS    0H                                                               
         TM    COVERFLG,X'80'      AUTO FORMAT ON?                              
         BZ    COVER150            NO                                           
*                                                                               
COVER100 DS    0H                  FIGURE OUT HOW MUCH SPACE LEFT               
         LA    RF,MYP              START OF PRINT LINE                          
         LA    R2,COVWIDE                                                       
         AR    R2,RF               END OF PRINT LINE                            
         SR    R4,R4               CLEAR COUNTER                                
         BCTR  R2,0                BACK UP 1 CHAR                               
         CLI   0(R2),C' '          SPACE ?                                      
         BNE   *+18                                                             
         LA    R4,1(R4)                                                         
         CR    R2,RF               EMPTY LINE?                                  
         BE    COVER105            GO MOVE TEXT TO LINE                         
         B     *-20                                                             
*                                                                               
         CH    R4,=H'2'            AT LEAST 2 SPACES ON LINE                    
         BL    COVER120            NO - FLUSH LINE & GET NEW ONE                
*                                                                               
         BCTR  R4,0                LEAVE A SPACE BEFORE INSERTION               
         LA    R2,2(R2)            A(INSERTION POINT)                           
*                                                                               
         CR    R3,R4               TEXT LEN VS SPACE LEFT                       
         BH    COVER110            CAN'T FIT ALL TEXT                           
*                                                                               
COVER105 DS    0H                                                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R5)       TEXT TO LINE                                 
         B     COVER050            NEXT ELEMENT                                 
*                                                                               
COVER110 DS    0H                                                               
         LA    R5,0(R4,R5)         MAX DISPL INTO DATA WE CAN TAKE              
         LA    RE,2(R6)                                                         
         CLI   0(R5),C' '                                                       
         BE    *+18                                                             
         BCTR  R4,0                                                             
         BCTR  R5,0                                                             
         CR    R5,RE                                                            
         BNH   COVER120            DON'T HAVE ANYTHING THAT FITS                
         B     *-18                                                             
*                                                                               
         BCTR  R4,0                DO PARTIAL MOVE TO PRINT LINE                
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),2(R6)                                                    
         BAS   RE,PRINT                                                         
         MVC   MYP,=(L'MYP)C' '                                                 
*                                                                               
         LA    R5,1(R5)            START OF REMAINDER OF TEXT                   
         LA    R4,2(R4)            ALLOW FOR PREV EX AND SPACE                  
         SR    R3,R4               LENGTH OF REMAINING TEXT                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   MYP(0),0(R5)        MOVE REST OF TEXT TO PRINT LINE              
         B     COVER050            GET NEXT ELEM                                
*                                                                               
COVER120 DS    0H                  NEED NEW LINE                                
         BAS   RE,PRINT                                                         
         MVC   MYP,=(L'MYP)C' '                                                 
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   MYP(0),0(R5)        START NEW LINE                               
         B     COVER050            GET NEXT ELEM                                
                                                                                
*                                                                               
COVER150 DS    0H                  HANDLE NON-AUTOFORMAT LINE                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   MYP(0),0(R5)                                                     
         BAS   RE,PRINT                                                         
         MVC   MYP,=(L'MYP)C' '                                                 
         B     COVER050            NEXT ELEMENT                                 
*                                                                               
COVER200 DS    0H                  CLOSE COVERSHEET SECTION                     
         TM    COVERFLG,X'80'      AUTO FORMAT ON?                              
         BZ    COVER210                                                         
         CLC   MYP,MYSPACES        ANYTHING LEFT ON LINE?                       
         BE    COVER210                                                         
         BAS   RE,PRINT                                                         
         MVC   MYP,=(L'MYP)C' '                                                 
COVER210 DS    0H                                                               
         LA    RF,COVWIDE                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MYP(0),=132C'*'                                                  
         BAS   RE,PRINT                                                         
         MVC   MYP,=(L'MYP)C' '                                                 
         MVI   PG,1                                                             
         GOTO1 =A(HEADLINE),DMCB,(RC),RR=Y                                      
*                                                                               
COVERX   XIT1                                                                   
*                                                                               
COVHEAD  DC    C' CONTRACT COVERSHEET '                                         
*                                                                               
***********************************************************************         
* GETCOV - POINTS R6 TO NEXT ELEMENT IN RECORD SET R6=0 WHEN END OF REC         
***********************************************************************         
GETCOV   DS    0H                                                               
         ST    RE,FULL             SAVE RE FOR RETURN                           
*                                                                               
         MVI   ELCODE,3                                                         
         BAS   RE,NEXTEL           NEXT TEXT ELEM                               
         BE    GETCOVX             GOT IT, ALL DONE                             
*                                                                               
         L     R1,AIO4                                                          
         USING RCOVREC,R1                                                       
         MVC   KEY(27),RCOVKEY     GET NEXT COVER RECORD IN SET                 
         ZIC   RE,RCOVKSEQ                                                      
         LA    RE,1(RE)                                                         
         STC   RE,KEY+26                                                        
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY                   
         CLC   KEY(27),KEYSAVE     ANOTHER RECORD IN SET?                       
         BE    GETCOV30            YES - PROCESS IT                             
         SR    R6,R6               NO - RETURN R6=0                             
         B     GETCOVX             RETURN                                       
*                                                                               
GETCOV30 DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,AIO4,DMWORK,0         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO4                                                          
         LA    R6,RCOVEL1          1ST ELEM IN REC                              
         CLI   0(R6),3                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
GETCOVX  DS    0H                                                               
         L     RE,FULL                                                          
         BR    RE                                                               
         DROP  R1                                                               
         B     COVERX                                                           
*                                                                               
* EXPLODE STANDARD COMMENTS                                                     
*                                                                               
STDCMT   NTR1                                                                   
         CLC   MYP,MYSPACES        ANYTHING ON LINE?                            
         BE    *+8                                                              
         BAS   RE,PRINT            YES, FLUSH IT                                
*                                                                               
         ZIC   R1,1(R6)                                                         
         AHI   R1,-3                                                            
         MVC   MYP(0),2(R6)        DEFAULT PRINT ACTUAL TEXT                    
         EX    R1,*-6                                                           
*                                                                               
         L     R3,AIO3                                                          
         GOTO1 VREGENSC,DMCB,(2,(R6)),(R3),DATAMGR,RCONREC,GETTXT               
         BNZ   OCM090              COMMENT NOT FOUND                            
         CLI   0(R3),0             IF NULL, PRINT FREE FORM COMMENT             
         BE    OCM090                                                           
*                                                                               
OCM070   DS    0H                                                               
         CLI   0(R3),X'FF'         DONE                                         
         BNE   OCM075                                                           
         ZIC   R1,1(R6)                                                         
         AR    R6,R1               NEXT ELEM                                    
         B     OCMX                                                             
OCM075   DS    0H                                                               
         ZIC   R1,0(R3)                                                         
         AHI   R1,-2                                                            
         MVC   MYP(0),1(R3)                                                     
         EX    R1,*-6                                                           
         BAS   RE,PRINT                                                         
         LA    R3,2(R1,R3)         NEXT LINE                                    
         L     RF,AIO3                                                          
         AHI   RF,L'IO3                                                         
         CR    R3,RF               BOUNDARY CHECK                               
         BH    OCMX                OUT OF BOUNDS - GET OUT                      
         B     OCM070              NEXT LINE                                    
*                                                                               
OCM090   DS    0H                                                               
         BAS   RE,PRINT                                                         
OCMX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* CHECK STATION PROFILES TO SEE IF WE NEED TO DISPLAY DEMO OR NOT               
*                                                                               
CKSTPROF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*SMY*    LR    RF,RA                                                            
*SMY*    AHI   RF,TWAWORKQ                                                      
*SMY*    USING TWAWORK,RF                                                       
*                                                                               
         TM    PROFILES+CNTDEMOB,CNTDEMOA                                       
         BO    CKST0100            DISALLOW STATION TO SEE DEMO                 
*                                                                               
         TM    TWASTAOB,X'02'                                                   
         BO    CKSTPRN             DO NOT PROCESS DEMO                          
         B     CKSTPRY                                                          
CKST0100 DS    0H                                                               
         TM    TWASTAOB,X'02'                                                   
         BZ    CKSTPRN                                                          
*SMY*    DROP  RF                                                               
*                                                                               
CKSTPRY  SR    RC,RC                                                            
CKSTPRN  LTR   RC,RC                                                            
CKSTPRX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* PRINT CONTRACT TYPE AND DEVELOPMENT TYPE DESCRIPTION IF ANY                   
* AS DICTATED BY CONTRACT PROFILE 22                                            
*********************************************************************           
TYPEDESC CSECT                                                                  
         NMOD1 0,*TYPDES*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         CLI   RCONTYPE,0                                                       
         BE    TYPED30                                                          
TKEYD    USING RCTYKEY,KEY                                                      
         XC    KEY,KEY             GET CONTRACT TYPE                            
         MVI   TKEYD.RCTYKTYP,X'32'                                             
         MVC   TKEYD.RCTYKCTY,RCONTYPE                                          
         MVC   TKEYD.RCTYKREP,REPALPHA                                          
         DROP  TKEYD                                                            
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   TYPED30                                                          
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA           SAVE DESCRIPTION IN TWA                      
         USING RCTYREC,R6                                                       
         MVC   TWACTDES,RCTYDESC                                                
         DROP  R6                                                               
*                                                                               
TYPED30  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'18'        DEVELOPMENTAL ELEMENT PRESENT?               
         BAS   RE,GETEL                                                         
         BNE   TYPEDX                                                           
*                                                                               
         USING RCONDVEL,R6                                                      
         CLI   RCONDVCT,0                                                       
         BE    TYPEDX                                                           
*                                                                               
DKEYD    USING RDCTKEY,KEY                                                      
         XC    KEY,KEY             GET DEVELOPMENTAL CONTRACT TYPE              
         MVI   DKEYD.RDCTKTYP,X'3B'                                             
         MVC   DKEYD.RDCTKCTY,RCONDVCT                                          
         MVC   DKEYD.RDCTKREP,REPALPHA                                          
         DROP  DKEYD                                                            
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   TYPEDX                                                           
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA           SAVE DESCRIPTION IN TWA                      
         USING RDCTREC,R6                                                       
         MVC   TWADTDES,RDCTDESC                                                
         DROP  R6                                                               
*                                                                               
TYPEDX   DS    0H                                                               
         XMOD1                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* FOR TYPE N CONTRACTS ONLY, IF A STANDARD COMMENT CODE IS FOUND                
* PRINT IT IN THE COMMENT SECTION                                               
*********************************************************************           
TYPECMT  CSECT                                                                  
         NMOD1 0,*TYPCMT*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         XC    KEY,KEY             GET CONTRACT TYPE                            
TKEYD    USING RCTYKEY,KEY                                                      
         MVI   TKEYD.RCTYKTYP,X'32'                                             
         MVC   TKEYD.RCTYKCTY,RCONTYPE                                          
         MVC   TKEYD.RCTYKREP,REPALPHA                                          
         DROP  TKEYD                                                            
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   TYPENX                                                           
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         USING RCTYREC,R6                                                       
         CLI   RCTY1LEN,RCTYELMX                                                
         BL    TYPENX              OLD RECORD DO NOT HAS S/C CODE               
*                                                                               
         L     R2,AIO4                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(2),=C'C='                                                   
         MVC   WORK+2(L'RCTYCMMT),RCTYCMMT                                      
         DROP  R6                                                               
*                                                                               
         GOTO1 VREGENSC,DMCB,(X'03',WORK),(R2),DATAMGR,RCONREC,GETTXT           
         BNZ   TYPENX              COMMENT NOT FOUND, PRINT NOTHING             
         CLI   0(R2),X'FF'         IF X'FF', PRINT NOTHING                      
         BE    TYPENX                                                           
         CLI   0(R2),0             IF NULL, PRINT FREE FORM COMMENT             
         BE    TYPENX                                                           
*                                                                               
         BAS   RE,PRTHEAD          PRINT K CMT HEADER LINE                      
*                                                                               
TYPEN10  DS    0H                                                               
         ZIC   R4,0(R2)            GET LENGTH OF COMMENT                        
         BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   MYP+13(0),1(R2)                                                  
         BAS   RE,PRINT                                                         
*                                                                               
         ZIC   R4,0(R2)            BUMP TO NEXT COMMENT ENTRY                   
         AR    R2,R4                                                            
         CLI   0(R2),X'FF'         IF X'FF', DONE                               
         BE    TYPENX                                                           
*                                                                               
         L     R4,AIO4             BOUNDARY CHECK FOR R2                        
         A     R4,=AL4(L'IO4+1)                                                 
         CR    R4,R2                                                            
         BH    TYPEN10                                                          
*                                                                               
TYPENX   DS    0H                                                               
         XMOD1                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* PRINT DARE AGENCY HIATUS DATES IF ANY                                         
*********************************************************************           
HIATUS   CSECT                                                                  
         NMOD1 0,*HIATUS*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   HIATUSX                                                          
         LR    R5,R6                                                            
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL                                                         
         BNE   HIATUSX                                                          
         USING RCONHIEL,R6                                                      
                                                                                
         CLI   RCONHILN,2          SKIP IF NO DATES                             
         BNH   HIATUSX                                                          
                                                                                
         BAS   RE,PRINT                                                         
         MVC   MYP+13(23),=C'*AGENCY HIATUS DATE(S)*'                           
         BAS   RE,PRINT                                                         
                                                                                
         ZIC   R2,RCONHILN                                                      
         SH    R2,=H'2'            SUBTRACT OVERHEAD AND                        
         SRL   R2,1                DIVIDE BY 2 TO GET NUMBER OF ENTRIES         
                                                                                
         LA    R6,RCONHIDT                                                      
         DROP  R6                                                               
                                                                                
         LA    R4,MYP+13                                                        
                                                                                
* IF WEEKLY, WILL TRY TO COLLASP DATES. IE AUG24-3W                             
                                                                                
HIATUS20 DS    0H                                                               
         LA    R3,1                NUMBER OF CONSECUTIVE WEEKS                  
         GOTO1 DATCON,DMCB,(2,0(R6)),(4,0(R4))                                  
         LA    R4,5(R4)                                                         
                                                                                
         USING RCONDREL,R5                                                      
         TM    RCONDRFG,X'08'      DAILY?                                       
         BO    HIATUS50                                                         
         DROP  R5                                                               
                                                                                
         CH    R2,=H'1'                                                         
         BNH   HIATUS40                                                         
                                                                                
HIATUS30 DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,0(R6)),(0,WORK2)                                  
         GOTO1 DATCON,DMCB,(2,2(R6)),(0,WORK2+6)                                
         GOTO1 ADDAY,DMCB,WORK2,WORK,7                                          
         CLC   WORK(6),WORK2+6     IF NEXT DATE IS EXACTLY ONE WEEK             
         BNE   HIATUS40            AWAY, KEEP LOOKING                           
                                                                                
         MVC   WORK2(6),WORK2+6                                                 
         LA    R3,1(R3)                                                         
         LA    R6,2(R6)                                                         
         BCTR  R2,0                                                             
         CH    R2,=H'1'                                                         
         BH    HIATUS30                                                         
         SR    R2,R2                                                            
                                                                                
HIATUS40 DS    0H                                                               
         MVI   0(R4),C'-'                                                       
         EDIT  (R3),(2,1(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         MVI   1(R4),C'W'                                                       
         LA    R4,2(R4)                                                         
                                                                                
HIATUS50 DS    0H                                                               
         LTR   R2,R2                                                            
         BZ    HIATUS80                                                         
         BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BZ    HIATUS80                                                         
                                                                                
         LA    R4,1(R4)                                                         
         LA    R6,2(R6)                                                         
         LA    RF,MYP+70                                                        
         CR    R4,RF                                                            
         BL    HIATUS20                                                         
         BAS   RE,PRINT                                                         
         LA    R4,MYP+13                                                        
         B     HIATUS20                                                         
                                                                                
HIATUS80 DS    0H                                                               
         BAS   RE,PRINT                                                         
                                                                                
HIATUSX  DS    0H                                                               
         XMOD1                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* PRINT NON-DARE HIATUS DATES AND COMMENTS IF ANY                               
*********************************************************************           
* TAKEN FROM RECNT63                                                            
NODAHIAT CSECT                                                                  
         NMOD1 0,*NODAHI*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         MVI   WORK3,C' '                                                       
         MVC   WORK3+1(L'WORK3-1),WORK3   CLEAR OUT WORK3                       
*                                                                               
*              PRINT EFFECTIVE DATES                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'25'        HIATUS DATES ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   NDHI205             PRINT COMMENTS                               
*                                                                               
         BAS   RE,PRINT                                                         
         MVC   MYP+13(16),=C'*HIATUS DATE(S)*'                                  
         BAS   RE,PRINT                                                         
*                                                                               
         LA    R2,WORK3            OUTPUT                                       
         LA    R5,WORK3+60         OUTPUT END                                   
         ZIC   R4,1(R6)                                                         
         AR    R4,R6               R4 = ELEMENT END                             
         LA    R6,2(R6)            R6 = POSITION IN ELEMENT                     
*                                                                               
PRINDTES LA    R3,WORK+20          BUILD AREA                                   
*              PRINT DATES                                                      
* 3 BYTE DATE ENTRIES: 2 BYTE COMPRESSED START DATE, 1 BYTE NUM OF              
* DAYS THAT HIATUS LASTS FROM START DATE                                        
         GOTO1 DATCON,DMCB,(2,(R6)),(4,(R3))     START DATE                     
*                                                                               
         LA    RE,5                                                             
         CLI   3(R3),C'0'          DAY =JAN01?                                  
         BNE   NDHI100                                                          
         MVC   3(1,R3),4(R3)       COMPRESS TO JAN1                             
         MVI   4(R3),C' '                                                       
         BCTR  RE,0                                                             
NDHI100  AR    R3,RE                                                            
*                                                                               
         CLI   2(R6),0             NON-ZERO NUM OF DAYS FROM START DAT?         
         BE    NDHI160             NO                                           
*                                                                               
         MVI   0(R3),C'-'          YES, THERE ARE SOME DAYS                     
         LA    R3,1(R3)                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(2,(R6)),WORK3+200   START DATE FOR ADDAY            
         ZIC   RE,2(R6)                         GET NUMBER OF DAYS              
         ST    RE,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,WORK3+200,WORK3+206   END DATE IN ADDAY FORM          
*                                                                               
         GOTO1 DATCON,DMCB,WORK3+206,(4,(R3))   END DATE IN PRINT FORM          
*                                                                               
         LA    RE,5                                                             
         CLI   3(R3),C'0'          DAY =JAN01?                                  
         BNE   NDHI120                                                          
         MVC   3(1,R3),4(R3)       COMPRESS TO JAN1                             
         MVI   4(R3),C' '                                                       
         BCTR  RE,0                                                             
NDHI120  AR    R3,RE                                                            
*                                                                               
NDHI160  LA    RE,WORK+20                                                       
         SR    R3,RE               GET ELEM PRINT LEN                           
         LR    RF,R2                                                            
         AR    RF,R3               OUTPUT PTR                                   
* CHECK IF ROOM IN FIRST LINE                                                   
         CR    RF,R5               WORK3+60                                     
         BNH   NDHI164                                                          
* FIRST LINE EXCEEDED - START AT SECOND LINE                                    
         LA    R5,500(R5)          ELIM. FIRST TEST                             
         LA    R2,WORK3+60         START 2D LINE                                
         CLI   WORK3+60,0          DELIMITER?                                   
         BNE   NDHI164                                                          
         LA    R2,1(R2)                                                         
*                                                                               
NDHI164  BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WORK+20                                                  
         MVC   WORK+20(20),MYSPACES                                             
         LA    R2,1(R3,R2)         OUTPUT PTR                                   
*                                                                               
         LA    RE,WORK3+170                                                     
         CR    R2,RE               SECOND LINE EXCEEDED?                        
         BNH   *+12                                                             
         MVI   WORK3+169,C'>'      DOESN'T FIT                                  
         B     NDHI200                                                          
*                                                                               
         LA    R6,3(R6)                                                         
         CR    R6,R4               END OF ELEMENT?                              
         BNL   NDHI200             YES                                          
*                                                                               
         MVI   0(R2),0             DELIMITER                                    
         LA    R2,1(R2)                                                         
         B     PRINDTES                                                         
*                                                                               
NDHI200  MVC   MYP+13(60),WORK3      DATES                                      
         BAS   RE,PRINT                                                         
         MVC   MYP+13(110),WORK3+60  MOVE 2D LINE (PLUS XTRA FOR OVRFL)         
         BAS   RE,PRINT                                                         
*                                                                               
* PRINT COMMENTS                                                                
NDHI205  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'26'                                                     
         BAS   RE,GETEL            ANY COMMENT ELEMENTS?                        
         BNE   NDHIX               NO, GO OUT                                   
*                                                                               
         BAS   RE,PRINT                                                         
         MVC   MYP+13(19),=C'*HIATUS COMMENT(S)*'                               
         BAS   RE,PRINT                                                         
*                                                                               
         USING RCONHCEL,R6                                                      
NDHI220  ZIC   R1,RCONHCLN         ELEMENT LENGTH                               
         LA    R4,2                                                             
         SR    R1,R4               SUBTRACT OVERHEAD                            
         LTR   R1,R1               ZERO LENGTH?                                 
         BZ    NDHI230             YES, DON'T PRINT ANYTHING                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP+13(0),RCONHCCM  MOVE TO PRINT LINE                           
         BAS   RE,PRINT                                                         
         DROP  R6                                                               
*                                                                               
NDHI230  BAS   RE,NEXTEL           ANY MORE ELEMENTS?                           
         BE    NDHI220             YES                                          
*                                                                               
NDHIX    DS    0H                                                               
         XMOD1                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*  TOTLTOTL:  ACCUMULATES EACH STATION'S TOTALS INTO A GRAND                    
*    TOTAL ARRAY, CONSIDERING DIFFERENCES IN THE MONTHS REPORTED                
*    WITHIN STATIONS.  THE FINAL TOTALS WILL BE FED BACK THROUGH                
*    THE PRINT ROUTINE TO PRODUCE GRAND TOTALS ON THE SCREEN                    
*    TWATTLAR      =    ADDRESS OF GRAND TOTALS                                 
*    WORK3         =    TOTALS OF STATION IN PROGRESS                           
*    IO2           =    TEMPORARY WORK SPACE (THIS IS IO2)                      
***********************************************************************         
BCKTDATE EQU   2                                                                
BCKTVALU EQU   6                                                                
BCKTSPOT EQU   10                                                               
BCKTGRPS EQU   14                                                               
*                                                                               
TOTLTOTL CSECT                                                                  
         NMOD1  0,*TOTL*                                                        
         L     RC,0(R1)                                                         
*                                                                               
*        LA    R2,RBUYREC          ADDRESS TEMP WORK SPACE                      
*        AH    R2,=H'1000'         1000 BYTES PAST RBUYREC IS IO2               
         L     R2,AIO2                                                          
*                                    OUTSIDE DSECT ADDRESSABILITY               
         MVC   0(200,R2),TWATTLAR  LOAD TEMPORARY WORK SPACE                    
         LA    RE,WORK3            A(STATION IN PROGRESS)                       
         ZICM  R0,0(RE),2          L(ENTRY)                                     
         AR    RE,R0               A(END OF ENTRIES)                            
         LR    RF,R2               A(TEMP WORK SPACE)                           
         ZICM  R0,0(RF),2          L(ENTRY)                                     
         AR    RF,R0               A(END OF ENTRIES)                            
         LA    R1,WORK3+2          A(1ST BUCKET STA IN PROG)                    
         LA    R2,2(R2)            A(1ST BUCKET TEMP WORK SPACE)                
         LA    R3,TWATTLAR+2       A(1ST BUCKET ACCUMULATOR)                    
TOTO0010 EQU   *                                                                
         CR    R1,RE               STA IN PROGRESS AT END?                      
         BNE   TOTO0020            NO  - CHECK TEMP WORK SPACE                  
         CR    R2,RF               YES - TEMP WORK SPACE AT END?                
         BNE   TOTO0050            NO  - RUN TEMP WORK SPACE                    
         B     TOTO0100            YES -                                        
TOTO0020 EQU   *                                                                
         CR    R2,RF               TEMP WORK SPACE AT END?                      
         BE    TOTO0060            YES - RUN OUT STA IN PROGRESS                
*                                                                               
*   NEITHER ARRAY AT END:  COMPARE DATES                                        
*                                                                               
         CLC   BCKTDATE(2,R1),BCKTDATE(R2)                                      
         BL    TOTO0030            STA IN PROG < TEMP WORK SPACE                
         BH    TOTO0040            TEMP WORK SPACE < STA IN PROG                
*                                                                               
*   DATES EQUAL:  ACCUMULATE SPOTS AND TOTALS                                   
*                                                                               
         MVC   DUB(4),BCKTVALU(R1)                                              
         L     R4,DUB                                                           
         MVC   DUB(4),BCKTVALU(R2)                                              
         L     R5,DUB                                                           
         AR    R4,R5                                                            
         ST    R4,DUB                                                           
         MVC   0(14,R3),0(R1)      SET UP ACCUMULATOR ELEMENT                   
         MVC   BCKTVALU(4,R3),DUB  LOAD NEW DOLLARS                             
*                                                                               
* ACCUMULATE TOTAL SPOTS                                                        
*                                                                               
         ZICM  R4,BCKTSPOT(R1),4                                                
         ZICM  R5,BCKTSPOT(R2),4                                                
         AR    R4,R5                                                            
         STCM  R4,15,BCKTSPOT(R3)                                               
*                                                                               
*        ACCUMULATE GRPS                                                        
*                                                                               
         CLI   1(R3),14            SKIP IF NO GRPS IN ELEMENT                   
         BNH   TOTO0025                                                         
*                                                                               
         ICM   R4,15,BCKTGRPS(R1)                                               
         ICM   R5,15,BCKTGRPS(R2)                                               
         AR    R4,R5                                                            
         STCM  R4,15,BCKTGRPS(R3)                                               
*                                                                               
TOTO0025 DS    0H                                                               
*                                                                               
         LLC   R4,1(R1)            BUMP TO NEXT ELEMENTS                        
         LA    R1,0(R4,R1)                                                      
         LLC   R4,1(R2)                                                         
         LA    R2,0(R4,R2)                                                      
         LLC   R4,1(R3)                                                         
         LA    R3,0(R4,R3)                                                      
*                                                                               
         B     TOTO0010                                                         
*                                                                               
TOTO0030 EQU   *                                                                
*                                                                               
         LLC   R4,1(R1)                                                         
         BCTR  R4,0                DECREMENT FOR EXECUTE                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)      RUN OUT STA IN PROGRESS                       
*                                                                               
         LLC   R4,1(R1)            BUMP TO NEXT ELEMENTS                        
         LA    R1,0(R4,R1)                                                      
         LLC   R4,1(R3)                                                         
         LA    R3,0(R4,R3)                                                      
*                                                                               
         B     TOTO0010                                                         
*                                                                               
TOTO0040 EQU   *                                                                
*                                                                               
         LLC   R4,1(R2)                                                         
         BCTR  R4,0                DECREMENT FOR EXECUTE                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)      RUN OUT TEMP WORK SPACE                       
*                                                                               
         LLC   R4,1(R2)                                                         
         LA    R2,0(R4,R2)                                                      
         LLC   R4,1(R3)                                                         
         LA    R3,0(R4,R3)                                                      
*                                                                               
         B     TOTO0010                                                         
*                                                                               
TOTO0050 EQU   *                   RUN OUT TEMP WORK SPACE                      
*                                                                               
         CR    R2,RF               END OF TEMP WORK SPACE?                      
         BE    TOTO0100            YES                                          
*                                                                               
         LLC   R4,1(R2)                                                         
         BCTR  R4,0                DECREMENT FOR EXECUTE                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)      RUN OUT TEMP WORK SPACE                       
*                                                                               
         LLC   R4,1(R2)                                                         
         LA    R2,0(R4,R2)                                                      
         LLC   R4,1(R3)                                                         
         LA    R3,0(R4,R3)                                                      
*                                                                               
         B     TOTO0050                                                         
*                                                                               
TOTO0060 EQU   *                   RUN OUT STA IN PROGRESS                      
*                                                                               
         CR    R1,RE               END OF STA IN PROGRESS?                      
         BE    TOTO0100            YES                                          
*                                                                               
         LLC   R4,1(R1)                                                         
         BCTR  R4,0                DECREMENT FOR EXECUTE                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)                                                    
*                                                                               
         LLC   R4,1(R1)            BUMP TO NEXT ELEMENTS                        
         LA    R1,0(R4,R1)                                                      
         LLC   R4,1(R3)                                                         
         LA    R3,0(R4,R3)                                                      
*                                                                               
         B     TOTO0060                                                         
*                                                                               
TOTO0100 EQU   *                                                                
*                                                                               
         LA    RF,TWATTLAR         RECALCULATE LENGTH                           
         SR    R3,RF                                                            
         STH   R3,DUB                                                           
         MVC   TWATTLAR(2),DUB     INSERT NEW LENGTH                            
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         TITLE 'T80217- HEADLINE ROUTINE'                                       
*********************************************************************           
* READ CONTYPE REC FOR FORMAT INFO & FORMAT CONTRACT                            
*********************************************************************           
FMT      CSECT                                                                  
         NMOD1 0,**FMT***                                                       
         L     RC,0(R1)                                                         
*                                                                               
* DEFAULT VALUES FOR PRINTED FIELDS                                             
*                                                                               
         MVI   TWAFMTFL,0               INITIALIZE FLAGS                        
         XC    TWACTDES,TWACTDES   CLEAR TYPE DESCRIPTION                       
         XC    TWADTDES,TWADTDES   FOR BOTH TYPE AND DEV TYPE                   
         MVC   TWABUYER,CONBUY          BUYER NAME                              
         XC    TWAADVNM,TWAADVNM                                                
         MVC   TWAADVNM(20),CONADVN     ADVERTISER NAME                         
         MVC   TWASALNM(20),CONSALN     SALESMAN NAME                           
         MVC   TWASALCD,RCONSAL         SALESMAN CODE                           
         MVC   TWAPRNAM,TWAREPNM        REP NAME                                
         XC    TWAPRREP,TWAPRREP        REAL REP NAME                           
         MVC   TWAPRAD1,TWAOFAD1        REP ADDRESS 1                           
*                                                                               
         XC    TWAPRAD2,TWAPRAD2        REP ADDRESS 2                           
         MVC   TWAPRAD2(18),TWAOFAD2                                            
         LA    R4,TWAPRAD2+33           FLOAT STATE & ZIP IN ADDRESS 2          
         LA    R5,TWAPRAD2                                                      
FMT010   CR    R4,R5                                                            
         BL    FMT020                                                           
         OI    0(R4),X'40'                                                      
         CLI   0(R4),X'40'                                                      
         BNE   FMT020                                                           
         BCT   R4,FMT010                                                        
FMT020   MVC   3(2,R4),TWAOFSTT                                                 
         MVC   7(10,R4),TWAOFZIP                                                
*                                                                               
*  SPECIAL CASE - KATZ CONVERTED CONTRACT - AGY NAME & ADDR IN CONREC           
*                                                                               
         TM    RCONMODR+1,X'10'    KATZ CONVERTED CONTRACT?                     
         BNO   FMT080                                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'70'        AGENCY NAME ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   FMT050              NOT FOUND - EXIT                             
         XC    TWAAGNM2,TWAAGNM2   SET A(PRINT FIELD)                           
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   TWAAGNM2(0),2(R6)                                                
FMT050   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'71'        FIRST ADDRESS ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   FMT060              NOT FOUND - EXIT                             
         XC    TWAAGAD1,TWAAGAD1                                                
         XC    TWAAGAD2,TWAAGAD2                                                
         XC    TWAAGAD3,TWAAGAD3                                                
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   TWAAGAD1(0),2(R6)                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'72'        SECOND ADDRESS ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   FMT060              NOT FOUND - EXIT                             
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   TWAAGAD2(0),2(R6)                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'73'        MAY BE A THIRD ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   FMT060              NOT FOUND - EXIT                             
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   TWAAGAD3(0),2(R6)                                                
FMT060   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'6F'        ADVERTISER NAME ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   FMT070              NOT FOUND - EXIT                             
         XC    TWAADVNM,TWAADVNM                                                
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   TWAADVNM(0),2(R6)                                                
FMT070   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'74'        SALESPERSON NAME ELEMENT                     
         BAS   RE,GETEL            (PUT INTO POINT PERSON)                      
         BNE   FMT080              NOT FOUND - EXIT                             
         XC    WPTPEXP,WPTPEXP                                                  
         XC    WPTPPH#,WPTPPH#                                                  
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   WPTPEXP(0),2(R6)                                                 
FMT075   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'75'        SALESPERSON PHONE# ELEMENT                   
         BAS   RE,GETEL            (PUT INTO POINT PERSON PHONE)                
         BNE   FMT078              NOT FOUND - EXIT                             
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   WPTPPH#(0),2(R6)                                                 
FMT078   DS    0H                       SPECIAL CASE - FOR CONVERTED            
         CLI   RCONTYPE,C'N'            TYPE N CONTRACTS, ALWAYS REPL           
         BNE   FMT080                   SAL NAME & PHONE                        
         MVC   TWASALNM,WPTPEXP         REPL SAL NAME W/PT PERS NAME            
         MVC   TWASALTL,WPTPPH#         REPL SAL PH# W/PT PERS PHONE            
         XC    TWASALFX,TWASALFX                                                
         XC    TWAOFFFX,TWAOFFFX                                                
         XC    TWASALCD,TWASALCD                                                
FMT080   DS    0H                                                               
*                                                                               
*  LOOKUP CONTYPE RECORD TO GET FORMAT INFO                                     
*                                                                               
         LA    R6,RCONREC                                                       
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         USING RCTYREC,R5                                                       
         USING RCONREC,R6                                                       
         MVI   RCTYKTYP,RCTYKTYQ     REC TYPE                                   
         MVC   RCTYKREP,RCONKREP     REP CODE                                   
         MVC   RCTYKCTY,RCONTYPE     CON TYPE                                   
         DROP  R6,R5                                                            
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   FMT700                                                           
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
*                                                                               
         MVI   ELCODE,X'10'             FORMAT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   FMT700                                                           
         LR    R4,R6                                                            
         USING RCTYFEL,R4                                                       
         MVC   TWAPRFK,RCTYFPRC         SAVE OFF PROFILE BYTES                  
         MVC   TWAPRFW,RCTYFPRW                                                 
*                                                                               
         TM    RCTYFPRA,X'08'           CARE OF AGENCY OVERRIDE?                
         BNZ   FMT180                   YES                                     
*                                                                               
         BAS   RE,COAGY                 CHECK FOR CARE OF AGENCY                
         BNE   FMT180                   NOT A CARE OF AGENCY                    
*                                                                               
         OI    TWAFMTFL,X'08'           SET C/O AGENCY FLAG OVERRIDE            
         B     FMT260                   SKIP CONTYPE AGENCY OVERRIDES           
*                                                                               
FMT180   DS    0H                                                               
         TM    RCTYFA1S,X'80'           REPLACE AGY ADDRESS 1?                  
         BNO   FMT200                   NO - NEXT FIELD                         
         XC    TWAAGAD1,TWAAGAD1                                                
         MVI   HALF,C'G'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT200                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAAGAD1(0),3(R6)                                                
FMT200   DS    0H                                                               
         TM    RCTYFA2S,X'80'           REPLACE AGY ADDRESS 2?                  
         BNO   FMT230                   NO - NEXT FIELD                         
         XC    TWAAGAD2,TWAAGAD2                                                
         MVI   HALF,C'H'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT230                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAAGAD2(0),3(R6)                                                
FMT230   DS    0H                                                               
         TM    RCTYFA3S,X'80'           REPLACE AGY ADDRESS 3?                  
         BNO   FMT240                   NO - NEXT FIELD                         
         XC    TWAAGAD3,TWAAGAD3                                                
         MVI   HALF,C'I'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT240                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAAGAD3(0),3(R6)                                                
FMT240   DS    0H                                                               
         TM    RCTYFANS,X'80'           REPLACE AGY NAME?                       
         BNO   FMT250                   NO - NEXT FIELD                         
         XC    TWAAGNM2,TWAAGNM2                                                
         MVI   HALF,C'E'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT250                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAAGNM2(0),3(R6)                                                
FMT250   DS    0H                                                               
         TM    RCTYFABS,X'80'           REPLACE BUYER NAME?                     
         BNO   FMT260                   NO - NEXT FIELD                         
         OI    TWAFMTFL,X'80'           SET BUYER REPLACED FLAG                 
         XC    TWABUYER,TWABUYER                                                
         MVI   HALF,C'F'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT260                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWABUYER(0),3(R6)                                                
*                                                                               
FMT260   DS    0H                                                               
         TM    RCTYFRNS,X'80'           REPLACE REP NAME?                       
         BNO   FMT270                   NO - NEXT FIELD                         
         XC    TWAPRNAM,TWAPRNAM                                                
         MVI   HALF,C'A'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT270                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAPRNAM(0),3(R6)                                                
FMT270   DS    0H                                                               
         TM    RCTYFR1S,X'80'           REPLACE REP ADDRESS 1?                  
         BNO   FMT280                   NO - NEXT FIELD                         
         XC    TWAPRAD1,TWAPRAD1                                                
         MVI   HALF,C'B'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT280                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAPRAD1(0),3(R6)                                                
FMT280   DS    0H                                                               
         TM    RCTYFR2S,X'80'           REPLACE REP ADDRESS 2?                  
         BNO   FMT290                   NO - NEXT FIELD                         
         XC    TWAPRAD2,TWAPRAD2                                                
         MVI   HALF,C'C'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT290                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAPRAD2(0),3(R6)                                                
FMT290   DS    0H                                                               
         MVC   TWAPRREP,TWAREPNM        DEFAULT TEXT - ACTUAL REP NAME          
         TM    RCTYFRRS,X'80'           REPL REAL REP NAME FIELD?               
         BNO   FMT300                   NO - NEXT FIELD                         
         XC    TWAPRREP,TWAPRREP                                                
         MVI   HALF,C'D'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT300                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAPRREP(0),3(R6)                                                
FMT300   DS    0H                                                               
         DROP  R4                                                               
*                                                                               
*  HANDLE FORMAT OPTION BITS HERE                                               
*                                                                               
         TM    TWAPRFK,X'80'            OPTION #1                               
         BNO   FMT650                                                           
         MVC   TWASALNM,WPTPEXP         REPL SAL NAME W/PT PERS NAME            
         MVC   TWASALTL,WPTPPH#         REPL SAL PH# W/PT PERS PHONE            
         XC    TWASALFX,TWASALFX                                                
         XC    TWAOFFFX,TWAOFFFX                                                
         XC    TWASALCD,TWASALCD                                                
FMT650   DS    0H                       OPTION #2                               
         TM    TWAPRFK,X'40'            SHOW CONTYPE & DEVTYPE DESCR            
         BZ    FMT702                                                           
         GOTO1 =A(TYPEDESC),DMCB,(RC),RR=Y                                      
         B     FMT702                                                           
*                                                                               
FMT700   DS    0H                                                               
         BAS   RE,COAGY                 CHECK FOR CARE OF AGENCY                
         BNE   FMT702                   NOT A CARE OF AGENCY                    
*                                                                               
         OI    TWAFMTFL,X'08'           SET C/O AGENCY FLAG OVERRIDE            
*                                                                               
FMT702   DS    0H                                                               
         BAS   RE,REPLADDR              REPLACE AGY ADDRESS?                    
                                                                                
FMTYES   CR    RB,RB                                                            
         B     FMTX                                                             
FMTNO    DS    0H                                                               
         LTR   RB,RB                                                            
FMTX     DS    0H                                                               
         XIT1                                                                   
*----------------------------                                                   
TXTSEEK  DS    0H                                                               
         LR    R0,RE                                                            
         MVI   ELCODE,X'12'                                                     
         LA    R6,IOAREA                                                        
         BAS   RE,GETEL                                                         
TS010    BNE   TSNO                                                             
         CLC   2(1,R6),HALF                                                     
         BE    TSYES                                                            
         BAS   RE,NEXTEL                                                        
         B     TS010                                                            
*                                                                               
TSNO     SR    R1,R1                                                            
         CR    R1,RB                                                            
         B     *+6                                                              
TSYES    CR    R1,R1                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
*----------------------------                                                   
* CHECK ADV REC FOR REPLACEMENT AGY ADDRESS                                     
*----------------------------                                                   
REPLADDR NTR1                                                                   
         XC    KEY,KEY                                                          
K        USING RADVKEY,KEY                                                      
         MVI   K.RADVKTYP,X'08'                                                 
         MVC   K.RADVKADV,RCONKADV                                              
         MVC   K.RADVKREP,RCONKREP                                              
         DROP  K                                                                
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTOX (RFGETREC,VREPFACS),DMCB,KEY,AIO3,0,DUB                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FULL(1),RCONTYPE                                                 
         MVC   FULL+1(2),RCONKOFF                                               
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(X'20',AIO3),(3,FULL),0             
         CLI   12(R1),0            GOT IT?                                      
         BNE   FMTYES              NO ADDRESS                                   
*                                                                               
         ZICM  R6,13(R1),3         ELEMENT                                      
R        USING RADVAGEL,R6                                                      
         OI    TWAFMTFL,X'80'           SET BUYER REPLACED FLAG                 
         XC    TWABUYER,TWABUYER                                                
         XC    TWAAGAD1,TWAAGAD1                                                
         XC    TWAAGAD2,TWAAGAD2                                                
         XC    TWAAGAD3,TWAAGAD3                                                
         MVC   TWAAGAD1(34),R.RADVAGA1                                          
         MVC   TWAAGAD2(34),R.RADVAGA2                                          
         MVC   TWAAGAD3(36),R.RADVAGA3                                          
         B     FMTYES                                                           
         DROP  R                                                                
*                                                                               
*----------------------------                                                   
* ROUTINE READS AGENCY RECORD AND RETURNS CC EQUAL IF THE IN CARE OF            
*  FLAG IS ON                                                                   
*                                                                               
*  ROUTINE GETS AN AIO AREA IN WORKING STORAGE AND POINTS R6 AT IT              
*----------------------------                                                   
COAGY    NTR1  WORK=(R6,2000/8)                                                 
         XC    KEY,KEY                                                          
K        USING RAGYKEY,KEY                                                      
         MVI   K.RAGYKTYP,X'0A'                                                 
         MVC   K.RAGYKAGY,RCONKAGY                                              
         MVC   K.RAGYKAOF,RCONKAOF                                              
         MVC   K.RAGYKREP,RCONKREP                                              
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RAGYKEY),KEYSAVE                                           
         BNE   FMTNO                                                            
         GOTO1 VGETREC,DMCB,(R6)                                                
*                                                                               
R        USING RAGYREC,R6                                                       
         TM    R.RAGYFLAG,X'20'    CARE OF AGENCY?                              
         BZ    FMTNO               NO                                           
         B     FMTYES              YES                                          
         DROP  R                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*********************************************************************           
* BUILD AND PRINT CONTRACT HEADLINE                                             
*********************************************************************           
HEADLINE CSECT                                                                  
         NMOD1 0,**HEAD**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         TM    KPRTOPTS,KPRTOPFX   EASYLINK/FAX HAS OWN HEADER                  
         BZ    HL                                                               
         TM    PROFILES+CNTFAXHB,CNTFAXHA  PROF TO FAX ORIGINAL STYLE           
         BO    HL                          YES - SKIP FAXHL ROUTINE             
         B     FAXHL                                                            
                                                                                
HEADLINX XMOD1                                                                  
                                                                                
HL       DS    0H                                                               
         MVI   LNE,0                 LNE CTR                                    
         XC    MYP,MYP                                                          
         MVI   MYP-1,X'89'           SKIP TO NEW PG                             
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   MYP+10(33),TWAPRNAM   REP NAME                                   
         BAS   RE,PRINT                                                         
         MVC   MYP+10(20),TWAPRAD1   OFF ADDR LN 1                              
         BAS   RE,PRINT                                                         
         MVC   MYP+10(34),TWAPRAD2   OFF ADDR LN 2                              
         BAS   RE,PRINT                                                         
*                                                                               
         MVI   MYP-1,SPACE1          SPACE 1 LINE BEFORE PRINT                  
         BAS   RE,PRINT                                                         
*                                                                               
* IF PROFILE ON, THERE WILL BE TYPE DESCIPTIONS IN THESE FIELDS                 
*                                                                               
         MVC   MYP+53(L'TWACTDES),TWACTDES                                      
         BAS   RE,PRINT                                                         
         MVC   MYP+53(L'TWADTDES),TWADTDES                                      
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   MYP+5(4),RCONKADV     ADV CODE                                   
         MVC   MYP+13(20),TWAADVNM                                              
         GOTO1 DATCON,DMCB,(3,TODAY),(5,MYP+41)                                 
*                                                                               
         ZAP   DUB,=P'0'                                                        
         MVO   DUB+3(5),RCONKCON   K NUMBER                                     
         EDIT  (P5,DUB+3),(8,MYP+53)                                            
*                                                                               
         CLI   TWACOMBO,0          IF THIS IS A COMBO ORDER, CLEAR              
         BE    HL60                CONTRACT NUMBER                              
         MVC   MYP+53(8),MYSPACES                                               
*                                                                               
HL60     EQU   *                                                                
         GOTO1 =A(HDLINE2),DMCB,(RC),RR=Y                                       
         BAS   RE,PRINT                                                         
*                                                                               
*   A SECTION OF CODE WAS SLID OUT HERE TO RESTORE ADDRESSABILITY.              
*                                                                               
         GOTO1 =A(GETSAR),DMCB,(RC),RR=Y                                        
*                                                                               
*   A SECTION OF CODE WAS SLID OUT HERE TO RESTORE ADDRESSABILITY.              
*                                                                               
***>     BAS   RE,PRINT                                                         
*                                                                               
         MVC   MYP+5(3),TWASALCD     SALESMAN CODE                              
         MVC   MYP+13(20),TWASALNM   SALESMAN NAME                              
***>     BAS   RE,PRINT                                                         
*                                                                               
         TM    SOPTIONS,STAOPT7    STATION OPT TO PRINT ADV, AGY, TRAF?         
         BZ    HL130                                                            
         LA    R6,RCONREC          GET ADVERTISER                               
         USING RCONXXEL,R6                                                      
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   HL130                                                            
         MVC   MYP+41(L'RCONXAGY),RCONXAGY                                      
HL130    BAS   RE,PRINT                                                         
         DROP  R6                                                               
*                                                                               
         MVC   MYP+13(12),TWASALTL SAL TELEPHONE                                
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   MYP+13(33),TWAPRREP      REAL REP NAME FIELD                     
         BAS   RE,PRINT                                                         
*                                                                               
         TM    TWAFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    *+10                NO                                           
         MVC   MYP+46(20),TWAADVNM                                              
*                                                                               
         BAS   RE,PRINT                                                         
*                                                                               
         LA    RE,MYP+46                                                        
         TM    TWAFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    *+14                NO                                           
         MVC   0(04,RE),=C'C/O '                                                
         LA    RE,4(RE)                                                         
*                                                                               
         MVC   0(33,RE),TWAAGNM2   AGENCY NAME                                  
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   MYP+5(2),RCONKOFF     OFFICE                                     
         MVC   MYP+13(L'CONOFFN),CONOFFN                                        
         TM    TWAFMTFL,X'80'           BUYER REPLACED ?                        
         BO    HL135                    YES - SKIP PRINTING FLD LABEL           
         MVC   MYP+46(12),=C'MEDIA BUYER-'                                      
         MVC   MYP+59(20),TWABUYER                                              
         B     *+10                                                             
HL135    MVC   MYP+46(20),TWABUYER                                              
         BAS   RE,PRINT                                                         
*                                                                               
*    DISPLAY POINT PERSON NAME                                                  
*                                                                               
HL136    DS    0H                                                               
         TM    PROFILES+CNTPTPRB,CNTPTPRA                                       
         BZ    HL137A              IF OFF SKIP POINT PERSON                     
*                                                                               
         CLC   =C'C=',CONPRD       DID THEY TYPE PROD CODE                      
         BNE   HL137A              NO--DONE                                     
*                                                                               
         LA    R6,RCONREC          CONTRACT RECORD                              
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL            GET CONTRACT DESCRIPTION ELEMENT             
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONELEM,R6                                                      
         CLC   RCONPRD,=C'   '        PROD CODE OR PRODUCT LITERAL ?            
         BE    HL137A                 NO--DONE                                  
*                                                                               
         MVC   WORK(27),KEY        SAVE THE KEY                                 
         XC    KEY,KEY                                                          
*                                                                               
         LA    R2,KEY                                                           
PRD      USING RPRDREC,R2              PROD REC DSECT                           
         MVI   PRD.RPRDKTYP,X'09'      BUILD PROD REC KEY                       
         MVC   PRD.RPRDKADV,RCONKADV   ADVER(FROM CONTRACT REC KEY)             
         MVC   PRD.RPRDKPRD,RCONPRD    PROD CODE(FROM CONTRACT ELEMENT)         
         MVC   PRD.RPRDKREP,RCONKREP   REP CODE(FROM CONTRACT REC KEY)          
         DROP  R6,PRD                                                           
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RPRDNELM,R6                                                      
         CLI   RPRDNPNT,0          IS THERE A POINT PERSON CODE?                
         BE    HL137               NO--DONE                                     
*                                                                               
         XC    KEY,KEY             YES--BUILD POINT PERSON REC KEY              
         LA    R2,KEY                                                           
PTP      USING RPTPREC,R2             POINT PERSON RECORD                       
         MVI   PTP.RPTPKTYP,X'31'     REC TYPE                                  
         MVC   PTP.RPTPKREP,RCONKREP  REP CODE                                  
         MVC   PTP.RPTPKREC,RPRDNPNT  POINT PERSON CODE(FROM PROD ELEM)         
         DROP  R6,PTP                                                           
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VGETREC,DMCB,IOAREA    GET POINT PERSON RECORD                   
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RPTPELEM,R6                                                      
*                                                                               
         MVC   MYP+5(3),=C'PTP'                                                 
         MVC   MYP+13(20),RPTPNAME                                              
         DROP  R6                                                               
HL137    MVC   KEY(27),WORK           RESTORE KEY                               
*                                                                               
HL137A   MVC   MYP+46(34),TWAAGAD1   AGY ADDR                                   
         BAS   RE,PRINT                                                         
         MVC   MYP+46(36),TWAAGAD2                                              
         BAS   RE,PRINT                                                         
         MVC   MYP+46(36),TWAAGAD3                                              
         BAS   RE,PRINT                                                         
*                                                                               
         LA    R3,RCONDATE         DEFAULT TO DISPLAY NORMAL DATES              
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   HL140                                                            
         USING RCONRFEL,R6                                                      
         OC    RCONRFLT,RCONRFLT   HAVE REVISED DATE?                           
         BZ    HL140                                                            
         LA    R3,RCONRFLT         YES - USE IT                                 
         DROP  R6                                                               
HL140    GOTO1 DATCON,DMCB,(3,0(R3)),(5,MYP+13)                                 
         MVI   MYP+21,C'-'                                                      
         GOTO1 (RF),(R1),(3,3(R3)),(5,MYP+22)                                   
         EDIT  (1,RCONWKS),(2,MYP+33)                                           
         BAS   RE,PRINT                                                         
         MVI   MYP-1,SPACE3          SPACE 3                                    
         BAS   RE,PRINT                                                         
         MVI   MYP-1,SPACE3          SPACE 3                                    
         BAS   RE,PRINT                                                         
         B     HEADLINX                                                         
         EJECT                                                                  
*                                                                               
*********************************************************************           
* BUILD AND PRINT EASYLINK/FAX CONTRACT HEADLINE                                
* THIS CONTRACT HEADER CONTAINS FIELD IDENTIFIERS                               
* SINCE THIS WON'T BE PRINTED ON A SPOT CONFIRMATION FORM                       
*********************************************************************           
FAXHL    DS    0H                                                               
         MVI   LNE,0                 LNE CTR                                    
         XC    MYP,MYP                                                          
         MVI   MYP-1,X'89'           SKIP TO NEW PG                             
         BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP(23),=C'CONFIRMATION OF ORDER #'                              
         CLI   TWACOMBO,0          IF THIS IS A COMBO ORDER, DON'T              
         BE    FAXHL10             PRINT ORDER NUMBER                           
         MVC   MYP+24(7),=C'*COMBO*'                                            
         B     FAXHL20                                                          
                                                                                
FAXHL10  DS    0H                                                               
         ZAP   DUB,=P'0'                                                        
         MVO   DUB+3(5),RCONKCON   K NUMBER                                     
         EDIT  (P5,DUB+3),(8,MYP+24),ALIGN=LEFT                                 
                                                                                
FAXHL20  DS    0H                                                               
         MVC   MYP+34(5),=C'MOD #'                                              
         MVI   HALF,0                                                           
         MVC   HALF+1(1),RCONMOD   MOD NUMBER                                   
         CLI   RCONMOD,255                                                      
         BNE   *+8                                                              
         MVI   HALF,X'FF'                                                       
         EDIT  (2,HALF),(3,MYP+40),FLOAT=-,ALIGN=LEFT                           
                                                                                
         MVC   MYP+44(4),=C'DATE'                                               
         GOTO1 DATCON,DMCB,(3,TODAY),(5,MYP+49)                                 
         MVI   MYP-1,SPACE2                                                     
         BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP(3),=C'TO:'                                                   
         MVC   MYP+5(20),TWABUYER                                               
         MVC   MYP+40(33),TWAPRREP  REAL REP NAME FIELD                         
         BAS   RE,PRINT                                                         
         MVC   MYP+5(33),TWAAGNM2   AGENCY NAME                                 
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   MYP+5(34),TWAAGAD1  AGY ADDR                                     
         BAS   RE,PRINT                                                         
         MVC   MYP+5(36),TWAAGAD2                                               
         BAS   RE,PRINT                                                         
         MVC   MYP+5(36),TWAAGAD3                                               
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   MYP(3),=C'FM:'                                                   
         MVC   MYP+5(20),TWASALNM   SALESMAN                                    
         MVI   MYP+25,C'/'                                                      
         MVC   MYP+26(12),TWASALTL SAL TELEPHONE                                
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   MYP+5(33),TWAPRNAM  REP NAME                                     
         BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP+5(20),TWAPRAD1  OFF ADDR LN 1                                
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   MYP+5(34),TWAPRAD2  OFF ADDR LN 2                                
         BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP(3),=C'RE:'                                                   
         MVC   MYP+5(20),TWAADVNM ADV NAME                                      
         MVI   MYP+25,C'/'                                                      
                                                                                
         MVC   MYP+26(20),CONPRD   PRODUCT NAME                                 
         CLC   RCONPRD,MYSPACES                                                 
         BE    *+10                                                             
         MVC   MYP+26(20),TWAPRDNM                                              
*                                                                               
* FIND EASI ELEM                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
*                                                                               
* WILL JUST RESULT IN SPACES BEING MOVED FOR EASI CODES IN LATER                
* LINES - I WON'T HAVE TO KEEP CHECKING TO SEE IF THEY REALLY EXIST.            
         LA    R6,MYSPACES         DIRTY TRICK, BUT IT WILL WORK...             
         B     FAXHL30             SKIP PRINTING LABELS                         
         USING RCONIEL,R6                                                       
*                                                                               
         MVC   MYP+51(3),=C'ADV'                                                
         MVC   MYP+60(3),=C'PRD'                                                
         MVC   MYP+69(3),=C'EST'                                                
FAXHL30  MVC   MYP+64(4),RCONIPRD                                               
         MVC   MYP+55(4),RCONIADV                                               
         MVC   MYP+73(10),RCONXEST                                              
         OC    MYP+73(10),MYSPACES                                              
         CLC   MYP+73(10),MYSPACES                                              
         BNE   *+10                                                             
         MVC   MYP+73(4),RCONIEST                                               
         BAS   RE,PRINT                                                         
         DROP  R6                                                               
*                                                                               
         TM    SOPTIONS,STAOPT7    STATION OPT TO PRINT ADV, AGY, TRAF?         
         BZ    FAXHL135                                                         
         LA    R6,RCONREC          GET TRAFFIC NUMBER                           
         USING RCONXEL,R6                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   FAXHL133                                                         
         MVC   MYP+17(L'RCONTRF),RCONTRF                                        
         DROP  R6                                                               
                                                                                
FAXHL133 LA    R6,RCONREC          GET ADVERTISER, AGENCY                       
         USING RCONXXEL,R6                                                      
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   FAXHL135                                                         
         MVC   MYP+5(L'RCONXADV),RCONXADV                                       
         MVC   MYP+29(L'RCONXAGY),RCONXAGY                                      
         DROP  R6                                                               
                                                                                
FAXHL135 BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP(4),=C'STA:'                                                  
                                                                                
* IF THIS IS COMBINED STATION, PRINT AM AND FM CALL LETTERS INSTEAD             
*  OF COMBINED STATION CALL LETTERS                                             
***                                                                             
*** CHECK FOR COMBO USAGE (ON/OFF)                                              
***                                                                             
         CLI   USECOMBO,C'N'                                                    
         BNE   FAXHL140                                                         
                                                                                
         OC    SAVAM,SAVAM                                                      
         BZ    FAXHL140                                                         
         MVC   MYP+5(4),SAVAM                                                   
         LA    R4,MYP+9                                                         
         CLI   MYP+8,C' '                                                       
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         MVC   0(4,R4),=C'-AM/'                                                 
                                                                                
         MVC   MYP+13(4),SAVFM                                                  
         LA    R4,MYP+17                                                        
         CLI   MYP+16,C' '                                                      
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         MVC   0(3,R4),=C'-FM'                                                  
         B     FAXHL150                                                         
                                                                                
FAXHL140 DS    0H                                                               
         CLI   TWACOMBO,0          IF COMBO, PRINT ALL STATIONS                 
         BE    FAXHL150                                                         
         GOTO1 PRTCALL,DMCB,MYP+5                                               
         BAS   RE,PRINT                                                         
         B     FAXHL155                                                         
                                                                                
FAXHL150 DS    0H                                                               
         MVC   MYP+5(7),CONSTA     STATION                                      
         MVI   MYP+25,C'/'                                                      
         MVC   MYP+26(20),CONSTAM  MARKET                                       
         BAS   RE,PRINT                                                         
                                                                                
FAXHL155 DS    0H                                                               
         TM    PROFILES+CNTPRN1B,CNTPRN1A        PRINT AFFL?                    
         BZ    *+10                NO                                           
         MVC   MYP+5(3),TEMP       STATION AFFL                                 
         BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP(4),=C'FLT:'                                                  
         LA    R3,RCONDATE         DEFAULT TO DISPLAY NORMAL DATES              
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   FAXHL158                                                         
         USING RCONRFEL,R6                                                      
         OC    RCONRFLT,RCONRFLT   HAVE REVISED DATE?                           
         BZ    FAXHL158                                                         
         LA    R3,RCONRFLT         YES - USE IT                                 
         DROP  R6                                                               
FAXHL158 GOTO1 DATCON,DMCB,(3,0(R3)),(5,MYP+5)                                  
         MVI   MYP+13,C'-'                                                      
         GOTO1 (RF),(R1),(3,3(R3)),(5,MYP+14)                                   
         MVI   MYP-1,SPACE2                                                     
         BAS   RE,PRINT                                                         
                                                                                
         TM    PROFILES+CNTPRN2B,CNTPRN2A     PRINT SRV, BOOK & DEMO?           
         BZ    FAXHL180            NO                                           
                                                                                
         MVC   MYP(8),=C'RTG SVC:'                                              
         LA    RE,RTGTBL           DISPLAY RATING SERVICE                       
         CLC   RCONRTGS,0(RE)                                                   
         BE    *+18                                                             
         LA    RE,L'RTGTBL(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   *-18                                                             
         DC    H'0'                                                             
         MVC   MYP+10(3),0(RE)                                                  
                                                                                
* PRINT FIRST DEMO (OR PRIME DEMO) AND FIRST BOOK                               
                                                                                
         MVC   MYP+17(12),=C'TARGET DEMO:'                                      
         LA    R2,MYP+31                                                        
         L     R4,ASPULAR                                                       
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
                                                                                
         LA    R5,WORK2                                                         
         XC    WORK2(30),WORK2                                                  
                                                                                
         CLI   RCONKSTA+4,C' '     TV???                                        
         BE    FAXHL160                                                         
         CLI   RCONKSTA+4,C'T'     TV???                                        
         BE    FAXHL160                                                         
         CLI   RCONKSTA+4,C'L'     TV???                                        
         BE    FAXHL160                                                         
                                                                                
* RADIO                                                                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'        BOP ELEM                                     
         BAS   RE,GETEL                                                         
         BNE   FAXHL180            NO BOP, NO DEMO                              
         USING RCONBPEL,R6                                                      
         MVC   MYP+41(3),RCONBPMK  TSA/ADI/MSA                                  
         MVC   MYP+45(6),RCONBBKS                                               
*                                                                               
         CLC   RCONBBKS,MYSPACES   NOW SUFFIX BOOK W/BOOK TYPE                  
         BE    FAXHL159                                                         
         CLI   RCONBBKT,0                                                       
         BE    FAXHL159                                                         
         CLI   RCONBBKT,C' '                                                    
         BE    FAXHL159                                                         
         OC    MYP+45(6),MYSPACES                                               
         LA    RF,MYP+51                                                        
         BCTR  RF,0                                                             
         CLI   0(RF),C' '                                                       
         BE    *-6                                                              
         MVC   1(3,RF),=C'( )'                                                  
         MVC   2(1,RF),RCONBBKT                                                 
*                                                                               
FAXHL159 DS    0H                                                               
         CLI   RCONBPDM,X'FF'      VALIDATED BY DEMOVAL?                        
         BE    *+14                YES                                          
         MVC   MYP+31(8),RCONBPDM DISPLAY UN-VALIDATED (OLD) DEMO               
         B     FAXHL180                                                         
                                                                                
         MVI   DBSELMED,C'R'                                                    
         MVC   0(L'RCONBPDM-1,R5),RCONBPDM+1     DEMOS + ENDING ZERO            
         DROP  R6                                                               
                                                                                
         CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         GOTO1 DEMCON,DMCB,(1,(R5)),(9,(R2)),(0,DBLOCKD)                        
         B     FAXHL180                                                         
                                                                                
FAXHL160 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'        SAR ELEM                                     
         BAS   RE,GETEL                                                         
         BNE   FAXHL180            NO SAR, NO DEMO OR BOOKS                     
         USING RSAREL,R6                                                        
NEWSAR2  USING RSARXEL,R6          USE THE NEW ELEMENT DSECT ALSO               
                                                                                
* IF PRIME DEMO, USE IT, ELSE USE FIRST DEMO                                    
         MVI   DBSELMED,C'T'                                                    
         MVC   0(L'RSARDEM,R5),RSARDEM                                          
         LA    R3,6                                                             
         TM    0(R5),X'40'                                                      
         BNZ   *+16                FOUND PRIME                                  
         LA    R5,3(R5)            NEXT DEM                                     
         BCT   R3,*-12                                                          
         LA    R5,WORK2                                                         
                                                                                
         CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         GOTO1 DEMCON,DMCB,(1,(R5)),(9,(R2)),(0,DBLOCKD)                        
                                                                                
         CLC   RSARBKS(2),=C'DR'                                                
         BNE   FAXHL170                                                         
         MVC   MYP+41(2),=C'DR'                                                 
         B     FAXHL180                                                         
                                                                                
FAXHL170 DS    0H                                                               
* SET UP DUMMY FIELD FOR UNBOOK                                                 
         XC    DFLDH,DFLDH                                                      
         XC    DFLDD,DFLDD                                                      
         MVI   DFLDH,X'10'                                                      
*                                                                               
         TM    NEWSAR2.RSARXFLG,X'04'                                           
         BZ    FAXHL175                                                         
         DROP  R6,NEWSAR2                                                       
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'0B'                                                     
         BAS   RE,GETEL                                                         
         BNE   FAXHL172                                                         
         CLC   2(5,R6),=X'4040404040'                                           
         BE    FAXHL172                                                         
         MVC   MYP+41(5),2(R6)                                                  
         B     FAXHL180                                                         
FAXHL172 MVI   ELCODE,X'40'                                                     
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    2(2,R6),2(R6)                                                    
         BZ    FAXHL173                                                         
         MVC   MYP+41(5),2(R6)                                                  
         B     FAXHL180                                                         
FAXHL173 GOTO1 =V(UNBOOK),DMCB,(1,4(R6)),DFLDH,0,0,RR=YES                       
         MVC   MYP+41(L'DFLDD-2),DFLDD+2   IGNORE RATING SRVC                   
         B     FAXHL180                                                         
*                                                                               
FAXHL175 XC    DMCB+8(8),DMCB+8                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'0B'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+12                                                             
         ST    R6,DMCB+8                                                        
         MVI   DMCB+8,C'L'         YES-SET LABEL OPTION                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'        RESTORE R6 TO SAR ELEM                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSAREL,R6                                                        
         GOTO1 =V(UNBOOK),DMCB,(1,RSARBKS),DFLDH,,0,RR=YES                      
         MVC   MYP+41(L'DFLDD-2),DFLDD+2   IGNORE RATING SRVC                   
         DROP  R4,R6                                                            
         EJECT                                                                  
FAXHL180 DS    0H                                                               
         MVI   MYP-1,SPACE2                                                     
         BAS   RE,PRINT                                                         
         B     HEADLINX                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*        PPTP --- READ POINT PERSON RECORD FOR NAME                             
*********************************************************************           
PPTP     CSECT                                                                  
         NMOD1 0,**PPTP**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LA    R5,KEY                                                           
         USING RPTPKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RPTPKTYP,X'31'                                                   
         MVC   RPTPKREP,REPALPHA                                                
         MVC   RPTPKREC,TWAPDPTP   POINT PERSON CODE FROM PRD '02' ELEM         
         DROP  R5                                                               
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PPTPXIT                                                          
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   PPTPXIT                                                          
         USING RPTPELEM,R6                                                      
         MVC   WPTPEXP,RPTPNAME    POINT PERSON NAME                            
         MVC   WPTPPH#,RPTPFONE    POINT PERSON PHONE                           
         DROP  R6                                                               
*                                                                               
PPTPXIT  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*   CONTRACTS CONVERTED FROM THE KATZ SYSTEM CONTAIN THE ADVERTISER             
*       NAME AS A SINGLE ELEMENT IN THE CONTRACT RECORD.  THIS                  
*       MUST BE USED TO PRODUCE THE ADVERTISER NAME.                            
*                                                                               
KATZADNM NMOD1 0,**ADNM**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            A(PRINT LOCATION)                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'6F'        ADVERTISER ELEMENT                           
         BAS   RE,GETEL                                                         
         BE    KADV0040            FOUND                                        
         LTR   RB,RB               NOT FOUND - SET CC NOT ZERO                  
         B     KADV0200            EXIT                                         
KADV0040 EQU   *                                                                
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,KADV0240         MOVE BY LENGTH                               
         SR    R0,R0               SET CC ZERO                                  
KADV0200 EQU   *                                                                
         XIT1                                                                   
KADV0240 MVC   0(0,R2),2(R6)                                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
HDLINE2  NMOD1 0,**HDL2**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   HALF,0                                                           
         MVC   HALF+1(1),RCONMOD   MOD NUMBER                                   
         CLI   RCONMOD,255                                                      
         BNE   *+8                                                              
         MVI   HALF,X'FF'                                                       
         EDIT  (2,HALF),(3,MYP+66),FLOAT=-                                      
         BAS   RE,PRINT                                                         
*                                                                               
***>     MVI   MYP-1,SPACE2                                                     
         EDIT  (1,PG),(3,MYP+76)     PG                                         
         IC    RE,PG                                                            
         LA    RE,1(RE)                                                         
         STC   RE,PG                                                            
*                                                                               
         TM    SOPTIONS,STAOPT7    STATION OPT TO PRINT ADV, AGY, TRAF?         
         BZ    HDL220                                                           
         LA    R6,RCONREC          GET TRAFFIC NUMBER                           
         USING RCONXEL,R6                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   HDL210                                                           
         MVC   MYP+59(L'RCONTRF),RCONTRF                                        
         DROP  R6                                                               
*                                                                               
HDL210   LA    R6,RCONREC          GET ADVERTISER                               
         USING RCONXXEL,R6                                                      
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   HDL220                                                           
         MVC   MYP+5(L'RCONXADV),RCONXADV                                       
         DROP  R6                                                               
*                                                                               
HDL220   BAS   RE,PRINT                                                         
*                                                                               
*                                                                               
* GET PRODUCT NAME                                                              
         MVC   MYP+5(3),RCONPRD      PRODUCT CODE                               
         MVC   MYP+13(20),CONPRD                                                
         CLC   RCONPRD,MYSPACES                                                 
         BE    *+10                                                             
         MVC   MYP+13(20),TWAPRDNM                                              
*                                                                               
* IF THIS IS COMBINED STATION, PRINT AM AND FM CALL LETTERS INSTEAD             
*  OF COMBINED STATION CALL LETTERS                                             
***                                                                             
*** CHECK FOR COMBO USAGE (ON/OFF)                                              
***                                                                             
         CLI   USECOMBO,C'N'                                                    
         BNE   HDL230                                                           
***                                                                             
***                                                                             
***                                                                             
         OC    SAVAM,SAVAM                                                      
         BZ    HDL230                                                           
         MVC   MYP+36(4),SAVAM                                                  
         LA    R4,MYP+40                                                        
         CLI   MYP+39,C' '                                                      
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         MVC   0(4,R4),=C'-AM/'                                                 
         SPACE 1                                                                
         MVC   MYP+44(4),SAVFM                                                  
         LA    R4,MYP+48                                                        
         CLI   MYP+47,C' '                                                      
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         MVC   0(3,R4),=C'-FM'                                                  
         B     HDL240                                                           
         SPACE 1                                                                
HDL230   MVC   MYP+41(7),CONSTA    STATION                                      
         CLI   TWACOMBO,0          INDICATE IF THIS IS A COMBO ORDER            
         BE    HDL240                                                           
         MVC   MYP+41(7),=C'*COMBO*'                                            
*                                                                               
HDL240   MVC   MYP+53(20),CONSTAM                                               
         TM    TWASTAST,X'08'      08=NO CONTRACT TO STATION                    
         BO    *+10                                                             
         MVC   MYP+77(3),=C'***'   STARS INDICATE COPY TO STATION               
         BAS   RE,PRINT                                                         
*                                                                               
         TM    PROFILES+CNTPRN1B,CNTPRN1A        PRINT AFFL?                    
         BZ    *+10                NO                                           
         MVC   MYP+41(3),TEMP      STATION AFFL                                 
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   MYP+51(7),CONAGY                                                 
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
*   ROUTINE TO PRINT CFC COMMENT                                                
**********************************************************************          
PRTCFC   NMOD1 0,*PRTCFC*                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         BAS   RE,SETCOMBO         GET CORRECT K NUMBER                         
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCFCREC,R6                                                       
         MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,RCONKREP                                                
         MVC   RCFCKCON,FULL                                                    
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCFCKEY),KEYSAVE                                           
         BNE   EXXMOD                                                           
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         MVC   MYP+10(66),=66C'*'                                               
         MVC   MYP+29(28),=C' CONFIRMATION WITH COMMENTS '                      
         BAS   RE,PRINT                                                         
         TM    RCFCIFLG,X'80'                                                   
         BZ    *+10                                                             
         MVC   MYP+13(L'MGOYMSG),MGOYMSG                                        
         TM    RCFCIFLG,X'40'                                                   
         BZ    *+10                                                             
         MVC   MYP+13(L'MGONMSG),MGONMSG                                        
         MVI   MYP+10,C'*'                                                      
         MVI   MYP+75,C'*'                                                      
         BAS   RE,PRINT                                                         
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PRTCFC10 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PRTCFC20                                                         
         MVC   MYP+13(60),2(R6)                                                 
         MVI   MYP+10,C'*'                                                      
         MVI   MYP+75,C'*'                                                      
         BAS   RE,PRINT                                                         
         B     PRTCFC10                                                         
PRTCFC20 DS    0H                                                               
         MVC   MYP+10(66),=66C'*'                                               
         BAS   RE,PRINT                                                         
         B     EXXMOD                                                           
         DROP  R6                                                               
*                                                                               
*  IF COMBO ORDER, RETURNS LOWEST K NUMBER IN COMBO IN 'FULL' ELSE              
*  RETURNS K NUMBER                                                             
*                                                                               
SETCOMBO NTR1                                                                   
         MVC   FULL,RCONKCON                                                    
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'                                                     
         BAS   RE,GETEL                                                         
         BNE   SCMBX                                                            
*                                                                               
         ZIC   R3,1(R6)            17 ELEM LEN                                  
         SH    R3,=H'2'            - ELCODE & LEN                               
         SR    R2,R2                                                            
         D     R2,=F'9'            LEN OF MINI ELEM                             
         LTR   R2,R2               DIVISION SHOULD BE EVEN                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R5,7(R6)            FIRST K NUMBER IN 17 ELEM                    
SCMB20   DS    0H                                                               
         CLC   FULL,0(R5)          FULL VS. CURRENT K?                          
         BL    *+10                FULL IS LOWER - SKIP                         
         MVC   FULL,0(R5)          FULL IS HIGHER - REPLACE W/CURRENT           
         LA    R5,9(R5)            NEXT MINI ELEM IN 17 ELEM                    
         BCT   R3,SCMB20                                                        
*                                                                               
SCMBX    DS    0H                                                               
         B     EXXMOD                                                           
*  ++INCLUDE RECFCMSG                                                           
       ++INCLUDE RECFCMSG                                                       
         PRINT ON                                                               
         LTORG                                                                  
**********************************************************************          
*   ROUTINE TO PROCESS BOP & SAR INFO                                           
**********************************************************************          
GETSAR   NMOD1 0,*GETSAR*                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         TM    PROFILES+CNTPRN2B,CNTPRN2A     PRINT SRV, BOOK & DEMO?           
         BZ    GS90                NO                                           
*                                                                               
         LA    RE,RTGTBL           DISPLAY RATING SERVICE                       
         CLC   RCONRTGS,0(RE)                                                   
         BE    *+18                                                             
         LA    RE,L'RTGTBL(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   *-18                                                             
         DC    H'0'                                                             
         MVC   MYP+59(3),0(RE)                                                  
*                                                                               
* PRINT FIRST DEMO (OR PRIME DEMO) AND FIRST BOOK                               
         LA    R2,MYP+63                                                        
         L     R4,ASPULAR                                                       
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
*                                                                               
         LA    R5,WORK2                                                         
         XC    WORK2(30),WORK2                                                  
*                                                                               
         CLI   RCONKSTA+4,C' '     TV???                                        
         BE    GS70                                                             
         CLI   RCONKSTA+4,C'T'     TV???                                        
         BE    GS70                                                             
         CLI   RCONKSTA+4,C'L'     TV???                                        
         BE    GS70                                                             
*                                                                               
* RADIO                                                                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'        BOP ELEM                                     
         BAS   RE,GETEL                                                         
         BNE   GS90                NO BOP, NO DEMO                              
         USING RCONBPEL,R6                                                      
         MVC   MYP+73(3),RCONBPMK  TSA/ADI/MSA                                  
         MVC   MYP+77(6),RCONBBKS  BOOK                                         
*                                                                               
         CLC   RCONBBKS,MYSPACES   NOW SUFFIX BOOK W/BOOK TYPE                  
         BE    GS50                                                             
         CLI   RCONBBKT,0                                                       
         BE    GS50                                                             
         CLI   RCONBBKT,C' '                                                    
         BE    GS50                                                             
         OC    MYP+77(6),MYSPACES                                               
         LA    RF,MYP+83                                                        
         BCTR  RF,0                                                             
         CLI   0(RF),C' '                                                       
         BE    *-6                                                              
         MVC   1(3,RF),=C'( )'                                                  
         MVC   2(1,RF),RCONBBKT                                                 
*                                                                               
GS50     DS    0H                                                               
         CLI   RCONBPDM,X'FF'      VALIDATED BY DEMOVAL?                        
         BE    *+14                YES                                          
         MVC   MYP+63(8),RCONBPDM DISPLAY UN-VALIDATED (OLD) DEMO               
         B     GS90                                                             
*                                                                               
         MVI   DBSELMED,C'R'                                                    
         MVC   0(L'RCONBPDM-1,R5),RCONBPDM+1     DEMOS + ENDING ZERO            
         DROP  R6                                                               
*                                                                               
         CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         GOTO1 DEMCON,DMCB,(1,(R5)),(9,(R2)),(0,DBLOCKD)                        
         B     GS90                                                             
*                                                                               
* TV                                                                            
GS70     DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'        SAR ELEM                                     
         BAS   RE,GETEL                                                         
         BNE   GS90                NO SAR, NO DEMO OR BOOKS                     
         USING RSAREL,R6                                                        
NEWSAR   USING RSARXEL,R6          USE THE NEW ELEMENT DSECT ALSO               
*                                                                               
* IF PRIME DEMO, USE IT, ELSE USE FIRST DEMO                                    
         MVI   DBSELMED,C'T'                                                    
         MVC   0(L'RSARDEM,R5),RSARDEM                                          
         LA    R3,6                                                             
         TM    0(R5),X'40'                                                      
         BNZ   *+16                FOUND PRIME                                  
         LA    R5,3(R5)            NEXT DEM                                     
         BCT   R3,*-12                                                          
         LA    R5,WORK2                                                         
*                                                                               
         CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         GOTO1 DEMCON,DMCB,(1,(R5)),(9,(R2)),(0,DBLOCKD)                        
*                                                                               
         CLC   RSARBKS(2),=C'DR'                                                
         BNE   GS80                                                             
         MVC   MYP+73(2),=C'DR'                                                 
         B     GS90                                                             
         SPACE 1                                                                
GS80     DS    0H                                                               
* SET UP DUMMY FIELD FOR UNBOOK                                                 
         XC    DFLDH,DFLDH                                                      
         XC    DFLDD,DFLDD                                                      
         MVI   DFLDH,X'10'                                                      
*                                                                               
         CLI   NEWSAR.RSARXLEN,RSARXLTH                                         
         BL    GS85                CHECK IF EXPANDED SAR ELEMENT                
         TM    NEWSAR.RSARXFLG,X'04'                                            
         BZ    GS85                                                             
         DROP  R6,NEWSAR                                                        
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'0B'                                                     
         BAS   RE,GETEL                                                         
         BNE   GS82                                                             
         CLC   2(5,R6),=X'4040404040'                                           
         BE    GS82                                                             
         MVC   MYP+73(5),2(R6)                                                  
         B     GS90                                                             
GS82     MVI   ELCODE,X'40'                                                     
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    2(2,R6),2(R6)                                                    
         BZ    GS83                                                             
         MVC   MYP+73(5),2(R6)                                                  
         B     GS90                                                             
GS83     GOTO1 =V(UNBOOK),DMCB,(1,4(R6)),DFLDH,0,0,RR=YES                       
         MVC   MYP+73(L'DFLDD-2),DFLDD+2   IGNORE RATING SRVC                   
         B     GS90                                                             
*                                                                               
GS85     XC    DMCB+8(8),DMCB+8                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'0B'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+12                                                             
         ST    R6,DMCB+8                                                        
         MVI   DMCB+8,C'L'         YES-SET LABEL OPTION                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'        RESTORE R6 TO SAR ELEM                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSAREL,R6                                                        
         GOTO1 =V(UNBOOK),DMCB,(1,RSARBKS),DFLDH,,0,RR=YES                      
         MVC   MYP+73(L'DFLDD-2),DFLDD+2   IGNORE RATING SRVC                   
         DROP  R4,R6                                                            
*                                                                               
GS90     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'237RECNT17   06/12/12'                                      
         END                                                                    
