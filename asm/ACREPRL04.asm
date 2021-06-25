*          DATA SET ACREPRL04  AT LEVEL 082 AS OF 03/02/21                      
*PROCESS USING(WARN(15))                                                        
*PHASE ACRL04C                                                                  
*&&UK                                                                           
*INCLUDE BDESND                                                                 
*&&                                                                             
*INCLUDE USSIO                                                                  
*INCLUDE DLFLD                                                                  
*&&US                                                                           
*INCLUDE XTSOUSR                                                                
*&&                                                                             
*                                                                               
* PID  LVl DATE    COMMENTS                                                     
* ---------------------------------                                             
* SMAN 027 10NOV11 PR000122 Estimate Phase 1                                    
* SMAN 028 21NOV11 BR45424L Insert space between product code and name          
* SMAN 029 16DEC11 PR002242 UK/US merge                                         
* YNGX     30NOV11 PR002442 NEW ROUTINE EDSTATY                                 
* SMAN 030 08FEB12 PR002625 Estimate currency keywords                          
* SMAN             Fixes from DCUR                                              
* JFOS 052 19AUG13 US fix for download headings                                 
* JFOS 054 16JAN14 DSPCA528 Send download reports to BDE server via MQ          
* JFOS 055 02APR14          Relink for fix to BDESND                            
* JFOS 056 03APR14          Flip BDE common name to l/case if '+' set           
* JFOS 057 25APR14 DSPCA233 Fix bug displaying long narrative                   
* YNGX 058 23SEP14 PCA01185 Send download reports to USS server                 
* YNGX 059 30OCT14 <PCA01322> NEW KEYWORDS AGEFAG and REFSER                    
* YNGX 060 05JAN15 <PCA01482> INCORRECT TRUNCATION IN USS PROCESS               
* JFOS 061 11FEB15 <PCA01569> EXR: test COLXRATE to skip                        
* YNGX 062 25MAR15 <OT82993L> INCLUDE NEW DDUSSIO                               
* JFOS 063 09NOV15 MERGE WITH US VERSION                                        
* YNGX     18FEB16 <PCA02307> REMOVE CURRENCY CODE FOR KEYWORD ITMUNT           
* YNGX 064 21MAR16 <PCA02359> MERGE WITH US VERSION                             
* SGAV 068 30NOV17 <SPEC14476> Scribe does not report 0 hour timesheets         
* GHOA 069 03AUG18 SPEC26412  EDIHUB via MQ message                             
* JSAY 070 16OCT18 SPEC28551  Report on Credit Card Payment method              
* CPAT 076 21sep18 SPEC27959  Scribe BILTYC,BOTH only prints name               
* JSAY 077 23Jun19 SPEC33734  Ability to request a person scribe report         
*                             based on timesheet APPROVED date.                 
* JSAY 077 23Jun19 SPEC33735  Ability to report the date an Aura                
*                             timesheet was submitted.                          
* GHOA 078 28Jun19 SPEC33366  Connect Billing Report for CSI                    
* VGUP 079 02APR19 SPEC-43802 Relink due to new ACREPRLWRK                      
* GHOA 080 22Jun20 SPEC37360  Exclude employees with no hours                   
* MPEN 081 14Sep20 SPEC-49867 Fix for EDTPID on DDS pid                         
* GHOA 082 09Sep20 SPEC-44169 Expand Narrative for PAY Scribe                   
*                                                                               
ACRL04   CSECT                                                                  
         TITLE 'Printing of report'                                             
         PRINT NOGEN                                                            
***********************************************************************         
*  PRINT THE REPORT                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING ACWORKD,RC                                                       
         USING ACRLD,RA                                                         
         USING FMTRECD,R5                                                       
CUR      USING RECXD,RECXSRT                                                    
PRV      USING RECXD,PRVXSRT                                                    
*                                                                               
PRTREP   NMOD1 0,**PRTRE*,R9,R8                                                 
         L     RC,RLBASEC                                                       
         CLI   MODE,RUNFRST                                                     
         BNE   PRTREP05                                                         
         TM    DDLNKIND,DDLNKINT                                                
         BO    PRTRUN10                                                         
         MVC   ADLFLD,=A(DATASET)           Hook down-load with routine         
         TM    DDLNKIND,DDLNKON+DDLNKTST    Are we using DDLINK                 
         BZ    *+10                                                             
         MVC   ADLFLD,=A(WORKOUT)  If DDLINK then hook with routine             
         MVC   REPORT,=A(REPORTER)                                              
         MVC   HEADHOOK,=A(BXHOOK)                                              
         MVC   ABTYPTAB,=A(BTYPTAB)                                             
                                                                                
PRTRUN10 GOTO1 =A(LANGSOFT)                                                     
         B     RPTXIT                                                           
*                                                                               
         USING ACRL2D,R7                                                        
PRTREP05 L     R7,AACRL2D                                                       
*&&UK                                                                           
         TM    DWNOPT3,DWNTBDE     Test BDE transmission request                
         BZ    *+10                                                             
         MVC   ADLFLD,=A(DATABDE)  Hook download with BDE routine               
*&&                                                                             
*&&UK*&& TM    DWNOPT3,DWNTUSS+DWNTEDI Test USS/EDIHUB transmission req         
*&&US*&& TM    DWNOPT3,DWNTEDI     Test EDIHUB transmission req                 
         BZ    *+10                                                             
         MVC   ADLFLD,=A(DATAUSS)  Hook download with USS routine               
*                                                                               
         XC    ALSORT,ALSORT                                                    
         MVI   NPIDS,0             INITIALIZE                                   
         MVI   XP#LINES,0                                                       
         MVI   BLDACT,0                                                         
         MVC   PAGE,=H'01'                                                      
         ZAP   DTLCOUNT,=P'0'                                                   
         MVI   PRTCNTRL,PRTSOR     START OF REPORT                              
         XC    FOOT#,FOOT#         NUMBER OF PUT LINES TO PRINT                 
         L     RE,ABUDWRK          CLEAR OUT BUDGET WORK AREA                   
         LH    RF,SRTRECLN                                                      
         XCEFL                                                                  
*                                                                               
         L     RE,ASRTWRK          CLEAR OUT SORT RECORD AREA                   
         LH    RF,SRTRECLN                                                      
         XCEFL                                                                  
*                                                                               
*&&US*&& TM    DWNOPT1,DWNGO       Are we down-loading ?                        
*&&US*&& BO    PRTREP06            Yes, so no need to PQIDX                     
         TM    PIXOPT1,PIXGO       Print queue index is on ?                    
         BZ    PRTREP06                                                         
         GOTO1 =A(PIXLOAD),PIXINIT                                              
*                                                                               
PRTREP06 TM    DWNOPT1,DWNGO       DOWN-LOADING?                                
         BZ    PRTREP10                                                         
         GOTO1 =A(DWNLOAD),DWNINIT                                              
*                                                                               
PRTREP10 TM    SORTSW,SORTDATA     ANY SORT DATA?                               
         BZ    PRTREP12                                                         
         GOTO1 ADSORTER,DMCB,=C'GET'                                            
         L     R6,DMCB+4                                                        
         B     PRTREP14                                                         
*                                                                               
PRTREP12 TM    TSARSW,TSARDATA     TSAR DATA?                                   
         BZ    PRTREP94                                                         
*        DC    H'00'               HAD TO BE SORT OR TSAR                       
*                                                                               
         USING RECXD,R2                                                         
         GOTO1 ATSAR,DMCB,=C'GET'                                               
         L     R6,DMCB+4                                                        
PRTREP14 ST    R6,ALSORT           ADDRESS OF LAST SORT                         
         LTR   R2,R6                                                            
         BZ    PRTREP38                                                         
         AH    R2,SRTKEYXT                                                      
         SR    R5,R5                                                            
         ICM   R5,1,RECRPT#        GET REPORT NUMBER                            
         BNZ   *+6                                                              
         DC    H'00'               REPORT ZERO IS INVALID                       
*                                                                               
         BCTR  R5,0                                                             
         MHI   R5,FMTLNQ                                                        
         A     R5,AFORMATS         POINT TO FORMAT SPECS                        
*                                                                               
         USING COLD,R3                                                          
         CLI   FMT#ACMS,0          Any accumlators ?                            
         BE    PRTREP17            No, so ok as is                              
         TM    FMTPOPT1,RPFIACT    Print inactive accounts?                     
         BO    PRTREP17            Yes                                          
         TM    RECIND,RECKEEP      Keep no matter what ?                        
         BO    PRTREP17            Yes                                          
*                                                                               
         L     R3,FMTCOL           Check for all zero type out of tsar          
         SR    R0,R0                                                            
         IC    R0,FMT#COLS                                                      
PRTREP15 LR    R4,R6                                                            
         AH    R4,COLDSP           Point to accum.                              
         TM    FMTSW,FMTFRATE      Do we need to loose the rates ?              
         BO    PRTRP15B            Yes, so don't check rate amounts             
         TM    COLIND2,COLRATE     Rate amount ?                                
         BO    PRTRP15C            Yes                                          
                                                                                
PRTRP15B DS    0H                                                               
*&&UK*&& TM    COLIND6,COLXRATE    If exchange rate...                          
*&&UK*&& BO    PRTREP16            don't treat as monetary amount here          
                                                                                
         TM    COLFLAGS,COLAMT     Regular amount ?                             
         BZ    PRTREP16            No                                           
                                                                                
PRTRP15C OC    0(PKLEN,R4),0(R4)                                                
         BZ    PRTREP16                                                         
*&&US                              IF TIME ACCOUNT & ZERO HOURS, THEN           
         CLI   QUNIT,C'1'          PRINT IT FOR ZERO HOUR TIMESHEETS            
         BNE   PRTRP15F                                                         
         CLI   QLEDGER,C'R'                                                     
         BNE   PRTRP15F                                                         
         TM    FMTPOPT3,RPFSZERO   SHOW ZERO HR TIMESHEETS                      
         BO    PRTREP17                                                         
*                                                                               
PRTRP15F CP    0(PKLEN,R4),PKZERO  Check to see if all cols are zero            
         BNE   PRTREP17            Have data keep record.                       
*&&                                IF TIME ACCOUNT & ZERO HOURS, THEN           
*                                                                               
PRTREP16 LA    R3,COLLNQ(,R3)      Try next column                              
         BCT   R0,PRTREP15                                                      
*&&US*&& TM    FMTROPT6,RPFZPST    Print $0 transactions?                       
*&&US*&& BO    PRTREP17                                                         
         B     PRTREP10            Get next sort record                         
         DROP  R3                                                               
*                                                                               
PRTREP17 TM    UPSI,UPSISRTO       DUMP OUT SORT RECORD                         
         BZ    PRTREP20                                                         
         LH    R0,SRTRECLN                                                      
         GOTO1 PRNTBL,DMCB,=C'SORT REC',(R6),C'DUMP',(R0),=CL2'2D'              
*                                                                               
PRTREP20 TM    FMTSW,FMTRANK+FMTRKRTE  ARE WE RANKING?                          
         BZ    PRTREP22                                                         
         LR    R4,R6                   USE R4 TO POINT TO SORT REC.             
         AH    R4,FMTRNKBY             POINT TO RANK AREA IN KEY                
         XC    0(PKLEN+2,R4),0(R4)     CLEAR OR RECTOT WON'T WORK RIGHT         
*                                      IF YOU ARE RANKING                       
         USING COLD,R3                                                          
PRTREP22 TM    FMTSW,FMTFRATE      Do we need to loose the rates ?              
         BZ    PRTREP30            No                                           
         TM    RECIND,RECTOTS      Is this special total record ?               
         BO    PRTREP30            Yes                                          
*                                                                               
         L     R3,FMTCOL           A(COLUMN TABLE)                              
         SR    R0,R0                                                            
         IC    R0,FMT#COLS         NUMBER OF COLUMNS TO LOOK AT                 
*                                  R4 = BEGINING OF ACCUM LINE                  
PRTREP24 TM    COLIND2,COLRCALC    RATE TYPE CALCULATION                        
         BZ    PRTREP25                                                         
         LR    R4,R6                                                            
         AH    R4,SRTDATLN         POINT TO ACCUMS                              
         GOTO1 EVAL,DMCB,(R3),(R4),(R6)                                         
*                                  R4 = BEGINING OF ACCUM LINE                  
         AH    R4,COLACM           R4 = WHERE ANSWER GOES                       
         MVC   0(PKLEN,R4),ANSWER                                               
PRTREP25 LA    R3,COLLNQ(,R3)                                                   
         BCT   R0,PRTREP24                                                      
*                                                                               
         L     R3,FMTCOL           BASE OF COL TABLE                            
         IC    R0,FMT#COLS         NUMBER OF COLS                               
PRTREP26 TM    COLIND2,COLRATE                                                  
         BZ    PRTREP28                                                         
         LR    R4,R6                                                            
         AH    R4,COLDSP                                                        
         XC    0(PKLEN,R4),0(R4)   CLEAR RATE                                   
PRTREP28 LA    R3,COLLNQ(,R3)                                                   
         BCT   R0,PRTREP26                                                      
         DROP  R2,R3                                                            
*                                                                               
PRTREP30 L     RF,ASRTWRK                                                       
         OC    0(L'FMTCODE+1,RF),0(RF) DO I HAVE ONE SAVED                      
         BNZ   PRTREP38                BRANCH IF I DO.                          
*                                                                               
PRTREP35 L     RE,ASRTWRK          A(DESTINATION), SORT WORK                    
         LR    R0,R6               A(SOURCE), SORT RECORD                       
         LH    R1,SRTRECLN         L'SOURCE                                     
         LR    RF,R1               L'DESTINATION                                
         MVCL  RE,R0                                                            
         B     PRTREP10            GET NEXT RECORD FOR SORT                     
*                                                                               
PRTREP38 L     RE,ASRTWRK          A(SRTWRK)                                    
         LR    RF,RE                                                            
         OC    ALSORT,ALSORT                                                    
         BNZ   PRTREP39                                                         
         OC    0(L'FMTCODE+1,RF),0(RF)   DO I HAVE ONE SAVED                    
         BZ    PRTREP94                  BRANCH IF I DON'T                      
*                                                                               
PRTREP39 AH    RF,SRTKEYXT         POINT TO SORT TYPE IN RECORD                 
         MVC   RECXSRT,0(RF)       SAVE RECORD TYPE (0,1,2, ETC...)             
         SR    R5,R5                                                            
         ICM   R5,1,CUR.RECRPT#                                                 
         BNZ   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         BCTR  R5,0                                                             
         MHI   R5,FMTLNQ                                                        
         A     R5,AFORMATS         POINT TO REPORT SPECS                        
*                                                                               
         LTR   R0,R6               END OF SORT RECORDS?                         
         BZ    PRTREP54            NO MORE RECORDS, SO PRINT LAST ONE           
         CLI   CUR.RECTYPE,RECVPCT VERTICAL % RECORD?                           
         BNE   PRTREP51            NO                                           
*                                                                               
PRTREP40 SR    R2,R2               VERTICAL % LEVEL                             
         ICM   R2,1,CUR.RECLVL     GET ROW LEVEL THAT TOTAL IS FOR              
         BNZ   *+6                                                              
         DC    H'0'                CAN'T TOTAL ON A NEGATIVE ROW NUMBER         
*                                                                               
         BCTR  R2,0                ADDJUST BY ONE                               
         MHI   R2,ROWLNQ           POINT TO ROW LEVEL TOTAL                     
         A     R2,FMTROW           ADD BASE OF ROW TABLE                        
*                                                                               
         USING ROWD,R2                                                          
         L     R4,ROWVPACM         POINT AT V% ACCUM LINE ROW TOTALS            
         LR    R3,RE               POINT TO ASRTWRK                             
         AH    R3,SRTDATLN         POINT TO ACCUMULATORS (SORT REC)             
         SR    R0,R0                                                            
         IC    R0,FMT#ACMS         NUMBER OF ACCUM TO ADD TOGETHER              
         MVC   0(PKLEN,R4),0(R3)   FILL IN ACCUMS                               
         LA    R3,PKLEN(,R3)       BUMP TO NEXT ACCUM                           
         LA    R4,PKLEN(,R4)       BUMP TO NEXT ACCUM                           
         BCT   R0,*-14             DECRIMENT BY ONE                             
*                                                                               
         USING COLD,R3                                                          
         IC    R0,FMT#COLS         NUMBER OF COLUMNS TO LOOK AT                 
         L     R3,FMTCOL           A(COLUMN TABLE)                              
         L     R4,ROWVPACM         RE-LOAD VERT% TOTAL LINE                     
PRTREP42 TM    COLFLAGS,COLCALC    PERFORE COLUMN CALCS ON VERT TOTALS          
         BZ    PRTREP45                                                         
         TM    COLOPT2,COLVPCT     IS THIS A VERT% ITSELF                       
         BO    PRTREP45            AVOID THESE                                  
         CLC   COLDD#,=AL2(AC#RSCME)                                            
         BE    PRTREP45                 YES SO SKIP FOR V%                      
*                                                                               
         GOTO1 EVAL,DMCB,(R3),(R4),0                                            
         LR    R2,R4               R4 = BEGINING OF ACCUM LINE                  
         AH    R2,COLACM           R6 = WHERE ANSWER GOES                       
         MVC   0(PKLEN,R2),ANSWER                                               
PRTREP45 LA    R3,COLLNQ(,R3)                                                   
         BCT   R0,PRTREP42                                                      
         B     PRTREP35            NEXT RECORD, PROMOTE A(SORT REC)             
         DROP  R2,R3                                                            
*                                                                               
         USING RECXD,R2                                                         
PRTREP51 LH    R1,SRTKEYLN         Load length of SORTKEY                       
         LR    R2,R0                                                            
         AH    R2,SRTKEYXT                                                      
         CLI   CUR.RECTYPE,RECSTMS Time sheet saved record ?                    
         BE    PRTREP53                                                         
         TM    RECIND,RECMRGE      Merge account level record                   
         BO    PRTREP52                                                         
         TM    FMTIND,FMTREVSG     SPECIAL EDIT                                 
         BZ    PRTREP53            NO, SO DON'T ADJUST SORTKEY LENGTH           
         DROP  R2                                                               
*                                                                               
PRTREP52 CLI   CUR.RECTYPE,RECNORM                                              
         BL    PRTREP53                                                         
         SHI   R1,RECXDQ           DON'T COMPARE RECTYPE                        
         TM    DDLNKIND,DDLNKGBL   GLobal RLP request                           
         BZ    PRTREP53                                                         
         AHI   R1,L'RECOFC         Compare for office change                    
*                                                                               
PRTREP53 LR    RF,R1               R1 = LENGTH TO COMPARE FOR                   
         CLCL  RE,R0                                                            
         BE    PRTREP55            SAME KEY                                     
         BAS   RE,MERGE            TRY TO MERGE FURTHER                         
         BE    PRTREP55            NOT SAME KEY, BUT MERGED ANYWAY              
*                                                                               
PRTREP54 OI    FMTIND,FMTNOREV                                                  
         CLI   CUR.RECTYPE,RECREV  SPECIAL EDIT (REVERSE SIGN)                  
         BNE   PRTREP80                                                         
         NI    FMTIND,TURNOFF-FMTNOREV                                          
         B     PRTREP80            NOT SAME KEY                                 
***********************************************************************         
*  MERGER RECORDS WITH DUPLICATE KEYS                                 *         
***********************************************************************         
         SPACE 1                                                                
PRTREP55 L     R4,ASRTWRK          POINT TO PREVIOUS REC                        
         LR    RE,R6               POINT TO CURRENT  REC                        
         SR    R0,R0                                                            
         ICM   R0,1,FMT#ACMS       NUMBER OF ACCUM TO ADD TOGETHER              
         BZ    PRTREP10                                                         
         TM    FMTFLAG,FMTBYCOL                                                 
         BZ    PRTREP70                                                         
*                                                                               
         USING COLD,R3                                                          
         IC    R0,FMT#COLS         NUMBER OF COLUMNS TO LOOK AT                 
         L     R3,FMTCOL           A(COLUMN TABLE)                              
         LR    RF,R4               SAVE R4 IN RF                                
PRTREP60 TM    COLFLAGS,COLAMT                                                  
         BZ    PRTREP64                                                         
         AH    R4,COLDSP                                                        
         AH    RE,COLDSP                                                        
         TM    COLIND3,COLNASRT    Merge column out of sort ?                   
         BZ    PRTREP61            Yes, merge                                   
         CP    0(PKLEN,R4),PKZERO       No but find one with a amount           
         BNE   PRTREP62                 Already ok so don't add                 
         CP    0(PKLEN,R4),0(PKLEN,RE)  Does other one have a value ?           
         BE    PRTREP62                 They are the same, don't add            
*                                       Replace with new constant value         
PRTREP61 AP    0(PKLEN,R4),0(PKLEN,RE)                                          
*                                                                               
PRTREP62 LR    R4,RF               RELOAD A(SRTWRK)                             
         LR    RE,R6               RELOAD PREVIOUS RECORD ADDRESS               
*                                                                               
PRTREP64 LA    R3,COLLNQ(,R3)                                                   
         BCT   R0,PRTREP60         LOOP FOR R0 ACCUMULATORS                     
         B     PRTREP10            AND GET NEXT REC OUT OF SORTER               
         DROP  R3                                                               
*                                                                               
PRTREP70 AH    R4,SRTDATLN         POINT TO ACCUMS                              
         AH    RE,SRTDATLN         POINT TO ACCUMS                              
*                                                                               
PRTREP72 AP    0(PKLEN,R4),0(PKLEN,RE)                                          
         LA    R4,PKLEN(,R4)       BUMP TO NEXT ACCUM IN PREV REC               
         LA    RE,PKLEN(,RE)       BUMP TO NEXT ACCUM IN CURR REC               
         BCT   R0,PRTREP72         LOOP FOR R0 ACCUMULATORS                     
         B     PRTREP10            AND GET NEXT REC OUT OF SORTER               
*                                                                               
         USING RECXD,R4                                                         
PRTREP80 L     R4,ASRTWRK                                                       
         AH    R4,SRTKEYXT                                                      
         MVC   LMTSECOF,RECOFC     Set security office                          
         TM    FCIND,FCISORT       Curreny sort ?                               
         BZ    PRTREP81                                                         
         MVC   CURFCCDE,RECFCUR    YES SO GET FOREIGN CURRENCY PARMS            
         GOTO1 GETFCUR                                                          
         DROP  R4                                                               
*                                                                               
         USING COLD,R3                                                          
PRTREP81 L     R4,ASRTWRK                                                       
         AH    R4,SRTDATLN                                                      
         L     R3,FMTCOL           POINT TO COLUMN INFO                         
         SR    R0,R0                                                            
         IC    R0,FMT#COLS         NUMBER OF COLUMNS TO WORK ON                 
PRTREP82 TM    COLOPT,COLFCAMT                                                  
         BZ    PRTREP83                                                         
         TM    COLOPT,COLFCFLT                                                  
         BO    PRTREP83                                                         
         MVC   COLFCGRP,CURFCCDE+3                                              
*&&DO                                                                           
*                                                                               
*  NOT SURE BUT I THINK WE NEED TO ADD THIS CODE FOR UK                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,FMTRNDTY         GET ROUND FACTOR                             
         CLI   FMTRNDTY,PENNY                                                   
         BE    PRTREP83            NONE                                         
         LA    R1,2(,R1)           ADJUST, ROUND FACTOR BASED ON 2 DEC          
         SR    RF,RF                                                            
         IC    RF,COLDCMLS                                                      
         SR    R1,RF                                                            
         STC   R1,COLROUND                                                      
         MVI   COLDCMLS,0                                                       
*&&                                                                             
PRTREP83 TM    COLFLAGS,COLCALC                                                 
         BZ    PRTREP85                                                         
         TM    COLIND2,COLRCALC    ALREADY PROCESSED                            
         BO    PRTREP88                                                         
         GOTO1 EVAL,DMCB,(R3),(R4),ASRTWRK                                      
         LR    R2,R4               R4 = BEGINING OF ACCUM LINE                  
         AH    R2,COLACM           R6 = WHERE ANSWER GOES                       
         MVC   0(PKLEN,R2),ANSWER                                               
*                                                                               
PRTREP85 TM    COLIND1,COLRANGE    MIN AND/OR MAX RANGE FILTER?                 
         BZ    PRTREP88                                                         
*                                                                               
         SR    R1,R1                                                            
         LR    R2,R4                                                            
         AH    R2,COLACM                                                        
*&&UK                                                                           
         MVC   PACKAMT,0(R2)                                                    
         TM    COLFLAGS,COLCALC    Calculated column ?                          
         BZ    *+10                                                             
         SRP   PACKAMT,64-2,5      Yes, round it to 2 demimal places            
*&&                                                                             
         OC    COLMIN$,COLMIN$     MIN RANGE?                                   
         BZ    PRTREP86            NO                                           
         IC    R1,COLMINCC                                                      
*&&UK*&& CP    COLMIN$,PACKAMT                                                  
*&&US*&& CP    COLMIN$,0(PKLEN,R2)                                              
         EX    R1,*+8                                                           
         B     PRTREP86                                                         
*&&UK*&& BC    0,PRTREP87          CLEAR AMOUNT                                 
*&&US*&& BC    0,PRTREP93          SKIP RECORD                                  
*                                                                               
PRTREP86 OC    COLMAX$,COLMAX$     MAX RANGE?                                   
         BZ    PRTREP88            NO                                           
         IC    R1,COLMAXCC                                                      
*&&UK*&& CP    COLMAX$,PACKAMT                                                  
*&&US*&& CP    COLMAX$,0(PKLEN,R2)                                              
         EX    R1,*+8                                                           
         B     PRTREP88                                                         
*&&UK*&& BC    0,PRTREP87          CLEAR AMOUNT                                 
*&&US*&& BC    0,PRTREP93          SKIP RECORD                                  
*&&UK                                                                           
PRTREP87 ZAP   0(PKLEN,R2),PKZERO                                               
*&&                                                                             
PRTREP88 LA    R3,COLLNQ(,R3)      BUMP UP TO NEXT COLUMN                       
         BCT   R0,PRTREP82                                                      
**temp                                                                          
*        TM    UPSI,UPSISRTO       DUMP OUT SORT RECORD                         
*        BZ    PRTREP90                                                         
*        LH    R0,SRTRECLN                                                      
*        GOTO1 PRNTBL,DMCB,=C'SORT REC',(R4),C'DUMP',(R0),=CL2'2D'              
**temp                                                                          
*                                                                               
PRTREP90 BAS   RE,ROUND                                                         
*                                                                               
         AP    DTLCOUNT,PKONE                                                   
         BAS   RE,PROCREC          Fill in data and print                       
*                                                                               
         TM    BLDACT,BLDPRV       Did we print last record ?                   
         BO    PRTREP92            Yes                                          
         TM    RECAPSW,RECAPON     Recapping ?                                  
         BZ    PRTREP92            No                                           
         TM    CUR.RECIND,RECTOTS  Is this special total record ?               
         BO    PRTREP92            Yes                                          
         TM    BLDACT,BLDGOT1      Did we get a record ?                        
         BZ    PRTREP93                                                         
         OI    PRTCNTRL,PRTSRT2                                                 
         GOTO1 RECTOT,DMCB,APRVWRK                                              
         NI    PRTCNTRL,TURNOFF-PRTSRT2                                         
*                                                                               
PRTREP92 GOTO1 RECTOT,DMCB,ASRTWRK                                              
*                                                                               
PRTREP93 TM    BLDACT,BLDPRV                                                    
         BZ    PRTREP94                                                         
         OI    BLDACT,BLDGOT1                                                   
         NI    BLDACT,TURNOFF-BLDPRV                                            
         NI    FMTSW,TURNOFF-FMTNOACT                                           
         L     R0,ASRTWRK          A(SOURCE), SORT DATA                         
         L     RE,APRVWRK          A(DESTINATION),SAVE AS PREVIOUS DATA         
         LH    R1,SRTRECLN         L'SOURCE, RECORD LENGTH                      
         LR    RF,R1               L'DESTINATION, RECORD LENGTH                 
         MVCL  RE,R0                                                            
         MVC   PRVXSRT,RECXSRT     SAVE REC TYPE AND ROW LEVEL                  
*                                                                               
PRTREP94 OC    ALSORT,ALSORT                                                    
         BNZ   PRTREP35                                                         
         TM    DWNOPT1,DWNGO       DOWN-LOADING?                                
         BZ    PRTREP95                                                         
         GOTO1 =A(DWNLOAD),DWNEOR                                               
         B     RPTXIT                                                           
*                                                                               
         USING BOXD,R6                                                          
PRTREP95 MVI   DEBUGRT#,30                                                      
         CLI   FORCEHED,YES                                                     
         BNE   PRTREP96                                                         
         TM    PRTACT,PRTHEADS     WAS LAST ACTION HEADINGS ?                   
         BZ    PRTREP96            NO, SO CONTINUE                              
         MVI   FORCEHED,NO         YES, SO ALREADY DONE                         
*                                                                               
PRTREP96 BAS   RE,PRINTIT                                                       
         OI    PRTCNTRL,PRTEOR     END OF REPORT                                
         BAS   RE,PRINTIT                                                       
         MVI   LINE,1              RESET                                        
         NI    PRTCNTRL,TURNOFF-PRTEOR                                          
         L     R6,ADBXAREA                                                      
         MVI   BOXPGDF#,1          RESET FOR LOGOS                              
         B     RPTXIT                                                           
         DROP  R6,R7                                                            
         EJECT                                                                  
***********************************************************************         
*  MERGE RECORDS THAT SEMI MATCH                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING ROWD,R2                                                          
         USING FMTRECD,R5                                                       
         USING RECXD,RF                                                         
MERGE    L     RF,ALSORT           MUST BE SAME RECORD TYPE                     
         AH    RF,SRTKEYXT                                                      
         CLC   CUR.RECTYPE,RECTYPE                                              
         BNE   MERGEOUT                                                         
         TM    FMTIND,FMTMERGE                                                  
         BO    MERGE01                                                          
*                                                                               
MERGEOUT LTR   RE,RE               SET TO NOT EQUAL                             
         BR    RE                                                               
         DROP  RF                                                               
*                                                                               
MERGE01  NTR1                                                                   
         L     RE,ABUFWRK          A(BUFFER WORK AREA)                          
         L     R0,ASRTWRK                                                       
         LH    R1,SRTDATLN         SORT KEY AND DATA                            
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   BYTE1,FMT#ROWS      NUMBER OF ROWS TO CHECK                      
         L     R2,FMTLROW          POINT TO LAST ROW                            
MERGE10  SR    R1,R1                                                            
         L     R4,ABUFWRK          SORT RECORD TO PRINT                         
         L     R6,ALSORT           NEXT ONE IN                                  
         ICM   R1,1,ROWDASZ                                                     
         BZ    MERGE20                                                          
         LH    RF,ROWDADSP                                                      
         AR    R4,RF               POINT AT DATA INTO SORT REC                  
         AR    R6,RF                                                            
         LR    RF,R4                                                            
         BAS   RE,ISNULL           SEE IF ANY REAL DATA?                        
         BNE   MERGE20             NO DATA SO CAN'T MERGE                       
         ICM   R1,1,ROWDASZ                                                     
         SHI   R1,1                                                             
         EXMVC R1,0(R4),0(R6)      MERGE DATA                                   
*                                                                               
MERGE20  L     R4,ABUFWRK          SORT RECORD TO PRINT                         
         L     R6,ALSORT           NEXT ONE IN                                  
         SR    R1,R1                                                            
         ICM   R1,3,ROWKYSZ        GET LENGTH OF DATA                           
         BZ    MERGE40                                                          
         LH    RF,ROWKYDSP                                                      
         AR    R4,RF               POINT AT DATA INTO SORT REC                  
         AR    R6,RF                                                            
*                                                                               
         LR    R0,R4                                                            
         LR    RE,R6                                                            
         LR    RF,R1                                                            
         CLCL  R0,RE                                                            
         BE    MERGE40             THEY ARE THE SAME SO DO NOTHING              
*                                                                               
         TM    CUR.RECIND,RECMRGE                                               
         BO    *+8                                                              
         TM    ROWKYIND,ROWKYMRG   IS IT MERGABLE                               
         BZ    MERGE90             NO, CAN'T MERGE THESE RECORDS                
         LR    RF,R6                                                            
         ICM   R1,3,ROWKYSZ        GET LENGTH OF DATA                           
         BAS   RE,ISNULL           SEE IF ANY REAL DATA?                        
         BE    MERGE40             NO DATA SO CAN'T MERGE                       
         LR    RF,R4               WASN'T NULL SO SEE IF OTHER IS NULL          
         ICM   R1,3,ROWKYSZ        RELOAD LENGTH OF DATA                        
         BAS   RE,ISNULL           SEE IF ANY REAL DATA?                        
         BNE   MERGE90             NO, CAN'T MERGE THESE RECORDS                
*                                                                               
         LR    R0,R4               MERGE DATA INTO KEY                          
         ICM   R1,3,ROWKYSZ        GET LENGTH OF DATA                           
         LR    RE,R6                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
MERGE40  SHI   R2,ROWLNQ                                                        
         XR    R0,R0                                                            
         IC    R0,BYTE1                                                         
         SHI   R0,1                                                             
         STC   R0,BYTE1            NUMBER OF ROWS LEFT TO DO                    
         LTR   R0,R0                                                            
         BNZ   MERGE10                                                          
*                                                                               
         L     RE,ASRTWRK          A(SORT DATA TO PRINT)                        
         L     R0,ABUFWRK          A(BUFFER WORK AREA)                          
         LH    R1,SRTDATLN         SORT KEY AND DATA                            
         LR    RF,R1                                                            
         MVCL  RE,R0               COLLAPSED DATA KEY                           
*                                                                               
         USING RECXD,RF                                                         
         TM    DDLNKIND,DDLNKGBL                                                
         BZ    MERGE80                                                          
         L     RF,ALSORT           Check on office security                     
         AH    RF,SRTKEYXT                                                      
         CLC   CUR.RECOFC,RECOFC                                                
         B     RPTXIT                                                           
         DROP  RF                                                               
                                                                                
MERGE80  SR    RE,RE                                                            
                                                                                
MERGE90  LTR   RE,RE                                                            
         B     RPTXIT                                                           
*                                                                               
ISNULL   CHI   R1,L'SPACES         IF SPACES OK TO MERGE SO FAR                 
         BL    ISNULL20                                                         
         CLC   0(L'SPACES,RF),SPACES                                            
         BNE   ISNULL40                                                         
         LA    RF,132(RF)          SPACES IS OF LENGTH 132                      
         SHI   R1,L'SPACES                                                      
         B     ISNULL                                                           
ISNULL20 LTR   R1,R1                                                            
         BZ    ISNULL90                                                         
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         BE    ISNULL90                                                         
         CLC   0(0,RF),SPACES                                                   
         AHI   R1,1                                                             
*                                                                               
ISNULL40 CH    R1,=H'250'          IF NULLS OK TO MERGE SO FAR                  
         BL    ISNULL60                                                         
         OC    0(250,RF),0(RF)                                                  
         BNZ   ISNULL70                                                         
         LA    RF,250(RF)                                                       
         SH    R1,=H'250'                                                       
         B     ISNULL40                                                         
ISNULL60 LTR   R1,R1                                                            
         BZ    ISNULL90                                                         
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         BZ    ISNULL90                                                         
         OC    0(0,RF),0(RF)                                                    
*                                                                               
ISNULL70 ICM   R1,3,ROWKYSZ                                                     
         LTR   R1,R1               MAKE SURE LENGTH IS > 1                      
         BZ    ISNULL80            NOT SO, DOESN'T PASS                         
         CLI   0(RF),X'FF'         IF X'FF' THEN OK TO MERGE                    
         BE    ISNULL90                                                         
*                                                                               
ISNULL80 LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
ISNULL90 CR    RE,RE                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*  Routine to setup adn print HEADLINES                               *         
***********************************************************************         
         SPACE 1                                                                
         USING BOXD,R6                                                          
         USING BIGPRNTD,R4                                                      
         USING HEADD,R3                                                         
HEADUP   NTR1                                                                   
         L     R4,VBIGPRNT                                                      
         L     R6,ADBXAREA                                                      
         MVI   ALT#HEAD,0          Set to no alternate headings                 
         XC    PRTFRMT,PRTFRMT                                                  
         MVC   BOXPGDF#,FMTPGDEF   Set page definition                          
         MVC   BOXFONT,FMTFONT     Set font                                     
         MVI   SKIPLINE,0                                                       
         L     R7,ASRTWRK                                                       
         TM    PRINTFG,PDETAIL     Printing totals or detail lines ?            
         BO    HEADUP01            Printing detail                              
         L     R7,APRVWRK          Printing total so use Prev. Sort Rec         
         SR    R1,R1               Reset to previous report printed             
         ICM   R1,1,PRV.RECRPT#                                                 
         BZ    HEADUP01            Probably never saved one                     
         OI    PRTCNTRL,PRTSRT2    Indicate using previous sort rec             
         LR    R5,R1                                                            
         BCTR  R5,0                                                             
         MHI   R5,FMTLNQ                                                        
         A     R5,AFORMATS         Use this formats information                 
         MVC   BOXFONT,FMTFONT     Reset font                                   
         MVC   BOXPGDF#,FMTPGDEF   Set page definition                          
*                                                                               
HEADUP01 TM    FMTRCAP,FMTMRGPG    Mergeing recapped reports ?                  
         BZ    HEADP01F            No                                           
         TM    FMTRCAP,FMTFPAGE    Print recap on seperate page ?               
         BO    HEADP01F            Yes, so use cur. recs. head specs.           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FMTTOP         Get top report of recap group                
         BZ    HEADP01F            None, so ignore                              
         BCTR  RF,0                                                             
         MHI   RF,FMTLNQ                                                        
         A     RF,AFORMATS         Use this format to format heads              
*        MVC   FULL,FMTROW         Use sort recs format code info               
*        MVC   BYTE,FMT#ROWS                                                    
         ST    R5,PRTFRMT                                                       
         LR    R5,RF                                                            
         MVC   BOXFONT,FMTFONT     Reset font                                   
         MVC   BOXPGDF#,FMTPGDEF   Set page definition                          
         GOTO1 =A(NEWHEAD),DMCB,PRTFRMT                                         
         DROP  R6                                                               
***********************************************************************         
*  Build headigns in Block (25 Lines of 198)                          *         
***********************************************************************         
         SPACE 1                                                                
HEADP01F L     R2,PTHEADS          My head line block                           
         LA    R0,SPACES           Source address                               
         SR    R1,R1               Source length                                
         ICM   R1,8,SPACES         Pad character ' '                            
         LR    RE,R2               Destination address                          
         LHI   RF,MAXHDLN*STDPGWD  Destination length                           
         MVCL  RE,R0               Clear                                        
*                                                                               
         SR    R1,R1                                                            
         IC    R1,FMTMLINE         Max lines for page type                      
         STC   R1,MAXLINES                                                      
         TM    FMTPOPT1,RPFBOX     Do we want boxes?                            
         BZ    *+8                 No                                           
         AHI   R1,-1               Minus 1 for bottom of box                    
         STC   R1,DETMAX           To reset # of detail lines                   
*                                                                               
*        OI    FMTFOOT,FMTFTPRT    OK to print                                  
         TM    FMTFOOT,FMTFTON                                                  
         BZ    HEADUP02                                                         
         XC    FOOT#,FOOT#         Number of foot lines to print                
         LA    R0,SPACES           Source address                               
         SR    R1,R1               Source lenght                                
         ICM   R1,8,SPACES         Pad character ' '                            
         L     RE,FOOTNOT1         Destination address                          
         LHI   RF,MAXFTLN*STDPGWD  Destination length                           
         MVCL  RE,R0               Clear                                        
*                                                                               
HEADUP02 L     R1,FMTPGEWD         Width of format                              
         SH    R1,MAXINDNT         Less indentation                             
         STH   R1,RTINDENT                                                      
*                                                                               
         ST    R2,CENTHEAD         Initialize cneter head address               
         ST    R2,LEFTHEAD         Initialize left   head address               
         AH    R2,RTINDENT         Add indent amount to right head              
         ST    R2,RGHTHEAD         Initialize right  head address               
         MVC   FOOTNOT2,FOOTNOT1   Initialize footline                          
         TM    PIXOPT1,PIXGO                                                    
         BZ    HEADP02A                                                         
         TM    DWNOPT1,DWNGO       Are we down-loading ?                        
         BO    HEADP02A            Yes, so no need to print <DATA...>           
         GOTO1 =A(PIXLOAD),PIXDATA                                              
*                                                                               
HEADP02A SR    R0,R0                                                            
         ICM   R0,1,ALT#HEAD                                                    
         BZ    HEADUP03                                                         
         L     R3,AIO3             Alternate headings (Recap related)           
         B     HEADUP04                                                         
*                                                                               
HEADUP03 L     R3,FMTHEAD          Point to table of heading info               
         ICM   R0,1,FMT#HEAD       Number of heading types to process           
         BZ    HEADUP32            There were no headings so exit               
*                                                                               
HEADUP04 MVI   CENTFLAG,YES                                                     
         L     R2,ATMPHEAD         Temp area to print headlines                 
         CLI   HEADTYPE,RHDTITL    If title use temp area                       
         BE    HEADUP05                                                         
         CLI   HEADTYPE,RHDCNTR    If centered heading use temp                 
         BE    HEADUP05                                                         
         L     R2,LEFTHEAD         Head for left side                           
         CLI   HEADTYPE,RHDLFTH                                                 
         BE    HEADUP05                                                         
         L     R2,RGHTHEAD         Head for right side                          
         CLI   HEADTYPE,RHDRHTH                                                 
         BE    HEADUP05                                                         
         L     R2,FOOTNOT2                                                      
*                                                                               
HEADUP05 MVI   XLINES,1                 Default to one line                     
         GOTO1 =A(PRTFORM),PRTHEADS     Print heading data at (R2)              
*                                                                               
         CLI   HEADTYPE,RHDTITL                                                 
         BE    *+8                                                              
         CLI   HEADTYPE,RHDCNTR    If TEMPHEAD was used                         
         BNE   *+8                                                              
         L     R2,CENTHEAD         Load actual headline address                 
*                                                                               
         SR    R1,R1                                                            
         IC    R1,XLINES           Number of headlines used                     
         MHI   R1,STDPGWD                                                       
         LR    R4,R2               Save R2, current heading address             
         AR    R1,R2               Bump to new head, save accordingly           
*                                                                               
HEADUP10 CLI   HEADTYPE,RHDLFTH                                                 
         BNE   *+12                                                             
         ST    R1,LEFTHEAD         new left heading location address            
         B     HEADUP30            Loop to print next heading                   
*                                                                               
         CLI   HEADTYPE,RHDRHTH                                                 
         BNE   *+12                                                             
         ST    R1,RGHTHEAD         New right heading locataion addres           
         B     HEADUP30            Loop to print next heading                   
*                                                                               
         CLI   HEADTYPE,RHDFTLN                                                 
         BNE   HEADUP14                                                         
         ST    R1,FOOTNOT2         New footline location address                
         SR    R1,R1               There's lines need to be centered            
         ICM   R1,1,XLINES         There are X lines to center                  
         AH    R1,FOOT#                                                         
         STH   R1,FOOT#                                                         
         B     HEADUP30            Footline just loop                           
*                                                                               
HEADUP14 L     R2,ATMPHEAD                                                      
         ST    R1,CENTHEAD         Save loc. for next heading to print          
         TM    HEADFLAG,HEADROW    Is it a centered row ?                       
         BO    HEADUP15                                                         
         ST    R1,LEFTHEAD                                                      
         AH    R1,RTINDENT         Set location to right side                   
         ST    R1,RGHTHEAD                                                      
*                                                                               
HEADUP15 CLI   CENTFLAG,NO         Force to not center                          
         BE    HEADUP30                                                         
***********************************************************************         
*  CENTER HEADLINE DATA                                               *         
***********************************************************************         
         SPACE 1                                                                
         SR    R6,R6                                                            
         ICM   R6,1,XLINES         There are R6=X lines to center               
         BZ    HEADUP30            No lines to center                           
*                                                                               
HEADUP25 GOTO1 CENTER,DMCB,(R2),FMTPGEWD                                        
         LA    RF,STDPGWD                                                       
         ST    R2,SVR2             R2 = data to move                            
         ST    R4,SVR4             R4 = real print line                         
*                                                                               
HEADP25A CLI   0(R2),C' '                                                       
         BH    HEADP25B            Find first character                         
         LA    R2,1(,R2)                                                        
         LA    R4,1(,R4)           Move in synic with real print line           
         BCT   RF,HEADP25A                                                      
         B     HEADUP29            No data to move in                           
*                                                                               
HEADP25B LA    RE,0(RF,R2)         Point to end of line                         
         BCTR  RE,0                Less one                                     
*                                                                               
HEADP25C CLI   0(RE),C' '          Find last character                          
         BH    HEADP25D                                                         
         AHI   RE,-1                                                            
         BCT   RF,HEADP25C         RF = length of data                          
*                                                                               
HEADP25D L     RE,AUPCASE                                                       
         BCTR  RF,0                                                             
         TM    FMTPOPT1,RPFMCASE   Mixed case?                                  
         BO    HEADUP28            Yes                                          
         EX    RF,*+8              Make upper case                              
         B     HEADUP28                                                         
         TR    0(0,R2),0(RE)                                                    
*                                                                               
HEADUP28 EXMVC RF,0(R4),0(R2)      Move data to real line                       
*                                                                               
HEADUP29 L     R2,SVR2                Restore register                          
         L     R4,SVR4                Restore register                          
         MVI   0(R2),C' '             Pad character blank                       
         MVC   1(STDPGWD-1,R2),0(R2)  Clear line                                
         LA    R2,STDPGWD(,R2)        Next line                                 
         LA    R4,STDPGWD(,R4)        Next line                                 
         BCT   R6,HEADUP25            Do again                                  
*                                                                               
HEADUP30 LA    R3,HEADLNQ(,R3)     Bump to next heading to process              
         BCT   R0,HEADUP04                                                      
*                                                                               
HEADUP32 MVI   FORCEHED,YES                                                     
         L     R4,VBIGPRNT                                                      
         CLI   FMT#HEAD,0          Are there any headings?                      
         BE    HEADUP50            No, set up boxes then                        
         L     R3,RGHTHEAD         Which has a greater address                  
         SH    R3,RTINDENT         Right heading or left?                       
         C     R3,LEFTHEAD                                                      
         BH    *+8                                                              
         L     R3,LEFTHEAD                                                      
         C     R3,CENTHEAD                                                      
         BH    *+8                                                              
         L     R3,CENTHEAD         Load actual headline address                 
         SR    R6,R6                                                            
         IC    R6,DETMAX                                                        
         SH    R6,FOOT#            Number of footlines used                     
         OC    FOOT#,FOOT#                                                      
         BZ    *+6                                                              
         BCTR  R6,0                                                             
         STC   R6,DETMAX           To leave room for footline block             
***********************************************************************         
*  MOVE HEADBLOCK DATA INTO PRINT LINES                               *         
***********************************************************************         
         SPACE 1                                                                
         L     R2,PTHEADS          Point to heading block                       
HEADUP40 LA    R1,4                Print max of four lines at a time            
         LA    RE,XP                                                            
*                                                                               
HEADUP45 CLC   0(STDPGWD,R2),XSPACES     Is the line blank ?                    
         BNE   *+8                       No, skip                               
         MVI   0(R2),X'00'               Force a blank line                     
         MVC   0(STDPGWD,RE),0(R2)                                              
         LA    R2,STDPGWD(,R2)                                                  
         LA    RE,STDPGWD(,RE)                                                  
         CR    R2,R3               Reached last headline to print ?             
         BH    HEADUP50            Yes, so exit                                 
         BCT   R1,HEADUP45         No, so print another                         
*                                                                               
         MVI   DEBUGPRT,1          Debug LOCATION 1                             
         MVI   DEBUGRT#,7                                                       
         GOTO1 REPORT                                                           
         CR    R2,R3               Reached last headling to print ?             
         BNH   HEADUP40            No, so keep printing                         
*                                                                               
         USING BOXD,R6                                                          
HEADUP50 MVI   DEBUGPRT,2          Debug LOCATION 2                             
         MVI   DEBUGRT#,8                                                       
         GOTO1 REPORT              Print last group of lines                    
*                                                                               
         L     R6,ADBXAREA                                                      
         OC    PRTFRMT,PRTFRMT     Did we print alternate headings ?            
         BZ    HEADUP52                                                         
         L     R5,PRTFRMT          Restore format rec's information             
         MVC   BOXFONT,FMTFONT     Reset font                                   
         MVC   BOXPGDF#,FMTPGDEF   Set page definition                          
         XC    PRTFRMT,PRTFRMT     Clear                                        
         XC    BYTE2,BYTE2         Clear for box control                        
***********************************************************************         
*  Print Column headings and set up boxes at start of page            *         
***********************************************************************         
         SPACE 1                                                                
HEADUP52 TM    FMTPOPT3,RPFNOCHD   No column headings ?                         
         BZ    HEADUP58            Print as normal                              
         LR    R2,R5                                                            
*                                                                               
LAST     USING FMTRECD,R2                                                       
         SR    R0,R0                                                            
         IC    R0,FMTNUM                                                        
HEADUP56 AHI   R0,-1                                                            
         BNP   HEADUP82               No column heading to print                
         AHI   R2,-FMTLNQ             Previous format                           
         TM    LAST.FMTPOPT3,RPFNOCHD Can we print these col headings ?         
         BO    HEADUP56                                                         
         ST    R5,PRTFRMT          Save off current format printing             
         LR    R5,R2               Swap reports format to use                   
         MVC   BOXFONT,FMTFONT                                                  
         DROP  R6                                                               
         DROP  LAST                                                             
*                                                                               
HEADUP58 TM    FMTIND,FMTSFTH      Soft column headings?                        
         BZ    HEADUP70            No                                           
*                                                                               
         USING COLD,R3                                                          
         USING DTED,R6                                                          
HEADUP60 L     R3,FMTCOL                                                        
         SR    R0,R0                                                            
         IC    R0,FMT#COLS         Look at each column                          
*        GOTOR SOFTHEAD,DMCB,(CUR.RECOTH,COLD),FMTRECD                          
         SR    R6,R6                                                            
         MVC   BYTE,CUR.RECOTH                                                  
         NI    BYTE,TURNOFF-RECMASK                                             
         ICM   R6,1,BYTE                                                        
         BZ    *+6                 DEFAULT to first set of dates                
         BCTR  R6,0                                                             
         MH    R6,DTECOLLN                                                      
         A     R6,FMTDTED                                                       
         GOTO1 DATCON,DMCB,(1,DTEPEDST),(X'20',DTE1)                            
         MVC   WORK(3),DTEPEDEN                                                 
         LA    R6,DTESTRT                                                       
*                                                                               
HEADUP61 SR    R7,R7                                                            
         IC    R7,COLDTE#                                                       
         MHI   R7,L'DTECOL                                                      
         AR    R7,R6               Point to correct dte column                  
         TM    COLIND1,COLCNDR     Calendar dates                               
         BZ    HEADUP68            Not applicable                               
         TM    COLOPT,COLSFTH      Soft heading                                 
         BZ    HEADUP68                                                         
         ICM   R2,15,COLHEAD1                                                   
         BZ    HEADUP64                                                         
         TM    COLFLAGS,COLAMT                                                  
         BO    HEADUP63                                                         
***********************************************************************         
*  Column head one is day of the week                                 *         
*  Column head two is either MMMDD/YY or DD, Depends of size of col.  *         
***********************************************************************         
         SPACE 1                                                                
         SR    RF,RF                                                            
         IC    RF,COLPERST                                                      
         BCTR  RF,0                Less one to adjust                           
         GOTO1 ADDAY,DMCB,(C'D',DTE1),DTE2,(RF)                                 
         GOTO1 DATCON,DMCB,(0,DTE2),(1,WORK+3)                                  
         CLC   WORK+3(3),WORK                                                   
         BH    HEADUP68            Next column, this one out of range           
         GOTO1 GETDAY,DMCB,DTE2,(R2)                                            
         ICM   R2,15,COLHEAD2                                                   
         BZ    HEADUP68            Next column                                  
         CLI   COLSIZE,DATESZQ                                                  
         BL    HEADUP62                                                         
         GOTO1 DATCON,DMCB,(1,WORK+3),(8,(R2))                                  
         B     HEADUP68                                                         
*                                                                               
HEADUP62 DS    0H                                                               
*&&US*&& MVC   0(2,R2),DTE2+4                                                   
*&&UK*&& MVC   0(2,R2),DTE2                                                     
         CLI   0(R2),C'0'          Is first digit zero ?                        
         BNE   *+8                 No                                           
         MVI   0(R2),C' '          Blank out first zero                         
         B     HEADUP68                                                         
*                                                                               
HEADUP63 OC    0(3,R7),0(R7)       No start date                                
         BZ    HEADUP65                                                         
         CLC   0(3,R7),=X'FFFFFF'  No dates associated if equal                 
         BE    HEADUP64                                                         
         GOTO1 DATCON,DMCB,(1,0(R7)),(8,(R2))                                   
*                                                                               
HEADUP64 ICM   R2,15,COLHEAD2                                                   
         BZ    HEADUP68                                                         
*                                                                               
HEADUP65 CLC   3(3,R7),=X'FFFFFF'  No end date                                  
         BE    HEADUP68                                                         
         OC    3(3,R7),3(R7)                                                    
         BZ    HEADUP68                                                         
         GOTO1 DATCON,DMCB,(1,3(R7)),(8,(R2))                                   
*                                                                               
HEADUP68 LA    R3,COLLNQ(,R3)                                                   
         BCT   R0,HEADUP61                                                      
         DROP  R3,R6                                                            
*                                                                               
ORG      USING FMTRECD,R2                                                       
         USING BOXD,R6                                                          
HEADUP70 L     RE,FMTCHEAD         Set up boxes & column headings               
         L     R6,ADBXAREA                                                      
         MVC   XPSECOND(STDPGWD),0(RE)                                          
         MVC   XPTHIRD(STDPGWD),STDPGWD(RE)                                     
         MVC   XPFOURTH(STDPGWD),2*STDPGWD(RE)                                  
         MVC   BOXROWS(BOXROWSL),SPACES               Clear boxes               
         MVC   BOXCOLS(STDPGWD),XSPACES                                         
*                                                                               
HEADUP80 TM    FMTPOPT1,RPFBOX                   Do we want boxes?              
         BZ    HEADUP88                                                         
***********************************************************************         
*  BUILD BOXES                                                        *         
***********************************************************************         
         SPACE 1                                                                
         MVC   XPFOURTH(STDPGWD),XSPACES                                        
         ZIC   RF,LINE                                                          
*&&US*&& LA    RF,BOXROWS-1(RF)                                                 
*&&UK*&& LA    RF,BOXROWS(RF)                                                   
         MVI   0(RF),C'T'          Top                                          
         AHI   RF,2                                                             
         CLC   XPTHIRD(STDPGWD),XSPACES                                         
         BE    *+8                                                              
         AHI   RF,1                                                             
         MVI   0(RF),C'M'          Mid-line box                                 
         ICM   R2,15,PRTFRMT       Check original format                        
         BZ    HEADUP84                                                         
         TM    ORG.FMTPOPT1,RPFBOX                                              
         BZ    HEADUP81            Box column headings, No box report           
         L     R3,FMTBOXCL                                                      
         MVC   TEMPSTRG,0(R3)                                                   
         L     R3,ORG.FMTBOXCL                                                  
         CLC   TEMPSTRG,0(R3)                                                   
         BE    HEADUP84            Box from 1 report match from 2nd             
*                                                                               
HEADUP81 MVI   0(RF),C'B'          Make it the bottom instead                   
         MVI   BYTE2,C'B'          Indicate we closed the box                   
         B     HEADUP84                                                         
         DROP  ORG                                                              
*                                                                               
HEADUP82 L     R6,ADBXAREA                                                      
         MVC   BOXROWS(BOXROWSL),SPACES     Clear boxes                         
         MVC   BOXCOLS(STDPGWD),XSPACES                                         
         TM    FMTPOPT1,RPFBOX              Do we want boxes?                   
         BZ    HEADUP88                                                         
         ZIC   RF,LINE                                                          
*&&US*&& LA    RF,BOXROWS-1(RF)                                                 
*&&UK*&& LA    RF,BOXROWS(RF)                                                   
         MVI   0(RF),C'T'          Only top, no column headings                 
*                                                                               
HEADUP84 L     R3,FMTBOXCL                                                      
         MVC   BOXFONT,FMTFONT     Set new possible box font                    
         MVC   BOXCOLS(STDPGWD),0(R3)                                           
         MVI   BOXINIT,0                                                        
         MVI   BOXYORN,YES                                                      
*                                                                               
HEADUP88 MVI   DEBUGPRT,3          Debug LOCATION 3                             
         MVI   DEBUGRT#,9                                                       
         GOTO1 REPORT              Print head lines and column headings         
*                                                                               
         ICM   R5,15,PRTFRMT       Did we save off format ? (Restore)           
         BZ    HEADUP90            No, so process as normal                     
         CLI   BYTE2,C'B'          This means we closed box and that            
         BNE   HEADUP90            this format doesn't have col heads           
         MVI   BOXREQ,C'C'         Close box                                    
         GOTO1 REPORT                                                           
         BAS   RE,OPENBOX                                                       
         B     HEADUP92                                                         
*                                                                               
HEADUP90 MVI   DEBUGPRT,4          Debug LOCATION 4                             
         MVI   DEBUGRT#,10                                                      
         GOTO1 REPORT                                                           
*                                                                               
HEADUP92 ICM   R2,15,PRTFRMT       Did we switch formats ?                      
         BZ    HEADUP95                                                         
         LR    R5,R2                                                            
         XC    PRTFRMT,PRTFRMT                                                  
         MVC   BOXFONT,FMTFONT     Restore sort records font                    
*                                                                               
HEADUP95 TM    PAGEFLAG,PAGEACOL   All  columns formatted ?                     
         BO    *+8                 Yes, skip                                    
         OI    PAGEFLAG,PAGERSET   Turn on print all details flag               
         NI    PRTCNTRL,TURNOFF-PRTSRT2                                         
         B     RPTXIT                                                           
                                                                                
TEMPSTRG DS    CL(STDPGWD)                                                      
         DROP  R4,R5,R6                                                         
         EJECT 1                                                                
***********************************************************************         
*  PRINT MIDLINES                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING ROWD,R2                                                          
         USING FMTRECD,R5                                                       
         USING ACRL2D,R7                                                        
MIDPRT   NTR1                                                                   
         L     R7,AACRL2D                                                       
         L     R2,FMTROW                                                        
         TM    DWNOPT1,DWNGO                                                    
         BZ    MIDPRT30                                                         
         ZIC   R0,FMT#ROWS                                                      
*                                                                               
MIDPRT10 CLI   ROWTYPE,ROWCNTR     CENTER HEADING                               
         BE    MIDPRT12                                                         
         CLI   ROWTYPE,ROWLEFT     LEFT HEADING                                 
         BE    MIDPRT12                                                         
         CLI   ROWTYPE,ROWRGHT     RIGHT HEADING                                
         BE    MIDPRT12                                                         
         CLI   ROWTYPE,ROWMID      MID-LINE                                     
         BNE   MIDPRT24                                                         
*                                                                               
MIDPRT12 CLI   ROWSTATE,ROWPTMID   PRINT MIDLINE   FOR  THIS ROW ?              
         BNE   *+8                 NO,   SKIP                                   
         MVI   ROWSTATE,ROWCLEAR   SET   TO   CLEAR     ACCUM FOR ROW           
*                                                                               
         TM    DWNOPT1,DWNROWS     DOWN-LOAD  ROWS ?                            
         BZ    MIDPRT24            NO,   SO   GO TO     NEXT ROW                
*                                                                               
         TM    DWTOTST,DWTROWBL    DOWN-LOAD  ROW  FIELDS    AS BLANK ?         
         BO    MIDPRT18            YES,  DO   NOT  FORMAT    THE  FIELD         
         CLI   ROWTYPE,ROWMID      MID-LINE ?                                   
         BNE   MIDPRT15            NO,   FORMAT    THE  FIELD                   
*                                  ARE   WE   DOWN-LOADING   TOTALS ?           
         TM    DWTOTST,DWTTREQ+DWTTFOR                                          
         BNZ   MIDPRT18            YES,  DO   NOT  FORMAT    THE  FIELD         
*                                                                               
MIDPRT15 ST    R2,ACURROW               Set ACURROW = A(ROW)                    
         GOTO1 =A(PRTFORM),PRTROWS      FORMAT THE  FIELD                       
*                                                                               
         TM    ROWFLAGS,ROWADDR    Using ADR attribute?                         
         BO    MIDPRT16                                                         
*&&US*&& TM    ROWDAIND,ROWBDR     or BDR attribute?                            
         BZ    MIDPRT24            NO,   FINSHED   WITH THIS ROW                
MIDPRT16 CLI   RAD#LDWN,0          DOWN-LOAD  ADDRESSES ?                       
         BE    MIDPRT24            NO,   FINISHED  WITH THIS ROW                
         GOTO1 =A(DWNROWAD),(R2)   OUTPUT     ROW  ADDR LINES                   
         B     MIDPRT24            FINISHED   WITH THIS ROW                     
*                                                                               
MIDPRT18 LA    RE,XLCHOP           CLEAR XLCHOP TO ZEROES OR SPACES             
         LHI   RF,GESIZEQ                                                       
*                                                                               
         ST    R0,SVR0             SAVE R0                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,8,SPACES         SET PADDING CHARACTER                        
         MVCL  RE,R0                                                            
         L     R0,SVR0             RESTORE R0                                   
*                                                                               
         MVI   PRTSIZE,1           SET   FLD  LENGTH    TO   1                  
         TM    DWNOPT1,DWNCHOP     DOWN-LOAD  FIXED     LENGTH DATA ?           
         BZ    *+10                NO,   SKIP                                   
         MVC   PRTSIZE,ROWPRTSZ    YES,  USE  ROW  PRINT     SIZE               
         GOTO1 =A(DWNLOAD),DWNTEXT OUTPUT     THIS ROW  AS   BLANKS             
*                                                                               
         TM    ROWFLAGS,ROWADDR    ROW   ADDRESS ?                              
         BO    MIDPRT21            NO,   SKIP                                   
*&&US*&& TM    ROWDAIND,ROWBDR     or BDR attribute?                            
         BZ    MIDPRT24            NO,   SKIP                                   
MIDPRT21 CLI   RAD#LDWN,0          DOWN-LOAD  ADDRESSES ?                       
         BE    MIDPRT24            NO,   SKIP                                   
         TM    DWNOPT1,DWNCHOP     DOWN-LOAD  FIXED     LENGTH DATA ?           
         BZ    *+10                NO,   SKIP                                   
         MVC   PRTSIZE,RADPRTSZ    YES,  USE  ROW  ADDRESS   PRT  SIZE          
*                                  NOTE: XLCHOP   AND  PRTSIZE   ARE            
*                                        NOT  CHANGED   IN   DWNLOAD            
         ZIC   R3,RAD#LDWN         ALWAYS     USE  NUM  ROW  ADDR LINES         
*                                                                               
MIDPRT22 GOTO1 =A(DWNLOAD),DWNTEXT OUTPUT     THIS LINE AS   BLANKS             
         BCT   R3,MIDPRT22         ANY   MORE LINES     TO   DOWN-LOAD?         
*                                                                               
MIDPRT24 LA    R2,ROWLNQ(,R2)                                                   
         BCT   R0,MIDPRT10                                                      
         B     RPTXIT                                                           
*                                                                               
MIDPRT30 NI    BLDACT,TURNOFF-BLDMGPG                                           
         CLC   CUR.RECRPT#,PRV.RECRPT#    WAS LAST REC FOR SAME REP ?           
         BE    MIDPRT34                                                         
         CLI   QOPT3,C'Q'                                                       
         BNE   MIDPR30A                                                         
         XC    P,P                                                              
         MVC   P+2(1),CUR.RECRPT#                                               
         MVC   P+3(1),PRV.RECRPT#                                               
         GOTO1 PRNTBL,DMCB,=C'MERGE',P,C'DUMP',L'P,=CL2'1D'                     
         GOTO1 PRNTBL,DMCB,=C'CUR',ASRTWRK,C'DUMP',40,=CL2'1D'                  
         GOTO1 PRNTBL,DMCB,=C'PRV',APRVWRK,C'DUMP',40,=CL2'1D'                  
*        TM    FMTRCAP,FMTMRGPG    DO WE NEED TO MERGE REPORTS ON PAGE          
*        BZ    MIDPRT34            NO                                           
*                                                                               
MIDPR30A DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,FMT#ROWS                                                      
         L     R2,FMTROW                                                        
MIDPRT31 CLI   ROWTYPE,ROWMID      IS IT A MID-LINE ?                           
         BE    MIDPRT32                                                         
         CLI   ROWTYPE,ROWCOL      IS IT A DETAIL LINE ?                        
         BE    MIDPRT32            NO, MUST HAVE PAGED UP                       
         LA    R2,ROWLNQ(,R2)                                                   
         BCT   R1,MIDPRT31                                                      
         B     MIDPRT34            ONLY HEADING TYPE ROWS                       
*                                                                               
MIDPRT32 CLI   QOPT3,C'Q'                                                       
         BNE   MIDPRT33                                                         
         XC    P,P                                                              
         MVC   P+2(2),ROWKYDSP                                                  
         GOTO1 PRNTBL,DMCB,=C'2ND',P,C'DUMP',40,=CL2'1D'                        
*                                                                               
MIDPRT33 LH    R1,ROWKYDSP                                                      
         BCTR  R1,0                LESS ONE FOR RECAP #                         
         L     RE,ASRTWRK          A(SRTWRK)                                    
         L     R0,APRVWRK          A(RECORD ALREADY PRINTED)                    
         LR    RF,R1               LENGTH TO COMPARE FOR                        
         CLCL  RE,R0                                                            
         BNE   MIDPRT34            MUST OF HAD A PAGEUP                         
         OI    BLDACT,BLDMGPG                                                   
         CLI   QOPT3,C'Q'                                                       
         BNE   MIDPRT34                                                         
         GOTO1 PRNTBL,DMCB,=C'3RD',P,C'DUMP',40,=CL2'1D'                        
*                                                                               
MIDPRT34 MVC   COUNT,SKIPLINE      SAVE NUMBER OF LINES TO SKIP                 
         MVI   SKIPLINE,1          Skip 1 Line before printing mid              
*        MVI   SKIPLINE,2          SKIP 2 LINES FOR 1ST MIDLINE PRINTED         
*        CLI   FORCEHED,YES                                                     
*        BNE   *+8                                                              
*        MVI   SKIPLINE,0          START OF PAGE, NO NEED TO SKIP               
*                                                                               
         SR    R1,R1                                                            
         ZIC   R0,FMT#ROWS                                                      
         L     R2,FMTROW                                                        
MIDPRT35 CLI   ROWSTATE,ROWPTMID                                                
         BNE   *+12                                                             
         AHI   R1,1                COUNT NUMBER OF MIDS TO PRINT                
         OI    BLDACT,BLDMIDS      MIDS TO PRINT                                
         AHI   R2,ROWLNQ                                                        
         BCT   R0,MIDPRT35                                                      
*                                                                               
         SLL   R1,1                NUMBER OF MIDS X 2                           
         TM    BLDACT,BLDMGPG      SEE IF WE CAN PRINT NEW COLUMNS              
         BZ    MIDPRT36            WE, WANT TO, BUT CAN WE                      
         AHI   R1,3                ALLOW MIN OF THREE COLUMN HEADS              
         TM    FMTPOPT1,RPFBOX                                                  
         BZ    MIDPRT36                                                         
         AHI   R1,4                BOX TOP/ 2 COL HEADS/ BOX MID                
*                                                                               
MIDPRT36 LTR   R1,R1                                                            
         BZ    MIDPRT90            NO MIDS TO PRINT                             
         IC    R0,LINE             CURRENT LINE NUMBER                          
         AR    R1,R0                                                            
         AHI   R1,4                ROOM TO PRINT DETAIL                         
         TM    FMTPOPT1,RPFBOX     PRINT BOXES ?                                
         BZ    *+8                 NO,  SKIP                                    
         AHI   R1,1                YES, ALLOW FOR END-OF-BOX LINE               
         CLM   R1,1,DETMAX         WILL IT FIT ON THIS PAGE?                    
         BH    MIDPRT38            NO                                           
         CLI   FORCEHED,YES                                                     
         BE    MIDPRT40                                                         
         TM    FMTRCAP,FMTMRGPG                                                 
         BZ    MIDPRT40                                                         
         TM    BLDACT,BLDMGPG      DO WE WANT TO START A NEW REPORT ?           
         BZ    MIDPRT40            NO                                           
         CLI   QOPT3,C'Q'                                                       
         BNE   MIDPR37A                                                         
         XC    P,P                                                              
         MVC   P+2(1),LINE                                                      
         MVC   P+3(1),DETMAX                                                    
         STC   R1,P+4                                                           
         GOTO1 PRNTBL,DMCB,=C'HERE',P,C'DUMP',40,=CL2'1D'                       
*                                                                               
MIDPR37A MVI   SKIPLINE,0                                                       
         BAS   RE,CLOSEBOX         CLOSE BOX / END LAST REPORT ON PAGE          
         BAS   RE,OPENBOX                                                       
         B     MIDPRT40                                                         
*                                                                               
MIDPRT38 CLI   QOPT3,C'Z'                                                       
         BNE   MIDPRT39                                                         
         GOTO1 PRNTBL,DMCB,=C'FORCE',P,C'DUMP',40,=CL2'1D'                      
*                                                                               
MIDPRT39 MVI   FORCEHED,YES        NO, SO HEADUP THEN PRINT MIDS                
         BAS   RE,PRINTIT                                                       
*                                                                               
MIDPRT40 TM    BLDACT,BLDMIDS      DO WE REALY HAVE MIDS TO PRINT ?             
         BZ    MIDPRT90            NO                                           
         NI    BLDACT,TURNOFF-BLDMIDS                                           
         ZIC   R0,FMT#ROWS                                                      
         L     R2,FMTROW                                                        
*                                                                               
MIDPRT45 CLI   ROWSTATE,ROWPTMID   IF 'M' THEN PRINT MIDLINE                    
         BNE   MIDPRT60                                                         
         MVI   COUNT,0             RESET COUNT NOT TO SKIP A LINE               
         MVI   ROWSTATE,ROWCLEAR   SET  TO   CLEAR ACCUM FOR ROW                
         MVI   XLINES,1                                                         
         GOTO1 =A(PRTFORM),PRTMIDS Parse out code & name (XLCHOP)               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,XLINES           NUMBER OF LINES USED IN CHOPPER              
         MHI   RF,STDPGWD          Bump under last line printed                 
         ICM   R4,15,ROWPRTAD                                                   
         BZ    MIDPRT60                                                         
         LA    R3,0(RF,R4)         ADDRESS WHERE TO UNDERLINE                   
         CLI   XLINES,1                                                         
         BNH   MIDPRT48                                                         
         MVC   P,SPACES                                                         
         SR    RF,RF               FORCE TO UNDERLINE LONGEST PART              
         IC    RF,XLINES                                                        
         SR    R1,R1                                                            
         IC    R1,ROWPRTSZ                                                      
         BCTR  R1,0                                                             
*                                  RF = # LINES, R1 = LENGTH OF AREA            
MIDPRT46 EXOC  R1,P,0(R4)                                                       
         LA    R4,STDPGWD(,R4)                                                  
         BCT   RF,MIDPRT46                                                      
         LA    R4,P                FOOL CHOPPER, UNDERLINE MERGED LINES         
*                                                                               
MIDPRT48 GOTO1 UNDERLIN,DMCB,(PRTSIZE,(R4)),(FMTULINE,(R3))                     
         MVI   DEBUGRT#,1                                                       
         BAS   RE,PRINTIT          PRINT THE MIDLINE                            
*                                                                               
MIDPRT60 LA    R2,ROWLNQ(,R2)      BUMP TO NEXT ROW                             
         BCT   R0,MIDPRT45         AND LOOP BACK                                
*                                                                               
MIDPRT90 MVC   SKIPLINE,COUNT      RESTORE NUMBER OF LINES TO SKIP              
         B     RPTXIT                                                           
         DROP  R2,R5,R7                                                         
         EJECT                                                                  
         USING FMTRECD,R5                                                       
         USING BOXD,R6                                                          
CLOSEBOX NTR1                                                                   
         L     R6,ADBXAREA                                                      
         MVI   NOOPEN,NO                                                        
         MVC   BYTE,FMTREPWD       Width of current report                      
         LR    R2,R5                                                            
         SR    R5,R5                                                            
         IC    R5,PRV.RECRPT#                                                   
         BCTR  R5,0                                                             
         MHI   R5,FMTLNQ                                                        
         A     R5,AFORMATS         POINT TO PREVIOUS FORMAT INFO                
         TM    FMTPOPT1,RPFBOX     PRINT END OF BOX ?                           
         BZ    CLOSEB10            NO, JUST PRINT A BLANK LINE                  
         L     R3,FMTBOXCL                                                      
         ZIC   RF,LINE             CURRENT LINE                                 
*&&US*&& LA    RF,BOXROWS-1(RF)    POINT TO BOTTOM OF BOX                       
*&&UK*&& LA    RF,BOXROWS(RF)                                                   
         MVI   0(RF),C'B'          MARK BOTTOM                                  
         CLC   FMTREPWD,BYTE       Are they the same                            
         BNE   CLOSEB02                                                         
         MVI   0(RF),C'M'          MARK as mid-line instead                     
         MVI   NOOPEN,YES                                                       
                                                                                
CLOSEB02 MVC   BOXCOLS(STDPGWD),0(R3)                                           
         MVC   BOXFONT,FMTFONT                                                  
         MVI   DEBUGPRT,20         DEBUG LOCATION 1                             
         MVI   DEBUGRT#,11                                                      
         GOTO1 REPORT                                                           
*                                                                               
CLOSEB10 MVI   DEBUGRT#,2          BLANK LINE                                   
         BAS   RE,PRINTIT          BLANK LINE                                   
         LR    R5,R2               RESTORE PREVIOUS FORMAT INFO                 
         MVC   BOXFONT,FMTFONT     RESTORE ORIGINAL BOX FONT                    
         B     RPTXIT                                                           
         DROP  R5,R6                                                            
         EJECT ,                                                                
         USING FMTRECD,R5                                                       
         USING BOXD,R6                                                          
         USING BIGPRNTD,R4                                                      
OPENBOX  NTR1                                                                   
         L     R4,VBIGPRNT                                                      
         L     R6,ADBXAREA                                                      
         MVC   BOXROWS(BOXROWSL),SPACES               CLEAR BOXES               
         MVC   BOXCOLS(STDPGWD),XSPACES                                         
         MVC   BOXFONT,FMTFONT                                                  
         TM    FMTPOPT3,RPFNOCHD   NO COLUMN HEADINGS ?                         
         BO    OPENB02                                                          
         L     RE,FMTCHEAD         SET UP BOXES & COLUMN HEADINGS               
         MVC   XPSECOND(STDPGWD),0(RE)                                          
         MVC   XPTHIRD(STDPGWD),STDPGWD(RE)                                     
         MVC   XPFOURTH(STDPGWD),2*STDPGWD(RE)                                  
*                                                                               
OPENB02  TM    FMTPOPT1,RPFBOX     PRINT END OF BOX ?                           
         BZ    OPENB10             NO, JUST PRINT A BLANK LINE                  
         MVC   XPFOURTH(STDPGWD),XSPACES                                        
         ZIC   RF,LINE                                                          
*&&US*&& LA    RF,BOXROWS-1(RF)                                                 
*&&UK*&& LA    RF,BOXROWS(RF)                                                   
         CLI   NOOPEN,YES                                                       
         BE    *+8                                                              
         MVI   0(RF),C'T'                                                       
         TM    FMTPOPT3,RPFNOCHD   NO COLUMN HEADINGS ?                         
         BO    OPENB04                                                          
         AHI   RF,2                                                             
         CLC   XPTHIRD(STDPGWD),XSPACES                                         
         BE    *+8                                                              
         AHI   RF,1                                                             
         MVI   0(RF),C'M'                                                       
*                                                                               
OPENB04  L     R3,FMTBOXCL                                                      
         MVC   BOXCOLS(STDPGWD),0(R3)                                           
*        CLI   NOOPEN,YES                                                       
*        BE    *+8                                                              
         MVI   BOXINIT,0                                                        
         MVI   BOXYORN,YES                                                      
*                                                                               
OPENB10  MVI   DEBUGPRT,21         DEBUG LOCATION                               
         GOTO1 REPORT                                                           
         MVI   DEBUGPRT,22         DEBUG LOCATION                               
         GOTO1 REPORT                                                           
         B     RPTXIT                                                           
                                                                                
NOOPEN   DS    CL1                                                              
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
*  MOVE FOOTBLOCK DATA INTO PRINT LINES                               *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R4                                                      
         USING FMTRECD,R5                                                       
         USING BOXD,R6                                                          
FOOTUP   NTR1                                                                   
         L     R4,VBIGPRNT                                                      
         L     R6,ADBXAREA                                                      
         TM    PRTCNTRL,PRTSOR     Start of report                              
         BO    FOOTUP90                                                         
         CLC   PRV.RECRPT#,CUR.RECRPT#                                          
         BE    FOOTUP03                                                         
         SR    R1,R1               Reset to previous report printed             
         ICM   R1,1,PRV.RECRPT#    First time through it's 0                    
         BZ    FOOTUP03                                                         
         LR    R5,R1                                                            
         BCTR  R5,0                                                             
         MHI   R5,FMTLNQ                                                        
         A     R5,AFORMATS                                                      
         MVC   BOXFONT,FMTFONT     Set font                                     
*                                                                               
FOOTUP03 DS    0H                                                               
         L     R2,FOOTNOT1         Point to footline block                      
         ZIC   RF,LINE             Current line                                 
         LR    R1,RF                                                            
         TM    FMTPOPT3,RPFTEDIT                                                
         BZ    *+8                                                              
         IC    RF,DETMAX           Always print bottom at detmax                
         TM    FMTPOPT1,RPFBOX     Print boxes?                                 
         BZ    FOOTUP04                                                         
*&&US*&& LA    RF,BOXROWS-1(RF)    Point to bottom of box                       
*&&UK*&& LA    RF,BOXROWS(RF)                                                   
         MVI   0(RF),C'B'          Mark bottom                                  
*                                                                               
FOOTUP04 TM    FMTFOOT,FMTFTON     Print foot?                                  
         BO    FOOTUP05                                                         
         TM    FMTPOPT2,RPFSTRPE+RPFOSHDE    Box shading?                       
         BNZ   FOOTUP05                                                         
         MVI   DEBUGPRT,94                                                      
         TM    FMTPOPT1,RPFBOX     Print boxes?                                 
         BZ    FOOTUP90                                                         
         GOTO1 REPORT              Close last box                               
         B     FOOTUP90                                                         
*                                                                               
FOOTUP05 SR    R3,R3                                                            
         IC    R3,DETMAX           LAST LINE TO PRINT TO                        
         SR    R3,R1                                                            
         BZ    FOOTUP15            CANNOT PRINT BLANK LINES                     
         BP    *+6                                                              
         DC    H'00'               WOOPS PASSED BOTTOM - GOT A BUG              
         STC   R3,SPACING          SKIP TO LAST DETAIL LINE                     
         TM    FMTPOPT1,RPFBOX     PRINT BOXES?                                 
         BZ    FOOTUP10            NO,  PRINT REST OF PAGE                      
         TM    FMTPOPT3,RPFTEDIT   TIME SHEET EDIT ?                            
         BZ    FOOTUP10            NO,  SKIP                                    
         TM    FMTPOPT3,RPFXTBOX   EXTRA BOXES FOR REPORTING ?                  
         BZ    FOOTUP10            NO,  SKIP                                    
*                                  ************************************         
*                                  * PRINT BOX LINES EVERY OTHER LINE *         
*                                  ************************************         
         SRL   R3,1                DIVIDE IN HALF                               
         LTR   R3,R3                                                            
         BNP   FOOTUP10                                                         
         SR    R1,R1                                                            
         IC    R1,LINE                                                          
*&&US*&& LA    R1,BOXROWS(R1)                                                   
*&&UK*&& LA    R1,BOXROWS+1(R1)                                                 
*                                                                               
FOOTUP08 MVI   0(R1),C'M'          BOX CRAZZZY                                  
         LA    R1,2(,R1)                                                        
         BCT   R3,FOOTUP08                                                      
*                                                                               
FOOTUP10 MVI   DEBUGPRT,5          DEBUG LOCATION 5                             
         GOTO1 REPORT              PRINT SPACING BEFORE BOTTOM                  
         MVI   SPACING,1           SINGLE SPACE                                 
*                                                                               
FOOTUP15 TM    FMTPOPT1,RPFBOX     IF BOXES THEN PRINT BOTTOM OF BOX            
         BZ    FOOTUP18                                                         
         CLC   LINE,DETMAX                                                      
         BH    FOOTUP18                                                         
         MVI   DEBUGPRT,99         DEBUG LOCATION 99                            
         GOTO1 REPORT              THIS PRINTS BOTTOM OF BOX                    
*                                                                               
FOOTUP18 MVI   SPACING,1           DEFAULT TO SINGLE SPACE                      
         SR    R3,R3                                                            
         ICM   R3,3,FOOT#          NUMBER OF FOOTLINES TO PRINT                 
         BZ    FOOTUP90                                                         
         MVI   DEBUGPRT,6          DEBUG LOCATION 6                             
         GOTO1 REPORT              PRINT A SPACE                                
*                                                                               
FOOTUP20 LA    R1,4                PRINT MAX OF FOUR LINES AT A TIME            
         CR    R1,R3               WHICH IS SMALLER                             
         BL    *+6                                                              
         LR    R1,R3               R3 = ACTUAL NUMBER OF PRINT LINES            
         LA    RE,XP                                                            
FOOTUP25 CLC   0(STDPGWD,R2),XSPACES                                            
         BNE   *+8                                                              
         MVI   0(R2),X'00'         FORCE A BLANK LINE                           
         MVC   0(STDPGWD,RE),0(R2)                                              
         LA    R2,STDPGWD(,R2)                                                  
         LA    RE,STDPGWD(,RE)                                                  
         BCTR  R3,0                ONE LESS LINE TO PRINT                       
         BCT   R1,FOOTUP25         LOOP TO PRINT UP TO FOUR                     
         MVI   DEBUGPRT,7          DEBUG LOCATION 7                             
         GOTO1 REPORT              PRINT THIS GROUP                             
         LTR   R3,R3               ANY MORE LEFT?                               
         BNZ   FOOTUP20            YUP                                          
*                                                                               
FOOTUP90 TM    PRTCNTRL,PRTEOR     END OF REPORT ?                              
         BO    FOOTUP99            YES                                          
         MVI   LINE,199            FORCE NEW PAGE                               
*                                                                               
FOOTUP99 NI    PRTCNTRL,TURNOFF-PRTSOR     START OF REQUEST                     
         B     RPTXIT                                                           
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
*  ROUTINE PROCESS SORT RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING ROWD,R2                                                          
         USING COLD,R3                                                          
         USING FMTRECD,R5                                                       
PROCREC  NTR1                                                                   
         L     R7,ASRTWRK                                                       
         SR    R0,R0                                                            
         ICM   R0,1,FMT#ACMS                                                    
         BZ    PROCRC40                                                         
*                                  * ELIMINATE SUPERFLUOUS ACCOUTNS *           
         TM    FMTPOPT1,RPFIACT    Print inactive accounts ? $ all = 0          
         BO    PROCRC40            Yes                                          
         TM    CUR.RECIND,RECKEEP  Keep this no matter what ?                   
         BO    PROCRC40            Yes                                          
         L     R3,FMTCOL                                                        
         IC    R0,FMT#COLS                                                      
         SR    R1,R1               DON'T BRANCH EVER R1=0                       
         TM    FMTPOPT2,RPFIABUD   INACTIVE BUDGETS                             
         BZ    PROCRC10                                                         
         LA    R1,BZ               CHECK ONLY BUDGETS COLS                      
         L     RE,ABUDWRK                                                       
         ZIC   R2,CUR.RECLVL       IS BUDGET MERGED WITH ACTUALS?               
         BZ    PROCRC11            YES IF ZERO                                  
         CLI   CUR.RECTYPE,RECBUDG IS THIS A BUDGET TYPE RECORD?                
         BE    PROCRC11            YES SO CHECK ACCUMS                          
         MHI   R2,ROWLNQ                                                        
         A     R2,FMTROW           POINT TO ROW IN ROW TABLE                    
         LH    RF,ROWKYDSP                                                      
         BCTR  RF,0                                                             
         EXCLC RF,0(RE),0(R7)      IS BUDGET FOR NEXT REOCORD?                  
         BE    PROCRC40            YES SO OK                                    
         B     PROCRC90            NO  SO SKIP                                  
*                                                                               
PROCRC10 TM    FMTPOPT2,RPFIATRN   INACTIVE ACTUALS (TRANSACTIONS)              
         BZ    PROCRC11                                                         
         LA    R1,BNZ              CHECK ONLY ACTUALS COLS                      
         CLI   CUR.RECTYPE,RECBUDG DON'T BOTHER IF BUDGETED RECORD              
         BNE   PROCRC11                                                         
         SR    R2,R2                                                            
         IC    R2,CUR.RECLVL       GET ROW LEVEL FOR BUDGETED REC.              
         MHI   R2,ROWLNQ                                                        
         A     R2,FMTROW           POINT TO ROW IN ROW TABLE                    
         L     RE,ALSORT           POINT TO NEXT RECORD                         
         LH    RF,ROWKYDSP                                                      
         BCTR  RF,0                                                             
         EXCLC RF,0(RE),0(R7)      IS BUDGET FOR NEXT REOCORD?                  
         BE    PROCRC40            YES SO PUT IN ROW  ACCUM                     
         NI    PRINTFG,NOPRINT     NO  SO DON'T  PROCESS THIS BUDGET            
         B     PROCRC80                                                         
*                                                                               
PROCRC11 TM    COLIND2,COLRATE     IS THIS A RATE COLUMN ?                      
         BZ    PROCRC12            YES SO CHECK                                 
         TM    COLFLAGS,COLHIDE    IF HIDDEN COLUMN THEN DON'T WORRY            
         BZ    PROCRC13                                                         
         B     PROCRC15            SKIP ZERO CHECK                              
*                                                                               
PROCRC12 TM    COLFLAGS,COLAMT                                                  
         BZ    PROCRC15            NEXT COLUMN                                  
*&&UK*&& TM    COLIND4,COLNCZAM                                                 
*&&UK*&& BO    PROCRC15            DON'T CHECK CREDIT LIMIT COLUMNS             
         TM    COLFLAGS,COLCALC                                                 
         BO    PROCRC14            DON'T CHECK CALCULATED COLUMNS               
*                                                                               
PROCRC13 TM    COLFLAGS,COLBUD     IS IT BUDGEDED                               
         EX    R1,*+8                                                           
         B     PROCRC14                                                         
         BC    B'0000',PROCRC15    DON'T CHECK                                  
*                                                                               
PROCRC14 DS    0H                                                               
*&&US                                                                           
         TM    COLIND4,COLCFAV     Cashflow Average Days?                       
         BZ    *+16                . no                                         
         TM    CFKIND,CFKCR+CFKDR  Any amount keywords?                         
         BZ    PROCRC30                                                         
         OI    PRINTFG,PACCUM      Add to accums even if no print               
*&&                                                                             
         TM    COLOPT,COLCZERO     SKIP THESE COLUMNS ONLY                      
         BO    PROCRC15            NEXT                                         
         LR    R4,R7                                                            
         AH    R4,COLDSP           POINT TO ACCUM                               
         OC    0(PKLEN,R4),0(R4)                                                
         BZ    PROCRC15                                                         
*&&US                              IF TIME ACCOUNT & ZERO HOURS, THEN           
         CLI   QUNIT,C'1'          PRINT IT FOR ZERO HOUR TIMESHEETS            
         BNE   PRCRC14F                                                         
         CLI   QLEDGER,C'R'                                                     
         BNE   PRCRC14F                                                         
         TM    FMTPOPT3,RPFSZERO   SHOW ZERO HR TIMESHEETS                      
         BO    PROCRC30            HAVE DATA KEEP IT                            
*                                                                               
PRCRC14F CP    0(PKLEN,R4),PKZERO  CHECK TO SEE IF ALL COLS ARE ZERO            
         BNE   PROCRC30            HAVE DATA KEEP IT                            
*&&                                                                             
*                                                                               
PROCRC15 LA    R3,COLLNQ(,R3)                                                   
         BCT   R0,PROCRC11                                                      
*&&US*&& TM    FMTROPT6,RPFZPST    Print $0 transactions?                       
*&&US*&& BO    PROCRC30                                                         
         NI    PRINTFG,NOPRINT     DON'T PRINT LINE                             
*                                                                               
         TM    PRINTFG,PACCUM                                                   
         BZ    PROCRC16                                                         
         L     R2,FMTLROW          KEEP ACTIVITY OF ACCTS FOR EACH ROW          
         ZIC   R1,FMT#ROWS         TO KEEP TRACK OF SUPERFLUOUS TOTAL           
         MVI   ROWNREC,2           FORCE THIS TOTAL TO PRINT                    
         SHI   R2,ROWLNQ           BUMP BACK THROUGH ROWS                       
         BCT   R1,*-8              LOOP TO NEXT ROW                             
         B     PROCRC76                                                         
*                                                                               
PROCRC16 TM    FMTPOPT2,RPFIATRN   INACTIVE ACTUALS (TRANSACTIONS)              
         BZ    PROCRC80                                                         
         CLI   PRV.RECTYPE,RECBUDG WAS PREVIOUS RECORD A BUDGET?                
         BNE   PROCRC80            NO SO DON'T CARE                             
         SR    R2,R2                                                            
         IC    R2,PRV.RECLVL       GET ROW LEVEL FROM PREV REC.                 
         MHI   R2,ROWLNQ                                                        
         A     R2,FMTROW           POINT TO ROW IN ROW TABLE                    
         L     R7,ALSORT           LOOK AT NEXT POSSIBLE RECORD                 
         L     RE,APRVWRK                                                       
         LH    RF,ROWKYDSP                                                      
         BCTR  RF,0                                                             
         EXCLC RF,0(RE),0(R7)      IS BUDGET FOR SAME RECORD?                   
         BE    PROCRC80            YES SO OK SO FAR                             
*                                                                               
         L     R6,ROWACM           Clear out previous budget from               
         SHI   R2,ROWLNQ              accumulators                              
         SR    R0,R0                                                            
         IC    R0,FMT#ACMS                                                      
PROCRC20 LR    RF,R6                                                            
         CLI   ROWNUM,1            First row?                                   
         BE    PROCRC25                                                         
         SHI   R2,ROWLNQ           Bump back one row                            
         L     R4,ROWACM                                                        
         SP    0(PKLEN,R4),0(PKLEN,RF)                                          
         LA    R4,PKLEN(,R4)                                                    
         LA    RF,PKLEN(,RF)                                                    
         BCT   R0,*-14                                                          
         CLI   ROWSTATE,ROWIDLE                                                 
         BNE   PROCRC20                                                         
*                                                                               
PROCRC25 IC    R0,FMT#ACMS                                                      
         ZAP   0(PKLEN,R6),=P'0'                                                
         LA    R6,PKLEN(,R6)                                                    
         BCT   R0,*-10                                                          
         B     PROCRC80                                                         
*                                                                               
PROCRC30 TM    FMTPOPT2,RPFIABUD                                                
         BZ    PROCRC40                                                         
         EXMVC RF,0(RE),0(R7)                                                   
***********************************************************************         
*  PRINT MIDLINES                                                     *         
***********************************************************************         
         SPACE 1                                                                
PROCRC40 MVI   SKIPLINE,0          SKIP N=1 LINES                               
         TM    PRINTFG,PDETAIL     WAS THE LAST PRINT LINE A DETAIL?            
         BO    *+8                 YES                                          
         MVI   SKIPLINE,1              SKIP ONE LINE FOR MIDLINE                
         OI    BLDACT,BLDPRV           SAVE PREVIOUS WHEN TIME                  
         MVI   PRINTFG,PDETAIL+PPRINT  PRINT DETAIL LINE                        
         BAS   RE,MIDPRT               PRINT MIDLINE BEFORE DETAIL              
         CLI   CUR.RECTYPE,RECNORM     IS IT NORMAL RECORD?                     
         BL    PROCRC55                NO                                       
*                                                                               
         L     R2,FMTLROW          KEEP ACTIVITY OF ACCTS FOR EACH ROW          
         ZIC   R1,FMT#ROWS         TO KEEP TRACK OF SUPERFLUOUS TOTAL           
*                                                                               
PROCRC50 CLI   ROWNREC,2           COUNT UP TO 2 ONLY                           
         BE    PROCRC55            FINISHED FOR NOW                             
         ZIC   R0,ROWNREC          NUMBER OF RECS PROCESSED PER ROW             
         AHI   R0,1                +1 TO NUMBER OF RECORDS PROCESSED            
         STC   R0,ROWNREC          SAVE IN ROW INFO                             
         SHI   R2,ROWLNQ           BUMP BACK THROUGH ROWS                       
         BCT   R1,PROCRC50         LOOP TO NEXT ROW                             
*                                                                               
*                                  TURN OFF AT END OF PAGE AND                  
PROCRC55 NI    PAGEFLAG,TURNOFF-PAGE@END-PAGEACOL  ALL COLS FORMATTED           
         MVI   STACKLVL,0          Initialize to no level being stacked         
         SR    RF,RF                                                            
         IC    RF,LINE             CURRENT LINE NUMBER                          
         SR    RE,RE                                                            
         IC    RE,DETMAX           MAX LINE NUMBER FOR DETAIL LINE              
         SR    RE,RF               RE = # OF LINE WE HAVE FOR PRINTING          
         IC    RF,SKIPLINE                                                      
         SR    RE,RF               LESS NUMBER OF LINES TO SKIP                 
*        AHI   RE,-1                                                            
         BP    *+8                 ALL DETIAL, SINCE PAGE UP WILL OCCUR         
         OI    PAGEFLAG,PAGERSET   PRINT ALL DETAILS SINCE NEW PAGE ?           
         STC   RE,LINE2END         NUMBER OF LINES TO END OF PAGE               
*        CLC   LINE2END,FMTSTKHT   MAX STACKING HEIGHT                          
*        BH    *+8                 WILL STACKED COLUMNS FORCE NEW PAGE?         
*        OI    PAGEFLAG,PAGERSET   YES, SO PRINT ALL DETIALS ?                  
         TM    PAGEFLAG,PAGERSET   DID WE SET OR WAS IT ON ?                    
         BO    PROCRC60            YES, SO NO NEED TO CHECK END OF PAGE         
         TM    FMTPOPT1,RPFRDAT    DO I WANT REDUNDANT DATA IN COLUMN?          
         BO    PROCRC60            YES, SO NO NEED TO CHECK END OF PAGE         
         CLI   LINE2END,3          MAX LINES NEEDED TO PRINT                    
         BNL   PROCRC60            WE'RE OK FOR NOW                             
         OI    PAGEFLAG,PAGE@END   SAY WE ARE AT END OF PAGE                    
*                                                                               
PROCRC60 MVC   SVR3(4),FMTCOL             Initialize SVR3 to begining           
         SR    R6,R6                                                            
         IC    R6,FMT#COLS                # of cols to process to print         
         MVI   XP#LINES,0                 # of lines on page to print           
         NI    PAGEFLAG,TURNOFF-PAGECSKP  Column will not be printed            
*                                                                               
PROCRC65 L     R3,SVR3             Load column working on                       
         ZIC   RF,COLPRTSQ         Print processing sequence                    
         TM    DDLNKIND,DDLNKON+DDLNKTST                                        
         BZ    *+8                                                              
         IC    RF,COLDDLSQ         DDLINK processing sequence                   
         BCTR  RF,0                                                             
         MHI   RF,COLLNQ                                                        
         A     RF,FMTCOL           POINT TO COLUMN TO PRINT IN SEQ.             
         LA    R3,COLLNQ(,R3)      BUMP TO NEXT COLUMN AND SAVE IT              
         ST    R3,SVR3                                                          
         LR    R3,RF               Point to actual column                       
                                                                                
         TM    COLFLAGS,COLHIDE    Is this a hidden column ?                    
         BO    PROCRC75            yes, so don't output                         
         TM    DWNOPT1,DWNGO       Download ?                                   
         BZ    PROCRC68            No                                           
         CLC   STACKLVL,COLSCLVL   Process columns at this level                
         BNE   PROCRC75            Next column                                  
         ST    R3,ACURCOL                                                       
         MVC   ACURROW,COLAROW                                                  
         GOTO1 =A(PRTFORM),PRTDTAIL                                             
         B     PROCRC75            Next column                                  
*                                                                               
PROCRC68 TM    COLFLAGS,COLAMT     An amount column ?                           
         BO    PROCRC70            Yes, always output amount columns            
         TM    FMTPOPT1,RPFRDAT    Do we want redundant data ?                  
         BO    PROCRC70            Yes, so output the column                    
         TM    COLOPT2,COLFCPRT    Output this column No-matter what ?          
         BO    PROCRC70            Yes                                          
*                                                                               
         L     R2,COLAROW          POINT TO ROW INFO FOR COL                    
         SR    R4,R4                                                            
         ICM   R4,3,ROWKYSZ        SIZE OF FIELD IN SORTKEY                     
         LH    R1,ROWKYDSP         LENGTH OF SORTKEY UP TO FIELD                
         AR    R1,R4               SORTKEY PLUS CURRENT SORT POSITION           
         BCTR  R1,0                LESS ONE FOR RECAP #                         
*                                                                               
         CLI   PRV.RECTYPE,RECNORM WAS PREVIOUS RECORD A BUDGET RECORD          
         BL    PROCRC70            YES SO PRINT DETAIL                          
         TM    PAGEFLAG,PAGERSET   PRINT ALL DETAILS SINCE NEW PAGE ?           
         BO    PROCRC70            YES, PRINT THE DETAILS                       
         L     RE,ASRTWRK          A(SRTWRK)                                    
         L     R0,APRVWRK          A(PRVWRK)                                    
         LR    RF,R1               COMPARE PART OF SORT KEY                     
         CLCL  RE,R0               IF PREVIOUS KEY ^= CURRENT KEY THEN          
         BNE   PROCRC70            PRINT THE COLUMN                             
         OI    PAGEFLAG,PAGECSKP   COLUMN WILL NOT BE PRINTED                   
         B     PROCRC75                                                         
*                                                                               
PROCRC70 TM    COLFLAGS,COLSUPDL   SUPPRESS DETAIL LINES?                       
         BZ    PROCRC74            NO, PRINT LINE                               
*---------------------------------------------------------------------*         
*  THIS MAKES SURE THAT I PRINT THE DATA A LEAST ONCE BEFORE I        *         
*       CAN SKIP PRINTING IT.                                         *         
*---------------------------------------------------------------------*         
         L     R2,FMTLROW          LOAD LAST ROW                                
         SR    R0,R0                                                            
         IC    R0,FMT#ROWS                                                      
PROCRC71 CLI   ROWNREC,1           MORE THAN ONE RECORD PRINTED?                
         BH    PROCRC75            YES, SO DON'T LOOK AHEAD FOR TOTAL           
         TM    ROWFLAGS,ROWTOTAL   DO I TOTAL ON THIS ROW?                      
         BO    PROCRC73            YES                                          
PROCRC72 SHI   R2,ROWLNQ           BOUNCE BACK ONE ROW                          
         BCT   R0,PROCRC71                                                      
         B     PROCRC74                                                         
*                                                                               
PROCRC73 ICM   RF,15,COLRTOTS      Suppress column total                        
         BZ    PROCR73A            No                                           
         LA    R1,1                                                             
         ZIC   RE,ROWNUM                                                        
         BCTR  RE,0                                                             
         SLL   R1,0(RE)            Find row for column                          
         NR    RF,R1               Isolate bit for test                         
         BNZ   PROCRC72            Yes suppress total                           
*                                                                               
PROCR73A AHI   R2,ROWLNQ           BOUNCE UP ONE ROW FOR ROWSZ + DSP            
         LH    R1,ROWKYDSP         GET LENGTH TO COMPARE FOR                    
         BCTR  R1,0                LESS ONE FOR RECAP #                         
         L     RE,ASRTWRK          CURRENT SORT RECORD (PRINTING NOW)           
         L     R0,ALSORT           NEXT SORT RECORD TO PRINT                    
         LR    RF,R1                                                            
         CLCL  RE,R0                                                            
         BE    PROCRC75            DON'T RE-PRINT DETAILS                       
*                                                                               
PROCRC74 GOTO1 =A(PRTFORM),PRTDTAIL   DETAIL PRINT                              
         TM    PAGEFLAG,PAGE@END                                                
         BZ    PROCRC75                                                         
         CLC   XLINES,LINE2END               LINES NEED VS. LINES HAVE          
         BNH   PROCRC75                      CONTINUE AS USUAL                  
         NI    PAGEFLAG,TURNOFF-PAGE@END     OK TO PRINT ALL DETAIL             
         OI    PAGEFLAG,PAGERSET             FORMAT ALL DETAILS                 
         B     PROCRC60                      RE-FORMAT FULL LINE                
*                                                                               
PROCRC75 BCT   R6,PROCRC65                                                      
*                                                                               
         TM    DWNOPT1,DWNGO       Download ?                                   
         BZ    PROCRC76            No                                           
         CLC   STACKLVL,FMTSTKHT   Process all stack levels ?                   
         BE    PROCRC76            Finished so countinue as before              
         BL    *+6                 Print stacked level line                     
         DC    H'00'               Should never go high                         
*                                                                               
         BAS   RE,PRINTIT          Print detail                                 
         SR    RF,RF               Increase the stack level                     
         IC    RF,STACKLVL         Process this new level of columns            
         AHI   RF,1                                                             
         STC   RF,STACKLVL         Process this new level of columns            
                                                                                
         USING ACRL2D,R7                                                        
         TM    DWNOPT1,DWNROWS     Download rows ?                              
         BZ    PROCRC60            No                                           
         SR    R0,R0                                                            
         ICM   R0,1,FMT#VROW       Number of rows to dummy up                   
         BZ    PROCRC60                                                         
PROCRC7X OI    DWNOPT2,DWNDUMF        Download a dummy field                    
         GOTO1 =A(DWNLOAD),DWNTEXT    Blank field                               
         BCT   R0,PROCRC7X                                                      
         B     PROCRC60            Repeat process again for next level          
***********************************************************************         
*  CLEAR ROW ACCUMULATOR TOTALS FOR THOUGHS MARKED 'C'                *         
***********************************************************************         
         SPACE 1                                                                
PROCRC76 TM    PAGEFLAG,PAGERSET   RESET PRINT DETAILS ?                        
         BZ    *+8                 NO, SKIP                                     
         OI    PAGEFLAG,PAGEACOL   ALL COLUMNS FORMATTED                        
         TM    PAGEFLAG,PAGECSKP   ANY COLUMN NOT PRINTED ?                     
         BO    *+8                 YES, SKIP                                    
         OI    PAGEFLAG,PAGEACOL   ALL COLUMNS FORMATTED                        
         NI    PAGEFLAG,TURNOFF-PAGE@END-PAGERSET                               
         BAS   RE,CLRTOTS                                                       
         SR    R1,R1                                                            
         ICM   R1,1,FMT#ACMS                                                    
         BZ    PROCRC90                                                         
         CLI   CUR.RECTYPE,RECNORM IS IT NORMAL RECORD?                         
         BNL   PROCRC80            YES                                          
*                                                                               
         SR    R2,R2               MUST BE BUDGET                               
         ICM   R2,1,CUR.RECLVL     GET ROW LEVEL TO TOTAL ON                    
         BNZ   *+6                                                              
         DC    H'0'                CAN'T TOTAL ON A NEGATIVE ROW NUMBER         
         LR    RE,R2                                                            
         BCTR  R2,0                ADDJUST BY ONE                               
         MHI   R2,ROWLNQ           POINT TO ROW LEVEL TOTAL                     
         A     R2,FMTROW           ADD BASE OF ROW TABLE                        
*                                                                               
         MVI   ROWNREC,2           FORCE REC THIS TOTAL TO PRINT                
         L     R6,ROWACM           POINT AT ACCUMULATOR ROW TOTALS              
         LR    R4,R7               POINT TO ASRTWRK                             
         AH    R4,SRTDATLN         POINT TO ACCUMULATORS (SORT REC)             
PROCRC78 MVC   0(PKLEN,R6),0(R4)   FILL IN ACCUMS                               
         OC    0(PKLEN,R4),0(R4)                                                
         BNZ   *+10                                                             
         ZAP   0(PKLEN,R6),PKZERO                                               
         LA    R6,PKLEN(,R6)       BUMP TO NEXT ACCUM                           
         LA    R4,PKLEN(,R4)       BUMP TO NEXT ACCUM                           
         BCT   R1,PROCRC78         DECRIMENT BY ONE                             
         NI    PRINTFG,NOPRINT                                                  
*                                                                               
         CHI   RE,1                Already on row 1?                            
         BE    PROCRC80            yes                                          
         SHI   RE,1                                                             
         SHI   R2,ROWLNQ           bump back to previous row                    
PROCRC79 CLI   ROWNREC,2           COUNT UP TO 2 ONLY                           
         BE    PROCRC80            FINISHED FOR NOW                             
         ZIC   R0,ROWNREC          NUMBER OF RECS PROCESSED PER ROW             
         AHI   R0,1                +1 TO NUMBER OF RECORDS PROCESSED            
         STC   R0,ROWNREC          SAVE IN ROW INFO                             
         SHI   R2,ROWLNQ           BUMP BACK THROUGH ROWS                       
         BCT   RE,PROCRC79         LOOP TO NEXT ROW                             
*                                                                               
PROCRC80 L     R2,FMTLROW                                                       
         L     R3,FMTCOL                                                        
         SR    R1,R1                                                            
         IC    R1,FMT#COLS         NUMBER OF COLUMNS TO LOOK AT                 
*                                                                               
PROCRC82 TM    COLFLAGS,COLAMT                                                  
         BZ    PROCRC88                                                         
         TM    COLFLAGS,COLCALC                                                 
         BZ    PROCRC84                                                         
*                                                                               
         L     R6,COLINDEX                                                      
         CLC   COLDD#,=AL2(AC#RSCME)                                            
         BNE   PROCRC84                                                         
         LR    R4,R7                                                            
         AH    R4,COLDSP           SWAP BACK VALUE SO TOTALS AS NORMAL          
         XC    COLDUB,0(R4)                                                     
         XC    0(PKLEN,R4),COLDUB                                               
         XC    COLDUB,0(R4)                                                     
*                                                                               
PROCRC84 L     R6,ROWACM                                                        
         AH    R6,COLACM           POINT TO ACCUMULATOR IN ROW                  
*                                                                               
PROCRC86 CLI   CUR.RECTYPE,RECSTMS DON'T ADD SAVED TIME SHEET TO TOTAL          
         BE    PROCRC88                                                         
         TM    PRINTFG,PACCUM      USE TO SAY ADD TO ACCUMS                     
         BO    *+12                                                             
         TM    PRINTFG,PPRINT      USE TO SAY NOT TO ADD TO ACCUMS              
         BZ    PROCRC88                                                         
         LR    R4,R7               POINT TO SORT RECORD                         
         AH    R4,COLDSP           PUT SORT RECS ACCUMS IN DETAIL ROW           
         MVC   0(PKLEN,R6),0(R4)   MOVE AMOUNT TO ROW DETIAL LINE               
*                                                                               
PROCRC88 LA    R3,COLLNQ(,R3)      NEXT COLUMN                                  
         BCT   R1,PROCRC82                                                      
*                                                                               
PROCRC90 TM    BLDACT,BLDPRV       DID WE WANT TO PROCESS RECORD ?              
         BO    PROCRC92            YES                                          
         OI    FMTSW,FMTNOACT                                                   
         B     RPTXIT              NO                                           
*                                                                               
PROCRC92 MVI   DEBUGRT#,3          DEBUG LOCATION 3                             
         BAS   RE,PRINTIT          Print detail                                 
         B     RPTXIT                                                           
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
***********************************************************************         
*  ROUND AMOUNTS, TO DOLLAR, HUNDREDS, THOUSANDS                      *         
***********************************************************************         
         USING ROWD,R2                                                          
         USING COLD,R3                                                          
         USING FMTRECD,R5                                                       
ROUND    NTR1                                                                   
         L     R3,FMTCOL           Point to start of column table               
         SR    R0,R0                                                            
         IC    R0,FMT#COLS         Number of columns                            
         L     R4,ASRTWRK          Sort record                                  
         AH    R4,SRTDATLN         Point to start of accumulators               
         LR    R6,R4               Save base of accumulator line                
*                                                                               
ROUND10  TM    COLIND2,COLRATE     Is it a rate type column ?                   
         BO    ROUND12             Yes                                          
         TM    COLFLAGS,COLAMT     Is it an accumulated column ?                
         BZ    ROUND30             no, next column                              
*                                                                               
ROUND12  CLI   COLROUND,0          Rounding for this column ?                   
         BE    ROUND30             No                                           
         TM    COLOPT2,COLNORND    Ignore rounding at this time ?               
         BO    ROUND30             Yes                                          
         TM    COLIND2,COLRNDBF    Did we round now ?                           
         BZ    ROUND30             No, latter                                   
*        TM    COLEDOPT,COLPCT     Is it a percent type column ?                
*        BO    ROUND30             Yes, so leave alone for now                  
         AH    R4,COLACM           Point to exact accumulator                   
         TM    COLIND2,COLRATE     Was this a rate column ?                     
         BZ    ROUND18             No, so countinue                             
         ICM   R2,15,COLAROW       Yes, point to row                            
         BZ    ROUND22             Some thing not right so skip                 
         L     R4,ASRTWRK          Point to being of sort record                
         AH    R4,ROWKYDSP         Point to rate since it's in sort key         
*                                                                               
ROUND18  OC    0(PKLEN,R4),0(R4)   If binary zeros error flaged set             
         BZ    ROUND22             Error, jump around it                        
         SR    R1,R1                                                            
         IC    R1,COLROUND         Round factor                                 
         SRP   0(PKLEN,R4),0(R1),5 Round accordingly                            
         LA    RF,64                                                            
         SR    RF,R1                                                            
         SRP   0(PKLEN,R4),0(RF),0 Shift back                                   
*                                                                               
ROUND22  LR    R4,R6               Restore base of accumulator line             
*                                                                               
ROUND30  LA    R3,COLLNQ(,R3)      Bump to next column                          
         BCT   R0,ROUND10          Countinue for all columns                    
         B     RPTXIT                                                           
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
***********************************************************************         
*  ROUTINE TO PRINT ACCOUNT TOTALS                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING ROWD,R2                                                          
         USING COLD,R3                                                          
         USING FMTRECD,R5                                                       
         USING ACRL2D,R7                                                        
RECTOT   NTR1                                                                   
         L     R7,AACRL2D          2ND  STORAGE AREA ACRL2D                     
         L     RE,0(,R1)           LOAD RECORD  TO   PROCESS                    
         ST    RE,SVREG            SAVE A(SORT RECORD TO PROCESS)               
         AH    RE,SRTREPNO                                                      
         SR    R1,R1                                                            
         ICM   R1,1,0(RE)                                                       
         BNZ   *+6                                                              
         DC    H'00'               RE-THINK THIS IF DIED                        
*                                                                               
         BCTR  R1,0                                                             
         MHI   R1,FMTLNQ                                                        
         A     R1,AFORMATS                                                      
         LR    R5,R1                                                            
*                                                                               
         NI    PAGEFLAG,TURNOFF-PAGENEW-PAGE#01-PAGEHOLD                        
         MVI   COUNT,0             SET TO ZERO FOR SKIPLINES                    
         NI    DWTOTST,TURNOFF-DWTROWBL   DON'T DOWNLOAD ROWS AS BLANK          
*                                                                               
         L     R6,ALSORT                                                        
         L     R2,FMTLROW                                                       
         CLI   FMTTOP,0            Recapped report ?                            
         BE    RECTOT10            No                                           
         LTR   RE,R6               Next record (Peek ahead)                     
         BZ    RECTOT10            No more records                              
                                                                                
         USING RECXD,RE                                                         
         AH    RE,SRTKEYXT                                                      
         TM    RECIND,RECTOTS      Is specail total record ?                    
         BZ    RECTOT10            No                                           
*                                                                               
SORT     USING FMTRECD,R1                                                       
         SR    R1,R1                                                            
         ICM   R1,1,RECRPT#        Get report #                                 
         BNZ   *+6                                                              
         DC    H'00'               Oh boy                                       
         DROP  RE                                                               
*                                                                               
         MHI   R1,FMTLNQ                                                        
         A     R1,AFORMATS                                                      
         CLC   FMTTOP,SORT.FMTTOP  Same recapped grouping ?                     
         BNE   RECTOT10            No                                           
         OI    PAGEFLAG,PAGEHOLD   Hold up pageing for total line               
         DROP  SORT                                                             
*                                                                               
RECTOT10 CLI   ROWNUM,1            First row of report ?                        
         BNE   RECTOT11            No                                           
         MVI   PIXROW#,0                                                        
         CLI   ROWNREC,0           Did we process anything at all ?             
         BE    RECTOT90            No                                           
*                                                                               
RECTOT11 L     R4,ROWACM           R4 = Current row of accums                   
         L     R3,FMTCOL           Point to column information                  
         CLI   ROWTYPE,ROWCNTR     Center heading                               
         BE    RECTO11A                                                         
         CLI   ROWTYPE,ROWLEFT     Left heading                                 
         BE    RECTO11A                                                         
         CLI   ROWTYPE,ROWRGHT     Right heading                                
         BNE   RECTOT12                                                         
*                                                                               
RECTO11A CLI   ROWNUM,1                                                         
         BE    RECTOT12            Don't go this far                            
         MVC   PIXROW#,ROWNUM      Save off number of last change row           
***********************************************************************         
*  EVALUATE ANY COLUMN CALCULATIONS FOR ROW                           *         
***********************************************************************         
         SPACE 1                                                                
RECTOT12 SR    R0,R0                                                            
         IC    R0,FMT#COLS         Number of columns to work on                 
*                                                                               
RECTOT13 TM    COLFLAGS,COLCALC                                                 
         BZ    RECTOT14                                                         
         TM    ROWFLAGS,ROWTOTAL                                                
         BZ    *+10                                                             
         ZAP   COLDUB,PKZERO       Yes, so clear running total                  
         C     R2,FMTLROW          If last row then                             
         BE    RECTOT14            We already processed calculations            
         CLI   ROWNREC,0           Was there activity                           
         BE    RECTOT14            No                                           
         TM    COLOPT,COLFCALC     Force calculation                            
         BZ    RECTOT14            No, do not evaluate                          
         GOTO1 EVAL,DMCB,(R3),ROWACM,0                                          
         LR    RE,R4               R4 = Begining of accum line                  
         AH    RE,COLACM           R6 = Where answer goes                       
*&&US                                                                           
         TM    COLIND4,COLCFAV     Show EEEs for totals?                        
         BO    *+14                                                             
*&&                                                                             
         OC    ANSWER,ANSWER       Was there an error ?                         
         BZ    RECTOT14            Yes                                          
         MVC   0(PKLEN,RE),ANSWER  No                                           
*                                                                               
RECTOT14 LA    R3,COLLNQ(,R3)      Bump up to next column                       
         BCT   R0,RECTOT13                                                      
         DROP  R3                                                               
*                                                                               
         CLI   ROWNREC,0           Was there activity ?                         
         BE    RECTOT40            No                                           
         CLI   ROWNUM,1            Is it last row ?                             
         BNE   RECTOT15            No                                           
         CLI   FMTTOP,0            Recapped report ?                            
         BE    RECTO14A            No                                           
         TM    CUR.RECIND,RECTOTS                                               
         BZ    RECTOT90                                                         
*                                                                               
RECTO14A TM    PAGEFLAG,PAGEHOLD   Hold up pageing for now ?                    
         BO    *+8                 Yes                                          
         OI    PAGEFLAG,PAGENEW    Page after printing totals                   
*                                                                               
         OI    DWTOTST,DWTTREQ+DWTROWBL    Turn on "Total For Req" &            
         TM    FMTPOPT3,RPFNORQT           No request total ?                   
         BO    RECTO14F                    Yes                                  
         MVC   UPFRONT(L'AC@TREQ),AC@TREQ  Down-load row field as C' '          
         CLI   FMTTOP,0                    Recapped report ?                    
         BE    RECTO14E                                                         
         CLC   FMTTOP,FMTNUM       Top report is this report ?                  
         BE    RECTO14E                                                         
         MVC   UPFRONT,SPACES                                                   
         MVC   UPFRONT(L'AC@TFORR),AC@TFORR                                     
         LA    RF,L'AC@TFORR                                                    
         LA    RE,UPFRONT-1(RF)                                                 
RECTO14B CLI   0(RE),C' '          Find last character                          
         BH    RECTO14C            Found                                        
         BCTR  RE,0                Bump backwards in string                     
         BCT   RF,RECTO14B         Count down length of string                  
         DC    H'00'                                                            
*                                                                               
RECTO14C LA    RE,2(,RE)                 Bump up to add blank character         
         MVC   0(L'FMTCODE,RE),FMTCODE   Add format code to print               
*                                                                               
RECTO14E BAS   RE,PRTTOT                   R2 is passed                         
*                                                                               
RECTO14F MVI   ROWNREC,0                                                        
         MVI   ROWSTATE,ROWCLEAR                                                
         MVC   UPFRONT,SPACES                                                   
         NI    DWTOTST,TURNOFF-DWTTREQ-DWTROWBL                                 
         B     RECTOT90                                                         
***********************************************************************         
*  ADD CURRENT ROW OF ACCUMS TO PREVIOUS ROW                          *         
***********************************************************************         
                                                                                
NEXT     USING ROWD,R3                                                          
                                                                                
RECTOT15 LR    R3,R2               SAVE R2, POINTING TO CURRENT ROW             
         AHI   R3,-ROWLNQ          BUMP BACK ONE ROW                            
         TM    ROWOPT,ROWNOREV                                                  
         BZ    *+8                                                              
         OI    NEXT.ROWOPT,ROWNOREV                                             
*                                                                               
         L     RE,NEXT.ROWACM      PREVIOUS OR NEXT ROW OF ACCUMS               
         SR    R1,R1                                                            
         ICM   R1,1,FMT#ACMS       NUMBER OF ACCUMS TO ADD                      
         BZ    RECTOT20                                                         
         LA    RF,FMT@COL#                                                      
         ST    RF,SVRF             Initialize                                   
RECTOT17 L     RF,SVRF                                                          
         SR    R0,R0                                                            
         IC    R0,0(,RF)                                                        
         MHI   R0,COLLNQ                                                        
         AHI   RF,1                Bump up for next time around                 
         ST    RF,SVRF             Set for next time around                     
         LR    RF,R0               Point to column entry                        
         A     RF,FMTCOL                                                        
*                                                                               
         USING COLD,RF                                                          
         OC    0(PKLEN,R4),0(R4)   Was there an error ?                         
         BZ    RECTOT18            Yes                                          
         TM    COLOPT2,COLFCPRT    Constant amount ?                            
         BZ    RECTO17D                                                         
         CLI   FMTROWLV,0          Any row level set ?                          
         BE    RECTO17D            No                                           
         CLC   ROWNUM,FMTROWLV     Add only to lower rows                       
         BNH   RECTO17D                                                         
         CP    0(PKLEN,RE),PKZERO                                               
         BNE   RECTOT18            Only add if not added to                     
         DROP  RF                                                               
*                                                                               
RECTO17D AP    0(PKLEN,RE),0(PKLEN,R4) Add current row to previous row          
*                                                                               
RECTOT18 LA    RE,PKLEN(,RE)           BUMP UP AN ACCUM IN PREV ROW             
         LA    R4,PKLEN(,R4)           BUMP UP AN ACCUM IN CURR ROW             
         BCT   R1,RECTOT17             REPEAT FOR R1 ACCUMS                     
         DROP  NEXT                                                             
*                                                                               
RPT2     USING FMTRECD,R1                                                       
                                                                                
RECTOT20 TM    FMTRCAP,FMTRWALL    SEPCIAL RECAP RULE TEST                      
         BZ    RECTOT25            NOT ON, SO DON'T CARE                        
         C     R2,FMTLROW          ONLY CHECK OF FIRST PASS                     
         BNE   RECTOT25            NOT ON LAST ROW, SO SKIP                     
         LTR   RE,R6               A(SORT RECORD FROM SORTER)                   
         BZ    RECTOT25            COULDN'T BE EQUAL, NO SORT RECORD            
         AH    RE,SRTREPNO                                                      
         SR    R1,R1                                                            
         ICM   R1,1,0(RE)                                                       
         BCTR  R1,0                                                             
         MHI   R1,FMTLNQ                                                        
         A     R1,AFORMATS         POINT TO FORMAT SPECS                        
         TM    RPT2.FMTRCAP,FMTMRGPG                                            
         BZ    RECTOT25            NOT GOING TO MERGE ON TO SAME PAGE           
         CLC   FMTTOP,RPT2.FMTTOP                                               
         BNE   RECTOT25            NOT IN SAME GROUPING                         
         LH    R1,ROWKYDSP                                                      
         BCTR  R1,0                LESS ONE FOR RECAP #                         
         LR    R0,R6               A(SORT RECORD FROM SORTER)                   
         L     RE,SVREG            A(SRTWRK) OR A(PRVWRK)                       
         LR    RF,R1               LENGTH TO COMPARE FOR                        
         CLCL  RE,R0                                                            
         BE    RECTOT40                                                         
         DROP  RPT2                                                             
*                                                                               
RECTOT25 TM    PAGEFLAG,PAGEHOLD   HOLD UP PAGEING FOR NOW                      
         BO    RECTOT30                                                         
         TM    ROWFLAGS,ROWNEWPG   SEE IF PAGE ON THIS LEVEL                    
         BZ    *+8                                                              
         OI    PAGEFLAG,PAGENEW    PAGE AFTER PRINTING TOTALS                   
         TM    ROWOPT,RRWPGNO      RESET PAGE TO 1                              
         BZ    *+8                                                              
         OI    PAGEFLAG,PAGE#01    SET PAGE NUMBER TO 1                         
*                                                                               
RECTOT30 OI    DWTOTST,DWTTFOR     TURN ON NEED "TOTAL FOR"                     
         MVC   UPFRONT(L'AC@TFOR),AC@TFOR                                       
         BAS   RE,PRTTOT           R2 IS PASSED                                 
         MVC   UPFRONT,SPACES                                                   
         NI    DWTOTST,TURNOFF-DWTTFOR  TURN OFF NEED "TOTAL FOR"               
*                                                                               
RECTOT40 CLI   QOPT3,C'+'                                                       
         BNE   RECTOT45                                                         
         LH    RF,ROWKYDSP                                                      
         AHI   RF,-2               LESS ONE FOR RECAP #                         
         L     RE,SVREG                                                         
         MVC   PFOURTH,SPACES                                                   
         MVC   PFOURTH(10),=C'SRTWRK(00)'                                       
         C     RE,APRVWRK                                                       
         BNE   RECTOT42                                                         
*        TM    FMTSW,FMTNOACT      NO ACTIVITY SINCE LAST RECORD                
*        BZ    RECTOT42                                                         
         MVC   PFOURTH(10),=C'PRVWRK(00)'                                       
*                                                                               
RECTOT42 SR    R1,R1                                                            
         IC    R1,ROWNUM                                                        
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PFOURTH+7(2),DUB                                                 
         EXMVC RF,P,0(RE)                                                       
         EXMVC RF,PSECOND,0(R6)                                                 
         LA    RF,1(,RF)                                                        
         LR    R0,RF                                                            
         BAS   RE,ROWLVLS                                                       
         GOTO1 PRNTBL,DMCB,(98,PFOURTH),P,C'DUMP',(R0),=CL2'2D'                 
         MVC   P,SPACES                                                         
         MVC   P(6),=C'SORTER'                                                  
         MVC   P+11(4),=C'PG=N'                                                 
         TM    PAGEFLAG,PAGENEW    NEW PAGE?                                    
         BZ    *+8                                                              
         MVI   P+14,C'Y'                                                        
         TM    PAGEFLAG,PAGEHOLD   HOLD PAGE?                                   
         BZ    *+8                                                              
         MVI   P+14,C'H'                                                        
         GOTO1 PRNTBL,DMCB,(98,P),PSECOND,C'DUMP',(R0),=CL2'2D'                 
*                                                                               
RECTOT45 MVI   ROWNREC,0           RESET TO NO RECORDS PRINTED                  
         MVI   ROWSTATE,ROWCLEAR   SET TO CLEAR NEXT TIME THROUGH               
         CLI   ROWTYPE,ROWMID      IS THIS ROW A MIDLINE ?                      
         BNE   *+8                 NO                                           
         MVI   ROWSTATE,ROWPTMID   YES, PRINT MID NEXT TIME THROUGH             
*                                                                               
         L     RE,SVREG            A(SRTWRK) OR A(PRVWRK)                       
*        TM    FMTSW,FMTNOACT      NO ACTIVITY SINCE LAST RECORD                
*        BZ    RECTOT55                                                         
*        L     RE,APRVWRK                                                       
*                                                                               
RECTOT55 LH    R1,ROWKYDSP                                                      
         BCTR  R1,0                LESS ONE FOR RECAP #                         
         AHI   R2,-ROWLNQ          BUMP BACK ONE ROW                            
         LTR   R0,R6               A(SORT RECORD FROM SORTER)                   
         BZ    RECTOT10            COULDN'T BE EQUAL, NO SORT RECORD            
         LR    RF,R1               LENGTH TO COMPARE FOR                        
         CLCL  RE,R0                                                            
         BNE   RECTOT10                                                         
*                                                                               
         TM    RECAPSW,RECAPON     IF RECAPING THEN                             
         BZ    RECTOT90                                                         
         LR    RE,R6               GET FORMAT # FROM SORT AND COMPARE           
         AH    RE,SRTREPNO                                                      
         OI    PAGEFLAG,PAGEHOLD                                                
         CLC   FMTNUM,0(RE)        IF NOT THE SAME PRINT OUT TOTALS             
         BNE   RECTOT10                                                         
*                                                                               
RECTOT90 NI    PAGEFLAG,TURNOFF-PAGEHOLD                                        
         LTR   R6,R6               WAS THERE A SORT RECORD ?                    
         BZ    RECTOT99            NO, SO FINISHED                              
         TM    PAGEFLAG,PAGENEW    NEW PAGE?                                    
         BZ    *+8                 NO                                           
         MVI   FORCEHED,YES                                                     
         TM    PAGEFLAG,PAGE#01    RESET PAGE NUMBER TO 1?                      
         BZ    *+10                NO                                           
         MVC   PAGE,=H'01'                                                      
         CLI   QOPT3,C'Q'                                                       
         BNE   RECTOT99                                                         
         MVC   P,SPACES                                                         
         MVC   P(3),=C'FH='                                                     
         MVC   P+3(1),FORCEHED                                                  
         GOTO1 PRINT,DMCB,P,=C'BL02'                                            
*                                                                               
RECTOT99 DS    0H                                                               
         NI    DWTOTST,TURNOFF-DWTROWBL  DOWN-LOAD ROW FLDS AS C' '             
         B     RPTXIT                                                           
         DROP  R5,R7                                                            
         EJECT                                                                  
         USING ROWD,R2                                                          
         USING FMTRECD,R5                                                       
ROWLVLS  NTR1                                                                   
         L     R2,FMTROW                                                        
         SR    RF,RF                                                            
         IC    RF,FMT#ROWS                                                      
         MVC   PFOURTH+15(4),=C'RPT='                                           
         MVC   PFOURTH+19(1),FMTNUM                                             
         OI    PFOURTH+19,X'F0'                                                 
         MVC   PFOURTH+22(4),=C'LVL='                                           
         LA    R4,PFOURTH+26                                                    
         SR    R1,R1                                                            
ROWLVL10 IC    R1,ROWNREC                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(1,R4),DUB                                                      
         MVI   1(R4),C','                                                       
         LA    R4,2(,R4)                                                        
         LA    R2,ROWLNQ(,R2)                                                   
         BCT   RF,ROWLVL10                                                      
         BCTR  R4,0                                                             
         MVI   0(R4),C' '                                                       
         B     RPTXIT                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*  CLEAR TOTALS IF MARKED                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING FMTRECD,R5                                                       
CLRTOTS  NTR1                                                                   
         SR    R1,R1                                                            
         SR    R6,R6                                                            
         IC    R1,FMT#ROWS         NUMBER OF ROW TO CHECK                       
         L     R2,FMTROW           POINT TO ROW INFO                            
CLRTOT10 CLI   ROWSTATE,ROWCLEAR   IF 'C' THEN CLEAR R1 ROWS                    
         BE    CLRTOT20                                                         
         LA    R2,ROWLNQ(,R2)      LOOP TO NEXT                                 
         BCT   R1,CLRTOT10                                                      
         B     CLRTOT40            EXIT, NO ROW TO CLEAR                        
*                                                                               
CLRTOT20 MVI   ROWSTATE,ROWIDLE    SET TO IDLE                                  
         L     R3,ROWACM           POINT TO ROW'S ACCUMS                        
         SR    R6,R6                                                            
         ICM   R6,1,FMT#ACMS       R6 ACCUMS TO CLEAR                           
         BZ    CLRTOT40                                                         
*                                                                               
         ZAP   0(PKLEN,R3),=P'0'   CLEAR                                        
         LA    R3,PKLEN(,R3)       BUMP TO NEXT ACCUM                           
         BCT   R6,*-10                                                          
*                                                                               
         LA    R2,ROWLNQ(,R2)      BUMP TO NEXT ROW AND CLEAR ACCUMS            
         BCT   R1,CLRTOT20         FOR R1 ROWS                                  
*                                                                               
CLRTOT40 B     RPTXIT                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*  PRINT TOTAL LINES IF SET                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING ROWD,R2                                                          
         USING FMTRECD,R5                                                       
         USING ACRL2D,R7                                                        
PRTTOT   NTR1                                                                   
         L     R7,AACRL2D                                                       
         NI    PRTOPT,TURNOFF-PRTNXTLN       Reset                              
         SR    R1,R1                                                            
         ICM   R1,1,FMT#ACMS       Are there accumultors to total for ?         
         BZ    PRTTOT90            No, so don't worry about it                  
*                                                                               
         L     RE,ROWACM                                                        
PRTTOT40 TM    FMTPOPT1,RPFZEROT   Print zero totals?                           
         BO    PRTTOT50            Yes                                          
         OC    0(PKLEN,RE),0(RE)                                                
         BZ    *+14                                                             
         CP    0(PKLEN,RE),PKZERO  * ELIMINATE TOTAL IF ALL ZEROS *             
         BNE   PRTTOT50                                                         
         LA    RE,PKLEN(,RE)                                                    
         BCT   R1,PRTTOT40                                                      
*                                                                               
         TM    ROWFLAGS,ROWTOTAL   Do we total on this row?                     
         BZ    PRTTOT90            No, so don't worry                           
         NI    PRINTFG,NOPRINT     Set no to print                              
         TM    PRINTFG,PTOTAL      Did we just print total?                     
         BO    PRTTOT80            Yes so don't put space                       
         MVI   SKIPLINE,1                                                       
         MVI   PRINTFG,PSKIP+PTOTAL                                             
         B     PRTTOT80                                                         
*                                                                               
PRTTOT50 TM    FMTPOPT1,RPFRTOT    Should we print redundant totals?            
         BO    PRTTOT56            Yes                                          
         CLI   ROWNREC,1           * ELEMINATE TOTAL IF 1 PRINT LINE *          
         BNE   PRTTOT52                                                         
         TM    ROWFLAGS,ROWTOTAL   Do we total on this row?                     
         BZ    PRTTOT90            No, so don't worry                           
         TM    PRINTFG,PTOTAL      Did we just print total line?                
         BO    PRTTOT90                                                         
         MVI   SKIPLINE,1                                                       
         MVI   PRINTFG,PSKIP+PTOTAL    Skip line and turn on total              
         B     PRTTOT80                Don't print                              
*                                                                               
PRTTOT52 TM    PRINTFG,PDETAIL     Did we just print a detail line?             
         BO    PRTTOT56            Yes                                          
         ZIC   R1,FMT#ACMS                                                      
         MHI   R1,PKLEN                                                         
         L     RE,ROWACM                                                        
         LA    R6,0(R1,RE)         Point to previous accums                     
         BCTR  R1,0                Subtract one for execute                     
         EXCLC R1,0(RE),0(R6)                                                   
         BNE   PRTTOT56            Previous Accums<>this level accums?          
         CLI   ROWSTATE,ROWPTTOT   Yes but check if should force total          
         BNE   PRTTOT90            No, so get out                               
*                                                                               
PRTTOT56 TM    ROWFLAGS,ROWTOTAL   Do we total on this row?                     
         BO    PRTTOT58            Print total                                  
         AHI   R2,-ROWLNQ          Bump to next level                           
         MVI   ROWSTATE,ROWPTTOT   Force total next time through                
         B     PRTTOT90            Finished                                     
*                                                                               
PRTTOT58 MVI   SKIPLINE,1                Yes Skip one line                      
         MVI   PRINTFG,PTOTAL+PPRINT     Print total line description           
         TM    DWNOPT1,DWNGO                                                    
         BZ    PRTTO58C                                                         
         TM    DWNOPT3,DWNXTTR                                                  
         BZ    PRTTO58C                                                         
         GOTO1 =A(DWNLOAD),DWNTOTR                                              
         AP    DTLCOUNT,PKONE                                                   
         B     PRTTOT90            Finished                                     
*                                                                               
PRTTO58C GOTO1 =A(PRTFORM),PRTRTOT                                              
*                                                                               
PRTTO58G MVI   COUNT,0             Skip zero lines if under two                 
         CLI   DMCB+11,2           How many lines did we use in CHOPPER         
         BL    *+8                                                              
         MVI   COUNT,1             Skip one line if more than one               
*                                                                               
         TM    DWNOPT1,DWNGO+DWNROWS    Download & DOWNLOAD ROWS REQ ?          
         BNO   PRTTOT59                 NO, SKIP                                
         BAS   RE,MIDPRT           DOWN-LOAD ROWS                               
         OI    DWTOTST,DWTROWBL    DOWN-LOAD ADDITIONAL TOTALS AS BLANK         
         MVI   STACKLVL,0          Initialize to no level being stacked         
*                                                                               
         USING COLD,R3             ** PRINT AMOUNT FOR ROW **                   
PRTTOT59 ZIC   R0,FMT#COLS                                                      
         L     R3,FMTCOL                                                        
         ST    R3,SVR3             INITALIZE TO FIRST COLUMN                    
*                                                                               
PRTTOT60 L     R3,SVR3             LOAD   COLUMN TO PROCESS                     
         ZIC   RF,COLPRTSQ         PRINT  SEQUENCE                              
         TM    DDLNKIND,DDLNKON+DDLNKTST                                        
         BZ    *+8                                                              
         IC    RF,COLDDLSQ         DDLINK processing sequence                   
         BCTR  RF,0                SWITCH TO   NEW  COLUMN                      
         MHI   RF,COLLNQ                                                        
         A     RF,FMTCOL           POINT TO   COL  TO   PRINT IN SEQ.           
         LA    R3,COLLNQ(,R3)      BUMP  TO   NEXT COLUMN                       
         ST    R3,SVR3             SAVE  NEXT COLUMN    TO   PROCESS            
         LR    R3,RF               POINT TO   NEW  COL  TO   PROCESS            
*                                                                               
         TM    COLFLAGS,COLHIDE    Is it a hidden column ?                      
         BO    PRTTOT70            Yes, so don't worry                          
         TM    DWNOPT1,DWNGO       Download ?                                   
         BZ    PRTTOT61            No                                           
         CLC   STACKLVL,COLSCLVL   Process columns at this level                
         BNE   PRTTOT70            Next column                                  
                                                                                
PRTTOT61 TM    COLFLAGS,COLAMT     Accumulated column ?                         
         BZ    PRTTOT68            No, so don't do anything                     
*                                                                               
PRTTOT62 CLI   COLTOTLV,0          SUPPRESS TOTAL?                              
         BE    PRTTOT63            NO                                           
         CLC   COLTOTLV,ROWNUM                                                  
         BH    PRTTOT68            NO TOTALS PAST COL TOT LEVEL                 
         J     PRTTOT64                                                         
                                                                                
PRTTOT63 ICM   RF,15,COLRTOTS      Supress column total                         
         JZ    PRTTOT64            No                                           
         LA    R1,1                                                             
         ZIC   RE,ROWNUM                                                        
         BCTR  RE,0                                                             
         SLL   R1,0(RE)            Find row for column                          
         NR    RF,R1               Isolate bit then test                        
         JNZ   PRTTOT68            Yes supress total                            
*                                                                               
PRTTOT64 DS    0H                                                               
         GOTO1 =A(PRTFORM),PRTCTOT                                              
         B     PRTTOT70            FINISHED PROCESSING THIS COLUMN              
*                                                                               
PRTTOT68 TM    DWNOPT1,DWNGO       DOWN-LOADING REQUESTED ?                     
         BZ    PRTTOT70            NO,  FINISHED PROCESSING THIS COLUMN         
         ST    R0,SVR0                                                          
         LA    RE,XLCHOP           CLEAR XLCHOP TO ZEROES OR SPACES             
         LHI   RF,GESIZEQ                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,8,SPACES         SET PADDING CHARACTER                        
         MVCL  RE,R0                                                            
         L     R0,SVR0                                                          
*                                                                               
         MVI   PRTSIZE,1           SET FIELD LENGTH TO 1                        
         LA    R1,DWNTEXT          ASSUME TEXT FIELD                            
         TM    COLFLAGS,COLAMT     NUMERIC FIELD ?                              
         BZ    PRTTO68B            NO,  BRANCH TO PROCESS TEXT                  
*                                                                               
PRTTO68A DS    0H                  PROCESS NUMERIC FIELD                        
         LA    R1,DWNPACK          SET TO NUMERIC FIELD                         
         TM    DWNOPT1,DWNTXT#     DOWN-LOAD NUMBER AS TEXT ?                   
         BO    PRTTO68B            YES, USE TEXT CHECKS FOR FIELD SIZE          
         TM    DWNOPT2,DWN#PAD     PAD NUMBERS WITH LEADING ZEROS               
         BZ    PRTTOT69            NO,  DOWN-LOAD THIS FIELD                    
         MVC   PRTSIZE,COLSIZE     YES, SET EXACT FIELD LENGTH                  
         B     PRTTOT69            AND  DOWN-LOAD THIS FIELD                    
*                                                                               
PRTTO68B DS    0H                  PROCESS TEXT FIELD                           
         TM    DWNOPT1,DWNCHOP     DOWN-LOAD FIXED LENGTH FIELDS ?              
         BZ    PRTTOT69            NO,  DOWN-LOAD THIS FIELD                    
         MVC   PRTSIZE,COLSIZE     YES, SET EXACT FIELD LENGTH                  
*                                                                               
PRTTOT69 GOTO1 =A(DWNLOAD)         OUTPUT THIS FIELD                            
*                                                                               
PRTTOT70 LA    R3,COLLNQ(,R3)                                                   
         BCT   R0,PRTTOT60                                                      
*                                                                               
         TM    DWNOPT1,DWNGO       Download ?                                   
         BZ    PRTTOT78            No                                           
         CLC   STACKLVL,FMTSTKHT   Process all stack levels ?                   
         BE    PRTTOT78            Finished so countinue as before              
         BL    *+6                 Print stacked level line                     
         DC    H'00'               Should never go high                         
*                                                                               
         BAS   RE,PRINTIT          Print detail                                 
         SR    RF,RF               Increase the stack level                     
         IC    RF,STACKLVL         Process this new level of columns            
         AHI   RF,1                                                             
         STC   RF,STACKLVL         Process this new level of columns            
                                                                                
         SR    R0,R0                                                            
         TM    DWNOPT1,DWNROWS     Download rows ?                              
         BZ    *+8                 No                                           
         IC    R0,FMT#VROW         Number of rows to dummy up                   
         TM    DWNOPT2,DWNTOTS     Download totals requested?                   
         BZ    *+8                                                              
         AHI   R0,1                Add one extra if printing totals             
         LTR   R0,R0                                                            
         BZ    PRTTOT59                                                         
                                                                                
PRTTOT74 OI    DWNOPT2,DWNDUMF        Download a dummy field                    
         GOTO1 =A(DWNLOAD),DWNTEXT    Blank field                               
         BCT   R0,PRTTOT74                                                      
         B     PRTTOT59            Repeat process again for next level          
                                                                                
PRTTOT78 TM    ROWOPT,ROWPBTM      Print totals on bottom of page ?             
         BZ    *+8                 No                                           
         OI    PRINTFG,PBOTTOM     Yes                                          
         TM    ROWFLAGS,ROWTSPPG   Do we want totals on seperate page ?         
         BZ    PRTTOT80            No                                           
         MVI   FORCEHED,YES        Yes, force HEADUP                            
*                                                                               
PRTTOT80 MVI   DEBUGRT#,4                                                       
         BAS   RE,PRINTIT                                                       
*                                                                               
PRTTOT90 B     RPTXIT                                                           
         DROP  R3,R5,R7                                                         
         EJECT                                                                  
*=====================================================================*         
*  PRINTING CONTROL FOR DETAILED LINES AND TOTALS                     *         
*=====================================================================*         
                                                                                
         USING BIGPRNTD,R4                                                      
         USING FMTRECD,R5                                                       
         USING BOXD,R6                                                          
PRINTIT  NTR1                                                                   
         TM    DWNOPT1,DWNGO       ARE WE DOWN-LOADING?                         
         BZ    PRT05               NO                                           
*&&UK                                                                           
         TM    RECAPSW,RECAPON     Recapping ?                                  
         BZ    PRT04               No                                           
         ZIC   R1,DWN#FLDS         # of fields downloaded                       
         ZIC   R2,FND#FLDS         Max # of cols found for any format           
         SR    R2,R1                                                            
         BNP   PRT04                                                            
                                                                                
PRT01    OI    DWNOPT2,DWNDUMF        Download a dummy field                    
         GOTO1 =A(DWNLOAD),DWNTEXT    BLANK FIELD                               
         BCT   R2,PRT01                                                         
*&&                                                                             
PRT04    GOTO1 =A(DWNLOAD),DWNEOL  MARKEND OF LINE                              
         B     RPTXIT                                                           
*                                                                               
PRT05    L     R4,VBIGPRNT                                                      
         L     R6,ADBXAREA                                                      
*&&DO                           Debug code, add when needed                     
         CLI   QOPT3,C'Q'                                                       
         BNE   PRT06                                                            
         MVC   P,SPACES                                                         
         MVC   P+2(9),=C'FORCEHED='                                             
         MVC   P+11(1),FORCEHED                                                 
         SR    R1,R1                                                            
         IC    R1,DEBUGRT#                                                      
         CVD   R1,DUB                                                           
         MVC   P+16(4),=C'RT#='                                                 
         CURED DUB,(3,P+20),0,DECS=NO                                           
         SR    R1,R1                                                            
         IC    R1,DETMAX                                                        
         CVD   R1,DUB                                                           
         MVC   P+30(4),=C'MAX='                                                 
         CURED DUB,(3,P+34),0,DECS=NO                                           
         MVC   P+40(4),=C'PRT='                                                 
         GOTO1 HEXOUT,DMCB,PRTACT,P+44,1,0,0                                    
         MVC   P+46(4),=C'PRI='                                                 
         GOTO1 HEXOUT,DMCB,PRTPRIOR,P+50,1,0,0                                  
         GOTO1 PRNTBL,DMCB,=C'PRINT',P,1,L'P,=CL2'1C'                           
*&&                                                                             
PRT06    TM    PRTCNTRL,PRTEOR     END OF REQUEST?                              
         BO    PRT08               YES, FINISH OFF PRINTING PAGE                
         CLI   FORCEHED,YES        HEAD UP?                                     
         BNE   PRT15               NO SO CONTINUE                               
         MVI   FORCEHED,NO         RESET HEAD FLAG                              
*                                                                               
PRT08    BAS   RE,FOOTUP                                                        
         TM    PRTCNTRL,PRTEOR     END OF REQUEST                               
         BO    PRT10                                                            
         BAS   RE,HEADUP                                                        
         B     PRT15                                                            
*                                                                               
PRT10    MVI   FORCEHED,NO                                                      
         CLI   BOXSTAT,C'B'        CHECK STATUS "A" OR "B"                      
         BNH   PRTXIT              DON'T BOTHER TO CLOSE THE BOX                
         MVI   BOXREQ,C'C'         CLOSE BOX                                    
         MVI   DEBUGPRT,90         DEBUG LOCATION 90                            
         GOTO1 REPORT                                                           
         B     PRTXIT              EXIT  THIS ROUTINE                           
*                                                                               
PRT15    TM    PRINTFG,PPRINT      SOULD I PRINT A LINE?                        
         BO    PRT16                                                            
         TM    PRINTFG,PSKIP       SKIP A LINE ONLY                             
         BZ    PRT99                                                            
*                                                                               
PRT16    SR    R0,R0                                                            
         ICM   R0,1,XP#LINES       NUMBER OF LINES TO PRINT                     
         BZ    PRT17                                                            
         LR    R7,R0               If xp#lines used then don't check            
         B     PRT22                                                            
*                                                                               
PRT17    DS    0H                                                               
*&&US*&& LA    R0,4                COUNT NUMBER OF LINES TO PRINT               
*&&UK*&& LA    R0,7                COUNT NUMBER OF LINES TO PRINT               
         LR    R7,R0               NUMBER OF POTENTIAL LINES TO PRINT           
         L     R2,XPSAVE                                                        
                                                                                
PRT18    CLC   0(STDPGWD,R2),XSPACES                                            
         BE    PRT20               BLANK LINE INDICATES FINISHED                
         LA    R2,STDPGWD(,R2)                                                  
         BCT   R0,PRT18                                                         
*                                                                               
PRT20    SR    R7,R0                                                            
         BZ    PRTXIT                                                           
         LR    R0,R7               R0,R7 have nubmer of lines to print          
*                                                                               
PRT22    SR    R1,R1                                                            
         IC    R1,LINE             Current line # of print line                 
         AR    R0,R1               Add current line plus # print lines          
         IC    R1,SKIPLINE         Number of lines to skip                      
         AR    R0,R1               Add this in also                             
         TM    FMTPOPT3,RPFXTBOX   eXtra boxes ?                                
         BZ    PRT38                                                            
         TM    PRTACT,PRTDTAIL                                                  
         BO    PRT37                                                            
         TM    PRTACT,PRTCTOT      If printing totals ?                         
         BZ    PRT36               Yes, then                                    
         TM    PRTPRIOR,PRTHEADS   If just printed headings then                
         BO    PRT37                 print a box-line                           
*                                                                               
PRT36    TM    PRTPRIOR,PRTMIDS+PRTHEADS                                        
         BNZ   PRT38                                                            
         TM    PRTACT,PRTHEADS     Don't do for heads                           
         BO    PRT38                                                            
         TM    PRTACT,PRTCTOT      Print column total amounts ?                 
         BZ    PRT37               No                                           
         TM    PRTOPT,PRTNXTLN     Yes, so print amount on next line ?          
         BO    PRT38               Yes, already printed box mid line            
*                                                                               
PRT37    AHI   R0,1                One for eXtra box line                       
*                                                                               
PRT38    STC   R0,BYTE                                                          
*        MVC   WORK(1),BYTE                                                     
*        MVC   WORK+1(1),DETMAX                                                 
*        GOTO1 PRNTBL,DMCB,=C'BD',WORK,C'DUMP',2,=CL2'2D'                       
         CLC   BYTE,DETMAX         Do we have room ?                            
         BNH   PRT40               Yes, not yet at end of page ?                
         MVI   SKIPLINE,0          Reset skipline since at end of page          
         BAS   RE,FOOTUP           Print current page footer                    
         BAS   RE,HEADUP           Print next page header                       
*                                                                               
PRT40    TM    FMTPOPT2,RPFEXCOL   Extended columns                             
         BZ    PRT42               No                                           
         TM    PRTACT,PRTMIDS      Print midlines ?                             
         BO    PRT40C              Yes, so mask out boxes                       
         TM    PRTACT,PRTRTOT      Printing row total description ?             
         BZ    PRT42               No                                           
         TM    PRINTFG,PBOTTOM     Print total on botton of page                
         BZ    PRT40C              No, so ok to mask                            
         TM    PRTOPT,PRTNXTLN     Print description on seperate line ?         
         BO    PRT42               Yes, skip for now                            
*                                                                               
PRT40C   LA    RE,BOXCOLS          Box columns                                  
         SR    RF,RF                                                            
         IC    RF,BXCOLDSP         Point into box column                        
         AR    RE,RF                                                            
         IC    RF,BXCOLMSK         Wipe on column line indicators               
         AHI   RF,-1                                                            
         BM    PRT42               Doesn't work if negative                     
         EXMVC RF,0(RE),XSPACES                                                 
*                                                                               
PRT42    TM    FMTPOPT3,RPFXTBOX   eXtra boxes ?                                
         BZ    PRT45               No                                           
         TM    PRTACT,PRTDTAIL                                                  
         BO    PRT42B                                                           
         TM    PRTACT,PRTCTOT      If printing totals ?                         
         BZ    PRT42A              Yes, then                                    
         TM    PRTPRIOR,PRTHEADS   If just printed headings then                
         BO    PRT42B                print a box-line                           
*                                                                               
PRT42A   TM    PRTPRIOR,PRTMIDS+PRTHEADS                                        
         BNZ   PRT45                                                            
         TM    PRTACT,PRTHEADS     Was last action mids or heads?               
         BO    PRT45               Yes, so don't do                             
         TM    PRTACT,PRTCTOT      Print column total amounts ?                 
         BZ    PRT42B              No                                           
         TM    PRTOPT,PRTNXTLN     Yes, so print amount on next line ?          
         BO    PRT45               Yes, already printed box mid line            
*                                                                               
PRT42B   SR    R1,R1                                                            
         IC    R1,LINE                                                          
*&&US*&& LA    R1,BOXROWS-1(R1)                                                 
*&&UK*&& LA    R1,BOXROWS(R1)                                                   
         MVI   0(R1),C'M'          Box crazzzy                                  
         MVI   SPACING,1                                                        
         MVI   DEBUGPRT,14         DEBUG LOCATION 14                            
         GOTO1 REPORT                                                           
*                                                                               
         TM    PRINTFG,PBOTTOM     Print total at bottom of page ?              
         BZ    PRT45               No                                           
         LR    R0,R7               Number of lines to print                     
         SR    RF,RF                                                            
         IC    RF,LINE             Current line on                              
         AR    R0,RF               Current line + # of print lines              
         SR    R3,R3                                                            
         IC    R3,DETMAX           Bottom of page for box or print line         
         TM    PRTACT,PRTRTOT      Print total row description                  
         BZ    PRT42C              No                                           
         TM    PRTOPT,PRTNXTLN     Print row total on seperate line ?           
         BZ    PRT42C              No                                           
         AHI   R3,-4               Four to print row total description          
*                                                                               
PRT42C   SR    R3,R0               Lines before bottom & printing               
         BNP   PRT45               None                                         
         STC   R3,SKIPLINE         Number of lines to skip                      
         TM    FMTPOPT3,RPFTEDIT   Timesheet edit ?                             
         BZ    PRT45               No                                           
         TM    FMTPOPT3,RPFXTBOX   eXtra boxes ?                                
         BZ    PRT45               No                                           
*                                                                               
PRT43    SR    R1,R1               Make a grid down what is left                
         IC    R1,LINE                                                          
         SRL   R3,1                Divide in half                               
         LTR   R3,R3               Number of lines we have room for             
         BNP   PRT45               None                                         
*&&US*&& LA    R1,BOXROWS(R1)                                                   
*&&UK*&& LA    R1,BOXROWS+1(R1)                                                 
*                                                                               
PRT44    MVI   0(R1),C'M'          Box crazzzy                                  
         LA    R1,2(,R1)                                                        
         BCT   R3,PRT44                                                         
*                                                                               
PRT45    CLI   SKIPLINE,0                                                       
         BE    PRT48                                                            
         MVC   SPACING,SKIPLINE                                                 
         MVI   DEBUGPRT,9          DEBUG LOCATION 9                             
         GOTO1 REPORT                                                           
         TM    PRTACT,PRTRTOT      Print row total description ?                
         BZ    PRT46               No                                           
         TM    PRTOPT,PRTNXTLN     Row total des. on seperate line              
         BZ    PRT46               No                                           
         TM    PRINTFG,PBOTTOM     Printing on bottom of page ?                 
         BZ    PRT46               No                                           
         LA    RE,BOXCOLS          Box columns                                  
         SR    RF,RF                                                            
         IC    RF,BXCOLDSP         Point into box column                        
         AR    RE,RF                                                            
         IC    RF,BXCOLMSK         Wipe on column line indicators               
         AHI   RF,-1                                                            
         BM    PRT46               Doesn't work if negative                     
         EXMVC RF,0(RE),XSPACES                                                 
         MVI   SKIPLINE,1                                                       
         MVI   DEBUGPRT,12         DEBUG LOCATION 12                            
         GOTO1 REPORT              Skip one line                                
*                                                                               
PRT46    MVI   SKIPLINE,1          Reset SKIPLINE to print buffer               
         MVC   SPACING,SKIPLINE                                                 
         TM    PRINTFG,PSKIP       Skip a line only?                            
         BO    PRT99               Yes, so exit                                 
*                                  No, so print buffer out                      
PRT48    L     R2,XPSAVE           Start of block                               
*                                                                               
PRT50    SR    R0,R0               Space lines to skip at end of block          
         LA    R1,4                Print a MAX of 4 lines at a time             
         LA    R3,XP                                                            
         CR    R1,R7               Less then 4 lines to print ?                 
         BNH   *+6                 No                                           
         LR    R1,R7               Yes, fewer then 4 lines to print             
         SR    R7,R1               R7 = number of lines left to print           
*                                                                               
PRT52    MVC   1(STDPGWD,R3),0(R2) Move my lines into XP                        
         LA    R2,STDPGWD(,R2)                                                  
         CLC   1(STDPGWD,R3),XSPACES                                            
         BNE   PRT54                                                            
         AHI   R0,1                Skip a line after printing block             
         B     PRT55                                                            
*                                                                               
PRT54    SR    R0,R0               Reset, no lines to skip                      
*                                                                               
PRT55    LA    R3,STDPGWD(,R3)                                                  
         BCT   R1,PRT52                                                         
*                                                                               
         MVI   DEBUGPRT,10         DEBUG LOCATION 10                            
         GOTO1 REPORT                                                           
         LTR   R0,R0               Any lines to skip                            
         BZ    PRT58                                                            
         STC   R0,SKIPLINE                                                      
         GOTO1 REPORT                                                           
         MVI   SKIPLINE,1                                                       
*                                                                               
PRT58    LTR   R7,R7               Any more to print ?                          
         BP    PRT50               Yes, so keep printing                        
*                                                                               
PRT60    TM    FMTPOPT2,RPFEXCOL   Extended columns                             
         BZ    PRT99                                                            
         TM    PRTACT,PRTRTOT+PRTMIDS                                           
         BZ    PRT99                                                            
         L     RE,FMTBOXCL         Restore box column information               
         MVC   BOXCOLS(STDPGWD),0(RE)                                           
*                                                                               
PRT99    LA    R0,SPACES           Clear out print lines                        
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         L     RE,XPSAVE                                                        
         LHI   RF,MAXXPLN*STDPGWD                                               
         MVCL  RE,R0                                                            
*                                                                               
PRTXIT   MVI   SKIPLINE,0          Reset to zero                                
         MVI   XP#LINES,0          Reset to zero                                
         MVC   PRTPRIOR,PRTACT     Save of prior PRTACT indicators              
         TM    PRTACT,PRTDTAIL                                                  
         BZ    *+8                                                              
         NI    PAGEFLAG,TURNOFF-PAGEACOL   Remove indication that all           
*                                            columns formated                   
         DROP  R2,R4,R5                                                         
*                                                                               
RPTXIT   XIT1                                                                   
         DROP  R8,R9                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*      FIND MAXIMUM number of download FIELDS in any one format       *         
*      This is used one recapping is being used with downloading      *         
***********************************************************************         
         SPACE 1                                                                
         USING FMTRECD,R5                                                       
FNDMFLDS NTR1  BASE=*,LABEL=*                                                   
         L     R5,AFORMATS         Point to first FORMAT                        
         MVI   FND#FLDS,0                                                       
         MVI   FMT#VROW,0                                                       
         LH    R0,FORMAT#          R0=# of formats                              
         SR    R3,R3               R3=# Total of rows + columns                 
FNDMF10  SR    R2,R2               R2=# of COLs to print for FORMAT             
*                                                                               
         USING COLD,R1                                                          
         L     R1,FMTCOL           Point to COLUMN info                         
         ICM   R2,1,FMT#COLS       Total # of COLs in FORMAT                    
         BZ    FNDMF19             None, then skip                              
FNDMF15  TM    COLFLAGS,COLHIDE    Hidden COLUMN?                               
         BO    FNDMF16               yes, don't count it                        
         CLI   COLSCLVL,0          Stack level zero                             
         BNE   FNDMF16               no, don't count stacked under cols         
         AHI   R3,1                Count COLs to print                          
FNDMF16  AHI   R1,COLLNQ           Bump to next COLUMN                          
         BCT   R2,FNDMF15                                                       
         DROP  R1                                                               
*                                                                               
         USING ROWD,R1                                                          
FNDMF19  L     R1,FMTROW           Point to ROW info                            
         ICM   R2,1,FMT#ROWS       Total # of ROWs in FORMAT                    
         BZ    FNDMF25             None, then skip                              
         SR    RF,RF                                                            
*                                                                               
FNDMF20  CLI   ROWTYPE,ROWCOL      Is this a ROW made from a COL?               
         BE    FNDMF21               yes, don't count it                        
         CLI   ROWTYPE,C'N'        Is this a hidden ROW?                        
         BE    FNDMF21               yes, don't count it                        
         AHI   RF,1                                                             
         AHI   R3,1                Count ROWs to print                          
FNDMF21  AHI   R1,ROWLNQ           Bump to next ROW                             
         BCT   R2,FNDMF20                                                       
         STC   RF,FMT#VROW         Visible real rows by format                  
         DROP  R1                                                               
*                                                                               
FNDMF25  ZIC   R1,FND#FLDS                                                      
         CR    R3,R1               Does this FORMAT have more fields?           
         BNH   *+8                  No, keep max cols found                     
         STC   R3,FND#FLDS          Yes, new max cols found                     
         SR    R3,R3                                                            
         LA    R5,FMTLNQ(,R5)       No, Next FORMAT CODE                        
         BCT   R0,FNDMF10                                                       
*                                                                               
FNDMF40  TM    DWNOPT2,DWNTOTS     DOWN-LOAD TOTALS requested?                  
         BZ    FNDXIT                No, exit                                   
         ZIC   R1,FND#FLDS           yes, add another column to down            
         AHI   R1,1                                                             
         STC   R1,FND#FLDS                                                      
*                                                                               
FNDXIT   XIT1                                                                   
         DROP  R5                                                               
         LTORG                                                                  
         TITLE 'Build a blend of headings when we have merged recaps'           
***********************************************************************         
* Pass FULL & BYTE from sort records format                           *         
*      FULL = A(FMTROW), BYTE = X(FMT#ROWS)                           *         
*      R5 is the top format of recaps that print underneath/same page *         
* Pass back ALT#HEAD, Number of headings built                        *         
***********************************************************************         
         SPACE 1                                                                
         USING HEADD,R2                                                         
TOP      USING HEADD,R4                                                         
SRH      USING HEADD,RE                                                         
         USING FMTRECD,R5                                                       
SRF      USING FMTRECD,R9                                                       
*                                                                               
NEWHEAD  NTR1  BASE=*,LABEL=*                                                   
         L     R9,0(,R1)                                                        
         L     R2,AIO3             Build temporary headings in IO3              
         L     R4,FMTHEAD                                                       
         SR    R0,R0                                                            
         SR    R1,R1               Keep track of # of headings built            
         SR    R8,R8                                                            
         ICM   R0,1,FMT#HEAD       Number of headings to process                
         BZ    NEWHDXIT                                                         
         MVI   BYTE1,NO                                                         
*                                                                               
NEWHD100 TM    TOP.HEADFLAG,HEADROW+HEADADR                                     
         BNZ   NEWHD120            Either a row or an address                   
         MVC   HEADD(HEADLNQ),TOP.HEADD                                         
         OC    HEADDISP,HEADDISP   Into sort record ?                           
         BZ    NEWHD200            No, get next                                 
         L     RE,SRF.FMTHEAD                                                   
         SR    RF,RF                                                            
         IC    RF,FMT#HEAD                                                      
NEWHD104 CLC   TOP.HEADINDX,SRH.HEADINDX                                        
         BE    NEWHD110                                                         
         LA    RE,HEADLNQ(,RE)                                                  
         BCT   RF,NEWHD104                                                      
         B     NEWHD200            Get next                                     
*                                                                               
NEWHD110 MVC   HEADD(HEADLNQ),SRH.HEADD                                         
         B     NEWHD200            Get next                                     
         DROP  SRH                                                              
*                                                                               
         USING ROWD,R3                                                          
NEWHD120 TM    TOP.HEADFLAG,HEADADR      Address                                
         BO    NEWHD150                                                         
         L     R3,SRF.FMTROW       First row related to sort record             
         SR    RF,RF                                                            
         IC    RF,SRF.FMT#ROWS     Number of rows to scan                       
*                                                                               
NEWHD125 CLC   ROWINDEX,TOP.HEADINDX                                            
         BNE   NEWHD130                                                         
         MVC   HEADD(HEADLNQ),TOP.HEADD                                         
         STCM  R3,7,HEADPTR+1      Store A(ROW)                                 
         LR    R8,R3               Save for Street address type                 
         MVC   HEADDISP,ROWKYDSP                                                
         MVI   BYTE1,YES           Had a hit on this row                        
         MVC   HBYTE1,HEADOPTS                                                  
         NI    HBYTE1,HEADNME+HEADCDE                                           
         MVC   HBYTE2,ROWFLAGS                                                  
         NI    HBYTE2,ROWBOTH                                                   
         CLC   HBYTE1,HBYTE2                                                    
         BE    NEWHD200            Best match                                   
*                                                                               
NEWHD130 LA    R3,ROWLNQ(,R3)      Next row                                     
         BCT   RF,NEWHD125                                                      
                                                                                
         CLI   BYTE1,YES           Had a match but not perfect                  
         BE    NEWHD200                                                         
         B     NEWHD210            Coundn't add heading                         
*                                                                               
         USING KYWD,R6                                                          
NEWHD150 LTR   R3,R8               Load previous row worked on                  
         BZ    NEWHD210            Can't replicate street address               
         SR    R8,R8                                                            
         TM    ROWXPIND,ROWXPADR                                                
         BZ    NEWHD210            Can't replicate street address               
         L     R6,ROWINDEX                                                      
         SR    RE,RE                                                            
         ICM   RE,3,KYWADDR        Street address ?                             
         BZ    NEWHD210            None to be found                             
         LR    R6,RE                                                            
         A     R6,KYWBASE          Point to Street address entry                
         C     R6,TOP.HEADINDX     Does it match ?                              
         BNE   NEWHD210            No, so can't build address                   
         MVC   HEADD(HEADLNQ),TOP.HEADD                                         
         STCM  R3,7,HEADPTR+1                                                   
         MVC   HEADDISP,ROWXPDSP                                                
         DROP  R3,R6                                                            
*                                                                               
NEWHD200 LA    R2,HEADLNQ(,R2)                                                  
         LA    R1,1(,R1)           Number of headings built so far              
         MVI   BYTE1,NO                                                         
*                                                                               
NEWHD210 LA    R4,HEADLNQ(,R4)     Next potential heading to add                
         BCT   R0,NEWHD100                                                      
*                                                                               
NEWHDXIT STC   R1,ALT#HEAD                                                      
         XIT1                                                                   
         SPACE 1                                                                
HBYTE1   DS    XL1                                                              
HBYTE2   DS    XL1                                                              
         DROP  TOP,SRF                                                          
         DROP  CUR,PRV                                                          
         DROP  R2,R5                                                            
         LTORG                                                                  
         TITLE 'Format data for printing on a print line'                       
***********************************************************************         
*  PARSE INFO AND CONVERT TO PRINTABLE FORM                           *         
***********************************************************************         
         SPACE 1                                                                
         USING ROWD,R2                                                          
         USING KYWD,R6                                                          
         USING FMTRECD,R5                                                       
         USING ACRL2D,R7                                                        
CUR      USING RECXD,RECXSRT                                                    
PRV      USING RECXD,PRVXSRT                                                    
*                                                                               
PRTFORM  NMOD1 0,**FORM**,R9                                                    
         L     RC,RLBASEC                                                       
         L     R7,AACRL2D          2ND STORAGE AREA ACRL2D                      
         MVC   ACURWRK,ASRTWRK                                                  
         TM    PRTCNTRL,PRTSRT2    USE A(PRVWRK) INSTEAD ?                      
         BZ    *+10                                                             
         MVC   ACURWRK,APRVWRK                                                  
         STC   R1,PRTACT                                                        
         SR    R1,R1                                                            
         SR    R4,R4                                                            
         SR    R6,R6                                                            
         MVI   PASS,0              Re-set                                       
         MVI   BXCOLMSK,0          SET BOX COLUMN MASK TO NULL                  
         MVI   BXCOLDSP,0          SET BOX COLUMN DISPLACEMENT TO ZERO          
                                                                                
         LA    RE,XLCHOP           CLEAR XLCHOP TO ZEROES OR SPACES             
         LHI   RF,GESIZEQ                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,8,SPACES         SET PADDING CHARACTER                        
         MVCL  RE,R0                                                            
         MVC   XLCHOP(L'UPFRONT),UPFRONT                                        
                                                                                
         NI    PRINTFG,TURNOFF-PUPFRONT                                         
         CLC   UPFRONT,SPACES                                                   
         BNH   *+8                                                              
         OI    PRINTFG,PUPFRONT    UPFRONT IN USE                               
         MVC   UPFRONT,SPACES      CLEAR                                        
         XC    PRT#LN,PRT#LN       NUMBER OF PRINTING LINES                     
         MVI   PRT#LN+3,3          DEFAULT TO 3                                 
         NI    PRTOPT,PRTNXTLN     TURNOFF ALL BUT NEXT LINE                    
***********************************************************************         
*  HEAD LINES                                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING HEADD,R3                                                         
         TM    PRTACT,PRTHEADS     Are we working on headings?                  
         BZ    PRTFRM10                                                         
         MVI   EDTDTIDX,0          This is date format to print                 
         ST    R2,PRTAREA          R2=Passed printing area (Head Addr)          
         MVC   PRTSIZE,HEADLN      Size of printing area                        
         L     R2,HEADPTR          Point to a row or freedata                   
         LH    R4,HEADDISP         Point to sort record data                    
         L     R6,HEADINDX         Load which table entry this is               
         MVC   OPTPRT,FMTPIND      Use main default                             
         TM    HEADFLAG,HEADROW    Is it row heading?                           
         BZ    PRTFRM08                                                         
         MVC   EDTDTIDX,ROWDTIDX   Move row date format                         
         TM    ROWOPT,ROWNOSQH                                                  
         BZ    *+8                                                              
         OI    PRTOPT,PRTNOSQH     Don't squash                                 
         ICM   RF,15,ROWPRFX       Row prefix?                                  
         BZ    PRTFRM08                                                         
         SR    R1,R1                                                            
         IC    R1,ROWPRFX          Length of string                             
         BCTR  R1,0                                                             
         L     RE,PRTAREA                                                       
         TM    HEADFLAG,HEADADR    If Address part of heading                   
         BO    PRTFRM07            then no prefix at this point                 
         EXMVC R1,0(RE),0(RF)                                                   
*                                                                               
         TM    FMTPOPT1,RPFMCASE   Print in mixed case ?                        
         BO    PRTFRM07            Yes                                          
         L     RF,AUPCASE                                                       
         EX    R1,*+8                                                           
         B     PRTFRM07                                                         
         TR    0(0,RE),0(RF)       Make upper case                              
*                                                                               
PRTFRM07 LA    RE,2(R1,RE)                                                      
         ST    RE,PRTAREA                                                       
*                                                                               
PRTFRM08 CLI   HEADTYPE,RHDFTLN    Is it a foot line?                           
         BNE   *+8                 No                                           
         MVI   PRT#LN+3,1          Yes, so only use one line in chopper         
***********************************************************************         
*  DETAIL LINES                                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING COLD,R3                                                          
PRTFRM10 TM    PRTACT,PRTDTAIL+PRTCTOT       Details or column total ?          
         BZ    PRTFRM14                                                         
         L     RE,COLPRT           Get Print area address                       
         MVC   PRTSIZE,COLSIZE     Size of column (PRINT AREA)                  
         MVC   OPTPRT,COLEDOPT                                                  
         XC    SVULINE,SVULINE     Clear underline address                      
*                                                                               
         TM    DWNOPT1,DWNGO       Are we downloading?                          
         BZ    PRTFM10F            Yes, check stack properties                  
         MVC   DATAMAP#,COLMAP#                                                 
*        MVI   DATAMTYP,MPD_FUL    Full word           32 bit                   
         MVI   DATAMTYP,MPD_LNG    Double word or Long 64 bit                   
                                                                                
         SR    RF,RF                                                            
         CLI   COLSTACK,0          Is this a stacked under column ?             
         BE    PRTFM10C            No                                           
         ICM   RF,1,COLSCPWD       Yes, Is there a prefix ?                     
         BZ    PRTFM10C            No prefix on this stacked column             
*                                                                               
PRTFM10B BCTR  RF,0                                                             
         ICM   R4,15,COLHEAD1                                                   
         BZ    PRTFM10C            If no prefix then put dummy field            
         EXMVC RF,XLCHOP,0(R4)     Move in prefix to download                   
*                                                                               
TOP      USING COLD,R4                                                          
PRTFM10C SR    R4,R4                                                            
         ICM   R4,1,COLSCTOP       Get to top stack column                      
         BNZ   PRTFM10D                                                         
         CLI   COLSCPWD,0          See if this is already the top               
         BE    PRTFRM12            Don't down-load anything                     
         LA    R4,COLD             Current column is top column                 
         B     PRTFM10E                                                         
*                                                                               
PRTFM10D BCTR  R4,0                                                             
         MHI   R4,COLLNQ                                                        
         A     R4,FMTCOL                                                        
*                                                                               
PRTFM10E MVC   PRTSIZE,TOP.COLSCPWD                                             
         TM    DWNOPT1,DWNCHOP     Down-load fixed length field ?               
         BO    *+8                 Yes, so use top columm prefix size           
         MVI   PRTSIZE,0           No, so down-load size of data                
         DROP  TOP                                                              
*                                                                               
         TM    COLIND4,COLBLKHD    Blank heading ?                              
         BO    PRTFRM12            Don't print any prefix                       
         GOTO1 =A(DWNLOAD),DWNTEXT                                              
         LA    RE,XLCHOP           Clear XLCHOP again to spaces                 
         LHI   RF,GESIZEQ                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,8,SPACES         SET PADDING CHARACTER                        
         MVCL  RE,R0                                                            
*                                                                               
         MVC   PRTSIZE,COLSIZE     Reset size of column                         
         B     PRTFRM12            Countinue as normal                          
*                                                                               
PRTFM10F TM    PRTACT,PRTCTOT      Print total lines ?                          
         BZ    PRTFM10G            No                                           
         TM    PRTOPT,PRTNXTLN     Print amount on next line ?                  
         BZ    *+8                 No                                           
         AHI   RE,STDPGWD          RE = line to start printing on               
         TM    COLIND3,COLULN      Any underline ?                              
         BO    PRTFM10G            No                                           
         ST    RE,SVULINE          Save address to put line above               
         AHI   RE,STDPGWD          RE = line to start printing on               
         TM    COLIND3,COLULA      Underline above ?                            
         BO    PRTFM10G                                                         
         SR    RF,RF                                                            
         IC    RF,FMTSTKHT         Number of max stacked lines                  
         MHI   RF,STDPGWD                                                       
         AR    RF,RE                                                            
         ST    RF,SVULINE                                                       
         SHI   RE,STDPGWD          Reset                                        
*                                                                               
PRTFM10G SR    RF,RF                                                            
         IC    RF,COLSIZE          Width of column                              
         LA    R1,1                                                             
         CLI   COLSCPWD,0          Stacked prefix ?                             
         BNE   PRTFM10H            Yes                                          
         CLI   COLSTACK,0          Stacked column ?                             
         BE    PRTFRM12            No                                           
*                                                                               
PRTFM10H BCTR  RF,0                                                             
         SR    R1,R1                                                            
         IC    R1,COLSCLVL         Level of print line                          
         MHI   R1,STDPGWD                                                       
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FMTSTKHT         Number of lines needed-1                     
         LA    R1,1(,R1)           Add the one back                             
*                                                                               
PRTFM11A TM    PRTACT,PRTCTOT      Print total lines ?                          
         BZ    PRTFM11B                                                         
         TM    COLIND3,COLULN      Underline ?                                  
         BO    PRTFM11B            No                                           
         LA    R1,1(,R1)                                                        
*                                                                               
PRTFM11B CLM   R1,1,XP#LINES       Keep track of max lines down                 
         BNH   *+8                                                              
         STC   R1,XP#LINES                                                      
*                                                                               
         ICM   RF,1,COLSCPWD       Column prefix width                          
         BZ    PRTFRM12            None                                         
         CLI   COLSCTOP,0          Is this the top column of stack ?            
         BE    PRTFRM12                                                         
         BCTR  RF,0                                                             
         ICM   R4,15,COLHEAD1                                                   
         BZ    PRTFM11D                                                         
         EXMVC RF,0(RE),0(R4)                                                   
*                                                                               
PRTFM11D AHI   RF,2                Add 1 for space and 1 for EX instr.          
         IC    R1,COLSIZE                                                       
         SR    R1,RF               Column prefix took up all the room           
         BP    PRTFM11F                                                         
         AHI   RE,STDPGWD          Bump to next line                            
         SR    RF,RF               Don't adjust                                 
         B     *+8                                                              
*                                                                               
PRTFM11F STC   R1,PRTSIZE          NEW SIZE     OF PRINT AREA                   
         AR    RE,RF               NEW LOCATION OF PRINT AREA                   
         ICM   R1,15,SVULINE       DO WE UNDERLINE TOTAL ?                      
         BZ    PRTFRM12                                                         
         AR    R1,RF                                                            
         ST    R1,SVULINE                                                       
*                                                                               
PRTFRM12 ST    RE,PRTAREA          POINT TO PRINT AREA                          
         TM    PRTACT,PRTCTOT      COLUMN TOTALS ?                              
         BZ    PRTFRM13                                                         
         L     R4,ROWACM           POINT TO ACCUM LINE FOR CURRENT ROW          
         AH    R4,COLACM           DISPLACEMENT TO ACCUM.                       
         L     R6,COLINDEX         LOAD WHICH TABLE ENTRY THIS IS               
         L     R8,EDTBASE                                                       
         AH    R8,KYWEDIT                                                       
         B     PRTFRM56            PRINT AMOUNT                                 
*                                                                               
PRTFRM13 LH    R4,COLDSP           Point to sort rec data                       
         L     R6,COLINDEX         Load which table entry this is               
         ICM   R2,15,COLAROW       Point to row                                 
         BZ    PRTFR13B                                                         
         TM    COLIND2,COLRATE     Rate keyword ?                               
         BO    PRTFR13B                                                         
         MVC   DATAMAP#,ROWMAP#    Soft map number                              
         MVI   DATAMTYP,MPD_CHRR   Default to character                         
         TM    ROWDAIND,ROWDAYMD   Date type ?                                  
         BZ    *+8                                                              
         MVI   DATAMTYP,MPD_DTE    Date type data                               
         MVC   EDTDTIDX,ROWDTIDX   Index into DATEFMT (ACRL03)                  
         TM    ROWOPT,ROWNOSQH                                                  
         BZ    *+8                                                              
         OI    PRTOPT,PRTNOSQH     DON'T SQUASH                                 
                                                                                
PRTFR13B TM    COLIND4,COLRIGHT                                                 
         BZ    *+8                                                              
         OI    PRTOPT,PRTRIGHT     Right justify data                           
***********************************************************************         
*  MIDLINES AND TOTAL LINES                                           *         
***********************************************************************         
         SPACE 1                                                                
PRTFRM14 TM    PRTACT,PRTPQIDX     Print queue indexing                         
         BZ    PRTFR14B                                                         
         ST    R3,PRTAREA                                                       
         MVI   PRTSIZE,55                                                       
         B     PRTFRM16                                                         
*                                                                               
PRTFR14B TM    PRTACT,PRTROWS                                                   
         BO    PRTFRM15                                                         
         TM    PRTACT,PRTRTOT+PRTMIDS                                           
         BZ    PRTFRM18                                                         
         TM    DWNOPT1,DWNGO       DOWN-LOAD REQUESTED ?                        
         BZ    PRTFR14D            NO, CONTINUE                                 
         TM    PRTACT,PRTRTOT      IS THIS A PRINT-TOTALS REQUEST ?             
         BZ    PRTFRMX             NO, RETURN                                   
         TM    DWNOPT2,DWNTOTS     DOWN-LOAD TOTALS REQUESTED ?                 
         BZ    PRTFRMX             NO, RETURN                                   
*                                                                               
PRTFR14D TM    FMTPOPT2,RPFEXCOL   EXTENDED COLUMNS?                            
         BZ    PRTFRM15                                                         
         L     RE,XPSAVE           START OF PRINT AREA                          
         ICM   RF,15,ROWPRTAD      CURRENT POSTION                              
         BZ    PRTFRM15                                                         
         SR    RF,RE               CALCULATE DISPLACEMENT                       
         AHI   RF,1                                                             
         STC   RF,BXCOLDSP                                                      
*                                                                               
PRTFRM15 MVC   PRTAREA,ROWPRTAD    Point print area                             
         MVC   PRTSIZE,ROWPRTSZ    Size of print area                           
         TM    PRTACT,PRTMIDS                                                   
         BZ    PRTFRM16                                                         
         TM    FMTPOPT2,RPFEXCOL   Extended columns?                            
         BZ    PRTFRM16                                                         
         MVI   PRTSIZE,55          Mid-lines won't run into anything            
*                                                                               
PRTFRM16 MVC   EDTDTIDX,ROWDTIDX   Index into DATEFMT (ACRL03)                  
         MVC   DATAMAP#,ROWMAP#                                                 
         MVI   DATAMTYP,MPD_CHRR   Default to character                         
         TM    ROWDAIND,ROWDAYMD   Date type ?                                  
         BZ    *+8                                                              
         MVI   DATAMTYP,MPD_DTE    Date type data                               
         TM    ROWOPT,ROWNOSQH                                                  
         BZ    *+8                                                              
         OI    PRTOPT,PRTNOSQH     Don't squash                                 
         CLI   ROWNUM,1            Is it "TOTAL FOR REQUEST" ?                  
         BE    PRTFRM74            Yes, so skip formating                       
         LH    R4,ROWKYDSP         Point to sort rec data (Disp in)             
         TM    ROWKYIND,ROWKYCDE                                                
         BO    *+8                                                              
         LH    R4,ROWDADSP                                                      
         L     R6,ROWINDEX         Load which table entry to point to           
*                                                                               
         USING EDITD,R8                                                         
PRTFRM18 A     R4,ACURWRK          POINT TO SORT REC & ADD DISPLACEMENT         
         L     R8,EDTBASE                                                       
         AH    R8,KYWEDIT                                                       
         SR    R1,R1                                                            
         CLI   PRTSIZE,0                                                        
         BNE   *+10                                                             
         MVC   PRTSIZE,EDITSIZE    NEW AREA SIZE                                
* EDT... ROUTINES                                                               
         CLI   EDITTYPE,CODE       SHOULD WE BRANCH TO OTHER CODE?              
         BNE   PRTFRM20                                                         
         SR    RF,RF                                                            
         ICM   RF,1,EDITROUT       LOAD ADDRESS TO BRANCH TO                    
         SLL   RF,2                X4                                           
         B     *(RF)                                                            
         B     EDFRMTC             1 - FORMAT CODE                              
         B     EDFRMTN             2 - FORMAT NAME                              
         B     EDAADDR             3 - ANY ADDRESS                              
         B     EDCADDR             4 - AGENCY ADDRESS                           
         B     EDOADDR             5 - OFFICE ADDRESS                           
         B     EDPERID             6 - PERIOD DATE                              
         B     EDDATES             7 - DATE BLOCK                               
         B     EDBLANK             8 - BLANK                                    
         B     EDPAGE              9 - PAGE NUMBER                              
*&&UK*&& B     PRTFRM74            10-                                          
*&&US*&& B     EDTIME              10- TIME - B, R OR N                         
         B     EDTTME              11- TYPE OF TIME - M, A OR T                 
         B     EDSTAT              12- TRANSACTION STATUS                       
         B     EDJSTA              13- JOB STATUS                               
         B     EDBTYP              14- BILLING TYPE                             
         B     EDOMEM              15- ONLINE MEMO DATA                         
         B     EDGPWC              16- GROUP WORKCODE TYPE                      
         B     EDEXCP              17- BIT MAP EXCEPTION CODES                  
         B     EDEXCS              18- SINGLE EXCEPTION CODE                    
         B     PRTFRM74            19-                                          
*        B     EDGPTR              19- EDIT GROUP TRANSACTIONS                  
         B     EDCUME              20- PRINT AMOUNT IN CUMULATIVE COL           
         B     EDCONT              21- COUNT THE NUMBER OF DETAIL LINES         
         B     EDAPER              22- ALTERNATE PERIOD DATE                    
         B     EDPYTY              23- EDIT PAY TYPE                            
         B     EDLOCT              24- LOCATION STATUS                          
         B     EDPERD              25- PERIOD END DATE (PERSON)                 
         B     EDPID#              26- PID CODE FROM NUMBER                     
         B     EDSTTC              27- Static column heading keyword            
*&&US*&& B     EDRSTA              28- Receivable status                        
*&&UK*&& B     PRTFRM74            28-                                          
*&&US*&& B     EDVSTA              29- Vendor status                            
*&&UK*&& B     PRTFRM74            29-                                          
         B     EDSIGN              30- Edit just sign of amount                 
         B     EDPID2              31- Edit pid code (BrandO vs non-BO)         
         B     EDLBILL             32- Edit long inv # for sj                   
         B     EDLBLL2             33- Remove dashes for long inv #             
*&&US*&& B     EDPYMTD             34- Edit payment method                      
*&&UK*&& B     PRTFRM74            34-                                          
         B     EDPID3              35- Edit pid code for others                 
*&&US*&& B     EDZIPC              36- Edit zip code for xtra 4 bytes           
*&&UK*&& B     PRTFRM74            36-                                          
*&&US*&& B     EDESTA              37- Estimate status                          
*&&UK*&& B     PRTFRM74            37-                                          
*&&US*&& B     EDAPMTD             38- Edit approval method                     
*&&UK*&& B     PRTFRM74            38-                                          
         B     EDPID4              39- PID for POA approvers                    
*&&UK*&& B     EDVATRG             40- Edit vat region                          
*&&US*&& B     PRTFRM74            40-                                          
*&&UK*&& B     EDPOAK              41- Order acknowledgement status             
*&&US*&& B     PRTFRM74            41-                                          
         B     EDBEST              42- Estimate status (BrO)                    
*&&UK*&& B     EDESAK              43- Estimate acknowledgement status          
*&&US*&& B     PRTFRM74            43-                                          
*&&UK*&& B     EDSTATY             44- Show Yes if status is on                 
*&&US*&& B     PRTFRM74            44-                                          
*&&UK*&& B     EDTYMAP             45- Type mapping keyword for GROUPM          
*&&US*&& B     PRTFRM74            45-                                          
         B     EDPID5              46- PID email                                
*&&US*&& B     EDPYMTC             47- Edit payment company                     
*&&UK*&& B     PRTFRM74            47-                                          
*                                                                               
PRTFRM20 CLI   EDITTYPE,STORAGE    Is data sitting around in storage?           
         BNE   PRTFRM30            No                                           
         SR    RF,RF                                                            
         SR    R0,R0                                                            
         ICM   RF,3,EDITSTRG       Load S(Rx,Disp)                              
         LR    R2,RF                                                            
         N     R2,=X'00000FFF'     This leaves Disp of S(Rx,Disp) in R2         
         SRL   RF,12               Get register from S(Rx,Disp) into RF         
         EX    RF,*+8                                                           
         BNZ   PRTFRM32            Must have a value                            
         AR    R2,R0                                                            
         DC    H'00'                                                            
*                                                                               
PRTFRM30 CLI   EDITTYPE,EDTMVC     SHOULD WE DO AN EXECUTED MOVE                
         BNE   PRTFRM38                                                         
*                                                                               
PRTFRM32 IC    R1,PRTSIZE                                                       
         CLI   EDITSIZE,0          Zero is special case                         
         BE    PRTFRM34                                                         
         CLC   PRTSIZE,EDITSIZE                                                 
         BNH   PRTFRM34                                                         
         IC    R1,EDITSIZE         Size of actual data                          
*                                                                               
C        USING XLCHOPD,XLCHOP                                                   
PRTFRM34 SHI   R1,1                R2=POINT TO DATA'S ADDRESS                   
         BNM   *+8                 Don't allow to go negative                   
         AHI   R1,1                                                             
         EXMVC R1,C.XLCHDAT,0(R2)                                               
         TM    PRTACT,PRTHEADS     CHANGE HEADING TO UPPER CASE?                
         BZ    PRTFRM74            NOT A HEADING, SO SKIP                       
         TM    FMTPOPT1,RPFMCASE   MIX CASE                                     
         BO    PRTFRM74            YES LEAVE IT AS IS                           
         EXOC  R1,C.XLCHDAT,SPACES                                              
         B     PRTFRM74                                                         
         DROP  C                                                                
*                                                                               
PRTFRM38 CLI   EDITTYPE,DATETYPE   PRINT A DATE?                                
         BNE   PRTFRM50            NO                                           
C        USING XLCHOPD,XLCHOP                                                   
         MVC   BYTE,EDITDTE1       Date form currently                          
         CLI   EDITDTE2,EDITDC9    Year month only, Date to convert to          
         BNE   *+8                                                              
         OI    BYTE,X'80'                                                       
         GOTO1 =A(DATING),DMCB,(BYTE,(R4)),(EDTDTIDX,C.XLCHDAT)                 
         B     PRTFRM74                                                         
         DROP  C                                                                
*                                                                               
PRTFRM50 CLI   EDITTYPE,BINARY     IS IT A BINARY NUMBER TO PRINT?              
         BNE   PRTFRM55                                                         
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,KYWSRTQ                                                     
         CHI   RE,4                                                             
         BNH   *+6                                                              
         DC    H'0'                BINARY NUMBERS >4 BYTES LONG                 
*                                                                               
         SR    RE,RE                                                            
         LA    R1,1                                                             
         IC    RE,KYWSRTQ+1        SIZE OF INFO IN SORT RECORD                  
         SLL   R1,0(RE)                                                         
*                                                                               
         CLI   EDITSPCL,SIGNED     IT A SIGNED OR UNSIGNED NUMBER?              
         BNE   PRTFRM51            BRANCH IF UNSIGNED                           
         TM    0(R4),X'80'         NEGITIVE NUMBER?                             
         BZ    PRTFRM51            NO                                           
         LHI   RE,-1               MAKE REGISTER X'FF'S                         
*                                                                               
PRTFRM51 BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+8                                                              
         ICM   RE,0,0(R4)          Get binary number into register              
         CVD   RE,DUB                                                           
C        USING XLCHOPD,XLCHOP                                                   
         TM    OPTPRT,RPFEDZRO     Print zero, not blank                        
         BO    PRTFRM52            Yes                                          
         TM    FMTPIND,RPFEDZRO    Global setting, Print zero                   
         BO    PRTFRM52                                                         
         CURED DUB,(15,C.XLCHDAT),0,DECS=NO,FLOAT=-,ZERO=BLANK                  
         B     PRTFRM74                                                         
*                                                                               
PRTFRM52 CURED DUB,(15,C.XLCHDAT),0,DECS=NO,FLOAT=-                             
         B     PRTFRM74                                                         
         DROP  C                                                                
*                                                                               
PRTFRM55 CLI   EDITTYPE,PACK       Is it a packed number to print ?             
         BNE   PRTFRM60            No                                           
*                                                                               
PRTFRM56 NI    PRTOPT,TURNOFF-PRTREVSG                                          
         TM    COLOPT2,COLREVSG    Reverse sign ?                               
         BZ    PRTFRM58            No                                           
         TM    FMTIND,FMTNOREV                                                  
         BO    PRTFRM58                                                         
         TM    PRTACT,PRTCTOT      Total columns printing ?                     
         BZ    PRTFRM57            No                                           
         TM    ROWOPT,ROWNOREV     Skip reverse sign for row ?                  
         BO    PRTFRM58                                                         
*                                                                               
PRTFRM57 OI    PRTOPT,PRTREVSG     Turn on to reverse sign                      
*                                                                               
*        Temporarly remove MPD_REV add turnoff PRTREVSG                         
         TM    DDLNKIND,DDLNKON+DDLNKTST                                        
         BZ    PRTFRM58                                                         
         NI    PRTOPT,TURNOFF-PRTREVSG                                          
*        MVI   DATAMTYP,MPD_REV    Full word reverse sign DDLINK                
*                                                                               
PRTFRM58 MVC   NDECIMAL,COLDCMLS   Pass to EDITIT                               
         MVC   ROUNDTO,COLROUND    Pass to EDITIT                               
         TM    PRTACT,PRTCTOT      Column totals ?                              
         BZ    PRTFRM59            No                                           
         NI    OPTPRT,TURNOFF-COLEDZRO                                          
         TM    FMTPOPT1,RPFZEROT   Print zero totals ?                          
         BZ    PRTFRM59                                                         
         OI    OPTPRT,COLEDZRO     Yes, so turn on to print zero                
*                                                                               
PRTFRM59 OC    PRTAREA,PRTAREA                                                  
         BZ    PRTFRM82                                                         
*&&UK*&& TM    COLIND6,COLNOCUR    Test no currency prefix                      
*&&UK*&& BO    *+12                Yes                                          
         TM    COLFLAGS,COLCALC                                                 
         BZ    *+8                                                              
         OI    PRTOPT,PRTNOPFX           No currency prefix                     
         GOTO1 =A(EDITIT),DMCB,(OPTPRT,(R4)),(PRTSIZE,PRTAREA),COLFCCDE         
         NI    PRTOPT,TURNOFF-PRTNOPFX   RESET                                  
         B     PRTFRM82                                                         
*                                                                               
PRTFRM60 CLI   EDITTYPE,EDTREC     Is data in sort record ?                     
         BE    *+6                 Yes                                          
         DC    H'00'               No such EDITTYPE                             
*                                                                               
PRTFR60A TM    DDLNKIND,DDLNKON+DDLNKTST                                        
         BZ    PRTFR60C                                                         
         TM    FMTIND,FMTNOREV                                                  
         BO    PRTFR60C                                                         
         TM    ROWOPT,ROWREV                                                    
         BZ    PRTFR60C                                                         
         MVI   DATAMTYP,MPD_REV    Full word reverse sign DDLINK                
                                                                                
PRTFR60C TM    ROWFLAGS,ROWBOTH    Do we get code and/or name ?                 
         BZ    PRTFRMX             Neither so exit                              
         BNO   PRTFRM61            Print Code or Name if branch                 
         TM    DDLNKIND,DDLNKON+DDLNKTST    DDLINK on                           
         BZ    PRTFRM61                     No                                  
         OI    PASS,PASS2X                                                      
         NI    ROWFLAGS,TURNOFF-ROWNME                                          
         TM    PASS,PASS2ND                                                     
         BZ    PRTFRM61                                                         
         NI    ROWFLAGS,TURNOFF-ROWCDE                                          
         OI    ROWFLAGS,ROWNME                                                  
                                                                                
PRTFRM61 SR    RF,RF               Initalize to save full length                
         XC    HALF,HALF                                                        
         TM    ROWFLAGS,ROWCDE     Get code ?                                   
         BZ    PRTFRM68            No, check name                               
         TM    DDLNKIND,DDLNKON+DDLNKTST         DDLINK on                      
         BNZ   PRTFR61A            Yes so ignore ROWNOCDE for now               
         TM    ROWDAIND,ROWNOCDE   Code is used for sort only                   
         BO    PRTFRM68                                                         
                                                                                
PRTFR61A SR    R1,R1                                                            
         ICM   R1,3,ROWKYSZ                                                     
         BCTR  R1,0                                                             
         LH    R4,ROWKYDSP         Point to code in sort record                 
         TM    ROWKYIND,ROWKYCDE   Is code key or data ?                        
         BO    PRTFRM62            Code type key                                
         LLC   R1,ROWDASZ          Data type, size of                           
         LH    R4,ROWDADSP         Displacement to data                         
*                                                                               
PRTFRM62 LA    RF,1(,R1)           Save len+1 in case code and name on          
         STH   RF,HALF                                                          
         TM    CUR.RECOTH,RECMASK  Do we need to mask data ?                    
         BZ    PRTFRM63            No                                           
         SR    RE,RE                                                            
         ICM   RE,1,ROWMASKL       Length to adjust for                         
         BZ    PRTFRM63                                                         
         SR    R1,RE               New length                                   
*                                                                               
PRTFRM63 A     R4,ACURWRK          Point to sort record                         
*                                                                               
         LTR   R1,R1                                                            
         BZ    PRTFRM68                                                         
         ST    R1,SVR1                                                          
         LA    R0,XLCHOP+L'XLCHUPF    Point to XLCHDAT                          
         LR    RE,R4                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         TM    DDLNKIND,DDLNKON+DDLNKTST                                        
         BZ    PRTFRM68            No                                           
         CLI   ROWTYPE,ROWCOL      If column then not empty node                
         JE    PRTFRM68                                                         
         CLI   0(R4),X'FF'                                                      
         BE    PRTFRM67            All FF's is null node                        
         L     R1,SVR1                                                          
*                                                                               
PRTFRM64 CH    R1,=H'250'                                                       
         BL    PRTFRM65                                                         
         OC    0(250,R4),0(R4)                                                  
         BNZ   PRTFRM68                                                         
         LA    R4,250(R4)                                                       
         SH    R1,=H'250'                                                       
         B     PRTFRM64                                                         
PRTFRM65 LTR   R1,R1                                                            
         BZ    PRTFRM67                                                         
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R4),0(R4)                                                    
         BNZ   PRTFRM68                                                         
*                                                                               
PRTFRM67 MVI   DATAMTYP,MPD_EMT    Null Data                                    
         OI    PASS,PASSEMTY                                                    
*                                                                               
PRTFRM68 TM    ROWFLAGS,ROWNME     Do we want name?                             
         BZ    PRTFRM74            No, finished                                 
         TM    CUR.RECOTH,RECMASK  Do we need to mask data ?                    
         BZ    PRTFRM69            No                                           
         CLI   ROWMASKL,0          Mask this row                                
         BNE   PRTFRM74            Yes, don't print name                        
*                                                                               
PRTFRM69 SR    R1,R1                                                            
         ICM   R1,3,ROWKYSZ                                                     
         BCTR  R1,0                                                             
         LH    R4,ROWKYDSP         Point to code in sort record                 
         TM    ROWKYIND,ROWKYNME   Is code key or data ?                        
         BO    PRTFRM70            Name                                         
         LLC   R1,ROWDASZ                                                       
         LH    R4,ROWDADSP                                                      
*                                                                               
PRTFRM70 A     R4,ACURWRK          Point to sort record                         
         LTR   R1,R1                                                            
         BZ    PRTFRM74                                                         
         ST    R1,SVR1                                                          
         XR    RF,RF                                                            
         ICM   RF,3,HALF                                                        
         LA    RE,XLCHOP+L'XLCHUPF      Point to XLCHDAT                        
         AR    RE,RF                                                            
         LR    RF,R1                                                            
         LR    R0,R4                                                            
         MVCL  RE,R0                                                            
         L     R1,SVR1                                                          
                                                                                
         TM    DDLNKIND,DDLNKON+DDLNKTST                                        
         BZ    PRTFRM74            No                                           
         CLI   ROWTYPE,ROWCOL      If column then not empty node                
         JE    PRTFRM74                                                         
PRTFRM71 CH    R1,=H'250'                                                       
         BL    PRTFRM72                                                         
         OC    0(250,R4),0(R4)                                                  
         BNZ   PRTFRM74                                                         
         LA    R4,250(R4)                                                       
         SH    R1,=H'250'                                                       
         B     PRTFRM71                                                         
PRTFRM72 LTR   R1,R1                                                            
         BZ    PRTFR72A                                                         
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R4),0(R4)                                                    
         BNZ   PRTFRM74                                                         
*                                                                               
PRTFR72A TM    PASS,PASSEMTY       Was code empty ?                             
         JO    PRTFRM73                                                         
         MVC   XLCHOP(15),=CL15'<No name found>'                                
         J     PRTFRM74                                                         
*                                                                               
PRTFRM73 MVI   DATAMTYP,MPD_EMT    Null Data                                    
*                                                                               
PRTFRM74 TM    PRTOPT,PRTNOSQH                                                  
         BO    PRTFRM76            NEED TO CHECK UPFRONT DATA                   
         GOTO1 ADSQUASH,DMCB,XLCHOP,GESIZEQ                                     
         L     R4,DMCB+4           SIZE OF DATA PRINTING (FROM SQUASH)          
         B     PRTFR81B                                                         
*                                                                               
PRTFRM76 LA    R4,GESIZEQ          R4 = LENGTH OF PERTINENT DATA SO FAR         
         TM    PRINTFG,PUPFRONT    DID WE MOVE PREFIX UPFRONT ?                 
         BZ    PRTFRM79                                                         
         LA    R1,L'UPFRONT-2      POINT TO LAST CHAR OF UPFRONT DATA           
         LA    RE,XLCHOP                                                        
         AR    RE,R1                                                            
*                                                                               
PRTFRM77 CLI   0(RE),BLANK         Find first non-blank                         
         BH    PRTFRM78                                                         
         BCTR  RE,0                RE = POSITION IN XLCHOP TO CHECK             
         BCT   R1,PRTFRM77         R1 = LENGTH OF UPFRONT THAT STAYS            
         B     PRTFRM79            TRY FROM THE OTHER DIRECTION THEN            
*                                                                               
PRTFRM78 LA    R4,GESIZEQ                                                       
         SHI   R4,L'UPFRONT-1      R4 = LEN OF PERTINENT DATA                   
         LA    R4,1(R4,R1)                  ADD IN UPFRONT PIECE                
*                                                                               
         LA    RE,2(,RE)           TWO TO POINT PAST 1ST BLANK                  
         LA    R0,XLCHOP+L'XLCHUPF                                              
         LHI   RF,GESIZEQ                                                       
         SHI   RF,L'XLCHUPF                                                     
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
PRTFRM79 LA    RE,XLCHOP                                                        
         LR    R0,RE                                                            
         CLI   0(RE),BLANK         FIND FIRST NON-BLANK                         
         BH    PRTFRM80                                                         
         AHI   RE,1                TRY NEXT CHARACTER                           
         BCT   R4,*-12             R1 = LEN OF PERTINENT DATA                   
         B     PRTFR81F            NO DATA FOUND                                
*                                                                               
PRTFRM80 LR    R1,R4               MOVE DATA UP TO REMOVE SPACES                
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
*&&US*&& LA    RF,XLCHOP           RELOAD START                                 
*&&UK*&& LR    RF,R0                                                            
         LA    RE,0(RF,R4)         POINT TO END OF AREA                         
         CLI   0(RE),BLANK         FIGURE OUT LEN OF PERTINENT DATA             
         BH    PRTFRM81            FIRST PIECE OF GOOD DATA FROM END            
         BCTR  RE,0                                                             
         BCT   R4,*-10                                                          
         CLI   0(RE),BLANK         FIGURE OUT LEN OF PERTINENT DATA             
         BNH   PRTFR81F            NO DATA FOUND                                
*                                                                               
PRTFRM81 AHI   R4,1                ADD ONE BACK                                 
         AHI   RE,1                POINT PAST FIRST GOOD CHARACTER              
         LA    RF,GESIZEQ                                                       
         SR    RF,R4               RF = LEN OF NON-PERTINENT DATA               
         BNP   PRTFR81B                                                         
         BCTR  RF,0                                                             
         MVI   0(RE),BLANK                                                      
         SHI   RF,1                                                             
         BM    PRTFR81B                                                         
         EXMVC RF,1(RE),0(RE)      WIPE DATA NOT MOVED                          
*                                                                               
PRTFR81B LTR   R1,R4               Replace all invalid chars with " "           
         BZ    PRTFR81F                                                         
         L     RE,AMIXCASE                                                      
         LA    RF,XLCHOP                                                        
*                                                                               
PRTFR81C CH    R1,=H'250'                                                       
         BL    PRTFR81D                                                         
         TR    0(250,RF),0(RE)                                                  
         LA    RF,250(RF)                                                       
         SH    R1,=H'250'                                                       
         B     PRTFR81C                                                         
PRTFR81D LTR   R1,R1                                                            
         BZ    PRTFR81E                                                         
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         TR    0(0,RF),0(RE)                                                    
*                                                                               
PRTFR81E TM    DWNOPT1,DWNGO       Downloading ?                                
         BZ    PRTFR81F            No                                           
         TM    PRTOPT,PRTRIGHT     Right justify data                           
         BZ    PRTFR81F            No                                           
         LR    R1,R4               Re-load R1 with length of data               
         SR    RF,RF                                                            
         IC    RF,PRTSIZE          Size of column                               
         SR    RF,R1                                                            
         BNP   PRTFR81F                                                         
         ST    RF,FULL                                                          
*                                                                               
         L     RE,GENAREA2         CLEAR GENAREA2 TO SPACES                     
         LHI   RF,GESIZEQ                                                       
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         ICM   R1,8,SPACES         SET PADDING CHARACTER                        
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,GENAREA2                                                      
         L     RF,FULL                                                          
         AR    RE,RF                                                            
         LA    R0,XLCHOP                                                        
         LR    R1,R4                                                            
         LR    RF,R4                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    RE,XLCHOP           COPY GENAREA2 TO XLCHOP                      
         LHI   RF,GESIZEQ                                                       
         L     R0,GENAREA2                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         L     RF,FULL                                                          
         AR    R4,RF                                                            
*                                                                               
PRTFR81F TM    PRTACT,PRTMIDS      Mid-lines ?                                  
         BO    PRTFR81I            Yes                                          
         TM    PRTACT,PRTRTOT      Total line ?                                 
         BZ    PRTFRM82            No                                           
         TM    FMTPOPT2,RPFEXCOL   Extended columns?                            
         BZ    PRTFR81I            No                                           
         CLM   R4,1,PRTSIZE        Equivalent to ROWPRTSZ                       
         BL    PRTFR81I            It will fit with out adjusting               
         MVI   PRTSIZE,55          Increase size and                            
         OI    PRTOPT,PRTNXTLN        move numbers down one line                
*                                                                               
PRTFR81I MVC   BXCOLMSK,PRTSIZE    Box column mask                              
         CLM   R4,1,PRTSIZE                                                     
         BH    *+8                                                              
         STC   R4,BXCOLMSK         USE SMALLER OF THE TWO                       
*                                                                               
PRTFRM82 TM    DWNOPT1,DWNGO       ARE WE DOWN-LOADING?                         
         BZ    PRTFRM85            NO                                           
         OC    PRTAREA,PRTAREA     PRINT AREA ZERO ?                            
         BZ    PRTFRMX             YES, SKIP DOWN-LOAD                          
         TM    PRTACT,PRTRTOT      IS THIS A PRINT-TOTALS REQUEST ?             
         BZ    PRTFRM83            NO, SKIP                                     
         TM    DWNOPT1,DWNCHOP     DOWN-LOAD FIXED LENGTH DATA ?                
         BZ    PRTFRM83            NO,  SKIP                                    
         ZIC   R1,ROWPRTSZ         YES, GET ROW PRINT SIZE                      
         AHI   R1,L'AC@TFOR        ADD "TOTAL FOR" SIZE                         
         STC   R1,PRTSIZE          AND SAVE IN PRINT SIZE                       
*---------------------------------------------------------------------*         
*    SAFETY NET CODE IN CASE PRINT SIZE IS ZERO AND NO DATA IS        *         
*    SPECIFIED. NOTE: PRINT SIZE IS NEVER 0 FOR ROWS AND              *         
*                     HEADINGS ARE NOT DOWN-LOADED                    *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
PRTFRM83 CLI   PRTSIZE,0           WAS PRINT SIZE SPECIFIED ?                   
         BNE   PRTFRM84            YES, SKIP                                    
         NC    XLCHOP(256),XLCHOP  Should be sufficient to check                
         BNZ   PRTFRM84            first 256 bytes                              
         TM    PRTACT,PRTHEADS     ARE WE WORKING ON HEADINGS ?                 
         BO    PRTFRM84            YES, SKIP                                    
         MVI   PRTSIZE,1           ASSUME PRINT SIZE OF 1 CHAR                  
         CLI   COLSIZE,0           IS  COLUMN SIZE ZERO ?                       
         BE    PRTFRM84            YES, SKIP                                    
         CLI   EDITTYPE,PACK       IS IT A NUMBER                               
         BE    PRTFR83A            YES, BRANCH TO PROCESS IT                    
         TM    DWNOPT1,DWNTXT#     DOWN-LOAD NUMBER AS TEXT ?                   
         BO    PRTFR83A            YES, USE TEXT LOGIC                          
         TM    DWNOPT2,DWN#PAD     PAD NUMBERS WITH LEADING ZEROS ?             
         BO    PRTFR83B            YES, SET EXACT FIELD LENGTH                  
         B     PRTFRM84            NO,  READY TO DOWN-LOAD THIS FIELD           
*                                                                               
PRTFR83A DS    0H                  PROCESS TEXT FIELD                           
         TM    DWNOPT1,DWNCHOP     DOWN-LOAD FIXED LENGTH FIELDS ?              
         BZ    PRTFRM84            NO,  READY TO DOWN-LOAD THIS FIELD           
                                                                                
PRTFR83B MVC   PRTSIZE,COLSIZE     YES, SET EXACT FIELD LENGTH                  
*                                                                               
PRTFRM84 DS    0H                                                               
*                                                                               
*&&UK*&& TM    DWNOPT3,DWNTUSS+DWNTEDI Test USS/EDIHUB transmission req         
*&&US*&& TM    DWNOPT3,DWNTEDI     Test EDIHUB transmission req                 
         BZ    PRTFR84A            No - OK                                      
         TM    PRTACT,PRTROWS      Test row ?                                   
         BO    PRTFR84A            Yes - don't truncate                         
         CLC   PRTSIZE,COLSIZE     Longer than column size                      
         BNH   PRTFR84A                                                         
         MVC   PRTSIZE,COLSIZE     Yes - use smaller size                       
*                                                                               
PRTFR84A LA    R1,DWNTEXT                                                       
         CLI   EDITTYPE,PACK       IS IT A NUMBER                               
         BNE   *+8                                                              
         LA    R1,DWNPACK                                                       
         CLI   EDITTYPE,CODE                                                    
         BNE   *+8                                                              
         CLI   EDITSPCL,PACK       IS IT ACTUALLY PACKED?                       
         BNE   *+8                                                              
         LA    R1,DWNPACK                                                       
         GOTO1 =A(DWNLOAD)                                                      
         TM    DDLNKIND,DDLNKON+DDLNKTST                                        
         BZ    PRTFRMX             Finished                                     
         TM    PASS,PASS2X         Is two pass activated ?                      
         BZ    PRTFRMX             No                                           
         OI    ROWFLAGS,ROWBOTH    Re-set                                       
         TM    PASS,PASS2ND        2nd pass ?                                   
         BO    PRTFRMX             Yes                                          
         LA    R0,XLCHOP           Clear XLCHOP area                            
         LHI   R1,GESIZEQ                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE                                                            
         OI    PASS,PASS2ND        Set to secound pass                          
         ZIC   RF,ROWMAP#                                                       
         AHI   RF,1                                                             
         STC   RF,DATAMAP#                                                      
         MVI   DATAMTYP,MPD_CHRR   Set to character                             
         B     PRTFRM60                                                         
*                                                                               
PRTFRM85 TM    PRTACT,PRTCTOT      COLUMN TOTALS ?                              
         BZ    PRTFRM86                                                         
         TM    COLIND3,COLULN      UNDERLINE ?                                  
         BO    PRTFRMX             NO                                           
         GOTO1 UNDERLIN,DMCB,(COLSIZE,PRTAREA),(COLULINE,SVULINE)               
         B     PRTFRMX                                                          
*                                                                               
PRTFRM86 CLI   EDITTYPE,PACK                                                    
         BE    PRTFRMX                                                          
         CLI   EDITTYPE,CODE                                                    
         BNE   *+8                                                              
         CLI   EDITSPCL,PACK       IS IT ACTUALLY PACKED?                       
         BE    PRTFRMX                                                          
         ICM   R2,15,PRTAREA       Load where to print                          
         BZ    PRTFRMX             No print address                             
         TM    PRTOPT,PRTRIGHT     Right justify data ?                         
         BZ    PRTFRM89                                                         
         SR    RF,RF                                                            
         IC    RF,PRTSIZE                                                       
         SR    RF,R4               RF = column size , R4 = data size            
         BNP   PRTFRM89            Branch if data > column width                
         AR    R2,RF               Right justify amount                         
*                                                                               
PRTFRM89 CHI   R4,1                Length of data >= 1                          
         BNH   PRTFRM90            Move just the one character in               
         CLI   PRT#LN+3,1          If only one line, don't use CHOPPER          
         BNH   PRTFRM90            More than one line to chop                   
         CLI   PRTSIZE,1           *** CHOPPER doesn't work for print           
         BH    PRTFRM92                area of size one ***                     
*                                                                               
PRTFRM90 CLM   R4,1,PRTSIZE        See which one is smaller                     
         BNH   *+8                 Length of string or area to print            
         IC    R4,PRTSIZE          Use the smaller of the two                   
         SHI   R4,1                less one for EX instruction                  
         BM    PRTFRMX             length of zero, so leave                     
         EXMVC R4,0(R2),XLCHOP                                                  
         MVI   XLINES,1            Only using one line                          
         B     PRTFRMX                                                          
*                                                                               
*&&UK                                                                           
PRTFRM92 LR    RF,R4               Work out how many lines needed...            
         SR    RE,RE               ...to fit the data                           
         SR    R0,R0                                                            
         IC    R0,PRTSIZE                                                       
         DR    RE,R0               L'data/width prtblk = n'lines needed         
         AHI   RF,1                Assume some remainder/remainder only         
         C     RF,PRT#LN           Test calc'd vs. predefined n'lines           
         BNL   *+8                                                              
         L     RF,PRT#LN           use whichever is greater                     
         GOTO1 CHOPPER,DMCB,XLCHOP,(PRTSIZE,(R2)),                     X        
               ('STDPGWD',(RF)),C'LEN=',(R4)                                    
*&&                                                                             
*&&US                                                                           
PRTFRM92 GOTO1 CHOPPER,DMCB,((R4),XLCHOP),(PRTSIZE,(R2)),              X        
               ('STDPGWD',PRT#LN),0,0                                           
*&&                                                                             
         MVC   XLINES,DMCB+11      NUMBER OF LINES USED IN CALL                 
         CLI   XLINES,0            XLINES CONTROLS HEADINGS                     
         BNE   *+8                                                              
         MVI   XLINES,1                                                         
*                                                                               
PRTFRMX  XIT1                                                                   
*                                                                               
PASS     DS    XL1                                                              
PASS2X   EQU   X'80'               Two passes needed                            
PASS2ND  EQU   X'40'               2nd pass                                     
PASSEMTY EQU   X'01'               Code was an empty node                       
                                                                                
EDTDTIDX DS    AL1                                                              
         EJECT                                                                  
***********************************************************************         
*  OFFICE / AGENCY / ANY ADDRESS                                      *         
***********************************************************************         
         SPACE 1                                                                
EDOADDR  MVC   XLINES,OFFADRLN     Number of lines of IDI address               
         MVI   ADDRSZ,L'OFFADDR                                                 
         SR    R1,R1                                                            
         ICM   R1,1,OFFADRLN                                                    
         BZ    PRTFRMX                                                          
         LA    R2,OFFADDR          Point to stored IDI address                  
         B     EDADRESS                                                         
*                                                                               
EDCADDR  MVC   XLINES,NADR         Number of lines of company address           
         MVI   ADDRSZ,L'COMPADDR                                                
         SR    R1,R1                                                            
         ICM   R1,1,NADR                                                        
         BZ    PRTFRMX                                                          
         LA    R2,COMPADDR         Point to stored company address              
         B     EDADRESS                                                         
*                                                                               
         USING ACCRECD,R2                                                       
EDAADDR  DS    0H                                                               
*&&US*&& ST    R2,FULL             Save current row                             
         MVC   IOKEY2(1),RCCOMPFL  Set company code                             
         LR    RE,R4                                                            
         XR    RF,RF                                                            
         ICM   RF,3,KYWSRTQ                                                     
         CHI   RF,LULAQ                  Is company code included ?             
         BE    *+8                       No, use data from sort record          
         LA    RE,1(,RE)                 Bump past company code                 
         MVC   IOKEY2+1(LULAQ),0(RE)                                            
*                                                                               
         CLI   FMT#ADRL,0          User specified # of row addr lines?          
         BNE   EDAADD10            Yes, skip                                    
*&&US*&& MVI   XLINES,0            No,  use US default                          
*&&UK*&& MVI   XLINES,4            No,  use UK default                          
         B     EDAADD20                                                         
*                                                                               
EDAADD10 MVI   XLINES,0            Initialize number of lines to print          
         TM    FMT#ADRL,RPFNALPV   Variable number of row addr. lines ?         
         BO    EDAADD20            Yes, skip                                    
         MVC   XLINES,FMT#ADRL     No, use fixed number of lines                
*                                                                               
EDAADD20 L     R2,AIO2                                                          
         CLC   IOKEY2,ACCKEY       Do we already haver record ?                 
         BE    EDAADD30            Yes                                          
*                                                                               
         MVC   ACCKEY,SPACES       No, get record                               
         MVC   ACCKEY(L'IOKEY2),IOKEY2                                          
         MVC   IOKEY,ACCKEY                                                     
         L     R1,=AL4(IOREAD+IOACFIL+IOAREA2)                                  
         GOTO1 AIO                                                              
         BNE   PRTFRMX             Did we get it ?                              
*                                                                               
         USING ADRELD,R2                                                        
EDAADD30 AH    R2,DATADISP         Get address element                          
*&&US                                                                           
         USING ROWD,RF                                                          
         L     RF,FULL             Current row                                  
         TM    ROWDAIND,ROWBDR     Using BDR attribute?                         
         BO    EDADD75                                                          
         DROP  RF                                                               
*&&                                                                             
EDAADD35 CLI   0(R2),0                                                          
         BE    PRTFRMX                                                          
         CLI   0(R2),ADRELQ        Find  x'22'                                  
         BE    EDAADD40            Yes                                          
         SR    R1,R1               No, so keep looking                          
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     EDAADD35            Try next element                             
*                                                                               
EDAADD40 SR    R1,R1               Find number of lines available               
         SR    R0,R0                                                            
         ICM   R1,1,ADRNUM         Number of address lines given ?              
         BNZ   EDAADD50            Yes,  skip                                   
         IC    R1,ADRLN            Get element length                           
         AHI   R1,-(ADRADD1-ADRELD)    less non-address data                    
         D     R0,=A(L'ADRADD1)    Divide by length of one addr. line           
         LTR   R1,R1               Any lines to process ?                       
         BZ    PRTFRMX             No, skip                                     
*                                                                               
EDAADD50 CLI   XLINES,0            is the output number set ?                   
         BNE   EDAADD60            Yes, skip                                    
         STC   R1,XLINES           No, save the number of lines                 
         B     EDAADD70            Now output the address lines                 
*                                                                               
EDAADD60 CLM   R1,1,XLINES         More lines then specified by user ?          
         BNH   EDAADD70            No, so ok as is                              
         IC    R1,XLINES           Yes, use the number user wanted              
*                                                                               
EDAADD70 MVI   ADDRSZ,L'COMPADDR                                                
         LA    R2,ADRADD1                                                       
         B     EDADRESS                                                         
         SPACE 3                                                                
*&&US                                                                           
         USING OATELD,R2                                                        
EDADD75  CLI   0(R2),0                                                          
         BE    PRTFRMX                                                          
         CLI   0(R2),OATELQ        Find  x'8C'                                  
         BE    EDADD80             Yes                                          
         SR    R1,R1               No, so keep looking                          
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     EDADD75             Try next element                             
*                                                                               
EDADD80  ICM   R4,15,PRTAREA                                                    
         BZ    EDADD150                                                         
         SR    R6,R6                                                            
EDADD90  CLC   OATLINE1,SPACES     Any address on this line?                    
         BNH   EDADD100                                                         
         MVC   0(L'OATLINE1,R4),OATLINE1                                        
         LA    R6,1                                                             
         LA    R4,STDPGWD(R4)      Bump to next print line                      
*                                                                               
EDADD100 CLC   OATLINE2,SPACES                                                  
         BNH   EDADD105                                                         
         CLI   XLINES,0                                                         
         BE    *+12                                                             
         CLM   R6,1,XLINES                                                      
         BNL   EDADD150                                                         
         MVC   0(L'OATLINE2,R4),OATLINE2                                        
         AHI   R6,1                                                             
         LA    R4,STDPGWD(R4)      Bump to next print line                      
*                                                                               
EDADD105 CLI   XLINES,0                                                         
         BE    *+12                                                             
         CLM   R6,1,XLINES         less than the # set on download scr?         
         BNL   EDADD150            No so finished                               
         CLC   OATCITY,SPACES                                                   
         BNH   EDADD130                                                         
         AHI   R6,1                                                             
         LA    RF,OATCITY+L'OATCITY-1  Point to end of city                     
         LA    R1,L'OATCITY                                                     
EDADD110 CLI   0(RF),C' '                                                       
         BH    EDADD120                                                         
         SHI   RF,1                                                             
         BCT   R1,EDADD110                                                      
         B     EDADD130                                                         
*                                                                               
EDADD120 BCTR  R1,0                                                             
         EXMVC R1,0(R4),OATCITY                                                 
         LA    R4,2(R1,R4)                                                      
*                                                                               
EDADD130 CLC   OATSTATE,SPACES                                                  
         BNH   EDADD140                                                         
         MVC   0(L'OATSTATE,R4),OATSTATE                                        
         LA    R4,L'OATSTATE(R4)                                                
         MVI   0(R4),C','                                                       
         AHI   R4,1                                                             
*                                                                               
EDADD140 CLC   OATZIP(L'OATZIP+L'OATZIPRN),SPACES                               
         BNH   EDADD150                                                         
         MVC   0(5,R4),OATZIP                                                   
         MVI   5(R4),C'-'                                                       
         MVC   6(4,R4),OATZIPRN                                                 
*                                                                               
EDADD150 STC   R6,XLINES                                                        
         B     PRTFRMX                                                          
*&&                                                                             
EDADRESS SR    R6,R6                                                            
         ICM   R4,15,PRTAREA                                                    
         BZ    PRTFRMX                                                          
         IC    R6,ADDRSZ                                                        
         BCTR  R6,0                                                             
*                                  Move in address from X'22' element           
EDADR10  EXMVC R6,0(R4),0(R2)                                                   
         LA    R2,1(R6,R2)         Bump to next line of address                 
         LA    R4,STDPGWD(R4)      Bump to next print line                      
         BCT   R1,EDADR10          Do until all lines processed                 
         B     PRTFRMX                                                          
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
*  BLANKS                                                             *         
***********************************************************************         
         SPACE 1                                                                
EDBLANK  MVI   CENTFLAG,NO                                                      
         MVC   XLINES,PRTSIZE      PRINT SIZE HAS # OF LINES TO SKIP            
         B     PRTFRMX                                                          
         SPACE 1                                                                
***********************************************************************         
*  FORMAT CODE / NAME                                                 *         
***********************************************************************         
         SPACE 1                                                                
EDFRMTC  MVC   XLCHOP+12(L'FMTCODE),FMTCODE                                     
         B     *+10                                                             
EDFRMTN  MVC   XLCHOP+12(L'FMTNAME),FMTNAME                                     
         MVI   XLCHOP+10,C':'                                                   
         MVC   XLCHOP(L'AC@FRMAT),AC@FRMAT                                      
         MVI   PRT#LN+3,1                                                       
         MVI   PRTSIZE,55                                                       
         B     PRTFRM74                                                         
         SPACE 1                                                                
***********************************************************************         
*  PAGE NUMBER                                                        *         
***********************************************************************         
         SPACE 1                                                                
EDPAGE   ICM   R4,15,PRTAREA                                                    
         BZ    PRTFRMX                                                          
         MVC   0(L'AC@PAGE,R4),AC@PAGE  PRINT PAGE NUMBER                       
         LA    R4,6(,R4)                MOVE RIGHT BY 6 CHARACTER               
         SR    R2,R2                                                            
         ICM   R2,3,PAGE                                                        
*        CLC   PRTAREA,FOOTNOT1                                                 
*        BE    *+10                                                             
*        CLC   PRTAREA,FOOTNOT2                                                 
*        BNE   *+6                                                              
*        BCTR  R2,0                                                             
         EDIT  (R2),(5,(R4)),0,ZERO=BLANK                                       
         B     PRTFRMX                                                          
         SPACE 1                                                                
***********************************************************************         
*  READ FOR ONLINE MEMO AND PUT IN PRINT AREA                         *         
***********************************************************************         
         SPACE 1                                                                
         USING OMEELD,R2                                                        
EDOMEM   MVC   IOKEY,SPACES                                                     
         MVC   IOKEY(15),0(R4)     MOVE IN ACCOUNT                              
         L     R2,AIO2                                                          
         CLC   IOKEY,0(R2)                                                      
         BE    EDOMEM2                                                          
         L     R1,=AL4(IOREAD+IOACFIL+IOAREA2)                                  
         GOTO1 AIO                                                              
         BNE   EDOMEMX                                                          
EDOMEM2  SR    R1,R1                                                            
         AH    R2,DATADISP                                                      
EDOMEM4  CLI   0(R2),0                                                          
         BE    EDOMEMX                                                          
         CLI   OMEEL,OMEELQ        X'3F' ONLINE-MEMO                            
         BE    EDOMEM6                                                          
         IC    R1,OMELN                                                         
         AR    R2,R1                                                            
         B     EDOMEM4                                                          
*                                                                               
EDOMEM6  IC    R1,OMELN            GET LENGTH                                   
         AHI   R1,-(OMELN1Q+1)                                                  
         BM    EDOMEMX                                                          
*                                                                               
C        USING XLCHOPD,XLCHOP                                                   
         TM    DWNOPT1,DWNGO       ARE WE DOWN-LOADING ?                        
         BZ    EDOMEM8             NO,  USE ALL OF XLCHOP                       
         NC    C.XLCHUPF,C.XLCHUPF IS THERE ALREADY DATA IN XLCHOP?             
         BZ    EDOMEM8             NO,  USE ALL OF XLCHOP                       
         CHI   R1,L'XLCHWRK        DOES THE WHOLE ON-LINE MEMO FIT ?            
         BNH   *+8                 YES, SKIP                                    
         LA    R1,L'XLCHDAT        NO,  USE SIZE OF XLCHDAT                     
         EXMVC R1,C.XLCHDAT,OMEMO  INSERT OMEMO                                 
         B     EDOMEMX             OUTPUT THIS FIELD                            
*                                                                               
EDOMEM8  DS    0H                                                               
         EXMVC R1,C.XLCHDAT,OMEMO                                               
*                                                                               
EDOMEMX  DS    0H                                                               
         B     PRTFRM74                                                         
         DROP  R2,C                                                             
         SPACE 1                                                                
***********************************************************************         
*  BIT MAP EXCEPTION CODES                                            *         
***********************************************************************         
         SPACE 1                                                                
C        USING XLCHOPD,XLCHOP                                                   
EDEXCP   LA    R0,32               Number of possible shifts                    
         SR    RE,RE               Shift factor                                 
         LA    R6,C.XLCHDAT                                                     
         ICM   RF,15,0(R4)         Get 4 byte bit MAP                           
         BZ    EDEXCPEX            No data, so finished                         
         ST    RF,SVRF                                                          
*                                                                               
EDEXCP10 LA    R1,1                                                             
         SLL   R1,0(RE)                                                         
         L     RF,SVRF                                                          
         NR    R1,RF               Is bit on ?                                  
         BNZ   EDEXCP20            No, try next bit shift                       
*                                                                               
EDEXCP15 LA    RE,1(,RE)           Increment for powers of 2                    
         BCT   R0,EDEXCP10                                                      
         B     EDEXCP60            All done, finished                           
*                                                                               
         USING ROWD,R2                                                          
         USING JXMSGD,R3                                                        
         USING JXCEPTD,R4                                                       
EDEXCP20 L     R4,AJXBLOCK                                                      
         LA    RF,1(,RE)           RF=    RE+1 FOR   SEQUENCE NUMBER            
         ST    RE,SVRE                                                          
         STC   RF,JXCODE                                                        
         MVI   JXMODE,JXMSEQ#+JXMWORKD                                          
         ST    RC,JXACWORK                                                      
         GOTO1 JOBEXCP,DMCB,(R4)                                                
         L     RE,SVRE                                                          
         ICM   R3,15,JXALIT                                                     
         BZ    EDEXCP60            NOT    FOUND,     EXIT                       
         TM    ROWFLAGS,ROWCDE     WANT   CODE ?                                
         BZ    EDEXCP25            NO,    SKIP                                  
         MVC   0(1,R6),JXMCODE     INSERT CODE                                  
         LA    R6,1(,R6)           NEXT   AVAILABLE  CHARACTER                  
*                                                                               
EDEXCP25 TM    ROWFLAGS,ROWNME     WANT   TEXT ?                                
         BZ    EDEXCP50            NO,    INSERT     COMMA                      
         TM    ROWFLAGS,ROWCDE     WANT   CODE AND   TEXT ?                     
         BZ    *+8                 NO,    SKIP                                  
         LA    R6,1(,R6)           NEED   EXTRA      BLANK                      
         MVC   0(15,R6),JXMTINY    INSERT TEXT                                  
         LA    R6,14(,R6)          BUMP   TO   LAST  CHARACTER                  
         LA    R1,15               DO NOT GO   BACK  TOO  FAR                   
*                                                                               
EDEXCP30 CLI   0(R6),C' '          BLANK ?                                      
         BNE   EDEXCP40            NO,    SKIP                                  
         BCTR  R6,0                BACK   UP   ONE   CHARACTER                  
         BCT   R1,EDEXCP30         TRY    PREVIOUS   CHARACTER                  
         LA    R6,2(,R6)           FORCE  A    BLANK CHARACTER                  
         B     EDEXCP50            INSERT COMMA                                 
*                                                                               
EDEXCP40 LA    R6,1(,R6)           MOVE   TO   FIRST BLANK CHARACTER            
*                                                                               
EDEXCP50 MVI   0(R6),C','          INSERT COMMA                                 
         LA    R6,2(,R6)           ALLOW  FOR  ', '                             
         B     EDEXCP15                                                         
*                                                                               
EDEXCP60 LA    RE,C.XLCHDAT                                                     
         CR    RE,R6               ANY    DATA INSERTED ?                       
         BE    EDEXCPEX            NO,    RETURN                                
         AHI   R6,-2                                                            
         MVI   0(R6),C' '          CLEAR  LAST COMMA                            
*                                                                               
EDEXCPEX B     PRTFRM74            RETURN                                       
         DROP  C                                                                
         SPACE 1                                                                
***********************************************************************         
*  SINGLE EXCEPTION CODE                                              *         
***********************************************************************         
         SPACE 1                                                                
C        USING XLCHOPD,XLCHOP                                                   
EDEXCS   SR    RF,RF                                                            
         ICM   RF,1,0(R4)          GET NUMBER OF CODE IN FILTER                 
         BZ    PRTFRM74                                                         
         BCTR  RF,0                                                             
         MHI   RF,2                                                             
         L     RE,AFLTEXCR         FLITER LIST OF EXCEPTION REASONS             
         LA    RE,1(RE,RF)         POINT TO CODE                                
*                                                                               
EDEXCS10 LA    R6,C.XLCHDAT                                                     
         TM    ROWFLAGS,ROWCDE                                                  
         BZ    EDEXCS15                                                         
         MVC   0(1,R6),0(RE)                                                    
         LA    R6,2(,R6)                                                        
EDEXCS15 TM    ROWFLAGS,ROWNME                                                  
         BZ    PRTFRM74                                                         
         L     R4,AJXBLOCK                                                      
         MVC   JXCODE,0(RE)                                                     
         MVI   JXMODE,JXMLIT+JXMWORKD                                           
         ST    RC,JXACWORK                                                      
         GOTO1 JOBEXCP,DMCB,(R4)                                                
         ICM   R3,15,JXALIT                                                     
         BZ    PRTFRM74                                                         
         MVC   0(40,R6),JXMSHORT                                                
         B     PRTFRM74                                                         
         DROP  R2,R3,R4,C                                                       
         SPACE 1                                                                
***********************************************************************         
*  GROUP WORKCODE TYPE                                                *         
***********************************************************************         
         SPACE 1                                                                
C        USING XLCHOPD,XLCHOP                                                   
EDGPWC   CLI   0(R4),GPWCTIME      TIME WORKCODE TYPE                           
         BNE   EDGPWC10                                                         
         MVCDD C.XLCHDAT(15),AC#TIME                                            
         B     EDEXPND                                                          
*                                                                               
EDGPWC10 CLI   0(R4),GPWCOOFP      OUT OF POCKET                                
         BNE   PRTFRM74                                                         
         MVCDD C.XLCHDAT(15),AC#OOFP                                            
*                                                                               
EDEXPND  GOTO1 ADDICTAT,DMCB,C'SL  ',C.XLCHDAT,0                                
         B     PRTFRM74                                                         
         DROP  C                                                                
         SPACE 1                                                                
*&&DO                                                                           
***********************************************************************         
*  EDIT GROUP TRANSACTIONS                                            *         
***********************************************************************         
C        USING XLCHOPD,XLCHOP                                                   
EDGPTR   CLI   0(R4),GPTRNCR     GROUP CR TRANSACTIONS WC=99                    
         BNE   *+10                                                             
         MVC   C.XLCHDAT(15),AC@BLG                                             
         CLI   0(R4),GPTRNDR     GROUP DR TRANSACTIONS                          
         BNE   PRTFRM74                                                         
         MVC   C.XLCHDAT(15),AC@CHGS                                            
         B     PRTFRM74                                                         
         DROP  C                                                                
*&&                                                                             
         SPACE 1                                                                
***********************************************************************         
*  PRINT AMOUNT IN CUMULATIVE COL                                     *         
*  Keep a running total for this column                               *         
***********************************************************************         
         SPACE 1                                                                
         USING COLD,R3                                                          
EDCUME   AP    COLDUB,0(PKLEN,R4)                                               
         LA    R4,COLDUB                                                        
         B     PRTFRM56            PRINT THE AMOUNT                             
         SPACE 1                                                                
***********************************************************************         
*  EDIT JUST SIGN OF AMOUNT                                           *         
*  Print the sign of the column, use col head 1 as + and head 2 as -  *         
***********************************************************************         
         SPACE 1                                                                
C        USING XLCHOPD,XLCHOP                                                   
EDSIGN   SR    RF,RF                                                            
         CP    0(PKLEN,R4),=P'0'                                                
         BZ    PRTFRMX             No sign, it is zero                          
         L     R2,COLHEAD1         Head 1 is the postive                        
         IC    RF,COLHEAD1         Column head width                            
         BP    *+12                                                             
         L     R2,COLHEAD2         Head 2 is the negative                       
         IC    RF,COLHEAD2         Column head width                            
                                                                                
         LTR   R2,R2                                                            
         BZ    PRTFRMX             No heading to use                            
         SHI   RF,1                                                             
         BM    PRTFRMX             No heading to use                            
         EXMVC RF,C.XLCHDAT,0(R2)                                               
         B     PRTFRM74            Print the sign                               
         DROP  C                                                                
         SPACE 1                                                                
***********************************************************************         
*  COUNT THE NUMBER OF DETAIL LINES                                   *         
***********************************************************************         
         SPACE 1                                                                
EDCONT   TM    PRTACT,PRTRTOT                                                   
         BO    EDCONT10                                                         
         AP    COLDUB,PKONE        COUNT BY ONES                                
         B     EDCONTX             PRINT THE AMOUNT FROM COLDUB                 
*                                  YES, OUTPUT THE NO. OF DETAIL LINES          
EDCONT10 TM    DWNOPT1,DWNGO       ARE WE DOWN-LOADING ?                        
         BZ    PRTFRM90            NO,  SKIP THIS FIELD                         
*                                  YES, OUTPUT THE NO. OF DETAIL LINES          
EDCONTX  LA    R4,COLDUB           POINT TO THE RUNNING TOTAL AMOUNT            
         B     PRTFRM56            PRINT THE AMOUNT FROM COLDUB                 
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
*  LOCATION STATUS                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING TRLTD,RE                                                         
C        USING XLCHOPD,XLCHOP                                                   
EDLOCT   L     RE,=A(LOCSTAB)                                                   
EDLOCT10 CLI   0(RE),EOT                                                        
         BNE   *+6                                                              
         DC    H'00'               LOCATION TYPE UNKNOWN                        
         CLC   TRLCODE,0(R4)       MATCH SORT KEY VALUE WITH TABLE              
         BE    EDLOCT20                                                         
         AHI   RE,TRLLNQ+4         NEXT ENTRY                                   
         B     EDLOCT10                                                         
                                                                                
EDLOCT20 MVC   C.XLCHDAT(4),TRLDATA   MOVE WORD VALUE TO PRINT                  
         B     PRTFRM74                                                         
         DROP  RE,C                                                             
         SPACE 1                                                                
***********************************************************************         
*  TYPE OF TIME - BILLABLE , NON-BILLABLE, R - TIME                   *         
***********************************************************************         
         SPACE 1                                                                
C        USING XLCHOPD,XLCHOP                                                   
*&&US                                                                           
EDTIME   TM    0(R4),PRTSBILQ      X'80' BILLABLE TIME                          
         BZ    *+8                                                              
         MVI   C.XLCHDAT,C'B'                                                   
         TM    0(R4),PRTSNOTQ      X'40' NON-BILLABLE                           
         BZ    *+8                                                              
         MVI   C.XLCHDAT,C'N'                                                   
         TM    0(R4),PRTSRTEQ      X'80' SPECAIL NON-BILLABLE TIME              
         BZ    *+8                                                              
         MVI   C.XLCHDAT,C'R'                                                   
         B     PRTFRM74                                                         
*&&                                                                             
EDTTME   TM    0(R4),TRSSTADJ      X'08' TIMESHEET ADJUSTMENT                   
         BZ    *+8                                                              
         MVI   C.XLCHDAT,C'A'                                                   
         TM    0(R4),TRSSTMSS      X'04' TIMESHEET MISSING                      
         BZ    *+8                                                              
         MVI   C.XLCHDAT,C'M'                                                   
         TM    0(R4),X'02'         X'02' TMS WRITE OFF                          
         BZ    *+8                                                              
         MVI   C.XLCHDAT,C'W'                                                   
         TM    0(R4),TRSSTIME      X'01' TIMESHEET REGULAR                      
         BZ    *+8                                                              
         MVI   C.XLCHDAT,C'T'                                                   
         B     PRTFRM74                                                         
         DROP  C                                                                
         SPACE 1                                                                
***********************************************************************         
*  RECEIVABLE / VENDOR / JOB / TRANSACTION STATUS                               
***********************************************************************         
         SPACE 1                                                                
         USING TRLTD,R3                                                         
*&&US                                                                           
EDRSTA   L     R3,=A(RCVSTAB)      Receivables status table                     
         B     EDSTATUS                                                         
EDVSTA   L     R3,=A(VENSTAB)      Vendor status table                          
         B     EDSTATUS                                                         
EDESTA   L     R3,=A(ESTSTAB)      Estimate status table                        
         B     EDSTATUS                                                         
*&&                                                                             
EDJSTA   L     R3,=A(ACCSTAB)      Account status table                         
         B     EDSTATUS                                                         
EDBEST   L     R3,=A(ESTTAB)       Estimate status table                        
         CLI   RCCTRY,CTRYGER      TEST GERMANY?                                
         BNE   EDESTAT                                                          
         L     R3,=A(ESTTABG)                                                   
         B     EDESTAT                                                          
EDSTAT   L     R3,=A(TRSTAB1)      Status table 1                               
         CLI   QPROG,RECEIVABLE                                                 
         BNE   *+8                                                              
         L     R3,=A(TRSTAB2)      Status table 2 (Receivable only)             
         CLI   QPROG,MANPOWER                                                   
         BNE   *+8                                                              
         L     R3,=A(TRSTAB3)      Status table 3 (Person writer only)          
         CLI   QPROG,CASH                                                       
         BNE   *+8                                                              
         L     R3,=A(TRSTAB4)      Status table 4 (Cash/Bank only)              
*                                                                               
C        USING XLCHOPD,XLCHOP                                                   
EDSTATUS LA    R2,C.XLCHDAT                                                     
         SR    RF,RF                                                            
EDSTAT2  CLI   0(R3),EOT           END OF TABLE                                 
         BE    EDSTAT8                                                          
         MVC   BYTE,0(R4)          MOVE IN STATUS FROM SORT REC.                
         NC    BYTE,TRLCODE        TEST BIT ON                                  
         BZ    EDSTAT3                                                          
         IC    RF,TRLDLEN          GET LENGTH                                   
         BCTR  RF,0                                                             
         EXMVC RF,0(R2),TRLDATA                                                 
         LA    R2,1(RF,R2)         BUMP UP IN XLCHOP                            
EDSTAT3  AHI   R3,TRLLNQ+8                                                      
         B     EDSTAT2                                                          
*                                                                               
EDSTAT8  CLI   0(R4),0             Any set?                                     
*&&US*&& BE    PRTFRM74            Changed because AIU not showing them         
*&&UK                                                                           
         BNE   EDSTA10                                                          
         CLC   KYWDD#,=AL2(AC#RSSTA)    Test Auth keyword Used ?                
         BNE   PRTFRM74                                                         
         MVC   0(6,R2),=C'UNAUTH'       If yes - show unauth                    
         B     PRTFRM74                                                         
*&&                                                                             
                                                                                
EDSTA10  BCTR  R2,0                Less one                                     
         MVI   0(R2),C' '          GET RID OF COMMA                             
         B     PRTFRM74                                                         
                                                                                
C        USING XLCHOPD,XLCHOP                                                   
EDESTAT  LA    R2,C.XLCHDAT                                                     
         SR    RF,RF                                                            
EDESTA2  CLI   0(R3),EOT           END OF TABLE                                 
         BE    EDESTA4                                                          
         CLC   0(1,R4),TRLCODE                                                  
         BNE   EDESTA3                                                          
         IC    RF,TRLDLEN          GET LENGTH                                   
         BCTR  RF,0                                                             
         EXMVC RF,0(R2),TRLDATA                                                 
         LA    R2,1(RF,R2)         BUMP UP IN XLCHOP                            
EDESTA3  AHI   R3,TRLLNQ+22                                                     
         B     EDESTA2                                                          
                                                                                
EDESTA4  CLI   0(R4),0             Any set?                                     
         BE    PRTFRM74                                                         
         BCTR  R2,0                Less one                                     
         MVI   0(R2),C' '          GET RID OF COMMA                             
         B     PRTFRM74                                                         
         DROP  R3,C                                                             
         SPACE 1                                                                
***********************************************************************         
*  BILLING TYPE                                                                 
***********************************************************************         
         SPACE 1                                                                
         USING ROWD,R2                                                          
         USING BTYPED,R3                                                        
EDBTYP   ICM   R3,15,ROWCON        A(Billing type table) BT or BILTYC           
         BNZ   EDBTYP1                                                          
         L     R3,ABTYPTAB         Default table                                
*                                                                               
EDBTYP1  CLC   KYWDD#,=AL2(AC#RSBTY)       Is it BT keyword ?                   
         BNE   EDBTYP2                                                          
         NI    ROWFLAGS,TURNOFF-ROWCDE                                          
         OI    ROWFLAGS,ROWNME                                                  
*                                                                               
EDBTYP2  CLI   0(R3),EOT           End of table                                 
         BE    PRTFRM74                                                         
         LA    RE,BTYPNUM                  BILTYC / BILTYN                      
         CLC   KYWDD#,=AL2(AC#RSBTY)       Is it BT keyword ?                   
         BNE   *+8                         No                                   
         LA    RE,BTYPCDE                  Yes                                  
         CLC   0(1,RE),0(R4)       Match entry in table                         
         BE    EDBTYP4                                                          
         AHI   R3,BTYPLNQ          Bump to next entry                           
         B     EDBTYP2                                                          
*                                                                               
C        USING XLCHOPD,XLCHOP                                                   
EDBTYP4  TM    ROWFLAGS,ROWCDE     Do we need code ?                            
         JZ    *+10                No                                           
         MVC   C.XLCHDAT(L'BTYPCDE),BTYPCDE                                     
*                                                                               
         TM    ROWFLAGS,ROWNME     Do we need name ?                            
         JZ    PRTFRM74                                                         
*                                                                               
         TM    ROWFLAGS,ROWCDE     Do we need code ?                            
         JNZ   EDBTYP6             yes                                          
*                                                                               
         MVC   C.XLCHDAT(L'BTYPNME),BTYPNME  Bill Type name to print            
         J     PRTFRM74                                                         
*                                                                               
EDBTYP6  MVC   C.XLCHDAT+L'BTYPCDE+2(L'BTYPNME),BTYPNME Bill Type name          
         J     PRTFRM74                                                         
*                                                                               
         DROP  R3,C                                                             
         SPACE 1                                                                
***********************************************************************         
*  PERIOD DATE                                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING ROWD,R2                                                          
C        USING XLCHOPD,XLCHOP                                                   
EDPERID  LH    R4,ROWKYDSP         DISPLACEMENT TO DATA                         
         TM    ROWKYIND,ROWKYCDE                                                
         BO    *+8                                                              
         LH    R4,ROWDADSP                                                      
         A     R4,ACURWRK          ADD BASE OF SORT RECORD                      
         CLC   0(6,R4),SPACES      ANY DATA?                                    
         BNH   EDPERID3                                                         
         LA    R2,8                                                             
         CLC   4(2,R4),=C'00'                                                   
         BH    *+8                                                              
         LA    R2,6                                                             
         GOTO1 DATCON,DMCB,(0,(R4)),((R2),C.XLCHDAT)                            
*                                                                               
EDPERID3 CLC   6(6,R4),SPACES                                                   
         BNH   PRTFRM74                                                         
         LA    R4,6(R4)                                                         
         CLI   0(R4),C'-'                                                       
         BNE   *+8                                                              
         LA    R4,1(R4)            BUMP UP TO COMPENSATE FOR "-"                
         GOTO1 DATCON,DMCB,(0,(R4)),(8,C.XLCHDAT+9)                             
         MVI   C.XLCHDAT+8,C'-'                                                 
         B     PRTFRM74                                                         
         DROP  C                                                                
         SPACE 1                                                                
***********************************************************************         
*  DATE BLOCK                                                         *         
***********************************************************************         
         SPACE 1                                                                
EDDATES  LA    R2,1                                                             
         ICM   R4,15,PRTAREA                                                    
         BZ    PRTFRMX                                                          
         MVC   0(L'AC@RUNDT,R4),AC@RUNDT                                        
         MVI   11(R4),C':'                                                      
         MVC   12(L'REQDATE,R4),REQDATE                                         
         LA    R4,STDPGWD(,R4)     BUMP TO NEXT LINE                            
*                                                                               
         CLC   OCUREND,SPACES      ANY DATE FROM REQUEST CARD ?                 
         BE    EDDATE10                                                         
         LA    R2,1(,R2)                                                        
         MVC   0(10,R4),AC@OVRI                                                 
         MVI   11(R4),C':'         SINGLE DATE DON'T SQUASH                     
         MVC   12(L'OCUREND,R4),OCUREND                                         
         LA    R4,STDPGWD(,R4)     BUMP TO NEXT LINE                            
*                                                                               
EDDATE10 CLC   PERDATE,SPACES                                                   
         BE    EDDATE20                                                         
         LA    R2,1(,R2)           BUMP UP BY ONE                               
         MVC   0(L'AC@PERD,R4),AC@PERD                                          
         MVI   11(R4),C':'                                                      
         MVC   12(L'PERDATE,R4),PERDATE                                         
         LA    R6,12(,R4)                                                       
         GOTO1 ADSQUASH,DMCB,(R6),L'PERDATE                                     
         LA    R4,STDPGWD(,R4)     BUMP TO NEXT LINE                            
*                                                                               
EDDATE20 CLC   MOADATE,SPACES                                                   
         BE    EDDATE30                                                         
         LA    R2,1(,R2)           BUMP UP BY ONE                               
         MVC   0(L'AC@MOARA,R4),AC@MOARA                                        
         MVI   11(R4),C':'                                                      
         MVC   12(L'MOADATE,R4),MOADATE                                         
         LA    R6,12(,R4)                                                       
         GOTO1 ADSQUASH,DMCB,(R6),L'MOADATE                                     
         LA    R4,STDPGWD(,R4)     BUMP TO NEXT LINE                            
*                                                                               
EDDATE30 CLC   ACTDATE,SPACES                                                   
         BE    EDDATE40                                                         
         LA    R2,1(,R2)           BUMP UP BY ONE                               
         LA    R6,12(,R4)                                                       
*        GOTO1 ADSQUASH,DMCB,(R6),L'ACTDATE                                     
         MVC   0(L'AC@ACTY,R4),AC@ACTY                                          
         MVI   11(R4),C':'                                                      
         MVC   12(L'ACTDATE,R4),ACTDATE                                         
         LA    R4,STDPGWD(,R4)     BUMP TO NEXT LINE                            
*                                                                               
EDDATE40 CLI   DATEFLT,0                                                        
         BE    EDDATE50                                                         
         LA    R2,1(,R2)           BUMP UP BY ONE                               
         LA    R6,12(,R4)                                                       
         TM    DATEFLT,DATECHK     CHECK DATE                                   
         BZ    *+10                                                             
         MVCDD 0(10,R4),AC#CHKDT                                                
         TM    DATEFLT,DATEDEP     DEPOSIT DATE                                 
         BZ    *+10                                                             
         MVCDD 0(10,R4),AC#DPSDT                                                
         TM    DATEFLT,DATEITF     INTERFACE DATE                               
         BZ    *+10                                                             
         MVCDD 0(10,R4),AC#RSXDT                                                
*        TM    DATEFLT,DATEBIL     BILLING DATE                                 
*        BZ    *+10                                                             
*        MVCDD 0(10,R4),AC#BILDT                                                
         TM    DATEFLT,DATEBIL     ALTERNATE DATE                               
         BZ    *+10                                                             
         MVCDD 0(10,R4),AC#ALT                                                  
         TM    DATEFLT,DATEOPN     OPEN DATE                                    
         BZ    *+10                                                             
         MVCDD 0(10,R4),AC#OPNDT                                                
         TM    DATEFLT,DATECLS     CLOSED DATE                                  
         BZ    *+10                                                             
         MVCDD 0(10,R4),AC#CLSDT                                                
         TM    DATEFLT,DATEHIR     HIRE DATE                                    
         BZ    *+10                                                             
         MVCDD 0(10,R4),AC#HREDT                                                
         TM    DATEFLT2,DATETRM    TERMINATED DATE                              
         BZ    *+10                                                             
         MVCDD 0(10,R4),AC#TRMDT                                                
         TM    DATEFLT2,DATETSBD   SUBMITTED DATE                               
         BZ    *+10                                                             
         MVCDD 0(10,R4),AC#SUBMT                                                
         TM    DATEFLT2,DATETAPR   APPROVED DATE                                
         BZ    *+10                                                             
         MVCDD 0(10,R4),AC#APRVD                                                
*&&UK*&& TM    DATEFLT,DATEEST     ESTIMATE DATE                                
*&&UK*&& BZ    *+10                                                             
*&&UK*&& MVCDD 0(10,R4),AC#ESTDT                                                
         MVI   11(R4),C':'                                                      
         GOTO1 ADDICTAT,DMCB,C'SU  ',(R4),0                                     
         MVC   12(L'ALTDATE,R4),ALTDATE                                         
*                                                                               
EDDATE50 STC   R2,XLINES                                                        
         B     PRTFRMX                                                          
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
*  ALTERNATE PERIOD DATE                                              *         
***********************************************************************         
         SPACE 1                                                                
EDAPER   MVI   XLINES,1                                                         
         ICM   R4,15,PRTAREA                                                    
         BZ    PRTFRMX                                                          
         CLC   PERDATE,SPACES                                                   
         BE    PRTFRMX                                                          
         MVC   0(L'AC@PERD,R4),AC@PERD                                          
         MVI   11(R4),C':'                                                      
         MVC   12(L'PERDATE,R4),PERDATE                                         
         LA    R6,12(,R4)                                                       
         GOTO1 ADSQUASH,DMCB,(R6),L'PERDATE                                     
         B     PRTFRMX                                                          
         SPACE 1                                                                
***********************************************************************         
*  PERIOD END DATE (PERSON)                                                     
***********************************************************************         
         SPACE 1                                                                
         USING DTED,R2                                                          
C        USING XLCHOPD,XLCHOP                                                   
EDPERD   MVI   XLINES,1                                                         
         SR    R2,R2                                                            
         MVC   BYTE,CUR.RECOTH                                                  
         NI    BYTE,TURNOFF-RECMASK                                             
         ICM   R2,1,BYTE                                                        
         BZ    *+6                 DEFAULT TO FIRST SET OF DATES                
         BCTR  R2,0                                                             
         MH    R2,DTECOLLN                                                      
         A     R2,FMTDTED                                                       
         GOTO1 DATCON,DMCB,(1,DTEPEDST),(EDITDTE1,C.XLCHDAT)                    
         MVI   C.XLCHDAT+10,C'-'                                                
         GOTO1 DATCON,DMCB,(1,DTEPEDEN),(EDITDTE1,C.XLCHDAT+12)                 
         B     PRTFRM74                                                         
         DROP  C                                                                
         SPACE 1                                                                
***********************************************************************         
*  EDIT PAY TYPE                                                                
***********************************************************************         
         SPACE 1                                                                
         USING TRLTD,R3                                                         
C        USING XLCHOPD,XLCHOP                                                   
EDPYTY   MVI   XLINES,1                                                         
         L     R3,=A(EDTPAYTB)                                                  
EDPYTY10 CLI   0(R3),EOT                                                        
         BE    PRTFRM74                                                         
         CLC   TRLCODE,0(R4)                                                    
         BE    EDPYTY20                                                         
         AHI   R3,TRLLNQ+10                                                     
         B     EDPYTY10                                                         
                                                                                
EDPYTY20 MVC   C.XLCHDAT(10),TRLDATA                                            
         B     PRTFRM74                                                         
         DROP  R3,C                                                             
         SPACE 1                                                                
***********************************************************************         
*  PID CODE FROM NUMBER                                                         
***********************************************************************         
         SPACE 1                                                                
EDPID#   L     R3,ABUDIO           Use as temporary work area                   
         OC    0(2,R4),0(R4)       Any PID ?                                    
         BZ    PRTFRM74            No data                                      
         SR    R0,R0                                                            
         ICM   R0,1,NPIDS          NUMBER OF PID# SAVED                         
         BZ    EDPID20                                                          
         LR    R1,R0                                                            
EDPID10  CLC   0(2,R4),0(R3)       MATCH PID# TO SAVE                           
         BE    EDPID90             FOUND                                        
         LA    R3,12(,R3)          BUMP TO NEXT ONE                             
         BCT   R1,EDPID10                                                       
*                                                                               
         LR    RE,R3               RE = A(LAST SLOT)                            
         L     R3,ABUDIO                                                        
EDPID12  SHI   RE,12                                                            
         MVC   12(12,RE),0(RE)                                                  
         CR    RE,R3                                                            
         BH    EDPID12                                                          
*                                                                               
EDPID20  MVC   0(2,R3),0(R4)       MOVE IN PID#                                 
         MVC   2(10,R3),SPACES                                                  
         CLI   NPIDS,20                                                         
         BE    EDPID22                                                          
         SR    R1,R1                                                            
         IC    R1,NPIDS                                                         
         LA    R1,1(,R1)                                                        
         STC   R1,NPIDS                                                         
*                                                                               
         USING SA0REC,R2                                                        
EDPID22  LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   SA0KTYP,SA0KTYPQ    C'0' - PERSONAL AUTH. RECORD                 
         MVC   SA0KAGY,SECALPHA    SECURITY ALPHA ID                            
         MVC   SA0KNUM,0(R3)                                                    
         L     R1,=AL4(IOHIGH+IOCNTRL+IOAREA3)                                  
         GOTO1 AIO                                                              
         L     R2,AIO3                                                          
         CLC   IOKEY(L'SA0KEY),0(R2)                                            
         BNE   EDPID90                                                          
         TM    SA0STAT,X'20'       LOCKED                                       
         BO    EDPID90                                                          
*                                                                               
         USING SAPALD,R2                                                        
         SR    R1,R1                                                            
         LA    R2,SAPEDATA-SAPEKEY(,R2)                                         
EDPID25  CLI   0(R2),0                                                          
         BE    EDPID90                                                          
         CLI   0(R2),SAPALELQ      X'C3' - PERSONAL ID ELEM                     
         BE    EDPID40                                                          
         IC    R1,1(,R2)           NEXT ELEMENT                                 
         AR    R2,R1                                                            
         B     EDPID25                                                          
*                                                                               
EDPID40  MVC   2(L'SAPALPID,R3),SAPALPID                                        
*                                                                               
C        USING XLCHOPD,XLCHOP                                                   
EDPID90  MVC   C.XLCHDAT(L'SAPALPID),2(R3)                                      
         B     PRTFRM74                                                         
         DROP  C                                                                
         SPACE 1                                                                
***********************************************************************         
* Edit pid's that are two byte hex.  Really need EDPID2 code but don't          
* need the special code to check for x'00' in last byte so skip it.             
***********************************************************************         
         SPACE 1                                                                
EDPID3   DS    0H                                                               
*&&US*&& B     EDPID2C                                                          
*&&UK*&& B     EDPID2A                                                          
         SPACE 1                                                                
***********************************************************************         
* For the POA keyword get data from the data portion of the sort rec            
***********************************************************************         
         USING ROWD,R2                                                          
EDPID4   LH    R4,ROWDADSP                                                      
         A     R4,ACURWRK          POINT TO SORT REC & ADD DISPLACEMENT         
*&&US*&& B     EDPID2C                                                          
*&&UK*&& B     EDPID2A                                                          
***********************************************************************         
* Edit pid for production approver and preparer keywords for BrandO             
* (handled differently.)                                                        
***********************************************************************         
         SPACE 1                                                                
EDPID2   CLI   7(R4),X'00'         X'00' in last byte means BrandO pid          
         BH    PRTFR60A            non-BrandO so continue as usual              
*                                                                               
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         MVC   ADDEL,ACMVHELO                                                   
         MVC   SECALPHA,ACMPFAGY                                                
         DROP  R2                                                               
*&&US                                                                           
         USING CT5REC,R2                                                        
         LA    R2,IOKEY                                                         
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ    ACCESS RECORD                                
         MVC   CT5KALPH,SECALPHA   AGENCY ALPHA                                 
         L     R1,=AL4(IOREAD+IOCNTRL+IOAREA2)                                  
         GOTO1 AIO                                                              
         BNE   EDPID2C                                                          
*                                                                               
         SR    R1,R1                                                            
         L     R2,AIO2                                                          
         LA    R2,CT5DATA                                                       
*                                                                               
         USING CTSEAD,R2                                                        
EDPID2B  CLI   0(R2),0                                                          
         BE    EDPID2C                                                          
         CLI   CTSEAEL,CTSEAELQ    X'B8' SECURITY ALPHA                         
         BNE   *+10                                                             
         MVC   SECALPHA,CTSEAAID   SAVE OFF SECECT'S ALPHA AGENCY               
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     EDPID2B                                                          
         DROP  R2                                                               
*                                                                               
EDPID2C  LA    R2,IOKEY                                                         
*&&                                                                             
         USING SA0REC,R2                                                        
*&&UK                                                                           
         USING ACMD,RF                                                          
EDPID2A  L     RF,AMONACC                                                       
         MVC   ADDEL,ACMVHELO                                                   
         MVC   SECALPHA,ACMPFAGY                                                
         DROP  RF                                                               
*&&                                                                             
         LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   SA0KTYP,SA0KTYPQ    C'0' - PERSONAL AUTH. RECORD                 
         MVC   SA0KAGY,SECALPHA    SECURITY ALPHA ID                            
         MVC   SA0KNUM,0(R4)                                                    
         L     R1,=AL4(IOHIGH+IOCNTRL+IOAREA3)                                  
         GOTO1 AIO                                                              
         L     R2,AIO3                                                          
         CLC   IOKEY(L'SA0KEY),0(R2)                                            
         BNE   EDPID2X                                                          
*                                                                               
         USING SAPALD,R2                                                        
         SR    R1,R1                                                            
         LA    R2,SA0DATA-SA0KEY(,R2)                                           
EDPID210 CLI   0(R2),0                                                          
         BE    EDPID2X                                                          
         CLI   0(R2),SAPALELQ      X'C3' - PERSON PERSONAL ID ELEM              
         BE    EDPID220                                                         
         IC    R1,1(,R2)           NEXT ELEMENT                                 
         AR    R2,R1                                                            
         B     EDPID210                                                         
*                                                                               
EDPID220 MVC   WORK(L'SAPALPID),SAPALPID                                        
*                                                                               
         USING SAPEREC,R2                                                       
         LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   SAPETYP,SAPETYPQ    C'F' - PERSON RECORD                         
         MVI   SAPESUB,SAPESUBQ    X'04'                                        
         MVC   SAPEAGY,SECALPHA    SECURITY ALPHA ID                            
         MVC   SAPEPID,WORK                                                     
         L     R1,=AL4(IOHIGH+IOCNTRL+IOAREA3)                                  
         GOTO1 AIO                                                              
         L     R2,AIO3                                                          
         CLC   IOKEY(SAPEDEF-SAPEKEY),0(R2)                                     
         BNE   EDPID2X                                                          
*                                                                               
         USING SANAMD,R2                                                        
         SR    R1,R1                                                            
         LA    R2,SAPEDATA-SAPEKEY(,R2)                                         
EDPID230 CLI   0(R2),0                                                          
         BE    EDPID2X                                                          
         CLI   0(R2),SANAMELQ      X'C5' - PERSONAL NAME ELELEMT                
         BE    EDPID240                                                         
         IC    R1,1(,R2)           NEXT ELEMENT                                 
         AR    R2,R1                                                            
         B     EDPID230                                                         
*                                                                               
EDPID240 LA    RF,SANAMES          NAME SUBELEMS                                
         TM    SANAMIND,SANAMIFN   IS THERE A FIRST NAME                        
         BNO   EDPID250                                                         
         ZIC   R1,0(RF)                                                         
         AHI   R1,-1                                                            
         BM    EDPID250                                                         
C        USING XLCHOPD,XLCHOP                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   C.XLCHDAT(0),1(RF)                                               
*                                                                               
         LA    RE,C.XLCHDAT                                                     
         AR    RE,R1               Point to next open spot in chopdata          
         AHI   RE,2                                                             
*                                                                               
EDPID250 LA    RF,2(R1,RF)                                                      
         TM    SANAMIND,SANAMIMN   IS THERE A MIDDLE NAME                       
         BNO   EDPID260                                                         
         ZIC   R1,0(RF)                                                         
         LA    RF,1(R1,RF)                                                      
EDPID260 TM    SANAMIND,SANAMILN   IS THERE A LAST NAME                         
         BNO   EDPID2X                                                          
         ZIC   R1,0(RF)                                                         
         AHI   R1,-1                                                            
         BM    EDPID2X                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),1(RF)                                                    
*                                                                               
EDPID2X  B     PRTFRM74                                                         
         DROP  C                                                                
         SPACE 1                                                                
***********************************************************************         
         USING SA0REC,R2                                                        
EDPID5   LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   SA0KTYP,SA0KTYPQ    C'0' - PERSONAL AUTH. RECORD                 
         MVC   SA0KAGY,SECALPHA    SECURITY ALPHA ID                            
         MVC   SA0KNUM,0(R4)                                                    
         L     R1,=AL4(IOHIGH+IOCNTRL+IOAREA3)                                  
         GOTO1 AIO                                                              
         L     R2,AIO3                                                          
         CLC   IOKEY(L'SA0KEY),0(R2)                                            
         BNE   EDPID5X                                                          
*                                                                               
         USING SAPALD,R2                                                        
         SR    R1,R1                                                            
         LA    R2,SA0DATA-SA0KEY(,R2)                                           
EDPID510 CLI   0(R2),0                                                          
         BE    EDPID5X                                                          
         CLI   0(R2),SAPALELQ      X'C3' - PERSON PERSONAL ID ELEM              
         BE    EDPID520                                                         
         IC    R1,1(,R2)           NEXT ELEMENT                                 
         AR    R2,R1                                                            
         B     EDPID510                                                         
*                                                                               
EDPID520 MVC   WORK(L'SAPALPID),SAPALPID                                        
*                                                                               
         USING SAPEREC,R2                                                       
         LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   SAPETYP,SAPETYPQ    C'F' - PERSON RECORD                         
         MVI   SAPESUB,SAPESUBQ    X'04'                                        
         MVC   SAPEAGY,SECALPHA    SECURITY ALPHA ID                            
         MVC   SAPEPID,WORK                                                     
         L     R1,=AL4(IOHIGH+IOCNTRL+IOAREA3)                                  
         GOTO1 AIO                                                              
         L     R2,AIO3                                                          
         CLC   IOKEY(SAPEDEF-SAPEKEY),0(R2)                                     
         BNE   EDPID5X                                                          
*                                                                               
         USING SAPEED,R2                                                        
         SR    R1,R1                                                            
         LA    R2,SAPEDATA-SAPEKEY(,R2)                                         
EDPID530 CLI   0(R2),0                                                          
         BE    EDPID5X                                                          
         CLI   0(R2),SAPEEELQ      X'E5' - PERSON EMAIL ELEM                    
         BE    EDPID540                                                         
         IC    R1,1(,R2)           NEXT ELEMENT                                 
         AR    R2,R1                                                            
         B     EDPID530                                                         
*                                                                               
D        USING XLCHOPD,XLCHOP                                                   
EDPID540 LLC   R1,SAPEELN                                                       
         AHI   R1,-3                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   D.XLCHDAT(0),SAPEEID                                             
*                                                                               
EDPID5X  B     PRTFRM74                                                         
         DROP  D                                                                
***********************************************************************         
* Edit static                                                                   
***********************************************************************         
         SPACE 1                                                                
         USING ROWD,R2                                                          
C        USING XLCHOPD,XLCHOP                                                   
EDSTTC   ICM   RE,15,ROWPRFX       No data                                      
         BZ    PRTFRM74                                                         
         ZIC   R1,ROWPRFX          Get full length of field                     
         BCTR  R1,0                                                             
         EXMVC R1,C.XLCHDAT,0(RE)                                               
         B     PRTFRM74                                                         
         DROP  C                                                                
         SPACE 1                                                                
***********************************************************************         
* Edit long invoice number (LBILL keyword) for SJ to include dashes             
***********************************************************************         
         SPACE 1                                                                
C        USING XLCHOPD,XLCHOP                                                   
EDLBILL  CLI   QPROG,PRODUCTION    Only need to do this for SJ                  
*&&UK                                                                           
         BE    *+14                                                             
         MVC   C.XLCHDAT(10),0(R4)                                              
         B     PRTFRM74                                                         
*&&                                                                             
*&&US                                                                           
         BNE   *+12                                                             
         CLI   1(R4),C'-'          It is SJ but is there already a              
         BNE   *+14                longer inv?                                  
         MVC   C.XLCHDAT(10),0(R4) Just move in number                          
         B     PRTFRM74                                                         
*&&                                                                             
         CLC   1(5,R4),SPACES      Any INV NUMBER?                              
         BNH   PRTFRM74            No data                                      
         MVC   C.XLCHDAT(1),0(R4)  Move in media portion                        
         MVI   C.XLCHDAT+1,C'-'                                                 
         MVC   C.XLCHDAT+2(2),1(R4) Move in ym portion                          
         MVI   C.XLCHDAT+4,C'-'                                                 
         MVC   C.XLCHDAT+5(4),3(R4) Move in ref portion                         
         B     PRTFRM74                                                         
         DROP  C                                                                
         SPACE 1                                                                
***********************************************************************         
* Remove dashes from long invoice number if applicable (LBLLND keyw)            
***********************************************************************         
         SPACE 1                                                                
EDLBLL2  DS    0H                                                               
*&&US*&& CLC   1(5,R4),SPACES      Any invoice number?                          
*&&UK*&& CLC   0(6,R4),SPACES                                                   
         BNH   PRTFRM74            No data                                      
C        USING XLCHOPD,XLCHOP                                                   
         LA    R1,C.XLCHDAT                                                     
         XR    RF,RF                                                            
         ICM   RF,3,ROWKYSZ        length of data+1                             
         BCTR  RF,0                                                             
EDLB210  CLI   0(R4),C' '                                                       
         BNH   PRTFRM74                                                         
         CLI   0(R4),C'-'                                                       
         BE    *+14                                                             
         MVC   0(1,R1),0(R4)                                                    
         AHI   R1,1                                                             
         AHI   R4,1                                                             
         BCT   RF,EDLB210                                                       
         B     PRTFRM74                                                         
         DROP  C                                                                
         SPACE 1                                                                
*&&US                                                                           
***********************************************************************         
* Edit zip code to insert dash if extra four characters used                    
***********************************************************************         
         SPACE 1                                                                
C        USING XLCHOPD,XLCHOP                                                   
EDZIPC   CLC   0(5,R4),SPACES      Any zip found?                               
         BNH   PRTFRM74            No                                           
         MVC   C.XLCHDAT(5),0(R4)                                               
         CLC   5(4,R4),SPACES      Extra 4 in zip used?                         
         BNH   PRTFRM74            No                                           
         MVI   C.XLCHDAT+5,C'-'                                                 
         MVC   C.XLCHDAT+6(4),5(R4)                                             
         B     PRTFRM74                                                         
         DROP  C                                                                
         SPACE 1                                                                
***********************************************************************         
* Edit payment method bit for printing (PAYMTD)                                 
***********************************************************************         
         SPACE 1                                                                
C        USING XLCHOPD,XLCHOP                                                   
EDPYMTD  DS    0H                                                               
         TM    0(R4),MPYEFT                                                     
         BZ    *+14                EFT?                                         
         MVC   C.XLCHDAT(L'AC@EFT),AC@EFT                                       
         B     PRTFRM74                                                         
         TM    0(R4),MPYSOON                                                    
         BZ    *+14                                                             
         MVC   C.XLCHDAT(L'AC@CHK),AC@CHK                                       
         B     PRTFRM74                                                         
         TM    0(R4),MPYPCRD                                                    
         BZ    *+14                                                             
         MVC   C.XLCHDAT(L'AC@PCARD),AC@PCARD                                   
         B     PRTFRM74                                                         
         TM    0(R4),MPYCCRD                                                    
         BZ    PRTFRM74                                                         
         MVC   C.XLCHDAT(L'AC@CCARD),AC@CCARD                                   
         B     PRTFRM74                                                         
         DROP  C                                                                
         SPACE 1                                                                
***********************************************************************         
* Edit payment method company bit for printing (PAYMTC)                         
***********************************************************************         
         SPACE 1                                                                
C        USING XLCHOPD,XLCHOP                                                   
EDPYMTC  DS    0H                                                               
         TM    0(R4),MPYPCRD                                                    
         BZ    *+14                                                             
         MVC   C.XLCHDAT(36),=CL36'JP Morgan Chase'                             
         B     PRTFRM74                                                         
         TM    0(R4),MPYCCRD                                                    
         BZ    PRTFRM74                                                         
         MVC   C.XLCHDAT(36),=CL36'CSI'                                         
         B     PRTFRM74                                                         
         DROP  C                                                                
         SPACE 1                                                                
***********************************************************************         
* Edit approval method bit for printing (APMTD)                                 
***********************************************************************         
C        USING XLCHOPD,XLCHOP                                                   
EDAPMTD  CLI   0(R4),GDAAPPMK                                                   
         BNE   *+14                Approved by marker?                          
         MVC   C.XLCHDAT(L'AC@MAPP),AC@MAPP                                     
         B     PRTFRM74                                                         
         CLI   0(R4),GDAAPPAA      Approved by auto approve?                    
         BNE   PRTFRM74                                                         
         MVC   C.XLCHDAT(L'AC@AAPP),AC@AAPP                                     
         B     PRTFRM74                                                         
         DROP  C                                                                
         SPACE 1                                                                
*&&                                                                             
*&&UK                                                                           
***********************************************************************         
* Edit VAT region                                                     *         
***********************************************************************         
         SPACE 1                                                                
C        USING XLCHOPD,XLCHOP                                                   
EDVATRG  DS    0H                                                               
         CLI   RCCTRY,CTRYGER      TEST GERMANY?                                
         BNE   EDVATRG2            No - don't show NATIONAL                     
         CLI   0(R4),DDTAXLNQ      Test NATIONAL                                
         BNE   EDVATRG2                                                         
         MVCDD C.XLCHDAT(8),AC#NAT                                              
         B     EDVATRD                                                          
*                                                                               
EDVATRG2 CLI   0(R4),DDTAXLEQ      Test EU                                      
         BNE   EDVATRG4                                                         
         MVCDD C.XLCHDAT(8),AC#EU                                               
         B     EDVATRD                                                          
*                                                                               
EDVATRG4 CLI   0(R4),DDTAXLXQ      or non-EU                                    
         BNE   PRTFRM74                                                         
         MVCDD C.XLCHDAT(8),AC#NONEU                                            
EDVATRD  GOTO1 ADDICTAT,DMCB,C'SL  ',C.XLCHDAT,0                                
         B     PRTFRM74                                                         
         DROP  C                                                                
***********************************************************************         
* Edit PO acknowledgement status                                      *         
***********************************************************************         
          SPACE 1                                                               
C        USING XLCHOPD,XLCHOP                                                   
EDPOAK   DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    PRTFRM74                                                         
*                                                                               
         CLI   0(R4),ORDGQUER      X'01' QUERIED                                
         BNE   EDPO02                                                           
         MVCDD C.XLCHDAT(9),AC#QURD                                             
         B     EDPOAKY                                                          
*                                                                               
EDPO02   CLI   0(R4),ORDGACKN      X'02' ACKNOWLEDGED                           
         BNE   EDPO04                                                           
         MVCDD C.XLCHDAT(12),AC#ACKD                                            
         B     EDPOAKY                                                          
*                                                                               
EDPO04   CLI   0(R4),ORDGACCS      X'03' ACCESSED                               
         BNE   EDPO06                                                           
         MVCDD C.XLCHDAT(9),AC#ACCSD                                            
         B     EDPOAKY                                                          
*                                                                               
EDPO06   CLI   0(R4),ORDGSENT      X'04' SENT                                   
         BNE   EDPO08                                                           
         MVCDD C.XLCHDAT(8),AC#SENT                                             
         B     EDPOAKY                                                          
*                                                                               
EDPO08   CLI   0(R4),ORDGNSNT      X'05' NOT SENT                               
         BNE   EDPO10                                                           
         MVCDD C.XLCHDAT(14),AC#NSNT                                            
         B     EDPOAKY                                                          
*                                                                               
EDPO10   CLI   0(R4),ORDGRECA      X'06' RECALLED/STOPPED                       
         BNE   EDPO12                                                           
         MVCDD C.XLCHDAT(8),AC#STPPD                                            
         B     EDPOAKY                                                          
*                                                                               
EDPO12   CLI   0(R4),ORDGREXP      X'07' EXPIRED                                
         BNE   PRTFRM74                                                         
         MVCDD C.XLCHDAT(10),AC#XPRD                                            
*                                                                               
EDPOAKY  GOTO1 ADDICTAT,DMCB,C'SL  ',C.XLCHDAT,0                                
         B     PRTFRM74                                                         
         DROP  C                                                                
***********************************************************************         
* Edit Estimate acknowledgement status                                *         
***********************************************************************         
          SPACE 1                                                               
C        USING XLCHOPD,XLCHOP                                                   
EDESAK   DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    PRTFRM74                                                         
         CLI   0(R4),EMDGREJ       X'01' REJECTED                               
         BNE   *+10                                                             
         MVC   C.XLCHDAT(8),=C'REJECTED'                                        
         CLI   0(R4),EMDGAPP       X'02' ACCEPTED                               
         BNE   *+10                                                             
         MVC   C.XLCHDAT(8),=C'ACCEPTED'                                        
         CLI   0(R4),EMDGACCS      X'03' ACCESSED                               
         BNE   *+10                                                             
         MVC   C.XLCHDAT(8),=C'ACCESSED'                                        
         CLI   0(R4),EMDGSENT      X'04' SENT                                   
         BNE   *+10                                                             
         MVC   C.XLCHDAT(4),=C'SENT'                                            
         CLI   0(R4),EMDGNSNT      X'05' NOT SENT                               
         BNE   *+10                                                             
         MVC   C.XLCHDAT(8),=C'NOT SENT'                                        
         CLI   0(R4),EMDGRECA      X'06' RECALLED                               
         BNE   *+10                                                             
         MVC   C.XLCHDAT(8),=C'RECALLED'                                        
         CLI   0(R4),EMDGREXP      X'07' EXPIRED                                
         BNE   *+10                                                             
         MVC   C.XLCHDAT(7),=C'EXPIRED'                                         
         B     PRTFRM74                                                         
         DROP  C                                                                
         EJECT ,                                                                
***********************************************************************         
* Edit - Show 'Yes' if status is on                                   *         
***********************************************************************         
         SPACE 1                                                                
C        USING XLCHOPD,XLCHOP                                                   
EDSTATY  DS    0H                                                               
         CLI   0(R4),0             Any set?                                     
         BE    PRTFRM74                                                         
         MVC   C.XLCHDAT(L'AC@YES),AC@YES                                       
         B     PRTFRM74                                                         
         DROP  C                                                                
                                                                                
***********************************************************************         
* Edit - Type mapping keyword for GROUPM                              *         
***********************************************************************         
         SPACE 1                                                                
         USING COLD,R3                                                          
C        USING XLCHOPD,XLCHOP                                                   
EDTYMAP  DS    0H                                                               
         CLI   0(R4),0             Any set?                                     
         BE    PRTFRM74                                                         
         MVC   C.XLCHDAT(1),0(R4)                                               
         CLI   0(R4),EDTMNBIL      TEST NON BILLING TYPE                        
         BNE   PRTFRM74            NO - OK                                      
*                                                                               
         L     RF,COLXTRA          LOAD STACKED EQUATION                        
         MVC   BYTE,EQCOL#-EQD(RF) MAPPING COLUMN NUMBER                        
         DROP  R3                                                               
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,BYTE                                                        
         BZ    PRTFRM74            No column number                             
         BCTR  R1,0                                                             
         MHI   R1,COLLNQ                                                        
         L     RF,FMTCOL                                                        
         AR    RF,R1               RF -> mapping column                         
         USING COLD,RF                                                          
         TM    COLFLAGS,COLAMT     Regular amount ?                             
         BZ    PRTFRM74                                                         
*                                                                               
         L     R1,ASRTWRK          POINT TO SORT REC                            
         AH    R1,COLDSP           Point to accum.                              
         OC    0(PKLEN,R1),0(R1)                                                
         BZ    PRTFRM74                                                         
         CP    0(PKLEN,R1),PKZERO  Test mapping column amount (Total)           
         BNL   *+8                                                              
         MVI   C.XLCHDAT,EDTMCRD   SET CHAR TO "C" (CREDIT)                     
         B     PRTFRM74                                                         
         DROP  C,RF                                                             
*&&                                                                             
         DROP  R2,R5,R7,R8                                                      
         DROP  CUR,PRV                                                          
         EJECT ,                                                                
*=====================================================================*         
* Edit LTORG                                                                    
*=====================================================================*         
         LTORG                                                                  
         DROP  R9                                                               
         TITLE 'EDIT OUT NUMBERS TO PRINT ON REPORT'                            
         USING FMTRECD,R5                                                       
         USING CURPARMD,R6                                                      
         USING ACRL2D,R7                                                        
         USING EDWRKD,R9                                                        
EDITIT   NMOD1 EDEND-EDWRKD,**EDIT**,CLEAR=YES                                  
         LR    R9,RC                                                            
         L     RC,RLBASEC                                                       
         L     R7,AACRL2D                                                       
         LA    R6,EDPARM           Covers CUREDIT parameters                    
         L     R4,0(,R1)                                                        
         MVC   EDPACK,0(R4)                                                     
         LA    R4,EDPACK                                                        
         STCM  R4,15,CURPIADD      ADDRESS OF PACKED NUMBER                     
         MVC   EDPRT,4(R1)         SAVE PRINT AREA ADDRESS                      
         MVC   EDSIZSV,4(R1)       SAVE FIELD SIZE                              
         MVC   EDFLAG,0(R1)        EDIT FLAGS                                   
         ICM   R8,15,8(R1)         ADDRESS OF FOREIGN CURRENCY DATA             
         BZ    *+10                                                             
         MVC   EDFCFLD,0(R8)       MOVE IN DATA                                 
         MVI   CURPINPT,PKLEN      LENGTH OF PACKED NUMBER                      
         OI    CURPINPT,CURPIPAK   PACKED TYPE INPUT                            
         LA    R8,EDNUM            WORK AREA                                    
         STCM  R8,7,CURPOADD       OUTPUT ADDRESS                               
         MVI   CURPOLEN,L'EDNUM                                                 
*                                                                               
         TM    DWNOPT1,DWNGO            Are we downloading ?                    
         BZ    EDIT020                  No                                      
         LA    RE,XLCHOP                                                        
         ST    RE,EDPRT                                                         
         LHI   RF,GESIZEQ                                                       
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         ICM   R1,8,SPACES         SET PADDING CHARACTER                        
         MVCL  RE,R0                                                            
         TM    DWNOPT1,DWNTXT#                                                  
         BO    EDIT010                                                          
         TM    EDFLAG,EDCRDIT+EDBRCKT                                           
         BZ    *+8                                                              
         OI    EDFLAG,EDMINUS      Switch to trailing                           
         NI    EDFLAG,TURNOFF-EDCOMMA-EDPRCNT-EDCRDIT-EDBRCKT                   
         OI    EDFLAG,EDZEROB                                                   
                                                                                
EDIT010  TM    DWNOPT1,DWNLEADM         Over-ride option, leading minus         
         BZ    EDIT012                  No, use general or column prof          
         OI    EDFLAG,EDFLOAT           Yes                                     
         NI    EDFLAG,TURNOFF-EDCRDIT-EDBRCKT-EDMINUS                           
                                                                                
EDIT012  MVC   EDSIZE,EDSIZSV      Re-load size                                 
         TM    DWNOPT1,DWNCHOP     Is it fixed len ?                            
         BO    EDIT020             Yes                                          
         TM    DWNOPT2,DWN#PAD     Pad with leading zeros ?                     
         BO    EDIT020             Keep size of column for now                  
         MVI   EDSIZE,L'EDNUM      No, set to max size                          
                                                                                
EDIT020  OC    EDPACK,EDPACK       If null then "EEEEE" for error               
         BZ    EDIT800             Yes                                          
         TM    EDFLAG,EDZEROB      Print blank for zero ?                       
         BO    EDIT100             Yes                                          
         CP    EDPACK,PKZERO       No, Is it zero ?                             
         BE    EDIT900             Yes, so exit, it will be blank               
                                                                                
EDIT100  TM    PRTOPT,PRTREVSG     Reverse sign ?                               
         BZ    *+10                No                                           
         MP    EDPACK,PKNEG        Yes reverse                                  
                                                                                
         TM    EDFLAG,EDBRCKT      Print amounts minus sign as "()"             
         BZ    EDIT120             No                                           
         OI    CURPEDT1,CURPBRAY   Yes                                          
         CP    EDPACK,PKZERO       Is amount minus ?                            
         BM    EDIT120             Yes                                          
         MVI   CURPOLEN,L'EDNUM-1          Shorten len to align pos #'s         
         NI    CURPEDT1,TURNOFF-CURPBRAY   Turn off minus as "()"               
         MVI   EDNUM+L'EDNUM-1,C' '        Clear last byte                      
*                                                                               
EDIT120  OI    CURPEDT1,CURPSYMN   Default to no currency symbol                
         TM    EDFLAG,EDCRDIT      Print sign as CR or DR ?                     
         BZ    *+8                 No                                           
         OI    CURPEDT1,CURPCRDY   Yes                                          
*                                                                               
         TM    EDFLAG,EDFLOAT      Leading minus sign ?                         
         BZ    EDIT130             No                                           
         OI    CURPEDT2,CURPFLON   Yes, float minus                             
         B     EDIT140                                                          
*                                                                               
EDIT130  TM    EDFLAG,EDMINUS      Show minus                                   
         BZ    *+8                 No                                           
         OI    CURPEDT1,CURPMINY   Yes                                          
*                                                                               
EDIT140  TM    EDFLAG,EDCOMMA      Show commas                                  
         BZ    *+8                 No                                           
         OI    CURPEDT1,CURPCOMY   Yes                                          
*                                                                               
         TM    PRTOPT,PRTNOPFX                 Over-ride prefix                 
         BO    EDIT150                         Yes                              
         TM    FCIND,FCIPRFIX                  Show currency prefix             
         BZ    EDIT150                         No                               
         NI    CURPEDT1,TURNOFF-CURPSYMN       Yes, use currency symbol         
*                                                                               
EDIT150  MVC   CURPCIND,EDFCFLD+4                                               
         MVC   CURPCSYM,EDFCFLD+5                                               
         CLI   NDECIMAL,5          Print 5 deciamals places ?                   
         BNE   EDIT180             No                                           
         OI    CURPEDT1,CURPSYMN   Yes, don't use currency symbol               
*                                                                               
EDIT180  MVC   CURPCDEC,NDECIMAL   Columns to print # decmails                  
         CLI   CURPCDEC,0          Print with no decimals                       
         BNE   *+8                 No                                           
         OI    CURPEDT1,CURPDECN   Yes. No decimal places                       
*                                                                               
         SR    R2,R2               Rounding                                     
         ICM   R2,1,ROUNDTO        Power of 10 to divide by                     
         BZ    EDIT200                                                          
         SRP   EDPACK,0(R2),5                                                   
*                                                                               
EDIT200  TM    DWNOPT1,DWNNODEC    No decimals                                  
         BZ    *+8                 No                                           
         MVI   CURPCDEC,0          Yes                                          
***********************************************************************         
*  EDIT NUMBER INTO EDNUM                                             *         
***********************************************************************         
                                                                                
         GOTO1 CUREDIT,EDPARM                                                   
                                                                                
         LA    R1,EDNUM            FIND FIRST SIGNIFICANT DIGIT                 
         LA    RF,L'EDNUM          AT THE END OF THIS LOOP                      
         CLI   0(R1),C' '             RF = NUMBER OF NON-BLANK DIGITS           
         BH    *+12                        (CHARACTERS) IN THE NUMBER           
         LA    R1,1(,R1)              R1 = ADDRESS OF FIRST NON-BLANK           
         BCT   RF,*-12                     DIGIT (CHARACTER)                    
                                                                                
         ST    R1,EDDATA@          Save start  of data                          
         ST    RF,EDDATASZ         Save length of data                          
*                                                                               
         CLI   NDECIMAL,5          SPECIAL CODE IF NO. OF DECIMALS = 5          
         BNE   EDIT230                                                          
         LA    RE,EDNUM+L'EDNUM-1  AT THE END OF THIS CODE                      
         CLI   0(RE),C' '             THE NON-SIGNIFICANT DECIMAL               
         BH    EDIT220                PLACES ARE REPLACED WITH BLANKS           
         BCTR  RE,0                                                             
         LA    R0,4                                                             
EDIT220  CLI   0(RE),C'0'                                                       
         BNE   EDIT230                                                          
         MVI   0(RE),C' '          BLANK OUT THIS ZERO                          
         BCTR  RE,0                BUMP BACK BY ONE                             
         BCT   R0,EDIT220                                                       
*                                                                               
EDIT230  TM    DWNOPT1,DWNGO       Downloading ?                                
         BZ    EDIT300             No                                           
         TM    DWNOPT2,DWN#PAD     Pad with leading zeros ?                     
         BO    EDIT240             Yes                                          
                                                                                
         TM    DWNOPT1,DWNTXT#+DWNCHOP  Numbers as text and fixed len ?         
         BNO   EDIT290                  No                                      
         ZIC   R2,EDSIZE                Yes                                     
         SR    R2,RF                    Will field fit in fixed area ?          
         BNM   EDIT500                  Yes                                     
                                                                                
EDIT235  L     R4,EDPRT                 No, print "*"s instead                  
         LA    R1,STARS                                                         
         B     EDIT810                                                          
                                                                                
EDIT240  DS    0H                  Insert leading zeros                         
         L     R8,EDDATA@          R8 = A(Start of data)                        
         L     RE,EDDATASZ         RE = Size of data                            
         ZIC   RF,EDSIZE           RF = Size of print area                      
         TM    DWNOPT1,DWNCHOP     Must use size of print area                  
         BO    EDIT242             Yes                                          
         CR    RE,RF               Is data larger then print area ?             
         BNH   EDIT242             This is ok                                   
         LR    RF,RE                                                            
         STC   RE,EDSIZE           Increase the size                            
                                                                                
EDIT242  LA    R1,EDNUM+L'EDNUM    R1 = A(End of the data field)                
         SR    R1,RF               R1 = A(Start of full size field)             
         ST    R1,EDDATA@          Save off new start location                  
         CR    RF,RE               RF = size of area, RE = data size            
         BL    EDIT235             Error, data won't fit                        
         BE    EDIT260             No room for zeros, but ok as is              
                                                                                
EDIT245  TM    EDFLAG,EDFLOAT+EDBRCKT                                           
         BZ    EDIT250                                                          
         CP    EDPACK,PKZERO       Was the number zero                          
         BNM   EDIT250                                                          
         MVC   0(1,R1),0(R8)       Move the sign                                
         AHI   R1,1                Bump past minus sign                         
         AHI   R8,1                Bump up to ignore minus sign                 
                                                                                
EDIT250  SR    R8,R1               R8 = length to pad with zeros                
         SHI   R8,1                See if any room to pad                       
         BM    EDIT260             No                                           
         MVI   0(R1),C'0'          Pad the first                                
         SHI   R8,1                Less one ex instr.                           
         BM    EDIT260             No room for more                             
         EXMVC R8,1(R1),0(R1)                                                   
                                                                                
EDIT260  L     R4,EDPRT            A(OUTPUT)                                    
         L     R1,EDDATA@          A(INPUT)                                     
         SHI   RF,1                                                             
         BM    EDIT800                                                          
         EXMVC RF,0(R4),0(R1)      INSERT THE NUMERIC BYTES                     
         B     EDIT900             EXIT                                         
*                                                                               
EDIT290  DS    0H                  STANDARD DOWN-LOAD LOGIC CONTINUED           
         STC   RF,PRTSIZE                                                       
         STC   RF,EDSIZE                                                        
*                                                                               
EDIT300  TM    EDFLAG,EDCRDIT      IS NEGATIVE "CR"                             
         BZ    EDIT400                                                          
         CLC   EDNUM+L'EDNUM-2(2),=C'CR'                                        
         BNE   EDIT400                                                          
         MVC   EDNUM+L'EDNUM-2(2),NEGCHAR                                       
*                                                                               
EDIT400  TM    EDFLAG,EDPRCNT      IS IT A PERCENT                              
         BZ    EDIT500                                                          
         BCTR  RF,0                                                             
         BCTR  R1,0                                                             
         ST    R1,EDDATA@          Save start  of data                          
         EXMVC RF,0(R1),1(R1)      Move it over one, to put "%" at end          
                                                                                
         MVI   EDNUM+L'EDNUM-1,C'%'                                             
         TM    EDFLAG,EDBRCKT      If minus "()" then edit to "(nn%)            
         BZ    EDIT410                                                          
         MVC   EDNUM+L'EDNUM-1(1),EDNUM+L'EDNUM-2                               
         MVI   EDNUM+L'EDNUM-2,C'%'                                             
                                                                                
EDIT410  LA    RF,2(,RF)           Increase size for EX instr & "%"             
         ST    RF,EDDATASZ         Save new length of data                      
*                                                                               
EDIT500  L     R4,EDPRT                                                         
         SR    R3,R3                                                            
         L     RF,EDDATASZ         Data size                                    
         IC    R3,EDSIZE           Size of area where number goes               
         SR    R3,RF               Indent = Column size - data size             
         BCTR  RF,0                                                             
         BNM   EDIT850             IS NUMBER TOO LARGE?                         
         LR    R2,R4                                                            
         AR    R2,R3               ADD NEG. NUMBER TO ADDRESS                   
         AHI   R2,-2               MAKE SURE 2 SPACES BETWEEN ANY CHAR          
         EX    RF,ANYROOM                                                       
         BE    EDIT850             PRINT OVER BOX                               
         LA    R2,STDPGWD(,R2)     BUMP TO NEXT LINE                            
         EX    RF,ANYROOM                                                       
         BE    EDIT700                                                          
         LA    R1,STARS            YES, SO PRINT "*******"                      
         B     EDIT810                                                          
*                                                                               
EDIT700  LA    R4,STDPGWD(,R4)     BUMP UP TO NEXT PRINT LINE                   
         B     EDIT850                                                          
*                                                                               
EDIT800  LA    R1,SPACES           YES, SO PRINT ERROR AS BLANK                 
         TM    DWNOPT1,DWNGO       ARE WE DOWNLOADING?                          
         BZ    *+8                 NO, SKIP                                     
         LA    R1,ZEROS            PRINT ERROR AS ZERO                          
         TM    FMTIND,FMTIERR                                                   
         BZ    *+8                                                              
         LA    R1,ERRRS            YES, SO PRINT ERROR AS "EEEEEEE"             
         L     R4,EDPRT                                                         
*                                                                               
EDIT810  SR    RF,RF                                                            
         IC    RF,EDSIZSV          SIZE OF AREA TO PRINT IN                     
         CLI   EDSIZSV,ERRRSQ      SIZE > ERROR DATA LENGTH ?                   
         BNH   *+8                 NO,  SKIP                                    
         LA    RF,ERRRSQ           USE  ERROR DATA LENGTH                       
         BCTR  RF,0                                                             
         SR    R3,R3                                                            
*                                                                               
EDIT850  AR    R4,R3               INDENT TO RIGHT JUSTIFY                      
         EXMVC RF,0(R4),0(R1)                                                   
*                                                                               
EDIT900  XIT1                                                                   
         DROP  R5,R7,R9                                                         
                                                                                
ANYROOM  CLC   0(0,R2),SPACES                                                   
         EJECT ,                                                                
ERRRS    DC    20CL1'E'                                                         
ERRRSQ   EQU   *-ERRRS                                                          
STARS    DC    20CL1'*'                                                         
ZEROS    DC    20CL1'0'                                                         
         SPACE 2                                                                
         LTORG                                                                  
         TITLE 'Control special forms of date printing'                         
DATING   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(,R1)           Date     to print                            
         L     R4,4(,R1)           Location to put                              
         LR    R9,R1                                                            
         MVC   FROMDATE,0(R1)      From date                                    
         MVI   YM_ONLY,NO                                                       
         TM    FROMDATE,X'80'      Year month only ?                            
         BZ    *+8                                                              
         MVI   YM_ONLY,YES                                                      
         NI    FROMDATE,TURNOFF-X'80'                                           
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,1,4(R1)          Index into DATEFMT (ACRL03)                  
         BNZ   DATI100                                                          
         LA    R3,DDEFAULT         Default if not set                           
         B     DATI102             No                                           
*                                                                               
DATI100  BCTR  R3,0                                                             
         MHI   R3,DATELNQ                                                       
         A     R3,ADATEFMT                                                      
*                                                                               
         USING DATED,R3                                                         
DATI102  SR    R6,R6                                                            
         IC    R6,DATELLEN         Long form                                    
         MVC   DATECONV,DATEL#                                                  
         CLI   YM_ONLY,YES                                                      
         BNE   DATI105                                                          
         IC    R6,DATESLEN         Short form                                   
         MVC   DATECONV,DATES#                                                  
*                                                                               
DATI105  CLI   0(R2),X'FF'         Check date for X'FF' data                    
         BE    DATIXIT                                                          
         CLI   0(R2),0                                                          
         BE    DATIXIT                                                          
         CLI   DATEFORM,100        Special date type                            
         BL    DATI120                                                          
         MVI   WORK+0,X'99'        Packed year      form YY                     
         CLI   FROMDATE,1          Packed form ?                                
         BE    DATI110             Yes                                          
         MVI   WORK+0,99           Binary year      form YY                     
         CLI   FROMDATE,3          Binary form ?                                
         BE    DATI110                                                          
         DC    H'00'               Not supported                                
*                                                                               
DATI110  MVI   WORK+1,01           Binary or packed form MM                     
         MVI   WORK+2,01           Binary or packed form DD                     
         CLI   DATEFORM,EDITMNTH                                                
         BNE   *+10                                                             
         MVC   WORK+1(1),0(R2)      Over-ride month                             
         CLI   DATEFORM,EDITYEAR                                                
         BNE   *+10                                                             
         MVC   WORK(1),0(R2)       Over-ride year                               
*                                  output type 23 is yyyy-mm-dd (ISO)           
         GOTO1 DATCON,DMCB,(FROMDATE,WORK),(DATEL#,WORK+6)                      
         SHI   R6,1                                                             
         BNM   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         EXMVC R6,0(R4),WORK+6      Move in YYYY or MMM                         
         B     DATIXIT                                                          
*                                                                               
*ATI120  CLI   0(R2),X'FF'         Check date for X'FF' data                    
*        BE    DATIXIT                                                          
*        CLI   0(R2),0                                                          
*        BE    DATIXIT                                                          
DATI120  MVC   WORK(12),SPACES                                                  
         GOTO1 DATCON,DMCB,(FROMDATE,(R2)),(DATECONV,WORK)                      
*                                                                               
         CLI   DATEFORM,30         Convert MMMDD/YY to DDMMMYY                  
         BNE   DATI140                                                          
         MVC   WORK+15(5),WORK     Save off MMMDD                               
         MVC   WORK+3(2),WORK+6    Move YY                                      
         CLI   YM_ONLY,YES         Want only month year ?                       
         BE    DATI250             Yes                                          
         MVC   WORK(2),WORK+18     Move DD                                      
         MVC   WORK+2(3),WORK+15   Move MMM                                     
         MVC   WORK+5(2),WORK+6    Move YY                                      
         B     DATI250                                                          
*                                                                               
DATI140  CLI   DATEFORM,31         Convert YYYY-MM-DD to MM/DD/YYYY             
         BNE   DATI141                                                          
         MVC   WORK+15(10),WORK                                                 
         MVC   WORK(2),WORK+20     Move in MM                                   
         MVI   WORK+2,C'/'                                                      
         CLI   YM_ONLY,YES         Want only month year ?                       
         BNE   DATI140A            Yes                                          
         MVC   WORK+3(4),WORK+15   Move in YYYY                                 
         B     DATI250                                                          
                                                                                
DATI140A MVC   WORK+3(2),WORK+23   Move in DD                                   
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(4),WORK+15   Move in YYYY                                 
         B     DATI250                                                          
*                                                                               
DATI141  CLI   DATEFORM,32         Convert YYYYMMDD to DDMMYYYY                 
         BNE   DATI142                                                          
         MVC   WORK+15(8),WORK                                                  
         CLI   YM_ONLY,YES         Want only month year ?                       
         BNE   *+20                Yes                                          
         MVC   WORK(2),WORK+19     Move in MM                                   
         MVC   WORK+2(4),WORK+15   Move in YYYY                                 
         B     DATI250                                                          
         MVC   WORK(2),WORK+21     Move in DD                                   
         MVC   WORK+2(2),WORK+19   Move in MM                                   
         MVC   WORK+4(4),WORK+15   Move in YYYY                                 
         B     DATI250                                                          
*                                                                               
DATI142  CLI   DATEFORM,33         Convert YYMMDD to MMDDYY                     
         BNE   DATI143                                                          
         MVC   WORK+15(6),WORK                                                  
         MVC   WORK(2),WORK+17     Move in MM                                   
         CLI   YM_ONLY,YES         Want only month year ?                       
         BNE   *+14                                                             
         MVC   WORK+2(2),WORK+15   Move in YY                                   
         B     DATI250                                                          
         MVC   WORK+2(2),WORK+19   Move in DD                                   
         MVC   WORK+4(2),WORK+15   Move in YY                                   
         B     DATI250                                                          
*                                                                               
DATI143  CLI   DATEFORM,34         Convert YYYYMMDD to MMDDYYYY                 
         BNE   DATI145                                                          
         MVC   WORK+15(8),WORK                                                  
         MVC   WORK(4),WORK+19     Move in MMDD                                 
         CLI   YM_ONLY,YES         Want only month year ?                       
         BNE   *+14                Yes                                          
         MVC   WORK+2(4),WORK+15   Move in YYYY                                 
         B     DATI250                                                          
         MVC   WORK+4(4),WORK+15   Move in YYYY                                 
         B     DATI250                                                          
*                                                                               
DATI145  CLI   YM_ONLY,YES         Do we only want month year data ?            
         BNE   DATI250             ok as is                                     
         CLI   DATEFORM,10         MM/DD/YY (US), DD/MM/YY (UK)                 
         BNE   *+10                                                             
*&&US*&& MVC   WORK+3(2),WORK+6    Move YY over DD                              
*&&UK*&& MVC   WORK(5),WORK+3                                                   
                                                                                
         CLI   DATEFORM,21         MMMDD/YYYY                                   
         BNE   *+10                                                             
         MVC   WORK+3(5),WORK+5    MOVE IN YEAR                                 
                                                                                
         CLI   DATEFORM,11         MMMDD/YY UK ONLY                             
         BNE   *+10                                                             
         MVC   WORK+4(3),WORK+5    MOVE IN /YY OVER DD/                         
*&&UK                                                                           
         CLI   DATEFORM,5          DD MMM YY (UK)                               
         BE    *+8                                                              
         CLI   DATEFORM,13         DD.MMM.YY (UK)                               
         BNE   DATI250                                                          
         MVC   WORK(6),WORK+3                                                   
*&&                                                                             
                                                                                
DATI250  SHI   R6,1                                                             
         BNM   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         EXMVC R6,0(R4),WORK                                                    
*                                                                               
DATIXIT  AHI   R6,1                                                             
         STC   R6,0(,R9)           Pass back length of data                     
         XIT1                                                                   
*&&US                                                                           
DDEFAULT DC    AL1(00,08,6,DATCON08,DATCON09)                                   
*&&                                                                             
*&&UK                                                                           
DDEFAULT DC    AL1(00,08,5,DATCON08,DATCON09)                                   
*&&                                                                             
                                                                                
YM_ONLY  DS    CL1                 Year Month only (Yes/No)                     
FROMDATE DS    AL1                 From date                                    
DATECONV DS    AL1                 Actual Datcon value                          
         DROP  R3                                                               
         LTORG                                                                  
         TITLE 'BOX HOOK --> CONTROL PRINTING OF BOXES'                         
***********************************************************************         
*  BOX HOOK                                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING ACWORKD,RC                                                       
         USING ACRLD,RA                                                         
         USING BIGPRNTD,R4                                                      
         USING FMTRECD,R5                                                       
         USING BOXD,R6                                                          
BXHOOK   NMOD1 0,**BXHK**                                                       
         L     RC,RLBASEC                                                       
         L     R4,VBIGPRNT                                                      
         L     R6,ADBXAREA                                                      
         TM    DDLNKIND,DDLNKON                                                 
         BO    BXHOOKX                                                          
         MVI   BOXWT,1             INITIALIZE                                   
         MVC   BOXROWS(BOXROWSL),SPACES               CLEAR BOXES               
         MVC   BOXCOLS,XSPACES                        CLEAR BOXES               
         MVI   BOXYORN,YES                                                      
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
         LA    R1,STDPGWD                                                       
         ST    R1,BOXWIDTH                                                      
         CLI   MODE,REQFRST        SKIP SHADE SETTINGS DURING REQFRST           
         BE    BXHOOKX                                                          
         MVC   BOXSHADE,FMTPOPT2                                                
         NI    BOXSHADE,RPFSTRPE+RPFISHDE+RPFOSHDE                              
         MVI   BOXSHCH1,X'42'      SHADE CHARACTER                              
*                                                                               
BXHOOKX  XMOD1                                                                  
         SPACE 2                                                                
         LTORG                                                                  
         DROP  R4,R5,R6                                                         
         TITLE 'Print queue indexing for HTML viewing'                          
         USING ACWORKD,RC                                                       
         USING ACRLD,RA                                                         
         USING BIGPRNTD,R4                                                      
         USING FMTRECD,R5                                                       
PIXLOAD  NMOD1 0,**PIDX**                                                       
         L     RC,RLBASEC          ->    ACWORKD                                
         L     R4,VBIGPRNT                                                      
         STC   R1,PIXMODE                                                       
         USING BOXD,RF                                                          
         L     RF,ADBXAREA                                                      
         MVC   BYTE1,BOXYORN       SAVE BOXES Y/N INDICATOR                     
         XC    BOXYORN,BOXYORN     SHUT IT OFF FOR PQ INDEX                     
*                                                                               
         TM    PIXMODE,PIXINIT     Initialize report                            
         BO    PIXINI10                                                         
         TM    PIXMODE,PIXDATA     Print out row data                           
         BO    PIXDAT10                                                         
         B     PIXXIT                                                           
                                                                                
         USING ROWD,R2                                                          
PIXINI10 NI    PIXOPT1,TURNOFF-PIXGO                                            
         L     R2,FMTROW                                                        
         SR    R0,R0                                                            
         IC    R0,FMT#ROWS         Number of rows to scan                       
         SHI   R0,1                Less the one I added                         
         BZ    PIXXIT              No rows - turn off indexing                  
         LA    R2,ROWLNQ(,R2)      Bump past first row                          
                                                                                
         CLI   ROWTYPE,ROWMID      Row is a mid-line                            
         BE    PIXXIT              Yes - turn off indexing                      
         CLI   ROWTYPE,ROWCOL      Row made from column                         
         BE    PIXXIT              Yes - turn off indexing                      
         OI    PIXOPT1,PIXGO       OK - if we have heading                      
         DROP  R2                                                               
                                                                                
         XC    ROWHEAD#,ROWHEAD#   Number of row headlines                      
         MVI   PIXROWH#,0          Initalize for PIXDATA                        
         MVI   PIXROW#,2           Initalize for PIXDATA                        
         MVC   XP,XSPACES                                                       
         MVC   XP(6),=CL6'<DECL>'     Start of PQ index information             
         GOTO1 PRINT,DMCB,XP,=C'BL01'                                           
*                                                                               
         MVC   XP,XSPACES                                                       
         MVC   XP(9),=CL9'<REQNAME'   Format code is reqname                    
         MVC   XP+9(L'FMTCODE),FMTCODE                                          
         LA    R1,L'FMTCODE                                                     
         LA    RE,XP+8(R1)         Point to end of format code less 1           
*                                                                               
PIXINI12 CLI   0(RE),C' '                                                       
         BH    PIXINI15                                                         
         BCTR  RE,0                                                             
         BCT   R1,PIXINI12                                                      
         DC    H'00'               No format code, ut-oh                        
*                                                                               
PIXINI15 MVI   1(RE),C'>'          Mark end of this piece of data               
         GOTO1 PRINT,DMCB,XP,=C'BL01'                                           
*                                                                               
         MVC   XP,XSPACES                                                       
         TM    DWNOPT1,DWNGO            DOWN-LOADING?                           
         BZ    *+12                     No - OK                                 
         TM    DWNOPT1,DWNROWS          DOWNLOAD ROWS REQ ?                     
         BZ    PIXINI40                 No - Don't add <IH ...>                 
*                                                                               
         USING ROWD,R2                                                          
         MVC   XP(3),=CL3'<IH'                                                  
         LA    R3,XP+4             Start of row heading data                    
         L     R2,FMTROW                                                        
         SR    R0,R0                                                            
         IC    R0,FMT#ROWS         Number of rows to scan                       
         SHI   R0,1                Less the one I added                         
         BZ    PIXINI40            No rows                                      
         LA    R2,ROWLNQ(,R2)      Bump past first row                          
         NI    PIXOPT1,TURNOFF-PIXMID-PIXROW                                    
*                                                                               
PIXINI20 CLI   ROWTYPE,ROWMID      Row is a mid-line, finished                  
         BNE   PIXINI22            Not applicable                               
         TM    PIXOPT1,PIXMID      Did we process a mid-line ?                  
         BO    PIXINI24                                                         
         TM    PIXOPT1,PIXROW      Did we process a regular row ?               
         BZ    PIXINI21            No                                           
         GOTO1 PRINT,DMCB,XP,=C'BL01'                                           
         MVC   XP,XSPACES                                                       
*                                                                               
PIXINI21 OI    PIXOPT1,PIXMID                                                   
         LA    R3,XP+4             Start of row heading data                    
         MVC   XP(3),=CL3'<IM'                                                  
         B     PIXINI24                                                         
*                                                                               
PIXINI22 CLI   ROWTYPE,ROWCOL      Row made from column                         
         BE    PIXINI30            Not applicable, print out data               
         OI    PIXOPT1,PIXROW                                                   
*                                                                               
PIXINI24 MVC   P,SPACES                                                         
         GOTO1 PUTIDX,DMCB,P,ROWINDEX,ROWPRFX,0,0                               
         L     R6,0(,R1)           New location within XP                       
         LA    RE,P                                                             
         SR    R6,RE               Length of data                               
         AHI   R6,1                Point past ">"                               
         LR    R1,R3                                                            
         LA    RE,XP                                                            
         SR    R1,RE                                                            
         AR    R1,R6                                                            
         CHI   R1,STDPGWD                Too big ?                              
         BNH   PIXIN24B                  No                                     
         GOTO1 PRINT,DMCB,XP,=C'BL01'                                           
         MVC   XP+3(L'XP-3),XSPACES      Everything but "<IM" or "<IH"          
         LA    R3,XP+4                   Start of row heading data              
*                                                                               
PIXIN24B BCTR  R6,0                                                             
         EXMVC R6,0(R3),P                                                       
         AR    R3,R6               New location                                 
*                                                                               
         LH    R1,ROWHEAD#                                                      
         LA    R1,1(,R1)                                                        
         STH   R1,ROWHEAD#                                                      
         CLI   ROWTYPE,ROWMID      Mid line row ?                               
         BE    PIXINI25                                                         
         CLI   ROWTYPE,ROWCOL      Column   row ?                               
         BE    PIXINI25                                                         
         STC   R1,PIXROWH#         Total number of row headings                 
*                                                                               
PIXINI25 LA    R2,ROWLNQ(,R2)                                                   
         BCT   R0,PIXINI20                                                      
*                                                                               
PIXINI30 TM    PIXOPT1,PIXMID+PIXROW     Did we have either ?                   
         BZ    PIXINI40                  No                                     
         GOTO1 PRINT,DMCB,XP,=C'BL01'                                           
         MVC   XP,XSPACES                                                       
*&&DO                                                                           
PIXINI10 XC    ROWHEAD#,ROWHEAD#   Number of row headlines                      
         MVI   PIXROWH#,0          Initalize for PIXDATA                        
         MVI   PIXROW#,2           Initalize for PIXDATA                        
         MVC   XP,XSPACES                                                       
         MVC   XP(6),=CL6'<DECL>'     Start of PQ index information             
         GOTO1 PRINT,DMCB,XP,=C'BL01'                                           
*                                                                               
         MVC   XP,XSPACES                                                       
         MVC   XP(9),=CL9'<REQNAME'   Format code is reqname                    
         MVC   XP+9(L'FMTCODE),FMTCODE                                          
         LA    R1,L'FMTCODE                                                     
         LA    RE,XP+8(R1)         Point to end of format code less 1           
*                                                                               
PIXINI12 CLI   0(RE),C' '                                                       
         BH    PIXINI15                                                         
         BCTR  RE,0                                                             
         BCT   R1,PIXINI12                                                      
         DC    H'00'               No format code, ut-oh                        
*                                                                               
PIXINI15 MVI   1(RE),C'>'          Mark end of this piece of data               
         GOTO1 PRINT,DMCB,XP,=C'BL01'                                           
*                                                                               
         USING ROWD,R2                                                          
         L     R2,FMTROW                                                        
         SR    R0,R0                                                            
         IC    R0,FMT#ROWS         Number of rows to scan                       
         SHI   R0,1                Less the one I added                         
         BZ    PIXINI65            No rows - turn off indexing                  
         LA    R2,ROWLNQ(,R2)      Bump past first row                          
*                                                                               
         CLI   ROWTYPE,ROWMID      Row is a mid-line                            
         BE    PIXINI65            Yes - turn off indexing                      
         CLI   ROWTYPE,ROWCOL      Row made from column                         
         BE    PIXINI65            Yes - turn off indexing                      
                                                                                
         MVC   XP,XSPACES                                                       
         MVC   XP(3),=CL3'<IH'                                                  
         LA    R3,XP+4             Start of row heading data                    
         L     R2,FMTROW                                                        
         SR    R0,R0                                                            
         IC    R0,FMT#ROWS         Number of rows to scan                       
         SHI   R0,1                Less the one I added                         
         BZ    PIXINI40            No rows                                      
         LA    R2,ROWLNQ(,R2)      Bump past first row                          
         NI    PIXOPT1,TURNOFF-PIXMID-PIXROW                                    
*                                                                               
PIXINI20 CLI   ROWTYPE,ROWMID      Row is a mid-line, finished                  
         BNE   PIXINI26            Not applicable                               
         TM    PIXOPT1,PIXMID      Did we process a mid-line ?                  
         BO    PIXINI28                                                         
         TM    PIXOPT1,PIXROW      Did we process a regular row ?               
         BO    *+8                 No                                           
         MVI   XP+4,C'>'                                                        
                                                                                
         GOTO1 PRINT,DMCB,XP,=C'BL01'                                           
         MVC   XP,XSPACES                                                       
         OI    PIXOPT1,PIXMID                                                   
         LA    R3,XP+4             Start of row heading data                    
         MVC   XP(3),=CL3'<IM'                                                  
         B     PIXINI28                                                         
*                                                                               
PIXINI26 CLI   ROWTYPE,ROWCOL      Row made from column                         
         BE    PIXINI35            Not applicable, print out data               
         OI    PIXOPT1,PIXROW                                                   
*                                                                               
PIXINI28 MVC   P,SPACES                                                         
         GOTO1 PUTIDX,DMCB,P,ROWINDEX,ROWPRFX,0,0                               
         L     R6,0(,R1)           New location within XP                       
         LA    RE,P                                                             
         SR    R6,RE               Length of data                               
         AHI   R6,1                Point past ">"                               
         LR    R1,R3                                                            
         LA    RE,XP                                                            
         SR    R1,RE                                                            
         AR    R1,R6                                                            
         CHI   R1,STDPGWD                Too big ?                              
         BNH   PIXINI30                  No                                     
         GOTO1 PRINT,DMCB,XP,=C'BL01'                                           
         MVC   XP+3(L'XP-3),XSPACES      Everything but "<IM" or "<IH"          
         LA    R3,XP+4                   Start of row heading data              
*                                                                               
PIXINI30 BCTR  R6,0                                                             
         EXMVC R6,0(R3),P                                                       
         AR    R3,R6               New location                                 
*                                                                               
         LH    R1,ROWHEAD#                                                      
         LA    R1,1(,R1)                                                        
         STH   R1,ROWHEAD#                                                      
         CLI   ROWTYPE,ROWMID      Mid line row ?                               
         BE    PIXINI32                                                         
         CLI   ROWTYPE,ROWCOL      Column   row ?                               
         BE    PIXINI32                                                         
         STC   R1,PIXROWH#         Total number of row headings                 
*                                                                               
PIXINI32 LA    R2,ROWLNQ(,R2)                                                   
         BCT   R0,PIXINI20                                                      
*                                                                               
PIXINI35 TM    PIXOPT1,PIXMID            Did we process a midline ?             
         BO    PIXINI38                  Yes                                    
         TM    PIXOPT1,PIXROW            Did we process a regular row ?         
         BO    PIXINI38                  Yes                                    
         MVI   XP+4,C'>'                 No                                     
                                                                                
PIXINI38 GOTO1 PRINT,DMCB,XP,=C'BL01'                                           
         MVC   XP,XSPACES                                                       
*&&                                                                             
         USING COLD,R2                                                          
PIXINI40 L     R2,FMTCOL           Process column information                   
         LA    R3,XP+4             Start of row heading data                    
         MVC   XP(3),=CL3'<IC'                                                  
         SR    R0,R0                                                            
         IC    R0,FMT#COLS                                                      
         ST    R2,SVR2                                                          
*                                                                               
PIXINI42 SR    R6,R6                                                            
         L     R2,SVR2                                                          
         ZIC   RF,COLPRTSQ         Column correct printing order                
         BCTR  RF,0                                                             
         MHI   RF,COLLNQ           Entry size                                   
         A     RF,FMTCOL           Point to column entry                        
         LA    R2,COLLNQ(,R2)                                                   
         ST    R2,SVR2             Save to process next                         
         LR    R2,RF               Use this column instead                      
*                                                                               
         TM    COLFLAGS,COLHIDE           Hidden column ?                       
         BO    PIXINI50                                                         
         TM    COLFLAGS,COLJOBR+COLCALC   Jobber or column calc. ?              
         BZ    PIXINI48                                                         
         LA    R6,COLKYWD                 Over-ride keyword text                
*                                                                               
PIXINI48 MVC   P,SPACES                                                         
         GOTO1 PUTIDX,DMCB,P,COLINDEX,COLHEAD1,COLHEAD2,(R6)                    
         L     R6,0(,R1)           New location within XP                       
         LA    RE,P                                                             
         SR    R6,RE               Length of data                               
         AHI   R6,1                Point past ">"                               
         LR    R1,R3                                                            
         LA    RE,XP                                                            
         SR    R1,RE                                                            
         AR    R1,R6                                                            
*&&US*&& CHI   R1,STDPGWD              Too big ?                                
*&&UK*&& CHI   R1,STDPGWD-1                                                     
         BNH   PIXINI49                No                                       
*&&UK                                                                           
         LA    RE,XP                   UK PC Accounting crashes if '<<'         
         SR    R1,R6                   is at the begining of print line         
         AR    RE,R1                   , so put it at the end.                  
         MVC   0(2,RE),=C'<<'          Continuation                             
*&&                                                                             
         GOTO1 PRINT,DMCB,XP,=C'BL01'                                           
*&&US*&& MVC   XP(3),=C'<< '           Replace <IC with <<                      
*&&UK*&& MVC   XP(3),=C'<  '                                                    
         MVC   XP+3(L'XP-3),XSPACES                                             
         LA    R3,XP+4                 Start of row heading data                
*                                                                               
PIXINI49 BCTR  R6,0                                                             
         EXMVC R6,0(R3),P                                                       
         AR    R3,R6               New location                                 
*                                                                               
PIXINI50 BCT   R0,PIXINI42                                                      
         GOTO1 PRINT,DMCB,XP,=C'BL01'                                           
         MVC   XP,XSPACES                                                       
*                                                                               
         TM    DWNOPT1,DWNGO       Are we downloading                           
         BZ    PIXINI55                                                         
         MVC   XP(10),=CL10'<FMT DATA>'                                         
         B     PIXINI60                                                         
*                                                                               
PIXINI55 ICM   R1,3,ROWHEAD#                                                    
         BZ    PIXINI65                                                         
         MVC   XP(7),=CL7'<HL 00>'      Number of row headings                  
         LH    R1,ROWHEAD#                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  XP+4(2),DUB                                                      
*                                                                               
PIXINI60 GOTO1 PRINT,DMCB,XP,=C'BL01'                                           
*                                                                               
PIXINI65 MVC   XP,XSPACES                                                       
         MVC   XP(7),=CL7'</DECL>'      End of report information               
         GOTO1 PRINT,DMCB,XP,=C'BL01'                                           
         MVC   XP,XSPACES                                                       
         CLI   PIXROWH#,0          Any row breaks ?                             
         BNE   PIXXIT                                                           
         NI    PIXOPT1,TURNOFF-PIXGO                                            
         B     PIXXIT                                                           
         EJECT                                                                  
PIXDAT10 DS    0H                                                               
*&&US                                                                           
         TM    DWTOTST,DWTTFOR     Are we waiting for "Total For"               
         BO    PIXXIT              Yes, leave and do that first                 
*&&                                                                             
         MVC   XP(5),=CL5'<DATA'                                                
         SR    RF,RF                                                            
         ICM   RF,1,PIXROW#        Row to start from                            
         BZ    PIXXIT              Not a sort key break                         
         BCTR  RF,0                Less one for 1st row we ignore               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  XP+6(2),DUB                                                      
         MVI   XP+8,C'='                                                        
         LA    R3,XP+9             Start DATA here                              
         SR    R0,R0                                                            
         IC    R0,PIXROWH#         # of row headings (page break type)          
         LR    R2,RF                                                            
         MHI   R2,ROWLNQ                                                        
         A     R2,FMTROW           Point to row that changed                    
         SR    R0,RF                                                            
         AHI   R0,1                                                             
         MVI   PRT#LN+3,1             Default chopper to 1 line                 
*                                                                               
PIXDAT20 GOTO1 =A(PRTFORM),PRTPQIDX   Print queue indexing                      
         LA    R1,55                                                            
         LA    R3,54(,R3)             Point to end of data                      
*                                                                               
PIXDAT22 CLI   0(R3),C' '                                                       
         BH    PIXDAT24                                                         
         BCTR  R3,0                                                             
         BCT   R1,PIXDAT22                                                      
*                                                                               
PIXDAT24 MVI   1(R3),X'5E'                                                      
         LA    R3,2(,R3)                                                        
         LA    R2,ROWLNQ(,R2)      Next row                                     
         BCT   R0,PIXDAT20                                                      
*                                                                               
         MVI   0(R3),C'>'                                                       
         BAS   RE,PUTXP                                                         
         MVC   XP,SPACES                                                        
         MVI   PIXROW#,0           Reset to no sort key change                  
*                                                                               
PIXXIT   L     RF,ADBXAREA                                                      
         MVC   BOXYORN,BYTE1       RESTORE  BOXES Y/N INDICATOR                 
         XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
*        Build   <keyword=heading>                                    *         
*           P1 = A(Location into XP through XPFORTH)                  *         
*           P2 = A(KEYWORD)                                           *         
*           P3 = AL1(length of string), AL3(String) or A(0)  1st      *         
*           P4 = AL1(length of string), AL3(String) or A(0)  2nd      *         
***********************************************************************         
         SPACE 1                                                                
         USING KYWD,R6                                                          
PUTIDX   NTR1                                                                   
         LR    R7,R1               Save address of parameter list               
         L     R3,0(,R1)           Get address within XP                        
         L     R6,4(,R1)           Keyword                                      
         MVC   0(L'KYWCODE,R3),KYWCODE                                          
         ICM   RE,15,16(R1)                                                     
         BZ    *+10                                                             
         MVC   0(L'KYWCODE,R3),0(RE)   Over-ride keyword data                   
         LA    R0,L'KYWCODE                                                     
         LA    RE,L'KYWCODE-1(,R3) Point to end of data less 1                  
*                                                                               
PUTIDX10 CLI   0(RE),C' '          Find end of data                             
         BH    PUTIDX12                                                         
         BCTR  RE,0                                                             
         BCT   R0,PUTIDX10                                                      
         DC    H'00'               No keyword ?                                 
*                                                                               
PUTIDX12 LA    R3,1(,RE)           Point to new location in XP                  
         MVC   0(2,R3),=C'="'                                                   
         LA    R3,2(,R3)           New location in XP                           
         ICM   RE,15,8(R1)         Get possible heading data                    
         BZ    PUTIDX15                                                         
         SR    RF,RF                                                            
         ICM   RF,1,8(R1)          Get length of string                         
         BZ    PUTIDX15            Zero, so try dictionary instead              
         BCTR  RF,0                                                             
         EXMVC RF,0(R3),0(RE)      Move in data                                 
         LA    R3,1(RF,R3)         Bump past data                               
         ICM   RE,15,12(R1)        2nd possbile heading                         
         BZ    PUTIDX16                                                         
*                                                                               
PUTIDX13 CLI   0(R3),C' '          Remove blank at end                          
         BH    PUTIDX14                                                         
         BCTR  R3,0                                                             
         BCT   RF,PUTIDX13                                                      
*                                                                               
PUTIDX14 LA    R3,1(,R3)                                                        
         MVC   0(3,R3),=C'","'                                                  
         LA    R3,3(,R3)                                                        
         SR    RF,RF                                                            
         ICM   RF,1,12(R1)         Get length of string                         
         BCTR  RF,0                                                             
         EXMVC RF,0(R3),0(RE)      Move in data                                 
         LA    R3,1(RF,R3)         Bump past data                               
         B     PUTIDX16                                                         
*                                                                               
PUTIDX15 OC    16(4,R1),16(R1)                                                  
         BNZ   PUTIDX20                                                         
         CLI   KYWREAD,KYWLVL1     Check account level type ?                   
         BL    PUTIX15C                                                         
         CLI   KYWREAD,KYWLVL4     Check account level type ?                   
         BH    PUTIX15C                                                         
         SR    RE,RE                                                            
         ICM   RE,3,KYWCOLH        See if a name heading exists                 
         BZ    PUTIX15C            No                                           
         A     RE,NMEBASE                                                       
         MVC   0(16,R3),0(RE)      Move in text                                 
         CLI   0(RE),ESC#HIGH      Is it in dictionary form ?                   
         BNL   PUTIX15B                                                         
         GOTO1 ADDICTAT,DMCB,C'SU  ',(R3),0                                     
*                                                                               
PUTIX15B LA    RF,16                                                            
         LA    R3,15(,R3)          Point to end less one                        
         B     PUTIDX16                                                         
*                                                                               
PUTIX15C MVI   0(R3),ESC#LFJT      Left justify                                 
         MVC   1(2,R3),KYWDD#      Dictionary number                            
         MVI   3(R3),24            Max length                                   
         GOTO1 ADDICTAT,DMCB,C'SU  ',(R3),0                                     
         LA    RF,24                                                            
         LA    R3,23(,R3)          Point to end less one                        
*                                                                               
PUTIDX16 CLI   0(R3),C' '          Find first sign of data                      
         BH    PUTIDX18                                                         
         BCTR  R3,0                                                             
         BCT   RF,PUTIDX16                                                      
*                                  If drop through, must of found C'"'          
PUTIDX18 LA    R3,1(,R3)                                                        
*                                                                               
PUTIDX20 MVI   0(R3),C'"'                                                       
         MVI   1(R3),X'5E'         Semi-colon                                   
         MVI   2(R3),C'>'                                                       
         LA    R3,2(,R3)           Point to end marker                          
         ST    R3,0(,R7)           Pass back location into XP                   
*                                                                               
PUTXIT   XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        Print out what is built in XP through XPFORTH then clear     *         
***********************************************************************         
         SPACE 1                                                                
PUTXP    NTR1                                                                   
         LA    R2,XP                                                            
         CLC   4(L'XP-4,R2),XSPACES    Any data to print ?                      
         BNH   PUTXIT                  Nothing to print                         
         LA    R0,4                                                             
*                                                                               
PUTXP10  CLC   0(L'XP,R2),XSPACES                                               
         BNH   PUTXP20                 Finished                                 
         GOTO1 PRINT,DMCB,(R2),=C'BL01'                                         
         LA    R2,L'XP(,R2)            Next line                                
         BCT   R0,PUTXP10                                                       
*                                                                               
PUTXP20  LA    RF,4*L'XP           XP through XPFORTH                           
         LA    R0,SPACES           Clear out print lines                        
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         LA    RE,XP                                                            
         MVCL  RE,R0                                                            
         B     PUTXIT              Finished                                     
         SPACE 2                                                                
         LTORG                                                                  
         DROP  R4,R5                                                            
         TITLE 'DOWN-LOAD REPORT ROUTINES'                                      
***********************************************************************         
*  DOWN-LOAD ROW ADDRESS FIELDS                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING ROWD,R1             ROW   DSECT                                  
         USING KYWD,R6                                                          
         USING ACRL2D,R7                                                        
DWNROWAD NMOD1 0,**DWRA**                                                       
         L     RC,RLBASEC          ->    ACWORKD                                
         L     R7,AACRL2D          ->    ACRL2D                                 
*                                                                               
*&&US*&& ST    R1,FULL             KEEP ADDRESS OF CURRENT ROW                  
         MVC   ACURWRK,ASRTWRK                                                  
         TM    PRTCNTRL,PRTSRT2    USE A(PRVWRK) INSTEAD OF A(SRTWRK)           
         BZ    *+10                                                             
         MVC   ACURWRK,APRVWRK                                                  
         LH    R4,ROWXPDSP         GET   ROW  PRT  DISPLACEMENT                 
         L     R6,ROWINDEX         INDEX INTO TABLE     ENTRY                   
         SR    RE,RE               FIND  KEYWORD   ADDRESS   ENTRY              
         ICM   RE,3,KYWADDR                                                     
         A     RE,KYWBASE                                                       
         LR    R6,RE               R6 =  A(KEYWORD ADDRESS   ENTRY)             
         A     R4,ACURWRK          ADD   SORT REC  ADDRESS                      
         SR    R5,R5               CLEAR REGISTER                               
         DROP  R1                                                               
         LA    RE,XLCHOP           CLEAR XLCHOP TO ZEROES OR SPACES             
         LHI   RF,GESIZEQ                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,8,SPACES         SET PADDING CHARACTER                        
         MVCL  RE,R0                                                            
*                                                                               
         USING ACCRECD,R2                                                       
         L     R2,AIO2                                                          
         MVC   IOKEY2(1),RCCOMPFL  Set company code                             
         LR    RE,R4               Address into sort record                     
         XR    RF,RF                                                            
         ICM   RF,3,KYWSRTQ                                                     
         CHI   RF,LULAQ            Is company code included ?                   
         BE    *+8                 No, use all of data from sort rec.           
         LA    RE,1(,RE)           Yes, bump past company code                  
         MVC   IOKEY2+1(LULAQ),0(RE)                                            
         CLC   IOKEY2,ACCKEY       DO    WE   ALREADY   HAVE RECORD ?           
         BE    DWNRA10             YES,  CONTINUE                               
*                                                                               
         MVC   ACCKEY,SPACES       GET   RECORD                                 
         MVC   ACCKEY(L'IOKEY2),IOKEY2                                          
         MVC   IOKEY,ACCKEY                                                     
         L     R1,=AL4(IOREAD+IOACFIL+IOAREA2)                                  
         GOTO1 AIO                                                              
         BNE   DWNRA50             OUTPUT     BLANKS    IF   NOT  FND           
*                                                                               
         USING ADRELD,R2                                                        
DWNRA10  DS    0H                                                               
         AH    R2,DATADISP         GET   TO   1ST  ELEMENT                      
*&&US                                                                           
         USING ROWD,R1                                                          
         L     R1,FULL             CURRENT ROW ADDRESS                          
         TM    ROWDAIND,ROWBDR     USING BDR ADDRESS ATTRIBUTE?                 
         BO    DWNRA32                                                          
         DROP  R1                                                               
*&&                                                                             
DWNRA15  DS    0H                  FIND  ADDRESS   ELEMENT                      
         CLI   0(R2),0                                                          
         BE    DWNRA50             OUTPUT     BLANKS    IF   NOT  FND           
         CLI   0(R2),ADRELQ                                                     
         BE    DWNRA20             FOUND ADDRESS   ELEMENT                      
         SR    R1,R1                                                            
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     DWNRA15             TRY   NEXT ELEMENT                           
*                                                                               
DWNRA20  DS    0H                  Find number of lines of address used         
         ZIC   R1,ADRNUM           Stored value of number of lines              
         LTR   R1,R1                                                            
         BNZ   DWNRA25             If zero calculate number of lines            
         SR    R0,R0                                                            
         IC    R1,ADRLN              Get elememt length                         
         AHI   R1,-(ADRADD1-ADRELD)  Less non-address part of data              
         D     R0,=A(L'ADRADD1)      Divide by length of one line               
         LTR   R1,R1                 R1 = Number of address lines               
         BZ    DWNRA50               No address lines, so put blanks            
*                                                                               
DWNRA25  DS    0H                                                               
*                                  MORE  THAN MAX  NUM  OF                      
         CLM   R1,1,RAD#LDWN             LINE ADDRESSES TO   DOWN-LOAD?         
         BNH   *+8                 NO,   USE  THIS NUMBER                       
         IC    R1,RAD#LDWN         USE   USER SPECIFIED NUMBER                  
*                                                                               
         LR    R3,R1               SAVE  NUMBER    OF   LINES                   
         LR    R5,R1               SAVE  NUMBER    OF   LINES                   
         ZIC   R6,RADPRTSZ         ROW   ADDR PRT  SIZE                         
         BCTR  R6,0                MINUS ONE  FOR  EXECUTE                      
         LA    R2,ADRADD1                                                       
*                                                                               
DWNRA30  DS    0H                  DOWN-LOAD  ADDR LINE FROM X'22' EL           
         MVC   PRTSIZE,RADPRTSZ    SET   FLD  LENGTH                            
         EXMVC R6,XLCHOP,0(R2)     INSERT     ONE  ADDRESS   LINE               
         GOTO1 =A(DWNLOAD),DWNTEXT OUTPUT     THIS ADDRESS   LINE               
         LA    R2,1(R6,R2)         BUMP  TO   NEXT ADDRESS   LINE               
         BCT   R3,DWNRA30          OUTPUT     NEXT ADDRESS   LINE               
         B     DWNRA50                                                          
*&&US                                                                           
         USING OATELD,R2                                                        
DWNRA32  CLI   0(R2),0                                                          
         BE    DWNRA50             OUTPUT     BLANKS    IF   NOT  FND           
         CLI   0(R2),OATELQ                                                     
         BE    DWNRA34             FOUND ADDRESS   ELEMENT                      
         SR    R1,R1                                                            
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     DWNRA32             TRY   NEXT ELEMENT                           
*                                                                               
DWNRA34  LHI   R6,L'OATLINE1          SET   FLD  LENGTH                         
         STCM  R6,1,PRTSIZE                                                     
         BCTR  R6,0                                                             
         CLC   OATLINE1,SPACES     Any address on this line?                    
         BNH   DWNRA36                                                          
         LA    R5,1                                                             
         EXMVC R6,XLCHOP,OATLINE1  INSERT     ONE  ADDRESS   LINE               
         GOTO1 =A(DWNLOAD),DWNTEXT OUTPUT     THIS ADDRESS   LINE               
*                                                                               
DWNRA36  CLM   R5,1,RAD#LDWN       less than the # set on download scr?         
         BNL   DWNRA50             No so finished                               
         CLC   OATLINE2,SPACES     Any address on this line?                    
         BH    DWNRA38                                                          
         EXMVC R6,XLCHOP,SPACES                                                 
         B     DWNRA40                                                          
DWNRA38  EXMVC R6,XLCHOP,OATLINE2  INSERT     ONE  ADDRESS   LINE               
DWNRA40  AHI   R5,1                                                             
         GOTO1 =A(DWNLOAD),DWNTEXT OUTPUT     THIS ADDRESS   LINE               
*                                                                               
DWNRA42  CLM   R5,1,RAD#LDWN       less than the # set on download scr?         
         BNL   DWNRA50             No so finished                               
         CLC   OATCITY,SPACES      Any city?                                    
         BNH   DWNRA44                                                          
         LHI   R6,L'OATCITY           SET   FLD  LENGTH                         
         STCM  R6,1,PRTSIZE                                                     
         BCTR  R6,0                                                             
         AHI   R5,1                                                             
         EXMVC R6,XLCHOP,OATCITY   INSERT     ONE  ADDRESS   LINE               
         GOTO1 =A(DWNLOAD),DWNTEXT OUTPUT     THIS ADDRESS   LINE               
*                                                                               
DWNRA44  LA    RE,XLCHOP           CLEAR XLCHOP AREA                            
         LHI   RF,GESIZEQ                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
         CLC   OATSTATE,SPACES     Any state?                                   
         BNH   DWNRA50                                                          
         LHI   R6,L'OATSTATE          SET   FLD  LENGTH                         
         STCM  R6,1,PRTSIZE                                                     
         BCTR  R6,0                                                             
         CLM   R5,1,RAD#LDWN       less than the # set on download scr?         
         BNL   DWNRA50             No so finished                               
         AHI   R5,1                                                             
         EXMVC R6,XLCHOP,OATSTATE  INSERT     ONE  ADDRESS   LINE               
*                                                                               
DWNRA46  CLC   OATZIP,SPACES       Any zip?                                     
         BNH   DWNRA48                                                          
         MVC   XLCHOP+3(5),OATZIP                                               
         MVI   XLCHOP+8,C'-'                                                    
         MVC   XLCHOP+9(4),OATZIPRN                                             
DWNRA48  GOTO1 =A(DWNLOAD),DWNTEXT OUTPUT     THIS ADDRESS   LINE               
*&&                                                                             
DWNRA50  LA    RE,XLCHOP           CLEAR XLCHOP TO ZEROES OR SPACES             
         LHI   RF,GESIZEQ                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,8,SPACES         SET PADDING CHARACTER                        
         MVCL  RE,R0                                                            
*                                        ADDR LINES     WERE NOT OUTPUT         
         ZIC   R1,RAD#LDWN         NUM   OF   ROW  ADDR LINES                   
         SR    R1,R5               NUM   OF   LINES     OUTPUT                  
         BZ    DWNRA90             NONE  LEFT TO   DOWN-LOAD, EXIT              
*                                                                               
         MVI   PRTSIZE,1           SET   FLD  LENGTH    TO   1                  
         TM    DWNOPT1,DWNCHOP     DOWN-LOAD  FIXED     LENGTH DATA ?           
         BZ    *+10                NO,   SKIP                                   
         MVC   PRTSIZE,RADPRTSZ    YES,  USE  ROW  ADDR PRINT     SIZE          
         LR    R3,R1               ADDRESS    LINES     LEFT                    
*                                                                               
DWNRA60  DS    0H                                                               
         GOTO1 =A(DWNLOAD),DWNTEXT OUTPUT     THIS LINE AS   BLANKS             
         BCT   R3,DWNRA60          ANY   MORE LINES     TO   DOWN-LOAD?         
*                                                                               
DWNRA90  DS    0H                                                               
         XMOD1 ,                   EXIT                                         
         DROP  R2,R6,R7                                                         
         SPACE 2                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  GENERAL DOWN-LOAD ROUTINE                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R4                                                      
         USING FMTRECD,R5                                                       
         USING ACRL2D,R7                                                        
         USING DLCBD,R9                                                         
         SPACE 1                                                                
DWNLOAD  NMOD1 0,**DOWN**                                                       
         L     RC,RLBASEC                                                       
         L     R7,AACRL2D                                                       
         STC   R1,DWNMODE          Save mode                                    
         L     R4,VBIGPRNT                                                      
         L     R9,DWNLDCB                                                       
*&&US*&& LA    RF,DWNLINE                                                       
*&&US*&& ST    RF,ADWNLINE                                                      
         CLI   DWNMODE,DWNINIT     Initialize                                   
         BE    DWNLD10                                                          
         CLI   DWNMODE,DWNPACK     Download number                              
         BE    DWNLD15                                                          
         CLI   DWNMODE,DWNTEXT     Download text                                
         BE    DWNLD15                                                          
         CLI   DWNMODE,DWNTOTR     Download total trailer                       
         BE    DWNLD05                                                          
         CLI   DWNMODE,DWNEOL      End of line                                  
         BE    DWNLD85                                                          
         CLI   DWNMODE,DWNEOR      End of report                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    DWNOPT3,DWNXTRL      Extended trailer                            
         BZ    DWNLD04                                                          
         ZAP   DTTCOUNT,DTLCOUNT                                                
         AP    DTTCOUNT,=P'1'      Add one to count for trailer                 
         TM    DWNOPT3,DWNXHDR     Is there a header too?                       
         BZ    *+10                                                             
         AP    DTTCOUNT,=P'1'      Add one to count for header                  
*        AP    DTTCOUNT,=P'2'      Add header + trailer                         
         BAS   RE,DOWNTRL                                                       
                                                                                
DWNLD04  MVI   DLCBACT,DLCBEOR                                                  
*&&UK*&& TM    DWNOPT3,DWNTUSS+DWNTEDI Test USS/EDIHUB transmission req         
*&&US*&& TM    DWNOPT3,DWNTEDI      Test EDIHUB transmission req                
         BNZ   DWNLD90                 Already close USS server                 
         CLI   DLCXEORC,C' '        End or report character                     
         BNE   DWNLD90              Yes                                         
         B     DWNXIT               No, so exit                                 
*                                                                               
DWNLD05  BAS   RE,DOWNTTR                                                       
         B     DWNXIT               No, so exit                                 
*                                                                               
DWNLD10  BRAS  RE,FNDMFLDS         max# of dwnload flds for all formats         
         MVI   DLCBACT,DLCBINIT    DOWN LOAD ACTION IS START                    
         LA    RE,XP               PRINT LINE                                   
         ST    RE,DLCBAPL                                                       
         LA    RE,DWNHOOK          POINT TO HOOK FOR APPLICATION                
         ST    RE,DLCBAPR                                                       
         MVC   DLCXMAXL,=Y(STDPGWD)                                             
         MVC   DLCXDELC,DWNFLDD    DELIMITER                                    
         MVC   DLCXEOTC,DWNTXTD    TEXT DELIMITER                               
*                                                                               
*&&UK*&& TM    DWNOPT3,DWNTUSS+DWNTEDI Test USS/EDIHUB transmission req         
*&&US*&& TM    DWNOPT3,DWNTEDI     Test EDIHUB transmission req                 
         BZ    DWNLD10A            NO - OK                                      
         CLI   DLCXEOTC,C' '       TEST BLANK TEXT DELIMITER                    
         BNE   DWNLD10A            NO - OK                                      
         MVI   DLCXEOTC,0          DON'T SHOW ALTERNATE TEXT DELIMITER          
*                                                                               
DWNLD10A MVI   DLCXEOTA,C''''      ALTERNATE TEXT DELIMITER                     
         MVC   DLCXEOLC,DWNEOLD    SEMI-COLON, END-OF-LINE                      
         MVC   DLCXEORC,DWNEORD    END-OF-REPORT                                
         MVC   REQHMS(2),REQHH                                                  
         MVC   REQHMS+2(2),REQMM                                                
         MVC   REQHMS+4(2),REQSS                                                
*                                                                               
DWNLD11  GOTO1 ADLFLD,(R9)                                                      
         MVI   FORCEHED,YES        EXCEPT FIRST TIME IN                         
         OI    DLCBFLG1,DLCBFXFL   USE LEN OF 160 LINE                          
         MVC   DLCBFLX,XSPACES     MUST CLEAR FIRST TIME IN                     
         TM    DWNOPT1,DWNHEAD     DOWN-LOAD HEADINGS                           
         BZ    DWNLD12             NO, SKIP                                     
         BAS   RE,DOWNHEAD                                                      
*                                                                               
DWNLD12  TM    DWNOPT2,DWNFTP            Headers for FTP                        
         BZ    DWNLD13                                                          
         BAS   RE,DOWNFTP          Put out headers                              
*                                                                               
DWNLD13  TM    DWNOPT3,DWNXHDR           Extended Header                        
         BZ    DWNLD14                                                          
         BAS   RE,DOWNHDR          Put out headers                              
*                                                                               
DWNLD14  OI    DWTOTST,DWTNEWLI          NEXT FIELD STARTS A NEW LINE           
         NI    DWTOTST,TURNOFF-DWTROWBL  TURN OFF ROW FLDS AS C' '              
         B     DWNXIT                                                           
*                                                                               
DWNLD15  TM    DWTOTST,DWTNEWLI         Need new line processing ?              
         BZ    DWNLD20                  No, skip                                
         TM    DWTOTST,DWTTFOR+DWTTREQ "TOTAL FOR" or "TOTAL FOR REQ" ?         
         BNZ   DWNLD18                  Yes, Insert 1st col totals info         
         TM    DWNOPT2,DWNTOTS          Down-load totals requested              
         BZ    DWNLD19                  No, do not output extra column          
*                                                                               
         MVI   DLCBLEN,1           Output a blanck column                       
         TM    DWNOPT1,DWNCHOP     Down-load fixed length data ?                
         BZ    DWNLD17             No, skip                                     
         ZIC   R1,DWNROWPS         Yes, get down-load row print size            
         LA    R1,L'AC@TFOR(,R1)      add "TOTAL FOR" size                      
         STC   R1,DLCBLEN          Set length                                   
*                                                                               
DWNLD17  MVC   DLCBFLX(1),SPACES   Set text blank                               
         MVI   DLCBACT,DLCBPUT     Action is put                                
         MVI   DLCBTYP,DLCBTXT     Type is text                                 
         GOTO1 ADLFLD,(R9)               Down-load field                        
         NI    DWTOTST,TURNOFF-DWTNEWLI     No longer new line                  
         B     DWNLD20                      Continue                            
*                                                                               
DWNLD18  TM    DWNOPT3,DWNXTTR     Down-load extending total data ?             
         BO    DWNLD19             Yes                                          
         TM    DWNOPT2,DWNTOTS     Down-load totals requested ?                 
         BZ    DWNXIT              No                                           
*                                                                               
DWNLD19  NI    DWTOTST,TURNOFF-DWTNEWLI                                         
*                                                                               
DWNLD20  MVI   DLCBTYP,DLCBTXT     Type is text                                 
         CLI   DWNMODE,DWNPACK     DOWN-LOAD NUMBER                             
         BNE   DWNLD32             NO, DOWN-LOAD TEXT                           
         TM    DWNOPT1,DWNTXT#     DOWNLOAD AS NUMBER AS TEXT ?                 
         BO    DWNLD32             YES, DOWN-LOAD AS TEXT                       
         CLI   XLCHOP,C'E'         BAD  DATA ?                                  
         BE    DWNLD32             YES, DOWN-LOAD AS TEXT                       
         CLI   XLCHOP,C'*'         BAD  DATA ?                                  
         BE    DWNLD32             YES, DOWN-LOAD AS TEXT                       
         MVI   DLCBTYP,DLCBNUM     TYPE IS NUMBER                               
         TM    DWNOPT1,DWNLEADM    DOWN-LOAD WITH LEADING MINUS ?               
         BO    *+8                 YES, SKIP                                    
         OI    DLCBFLG1,DLCBFTRM   NO,  DOWN-LOAD WITH TRAILING MINUS           
*                                  DOWN-LOAD AS NUMBER                          
DWNLD32  MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         CLI   DLCBTYP,DLCBNUM     IS TYPE NUMERIC ?                            
         BE    DWNLD34             YES, BRANCH TO CHECK NUMERIC LENGTH          
*                                                                               
         TM    DWNOPT2,DWNDUMF     DOWNLOAD a dummy field?                      
         BZ    DWNLD33                                                          
         MVI   PRTSIZE,1                                                        
         LA    RE,XLCHOP                                                        
         LHI   RF,GESIZEQ                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
         NI    DWNOPT2,X'FF'-DWNDUMF                                            
*                                                                               
DWNLD33  TM    DWNOPT1,DWNCHOP     PRINT FIXED LENGTH DATA ?                    
         BZ    DWNLD37             NO, SKIP SPECIFYING THE LENGTH               
         MVI   DLCBLEN,L'DLCBFLX   MAX LENGTH                                   
         CLI   PRTSIZE,L'DLCBFLX                                                
         BNL   DWNLD37                                                          
         MVC   DLCBLEN,PRTSIZE     USE SMALLER SIZE                             
         B     DWNLD37             CONTINUE                                     
*                                                                               
DWNLD34  TM    DWNOPT2,DWNDUMF     Download a dummy field?                      
         BZ    DWNLD35                                                          
         MVI   PRTSIZE,1                                                        
         MVI   XLCHOP,0                                                         
         NI    DWNOPT2,X'FF'-DWNDUMF                                            
*                                                                               
DWNLD35  TM    DWNOPT2,DWN#PAD     DOWN-LOAD FIXED LENGTH NUMBERS ?             
         BZ    DWNLD37             NO,  SKIP SPECIFYING THE LENGTH              
*        MVC   DLCBLEN,PRTSIZE     YES, USE PRTSIZE                             
*        CLI   DLCBLEN,L'DLCBFLD   IS   THE SIZE TOO BIG ?                      
*        BNH   DWNLD37             NO,  CONTINUE                                
*        MVI   DLCBLEN,L'DLCBFLD   YES, USE MAXIMUM LENGTH OF NUMERICS          
*                                                                               
DWNLD37  DS    0H                  SET UP TO INSERT THE DATA                    
         LA    RE,DLCBFLD          DOWN-LOAD NUMBER                             
         LA    RF,L'DLCBFLD                                                     
         CLI   DLCBTYP,DLCBNUM                                                  
         BE    DWNLD40                                                          
         LA    RE,DLCBFLX          DOWN-LOAD TEXT                               
         LA    RF,L'DLCBFLX                                                     
*                                                                               
DWNLD40  CLI   DLCBLEN,0           CHECK SIZE OF DATA TO DOWN LOAD              
         BE    *+8                                                              
         IC    RF,DLCBLEN                                                       
         AHI   RF,-1               MOVE IT IN FROM XLCHOP                       
         BM    DWNLD50             NO DATA SO SKIP                              
         TM    DWNOPT2,DWNCONT                                                  
         BNZ   DWNLD43                                                          
         LA    R3,XLCHOP                                                        
         CLI   DWNMODE,DWNTEXT     Text?                                        
         BE    DWNLD41C            Yes, check for zero chars                    
         CLI   DWNMODE,DWNPACK     Must be a number                             
         BNE   DWNLD42             Not                                          
         TM    DWNOPT2,DWNLEFTJ    Left justify ?                               
         BZ    DWNLD42             No                                           
*                                                                               
DWNLD41  CLI   0(R3),C' '          Find first non-space                         
         BH    DWNLD42                                                          
         LA    R3,1(,R3)                                                        
         BCT   RF,DWNLD41                                                       
         CLI   0(R3),C' '          Any data at all ?                            
         BNH   DWNLD50             No data to down-load                         
         B     DWNLD42             No data to down-load                         
*                                                                               
DWNLD41C AHI   RF,1                                                             
DWNLD41D CLI   0(R3),C' '          Find first non-space                         
         BH    DWNLD42                                                          
         MVI   0(R3),C' '          Convert these to space                       
         AHI   R3,1                                                             
         BCT   RF,DWNLD41D                                                      
*                                                                               
DWNLD42  DS    0H                                                               
*                                                                               
         TM    DWNOPT2,DWNCONT                                                  
         BZ    DWNLD43C                                                         
DWNLD43  LA    R0,XLCHOP+L'XLCHOP                                               
         LA    R3,1(RF,R3)                                                      
         SR    R0,R3                                                            
         SHI   R0,1                                                             
         CR    R0,RF                                                            
         BH    DWNLD43B                                                         
         BE    *+6                                                              
         LR    RF,R0                                                            
*                                                                               
DWNLD43A EXMVC RF,0(RE),0(R3)                                                   
         NI    DLCXCIND,X'FF'-DLCXEOTE                                          
         OI    DLCXCIND,DLCXEOTB                                                
         B     DWNLD50                                                          
*                                                                               
DWNLD43B EXMVC RF,0(RE),0(R3)                                                   
         OI    DLCXCIND,DLCXEOTE                                                
         SR    R0,RF                                                            
         CR    R0,RF               SET R0 TO BE MIN(RF,R0)                      
         BNH   *+8                                                              
         LR    R0,RF                                                            
         LA    R1,1(RF,R3)                                                      
         CHI   R0,L'XSPACES-1                                                   
         BNH   *+8                                                              
         LHI   R0,L'XSPACES-1                                                   
         LR    RF,R0                                                            
         EXCLC RF,0(R1),XSPACES    Use RF, not R0, for EX                       
         BNE   *+12                                                             
         NI    DWNOPT2,X'FF'-DWNCONT                                            
         NI    DLCXCIND,X'FF'-DLCXEOTE                                          
         OI    DLCXCIND,DLCXEOTB                                                
         B     DWNLD50                                                          
*                                                                               
DWNLD43C DS    0H                                                               
*&&UK*&& TM    DWNOPT3,DWNTUSS+DWNTEDI Test USS/EDIHUB transmission req         
*&&US*&& TM    DWNOPT3,DWNTEDI     Test EDIHUB transmission req                 
         BZ    DWNLD44                 No - OK                                  
         TM    PRTACT,PRTROWS      Test row ?                   ?               
         BO    DWNLD44             Yes - don't truncate column                  
         LLC   R1,PRTSIZE                                                       
         BCTR  R1,0                                                             
         CR    RF,R1               TEST FIELD TOO LONG                          
         BNH   DWNLD44                                                          
         LR    RF,R1               YES USE MAX LENGTH                           
*                                                                               
DWNLD44  EXMVC RF,0(RE),0(R3)                                                   
*                                                                               
*&&UK*&& TM    DWNOPT3,DWNTUSS+DWNTEDI Test USS/EDIHUB transmission req         
*&&US*&& TM    DWNOPT3,DWNTEDI     Test EDIHUB transmission req                 
         BNZ   DWNLD50                 Yes - done                               
*&&US*&& TM    DWNOPT1,DWNCHOP     Fixed length data?                           
*&&US*&& BO    DWNLD50                 Yes - done                               
         LA    R1,L'XLCHOP-1                                                    
         SR    R1,RF                                                            
         BNP   DWNLD50                                                          
         SHI   R1,1                                                             
         CHI   R1,L'SPACES-1                                                    
         BNH   *+8                                                              
         LHI   R1,L'SPACES-1                                                    
         LA    RF,1(RF,R3)                                                      
         EXCLC R1,0(RF),XSPACES                                                 
         BNH   DWNLD50                                                          
         OI    DWNOPT2,DWNCONT                                                  
         OI    DLCXCIND,DLCXEOTE                                                
*                                                                               
DWNLD50  CLI   DLCBLEN,0           Did we specify a length?                     
         JE    DWNLD55             No, so check integrity                       
         TM    DWNOPT2,DWNDSN      Transmit through EDICT via Dataset ?         
         BO    DWNLD90             Yes, so don't convert x'40' to x'42'         
         TM    DWNOPT1,DWNCHOP                                                  
         JZ    DWNLD90                                                          
         CLI   DLCBTYP,DLCBTXT     Downloading text ?                           
         JNE   DWNLD90                                                          
         CLI   DLCXEOTC,X'40'      Was text delimiter set to blank              
         JE    DWNLD52                                                          
         CLI   DLCXEOTC,0          Was text delimiter set to none               
         JNE   DWNLD90                                                          
                                                                                
DWNLD52  CLI   DLCXEOLC,X'40'      Was EOL delimiter set to blank               
         JE    DWNLD53                                                          
         CLI   DLCXEOLC,0          Was EOL delimiter set to none                
         JNE   DWNLD90                                                          
                                                                                
DWNLD53  ZIC   RF,DLCBLEN                                                       
         BCTR  RF,0                                                             
         LA    RE,DLCBFLX(RF)                                                   
         CLI   0(RE),C' '          Check last character for space               
         JNE   DWNLD90                                                          
         MVI   0(RE),X'42'         Prevent last field on EOL truncation         
         J     DWNLD90             Download field                               
                                                                                
DWNLD55  CLI   DLCBTYP,DLCBNUM     DOWN-LOADING AS NUMERIC ?                    
         BNE   DWNLD58             NO,  BRANCH TO CHECK TEXT FIELD              
         NC    DLCBFLD,DLCBFLD     YES, MAKE SURE NUMERIC FLD NOT ZEROS         
         BNZ   DWNLD90             NOT  ZERO, DOWN-LOAD FIELD                   
         MVI   DLCBLEN,1           ZERO, SET LENGTH 1 TO DOWN-LOAD A 0          
         B     DWNLD90             BRANCH TO DOWN-LOAD FIELD                    
*                                                                               
DWNLD58  DS    0H                  CHECK TEXT FIELD FOR ZEROS                   
         NC    DLCBFLX,DLCBFLX     YES, MAKE SURE TEXT FIELD NOT ZEROS          
         BNZ   DWNLD90             NOT  ZERO, DOWN-LOAD FIELD                   
         MVI   DLCBLEN,1           ZERO, SET LENGTH 1 TO DOWN-LOAD C' '         
         B     DWNLD90             BRANCH TO DOWN-LOAD FIELD                    
*                                                                               
DWNLD85  TM    RECAPSW,RECAPON     Recapping ?                                  
         BZ    DWNLD88             No                                           
         TM    DWNOPT2,DWNFSTL     Did we download the 1st line ?               
         BO    DWNLD88                                                          
         ZIC   R1,DWN#FLDS         # of fields downloaded                       
         ZIC   R0,FND#FLDS         Max # of cols found for any format           
         SR    R0,R1                                                            
         BNP   DWNLD88             Skip padding                                 
*                                                                               
DWNLD86  OI    DWNOPT2,DWNDUMF        Download a dummy field                    
         GOTO1 DWNLOAD,DWNTEXT        BLANK FIELD                               
         BCT   R0,DWNLD86                                                       
         NI    DWNOPT2,TURNOFF-DWNDUMF                                          
*                                                                               
DWNLD88  OI    DWTOTST,DWTNEWLI    Next field starts a new line                 
         MVI   DLCBACT,DLCBEOL                                                  
*        LH    RF,DLCBNUMC         Attempt to delete last delimeter             
*        SHI   RF,1                                                             
*        STH   RF,DLCBNUMC                                                      
*                                                                               
         GOTO1 ADLFLD,(R9)         DOWN-LOAD FIELD                              
         XC    DWN#FLDS,DWN#FLDS   # of fields downloaded                       
         B     DWNXIT                                                           
*                                                                               
DWNLD90  GOTO1 ADLFLD,(R9)         DOWN-LOAD FIELD                              
*                                                                               
         TM    DWNOPT2,DWNCONT                                                  
         BO    DWNLD20                                                          
                                                                                
DWNLD92  ZIC   R0,DWN#FLDS                                                      
         AHI   R0,1                                                             
         STC   R0,DWN#FLDS         # of fields downloaded                       
         NI    DLCXCIND,X'FF'-DLCXEOTB                                          
*                                                                               
DWNXIT   XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*  DOWN-LOAD HEADERS ROUTINE                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING ROWD,R3                                                          
DOWNHEAD NTR1                                                                   
         SR    R0,R0                                                            
         LA    R8,2                                                             
*                                                                               
DWNHD10  DS    0H                                                               
         TM    DWNOPT2,DWNTOTS     Are totals being down-loaded ?               
         BZ    DWNHD14             No, skip "TOTALS FOR"                        
         CHI   R8,1                       2nd pass, 2nd set of headings         
         BE    DWNHD11                    Yes, skip                             
         MVC   DLCBFLX(L'AC@TFOR),AC@TFOR Insert "TOTAL FOR"                    
         MVI   DLCBLEN,0                  No length defined                     
         B     DWNHD12                    Branch to output data                 
*                                                                               
DWNHD11  DS    0H                  On 2nd pass                                  
         MVC   DLCBFLX,XSPACES     Insert spaces                                
         MVI   DLCBLEN,1           Set length of output to one                  
*                                                                               
DWNHD12  DS    0H                                                               
         TM    DWNOPT1,DWNCHOP     DOWN-LOAD FIXED LENGTH DATA ?                
         BZ    DWNHD13             NO,    SKIP                                  
         ZIC   R1,DWNROWPS         YES,   GET DOWN-LOAD ROW PRINT SIZE          
         LA    R1,L'AC@TFOR(,R1)          ADD    "TOTAL FOR" SIZE               
         STC   R1,DLCBLEN                 AND SET LENGTH                        
*                                                                               
DWNHD13  DS    0H                                                               
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBTXT     TYPE   IS TEXT                               
         GOTO1 ADLFLD,(R9)         DOWN-LOAD FIELD                              
*                                                                               
DWNHD14  DS    0H                                                               
         TM    DWNOPT1,DWNROWS     Are rows being down-loaded ?                 
         BZ    DWNHD30             No                                           
         IC    R0,FMT#ROWS         Yes, get number of rows                      
         L     R3,FMTROW           A(Row table)                                 
*                                                                               
DWNHD15  DS    0H                  ASSUME NO PREFIX, I.E.                       
         MVI   DLCBLEN,1           Set length to 1                              
         MVC   DLCBFLX,XSPACES     Set text to blank                            
         CLI   ROWTYPE,ROWCNTR     Center type row heading ?                    
         BE    *+8                                                              
         CLI   ROWTYPE,ROWLEFT     Left type row heading ?                      
         BE    *+8                                                              
         CLI   ROWTYPE,ROWRGHT     Right type row heading ?                     
         BE    *+8                                                              
         CLI   ROWTYPE,ROWMID      Mid-line ?                                   
         BNE   DWNHD20             None of the above                            
         ICM   R2,15,ROWPRFX       Row prefix ?                                 
         BZ    DWNHD16             No, so pad with dummy output                 
         CHI   R8,1                2nd pass, 2nd set of headings                
         BE    DWNHD18             Print blank for 2nd head                     
         SR    R1,R1               Yes, so use prifix                           
         IC    R1,ROWPRFX          Length of prefix                             
         STC   R1,DLCBLEN          Use field length                             
         BCTR  R1,0                                                             
         EXMVC R1,DLCBFLX,0(R2)    Insert prefix text to be output              
         B     DWNHD18                                                          
*                                                                               
DWNHD16  DS    0H                                                               
*&&UK                                                                           
         USING KYWD,R6             Get column heading name                      
         CHI   R8,1                2nd pass, 2nd set of headings                
         BE    DWNHD18             Yes, skip                                    
         L     R6,ROWINDEX         Index into table entry                       
         XC    WORK,WORK                                                        
         MVI   WORK,ESC#LFJT       Left justify                                 
         MVC   WORK+1(2),KYWDD#    Dictionary number                            
         MVI   WORK+3,24           Max length                                   
         GOTO1 ADDICTAT,DMCB,C'SL  ',WORK,0                                     
*                                                                               
DWNHD17  TM    FMTPOPT1,RPFMCASE   Leave headings in mixed case ?               
         BO    DWNHD17A            Yes                                          
         L     RE,AUPCASE          No                                           
         TR    WORK,0(RE)          Make headings upper case                     
DWNHD17A LA    R1,WORK+24          Point to end less one                        
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    RF,WORK                                                          
         SR    R1,RF               Length of heading - 1                        
         EXMVC R1,DLCBFLX,WORK     Insert heading to be output                  
         LA    R1,1(,R1)                                                        
         STC   R1,DLCBLEN          Field length                                 
         DROP  R6                                                               
*&&                                                                             
DWNHD18  DS    0H                                                               
         TM    DWNOPT1,DWNCHOP     Down-load fixed length data ?                
         BZ    *+10                No,  use length of text                      
         MVC   DLCBLEN,ROWPRTSZ    Yes, use row print size                      
         MVI   DLCBACT,DLCBPUT     Put text                                     
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 ADLFLD,(R9)                                                      
*                                                                               
         TM    ROWFLAGS,ROWADDR    Row address ?                                
         BO    DWNHD18A            No, skip                                     
*&&US*&& TM    ROWDAIND,ROWBDR     or BDR attribute?                            
         BZ    DWNHD20                                                          
DWNHD18A CLI   RAD#LDWN,0          Down-load addresses ?                        
         BE    DWNHD20             No, skip                                     
         ZIC   R2,RAD#LDWN         # of row address lines to output             
*                                                                               
DWNHD19  DS    0H                                                               
         TM    DWNOPT1,DWNCHOP     Down-load fixed length data ?                
         BZ    *+10                No,  use length of text                      
         MVC   DLCBLEN,RADPRTSZ    Yes, use row address print size              
         MVI   DLCBACT,DLCBPUT     Put text                                     
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 ADLFLD,(R9)                                                      
         BCT   R2,DWNHD19          Any more lines to insert (pad) >             
*                                                                               
DWNHD20  AHI   R3,ROWLNQ           Bump to next row                             
         BCT   R0,DWNHD15                                                       
*                                                                               
         USING COLD,R3                                                          
DWNHD30  SR    R2,R2               Down-load column headings                    
         L     R3,FMTCOL                                                        
         IC    R0,FMT#COLS         NUMBER OF COLUMNS TO PROCESS                 
         ST    R3,SVR3             INITALIZE TO FIRST COLUMN                    
*                                                                               
DWNHD40  L     R3,SVR3             LOAD COLUMN TO PROCESS                       
         ZIC   RF,COLPRTSQ         PRINT SEQUENCE                               
         BCTR  RF,0                SWITCH TO NEW COLUMN                         
         MHI   RF,COLLNQ                                                        
         A     RF,FMTCOL           POINT TO COLUMN TO PRINT IN SEQ.             
         AHI   R3,COLLNQ           BUMP TO NEXT COLUMN                          
         ST    R3,SVR3             SAVE NEXT COLUMN TO PROCESS                  
         LR    R3,RF               POINT TO NEW COLUMN TO PROCESS               
         TM    COLFLAGS,COLHIDE    SKIP HIDDEN COLUMNS                          
         BO    DWNHD80                                                          
         CLI   COLSTACK,0                                                       
         BNE   DWNHD80                                                          
         CLI   COLSCPWD,0          Any stack width ?                            
         BE    DWNHD42             No                                           
         MVI   DLCBLEN,1           Yes, output dummy column for prefix          
         TM    DWNOPT1,DWNCHOP     Down-load fixed length data ?                
         BZ    *+10                No,  use length of text                      
         MVC   DLCBLEN,COLSCPWD    Yes top stack prefix width                   
         MVI   DLCBACT,DLCBPUT     Put text                                     
         MVI   DLCBTYP,DLCBTXT                                                  
         MVC   DLCBFLX,XSPACES                                                  
         GOTO1 ADLFLD,(R9)                                                      
*                                                                               
DWNHD42  NC    COLPRT,COLPRT       COLUMN PRINT ZERO ?                          
         BZ    DWNHD80             YES, SKIP COLUMN                             
         TM    FMTIND,FMTSFTH      Soft headings ?                              
         BZ    DWNHD60             No                                           
         CHI   R8,1                AREADY PROCESSED THIS CODE ?                 
         BE    DWNHD60             YES                                          
         GOTOR SOFTHEAD,DMCB,(0,COLD),FMTRECD                                   
*                                                                               
DWNHD60  MVI   DLCBLEN,0           Zero is default to length of text            
         SR    RE,RE                                                            
         IC    RE,COLHEAD1         Length of area                               
         L     RF,COLHEAD1                                                      
         CHI   R8,1                Process 1st or 2nd heading                   
         BNE   DWNHD61                                                          
         L     RF,COLHEAD2                                                      
         IC    RE,COLHEAD2         Length of area                               
                                                                                
DWNHD61  TM    COLFLAGS,COLAMT     IS   THIS A NUMERIC FIELD ?                  
         BZ    DWNHD62             NO,  BRANCH TO TEXT LENGTH CHECK             
         TM    DWNOPT1,DWNTXT#     DOWN-LOAD NUMBERS AS TEXT ?                  
         BO    DWNHD62             YES, BRANCH TO TEXT LENGTH CHECK             
         TM    DWNOPT2,DWN#PAD     DOWN-LOAD NUM.S WITH LEADING ZEROS ?         
         BZ    *+10                NO,  LEAVE DLCBLEN ZERO                      
         MVC   DLCBLEN,COLSIZE     YES, SO SET FIXED LENGTH                     
         B     DWNHD64             CONTINUE                                     
*                                                                               
DWNHD62  DS    0H                  FIXED TEXT LENGTH CHECK                      
         TM    DWNOPT1,DWNCHOP     USE  EXACT COLUMN WIDTH?                     
         BZ    *+10                NO                                           
         MVC   DLCBLEN,COLSIZE     YES, SO SET FIXED LENGTH                     
*                                                                               
DWNHD64  DS    0H                                                               
         SHI   RE,1                                                             
         BM    DWNHD70                                                          
         EXMVC RE,DLCBFLX,0(RF)                                                 
*                                                                               
DWNHD70  DS    0H                                                               
         CLI   DLCBLEN,0           ANY  LENGTH SPECIFIED ?                      
         BNE   DWNHD75             YES, SKIP CHECKS                             
         NC    DLCBFLX,DLCBFLX     YES, MAKE SURE TEXT FIELD NOT ZEROS          
         BNZ   DWNHD75             NOT  ZERO, DOWN-LOAD FIELD                   
         MVI   DLCBLEN,1           ZERO, SET LENGTH 1 TO DOWN-LOAD C' '         
*                                                                               
DWNHD75  DS    0H                                                               
         MVI   DLCBACT,DLCBPUT     PUT TEXT                                     
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 ADLFLD,(R9)                                                      
*                                                                               
DWNHD80  AHI   R3,COLLNQ           BUMP TO NEXT COLUMN                          
         BCT   R0,DWNHD40                                                       
*                                                                               
*        GOTO1 DWNLOAD,DWNEOL      Not yet installed                            
         MVI   DLCBACT,DLCBEOL     MARK END OF LINE                             
         GOTO1 ADLFLD,(R9)                                                      
         BCT   R8,DWNHD10          PROCESS SECOND HEADING                       
*                                                                               
DWNHD99  B      DWNXIT                                                          
         EJECT ,                                                                
***********************************************************************         
*  User defined headers (up to 3)                                     *         
***********************************************************************         
         USING XTRELD,R3                                                        
         USING ACRL2D,R7                                                        
DOWNHDR  NTR1                                                                   
         L     R3,AHDRDATA         Location of data                             
         L     R7,AACRL2D                                                       
         L     R9,DWNLDCB                                                       
*                                                                               
DOWNH050 CLI   0(R3),0             End of elements                              
         BE    DOWNHXIT                                                         
         SR    R2,R2                                                            
         IC    R2,XTRSUB#          Number of sub-elements                       
         LA    R3,XTRSUBEL                                                      
*                                                                               
         USING XTRSUBEL,R3                                                      
DOWNH100 TM    XTRSIND,XTRSKYW     Keyword type                                 
         BZ    DOWNH200                                                         
*                                                                               
         MVC   DLCBLEN,XTRSWDTH    Length to download                           
         BAS   RE,DWNKEYTY                                                      
         BE    DOWNH300                                                         
         B     DOWNH310                                                         
*                                                                               
DOWNH200 LA    RE,XLCHOP           CLEAR XLCHOP TO ZEROES OR SPACES             
         LHI   RF,GESIZEQ                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,8,SPACES         SET PADDING CHARACTER                        
         MVCL  RE,R0                                                            
*                                                                               
         SR    RF,RF               Must be text data                            
         ICM   RF,1,XTRSUBLN       Get length of text                           
         SHI   RF,(XTRSDATA-XTRSUBEL)                                           
         BP    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         STC   RF,DLCBLEN           Length of data to download                  
         STC   RF,PRTSIZE                                                       
         EXMVC RF,XLCHOP,XTRSDATA                                               
*                                                                               
DOWNH300 MVI   DLCBACT,DLCBPUT     Put text                                     
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DWNLOAD,DWNTEXT                                                  
*        GOTO1 ADLFLD,(R9)                                                      
*                                                                               
DOWNH310 SR    RF,RF                                                            
         IC    RF,XTRSUBLN         Length of sub-element                        
         AR    R3,RF                                                            
         BCT   R2,DOWNH100         Get next                                     
         B     DOWNH050            Should be @ next element or EOT              
*                                                                               
DOWNHXIT MVI   DLCBACT,DLCBEOL     Mark EOL                                     
         GOTO1 ADLFLD,(R9)                                                      
         B     DWNXIT                                                           
         DROP  R3,R4                                                            
*=====================================================================*         
*  User defined total trailer                                         *         
*=====================================================================*         
         USING XTRELD,R3                                                        
DOWNTTR  NTR1                                                                   
         L     R3,ATTRDATA                                                      
         ST    R2,ACURROW          Row working on                               
         B     DOWNT05                                                          
*=====================================================================*         
*  User defined trailer                                               *         
*=====================================================================*         
         USING XTRELD,R3                                                        
         USING ACRL2D,R7                                                        
DOWNTRL  NTR1                                                                   
         L     R3,ATRLDATA         Location of data                             
         L     R7,AACRL2D                                                       
         L     R9,DWNLDCB                                                       
         MVC   ACURROW,FMTROW      Report request row                           
*                                                                               
DOWNT05  SR    R8,R8                                                            
         IC    R8,XTRSUB#          Number of sub-elements                       
         LA    R3,XTRSUBEL                                                      
*                                                                               
         USING XTRSUBEL,R3                                                      
DOWNT10  SR    RF,RF                                                            
         IC    RF,XTRSUBLN                                                      
         TM    XTRSIND,XTRSKYW     Keyword                                      
         BZ    DOWNT20                                                          
         BAS   RE,DWNKEYTY                                                      
         BE    DOWNT80             Data ok so download                          
         B     DOWNT82             Already download data                        
*                                                                               
         USING ROWD,R6                                                          
DOWNT20  TM    XTRSIND,XTRSROW                                                  
         BZ    DOWNT30                                                          
         ICM   R6,15,XTRSNUM                                                    
         B     DOWNT80                                                          
*                                                                               
         USING COLD,R4                                                          
DOWNT30  TM    XTRSIND,XTRSCOL     Column                                       
         BZ    DOWNT50                                                          
         ICM   R4,15,XTRSNUM       A(Column table entry)                        
         TM    COLFLAGS,COLAMT                                                  
         BZ    DOWNT42                                                          
         L     R6,ACURROW                                                       
         L     R2,ROWACM           Report total row                             
         AH    R2,COLACM           Displacement into row                        
         MVC   OPTPRT,COLEDOPT                                                  
         MVC   NDECIMAL,COLDCMLS                                                
         MVC   ROUNDTO,COLROUND                                                 
         GOTO1 =A(EDITIT),DMCB,(OPTPRT,(R2)),(XTRSWDTH,DLCBFLX),       X        
               COLFCCDE                                                         
         MVC   PRTSIZE,XTRSWDTH                                                 
         GOTO1 =A(DWNLOAD),DWNPACK                                              
         B     DOWNT82                                                          
         DROP  R6                                                               
*                                                                               
DOWNT42  DS    0H                                                               
         LA    RE,XLCHOP           CLEAR XLCHOP TO ZEROES OR SPACES             
         LHI   RF,GESIZEQ                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,8,SPACES         SET PADDING CHARACTER                        
         MVCL  RE,R0                                                            
         L     R6,ASRTWRK                                                       
         AH    R6,COLDSP                                                        
         SR    RF,RF                                                            
         ICM   RF,3,COLSRTSZ                                                    
         CHI   RF,L'XLCHOP                                                      
         BNH   *+8                                                              
         LA    RF,L'XLCHOP                                                      
         SHI   RF,1                                                             
         BNM   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   PRTSIZE,XTRSWDTH                                                 
         EXMVC RF,XLCHOP,0(R6)                                                  
         B     DOWNT80                                                          
*                                                                               
DOWNT50  ST    RF,SVRF                                                          
         LA    RE,XLCHOP           CLEAR XLCHOP TO ZEROES OR SPACES             
         LHI   RF,GESIZEQ                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,8,SPACES         SET PADDING CHARACTER                        
         MVCL  RE,R0                                                            
*                                                                               
         L     RF,SVRF                                                          
         SHI   RF,(XTRSDATA-XTRSUBEL)                                           
         STC   RF,PRTSIZE                                                       
         SHI   RF,1                                                             
         BNM   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         EXMVC RF,XLCHOP,XTRSDATA                                               
*                                                                               
*        EXMVC RF,DLCBFLX,XTRSDATA                                              
*                                                                               
DOWNT80  MVI   DLCBACT,DLCBPUT     Put text                                     
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 =A(DWNLOAD),DWNTEXT                                              
*        GOTO1 ADLFLD,(R9)                                                      
*                                                                               
DOWNT82  SR    RF,RF                                                            
         IC    RF,XTRSUBLN         Length of sub-element                        
         AR    R3,RF                                                            
         BCT   R8,DOWNT10          Get next                                     
*                                                                               
         MVI   DLCBACT,DLCBEOL     Mark EOL                                     
         GOTO1 ADLFLD,(R9)                                                      
         B     DWNXIT                                                           
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*        R3 = XTRSUBEL                                                *         
***********************************************************************         
         USING XTRSUBEL,R3                                                      
         USING HDRKYWD,R4                                                       
DWNKEYTY NTR1                                                                   
         LA    R4,HDRKYW           Table of keywords                            
DWNKTY05 CLI   0(R4),EOT                                                        
         BNE   *+6                                                              
         DC    H'00'               Got to be in table                           
*                                                                               
         CLC   XTRSDD#,HDRKDD#     Data dictionary                              
         BE    DWNKTY10                                                         
         AHI   R4,HDRKLNQ                                                       
         B     DWNKTY05                                                         
*                                                                               
DWNKTY10 SR    R2,R2                                                            
         ICM   R2,3,HDRKDATA       Get S(0) type into regs                      
         LR    R1,R2                                                            
         N     R1,=XL4'00000FFF'   Isolate displacement                         
         SRL   R2,12               Isolate Base Register  0000000R              
         EX    R2,*+8                                                           
         B     *+6                                                              
         AR    R1,R0               R0 is ignored, base + displacement           
*                                                                               
         CLI   HDRKOTH,PCKTYP                                                   
         BNE   DWNKTY25                                                         
         LR    R6,R1                                                            
         MVI   NDECIMAL,ZERODEC                                                 
         MVI   ROUNDTO,0                                                        
         GOTO1 =A(EDITIT),DMCB,(FMTPIND,(R6)),(XTRSWDTH,DLCBFLX),0              
         MVC   PRTSIZE,XTRSWDTH                                                 
         GOTO1 =A(DWNLOAD),DWNPACK                                              
         B     DWNKNO              Set to indicate already donwloaded           
*                                                                               
DWNKTY25 ST    R1,FULL             Save off contents of R1                      
         LA    RE,XLCHOP           CLEAR XLCHOP TO ZEROES OR SPACES             
         LHI   RF,GESIZEQ                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,8,SPACES         SET PADDING CHARACTER                        
         MVCL  RE,R0                                                            
         L     R1,FULL             Reset Address of current field               
*                                                                               
         CLI   HDRKOTH,DTETYP                                                   
         BNE   DWNKTY30                                                         
         LR    R6,R1                                                            
         GOTO1 =A(DATING),DMCB,(1,(R6)),(FMTDTIDX,WORK)                         
         SR    RF,RF                                                            
         IC    RF,DMCB                                                          
         LA    R1,WORK                                                          
         B     DWNKTY32                                                         
*                                                                               
DWNKTY30 SR    RF,RF                                                            
         IC    RF,HDRKLEN          Length of data                               
*                                                                               
DWNKTY32 SHI   RF,1                                                             
         BNM   *+6                                                              
         DC    H'00'                                                            
         MVC   PRTSIZE,XTRSWDTH                                                 
         EXMVC RF,XLCHOP,0(R1)                                                  
         SR    RE,RE                                                            
*                                                                               
DWNKNO   LTR   RE,RE                                                            
         B     DWNXIT                                                           
         DROP  R3,R4                                                            
                                                                                
***********************************************************************         
*        Table of keywords supported in header/trailer/total lines    *         
***********************************************************************         
HDRKYW   DS    0H                                                               
         DC    S(TDDATE),AL2(AC#RSTDY),AL1(08,DTETYP)                           
         DC    S(UNIQUE#),AL2(AC#GENNO),AL1(12,0)                               
         DC    S(COMPNAME),AL2(AC#RSAGN),AL1(36,0)                              
         DC    S(OFFNAME),AL2(AC#RSONN),AL1(33,0)                               
         DC    S(SPACES),AL2(AC#RSBLK),AL1(1,0)                                 
         DC    S(SPACES),AL2(AC#RSGAP),AL1(1,0)                                 
         DC    S(FMTCODE),AL2(AC#RSFTC),AL1(8,0)                                
         DC    S(FMTNAME),AL2(AC#RSFTC),AL1(36,0)                               
         DC    S(REQTIME),AL2(AC#TIME),AL1(8,0)                                 
         DC    S(REQHMS),AL2(AC#RSHMS),AL1(6,0)                                 
         DC    S(DTLCOUNT),AL2(AC#RSCNT),AL1(8,PCKTYP)                          
         DC    S(DTTCOUNT),AL2(AC#RECCT),AL1(8,PCKTYP)                          
         DC    AL1(EOT)                                                         
*                                                                               
DTETYP   EQU   1                                                                
PCKTYP   EQU   2                                                                
                                                                                
REQHMS   DS    CL6                 HHMMSS                                       
DTTCOUNT DS    PL(L'DTLCOUNT)                                                   
         EJECT                                                                  
***********************************************************************         
*  Standard DDS FTP header                                            *         
***********************************************************************         
         USING BIGPRNTD,R4                                                      
         USING ACRL2D,R7                                                        
DOWNFTP  NTR1                                                                   
         L     R7,AACRL2D                                                       
         MVC   XP+4(5),=C'*HDR*'                                                
         MVC   XP+9(6),=C'EDICT='                                               
         MVI   XP+34,C'W'          Wide 132 report. So how about 198 ?          
         MVC   XP+15(8),DESTID                                                  
         GOTO1 REPORT                                                           
                                                                                
         MVC   XP(5),=C'++DDS'                                                  
         MVC   XP+6(3),=C'ACW'                                                  
         MVC   XP+9(2),ALPHAID                                                  
         MVC   XP+11(3),=C'TRN'                                                 
         MVI   XP+15,C'A'                                                       
         MVC   XP+16(2),QPROG                                                   
         MVI   XP+18,C','                                                       
         MVC   XP+19(8),FMTCODE                                                 
         GOTO1 REPORT                                                           
                                                                                
         CLC   DWNSUB,SPACES                                                    
         BNH   DWNFTP35                                                         
         MVC   XP(5),=C'++DDS'                                                  
         MVC   XP+11(3),=C'SUB'                                                 
         MVC   XP+15(L'DWNSUB),DWNSUB                                           
         GOTO1 REPORT                                                           
         B     DWNFTP40                                                         
                                                                                
DWNFTP35 MVC   XP(5),=C'++DDS'                                                  
         MVC   XP+11(3),=C'FTP'                                                 
         MVC   XP+15(8),FMTCODE                                                 
         GOTO1 REPORT                                                           
                                                                                
DWNFTP40 CLC   DWNFIL,SPACES                                                    
         BNH   DWNFTPX                                                          
         MVC   XP(5),=C'++DDS'                                                  
         MVC   XP+11(3),=C'FIL'                                                 
         MVC   XP+15(L'DWNFIL),DWNFIL                                           
         GOTO1 REPORT                                                           
                                                                                
DWNFTPX  B     DWNXIT                                                           
         DROP  R7                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  Down-load hook routine (Relative code) RB may be someone elses     *         
***********************************************************************         
         SPACE 1                                                                
DWNHOOK  TM    DWNOPT2,DWNFSTL     First line already downloaded?               
         JNO   *+8                                                              
         MVI   FORCEHED,NO         Never head up                                
*&&US                                                                           
         TM    DWNOPT3,DWNFDSDN    DSN DOWNLOAD FILE                            
         JZ    DWNHOOK5                                                         
         CLI   DLCBACT,DLCBEOL     End of line                                  
         JE    DWNHOOK5            No                                           
*                                                                               
         L     RF,ADWNLINE                                                      
*        CLI   0(RF),C' '                                                       
         CLC   0(L'SPACES,RF),SPACES                                            
         JE    DWNHOOK4                                                         
         AHI   RF,1999                                                          
         LHI   R0,1999                                                          
DWNHOOK2 CLI   0(RF),C' '                                                       
         JNE   DWNHOOK3                                                         
         AHI   RF,-1                                                            
         JCT   R0,DWNHOOK2                                                      
         J     DWNHOOK4                                                         
DWNHOOK3 AHI   RF,1                                                             
DWNHOOK4 MVC   0(L'XP,RF),XP                                                    
*&&                                                                             
DWNHOOK5 OI    DWNOPT2,DWNFSTL                                                  
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         L     RF,REPORT                                                        
         BR    RF                                                               
         DROP  R5                                                               
*&&US                                                                           
DWNLINE  DS    XL2000                                                           
*&&                                                                             
         EJECT                                                                  
         TITLE 'Hook into downloading to put to work file'                      
*=====================================================================*         
*        This is an interface code to output the data to a            *         
*           worker file, using DDLINK as the carrier.                 *         
*=====================================================================*         
                                                                                
         USING FMTRECD,R5                                                       
         USING DLCBD,R9                                                         
         USING ACRL2D,R7                                                        
         USING LP_D,R6                                                          
WORKOUT  NMOD1 0,**WOUT**                                                       
         L     RC,RLBASEC                                                       
         L     R7,AACRL2D                                                       
         L     R9,DWNLDCB                                                       
         L     R5,AFORMATS                                                      
                                                                                
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         L     R6,ACMALP_D         R6=A(LP_D)                                   
         DROP  R2                                                               
                                                                                
WOUT010  MVC   WOUTACT,DLCBACT                                                  
         CLI   WOUTACT,DLCBINIT    Initialize                                   
         BE    WOUTINI                                                          
         CLI   WOUTACT,DLCBEOR     End of report                                
         BE    WOUTEOR                                                          
         CLI   WOUTACT,DLCBPUT     Put text to DDLINKIO for worker file         
         BE    WOUTPUT                                                          
         CLI   WOUTACT,DLCBEOL     End of line                                  
         BE    WOUTEOL                                                          
         DC    H'00'                                                            
         EJECT ,                                                                
***********************************************************************         
* Initialize and output the schema record                                       
***********************************************************************         
                                                                                
WOUTINI  DS    0C                                                               
         GOTOR ACREPORT                                                         
         MVI   NEWLINE,YES                                                      
         MVI   DBREAK,YES                                                       
         XC    LSTSECOF,LSTSECOF                                                
         XC    WOUTMASK,WOUTMASK                                                
         L     RE,AMAPDATA                          Base of block               
         L     RF,=A(RATEQ)                                                     
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         TM    DDLNKIND,DDLNKGBL                                                
         BZ    *+8                                                              
         BRAS  RE,SECDEF           Build Secuirty Definitions                   
         BRAS  RE,SCHEMA           Build Schema                                 
         BRAS  RE,RPTDEF           Build Report   Definition data               
         BRAS  RE,REQDEF           Build Request  Definition data               
         B     WOUTXIT                                                          
         EJECT ,                                                                
***********************************************************************         
* Output the data to DDLINK                                                     
***********************************************************************         
                                                                                
WOUTPUT  DS    0H                                                               
         SR    R2,R2                                                            
         ICM   R2,1,DATAMAP#         R2 = Map relative code                     
         BZ    WOUTXIT                                                          
                                                                                
         LA    R1,L'DLCBFLX        Find length of data, use length 160          
         LA    RE,XLCHOP(R1)       Point to end of data                         
WOUTP10  BCTR  RE,0                Point to last character of data              
         CLI   0(RE),C' '                                                       
         BH    WOUTP15                                                          
         BCT   R1,WOUTP10                                                       
         LA    R1,1                Default to length 1 when no data             
                                                                                
WOUTP15  LR    R3,R1               R3 = Length of data                          
         CLI   QOPT4,C'1'                                                       
         BNE   *+8                                                              
         BRAS  RE,SHOWPUT          Debug                                        
         LR    RE,R2               R2 = Data map #                              
         BCTR  RE,0                                                             
         MHI   RE,L'DLCBFLX+1      AL1(Len),CL160'Data'                         
         A     RE,AMAPDATA         Base of block                                
*                                                                               
         TM    DDLNKIND,DDLNKGBL+DDLNKTST    Only batch or test mode            
         BZ    WOUTP16             Print all data once there is a break         
         CLI   DBREAK,YES          Yes                                          
         BE    WOUTP18             Output data even if same as prior            
         CLI   NEWLINE,YES                                                      
         BNE   WOUTP16                                                          
         CLC   LMTSECOF,LSTSECOF                                                
         BE    WOUTP16                                                          
         MVI   DBREAK,YES                                                       
         B     WOUTP18                                                          
*                                                                               
WOUTP16  SHI   R1,1                                                             
         BM    WOUTP18                                                          
                                                                                
WOUTP17  CLM   R3,1,0(RE)          Same length data ?                           
         BNE   WOUTP18             No, different so output data                 
         EXCLC R1,1(RE),XLCHOP     Same as last time ?                          
         BE    WOUTXIT             Yes                                          
*        TM    DDLNKIND,DDLNKGBL                                                
*        BZ    WOUTXIT                                                          
*        CLC   LMTSECOF,LSTSECOF                                                
*        BE    WOUTXIT                                                          
*        MVI   DBREAK,YES          Data has changed                             
*&&DO                                                                           
         CLI   QOPT3,C'X'                                                       
         BE    WOUTP18             No compression                               
                                                                                
         LR    RE,R2               RE = DATAMAP#                                
         SHI   RE,2                Starts at 2 so make it 0 based               
*                                  Divide by 32 (# of bits in register)         
         SRDL  RE,5                /32 - RE = quotient, RF = remainder          
         SHI   RE,3                Reverse displacement (4 x 32 bits)           
         LPR   RE,RE                                                            
         SRL   RF,(32-5)           Adjust remainder into register               
         SLL   RE,2                x4 - adjust full word                        
         LA    R1,1                Setup bit in mask                            
         SLL   R1,0(RF)            Move bit to correct location in 32           
         ST    R1,FULL                                                          
         LA    RE,WOUTMASK(RE)                                                  
         OC    0(L'FULL,RE),FULL   Or on bit or not                             
         B     WOUTXIT                                                          
*&&                                                                             
                                                                                
WOUTP18  DS    0H                                                               
         CLI   RCRERUN,C'X'        XML case                                     
         BNE   *+8                                                              
         MVI   DBREAK,YES          Data has changed                             
                                                                                
         STC   R3,0(,RE)           Save new length                              
         LTR   R1,R1                                                            
         BM    WOUTP19                                                          
         EXMVC R1,1(RE),XLCHOP     Save off the data                            
                                                                                
WOUTP19  CLI   NEWLINE,YES                                                      
         BNE   WOUTP20                                                          
         MVI   NEWLINE,NO                                                       
         GOTOR DDLINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',REC_DATA)          
                                                                                
         USING LQ_D,RE                                                          
         TM    DDLNKIND,DDLNKGBL                                                
         BZ    WOUTP20                                                          
         LA    RE,WORK                                                          
         XC    WORK(15),WORK                                                    
         MVI   LQ_EL,LQ_VALUQ                                                   
         MVI   LQ_LN+1,LQ_LNCQ+L'LMTSECOF                                       
         MVI   LQ_DCODE+1,EL_OFSEC                                              
         MVC   LQ_VVALU(L'LMTSECOF),LMTSECOF                                    
         GOTOR DDLINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTGEL',0),WORK            
         MVC   LSTSECOF,LMTSECOF                                                
         DROP  RE                                                               
*&&DO                                                                           
         CLI   QOPT3,C'X'                                                       
         BE    WOUTP20             No compression                               
         GOTOR DDLINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTRAW',1),       X        
               ('LD_HEXDQ',WOUTMASK),(L'WOUTMASK,0)                             
*&&                                                                             
                                                                                
WOUTP20  DS    0H                                                               
         SR    RF,RF                                                            
         IC    RF,DATAMTYP         Current data type                            
         LA    R4,LIOTEDT                                                       
         CLI   DATAMTYP,X'40'      Raw or edit type                             
         BH    *+8                                                              
         LA    R4,LIOTRAW          Put data out as raw                          
                                                                                
         CLI   QOPT4,C'2'                                                       
         BNE   *+8                                                              
         BRAS  RE,SHOWPUT                                                       
         GOTOR DDLINKIO,DMCB,('LIOAPUT',LP_ALIOB),((R4),(R2)),         X        
               ((RF),XLCHOP),((R3),0)                                           
         B     WOUTXIT                                                          
                                                                                
SHOWPUT  NTR1                                                                   
         L     R4,VBIGPRNT                                                      
         CVD   R2,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  XP+1(2),DUB                                                      
         MVI   XP+3,C'='                                                        
         LR    RF,R3                                                            
         BCTR  RF,0                                                             
         EXMVC RF,XP+4,XLCHOP                                                   
         GOTOR ACREPORT                                                         
         J     WOUTXIT                                                          
***********************************************************************         
* Finish up the line and output mask                                            
***********************************************************************         
                                                                                
WOUTEOL  DS    0C                                                               
*&&DO                                                                           
         CLI   QOPT4,C'1'                                                       
         BNE   WOUTE02                                                          
         GOTOR ACREPORT                                                         
                                                                                
WOUTE02  OC    WOUTMASK,WOUTMASK                                                
         BZ    WOUTE20                                                          
         CLI   NEWLINE,YES                                                      
         BNE   WOUTE10                                                          
         GOTOR DDLINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',REC_DATA)          
                                                                                
WOUTE10  GOTOR DDLINKIO,DMCB,('LIOAUPD',LP_ALIOB),('LIOTRAW',1),       X        
               ('LD_HEXDQ',WOUTMASK),(L'WOUTMASK,0)                             
         XC    WOUTMASK,WOUTMASK                                                
*&&                                                                             
                                                                                
WOUTE20  MVI   NEWLINE,YES                                                      
         MVI   DBREAK,NO                                                        
         B     WOUTXIT                                                          
                                                                                
WOUTEOR  DS    0C                                                               
         TM    DDLNKIND,DDLNKTST                                                
         BZ    WOUTXIT                                                          
         GOTOR DDLINKIO,DMCB,('LIOACLO',LP_ALIOB),0,0,0                         
                                                                                
WOUTXIT  DS    0C                                                               
         XIT1                                                                   
         DROP  R6                                                               
         TITLE 'Build Schema record for data'                                   
***********************************************************************         
* Build Schema record based on rows/columns                           *         
*       R5 = A(Current format)                                        *         
*       R6 = A(LP_D)               Interface for DDLINKIO             *         
***********************************************************************         
         USING FMTRECD,R5                                                       
         USING LP_D,R6                                                          
         USING ACRL2D,R7                                                        
SCHEMA   NTR1                                                                   
                                                                                
*---------------------------------------*                                       
* Build first part of Schema from table *                                       
*---------------------------------------*                                       
                                                                                
         GOTOR PUTMAPEL,DMCB,MAPH001                                            
                                                                                
*-----------------------------------*                                           
* Build row schemes                 *                                           
*-----------------------------------*                                           
         USING ROWD,R3                                                          
         L     R3,FMTROW                                                        
         ZIC   R0,FMT#ROWS                                                      
         AHI   R3,ROWLNQ           Don't process first row                      
         SHI   R0,1                                                             
         XC    ATRB#,ATRB#                                                      
         MVC   NODE#,=AL2(REC_DATA)    First node, record                       
         XC    PARENT,PARENT           No parent yet                            
*                                                                               
* MAP first node                                                                
*                                                                               
         GOTOR PUTMAPEL,DMCB,MAPD001                                            
         LTR   R0,R0                                                            
         BZ    SCHM090             No rows                                      
*                                                                               
         USING COLD,R4                                                          
SCHM032  DS    0H                                                               
         CLI   ROWTYPE,ROWCOL                                                   
         BNE   SCHM033             Only process true rows                       
         ICM   R4,15,ROWACOL                                                    
         BNZ   *+6                                                              
         DC    H'00'                                                            
                                                                                
         TM    ROWXPIND,ROWRATE    Rate keyword                                 
         BO    SCHM060                                                          
         TM    COLFLAGS,COLHIDE                                                 
         BO    SCHM060                                                          
         DROP  R4                                                               
                                                                                
SCHM033  MVI   NEWNODE,NO                                                       
         ZIC   RF,ROWNODE#                                                      
         CH    RF,NODE#            Processing same node                         
         BE    SCHM034             Yes, so must be an attribute                 
         MVI   NEWNODE,YES                                                      
         MVC   PARENT,NODE#                                                     
         STH   RF,NODE#            Set new node #                               
         XC    ATRB#,ATRB#         Set to base 0                                
         B     SCHM036                                                          
                                                                                
SCHM034  LH    RF,ATRB#            Attribute counter, less one                  
         AHI   RF,1                                                             
         STH   RF,ATRB#                                                         
*                                                                               
* MAP attributes, even ones that turn into nodes                                
*                                                                               
         USING MPDELD,R2                                                        
SCHM036  LA    R2,MAPALBL          *** LABEL ***  use keyword                   
         BRAS  RE,ROWLBL                                                        
         MVC   MPDDATA(5),WORK     Row number                                   
                                                                                
         USING KYWD,RF                                                          
         L     RF,ROWINDEX         Point to keyword in table                    
         MVC   MPDDATA+5(6),KYWCODE                                             
         CLI   NEWNODE,YES                                                      
         BE    SCHM037                                                          
         MVC   MPDDATA+5(6),=CL6'Code'                                          
         TM    ROWFLAGS,ROWCDE                                                  
         BO    SCHM037                                                          
         MVC   MPDDATA+5(6),=CL6'Name'                                          
         TM    ROWFLAGS,ROWNME                                                  
         BO    SCHM037                                                          
         MVC   MPDDATA+5(6),SPACES                                              
                                                                                
SCHM037  LA    R2,MAPAID#             *** ID# ***                               
         MVC   MPDDATA+1(1),ROWMAP#                                             
*                                                                               
         LA    R2,MAPATYP             *** TYPE ***                              
         MVI   MPDDATA,MPD_CHR        Character type (default)                  
         TM    ROWDAIND,ROWDAYMD                                                
         BZ    SCHM038                                                          
         MVI   MPDDATA,MPD_DTE        Date type                                 
*                                                                               
SCHM038  LA    R2,MAPANODE         *** NODE or ATTRIBUTE ***                    
         MVC   MPDDATA(2),NODE#                                                 
         MVI   MPDID#,ATR_NODE     Default to Node                              
         CLI   NEWNODE,YES         New node ? (Same keyword as before)          
         BE    *+8                 No,  so make node                            
         MVI   MPDID#,ATR_ATRB     Yes, so make attribue of                     
*                                                                               
         LA    R2,MAPAPRNT         *** PARENT ***                               
         OI    MPDIND,MPDIOFF      Set to turn off element for process          
         CLI   NEWNODE,YES         New node                                     
         BNE   SCHM040                 No, so no parent                         
         NI    MPDIND,TURNOFF-MPDIOFF  Yes, process element                     
         MVC   MPDDATA(2),PARENT                                                
*                                                                               
SCHM040  DS    0H                                                               
*        LA    R2,MAPACMP1         *** COMPRESSION BIT ***                      
*        ZIC   RF,NODE#                                                         
*        AH    RF,ATRB#                                                         
*        STCM  RF,3,MPDDATA                                                     
***      MVC   MPDDATA+1(1),ROWNUM                                              
*                                                                               
         USING KYWD,RF                                                          
         L     RF,ROWINDEX         Point to keyword in table                    
         LA    R2,MAPAGID#         *** DATA DICTIONARY # ***                    
         SR    RE,RE                                                            
         MVC   MPDDATA(2),KYWDD#                                                
         TM    ROWFLAGS,ROWCDE     Are we printing code ?                       
         BZ    SCHM042             No                                           
         TM    KYWIND1,KYWCDE      Code data ?                                  
         BZ    SCHM044             No, so get code                              
         B     SCHM045             Yes                                          
                                                                                
SCHM042  TM    KYWIND1,KYWNME      Name data ?                                  
         BO    SCHM045                                                          
                                                                                
SCHM044  SR    RE,RE                                                            
         ICM   RE,3,KYWLINK                                                     
         BZ    SCHM045                                                          
         L     RF,KYWBASE                                                       
         AR    RF,RE                                                            
         MVC   MPDDATA(2),KYWDD#                                                
                                                                                
SCHM045  LA    R2,MAPAWTH1                                                      
         OI    MPDIND,MPDIOFF      Set to turn off element for process          
         TM    ROWDAIND,ROWDAYMD   Date width is stored based on system         
         BO    SCHM048                                                          
         NI    MPDIND,TURNOFF-MPDIOFF  Yes, process element                     
         MVC   MPDDATA+1(1),KYWSRTQ+1                                           
*        MVC   MPDDATA+1(1),ROWKYSZ                                             
*        TM    ROWKYIND,ROWKYCDE                                                
*        BO    SCHM048                                                          
*        MVC   MPDDATA+1(1),ROWDASZ                                             
         DROP  RF                                                               
                                                                                
SCHM048  GOTOR PUTMAPEL,DMCB,MAPATRB1                                           
*                                                                               
         TM    ROWFLAGS,ROWBOTH    Code and Name ?                              
         BNO   SCHM060             No                                           
         LA    R2,MAPALBL2         Has to be name                               
         BRAS  RE,ROWLBL                                                        
         MVC   MPDDATA(5),WORK     Row number                                   
                                                                                
         LA    R2,MAPAID#2         *** ID# ***                                  
         ZIC   R1,ROWMAP#                                                       
         LH    RF,ATRB#                                                         
         AHI   RF,1                                                             
         STH   RF,ATRB#                                                         
         AH    R1,ATRB#                                                         
         STCM  R1,3,MPDDATA                                                     
                                                                                
         LA    R2,MAPAIDX          *** DDLINK indexing ***                      
         OI    MPDIND,MPDIOFF      Default to off                               
         TM    ROWDAIND,ROWIDXON                                                
         BZ    SCHM049                                                          
         NI    MPDIND,TURNOFF-MPDIOFF                                           
         AHI   R1,1                                                             
         STCM  R1,3,MPDDATA                                                     
                                                                                
SCHM049  LA    R2,MAPAOF           *** ATTRIBUTE OF ***                         
         MVC   MPDDATA(2),NODE#                                                 
*                                                                               
*        LA    R2,MAPACMP2         *** COMPRESSION BIT ***                      
*        ZIC   R1,ROWNUM                                                        
*        AHI   R1,1                                                             
*        STC   R1,MPDDATA+1                                                     
*                                                                               
         USING KYWD,RF                                                          
         L     RF,ROWINDEX         Point to keyword in table                    
         LA    R2,MAPAGID2         *** DATA DICTIONARY # ***                    
         SR    RE,RE                                                            
         MVC   MPDDATA(2),KYWDD#                                                
         TM    KYWIND1,KYWNME                                                   
         BO    SCHM050                                                          
         SR    RE,RE                                                            
         ICM   RE,3,KYWLINK                                                     
         BZ    SCHM050                                                          
         L     RF,KYWBASE                                                       
         AR    RF,RE                                                            
         MVC   MPDDATA(2),KYWDD#                                                
                                                                                
SCHM050  LA    R2,MAPAWTH2                                                      
         OI    MPDIND,MPDIOFF      Set to turn off element for process          
         TM    ROWDAIND,ROWDAYMD   Date width is stored based on system         
         BO    SCHM055                                                          
         NI    MPDIND,TURNOFF-MPDIOFF  Yes, process element                     
         MVC   MPDDATA+1(1),KYWSRTQ+1                                           
*        MVC   MPDDATA+1(1),ROWKYSZ                                             
*        TM    ROWKYIND,ROWKYNME                                                
*        BO    SCHM055                                                          
*        MVC   MPDDATA+1(1),ROWDASZ                                             
         DROP  RF                                                               
                                                                                
SCHM055  GOTOR PUTMAPEL,DMCB,MAPATRB2                                           
                                                                                
SCHM060  AHI   R3,ROWLNQ                                                        
         BCT   R0,SCHM032                                                       
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
* Process column amounts as atributes of last row                     *         
***********************************************************************         
         USING COLD,R3                                                          
SCHM090  L     R3,FMTCOL                                                        
         ST    R3,SVR3                                                          
         ZIC   R0,FMT#COLS                                                      
                                                                                
SCHM100  DS    0H                                                               
         L     R3,SVR3                                                          
         ZIC   RF,COLDDLSQ         Order of printing sequence                   
         BCTR  RF,0                                                             
         MHI   RF,COLLNQ                                                        
         A     RF,FMTCOL           Add base of columns table                    
         AHI   R3,COLLNQ                                                        
         ST    R3,SVR3             Set for next time around                     
         LR    R3,RF               Set to one we want to process                
                                                                                
         TM    COLIND2,COLRATE     Rate keyword                                 
*&&US*&& BO    SCHM110                                                          
*&&UK                                                                           
         BZ    SCHM104                                                          
         TM    DDLNKIND,DDLNKON+DDLNKTST    DDLINK running                      
         BZ    SCHM110             No                                           
         B     *+12                Yes, don't pass hidden cols                  
*&&                                                                             
SCHM104  TM    COLFLAGS,COLAMT     Amount of some sort?                         
         BZ    SCHM190                                                          
         TM    COLFLAGS,COLHIDE                                                 
         BO    SCHM190             Don't process hidden columns                 
                                                                                
         USING MPDELD,R2                                                        
SCHM110  LA    R2,MAPALBL3         *** LABEL ***  use keyword                   
         BRAS  RE,COLLBL                                                        
         MVC   MPDDATA(5),WORK     Row number                                   
                                                                                
         USING KYWD,RF                                                          
         L     RF,COLINDEX         Point to keyword in table                    
         MVC   MPDDATA+5(6),KYWCODE                                             
         DROP  RF                                                               
                                                                                
         LA    R2,MAPAID#3            *** ID# ***                               
         MVC   MPDDATA+1(1),COLMAP#                                             
*                                                                               
         LA    R2,MAPAOF3             *** ATTRIBUTE OF ***                      
         MVC   MPDDATA(2),NODE#                                                 
                                                                                
         USING KYWD,RF                                                          
         L     RF,COLINDEX                                                      
         LA    R2,MAPAGID3            *** DATA DICTIONARY # ***                 
         MVC   MPDDATA(2),KYWDD#                                                
         DROP  RF                                                               
                                                                                
         LA    R2,MAPATLVL            *** TOTAL LEVEL ***                       
         MVC   MPDDATA+1(1),COLTOTLV                                            
                                                                                
         GOTOR PUTMAPEL,DMCB,MAPATRB3                                           
                                                                                
SCHM190  BCT   R0,SCHM100                                                       
         DROP  R3                                                               
*---------------------------------------*                                       
* Build end part of Schema from table   *                                       
*---------------------------------------*                                       
                                                                                
         GOTOR PUTMAPEL,DMCB,MAPE001                                            
                                                                                
         XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* Set row number in character R##- in WORK(4)                                   
*     Pass      R2 = A(ROW)                                                     
*---------------------------------------------------------------------*         
         USING ROWD,R3                                                          
ROWLBL   MVC   WORK,SPACES                                                      
         MVI   WORK,C'R'           "R" for Row                                  
         ZIC   R1,ROWNUM                                                        
         SHI   R1,1                                                             
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  WORK+1(3),DUB                                                    
         MVI   WORK+4,C'-'                                                      
         BR    RE                                                               
         DROP  R3                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* Set column number in character C##- in WORK(4)                                
*     Pass      R2 = A(Column)                                                  
*---------------------------------------------------------------------*         
                                                                                
         USING COLD,R3                                                          
COLLBL   MVC   WORK,SPACES                                                      
         MVI   WORK,C'C'           "C" for Row                                  
         ZIC   R1,COLMAP#                                                       
         BCTR  R1,0                                                             
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  WORK+1(3),DUB                                                    
         MVI   WORK+4,C'-'                                                      
         BR    RE                                                               
         DROP  R3                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* Convert with no sign number to string                                         
*     Pass     R1 = Number to convert                                           
*     Return   RF = A(Converted number)                                         
*              R1 = Length                                                      
*---------------------------------------------------------------------*         
NUM2STR  CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  WORK(11),DUB                                                     
         LA    R1,11                                                            
         LA    RF,WORK                                                          
NUM2STR3 CLI   0(RF),C'0'                                                       
         BNER  RE                                                               
         AHI   RF,1                                                             
         BRCT  R1,NUM2STR3                                                      
         AHI   R1,1                Return 1 for number zero                     
         SHI   RF,1                Bump back one for C'0'                       
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
* Routine values                                                                
***********************************************************************         
NODE#    DS    H                                                                
ATRB#    DS    H                                                                
PARENT   DS    H                                                                
NEWNODE  DS    CL1                 Yes / No                                     
NEWLINE  DS    CL1                                                              
DBREAK   DS    CL1                 Yes / No                                     
WOUTACT  DS    CL1                                                              
LSTSECOF DS    CL2                 Last security office                         
WOUTMASK DS    XL16                128 bit mask, repeat indicator mask          
         EJECT ,                                                                
***********************************************************************         
* Table for building schema                                                     
***********************************************************************         
MAPH001  DC    AL1(MPCELQ,MPCLNQ),AL2(REC_NODE)                 LVL 1           
         DC    AL1(MPDELQ,MPDLNQ+3,NDE_LBL,MPD_CHRR,0),C'Doc'                   
         DC    AL1(MPDELQ,MPDLNSQ,NDE_ID,MPD_SML,MPDRAW),AL2(9999)              
         DC    AL1(EOR)                                                         
*                                                                               
* Map data start node                                                           
*                                                                               
MAPD001  DC    AL1(MPCELQ,MPCLNQ),AL2(REC_NODE)                                 
         DC    AL1(MPDELQ,MPDLNQ+6,NDE_LBL,MPD_CHRR,0),C'Scribe'                
         DC    AL1(MPDELQ,MPDLNFQ,NDE_ID,MPD_FUL,MPDRAW),AL4(REC_DATA)          
         DC    AL1(MPDELQ,MPDLNQ,NDE_TOP,MPD_BIN,0)                             
         DC    AL1(EOR)                                                         
*                                                                               
* Map data for end of schema                                                    
*                                                                               
MAPE001  DC    AL1(MPCELQ,MPCLNQ),AL2(REC_TERM)                 END 2           
         DC    AL1(MPCELQ,MPCLNQ),AL2(REC_TERM)                 END 1           
         DC    AL1(EOR)                                                         
*                                                                               
* Map data for row data (Template like)                                         
*                                                                               
MAPATRB1 DC    AL1(MPCELQ,MPCLNQ),AL2(REC_ATTR)                                 
MAPALBL  DC    AL1(MPDELQ,MPDLNQ+11,ATR_LBL,MPD_CHRR,0),CL11' '                 
MAPAID#  DC    AL1(MPDELQ,MPDLNSQ,ATR_ID,MPD_SML,MPDRAW),AL2(0)                 
MAPATYP  DC    AL1(MPDELQ,MPDLNQ+1,ATR_TYPE,MPD_CHRR,0,MPD_CHR)                 
MAPANODE DC    AL1(MPDELQ,MPDLNSQ,ATR_NODE,MPD_SML,MPDRAW),AL2(0)               
MAPAPRNT DC    AL1(MPDELQ,MPDLNSQ,ATR_PRNT,MPD_SML,MPDRAW),AL2(0)               
MAPAGID# DC    AL1(MPDELQ,MPDLNSQ,ATR_GID#,MPD_SML,MPDRAW),AL2(0)               
MAPAWTH1 DC    AL1(MPDELQ,MPDLNSQ,ATR_WDTH,MPD_SML,MPDRAW),AL2(0)               
         DC    AL1(EOR)                                                         
                                                                                
MAPATRB2 DC    AL1(MPCELQ,MPCLNQ),AL2(REC_ATTR)                                 
MAPALBL2 DC    AL1(MPDELQ,MPDLNQ+11,ATR_LBL,MPD_CHRR,0),CL11'     Name'         
MAPAID#2 DC    AL1(MPDELQ,MPDLNSQ,ATR_ID,MPD_SML,MPDRAW),AL2(0)                 
MAPAIDX  DC    AL1(MPDELQ,MPDLNSQ,ATR_DIDX,MPD_SML,MPDRAW),AL2(0)               
MAPATYP2 DC    AL1(MPDELQ,MPDLNQ+1,ATR_TYPE,MPD_CHRR,0,MPD_CHR)                 
MAPAOF   DC    AL1(MPDELQ,MPDLNSQ,ATR_ATRB,MPD_SML,MPDRAW),AL2(0)               
MAPAGID2 DC    AL1(MPDELQ,MPDLNSQ,ATR_GID#,MPD_SML,MPDRAW),AL2(0)               
MAPAWTH2 DC    AL1(MPDELQ,MPDLNSQ,ATR_WDTH,MPD_SML,MPDRAW),AL2(0)               
         DC    AL1(EOR)                                                         
                                                                                
MAPATRB3 DC    AL1(MPCELQ,MPCLNQ),AL2(REC_ATTR)                                 
MAPALBL3 DC    AL1(MPDELQ,MPDLNQ+11,ATR_LBL,MPD_CHRR,0),CL11'     Name'         
MAPAID#3 DC    AL1(MPDELQ,MPDLNSQ,ATR_ID,MPD_SML,MPDRAW),AL2(0)                 
         DC    AL1(MPDELQ,MPDLNQ+1,ATR_TYPE,MPD_CHRR,0,MPD_FUL)                 
MAPAOF3  DC    AL1(MPDELQ,MPDLNSQ,ATR_ATRB,MPD_SML,MPDRAW),AL2(0)               
MAPAGID3 DC    AL1(MPDELQ,MPDLNSQ,ATR_GID#,MPD_SML,MPDRAW),AL2(0)               
MAPATLVL DC    AL1(MPDELQ,MPDLNSQ,ATR_TLVL,MPD_SML,MPDRAW),AL2(0)               
         DC    AL1(EOR)                                                         
                                                                                
         TITLE 'Build Report definition data'                                   
***********************************************************************         
* Build Definition data base of Format                                *         
*       R5 = A(Current format)                                        *         
*       R6 = A(LP_D)               Interface for DDLINKIO             *         
***********************************************************************         
         USING FMTRECD,R5                                                       
         USING LP_D,R6                                                          
RPTDEF   NTR1  BASE=*,LABEL=*                                                   
         GOTOR PUTMAPEL,DMCB,A(DATAEL)                                          
                                                                                
         L     R2,AIO2                                                          
         LR    RE,R2               CLEAR OUT AIO2                               
         LHI   RF,IOSIZEQ*10                                                    
         XCEFL                                                                  
                                                                                
         MVI   SEQPOS#2,0                                                       
         XC    SEQPOS#L,SEQPOS#L                                                
         XC    SEQPOS#R,SEQPOS#R                                                
         XC    SEQPOS#C,SEQPOS#C                                                
         MVI   COLCOUNT,0                                                       
*                                                                               
* Report definition X'FE12'                                                     
*                                                                               
         USING MPCELD,R2                                                        
         MVI   MPCEL,MPCELQ        Map code                                     
         MVI   MPCLN,MPCLNQ                                                     
         MVC   MPCID#,=AL2(REC_RDEF)                                            
         AHI   R2,MPCLNQ                                                        
*                                  Report type                                  
         USING MPDELD,R2                                                        
         MVI   MPDEL,MPDELQ        Map code data                                
         MVI   MPDLN,MPDLNQ+1                                                   
         MVI   MPDID#,1                                                         
         MVI   MPDIND,0                                                         
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVC   MPDDATA(1),QPROG    Report type                                  
         AHI   R2,MPDLNQ+1                                                      
*                                  Format code                                  
         MVI   MPDEL,MPDELQ        Map code data                                
         MVI   MPDLN,MPDLNQ+L'FMTCODE                                           
         MVI   MPDID#,2                                                         
         MVI   MPDIND,0                                                         
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVC   MPDDATA(L'FMTCODE),FMTCODE                                       
         AHI   R2,MPDLNQ+L'FMTCODE                                              
*                                  Format name                                  
         CLC   FMTNAME,SPACES                                                   
         BNH   RPTDEF12                                                         
         MVI   MPDEL,MPDELQ        Map code data                                
         MVI   MPDLN,MPDLNQ+L'FMTNAME                                           
         MVI   MPDID#,3                                                         
         MVI   MPDIND,0                                                         
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVC   MPDDATA(L'FMTNAME),FMTNAME                                       
         AHI   R2,MPDLNQ+L'FMTNAME                                              
                                                                                
         USING GRPNMED,R4                                                       
RPTDEF12 L     R4,AGRPNMES         Group name table                             
         SR    R0,R0               Group number                                 
RPTDEF13 CLI   0(R4),EOT           End of table                                 
         BE    RPTDEF15            Yes                                          
         MVI   GRPNEWLV,0          Reset                                        
         CLI   GRPNMEON,YES        Format has this one?                         
         BNE   RPTDEF14            No                                           
         AHI   R0,1                Group number                                 
         STC   R0,GRP#HIGH         High water mark                              
         STC   R0,GRPNEWLV         Set new group number                         
         MVI   GRPNMEON,NO         Reset                                        
         MVI   MPDEL,MPDELQ        Map code data                                
         MVI   MPDLN,MPDLNQ+39                                                  
         MVI   MPDID#,4                                                         
         MVI   MPDIND,0                                                         
         MVI   MPDTYPE,MPD_CHRR                                                 
         XC    MPDDATA(39),MPDDATA                                              
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MPDDATA(3),DUB                                                   
         MVI   MPDDATA+3,ESC#LFJT    Left justify                               
         MVC   MPDDATA+4(2),GRPNMDD#                                            
         MVI   MPDDATA+6,36        Max length                                   
         GOTO1 ADDICTAT,DMCB,C'SL  ',MPDDATA+3,0                                
         ZIC   RF,MPDLN                                                         
         AR    R2,RF                                                            
                                                                                
RPTDEF14 AHI   R4,GRPNMLNQ                                                      
         B     RPTDEF13                                                         
         DROP  R4                                                               
*                                                                               
* Title and Center headings                                                     
*                                                                               
         USING HEADD,R3                                                         
RPTDEF15 L     R3,FMTHEAD          A(Heading table info)                        
         SR    R0,R0                                                            
         ICM   R0,1,FMT#HEAD       Number of headings                           
         BZ    RPTDEF20                                                         
         MVI   BYTE,NO                                                          
*                                                                               
* Report element X'FE31'                                                        
*                                                                               
RPTDEF16 TM    HEADFLAG,HEADFREE                                                
         BZ    RPTDEF18                                                         
         BRAS  RE,PUTHEAD                                                       
                                                                                
RPTDEF18 AHI   R3,HEADLNQ                                                       
         BCT   R0,RPTDEF16                                                      
         DROP  R3                                                               
                                                                                
         USING ROWD,R3                                                          
RPTDEF20 L     R3,FMTROW                                                        
         IC    R0,FMT#ROWS                                                      
         AHI   R3,ROWLNQ           Skip first row                               
         SHI   R0,1                                                             
         BNP   RPTDEF66                                                         
                                                                                
         USING COLD,R4                                                          
RPTDEF32 CLI   ROWTYPE,ROWCOL      Is it a column ?                             
         JNE   RPTDEF34                                                         
         ICM   R4,15,ROWACOL       A(Column info)                               
         BNZ   *+6                                                              
         DC    H'00'                                                            
                                                                                
         TM    ROWXPIND,ROWRATE    Rate keyword                                 
         BO    RPTDEF65                                                         
         TM    COLFLAGS,COLHIDE    Skip hidden columns                          
         BO    RPTDEF65                                                         
                                                                                
RPTDEF34 BAS   RE,DFELM            Define element                               
         BAS   RE,DFID#            Define ID#                                   
         ZIC   R1,ROWMAP#                                                       
         BRAS  RE,DFATRB           Define attribute - map code #                
         BAS   RE,DFTYPE           Define row type                              
         MVI   SUBTYPE,SBTCODE                                                  
         TM    ROWDAIND,ROWDAYMD   Date width is stored based on system         
         BZ    *+8                                                              
         MVI   SUBTYPE,SBTDATE     Date type                                    
         BRAS  RE,DFSBTY           Define sub-type                              
         BRAS  RE,DFPPTY           Define properties                            
         BRAS  RE,DFGRPI           Define group infomation                      
         CLI   ROWTYPE,ROWMID      Mid-line?                                    
         BE    RPTDEF40            Skip for mid-line                            
         MVI   SEQPOS#2,C'1'                                                    
         BRAS  RE,DFPOS            Define row postion L,C,R or column           
*                                                                               
* Row    then prefix if any                                                     
* Column then column heading 1                                                  
*                                                                               
RPTDEF40 MVI   MPDEL,MPDELQ                                                     
         MVI   MPDID#,ELM_HD1                                                   
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVI   MPDIND,0                                                         
         SR    RF,RF                                                            
         CLI   ROWTYPE,ROWCOL                                                   
         BE    RPTDEF42                                                         
         ICM   RE,15,ROWPRFX       A(Row prefix)                                
         BZ    RPTDEF50                                                         
         ICM   RF,1,ROWPRFX        Length in high order byte                    
         B     RPTDEF45                                                         
                                                                                
RPTDEF42 ICM   R4,15,ROWACOL                                                    
         BZ    RPTDEF50                                                         
         L     RE,COLHEAD1                                                      
         ICM   RF,1,COLHEAD1                                                    
         BZ    RPTDEF50                                                         
                                                                                
RPTDEF45 BCTR  RF,0                                                             
         EXMVC RF,MPDDATA,0(RE)                                                 
         LA    RF,MPDLNQ+1(,RF)    RF = Length of element                       
         STC   RF,MPDLN                                                         
         AR    R2,RF                                                            
                                                                                
RPTDEF50 CLI   ROWTYPE,ROWCOL                                                   
         BNE   RPTDEF52                                                         
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDID#,ELM_HD2                                                   
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVI   MPDIND,0                                                         
         SR    RF,RF                                                            
         ICM   RE,15,COLHEAD2                                                   
         BZ    RPTDEF52                                                         
         IC    RF,COLHEAD2                                                      
         SHI   RF,1                                                             
         BM    RPTDEF51            No heading 2                                 
         EXCLC RF,0(RE),SPACES                                                  
         BNH   RPTDEF51                                                         
         EXMVC RF,MPDDATA,0(RE)                                                 
         LA    RF,MPDLNQ+1(,RF)    RF = Length of element                       
         STC   RF,MPDLN                                                         
         AR    R2,RF                                                            
*                                                                               
RPTDEF51 MVI   MPDEL,MPDELQ                                                     
         MVI   MPDID#,ELM_HD3                                                   
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVI   MPDIND,0                                                         
         SR    RF,RF                                                            
         ICM   RE,15,COLHEAD3                                                   
         BZ    RPTDEF52                                                         
         IC    RF,COLHEAD3                                                      
         SHI   RF,1                                                             
         BM    RPTDEF52            No heading 3                                 
         EXCLC RF,0(RE),SPACES                                                  
         BNH   RPTDEF52                                                         
         EXMVC RF,MPDDATA,0(RE)                                                 
         LA    RF,MPDLNQ+1(,RF)    RF = Length of element                       
         STC   RF,MPDLN                                                         
         AR    R2,RF                                                            
         DROP  R4                                                               
                                                                                
*PTDEF52 ZIC   R1,ROWKYSZ                                                       
*        TM    ROWKYIND,ROWKYCDE   Row code key ?                               
*        BZ    *+8                                                              
*        IC    R1,ROWDASZ                                                       
*        BRAS  RE,DFWDTH           Define sort row width                        
                                                                                
RPTDEF52 OC    ROWPRFX,ROWPRFX     A(Row prefix)                                
         JNZ   *+8                                                              
         BRAS  RE,DFDESC           Define description                           
                                                                                
         TM    ROWFLAGS,ROWBOTH                                                 
         BNO   RPTDEF65                                                         
         BAS   RE,DFELM            Define element                               
         BAS   RE,DFID#            Define ID#                                   
         ZIC   R1,ROWMAP#                                                       
         AHI   R1,1                                                             
         BRAS  RE,DFATRB           Define attribute - map code #                
         BAS   RE,DFTYPE           Define row type                              
         MVI   SUBTYPE,SBTNAME                                                  
         BRAS  RE,DFSBTY           Define sub-type                              
         BRAS  RE,DFPPTY           Define properties                            
         CLI   ROWTYPE,ROWMID      Mid-line?                                    
         BE    RPTDEF65            Skip for mid-line                            
         MVI   SEQPOS#2,C'2'                                                    
         BRAS  RE,DFPOS            Define row postion L,C,R or column           
                                                                                
*PTDEF54 ZIC   R1,ROWDASZ                                                       
*        TM    ROWKYIND,ROWKYCDE   Row code key ?                               
*        BZ    *+8                                                              
*        IC    R1,ROWKYSZ                                                       
*        BRAS  RE,DFWDTH           Define sort row width                        
                                                                                
RPTDEF65 DS    0H                                                               
         AHI   R3,ROWLNQ                                                        
         BCT   R0,RPTDEF32                                                      
                                                                                
         USING COLD,R4                                                          
RPTDEF66 L     R4,FMTCOL                                                        
         ST    R4,SVR4                                                          
         IC    R0,FMT#COLS                                                      
RPTDEF68 L     R4,SVR4                                                          
         ZIC   RF,COLDDLSQ         Order printing sequence                      
         BCTR  RF,0                                                             
         MHI   RF,COLLNQ                                                        
         A     RF,FMTCOL           Add base of column table                     
         AHI   R4,COLLNQ                                                        
         ST    R4,SVR4             Set for next time around                     
         LR    R4,RF                                                            
                                                                                
         TM    COLIND2,COLRATE     Rate keyword                                 
*&&US*&& BO    RPTDEF7X                                                         
*&&UK                                                                           
         BZ    RPTDEF70                                                         
         TM    DDLNKIND,DDLNKON+DDLNKTST    DDLINK running                      
         BZ    RPTDEF7X            No                                           
         B     *+12                Yes, don't pass hidden cols                  
*&&                                                                             
RPTDEF70 TM    COLFLAGS,COLAMT                                                  
         BZ    RPTDEF80                                                         
         TM    COLFLAGS,COLHIDE                                                 
         BO    RPTDEF80            Skip hidden columns                          
                                                                                
RPTDEF7X BAS   RE,DFELM                                                         
         BAS   RE,DFID#                                                         
         ZIC   R1,COLMAP#                                                       
         BRAS  RE,DFATRB                                                        
*                                                                               
*  Column then C                                                                
*                                                                               
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDID#,ELM_TYPE                                                  
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVI   MPDIND,0                                                         
         MVI   MPDLN,MPDLNQ+1                                                   
         MVI   MPDDATA,C'C'        Yes                                          
         AHI   R2,MPDLNQ+1                                                      
*                                                                               
*  Column sub type                                                              
*                                                                               
         MVI   SUBTYPE,SBTAMNT     Amount type                                  
         BRAS  RE,DFSBTY           Define sub-type                              
         BRAS  RE,DFPPTY                                                        
*                                                                               
*  Column displayed positioning value                                           
*                                                                               
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDID#,ELM_POS                                                   
         MVI   MPDTYPE,MPD_CHRR                                                 
         ZIC   R1,COLSEQ#                                                       
         BRAS  RE,NUM2STR          R1=value, return RF=len                      
         EXMVC R1,MPDDATA,0(RF)                                                 
         AHI   R1,MPDLNQ                                                        
         STC   R1,MPDLN                                                         
         AR    R2,R1               Add data length                              
*                                                                               
* If soft headings then go build now                                            
*                                                                               
         TM    FMTIND,FMTSFTH      Soft headings ?                              
         BZ    RPTDEF71                                                         
         GOTOR SOFTHEAD,DMCB,(0,COLD),FMTRECD                                   
*                                                                               
* Column heading 1                                                              
*                                                                               
RPTDEF71 MVI   MPDEL,MPDELQ                                                     
         MVI   MPDID#,ELM_HD1                                                   
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVI   MPDIND,0                                                         
         SR    RF,RF                                                            
         ICM   RE,15,COLHEAD1                                                   
         BZ    RPTDEF72                                                         
         ICM   RF,1,COLHEAD1                                                    
         BZ    RPTDEF72                                                         
         BCTR  RF,0                                                             
         EXCLC RF,0(RE),SPACES                                                  
         BNH   RPTDEF72                                                         
         EXMVC RF,MPDDATA,0(RE)                                                 
         LA    RF,MPDLNQ+1(,RF)    RF = Length of element                       
         STC   RF,MPDLN                                                         
         AR    R2,RF                                                            
*                                                                               
* Column heading 2                                                              
*                                                                               
RPTDEF72 MVI   MPDEL,MPDELQ                                                     
         MVI   MPDID#,ELM_HD2                                                   
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVI   MPDIND,0                                                         
         SR    RF,RF                                                            
         ICM   RE,15,COLHEAD2                                                   
         BZ    RPTDEF73                                                         
         ICM   RF,1,COLHEAD2                                                    
         BZ    RPTDEF73                                                         
         BCTR  RF,0                                                             
         EXCLC RF,0(RE),SPACES                                                  
         BNH   RPTDEF73                                                         
         EXMVC RF,MPDDATA,0(RE)                                                 
         AHI   RF,MPDLNQ+1         RF = Length of element                       
         STC   RF,MPDLN                                                         
         AR    R2,RF                                                            
*                                                                               
* Column heading 3                                                              
*                                                                               
RPTDEF73 MVI   MPDEL,MPDELQ                                                     
         MVI   MPDID#,ELM_HD3                                                   
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVI   MPDIND,0                                                         
         SR    RF,RF                                                            
         ICM   RE,15,COLHEAD3                                                   
         BZ    RPTDEF74                                                         
         ICM   RF,1,COLHEAD3                                                    
         BZ    RPTDEF74                                                         
         BCTR  RF,0                                                             
         EXCLC RF,0(RE),SPACES                                                  
         BNH   RPTDEF74                                                         
         EXMVC RF,MPDDATA,0(RE)                                                 
         AHI   RF,MPDLNQ+1         RF = Length of element                       
         STC   RF,MPDLN                                                         
         AR    R2,RF                                                            
*&&DO                                                                           
RPTDEFXX MVI   MPDEL,MPDELQ        Total Level                                  
         MVI   MPDID#,ELM_TOTL                                                  
         MVI   MPDTYPE,MPD_SML                                                  
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDLN,MPDLNSQ                                                    
         XC    MPDDSML,MPDDSML                                                  
         MVC   MPDDSML+1(1),COLTOTLV                                            
         AHI   R2,MPDLNSQ                                                       
*&&                                                                             
RPTDEF74 MVI   MPDEL,MPDELQ        Number of decimals                           
         MVI   MPDID#,ELM_DCML                                                  
         MVI   MPDTYPE,MPD_SML                                                  
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDLN,MPDLNSQ                                                    
         XC    MPDDSML,MPDDSML                                                  
         MVC   MPDDSML+1(1),COLDCMLS                                            
         AHI   R2,MPDLNSQ                                                       
                                                                                
         CLI   COLCLVL,1           Any level assigned ?                         
         BL    RPTDEF80            No                                           
         MVI   MPDEL,MPDELQ        Level of column expand/colapse               
         MVI   MPDID#,ELM_CLVL                                                  
         MVI   MPDTYPE,MPD_SML                                                  
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDLN,MPDLNSQ                                                    
         XC    MPDDSML,MPDDSML                                                  
         MVC   MPDDSML+1(1),COLCLVL                                             
         AHI   R2,MPDLNSQ                                                       
                                                                                
RPTDEF80 DS    0H                                                               
         BCT   R0,RPTDEF68                                                      
         DROP  R4                                                               
*&&DO                                                                           
         USING MPCELD,R2                                                        
         MVI   MPCEL,MPCELQ        Map code                                     
         MVI   MPCLN,MPCLNQ                                                     
         MVC   MPCID#,=AL2(REC_RDFN) Report definition end                      
         AHI   R2,MPCLNQ                                                        
         MVI   0(R2),EOR                                                        
*&&                                                                             
         L     RF,AIO2                                                          
         SR    R2,RF                                                            
         AHI   R2,1                Add one for EOR                              
         CHI   R2,IOSIZEQ*10       Record built larger then IO area             
         BNH   *+6                 No                                           
         DC    H'00'               Yes                                          
         DROP  R2                                                               
                                                                                
         GOTOR PUTMAPEL,DMCB,AIO2                                               
                                                                                
RPTDEFX  XIT1                                                                   
         EJECT ,                                                                
*                                                                               
* Map node of element                                                           
*                                                                               
         USING MPCELD,R2                                                        
DFELM    MVI   MPCEL,MPCELQ        Map code                                     
         MVI   MPCLN,MPCLNQ                                                     
         MVC   MPCID#,=AL2(REC_ELMD)                                            
         AHI   R2,MPCLNQ                                                        
         BR    RE                                                               
*                                                                               
* Node id - always the same since everything is an atribute remapped            
*                                                                               
         USING MPDELD,R2                                                        
DFID#    MVI   MPDEL,MPDELQ                                                     
         MVI   MPDID#,ELM_ID                                                    
         MVI   MPDTYPE,MPD_SML                                                  
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDLN,MPDLNSQ                                                    
         XC    MPDDSML,MPDDSML                                                  
         MVC   MPDDATA(2),=AL2(REC_DATA)                                        
         AHI   R2,MPDLNSQ                                                       
         BR    RE                                                               
*                                                                               
*  Sort width of row data                                                       
*                                                                               
*FWDTH   MVI   MPDEL,MPDELQ                                                     
*        MVI   MPDID#,ELM_WDTH                                                  
*        MVI   MPDTYPE,MPD_SML                                                  
*        MVI   MPDIND,MPDRAW                                                    
*        MVI   MPDLN,MPDLNSQ                                                    
*        XC    MPDDSML,MPDDSML                                                  
*        STCM  R1,3,MPDDATA                                                     
*        AHI   R2,MPDLNSQ                                                       
*        BR    RE                                                               
*                                                                               
*  Attibute sub type                                                            
*                                                                               
DFSBTY   MVI   MPDEL,MPDELQ                                                     
         MVI   MPDID#,ELM_SBTY                                                  
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVI   MPDIND,0                                                         
         MVI   MPDLN,MPDLNQ+1                                                   
         MVC   MPDDATA,SUBTYPE                                                  
         CLI   SUBTYPE,SBTDATE                                                  
         JNE   DFSBTY10                                                         
         MVI   MPDLN,MPDLNQ+1+L'SBTYMD                                          
         MVC   MPDDATA+1(L'SBTYMD),SBTYMD                                       
         J     DFSBTY40                                                         
                                                                                
         USING COLD,R4                                                          
DFSBTY10 CLI   SUBTYPE,SBTAMNT     Amount column ?                              
         JNE   DFSBTY40                                                         
         TM    COLIND4,COLCONST    Constant                                     
         BO    *+8                 Yes                                          
         TM    COLFLAGS,COLCALC                                                 
         JZ    DFSBTY18                                                         
         ICM   R1,15,COLEQUTN      Column equation                              
         JZ    DFSBTY18            None                                         
         IC    RF,0(,R1)           Length of data                               
         MVC   MPDDATA(MAXEQLN),1(R1)                                           
         LA    RF,MPDLNQ(RF)                                                    
         STC   RF,MPDLN            Save off length of data                      
         J     DFSBTY40                                                         
                                                                                
DFSBTY18 TM    COLEDOPT,COLPCT                                                  
         JZ    DFSBTY40                                                         
         MVI   MPDDATA+1,C'%'      Percentage                                   
         MVI   MPDLN,MPDLNQ+2                                                   
         DROP  R4                                                               
                                                                                
DFSBTY40 ZIC   RF,MPDLN                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
*  Attibute properties                                                          
*                                                                               
         USING COLD,R4                                                          
DFPPTY   CLI   SUBTYPE,SBTAMNT     Amount type                                  
         JNE   DFPPTY10                                                         
         MVI   BYTE,0                                                           
         TM    COLIND4,COL2HIDE    Hidden column                                
         JZ    *+8                                                              
         OI    BYTE,PPTHIDE        Hide code for user                           
         TM    COLOPT2,COLREVSG    This is one to potentially rev               
         JZ    *+8                                                              
         OI    BYTE,PPTREVSG       Let PC know this is one                      
         CLI   COLTOTLV,X'FF'                                                   
         JNE   *+8                                                              
         OI    BYTE,PPTNOTOT       No totaling                                  
         CLI   BYTE,0                                                           
         BER   RE                                                               
         J     DFPPTY30                                                         
                                                                                
DFPPTY10 MVI   BYTE,PPTSORTA       Sort accending  (default)                    
         TM    ROWKYIND,ROWKYCDE+ROWKYNME                                       
         BZR   RE                                                               
         CLI   ROWTYPE,ROWCOL                                                   
         JNE   DFPPTY12                                                         
         TM    COLIND4,COL2HIDE    Hidden column                                
         JZ    *+8                                                              
         OI    BYTE,PPTHIDE        Hide code for user                           
                                                                                
DFPPTY12 CLI   SUBTYPE,SBTCODE                                                  
         JNE   DFPPTY20                                                         
         TM    ROWDAIND,ROWNOCDE   Code is used for sort only                   
         BZ    *+8                                                              
         OI    BYTE,PPTHIDE        Hide code for user                           
         TM    ROWKYIND,ROWKYCDE                                                
         BZR   RE                                                               
         J     DFPPTY30                                                         
                                                                                
DFPPTY20 CLI   SUBTYPE,SBTNAME                                                  
         JNE   DFPPTY30                                                         
         TM    ROWKYIND,ROWKYNME                                                
         BZR   RE                                                               
         DROP  R4                                                               
                                                                                
DFPPTY30 MVI   MPDEL,MPDELQ                                                     
         XC    MPDDFUL,MPDDFUL                                                  
         MVI   MPDID#,ELM_PPTY                                                  
         MVI   MPDTYPE,MPD_FUL                                                  
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDLN,MPDLNFQ                                                    
         MVC   MPDDFUL+3(1),BYTE  Sort accending                                
         ZIC   RF,MPDLN                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
*  Attibute map number                                                          
*                                                                               
DFATRB   MVI   MPDEL,MPDELQ                                                     
         MVI   MPDID#,ELM_ATRB                                                  
         MVI   MPDTYPE,MPD_SML                                                  
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDLN,MPDLNSQ                                                    
         XC    MPDDSML,MPDDSML                                                  
         STCM  R1,3,MPDDATA                                                     
         AHI   R2,MPDLNSQ                                                       
         BR    RE                                                               
*                                                                               
*  If Row then Row or Midline. If column then C                                 
*                                                                               
DFTYPE   MVI   MPDEL,MPDELQ                                                     
         MVI   MPDID#,ELM_TYPE                                                  
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVI   MPDIND,0                                                         
         MVI   MPDLN,MPDLNQ+1                                                   
         MVI   MPDDATA,C'R'        Set to Row                                   
         CLI   ROWTYPE,ROWMID      Mid-line?                                    
         JNE   *+8                                                              
         MVI   MPDDATA,C'M'        Yes                                          
         CLI   ROWTYPE,ROWCOL      Row from column ?                            
         JNE   *+8                                                              
         MVI   MPDDATA,C'C'        Yes                                          
         CLI   ROWTYPE,ROWCOL                                                   
         JE    DFTYPE10                                                         
         TM    ROWFLAGS,ROWTOTAL                                                
         JZ    DFTYPE10                                                         
         MVI   MPDDATA,C'G'                                                     
                                                                                
DFTYPE10 AHI   R2,MPDLNQ+1                                                      
         BR    RE                                                               
*                                                                               
*  Row position in head line, left / center/ right                              
*  Column displayed positioning value                                           
*                                                                               
DFPOS    MVI   MPDEL,MPDELQ                                                     
         MVI   MPDID#,ELM_POS                                                   
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVI   MPDIND,0                                                         
         MVI   MPDLN,MPDLNQ+6                                                   
         MVC   MPDDATA,ROWTYPE                                                  
         CLI   ROWTYPE,ROWLEFT     Left heading                                 
         JNE   DFPOS02                                                          
         LH    R1,SEQPOS#L                                                      
         AHI   R1,1                                                             
         STH   R1,SEQPOS#L                                                      
                                                                                
DFPOS02  CLI   ROWTYPE,ROWRGHT     Right heading                                
         JNE   DFPOS04                                                          
         LH    R1,SEQPOS#R                                                      
         AHI   R1,1                                                             
         STH   R1,SEQPOS#R                                                      
                                                                                
DFPOS04  CLI   ROWTYPE,ROWCNTR     Center heading                               
         JNE   DFPOS06                                                          
         LH    R1,SEQPOS#C                                                      
         AHI   R1,1                                                             
         STH   R1,SEQPOS#C                                                      
                                                                                
DFPOS06  MVI   MPDDATA+1,C','                                                   
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MPDDATA+2(2),DUB                                                 
         MVI   MPDDATA+4,C','                                                   
         MVC   MPDDATA+5(1),SEQPOS#2                                            
                                                                                
         USING COLD,R4                                                          
         CLI   ROWTYPE,ROWCOL      Row from column ?                            
         JNE   DFPOS10                                                          
         ICM   R4,15,ROWACOL       A(Column info)                               
         ST    RE,SVRE                                                          
         ZIC   R1,COLSEQ#                                                       
         BRAS  RE,NUM2STR          R1 = len of str, RF = A(STR)                 
         BCTR  R1,0                                                             
         EXMVC R1,MPDDATA,0(RF)                                                 
         AHI   R1,MPDLNQ+1                                                      
         STC   R1,MPDLN                                                         
         L     RE,SVRE                                                          
                                                                                
DFPOS10  ZIC   RF,MPDLN                                                         
         AR    R2,RF                                                            
         MVI   0(R2),EOR                                                        
         BR    RE                                                               
*                                                                               
*  Group keywords information                                                   
*                                                                               
         USING GRPKYWD,RF                                                       
DFGRPI   SR    RF,RF                                                            
         ST    R4,SVR4                                                          
         ICM   RF,1,ROWGPIDX       Any ?                                        
         BZR   RE                  No                                           
         CLI   ROWGPIDX,GRP#NONE   Specail case                                 
         JL    DFGRP10             No                                           
         LR    R1,RF                                                            
         SHI   R1,GRP#NONE         Yes                                          
         ZIC   R4,GRP#HIGH                                                      
         AR    R1,R4                                                            
                                                                                
DFGRP10  MVI   MPDEL,MPDELQ                                                     
         MVI   MPDID#,ELM_GRPI                                                  
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVI   MPDIND,0                                                         
         MVI   MPDLN,MPDLNQ+6                                                   
         CLI   ROWGPIDX,GRP#NONE   Specail case                                 
         JNL   DFGRP20             R1 already has group #                       
         BCTR  RF,0                                                             
         MHI   RF,GRPKYLNQ                                                      
         A     RF,AGRPTAB                                                       
                                                                                
         USING GRPNMED,R4                                                       
         SR    R4,R4                                                            
         IC    R4,GRPKYW#                                                       
         BCTR  R4,0                                                             
         MHI   R4,GRPNMLNQ         Find name entry                              
         A     R4,AGRPNMES         Group name table                             
         ZIC   R1,GRPNEWLV         New group number (re-sequence)               
         DROP  R4                                                               
                                                                                
DFGRP20  CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MPDDATA(3),DUB                                                   
         LA    R1,1                Always one if non-group                      
         CLI   ROWGPIDX,GRP#NONE   Specail case                                 
         JNL   *+8                                                              
         IC    R1,GRPKYWL                                                       
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MPDDATA+3(3),DUB                                                 
         ZIC   RF,MPDLN                                                         
         AR    R2,RF                                                            
         MVI   0(R2),EOR                                                        
         L     R4,SVR4                                                          
         BR    RE                                                               
         DROP  RF                                                               
***********************************************************************         
* Define discription that shows in reporting levels in ACCENT                   
***********************************************************************         
         USING KYWD,RF                                                          
DFDESC   ST    RE,SVRE                                                          
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDID#,ELM_DESC                                                  
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVI   MPDIND,0                                                         
         MVI   MPDLN,MPDLNQ+DESCLNQ Try 14 for now                              
         MVI   MPDDATA,ESC#LFJT     Left justify                                
         MVI   MPDDATA+3,DESCLNQ    Max length                                  
         LHI   R1,AC#UNKWN          Unknown                                     
         ICM   RF,15,ROWINDEX                                                   
         JZ    DFDESC10                                                         
         OC    KYWDD#,KYWDD#       Was this set ?                               
         JZ    DFDESC10            No                                           
         ICM   R1,3,KYWDD#         Dictionary number                            
                                                                                
DFDESC10 STCM  R1,3,MPDDATA+1                                                   
         GOTOR ADDICTAT,DMCB,C'SL  ',MPDDATA,0                                  
         ZIC   RF,MPDLN                                                         
         AR    R2,RF                                                            
         MVI   0(R2),EOR                                                        
         L     RE,SVRE                                                          
         BR    RE                                                               
         DROP  RF                                                               
                                                                                
DESCLNQ  EQU   14                                                               
SEQPOS#L DS    H                                                                
SEQPOS#R DS    H                                                                
SEQPOS#C DS    H                                                                
SEQPOS#2 DS    C                                                                
COLCOUNT DS    X                                                                
SUBTYPE  DS    C                                                                
SBTYMD   DC    C'yyyymmdd'                                                      
         EJECT ,                                                                
***********************************************************************         
* Resolve heading data and put out data                                         
***********************************************************************         
         USING MPCELD,R2                                                        
         USING HEADD,R3                                                         
PUTHEAD  MVI   MPCEL,MPCELQ        Map code                                     
         MVI   MPCLN,MPCLNQ                                                     
         MVC   MPCID#,=AL2(REC_ELMD)                                            
         AHI   R2,MPCLNQ                                                        
                                                                                
         USING MPDELD,R2                                                        
PUTHED10 MVI   MPDEL,MPDELQ        Map code data                                
         MVI   MPDLN,MPDLNQ+1                                                   
         MVI   MPDID#,ELM_TYPE                                                  
         MVI   MPDIND,0                                                         
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVI   MPDDATA,C'H'        Heading type                                 
         CLI   HEADTYPE,HEADFOOT                                                
         BNE   *+8                                                              
         MVI   MPDDATA,C'F'        Footing type                                 
         AHI   R2,MPDLNQ+1                                                      
                                                                                
         MVI   MPDEL,MPDELQ        Map code data                                
         MVI   MPDLN,MPDLNQ+6                                                   
         MVI   MPDID#,ELM_POS                                                   
         MVI   MPDDATA,ROWLEFT                                                  
         TM    HEADTYPE,HEADLEFT                                                
         BO    PUTHED12                                                         
         MVI   MPDDATA,ROWRGHT                                                  
         TM    HEADTYPE,HEADRGHT                                                
         BO    PUTHED12                                                         
         MVI   MPDDATA,ROWCNTR                                                  
         TM    HEADTYPE,HEADCNTR+HEADTITL                                       
         BNZ   PUTHED12                                                         
         DC    H'00'               Should be one of the above                   
                                                                                
PUTHED12 MVI   MPDDATA+1,C','                                                   
         ZIC   RF,HEADSEQ                                                       
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MPDDATA+2(2),DUB                                                 
         MVC   MPDDATA+4(2),=C',1'                                              
         MVI   MPDIND,0                                                         
         MVI   MPDTYPE,MPD_CHRR                                                 
         AHI   R2,MPDLNQ+6                                                      
                                                                                
         L     R4,HEADPTR          Point to free form data                      
         MVI   MPDEL,MPDELQ        Map code data                                
         MVI   MPDID#,ELM_DATA                                                  
         MVI   MPDIND,0                                                         
         MVI   MPDTYPE,MPD_CHRR                                                 
         SR    RF,RF                                                            
         IC    RF,HEADLN                                                        
         SHI   RF,1                                                             
         EXMVC RF,MPDDATA,0(R4)                                                 
         AHI   RF,MPDLNQ+1                                                      
         STC   RF,MPDLN                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         DROP  R2,R3                                                            
                                                                                
         LTORG                                                                  
         EJECT ,                                                                
         TITLE 'Build Request definition data'                                  
***********************************************************************         
* Build Definition data based on definition in DDLINK                 *         
*       R6 = A(LP_D)    interface to dig out request detail table     *         
***********************************************************************         
         USING FMTRECD,R5                                                       
*        USING LP_D,R6                                                          
REQDEF   NTR1  BASE=*,LABEL=*                                                   
         TM    RECAPSW,RECANLYS    Any Analysis reporting                       
         BZ    REQDEFX                                                          
                                                                                
         SR    R0,R0                                                            
         ICM   R0,3,ANLYRPT#       Number of analysis reports                   
         BZ    REQDEFX             Somethings is wrong                          
*        BRAS  RE,CHKUSED          Check if any of the items are used           
*        BL    REQDEFX             No so don't build request                    
         BRAS  RE,BLDANL                                                        
                                                                                
         USING RQTABD,R3                                                        
         USING MPDELD,RE                                                        
         L     R3,AREQTAB                                                       
         LA    RE,REQNUM                                                        
         MVI   MPDDATA+1,0         Initialize                                   
         LA    RE,REQITM                                                        
         MVI   MPDDATA+1,0         Initialize                                   
         AHI   R5,FMTLNQ                                                        
         DROP  RE                                                               
                                                                                
REQDEF10 CLI   0(R3),EOT           Find which request we want                   
         BE    REQDEF50                                                         
         CLC   RQTYPE,QPROG        Match on type                                
         BE    REQDEF15                                                         
REQDEF12 LA    R3,RQTLNQ(,R3)      Next                                         
         B     REQDEF10                                                         
                                                                                
         USING RQLISTD,R4                                                       
         USING MPDELD,RE                                                        
REQDEF15 LA    RE,REQNUM           Keep track of request number                 
         LLC   RF,MPDDATA+1                                                     
         AHI   RF,1                                                             
         STC   RF,MPDDATA+1        Increament                                   
         DROP  RE                                                               
                                                                                
         USING RQD,RE                                                           
         L     RE,AREQITEM         A(REQFRMT) or A(REQITEMS)                    
         MVC   RQXDATA(L'FMTCODE),FMTCODE        Preset                         
         DROP  RE                                                               
                                                                                
         SR    R4,R4                                                            
         ICM   R4,3,RQTLIST        A(list of entries)                           
         A     R4,AREQLIST                                                      
         LLC   R9,RQT#ITEM         # of items in list                           
REQDEF16 CLI   RQLUSED,YES                                                      
         BE    REQDEF20                                                         
REQDEF18 LA    R4,RQLLNQ(,R4)      Next list item                               
         BRCT  R9,REQDEF16                                                      
                                                                                
         AHI   R5,FMTLNQ                                                        
         BRCT  R0,REQDEF15         Next format                                  
                                                                                
         L     R5,AFORMATS         A(format speces)                             
         AHI   R5,FMTLNQ           Skip first formtat                           
         ICM   R0,3,ANLYRPT#       Number of analysis reports                   
         B     REQDEF12            Next Request                                 
                                                                                
         USING RQD,R8                                                           
         USING MPDELD,RE                                                        
REQDEF20 LA    RE,REQITM           Keep track of item number                    
         LLC   RF,MPDDATA+1                                                     
         AHI   RF,1                                                             
         STC   RF,MPDDATA+1        Increament                                   
         DROP  RE                                                               
                                                                                
         SR    R8,R8                                                            
         ICM   R8,3,RQLITEM                                                     
         A     R8,AREQITEM                                                      
REQDEF22 CLI   0(R8),EOR           Any more?                                    
         BE    REQDEF18            Next item in list                            
         TM    RQIND,RQDATA        Data only                                    
         BO    REQDEF24                                                         
         CLI   RQNODE#,0           If set then use                              
         BE    REQDEF28                                                         
                                                                                
REQDEF24 BRAS  RE,BLDREQ           Build rest of request                        
         GOTOR PUTMAPEL,DMCB,AIO2                                               
                                                                                
REQDEF28 LLC   RF,RQLN                                                          
         AR    R8,RF                                                            
         B     REQDEF22                                                         
                                                                                
         USING CCLISTD,R3                                                       
         USING CCND,R4                                                          
REQDEF50 L     R3,ACCNLIST                                                      
REQDEF52 CLI   0(R3),EOT           End of table                                 
         BE    REQDEFX                                                          
         TM    CCLIND,CCLACTV      Active condition to pass to PC               
         BZ    REQDEF58            No                                           
         SR    R4,R4                                                            
         ICM   R4,3,CCLITEM                                                     
         A     R4,ACCNTAB                                                       
REQDEF54 CLI   0(R4),0                                                          
         BE    REQDEF58                                                         
         CLI   CCNNODE#,0                                                       
         BE    REQDEF56            Not set so don't process this one            
         BRAS  RE,BLDCON           Build conditionals                           
         L     R2,AIO2                                                          
         CLI   0(R2),EOR                                                        
         BE    REQDEF56            Don't bother, nothing built                  
         GOTOR PUTMAPEL,DMCB,AIO2                                               
REQDEF56 LLC   RF,CCNLN                                                         
         AR    R4,RF                                                            
         B     REQDEF54                                                         
                                                                                
REQDEF58 LA    R3,CCLLNQ(,R3)                                                   
         B     REQDEF52                                                         
*&&DO                                                                           
         USING LH_D,R3                                                          
         USING MPDELD,R4                                                        
         L     R3,LP_AQMAP         Record for request                           
         SHI   R3,LH_LNQ           Point to header info                         
         LA    R4,REQMREC                                                       
         MVC   MPDDSML,LH_MAPN                                                  
                                                                                
         LA    R4,REQRUNDT                                                      
         GOTOR DATCON,DMCB,(4,RCDATE),(23,MPDDATA)                              
         GOTOR PUTMAPEL,DMCB,REQREC                                             
                                                                                
         USING LD_D,R3                                                          
         L     R3,LP_AQMAP         Map entries for request                      
REQDEF30 CLI   0(R3),LD_EOTQ       End of table?                                
         JE    REQDEFX                                                          
         LA    R4,REQMAPIT                                                      
         MVC   MPDDSML,LD_CODE                                                  
         LA    R4,REQTYPE                                                       
         MVI   MPDDATA,C'C'           Character  (default)                      
         CLI   LD_TYPE,LD_EDATQ       Date ?                                    
         JNE   *+8                                                              
         MVI   MPDDATA,C'D'           Yes date                                  
         LA    R4,REQILEN             Input length                              
         MVC   MPDDSML+1(1),LD_LEN                                              
         LA    R4,REQTEXT                                                       
         MVI   MPDDATA,ESC#LFJT                                                 
         MVC   MPDDATA+1(L'LD_TNUM),LD_TNUM                                     
         MVI   MPDDATA+3,20                                                     
         GOTO1 ADDICTAT,DMCB,C'SL  ',MPDDATA,0                                  
         L     RE,LP_BLKS                                                       
         SR    RF,RF                                                            
         ICM   RF,3,LD_DISP                                                     
         AR    RE,RF                                                            
         CLI   0(RE),C' '                                                       
         BNH   *+6                                                              
         SR    R1,R1                                                            
                                                                                
REQDEF40 GOTOR PUTMAPEL,DMCB,REQDET   Details                                   
         AHI   R3,LD_LNQ                                                        
         J     REQDEF30                                                         
                                                                                
*&&                                                                             
REQDEFX  GOTOR PUTMAPEL,DMCB,REQEND   Details                                   
         XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT ,                                                                
***********************************************************************         
* Make sure that at least one of the items in the table is marked               
* used before putting out the request.                                          
***********************************************************************         
         USING RQTABD,R3                                                        
CHKUSED  NTR1                                                                   
         L     R3,AREQTAB          A(Request to process)                        
CHKUD10  CLI   0(R3),EOT                                                        
         JE    CHKUDE                                                           
         CLC   RQTYPE,QPROG        Match on reporting type                      
         JE    CHKUD20                                                          
         LA    R3,RQTLNQ(,R3)                                                   
         J     CHKUD10                                                          
                                                                                
         USING RQLISTD,R4                                                       
CHKUD20  LLC   R1,RQT#ITEM         Number of items in list                      
         SR    R4,R4                                                            
         ICM   R4,3,RQTLIST        Displacement to list                         
         A     R4,AREQLIST                                                      
CHKUD22  CLI   RQLUSED,YES                                                      
         BE    CHKUDE                                                           
         LA    R4,RQLLNQ(,R4)      Next item                                    
         BRCT  R1,CHKUD22                                                       
*                                                                               
CHKUDL   CLI   *,X'FF'             SET CC = LOW                                 
         J     CHKUDX                                                           
CHKUDE   CR    RB,RB               SET CC = EQUAL                               
CHKUDX   XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* R0 = # of analysis reports                                                    
***********************************************************************         
         USING MPDELD,R2                                                        
ANL      USING FMTRECD,R3                                                       
BLDANL   NTR1                                                                   
         GOTOR PUTMAPEL,DMCB,LSTFMT   List format information                   
                                                                                
         LHI   R7,1                                                             
         LA    R3,FMTLNQ(,R5)      R3 = A(1St Analysis report)                  
BLDANL10 L     R2,AIO2             10K area                                     
         LR    RE,R2               Clear out AIO2                               
         SR    R1,R1                                                            
         LHI   RF,IOSIZEQ*10                                                    
         MVCL  RE,R0                                                            
                                                                                
         MVC   0(MPCLNQ,R2),REQDDF                                              
         LA    R2,MPCLNQ(,R2)                                                   
*                                                                               
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNSQ                                                    
         MVI   MPDID#,DDF_LST#     Data definition list number                  
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDTYPE,MPD_SML                                                  
         MVI   MPDDATA+1,LST_ANLS  Analysis list                                
         LA    R2,MPDLNSQ(,R2)                                                  
                                                                                
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNSQ                                                    
         MVI   MPDID#,DDF_UNQ#     Data definition list number                  
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDTYPE,MPD_SML                                                  
         STC   R7,MPDDATA+1                                                     
         LA    R2,MPDLNSQ(,R2)                                                  
*                                                                               
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNQ+L'FMTCODE                                           
         MVI   MPDID#,ATBFMTC      Format code (Attibute #=1)                   
         MVI   MPDIND,0                                                         
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVC   MPDDATA(L'FMTCODE),ANL.FMTCODE                                   
         LLC   RF,MPDLN                                                         
         AR    R2,RF                                                            
                                                                                
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNQ+L'FMTNAME                                           
         MVI   MPDID#,ATBFMTN      Format code (Attibute #=2)                   
         MVI   MPDIND,0                                                         
         MVI   MPDTYPE,MPD_CHRR                                                 
         MVC   MPDDATA(L'FMTNAME),ANL.FMTNAME                                   
         LLC   RF,MPDLN                                                         
         AR    R2,RF                                                            
                                                                                
         GOTOR PUTMAPEL,DMCB,AIO2  Data                                         
         AHI   R7,1                Add 1 to unique number                       
         LA    R3,FMTLNQ(,R3)                                                   
         BRCT  R0,BLDANL10                                                      
BLDANLX  XIT1                                                                   
         DROP  ANL                                                              
         DROP  R2                                                               
***********************************************************************         
*                                                                               
***********************************************************************         
         USING MPDELD,R2                                                        
         USING CCLISTD,R3                                                       
         USING CCND,R4                                                          
BLDCON   NTR1                                                                   
         L     R2,AIO2             10K area                                     
         LR    RE,R2               Clear out AIO2                               
         SR    R1,R1                                                            
         LHI   RF,IOSIZEQ*10                                                    
         MVCL  RE,R0                                                            
                                                                                
         MVC   0(MPCLNQ,R2),REQCON                                              
         LA    R2,MPCLNQ(,R2)                                                   
                                                                                
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNSQ                                                    
         MVI   MPDID#,CON_NUM      Condition Id #                               
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDTYPE,MPD_SML                                                  
         MVC   MPDDATA+1(1),CCLID#                                              
         NI    MPDDATA+1,TURNOFF-RQCC                                           
         LA    R2,MPDLNSQ(,R2)                                                  
                                                                                
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNSQ                                                    
         MVI   MPDID#,CON_TYPE     Condition type, If / or / and                
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDTYPE,MPD_SML                                                  
         MVC   MPDDATA+1(1),CCNTYP                                              
         LA    R2,MPDLNSQ(,R2)                                                  
                                                                                
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNSQ                                                    
         MVI   MPDID#,CON_OPRD     Operator type                                
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDTYPE,MPD_SML                                                  
         MVC   MPDDATA+1(1),CCNOPR                                              
         LA    R2,MPDLNSQ(,R2)                                                  
                                                                                
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNSQ                                                    
         MVI   MPDID#,CON_EID#     Report level Id #                            
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDTYPE,MPD_SML                                                  
         MVC   MPDDATA+1(1),CCNNODE#                                            
         LA    R2,MPDLNSQ(,R2)                                                  
                                                                                
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNSQ                                                    
         MVI   MPDID#,CON_IDAT     Data to compare to                           
         LLC   RF,CCNLN                                                         
         SHI   RF,CCLLNQ                                                        
         LA    R1,MPDLNQ(,RF)                                                   
         STC   R1,MPDLN            Length of data                               
         MVI   MPDIND,0                                                         
         MVI   MPDTYPE,MPD_CHRR                                                 
         BCTR  RF,0                                                             
         EXMVC RF,MPDDATA,CCNXDATA                                              
         AR    R2,R1                                                            
                                                                                
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNSQ                                                    
         MVI   MPDID#,CON_DLEN          Data length                             
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDTYPE,MPD_SML                                                  
         MVC   MPDDATA+1(1),CCNNLEN     Length to compare for                   
         LA    R2,MPDLNSQ(,R2)                                                  
                                                                                
         CLI   CCNNPOS,0           Any position defined                         
         BE    BLDCN100                                                         
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNSQ                                                    
         MVI   MPDID#,CON_DPOS     Data length                                  
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDTYPE,MPD_SML                                                  
         MVC   MPDDATA+1(1),CCNNPOS                                             
         LA    R2,MPDLNSQ(,R2)                                                  
                                                                                
BLDCN100 SR    RF,RF                                                            
         ICM   RF,1,CCLRTN#        Get routine #                                
         BZ    BLDCNXIT                                                         
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     SLDGLEN                                                          
         B     SLDGPOS             The +1 is hard coded for now                 
         B     SVALUE                                                           
                                                                                
         USING LDGD,R7                                                          
SLDGPOS  LHI   R0,3                2 for unit/ledger +1 for prefix              
         B     *+6                                                              
SLDGLEN  SR    R0,R0               No extra value                               
         GOTOR GETLDGR,DMCB,CCNXDATA                                            
         ICM   R7,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'00'                                                            
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,CCLOTHR        Get displacment to value                     
         AR    RE,R7               Add base of area                             
         SR    RF,RF                                                            
         ICM   RF,1,0(RE)          Get length or position                       
         BZ    BLDCLR              Value is zero so don't send                  
         LTR   R0,R0                                                            
         BZ    SLDG010                                                          
         CLI   0(RE),12            Did we max out the prior level               
         BE    BLDCLR              Yes, so don't send this one                  
                                                                                
SLDG010  AR    RF,R0               Add any extra value                          
         STC   RF,MPDDATA+1        Save it                                      
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNSQ                                                    
         MVI   MPDID#,CON_RVAL     Return value                                 
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDTYPE,MPD_SML     Small (half word)                            
         LA    R2,MPDLNSQ(,R2)                                                  
         B     BLDCNXIT                                                         
         DROP  R7                                                               
                                                                                
SVALUE   OC    CCLOTHR,CCLOTHR                                                  
         BZ    BLDCNXIT            Don't send zero                              
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNSQ                                                    
         MVI   MPDID#,CON_RVAL     Return value                                 
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDTYPE,MPD_SML                                                  
         MVC   MPDDATA(2),CCLOTHR                                               
         LA    R2,MPDLNSQ(,R2)                                                  
         B     BLDCNXIT                                                         
                                                                                
BLDCLR   L     R2,AIO2                                                          
         MVI   0(R2),EOR           Clear so don't send                          
                                                                                
BLDCNXIT XIT1                                                                   
         DROP  R2,R3,R4                                                         
***********************************************************************         
*                                                                               
***********************************************************************         
         USING MPDELD,R2                                                        
BLDREQ   NTR1                                                                   
         L     R2,AIO2             10K area                                     
         LR    RE,R2               Clear out AIO2                               
         SR    R1,R1                                                            
         LHI   RF,IOSIZEQ*10                                                    
         MVCL  RE,R0                                                            
                                                                                
         MVC   0(MPCLNQ,R2),REQREC        Record REC_REQD X'FE34'               
         LA    R2,MPCLNQ(,R2)                                                   
         MVC   0(MPDLNSQ,R2),REQNUM       Request number                        
         LA    R2,MPDLNSQ(,R2)                                                  
         MVC   0(MPDLNSQ,R2),REQITM       Item number                           
         LA    R2,MPDLNSQ(,R2)                                                  
*                                         Required field + prior level          
         TM    RQIND,RQREQ+RQPRVLV+RQDDF  Data defined being used               
         BZ    BLDRQ080                                                         
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNSQ                                                    
         MVI   MPDID#,REQ_IND                                                   
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDTYPE,MPD_SML                                                  
         MVC   MPDDATA+1(1),RQIND                                               
         NI    MPDDATA+1,RQREQ+RQPRVLV+RQDDF                                    
         LA    R2,MPDLNSQ(,R2)                                                  
                                                                                
BLDRQ080 MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNSQ                                                    
         MVI   MPDID#,REQ_MAP#     Request input field map#                     
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDTYPE,MPD_SML                                                  
         MVC   MPDDATA+1(1),RQMAP#                                              
         LA    R2,MPDLNSQ(,R2)                                                  
                                                                                
         TM    RQIND,RQDATA        Data only                                    
         BZ    BLDRQ085                                                         
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDID#,REQ_IDAT                                                  
         LLC   RF,RQLN                                                          
         SHI   RF,RQLNQ                                                         
         LA    R1,MPDLNQ(,RF)                                                   
         STC   R1,MPDLN            Length of data                               
         MVI   MPDIND,0                                                         
         MVI   MPDTYPE,MPD_CHRR                                                 
         BCTR  RF,0                                                             
         EXMVC RF,MPDDATA,RQXDATA                                               
         AR    R2,R1                                                            
                                                                                
BLDRQ085 CLI   RQNODE#,0           List of data or node#                        
         BE    BLDRQ090                                                         
         MVI   MPDID#,REQ_LST#                                                  
         TM    RQIND,RQDDF         Data list?                                   
         BO    *+8                                                              
         MVI   MPDID#,REQ_EID#                                                  
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNSQ                                                    
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDTYPE,MPD_SML                                                  
         MVC   MPDDATA+1(1),RQNODE#                                             
         LA    R2,MPDLNSQ(,R2)                                                  
                                                                                
         TM    RQIND,RQDDF         If on then pass attibute #                   
         BZ    BLDRQ090                                                         
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNSQ                                                    
         MVI   MPDID#,REQ_ATB#                                                  
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDTYPE,MPD_SML                                                  
         MVC   MPDDATA+1(1),RQATB#                                              
         LA    R2,MPDLNSQ(,R2)                                                  
                                                                                
BLDRQ090 TM    RQIND,RQPRFIX                                                    
         BZ    BLDRQ100                                                         
         LLC   RF,RQLN                                                          
         SHI   RF,RQLNQ                                                         
         LA    R1,MPDLNQ(,RF)                                                   
         STC   R1,MPDLN            Length of data                               
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDIND,0                                                         
         MVI   MPDID#,REQ_IPFX     Hard coded prefix value                      
         MVI   MPDTYPE,MPD_CHRR                                                 
         BCTR  RF,0                                                             
         EXMVC RF,MPDDATA,RQXDATA                                               
         AR    R2,R1                                                            
                                                                                
BLDRQ100 CLI   RQLEN,0                                                          
         BE    BLDRQ110                                                         
         MVI   MPDID#,REQ_ILEN     Value is as indicated                        
         TM    RQLEN,RQCC          Variable length and position                 
         BZ    *+8                 Value                                        
         MVI   MPDID#,REQ_VLEN     Conditional, variable length                 
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNSQ                                                    
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDTYPE,MPD_SML                                                  
         MVC   MPDDATA+1(1),RQLEN                                               
         NI    MPDDATA+1,TURNOFF-RQCC                                           
         LA    R2,MPDLNSQ(,R2)                                                  
                                                                                
BLDRQ110 CLI   RQPOS,0                                                          
         BE    BLDRQXIT                                                         
         MVI   MPDID#,REQ_IPOS     Value is as indicated                        
         TM    RQPOS,RQCC          Is value or conditional                      
         BE    *+8                 Value                                        
         MVI   MPDID#,REQ_VPOS     Conditional, variable position               
         MVI   MPDEL,MPDELQ                                                     
         MVI   MPDLN,MPDLNSQ                                                    
         MVI   MPDIND,MPDRAW                                                    
         MVI   MPDTYPE,MPD_SML                                                  
         MVC   MPDDATA+1(1),RQPOS                                               
         NI    MPDDATA+1,TURNOFF-RQCC                                           
         LA    R2,MPDLNSQ(,R2)                                                  
                                                                                
BLDRQXIT DS    0H                                                               
         XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
* Table for building schema                                                     
***********************************************************************         
                                                                                
REQNUM   DC    AL1(MPDELQ,MPDLNSQ,REQ_NUM,MPD_SML,MPDRAW),AL2(0)                
REQITM   DC    AL1(MPDELQ,MPDLNSQ,REQ_ITM#,MPD_SML,MPDRAW),AL2(0)               
REQMAP#  DC    AL1(MPDELQ,MPDLNSQ,REQ_MAP#,MPD_SML,MPDRAW),AL2(0)               
                                                                                
REQREC   DC    AL1(MPCELQ,MPCLNQ),AL2(REC_REQD)                                 
REQMREC  DC    AL1(MPDELQ,MPDLNSQ,REQ_ID,MPD_SML,MPDRAW),AL2(0)                 
REQRUNDT DC    AL1(MPDELQ,MPDLNQ+10,REQ_DATE,MPD_CHRR,0),CL10' '                
         DC    AL1(EOR)                                                         
                                                                                
REQCON   DC    AL1(MPCELQ,MPCLNQ),AL2(REC_COND)                                 
         DC    AL1(EOR)                                                         
                                                                                
REQDET   DC    AL1(MPCELQ,MPCLNQ),AL2(REC_REQD)                                 
REQMAPIT DC    AL1(MPDELQ,MPDLNSQ,REQ_MAP#,MPD_SML,MPDRAW),AL2(0)               
REQTYPE  DC    AL1(MPDELQ,MPDLNQ+1,REQ_TYPE,MPD_CHRR,0),CL1' '                  
REQILEN  DC    AL1(MPDELQ,MPDLNSQ,REQ_ILEN,MPD_SML,MPDRAW),AL2(0)               
REQTEXT  DC    AL1(MPDELQ,MPDLNQ+20,REQ_TEXT,MPD_CHRR,0),CL20' '                
         DC    AL1(EOR)                                                         
                                                                                
REQDDF   DC    AL1(MPCELQ,MPCLNQ),AL2(REC_DDFN)                                 
         DC    AL1(EOR)                                                         
REQEND   DC    AL1(MPCELQ,MPCLNQ),AL2(REC_RDFN)                                 
         DC    AL1(EOR)                                                         
*EQRNODE DC    AL1(MPDELQ,MPDLNSQ,REQ_RND#,MPD_SML,MPDRAW),AL2(0)               
*EQVALU  DC    AL1(MPDELQ,MPDLNQ,REQ_VAL#,MPD_CHRR,0)                           
*EQPRFX  DC    AL1(MPDELQ,MPDLNQ,REQ_PRFX,MPD_CHRR,0)                           
                                                                                
LSTFMT   DC    AL1(MPCELQ,MPCLNQ),AL2(REC_LIST)                                 
         DC    AL1(MPDELQ,MPDLNSQ,LST_NUM,MPD_SML,MPDRAW),AL2(LST_ANLS)         
         DC    AL1(MPDELQ,MPDLNQ+16,LST_DSCP,MPD_CHRR,0)                        
         DC    CL16'Analysis reports'                                           
         DC    AL1(MPDELQ,MPDLNSQ,LST_TYPE,MPD_SML,MPDRAW),AL2(1)               
         DC    AL1(MPDELQ,MPDLNSQ,LST_PNO#,MPD_SML,MPDRAW),AL2(1)               
         DC    AL1(MPDELQ,MPDLNSQ,LST_ATR#,MPD_SML,MPDRAW),AL2(ATBFMTC)         
         DC    AL1(MPDELQ,MPDLNQ+1,LST_ATYP,MPD_CHRR,0),C'C'                    
         DC    AL1(MPDELQ,MPDLNSQ,LST_ALEN,MPD_SML,MPDRAW)                      
         DC    AL2(L'FMTCODE)                                                   
         DC    AL1(MPDELQ,MPDLNSQ,LST_ATR#,MPD_SML,MPDRAW),AL2(ATBFMTN)         
         DC    AL1(MPDELQ,MPDLNQ+1,LST_ATYP,MPD_CHRR,0),C'N'                    
         DC    AL1(MPDELQ,MPDLNSQ,LST_ALEN,MPD_SML,MPDRAW)                      
         DC    AL2(L'FMTNAME)                                                   
         DC    AL1(EOR)                                                         
         EJECT ,                                                                
         TITLE 'Build Security definition data'                                 
***********************************************************************         
* Build Definition data base of Format                                *         
*       R5 = A(Current format)                                        *         
*       R6 = A(LP_D)               Interface for DDLINKIO             *         
***********************************************************************         
         USING FMTRECD,R5                                                       
         USING LP_D,R6                                                          
SECDEF   NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIO2                                                          
         LR    RE,R2               CLEAR OUT AIO2                               
         LHI   RF,IOSIZEQ*10                                                    
         XCEFL                                                                  
                                                                                
         USING LQ_D,R2                                                          
         L     R2,AIO2                                                          
         MVI   LQ_EL,LQ_LIMAQ      Office limited access                        
         MVI   LQ_LN+1,LQ_LNAQ+LQ_AVALL+L'LQ_ARMAP+1                            
         MVI   LQ_AMTHD,LQ_AMACC   Accounting                                   
         MVI   LQ_ARMAP+1,REC_DATA                                              
                                                                                
         LA    R8,LQ_ADTYP                                                      
AD       USING LQ_ADTYP,R8                                                      
                                                                                
         MVI   AD.LQ_ADTYP,LQ_ATOFF                                             
         MVI   AD.LQ_ADMAP+1,EL_OFSEC                                           
         AHI   R8,LQ_AVALL                                                      
         MVI   AD.LQ_ADTYP,LQ_ATEOL                                             
         GOTOR DDLINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTGEL',0),AIO2            
         DROP  AD                                                               
                                                                                
SECDEF08 MVI   BYTE,NO             Any element needed                           
         MVI   LQ_EL,LQ_FSECQ                                                   
         MVI   LQ_FSSYS,X'06'      Accounting system                            
         MVI   LQ_FSPRG,X'0C'      Scribe program                               
         LA    RF,REC_DATA                                                      
         STCM  RF,3,LQ_FSRM#                                                    
                                                                                
         LA    R8,LQ_FSDV                                                       
FS       USING LQ_FSDV,R8                                                       
                                                                                
         USING ROWD,R3                                                          
         L     R3,FMTROW                                                        
         ZIC   R0,FMT#ROWS                                                      
SECDEF10 CLI   ROWSEC#,0                                                        
         BE    SECDEF20                                                         
         MVI   BYTE,YES            Yes we need to put element                   
         MVC   FS.LQ_FSFLD,ROWSEC#                                              
         ZIC   RF,ROWMAP#                                                       
         STCM  RF,3,FS.LQ_FSDM#                                                 
         AHI   R8,LQ_FSDVL                                                      
         TM    ROWFLAGS,ROWBOTH                                                 
         BNO   SECDEF20                                                         
         MVC   FS.LQ_FSFLD,ROWSEC#                                              
         AHI   RF,1                                                             
         STCM  RF,3,FS.LQ_FSDM#                                                 
         AHI   R8,LQ_FSDVL                                                      
                                                                                
SECDEF20 AHI   R3,ROWLNQ                                                        
         BRCT  R0,SECDEF10                                                      
*                                                                               
* Need to modify security for hidden columns                                    
*                                                                               
         USING COLD,R3                                                          
         L     R3,FMTCOL                                                        
         ZIC   R0,FMT#COLS                                                      
SECDEF30 CLI   COLSEC#,0                                                        
         BE    SECDEF40                                                         
         TM    COLFLAGS,COLHIDE                                                 
         BO    SECDEF40            Ignore hidden columns                        
         TM    COLFLAGS,COLAMT     Amount column                                
         BZ    SECDEF40            Not an amount                                
         MVI   BYTE,YES            Yes we need to put element                   
         MVC   FS.LQ_FSFLD,COLSEC#                                              
         ZIC   RF,COLMAP#                                                       
         STCM  RF,3,FS.LQ_FSDM#                                                 
         AHI   R8,LQ_FSDVL                                                      
SECDEF40 AHI   R3,COLLNQ                                                        
         BRCT  R0,SECDEF30                                                      
         DROP  FS                                                               
                                                                                
         CLI   BYTE,YES                                                         
         BNE   SECDEFX                                                          
         SR    R8,R2                                                            
         STCM  R8,3,LQ_LN                                                       
         GOTOR DDLINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTGEL',0),AIO2            
                                                                                
SECDEFX  XIT1                                                                   
         DROP  R2,R3,R5                                                         
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------*                                      
* Build map codes and data from elements *                                      
*----------------------------------------*                                      
PUTMAPEL NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(,R1)                                                        
PUTMAP10 CLI   0(R2),EOR                                                        
         JE    PUTMAPX                                                          
                                                                                
         USING MPCELD,R2                                                        
         CLI   0(R2),MPCELQ        MAP ID #, X'01'                              
         JNE   PUTMAP30                                                         
         SR    RF,RF                                                            
         ICM   RF,3,MPCID#                                                      
         GOTOR DDLINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',(RF)),0,0          
         J     PUTMAP80                                                         
                                                                                
         USING MPDELD,R2                                                        
PUTMAP30 CLI   0(R2),MPDELQ        MAP ELEMENT DATA, X'02'                      
         JNE   PUTMAP80                                                         
         TM    MPDIND,MPDIOFF      Skip this one                                
         JO    PUTMAP80                                                         
         ZIC   R4,MPDTYPE          R4 = Data type                               
         ZIC   R7,MPDID#           R7 = ID # of data                            
         TM    MPDIND,MPDRAW       Scribe raw data                              
         JO    PUTMAP40            Yes                                          
         ZIC   RF,MPDLN                                                         
         SHI   RF,MPDLNQ           RF = Length of data                          
         LA    R3,LIOTEDT                                                       
         CLI   MPDTYPE,X'40'       DDLINKIO raw data ?                          
         JH    *+8                                                              
         LA    R3,LIOTRAW                                                       
         GOTOR DDLINKIO,DMCB,('LIOAPUT',LP_ALIOB),((R3),(R7)),         X        
               ((R4),MPDDATA),((RF),0)                                          
         J     PUTMAP80                                                         
                                                                                
PUTMAP40 SR    R1,R1                                                            
         IC    R1,MPDDTNY                                                       
         CLI   MPDTYPE,MPD_TNY     Byte                                         
         JE    PUTMAP44                                                         
         ICM   R1,3,MPDDSML                                                     
         CLI   MPDTYPE,MPD_SML     Half word                                    
         JE    PUTMAP44                                                         
         ICM   R1,15,MPDDFUL                                                    
         CLI   MPDTYPE,MPD_FUL     Full word                                    
         JE    PUTMAP44                                                         
         DC    H'00'               MPD_LNG not support for now                  
                                                                                
PUTMAP44 BRAS  RE,NUM2STR          R1=value, return RF=len                      
         STC   R1,BYTE                                                          
                                                                                
         GOTOR DDLINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTEDT',(R7)),    X        
               ((R4),(RF)),(BYTE,0)                                             
                                                                                
PUTMAP80 ZIC   RF,1(,R2)                                                        
         AR    R2,RF                                                            
         J     PUTMAP10                                                         
         DROP  R2                                                               
                                                                                
PUTMAPX  XIT1                                                                   
         EJECT ,                                                                
         LTORG                                                                  
                                                                                
DATAEL   DS    0C                                                               
         DC    AL1(MPCELQ,MPCLNQ),AL2(REC_DOCI)                                 
         DC    AL1(MPDELQ,MPDLNQ+6,1,MPD_CHRR,0),C'REPORT'                      
         DC    AL1(MPDELQ,MPDLNQ+6,2,MPD_CHRR,0),C'SCRIBE'                      
         DC    AL1(0)                                                           
         EJECT ,                                                                
***********************************************************************         
* Build soft column headings                                          *         
***********************************************************************         
         USING COLD,R3                                                          
         USING FMTRECD,R5                                                       
SOFTHEAD NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(,R1)           A(column entry)                              
         L     R5,4(,R1)           A(format entry)                              
         TM    COLOPT,COLSFTH      Is this column one of them ?                 
         BZ    SOFTHD90            No                                           
                                                                                
         USING DTED,R6                                                          
         SR    R6,R6                                                            
         MVC   BYTE,0(R1)                                                       
         NI    BYTE,TURNOFF-RECMASK                                             
         ICM   R6,1,BYTE                                                        
         BZ    *+6                 DEFAULT to first set of dates                
         BCTR  R6,0                                                             
         MH    R6,DTECOLLN                                                      
         A     R6,FMTDTED                                                       
         GOTO1 DATCON,DMCB,(1,DTEPEDST),(X'20',DTE1)                            
         MVC   WORK(3),DTEPEDEN                                                 
         LA    R6,DTESTRT                                                       
         DROP  R6                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,COLDTE#          Index into DTED                              
         MHI   RF,L'DTECOL                                                      
         AR    R6,RF               A(entry)                                     
         LA    R8,COLHEAD1         Use 1st and 2nd heading                      
         TM    DDLNKIND,DDLNKON+DDLNKTST   DDLINK on                            
         BZ    SOFTDT12                                                         
         LA    R8,COLHEAD2         Use 2nd and 3rd heading                      
*&&UK*&& TM    DDLNKIND,DDLNKQKR   Quick reports ?                              
*&&UK*&& BZ    *+8                                                              
*&&UK*&& LA    R8,COLHEAD3         Use 3rd heading only                         
         MVI   0(R8),MAXHEADQ      Max heading length                           
                                                                                
SOFTDT12 ICM   R4,15,0(R8)                                                      
         BZ    SOFTHD90                                                         
         IC    RF,0(R8)            Get length of area                           
         SHI   RF,1                                                             
         BM    SOFTHD90                                                         
         EXMVC RF,0(R4),SPACES     Clear heading                                
                                                                                
         TM    COLFLAGS,COLAMT     An amount column ?                           
         BO    SOFTDT40            Yes                                          
         IC    RF,COLPERST         No, special day column                       
         BCTR  RF,0                                                             
         GOTO1 ADDAY,DMCB,(C'D',DTE1),DTE2,(RF)                                 
         GOTO1 DATCON,DMCB,(0,DTE2),(1,WORK+3)                                  
         CLC   WORK+3(3),WORK                                                   
         BH    SOFTHD90                                                         
         GOTO1 GETDAY,DMCB,DTE2,(R4)                                            
                                                                                
         LA    R8,COLHEAD2         Use 1st and 2nd heading                      
         TM    DDLNKIND,DDLNKON+DDLNKTST   DDLINK on                            
         BZ    SOFTDT14                                                         
         LA    R8,COLHEAD3         Use 2nd and 3rd heading                      
         MVI   0(R8),MAXHEADQ      Max heading length                           
                                                                                
SOFTDT14 ICM   R4,15,0(R8)                                                      
         BZ    SOFTHD90                                                         
*&&UK*&& TM    DDLNKIND,DDLNKQKR   Quick reports ?                              
*&&UK*&& BO    SOFTDT16                                                         
         IC    RF,0(R8)            Get length of area                           
         SHI   RF,1                                                             
         BM    SOFTHD90                                                         
         EXMVC RF,0(R4),SPACES     Clear heading                                
                                                                                
SOFTDT16 CLI   COLSIZE,DATESZQ                                                  
         BL    SOFTDT20                                                         
*&&UK                                                                           
         TM    DDLNKIND,DDLNKQKR   Quick reports ?                              
         BZ    *+14                                                             
         MVC   10(2,R4),=C'^^'     Yes, split headings into two lines           
         LA    R4,14(,R4)                                                       
*&&                                                                             
         GOTO1 DATCON,DMCB,(1,WORK+3),(8,(R4))                                  
         B     SOFTHD90                                                         
*                                                                               
SOFTDT20 DS    0H                                                               
*&&US*&& MVC   0(2,R4),DTE2+4                                                   
*&&UK*&& MVC   0(2,R4),DTE2                                                     
         CLI   0(R4),C'0'          IS FIRST DIGIT A ZERO                        
         BNE   SOFTHD90                                                         
         MVI   0(R2),BLANK                                                      
         B     SOFTHD90                                                         
*                                                                               
SOFTDT40 OC    0(3,R6),0(R6)                                                    
         BZ    SOFTHD70                                                         
         CLC   0(3,R6),=X'FFFFFF'                                               
         BE    SOFTHD60                                                         
         GOTO1 DATCON,DMCB,(1,0(R6)),(8,(R4))                                   
*                                                                               
SOFTHD60 LA    R8,COLHEAD2                                                      
         TM    DDLNKIND,DDLNKON+DDLNKTST   DDLINK on                            
         BZ    *+8                                                              
         LA    R8,COLHEAD3         Use 2nd and 3rd heading                      
         ICM   R4,15,0(R8)                                                      
         BZ    SOFTHD90                                                         
*&&UK*&& TM    DDLNKIND,DDLNKQKR   Quick reports ?                              
*&&UK*&& BO    SOFTHD70                                                         
         SR    RF,RF                                                            
         IC    RF,0(R8)            Get length of area                           
         SHI   RF,1                                                             
         BM    SOFTHD90                                                         
         EXMVC RF,0(R4),SPACES     Clear heading                                
                                                                                
SOFTHD70 CLC   3(3,R6),=X'FFFFFF'                                               
         BE    SOFTHD90                                                         
         OC    3(3,R6),3(R6)                                                    
         BZ    SOFTHD90                                                         
*&&UK                                                                           
         TM    DDLNKIND,DDLNKQKR   Quick reports ?                              
         BZ    *+14                                                             
         MVC   10(2,R4),=C'^^'      Yes, split headings into two lines          
         LA    R4,13(,R4)                                                       
*&&                                                                             
         GOTO1 DATCON,DMCB,(1,3(R6)),(8,(R4))                                   
                                                                                
SOFTHD90 XIT1                                                                   
                                                                                
         LTORG                                                                  
         DROP  R3,R5,R8,RB                                                      
         EJECT ,                                                                
         TITLE 'Hook into downloading to put to data set/tape'                  
*=====================================================================*         
*        Dynamicly create a tape, fixed record type only for now      *         
*        Dataset name is ACCTAPE.ACppaa.format                        *         
*                    pp = Program RL, IL, VL etc.                     *         
*                    aa = aa is agency alpha                          *         
*                    Format is format code requested                  *         
*=====================================================================*         
         USING FMTRECD,R5                                                       
         USING DLCBD,R9                                                         
         USING ACRL2D,R7                                                        
DATASET  NMOD1 0,**DSET**,END=DATASETX                                          
*                                                                               
         LARL  R8,GLOBALS                                                       
         USING GLOBALS,R8                                                       
*                                                                               
         L     RC,RLBASEC                                                       
         L     R7,AACRL2D                                                       
         L     R9,DWNLDCB                                                       
         L     R5,AFORMATS                                                      
         CLI   QOPT6,C'T'          Just create tape/report                      
         JE    DATAST10            Yes                                          
         TM    DWNOPT2,DWNDSN      Transmit through EDICT ?                     
         JZ    DATASEND            No                                           
                                                                                
DATAST10 MVC   DSETACT,DLCBACT                                                  
         L     R3,=A(OUTWRK)                                                    
         CLI   DSETACT,DLCBINIT    Initialize                                   
         JE    DATASOPN                                                         
         CLI   DSETACT,DLCBPUT     Text to write out                            
         JE    DATASTXT                                                         
         CLI   DSETACT,DLCBEOL     End of line                                  
         JE    DATASPUT                                                         
         CLI   DSETACT,DLCBEOR     End of report                                
         JE    DATASCLS                                                         
         DC    H'00'                                                            
*                                                                               
DSN      USING DSNMD,DDSNAME1                                                   
                                                                                
DATASOPN DS    0H                                                               
*&&US                                                                           
         MVC   DDSNAME1,=CL30'ACCTAPE.ACPPAA.FORMAT'                            
                                                                                
         USING MASTD,RF                                                         
         L     RF,ADMASTC                                                       
         ICM   RE,15,MCSSB                                                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
SSBD     USING SSOOFF,RE                                                        
         ICM   RE,15,MCSSB         Get SSB                                      
         JZ    DATAS010                                                         
         CLI   SSBD.SSODSPAC,C'A'  ADV                                          
         JE    DATAS010                                                         
         MVC   DDSNAME1(3),=C'CSC'                                              
         CLI   SSBD.SSODSPAC,C'C'  CSC                                          
         JE    DATAS010                                                         
         MVC   DDSNAME1(3),=C'TST'                                              
         CLI   SSBD.SSODSPAC,C'T'  TST                                          
         JE    DATAS010                                                         
         MVC   DDSNAME1(3),=C'FQA'                                              
         CLI   SSBD.SSODSPAC,C'Q'  FQA                                          
         JE    DATAS010                                                         
         MVC   DDSNAME1(3),=C'???'                                              
         DROP  SSBD                                                             
*&&                                                                             
DATAS010 BRAS  R2,TAPECLR                                                       
         BRAS  R2,CRECLEN          Get length of record in R1                   
         STCM  R1,3,82(R3)         Record length                                
         MHI   R1,10                                                            
         STCM  R1,3,62(R3)         Block size                                   
*        BAS   R2,DATASPC                                                       
*        JE                                                                     
*                                                                               
         MVC   DSN.DSNPRG,QPROG                                                 
*                                                                               
DATAS015 MVC   DSN.DSNALPHA,ALPHAID                                             
         MVC   DSN.DSNFMT,QAPPL                                                 
         CLC   =C'DJCTEST',QUESTOR                                              
         JNE   DATASO18                                                         
         MVC   DDSNAME1,SPACES                                                  
         MVC   DDSNAME1(12),=C'DCUR.BIGOUT3'                                    
         J     DATASO40                                                         
*                                                                               
DATASO18 CLI   QOPT6,C'D'                                                       
         JNE   DATASO20                                                         
         MVC   DDSNAME1,SPACES                                                  
*&&UK                                                                           
         MVC   DDSNAME1(9),=C'AHYD.DUMP'                                        
*&&                                                                             
*&&US                                                                           
         GOTO1 =V(XTSOUSR),DMCB,A(DDSNAME1)                                     
         MVC   DDSNAME1+4(5),=C'.DUMP'                                          
*&&                                                                             
         J     DATASO40                                                         
                                                                                
DATASO20 CLC   =C'AH3TEST',QUESTOR                                              
         JE    DATASO30                                                         
         CLC   ALPHAID,=C'SJ'                                                   
*        JNE   DATASO40                                                         
         JE    DATASO30                                                         
         CLC   ALPHAID,=C'*B'                                                   
         JNE   DATASO40                                                         
                                                                                
DATASO30 MVC   DSN.DSNALPHA,=C'F9'                                              
         MVC   DSN.DSNFMT,=CL8'EDICT'                                           
*&&US                                                                           
DATASO40 MVI   BYTE,0              VTS / TAPE (use TAPE=VTS in JCL)             
         MVI   BYTE2,X'FE'                                                      
         CLI   QOPT6,C'D'                                                       
         JE    DATASO42                                                         
         CLC   =C'DJCTEST',QUESTOR                                              
         JNE   *+12                                                             
DATASO42 MVI   BYTE,X'FF'          DSN                                          
         MVI   BYTE2,0                                                          
         MVI   BYTE1,01            stick the gen # in here                      
         GOTO1 DYNALLOC,DMCB,(BYTE,DDNAME),(BYTE2,DDSNAME1),           +        
               (X'80',BYTE1),0                                                  
*&&                                                                             
*&&UK                                                                           
DATASO40 MVI   BYTE,0              VTS / TAPE                                   
         CLI   QOPT6,C'D'                                                       
         JNE   *+8                                                              
         MVI   BYTE,X'FF'          DSN                                          
         CLC   =C'DJCTEST',QUESTOR                                              
         JNE   *+8                                                              
         MVI   BYTE,X'FF'          DSN                                          
         MVI   BYTE2,1             GENERATION +1                                
         GOTO1 DYNALLOC,DMCB,(BYTE,DDNAME),(X'FE',DDSNAME1),           +        
               (X'80',BYTE2)                                                    
*&&                                                                             
         CLI   QOPT6,C'D'                                                       
         JE    DATASO50                                                         
         CLI   QOPT6,C'T'                                                       
         JE    DATASO50                                                         
         CLC   =C'DJCTEST',QUESTOR                                              
         JE    DATASO50                                                         
         GOTO1 DYNALLOC,DMCB,(C'D',DDNAME),FULLDSN  Fully-qualified DSN         
         CLI   DMCB+4,0                                                         
         JE    *+2                 Unsuccessful DSN retrieval !?!               
                                                                                
DATASO50 OPEN  ((R3),OUTPUT)       Open dataset (tape)                          
         LTR   RF,RF                                                            
         JZ    DATASEND                                                         
         DC    H'00'                                                            
         DROP  DSN                                                              
*                                                                               
DATASTXT SR    R1,R1                                                            
         IC    R1,DLCBLEN                                                       
         SHI   R1,1                                                             
         LH    RF,TAPEDSP          Get displacement to put data                 
         LA    RE,TAPEDATA(RF)     Point to location                            
         LA    RF,1(R1,RF)         Calculate for next time                      
         STH   RF,TAPEDSP          Store next new displacement                  
         EXMVC R1,0(RE),XLCHOP     Move data to tape                            
*                                                                               
         USING BIGPRNTD,R4                                                      
         L     R4,VBIGPRNT                                                      
         J     DATASEND                                                         
*                                                                               
DATASPUT DS    0H                                                               
*&&US                                                                           
         TM    DWNOPT3,DWNFDSDN    DSN DOWNLOAD FILE                            
         JZ    DATASPT5                                                         
         OC    DLCBNUMC,DLCBNUMC   ANY CHARS TO PUT?                            
         JZ    DATASPT7            NO, DON'T BOTHER                             
         USING BIGPRNTD,R4                                                      
         L     R4,VBIGPRNT                                                      
*                                                                               
         L     RF,ADWNLINE                                                      
         AHI   RF,1999                                                          
         LHI   R0,1999                                                          
DATASPT2 CLI   0(RF),C' '                                                       
         JH    DATASPT3                                                         
         AHI   RF,-1                                                            
         JCT   R0,DATASPT2                                                      
         J     DATASPT4                                                         
DATASPT3 AHI   RF,1                                                             
DATASPT4 MVC   0(L'XP,RF),XP                                                    
*                                                                               
         AH    RF,DLCBNUMC         Number of characters in line                 
         AHI   RF,-1                                                            
         MVI   0(RF),C' '          Clear out last comma (CARAT)                 
                                                                                
         LA    RE,TAPEDATA         A(DESTINATION), SORT WORK                    
         L     R0,ADWNLINE         A(SOURCE), SORT RECORD                       
         LHI   R1,L'TAPEDATA       L'SOURCE                                     
         LR    RF,R1               L'DESTINATION                                
         MVCL  RE,R0                                                            
                                                                                
         DROP  R4                                                               
*&&                                                                             
DATASPT5 PUT   (R3),TAPEDATA                                                    
DATASPT7 BRAS  R2,TAPECLR                                                       
         J     DATASEND                                                         
*                                                                               
DATASCLS CLOSE ((R3))                                                           
         TM    DWNOPT2,DWNDSN      Transmit through EDICT ?                     
         JZ    DATASEND                                                         
         CLC   DESTID,=CL8'GENERIC'  Are we doing a true transmission?          
         JE    DATASEND              No - Skip putting out the header           
         BRAS  R2,PUTPQDSN           Put out Header to print queue              
         J     DATASEND                                                         
*                                                                               
DATASEND CLI   DLCBACT,DLCBEOL     End of line                                  
         JNE   DATASGO             No                                           
         CLI   DLCXEOLC,0          Was EOL delimiter set to NONE                
         JNE   DATASGO             No                                           
         L     RE,DLCBAPL          Get A(DATA OUTPUT)                           
         LH    RF,DLCBNUMC         Number of characters in line                 
         SHI   RF,1                Bump back one character                      
         AR    RE,RF                                                            
         CLC   0(1,RE),DLCXDELC    Remove last delimiter if match               
         JNE   DATASGO             No                                           
         STH   RF,DLCBNUMC         Yes                                          
         MVI   0(RE),C' '                                                       
                                                                                
DATASGO  GOTO1 =V(DLFLD),(R9)                                                   
         XIT1                                                                   
*                                                                               
                                                                                
*&&DO                                                                           
*=====================================================================*         
*        Check for special characters in agency alpha                 *         
*=====================================================================*         
DATASPC  LA    RF,CHARTBL          Before using alpha id in the dataset         
         CLI   0(RF),X'FF'         name make sure it does not contain           
         BE    DATASL              any special characters.  If so then          
         LA    RE,ALPHAID          use the four char Agency Label from          
         LHI   R1,L'ALPHAID        the Access record.                           
DATASO05 CLC   0(1,RE),0(RF)                                                    
         BE    DATASO10                                                         
         LA    RE,1(RE)                                                         
         BCT   R1,DATASO05                                                      
*                                                                               
DSN      USING DSNMD,DDSNAME2                                                   
DATASO10 L     RF,VMASTC                                                        
         L     RE,MCAEXTRA-MASTD(RF)                                            
         MVC   DSN.DSNAGYL,MCAGYCOD-MCEXTRA(RE)                                 
         MVC   DSN.DSNFMT2,QAPPL                                                
         MVC   DSN.DSNPRG,QPROG                                                 
*                                                                               
DATASL   CLI   *,X'FF'             SET CC LOW                                   
         B     DATASX                                                           
DATASOK  CR    RB,RB               SET CC EQUAL                                 
DATASX   BR    R2                                                               
                                                                                
*&&                                                                             
*=====================================================================*         
*        Figure out record length based on columns                    *         
*        Return R1 = record length                                    *         
*=====================================================================*         
         USING COLD,RE                                                          
CRECLEN  SR    R1,R1               Calculate record length                      
         L     RE,FMTCOL           Point to COLUMN info                         
         ZIC   RF,FMT#COLS         Total # of COLs in FORMAT                    
*&&US                                                                           
         TM    DWNOPT3,DWNFDSDN    DSN DOWNLOAD FILE                            
         JZ    CRECL10                                                          
         AR    R1,RF                                                            
         AR    R1,RF                                                            
         AR    R1,RF                                                            
*&&                                                                             
CRECL10  TM    COLFLAGS,COLHIDE    Hidden COLUMN?                               
         JO    CRECL15               yes, don't count it                        
         CLI   COLSCLVL,0          Stack level zero                             
         JNE   CRECL15               no, don't count stacked under cols         
         IC    R0,COLSIZE          Column size of data                          
         AR    R1,R0               Add up record length                         
*                                                                               
CRECL15  AHI   RE,COLLNQ           Bump to next COLUMN                          
         JCT   RF,CRECL10                                                       
         DROP  RE                                                               
*                                                                               
         TM    DWNOPT1,DWNROWS     Are rows being down-loaded?                  
         JZ    CRECLNX             No so done figuring length                   
         USING ROWD,RE                                                          
         L     RE,FMTROW           A(Rows)                                      
         LA    RE,ROWLNQ(,RE)      Skip first row                               
         ZIC   RF,FMT#ROWS                                                      
         SHI   RF,1                                                             
CRECL20  CLI   ROWTYPE,ROWNOSHW    Hidden Row?                                  
         JE    CRECL30             skip it                                      
         AHI   R1,36               add 36 for this row                          
         TM    ROWFLAGS,ROWADDR    Is there an Addr on this row?                
         JO    CRECL25                                                          
*&&US*&& TM    ROWDAIND,ROWBDR     or BDR attribute?                            
         JZ    CRECL30             No then nothing else to add                  
CRECL25  ZIC   R4,RAD#LDWN         Get number of address lines                  
         LTR   R4,R4               to download and add 36 per                   
         JZ    CRECL30             line and add to total                        
         MHI   R4,36                                                            
         AR    R1,R4                                                            
*                                                                               
CRECL30  LA    RE,ROWLNQ(,RE)      bump to next row                             
         JCT   RF,CRECL20                                                       
*                                                                               
CRECLNX  BR    R2                                                               
         DROP  RE                                                               
DATASETX EQU   *                                                                
         EJECT ,                                                                
**********************************************************************          
* Send download reports to BDE server                                *          
**********************************************************************          
*&&UK                                                                           
         ENTRY SSB                                                              
                                                                                
         USING FMTRECD,R5                                                       
         USING DLCBD,R9                                                         
         USING ACRL2D,R7                                                        
DATABDE  NMOD1 0,**DBDE**                                                       
         LARL  R8,GLOBALS                                                       
         USING GLOBALS,R8                                                       
         L     RC,RLBASEC                                                       
         L     R7,AACRL2D                                                       
         L     R9,DWNLDCB                                                       
         L     R5,AFORMATS                                                      
         L     R3,=A(OUTWRK)                                                    
                                                                                
         CLI   DLCBACT,DLCBINIT    Initialize                                   
         BNE   DABDEGO                                                          
*                                                                               
         BAS   R2,TAPECLR          (Clears BDEDATA)                             
         L     RF,ADMASTC                                                       
         L     RF,MCSSB-MASTD(RF)                                               
         L     RE,=A(SSB)          Dummy up SSB for BDESND                      
         MVC   SSODSPAC-SSOOFF(1,RE),SSODSPAC-SSOOFF(RF)                        
*                                                                               
         LAY   R2,BDECBLK          Build BDE control block                      
         USING BDESNDD,R2                                                       
                                                                                
* BSNCNM = profile (common) name on BDE server                                  
         MVC   BSNCNM,DWNSUB                                                    
         CLI   DWNSUB,C'+'         = lower case required                        
         BNE   *+16                                                             
         MVC   BSNCNM,DWNSUB+1     strip off '+'                                
         NC    BSNCNM,=40X'BF'     convert to lower case                        
*                                                                               
         LA    R1,BSNCNM+L'BSNCNM-1                                             
         LA    R0,L'BSNCNM         Space-fill any nulls                         
DABDIN02 CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         MVI   0(R1),C' '                                                       
         AHI   R1,-1                                                            
         BCT   R0,DABDIN02                                                      
         MVC   BSNSUB(L'FMTNAME),FMTNAME    Format name                         
         OC    BSNSUB,SPACES       (Format name may include nulls)              
         MVC   BSNRID,SPACES                                                    
         MVC   BSNRID(L'QUESTOR),QUESTOR  Requestor initials                    
         MVC   BSNAPN,SPACES                                                    
         MVC   BSNAPN(L'QAPPL),QAPPL      Format code                           
         MVC   BSNFIL,SPACES                                                    
         MVC   BSNFIL(L'DWNFIL),DWNFIL    Requestor-defined file name           
         TIME  DEC                                                              
         ST    R0,DUB                                                           
         UNPK  DOUBLE(7),DUB(4)           Save HHMMSS                           
         LA    R1,BSNFIL+L'BSNFIL-1       Append:                               
         LA    R0,L'BSNFIL                                                      
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         AHI   R1,-1                                                            
         BCT   R0,*-12                                                          
         MVI   1(R1),C'.'                                                       
         L     RE,ADMASTC                                                       
         LA    RE,MCUSERID-MASTD(RE)                                            
         MVC   2(L'MCUSERID,R1),0(RE)     Userid                                
         LA    R1,L'MCUSERID(R1)                                                
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'.'                                                       
         MVC   2(2,R1),RCDATE+6           YYMMDD                                
         MVC   4(2,R1),RCDATE+3                                                 
         MVC   6(2,R1),RCDATE+0                                                 
         LA    R1,8(R1)                                                         
         MVI   0(R1),C'.'                                                       
         MVC   1(6,R1),DOUBLE             HHMMSS                                
         MVI   7(R1),C'.'                                                       
         MVC   8(L'DWNEXT,R1),DWNEXT      Requestor-defined file extn.          
         MVC   TEMP1,BSNFIL              (Save for REQDET print later)          
         DROP  R2                                                               
*                                                                               
         GOTO1 =V(BDESND),DMCB,=C'OPEN',BDECBLK,BSNDDQ,0,0,0                    
         TM    DMCB+12,X'80'                                                    
         JO    *+2                 x'80' = error                                
         MVI   SKIPSPEC,C'R'       Force request details to print to PQ         
         GOTO1 ACREPORT            <-- kills IDF, skip over this call           
*                                                                               
         LA    R2,BDEDATA          Output request details to BDE too            
         MVI   0(R2),C'"'                                                       
         MVC   01(6,R2),=C'RUN ON'                                              
         MVC   10(8,R2),RCDATE                                                  
         MVC   20(2,R2),=C'AT'                                                  
         MVC   24(2,R2),DOUBLE     HHMMSS                                       
         MVI   26(R2),C'.'                                                      
         MVC   27(2,R2),DOUBLE+2                                                
         MVI   29(R2),C'"'                                                      
         MVI   30(R2),X'0D'                                                     
         LA    R2,31(R2)                                                        
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   01(13,R2),=C'REQUEST FIELD'                                      
         MVC   22(4,R2),=C'DATA'                                                
         MVI   29(R2),C'"'                                                      
         MVI   30(R2),X'0D'                                                     
         LA    R2,31(R2)                                                        
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   01(26,R2),=26C'-'                                                
         MVI   29(R2),C'"'                                                      
         MVI   30(R2),X'0D'                                                     
         LA    R2,31(R2)                                                        
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   01(6,R2),=C'REPORT'                                              
         MVC   22(2,R2),=C'AC'                                                  
         MVC   24(2,R2),RCPROG                                                  
         MVI   29(R2),C'"'                                                      
         MVI   30(R2),X'0D'                                                     
         LA    R2,31(R2)                                                        
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   01(7,R2),=C'COMPANY'                                             
         MVC   22(2,R2),ALPHAID                                                 
         MVI   29(R2),C'"'                                                      
         MVI   30(R2),X'0D'                                                     
         LA    R2,31(R2)                                                        
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   01(9,R2),=C'UNIT CODE'                                           
         MVC   22(1,R2),QUNIT                                                   
         MVI   29(R2),C'"'                                                      
         MVI   30(R2),X'0D'                                                     
         LA    R2,31(R2)                                                        
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   01(6,R2),=C'LEDGER'                                              
         MVC   22(1,R2),QLEDGER                                                 
         MVI   29(R2),C'"'                                                      
         MVI   30(R2),X'0D'                                                     
         LA    R2,31(R2)                                                        
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   01(6,R2),=C'FORMAT'                                              
         MVC   22(8,R2),QAPPL                                                   
         MVI   30(R2),C'"'                                                      
         MVI   31(R2),X'0D'                                                     
         LA    R2,32(R2)                                                        
*                                                                               
         CLC   QACCOUNT,SPACES                                                  
         BNH   DABDIN04                                                         
         MVI   0(R2),C'"'                                                       
         MVC   01(12,R2),=C'ACCOUNT CODE'                                       
         MVC   22(12,R2),QACCOUNT                                               
         MVI   34(R2),C'"'                                                      
         MVI   35(R2),X'0D'                                                     
         LA    R2,36(R2)                                                        
*                                                                               
DABDIN04 MVI   0(R2),C'"'                                                       
         CLC   QOFFICE,SPACES                                                   
         BNH   DABDIN06                                                         
         MVC   01(6,R2),=C'OFFICE'                                              
         MVC   22(2,R2),QOFFICE                                                 
         MVI   29(R2),C'"'                                                      
         MVI   30(R2),X'0D'                                                     
         LA    R2,31(R2)                                                        
*                                                                               
DABDIN06 CLC   QSTART(L'QSTART+L'QEND),SPACES                                   
         BNH   DABDIN12                                                         
         MVI   0(R2),C'"'                                                       
         MVC   01(6,R2),=C'PERIOD'                                              
         CLC   QSTART,SPACES                                                    
         BH    *+16                                                             
         MVI   22(R2),C'-'                                                      
         LA    R2,23(R2)                                                        
         B     DABDIN08                                                         
         GOTO1 DATCON,DMCB,(0,QSTART),(17,22(R2))                               
         MVI   29(R2),C'-'                                                      
         LA    R2,30(R2)                                                        
DABDIN08 CLC   QEND,SPACES                                                      
         BH    DABDIN10                                                         
         MVI   0(R2),C'"'                                                       
         MVI   1(R2),X'0D'                                                      
         LA    R2,2(R2)                                                         
         B     DABDIN12                                                         
DABDIN10 GOTO1 DATCON,DMCB,(0,QEND),(17,0(R2))                                  
         MVI   7(R2),C'"'                                                       
         MVI   8(R2),X'0D'                                                      
         LA    R2,9(R2)                                                         
*                                                                               
         USING ACMD,RF                                                          
DABDIN12 L     RF,AMONACC                                                       
         CLC   ACMCMSTR(L'ACMCMSTR+L'ACMCMEND),SPACES                           
         BNH   DABDIN18                                                         
         MVI   0(R2),C'"'                                                       
         MVC   01(9,R2),=C'MOA RANGE'                                           
         CLC   ACMCMSTR,SPACES                                                  
         BH    DABDIN14                                                         
         MVC   21(4,R2),=C'THRU'                                                
         MVC   26(5,R2),ACMCMEND                                                
         MVI   31(R2),C'"'                                                      
         MVI   32(R2),X'0D'                                                     
         LA    R2,33(R2)                                                        
         B     DABDIN18                                                         
DABDIN14 MVC   22(5,R2),ACMCMSTR                                                
         CLC   ACMCMSTR,ACMCMEND                                                
         BNE   DABDIN16                                                         
         MVI   29(R2),C'"'                                                      
         MVI   30(R2),X'0D'                                                     
         LA    R2,31(R2)                                                        
         B     DABDIN18                                                         
DABDIN16 MVI   27(R2),C'-'                                                      
         MVC   28(5,R2),ACMCMEND                                                
         MVI   33(R2),C'"'                                                      
         MVI   34(R2),X'0D'                                                     
         LA    R2,35(R2)                                                        
         DROP  RF                                                               
*                                                                               
DABDIN18 MVI   0(R2),C'"'                                                       
         MVC   01(12,R2),=C'DRAFT OPTION'                                       
         MVC   22(L'QDRAFT,R2),QDRAFT                                           
         MVI   29(R2),C'"'                                                      
         MVI   30(R2),X'0D'                                                     
         LA    R2,31(R2)                                                        
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   01(14,R2),=C'REVERSE OPTION'                                     
         MVC   22(L'QREVERSE,R2),QREVERSE                                       
         MVI   29(R2),C'"'                                                      
         MVI   30(R2),X'0D'                                                     
         LA    R2,31(R2)                                                        
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   01(13,R2),=C'LOCKED OPTION'                                      
         MVC   22(L'QLOKFILT,R2),QLOKFILT                                       
         MVI   29(R2),C'"'                                                      
         MVI   30(R2),X'0D'                                                     
         LA    R2,31(R2)                                                        
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   01(14,R2),=C'REQUESTOR NAME'                                     
         MVC   22(L'QUESTOR,R2),QUESTOR                                         
         MVI   29(R2),C'"'                                                      
         MVI   30(R2),X'0D'                                                     
         LA    R2,31(R2)                                                        
*                                                                               
         MVI   0(R2),C'"'                                                       
         MVC   01(7,R2),=C'USER ID'                                             
         L     RE,ADMASTC                                                       
         LA    RE,MCUSERID-MASTD(RE)                                            
         MVC   22(L'MCUSERID,R2),0(RE)                                          
         MVI   32(R2),C'"'                                                      
         MVC   33(3,R2),=X'0D0D0D'  CR/CR/CR                                    
         MVC   36(1,R2),DLCXDELC   Field delimiter/space                        
         LA    R2,36(R2)                                                        
*                                                                               
         LR    RF,R2                                                            
         LA    RE,BDEDATA                                                       
         SR    RF,RE                                                            
         GOTO1 =V(BDESND),DMCB,=C'PUT',BDEDATA,(RF),0,0,0                       
         TM    DMCB+12,X'80'                                                    
         JO    *+2                 x'80' = error                                
         BAS   R2,TAPECLR          (Clears BDEDATA)                             
         LA    RF,BDEDATA          point DLFLD to local data area               
         ST    RF,DLCBAPL                                                       
         MVC   DLCXMAXL,=Y(L'BDEDATA)                                           
         LA    RF,DABDHOOK         Hook to THIS process                         
         ST    RF,DLCBAPR                                                       
         B     DABDEGO                                                          
*                                                                               
DABDHOOK DS    0H                  'Print' hook for DLFLD                       
         CLI   DLCBACT,DLCBPUT     Text to write out                            
         BE    DABDETXT                                                         
         CLI   DLCBACT,DLCBEOL     End of line                                  
         BE    DABDEPUT                                                         
         CLI   DLCBACT,DLCBEOR     End of report                                
         BE    DABDECLS                                                         
         DC    H'00'                                                            
*                                                                               
DABDETXT DS    0H                                                               
         B     DABDEX              Nothing to do                                
*                                                                               
DABDEPUT DS    0H                                                               
         LH    RF,DLCBNUMC         l'data to put                                
         LA    RE,BDEDATA-1(RF)                                                 
         CLI   0(RE),C';'          Test logical EOL                             
         BNE   *+8                                                              
         MVI   0(RE),X'0D'         Replace ';' with CR                          
         GOTO1 =V(BDESND),DMCB,=C'PUT',BDEDATA,(RF),0,0,0                       
         TM    DMCB+12,X'80'                                                    
         JO    *+2                 x'80' = error                                
         BAS   R2,TAPECLR          (Clears BDEDATA)                             
         B     DABDEX                                                           
*                                                                               
DABDECLS DS    0H                                                               
         GOTO1 =V(BDESND),DMCB,=C'CLOSE',0,0,0,0,0                              
         TM    DMCB+12,X'80'                                                    
         JO    *+2                 x'80' = error                                
         USING BIGPRNTD,R4                                                      
         L     R4,VBIGPRNT         Print file details to PQ                     
         MVC   XP+1(5),=C'File:'                                                
         MVC   XPSECOND+1(L'TEMP1),TEMP1                                        
         MVC   XPTHIRD+1(32),=C'Has been sent to your BDE server'               
         GOTO1 ACREPORT                                                         
         B     DABDEX                                                           
*                                                                               
DABDEGO  GOTO1 =V(DLFLD),(R9)                                                   
                                                                                
DABDEX   CR    R1,R1               DUMMY FOR IDF                                
         XIT1                                                                   
*&&                                                                             
         DROP  R8,RB                                                            
**********************************************************************          
* Send download reports to USS server                                *          
**********************************************************************          
         USING FMTRECD,R5                                                       
         USING DLCBD,R9                                                         
         USING ACRL2D,R7                                                        
DATAUSS  NMOD1 0,**DAUS**,END=DATAUSSX                                          
         LARL  R8,GLOBALS                                                       
         USING GLOBALS,R8                                                       
         L     RC,RLBASEC                                                       
         L     R7,AACRL2D                                                       
         L     R9,DWNLDCB                                                       
         L     R5,AFORMATS                                                      
         L     R3,=A(OUTWRK)                                                    
                                                                                
         CLI   DLCBACT,DLCBINIT    Initialize                                   
         JNE   DAUSSGO                                                          
*                                                                               
         BRAS  R2,TAPECLR          (Clears USSDATA)                             
         L     RF,ADMASTC                                                       
         L     RF,MCSSB-MASTD(RF)                                               
         L     RE,=A(SSB)          Dummy up SSB for USSIO                       
         MVC   SSODSPAC-SSOOFF(1,RE),SSODSPAC-SSOOFF(RF)                        
*                                                                               
         TIME  DEC                                                              
         ST    R0,DUB                                                           
         UNPK  DOUBLE(7),DUB(4)           Save HHMMSS                           
*                                                                               
         MVI   USSFNML,0                                                        
         MVC   USSFNM,SPACES                                                    
                                                                                
*        File name: AID_UserID_ReportID_FormatCode_ReqID_YYMMDD_HHMMSS          
         MVC   USSFNM(L'ALPHAID),ALPHAID  Agency Alpha ID                       
         MVI   USSFNM+L'ALPHAID,C'_'                                            
         LA    R1,USSFNM+L'ALPHAID+1                                            
*                                                                               
         L     RF,ADMASTC                                                       
         LA    RF,MCUSERID-MASTD(RF)                                            
         MVC   0(L'MCUSERID,R1),0(RF)     User ID                               
         LA    R1,L'MCUSERID-1(,R1)                                             
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         JCT   R1,*-8                                                           
         MVI   1(R1),C'_'                                                       
*                                                                               
         MVC   2(2,R1),=C'AC'             Report ID                             
         MVC   4(L'RCPROG,R1),RCPROG                                            
         MVI   4+L'RCPROG(R1),C'_'                                              
         LA    R1,5+L'RCPROG(,R1)                                               
*                                                                               
         MVC   0(L'QAPPL,R1),QAPPL        Format code                           
         LA    R1,L'QAPPL-1(,R1)                                                
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         JCT   R1,*-8                                                           
         MVI   1(R1),C'_'                                                       
*                                                                               
         MVC   2(L'QUESTOR,R1),QUESTOR    Requestor initials                    
         LA    R1,L'QUESTOR+1(,R1)                                              
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         JCT   R1,*-8                                                           
         MVI   1(R1),C'_'                                                       
*                                                                               
         MVI   2(R1),C'D'                 Date: Dyymmdd                         
         MVC   3(2,R1),RCDATE+6                                                 
*&&UK                                                                           
         MVC   5(2,R1),RCDATE+3           UK Date: yyddmm                       
         MVC   7(2,R1),RCDATE+0                                                 
*&&                                                                             
*&&US                                                                           
         MVC   5(2,R1),RCDATE+0           US Date: yymmdd                       
         MVC   7(2,R1),RCDATE+3                                                 
*&&                                                                             
         MVC   9(2,R1),=C'_T'             Time: Thhmmss                         
         MVC   11(6,R1),DOUBLE                                                  
         LA    R1,17(,R1)                 End of file name                      
         LA    RF,USSFNM                                                        
         SR    R1,RF                                                            
         STC   R1,USSFNML                 Length of file name                   
         LR    RF,R1                                                            
*                                                                               
         XR    R0,R0                                                            
         TM    DWNOPT3,DWNTEDI     Test EDIHUB transmission request             
         JZ    *+8                                                              
         LA    R0,C'E'             SENT TO EDIHUB                               
         GOTO1 =V(USSIO),DMCB,CREATE,((R0),USSFNM),(RF),0,0,0                   
         TM    DMCB+12,X'80'                                                    
         JO    *+2                 x'80' = error                                
         MVI   SKIPSPEC,C'R'       Force request details to print to PQ         
*&&UK*&& GOTO1 ACREPORT            <-- kills IDF, skip over this call           
*                                                                               
DATAUSS2 BRAS  R2,TAPECLR          (Clears TAPEDATA)                            
         LA    RF,TAPEDATA         point DLFLD to local data area               
         ST    RF,DLCBAPL                                                       
         MVC   DLCXMAXL,=Y(L'TAPEDATA)                                          
         LA    RF,DAUSHOOK         Hook to THIS process                         
         ST    RF,DLCBAPR                                                       
         J     DAUSSGO                                                          
*                                                                               
DAUSHOOK DS    0H                  'Print' hook for DLFLD                       
         CLI   DLCBACT,DLCBPUT     Text to write out                            
         JE    DAUSSTXT                                                         
         CLI   DLCBACT,DLCBEOL     End of line                                  
         JE    DAUSSPUT                                                         
         CLI   DLCBACT,DLCBEOR     End of report                                
         JE    DAUSSCLS                                                         
         DC    H'00'                                                            
*                                                                               
DAUSSTXT DS    0H                                                               
         J     DAUSSX              Nothing to do                                
*                                                                               
DAUSSPUT DS    0H                                                               
         LH    RF,DLCBNUMC         l'data to put                                
         LA    R1,TAPEDATA(RF)     point to end of line                         
         SHI   R1,1                                                             
         TM    DWNOPT4,DWNMSFTP    Test send report to MS SFTP server           
         JZ    *+12                                                             
         MVI   0(R1),X'0D'         USE X'0D' plus X'15' for M/S server          
         LA    R1,1(,R1)                                                        
*                                                                               
         MVI   0(R1),X'15'         USE X'15' for Unix                           
         LA    RF,TAPEDATA                                                      
         SR    R1,RF                                                            
         LA    RF,1(,R1)           Length of data                               
*                                                                               
         GOTO1 =V(USSIO),DMCB,PUT,TAPEDATA,(RF),0,0,0                           
         TM    DMCB+12,X'80'                                                    
         JO    *+2                 x'80' = error                                
         BRAS  R2,TAPECLR          (Clears TAPEDATA)                            
         J     DAUSSX                                                           
*                                                                               
DAUSSCLS DS    0H                                                               
         GOTO1 =V(USSIO),DMCB,CLOSE,0,0,0,0,0                                   
         TM    DMCB+12,X'80'                                                    
         JO    *+2                 x'80' = error                                
*                                                                               
         TM    DWNOPT3,DWNTEDI     Test EDIHUB transmission request             
         JNZ   DAUSSC04                                                         
*        Add SFTP url info for this file to USS                                 
         LA    RF,L'SFTPFILN                                                    
         GOTO1 =V(USSIO),DMCB,OPEN,SFTPFILN,(RF),0,0,0                          
         TM    DMCB+12,X'80'                                                    
         JO    *+2                 x'80' = error                                
*                                                                               
         BRAS  R2,TAPECLR          (Clears TAPEDATA)                            
         MVC   TAPEDATA(L'USSFNM),USSFNM FILE NAME                              
         LLC   RF,USSFNML                                                       
         LA    RF,1(,RF)                                                        
         LA    R1,TAPEDATA(RF)                                                  
         MVC   0(L'DWNFIL,R1),DWNFIL     Requestor-defined SFTP                 
*                                                                               
         LA    R1,L'DWNFIL-1(,R1)                                               
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         JCT   R1,*-8                                                           
         TM    DWNOPT4,DWNMSFTP    Test send report to MS SFTP server           
         JZ    *+12                                                             
         MVI   1(R1),X'0D'         USE X'0D' plus X'15' for M/S server          
         LA    R1,1(,R1)                                                        
*                                                                               
         MVI   1(R1),X'15'         Add a NL at the end                          
         LA    RF,TAPEDATA                                                      
         SR    R1,RF                                                            
         LA    RF,2(,R1)           Length of SFPT url info line                 
*                                                                               
         GOTO1 =V(USSIO),DMCB,PUT,TAPEDATA,(RF),0,0,0                           
         TM    DMCB+12,X'80'                                                    
         JO    *+2                 x'80' = error                                
*                                                                               
         GOTO1 =V(USSIO),DMCB,CLOSE,0,0,0,0,0                                   
         TM    DMCB+12,X'80'                                                    
         JO    *+2                 x'80' = error                                
         J     DAUSSC08                                                         
*                                                                               
DAUSSC04 DS    0H                  ADD EDIHUB MESSAGE TO MQ                     
         MVC   EDIAGYID,EDIHUBID   EDIHUB                                       
         MVC   EDIDATE,TODAYC                                                   
         LLC   RF,TODAYC                                                        
         CHI   RF,X'F9'                                                         
         JNH   *+12                                                             
         SHI   RF,X'0A'            CONVERT X'FB' TO X'F1' ETC..                 
         STC   RF,EDIDATE                                                       
*                                                                               
         TIME  DEC                 Get the time of this extract                 
         ST    R0,DUB                                                           
         UNPK  DOUBLE(7),DUB(4)    Save HHMMSS                                  
         MVC   EDITIME,DOUBLE      HHMMSS                                       
         L     RF,ADCOMFAC                                                      
         L     RF,CMQRPT-COMFACSD(RF)                                           
         GOTOR (RF),DMCB,OPEN,EDIMQHDR,(X'A0',0)                                
         CLI   8(R1),0             All okay ?                                   
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   EDIFILE,USSFNM                                                   
         LHI   R4,EDIMSGLQ                                                      
         GOTOR (RF),DMCB,PUT,EDIRECTY,(R4)                                      
         CLI   8(R1),0             All okay ?                                   
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (RF),DMCB,CLOSE                                                  
         CLI   8(R1),0             All okay ?                                   
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING BIGPRNTD,R4                                                      
DAUSSC08 L     R4,VBIGPRNT         Print file details to PQ                     
         MVC   XP+1(5),=C'File:'                                                
         MVC   XPSECOND+1(L'USSFNM),USSFNM                                      
         TM    DWNOPT3,DWNTEDI     Test EDIHUB transmission request             
         JO    *+14                                                             
         MVC   XPTHIRD+1(32),=C'has been sent to your USS server'               
         J     *+10                                                             
         MVC   XPTHIRD+1(32),=C'has been sent to your MQ server '               
*&&UK*&& GOTO1 ACREPORT                                                         
         J     DAUSSX                                                           
*                                                                               
DAUSSGO  GOTO1 =V(DLFLD),(R9)                                                   
                                                                                
DAUSSX   CR    R1,R1               DUMMY FOR IDF                                
         XIT1                                                                   
*                                                                               
         EJECT ,                                                                
                                                                                
*=====================================================================*         
* Init and clear TAPEDATA/BDEDATA, TAPEDSP/BDEDSP after output        *         
*=====================================================================*         
TAPECLR  LA    RE,TAPEDATA                                                      
         LHI   RF,L'TAPEDATA                                                    
         SR    R1,R1                                                            
         ICM   R1,8,SPACES         Pad character ' '                            
         MVCL  RE,R0                                                            
         XC    TAPEDSP,TAPEDSP                                                  
*&&US                                                                           
         L     RE,ADWNLINE                                                      
         LHI   RF,L'DWNLINE                                                     
         SR    R1,R1                                                            
         ICM   R1,8,SPACES         Pad character ' '                            
         MVCL  RE,R0                                                            
*&&                                                                             
         BR    R2                                                               
                                                                                
         EJECT                                                                  
                                                                                
         USING BIGPRNTD,R4                                                      
         USING ACRL2D,R7                                                        
PUTPQDSN L     R4,VBIGPRNT                                                      
         L     R7,AACRL2D                                                       
         MVC   XP+4(5),=C'*HDR*'                                                
         MVC   XP+9(6),=C'EDICT='                                               
         MVI   XP+34,C'W'          Wide 132 report. So how about 198 ?          
         MVI   XP+37,C'D'          Let it know it is a dataset                  
         MVC   XP+15(8),DESTID     EDICT KEY                                    
         GOTO1 ACREPORT                                                         
                                                                                
         MVC   XP(5),=C'++DDS'                                                  
         MVC   XP+6(3),=C'ACW'                                                  
         MVC   XP+9(2),ALPHAID                                                  
         MVC   XP+11(3),=C'TRN'                                                 
         MVI   XP+15,C'A'                                                       
         MVC   XP+16(2),QPROG                                                   
         MVI   XP+18,C','                                                       
         MVC   XP+19(8),FMTCODE                                                 
         GOTO1 ACREPORT                                                         
                                                                                
         MVC   XP(5),=C'++DDS'                                                  
         MVC   XP+11(3),=C'DSN'                                                 
         MVC   XP+15(44),FULLDSN                                                
         GOTO1 ACREPORT                                                         
                                                                                
         CLC   DWNSUB,SPACES                                                    
         JNH   PUTPQ20                                                          
         MVC   XP(5),=C'++DDS'                                                  
         MVC   XP+11(3),=C'SUB'                                                 
         MVC   XP+15(60),DWNSUB                                                 
         GOTO1 ACREPORT                                                         
         J     PUTPQ30                                                          
                                                                                
PUTPQ20  MVC   XP(5),=C'++DDS'                                                  
         MVC   XP+11(8),=C'SUB CLA('                                            
         MVC   XP+19(L'DESTID),DESTID                                           
                                                                                
         LA    RF,L'DESTID-3                                                    
         LA    RE,XP+19+L'DESTID-1                                              
PUTPQ24  CLI   0(RE),C' '          Clear alpha code from end                    
         JH    PUTPQ26                                                          
         SHI   RE,1                                                             
         JCT   RF,PUTPQ24                                                       
                                                                                
PUTPQ26  SHI   RE,1                                                             
         MVC   0(2,RE),SPACES                                                   
                                                                                
         MVC   XP+27(39),=C'),CHA(3),ACC(IGWU),USE(U9996551),MOD(1)'            
         GOTO1 ACREPORT                                                         
                                                                                
PUTPQ30  CLC   DWNFIL,SPACES       FILE=                                        
         JNH   PUTPQ40                                                          
         MVC   XP(5),=C'++DDS'                                                  
*&&US*&& MVC   XP+11(3),=C'FIL'                                                 
*&&UK*&& MVC   XP+11(4),=C'FILE'                                                
         MVC   XP+15(L'DWNFIL),DWNFIL                                           
         GOTO1 ACREPORT                                                         
                                                                                
PUTPQ40  CLC   DWNEXT,SPACES       EXT=                                         
         JNH   PUTPQ50                                                          
         MVC   XP(5),=C'++DDS'                                                  
         MVC   XP+11(3),=C'EXT'                                                 
         MVC   XP+15(L'DWNEXT),DWNEXT                                           
         GOTO1 ACREPORT                                                         
                                                                                
PUTPQ50  MVC   XP+1(40),=C'*TAPE GENERATED, SENDING DATA VIA EDICT*'            
         MVI   XP+43,X'5E'         Move in semi colon for EDICT scan            
         GOTO1 ACREPORT                                                         
                                                                                
         L     R3,=A(OUTWRK)                                                    
         SR    R1,R1                                                            
         ICM   R1,3,62(R3)         Block size                                   
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         MVC   XP+1(20),=C'BLOCK SIZE=00000000'                                 
         UNPK  XP+12(8),DUB                                                     
         MVI   XP+22,X'5E'         Move in semi colon for EDICT scan            
         GOTOR ACREPORT                                                         
                                                                                
         SR    R1,R1                                                            
         ICM   R1,3,82(R3)         Record length                                
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         MVC   XP+1(20),=C'RECORD LENGTH=000000'                                
         UNPK  XP+15(6),DUB                                                     
         MVI   XP+22,X'5E'         Move in semi colon for EDICT scan            
         GOTOR ACREPORT                                                         
         BR    R2                                                               
*                                                                               
         DROP  R4,R5,R7,R8                                                      
DATAUSSX EQU   *                                                                
         EJECT                                                                  
GLOBALS  DS    0D                  ** Global literals **                        
*                                                                               
         LTORG                                                                  
                                                                                
DDNAME   DC    CL8'OUTWRK'                                                      
DDSNAME1 DC    CL30'ACCTAPE.ACPPAA.FORMAT'                                      
DDSNAME2 DC    CL30'ACCTAPE.ACPPAAAA.FORMAT'                                    
FULLDSN  DC    CL44' '                                                          
DSETACT  DS    CL1                                                              
BDEDSP   DS    0H                                                               
TAPEDSP  DS    H                                                                
BDEDATA  DS    0XL1200                                                          
TAPEDATA DS    XL1200                                                           
*                                                                               
CREATE   DC    C'CREATE'                                                        
OPEN     DC    C'OPEN'                                                          
PUT      DC    C'PUT'                                                           
CLOSE    DC    C'CLOSE'                                                         
SFTPFILN DC    C'DDSPROD.SFTP.FILES.INFO'                                       
USSFNML  DC    X'00'                                                            
USSFNM   DC    CL60' '                                                          
*&&UK                                                                           
BDECBLK  DC    (BSNDDQ)X'00'                                                    
*&&                                                                             
*                                                                               
         PRINT DATA                                                             
COL1     EQU   1-1                                                              
COL17    EQU   17-1                                                             
COL23    EQU   23-1                                                             
COL26    EQU   26-1                                                             
COL30    EQU   30-1                                                             
COL46    EQU   46-1                                                             
COL122   EQU   122-1                                                            
EDIMQLNQ EQU   200                                                              
EDIMQMSG DC    (EDIMQLNQ)C' '      200 byte space filled EDIHUB msg             
                                                                                
                                                                                
* Header                                                                        
         ORG   EDIMQMSG+COL1                                                    
EDIMQHDR DC    16C'*'              16 byte EDISCHED routing label               
         ORG   EDIMQHDR+COL1                                                    
EDIROUTE DC    C'EDIHUBFT'         EDISCHED routing label                       
                                                                                
* Message                                                                       
         ORG   EDIMQMSG+COL17                                                   
EDIRECTY DC    C'DANOT1'           Record type                                  
         ORG   EDIMQMSG+COL23                                                   
EDISYS   DC    C'ACC'              System                                       
         ORG   EDIMQMSG+COL26                                                   
EDIAGYID DC    C'0000'             Agency ID                                    
         ORG   EDIMQMSG+COL30                                                   
EDISUBTY DC    C'BILLING'          Message subtype                              
*DISUBTY DC    C'ACCDATA'          Message subtype                              
         ORG   EDIMQMSG+COL46                                                   
EDIDATE  DC    C'YYMMDD'           Date                                         
EDITIME  DC    C'HHMMSS'           Time                                         
         ORG   EDIMQMSG+COL122                                                  
EDIFILE  DC    CL(L'USSFNM)'USS.USS.' MVS or USS file name                      
         ORG   ,                                                                
EDIMSGLQ EQU   *-EDIRECTY          L' msg                                       
         PRINT NODATA                                                           
                                                                                
*                                                                               
         DS    0D                                                               
*FASSBOFF                                                                       
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
*                                                                               
CHARTBL  DS    0H                                                               
         DC    C'#$+*'                                                          
         DC    X'FF'                                                            
*                                                                               
         TITLE 'HOOK ACREPORT PRIOR TO GOING'                                   
***********************************************************************         
*  ACREPORT HOOK                                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING ACWORKD,RC                                                       
         USING ACRLD,RA                                                         
         USING BIGPRNTD,R4                                                      
         USING FMTRECD,R5                                                       
         USING BOXD,R6                                                          
CUR      USING RECXD,RECXSRT                                                    
PRV      USING RECXD,PRVXSRT                                                    
*                                                                               
REPORTER NMOD1 0,**RPHK**                                                       
         L     RC,RLBASEC                                                       
         L     R4,VBIGPRNT                                                      
         L     R6,ADBXAREA                                                      
         TM    DWNOPT2,DWNDSN      Transmit Dataset ?                           
         BO    REPORTX             Don't allow this to print the report         
*&&UK                                                                           
         TM    DWNOPT3,DWNTBDE     Test transmit BDE                            
         BO    REPORTX             Don't allow this to print the report         
*&&                                                                             
         TM    DDLNKIND,DDLNKON                                                 
         BO    REPORT10                                                         
         LA    R1,STDPGWD          SET TO 198 EACH TIME IN                      
         ST    R1,BOXWIDTH                                                      
*&&DO                              Debug code add when needed                   
         CLI   QOPT3,C'Q'                                                       
         BNE   REPORT10                                                         
         CLI   DEBUGPRT,0          DEBUG CODE                                   
         BZ    REPORT10                                                         
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P+2(5),=C'LINE='                                                 
         SR    R2,R2                                                            
         IC    R2,LINE                                                          
         CVD   R2,DUB                                                           
         CURED DUB,(3,P+7),0,DECS=NO                                            
*                                                                               
         MVC   P+14(4),=C'REP='                                                 
         IC    R2,CUR.RECRPT#                                                   
         CVD   R2,DUB                                                           
         CURED DUB,(2,P+18),0,DECS=NO                                           
*                                                                               
         MVC   P+22(4),=C'PRV='                                                 
         IC    R2,PRV.RECRPT#                                                   
         CVD   R2,DUB                                                           
         CURED DUB,(2,P+26),0,DECS=NO                                           
*                                                                               
         ST    R5,FULL                                                          
         L     R5,AFORMATS                                                      
         MVC   P+30(4),=C'FM1='                                                 
         GOTO1 HEXOUT,DMCB,FMTRCAP,P+34,1,0,0                                   
         LA    R5,FMTLNQ(,R5)                                                   
         MVC   P+40(4),=C'FM2='                                                 
         GOTO1 HEXOUT,DMCB,FMTRCAP,P+44,1,0,0                                   
         MVC   P+50(4),=C'FM3='                                                 
         LA    R5,FMTLNQ(,R5)                                                   
         GOTO1 HEXOUT,DMCB,FMTRCAP,P+54,1,0,0                                   
         L     R5,FULL                                                          
         MVC   P+60(9),=C'FORCEHED='                                            
         MVC   P+69(1),FORCEHED                                                 
         MVC   P+75(3),=C'SK='                                                  
         SR    R2,R2                                                            
         IC    R2,SKIPLINE                                                      
         CVD   R2,DUB                                                           
         CURED DUB,(2,P+78),0,DECS=NO                                           
*                                                                               
*        MVC   TMPSKIP,SKIPLINE                                                 
*        MVC   TMPDETMX,DETMAX                                                  
*                                                                               
         MVC   WORK,SPACES                                                      
         SR    R2,R2                                                            
         IC    R2,DEBUGRT#                                                      
         MVI   DEBUGRT#,0          CLEAR DEBUG LOCATION                         
         CVD   R2,DUB                                                           
         CURED DUB,(4,WORK+8),0,DECS=NO                                         
         MVC   WORK(7),=C'ROUT# ='                                              
         IC    R2,DEBUGPRT                                                      
         MVI   DEBUGPRT,0          CLEAR DEBUG LOCATION                         
         CVD   R2,DUB                                                           
         CURED DUB,(4,WORK+23),0,DECS=NO                                        
         MVC   WORK+15(8),=C'DEBUG# ='                                          
         GOTO1 PRNTBL,DMCB,(30,WORK),P,1,L'P,=CL2'1C'                           
*&&                                                                             
REPORT10 GOTO1 ACREPORT                                                         
                                                                                
REPORTX  XMOD1                                                                  
         DROP  R4,R5,R6                                                         
         DROP  CUR,PRV                                                          
         SPACE 2                                                                
         LTORG                                                                  
         TITLE 'RESOLVE LANGUAGE SOFT TABLES'                                   
***********************************************************************         
*  TABLES USED WHEN PRINTING OUT REPORT                               *         
***********************************************************************         
         SPACE 1                                                                
         USING ACWORKD,RC                                                       
         USING ACRLD,RA                                                         
LANGSOFT NMOD1 0,**SOFT**                                                       
         L     RC,RLBASEC                                                       
         TM    DDLNKIND,DDLNKON    Need to re-translate                         
         BO    LANGSF10                                                         
         CLI   RLLANG,YES                                                       
         BE    LNGSFT90            Already translated                           
*                                                                               
LANGSF10 MVI   COMMA,C','                                                       
         CLI   RCLANG,LANGGER                                                   
         BNE   *+8                                                              
         MVI   COMMA,C'#'                                                       
                                                                                
         USING TRLTD,R5                                                         
         L     R8,=A(TRLTAB)                                                    
         LA    R0,TRLTAB#          Number of tables                             
LANGSF20 L     R5,0(,R8)           Load table to translate                      
         ZIC   R3,4(,R8)           Get table length                             
*                                                                               
LANGSF25 CLI   0(R5),EOT                                                        
         BE    LANGSF50            Finished                                     
         OC    TRLDD#,TRLDD#       Need to translate ?                          
         BZ    LANGSF40            No                                           
         MVI   TRLDDESC,ESC#LFJT   Escape sequense                              
         MVC   TRLDDNUM,TRLDD#     Move in dictionary number                    
         LR    R1,R3               Length of table entry                        
         SHI   R1,4                Less header info                             
         STC   R1,TRLDDLEN         Store length                                 
         LA    R6,TRLDATA                                                       
         GOTO1 ADDICTAT,DMCB,C'SU  ',(R6),0                                     
         CLI   5(R8),NO            Comma delimited entries ?                    
         BE    LANGSF40            No                                           
*                                                                               
         LR    R1,R6               R1 = A(translated data)                      
         LR    RF,R3               Table entry length                           
         SHI   RF,4                Less 4 for max length of data                
         AR    R1,RF               R1 = A(End of data)                          
         BCTR  R1,0                R1 = A(Last byte of data)                    
         CLI   0(R1),C' '                                                       
         BNH   *+6                                                              
         DC    H'00'               No room for comma                            
                                                                                
LANGSF30 CLI   0(R1),C' '                                                       
         BH    LANGSF35                                                         
         BCTR  R1,0                Decrease location of string                  
         BCT   RF,LANGSF30         Decrease size     of string                  
         DC    H'00'                                                            
*                                                                               
LANGSF35 MVC   1(1,R1),COMMA                                                    
         LA    RF,1(,RF)           Add one for comma                            
         STC   RF,TRLDLEN          Save off new data length                     
                                                                                
LANGSF40 AR    R5,R3               NEXT ENTRY                                   
         B     LANGSF25                                                         
         DROP  R5                                                               
*                                                                               
LANGSF50 AHI   R8,TRLTABQ          Next entry                                   
         BCT   R0,LANGSF20                                                      
                                                                                
LNGSFT90 XMOD1                                                                  
         SPACE 1                                                                
COMMA    DS    CL1                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* Table of tables to translate                                        *         
***********************************************************************         
TRLTAB   DS    0F                                                               
         DC    A(ACCSTAB),AL1(ACCSTABQ,YES,0,0)                                 
TRLTABQ  EQU   *-TRLTAB                                                         
         DC    A(TRSTAB1),AL1(TRSTAB1Q,YES,0,0)                                 
         DC    A(TRSTAB2),AL1(TRSTAB1Q,YES,0,0)                                 
         DC    A(TRSTAB3),AL1(TRSTAB1Q,YES,0,0)                                 
         DC    A(TRSTAB4),AL1(TRSTAB1Q,YES,0,0)                                 
*&&US*&& DC    A(RCVSTAB),AL1(RCVSTABQ,YES,0,0)                                 
*&&US*&& DC    A(VENSTAB),AL1(VENSTABQ,YES,0,0)                                 
         DC    A(BTYPTAB),AL1(BTYPLNQ,NO,0,0)                                   
         DC    A(LOCSTAB),AL1(LOCSTABQ,NO,0,0)                                  
         DC    A(EDTPAYTB),AL1(EDTPAYTQ,NO,0,0)                                 
TRLTAB#  EQU   (*-TRLTAB)/TRLTABQ                                               
         EJECT                                                                  
***********************************************************************         
* Account status table                                                          
***********************************************************************         
ACCSTAB  DS    0C                                                               
         DC    AL1(ACCCLSE,2),AL2(0),CL8'C,'                                    
ACCSTABQ EQU   *-ACCSTAB                                                        
         DC    AL1(ACCLOCK,2),AL2(0),CL8'L,'                                    
         DC    AL1(ACCXJOB,4),AL2(0),CL8'EXP,'                                  
         DC    AL1(ACCAPPE,4),AL2(0),CL8'APP,'                                  
         DC    AL1(ACCDJOB,6),AL2(0),CL8'DRAFT,'                                
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* Transaction status table 1                                                    
***********************************************************************         
TRSTAB1  DS    0C                                                               
*&&US*&& DC    AL1(CURHELD,5),AL2(0),CL8'HELD,'                                 
*&&UK*&& DC    AL1(CURAUTH,5),AL2(0),CL8'AUTH,'                                 
TRSTAB1Q EQU   *-TRSTAB1                                                        
*&&UK*&& DC    AL1(CURHELD,5),AL2(0),CL8'HELD,'                                 
*&&US*&& DC    AL1(CURQURD,8),AL2(0),CL8'QUERIED,'                              
         DC    AL1(CURREVL,4),AL2(0),CL8'REV,'                                  
         DC    AL1(CURBILL,4),AL2(0),CL8'BIL,'                                  
*&&US*&& DC    AL1(CURPBIL,5),AL2(0),CL8'PBIL,'                                 
         DC    AL1(CURCSHD,3),AL2(0),CL8'CD,'                                   
         DC    AL1(CURCOMM,4),AL2(0),CL8'COM,'                                  
         DC    AL1(CURNONC,3),AL2(0),CL8'NC,'                                   
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* Tranaction status table 1 (Recievables)                                       
***********************************************************************         
TRSTAB2  DS    0C                                                               
         DC    AL1(CURHELD,5),AL2(0),CL8'HELD,'                                 
TRSTAB2Q EQU   *-TRSTAB2                                                        
         DC    AL1(CURREVL,4),AL2(0),CL8'REV,'                                  
         DC    AL1(CURCSHD,3),AL2(0),CL8'CD,'                                   
         DC    AL1(CURQURD,8),AL2(0),CL8'QUERIED,'                              
*&&UK*&& DC    AL1(CURCLRD,8),AL2(0),CL8'CLEARED,'                              
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* Tranaction status table 2 (Manpower)                                          
***********************************************************************         
TRSTAB3  DS    0C                                                               
         DC    AL1(CURSVED,2),AL2(0),CL8'*,'                                    
TRSTAB3Q EQU   *-TRSTAB3                                                        
         DC    AL1(CURWOFF,2),AL2(0),CL8'W,'                                    
         DC    AL1(CURADJT,2),AL2(0),CL8'A,'                                    
         DC    AL1(CURTMPO,6),AL2(0),CL8'TEMPO,'                                
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* Tranaction status table 4 (Cash)                                              
***********************************************************************         
TRSTAB4  DS    0C                                                               
         DC    AL1(CURREVL,4),AL2(0),CL8'REV,'                                  
TRSTAB4Q EQU   *-TRSTAB4                                                        
         DC    AL1(CURRCON,2),AL2(0),CL8'C,'                                    
         DC    AL1(CURVOID,5),AL2(0),CL8'VOID,'                                 
         DC    AL1(EOT)                                                         
*&&US                                                                           
***********************************************************************         
* Transaction status table 3 (Receivables)                                      
***********************************************************************         
RCVSTAB  DS    0C                                                               
         DC    AL1(TRXSRQRD,8),AL2(0),CL8'QUERIED,'                             
RCVSTABQ EQU   *-RCVSTAB                                                        
         DC    AL1(TRXSRHLD,5),AL2(0),CL8'HELD,'                                
         DC    AL1(TRNSREV,4),AL2(0),CL8'REV,'                                  
         DC    AL1(CFWRSWO,4),AL2(0),CL8'W/O,'                                  
         DC    AL1(CFWRSOF,4),AL2(0),CL8'OFF,'                                  
         DC    AL1(CFWRSTR,4),AL2(0),CL8'TRF,'                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
* Vendor status table                                                           
***********************************************************************         
VENSTAB  DS    0C                                                               
         DC    AL1(TRNSURG,4),AL2(0),CL8'URG,'                                  
VENSTABQ EQU   *-VENSTAB                                                        
         DC    AL1(TRNSREV,4),AL2(0),CL8'REV,'                                  
         DC    AL1(TRNSHOLD,5),AL2(0),CL8'HELD,'                                
         DC    AL1(TRNSAPPR,4),AL2(0),CL8'APP,'                                 
         DC    AL1(EOT)                                                         
*&&                                                                             
***********************************************************************         
* Estimate status table                                                         
***********************************************************************         
*&&US                                                                           
ESTSTAB  DS    0C                                                               
         DC    AL1(ESTME,3),AL2(0),CL8'ME,'                                     
ESTSTABQ EQU   *-ESTSTAB                                                        
         DC    AL1(ESTRE,3),AL2(0),CL8'RE,'                                     
         DC    AL1(ESTCA,3),AL2(0),CL8'CA,'                                     
         DC    AL1(ESTIP,3),AL2(0),CL8'IP,'                                     
         DC    AL1(ESTSI,3),AL2(0),CL8'SI,'                                     
         DC    AL1(ESTSC,3),AL2(0),CL8'SC,'                                     
         DC    AL1(ESTIA,3),AL2(0),CL8'IA,'                                     
         DC    AL1(EOT)                                                         
*&&                                                                             
ESTTAB   DS    0C                                                               
         DC    C'P',AL1(12),AL2(0),CL22'In progress,'                           
ESTTABQ  EQU   *-ESTTAB                                                         
         DC    C'B',AL1(21),AL2(0),CL22'Submitted (internal),'                  
         DC    C'S',AL1(19),AL2(0),CL22'Submitted (client),'                    
         DC    C'A',AL1(18),AL2(0),CL22'Approved (client),'                     
         DC    C'R',AL1(9),AL2(0),CL22'Rejected,'                               
         DC    C'D',AL1(8),AL2(0),CL22'Deleted,'                                
         DC    C'I',AL1(20),AL2(0),CL22'Approved (internal),'                   
         DC    C'M',AL1(7),AL2(0),CL22'Merged,'                                 
         DC    AL1(EOT)                                                         
*                                                                               
ESTTABG  DS    0C                                                               
         DC    C'P',AL1(8),AL2(0),CL22'Entwurf,'                                
         DC    C'B',AL1(19),AL2(0),CL22'Abgegeben (intern),'                    
         DC    C'S',AL1(18),AL2(0),CL22'Abgegeben (Kunde),'                     
         DC    C'A',AL1(18),AL2(0),CL22'Genehmigt (Kunde),'                     
         DC    C'R',AL1(10),AL2(0),CL22'Abgelehnt,'                             
         DC    C'D',AL1(10),AL2(0),CL22'Geloescht,'                             
         DC    C'I',AL1(19),AL2(0),CL22'Genehmigt (intern),'                    
         DC    C'M',AL1(16),AL2(0),CL22'Zusammengefasst,'                       
         DC    AL1(EOT)                                                         
***********************************************************************         
* Billing types table                                                           
***********************************************************************         
BTYPTAB  DS    0C                                                               
         DC    C'A',CL1'A',AL2(AC#RSATB),CL11' '          Auto bill             
BTYPTABQ EQU   *-BTYPTAB                                                        
         DC    C'G',CL1'G',AL2(AC#GRPBL),CL11' '          Group billing         
         DC    C'U',CL1'U',AL2(AC#RSUNB),CL11' '          Unbillable            
         DC    C'L',AL1(TRNBTALL),AL2(AC#ALCT2),CL11' '   Allocated             
         DC    C'C',AL1(TRNBTCLI),AL2(AC#RSCLB),CL11' '   Client bill           
         DC    C'P',AL1(TRNBTPRO),AL2(AC#RSPGB),CL11' '   Progressive           
         DC    C'T',AL1(TRNBTTOT),AL2(AC#RSTLB),CL11' '   Total bill            
         DC    C'E',AL1(TRNBTPER),AL2(AC#RSESB),CL11' '   Estimate bill         
         DC    C'M',AL1(TRNBTMAN),AL2(AC#RSMNB),CL11' '   Manual bill           
         DC    C'S',AL1(TRNBTSPE),AL2(AC#RSSPB),CL11' '   Special amt           
         DC    C'1',AL1(TRNBTONE),AL2(AC#RS1BL),CL11' '   One line              
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* Location status table                                                         
***********************************************************************         
LOCSTAB  DC    AL1(LOCSACT,0),AL2(AC#ACTV),CL4' '         Active                
LOCSTABQ EQU   *-LOCSTAB                                                        
         DC    AL1(LOCSTRM,0),AL2(AC#TRM),CL4' '          Terminated            
         DC    AL1(LOCSLOA,0),AL2(AC#LOA),CL4' '          Leave of Abs.         
         DC    AL1(LOCSTRAN,0),AL2(AC#XFRD),CL4' '        Transferred           
         DC    AL1(LOCSOTH,0),AL2(AC#OTHER),CL4' '        Other                 
*&&US*&& DC    AL1(LOCFRL,0),AL2(AC#FGAP),CL4' '          Freelancer            
         DC    AL1(254,0),AL2(0),CL4'None'                Unknown               
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* Pay type description table                                                    
***********************************************************************         
EDTPAYTB DC    C'1',AL1(0),AL2(AC#SLRY),CL10' '           Salary                
EDTPAYTQ EQU   *-EDTPAYTB                                                       
         DC    C'2',AL1(0),AL2(AC#BEN),CL10' '            Benefit               
         DC    C'3',AL1(0),AL2(AC#PEN),CL10' '            Pension               
         DC    C'4',AL1(0),AL2(AC#IDR),CL10' '            Indirect              
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*                                                                               
***********************************************************************         
OUTWRK   DCB   DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=PM,                                               X        
               BLKSIZE=1,                                              X        
               LRECL=1,                                                X        
               DDNAME=OUTWRK,                                          X        
               EODAD=*                                                          
         EJECT ,                                                                
***********************************************************************         
*                                                                               
***********************************************************************         
TRLTD    DSECT                                                                  
TRLCODE  DS    AL1                 Code                                         
TRLVALUE DS    0AL1                Code value                                   
TRLDLEN  DS    AL1                 Length                                       
TRLDD#   DS    AL2                 Tranlation data dictionary number            
TRLLNQ   EQU   *-TRLTD                                                          
TRLDATA  DS    0C                                                               
TRLDDESC DS    AL1                 Esc sequence                                 
TRLDDNUM DS    AL2                 TRLDD#                                       
TRLDDLEN DS    AL1                                                              
                                                                                
HDRKYWD  DSECT                                                                  
HDRKDATA DS    S                   Location of data                             
HDRKDD#  DS    AL2                 Data dictionary number                       
HDRKLEN  DS    AL1                 Length of data                               
HDRKOTH  DS    AL1                 Data type or indicators                      
HDRKLNQ  EQU   *-HDRKYWD                                                        
*                                                                               
DSNMD    DSECT                                                                  
DSNACC   DS    CL8                 ACCTAPE.                                     
DSNSYS   DS    CL2                 AC                                           
DSNPRG   DS    CL2                 QPROG                                        
DSNALPHA DS    CL2                 Agency alpha                                 
DSNSEP   DS    CL1                 .                                            
DSNFMT   DS    CL8                 Scribe format name                           
         ORG   DSNALPHA                                                         
DSNAGYL  DS    CL4                 Agency label                                 
DSNSEP2  DS    CL1                                                              
DSNFMT2  DS    CL8                 Scribe format name                           
*                                                                               
MAPD     DSECT                                                                  
MAPLEN   DS    AL1                 Length to next entry                         
MAPCODE  DS    AL2                 Map code                                     
MAPLNQ   EQU   *-MAPD                                                           
MAPELMS  DS    0C                                                               
                                                                                
MAPELD   DSECT                                                                  
MAPEL    DS    0XL3                                                             
MAPELQ   DS    AL1                 Element map code                             
MAPDATLN DS    AL1                                                              
MAPEDTYP DS    AL1                                                              
MAPELLNQ EQU   *-MAPEL                                                          
MAPDATA  DS    0CL1                                                             
                                                                                
MPCELD   DSECT                     Map Code                                     
MPCEL    DS    XL1                                                              
MPCELQ   EQU   X'01'                                                            
MPCLN    DS    XL1                                                              
MPCID#   DS    AL2                                                              
MPCLNQ   EQU   *-MPCELD                                                         
                                                                                
MPDELD   DSECT                     Map data                                     
MPDEL    DS    XL1                                                              
MPDELQ   EQU   X'02'                                                            
MPDLN    DS    XL1                                                              
MPDID#   DS    AL1                 1 to 255, data ID (AL2 one day?)             
MPDTYPE  DS    CL1                 Type of data                                 
MPD_BIN  EQU   C'B'                .  Binary                                    
MPD_CHR  EQU   LP_OTCHR            .  Character                                 
MPD_HEX  EQU   LP_OTHEX            .  Hex                                       
MPD_DTE  EQU   LP_OTDAT            .  YYYYMMDD                                  
MPD_YMO  EQU   C'O'                .  YYYYMM                                    
MPD_YRO  EQU   C'Y'                .  YYYY                                      
MPD_MTH  EQU   C'M'                .  MM                                        
MPD_TNY  EQU   LP_OTTNY            .  Byte                                      
MPD_SML  EQU   LP_OTSML            .  Half word                                 
MPD_FUL  EQU   LP_OTFUL            .  Full word                                 
MPD_REV  EQU   C'R'                .  Full word reverse sign                    
MPD_LNG  EQU   LP_OTLNG            .  Double word                               
MPD_EMT  EQU   C'E'                .  Empty node                                
*                                  Raw types                                    
MPD_CHRR EQU   LD_CHARQ            .  Raw character                             
MPDIND   DS    XL1                                                              
MPDRAW   EQU   X'80'               .  Raw data was passed                       
MPDIOFF  EQU   X'01'               .  Don't process this element                
MPDLNQ   EQU   *-MPDELD                                                         
MPDDATA  DS    0C                  Data                                         
         ORG   MPDDATA                                                          
MPDDTNY  DS    AL1                                                              
MPDLNTQ  EQU   *-MPDELD                                                         
         ORG   MPDDATA                                                          
MPDDSML  DS    AL2                                                              
MPDLNSQ  EQU   *-MPDELD                                                         
         ORG   MPDDATA                                                          
MPDDFUL  DS    AL4                                                              
MPDLNFQ  EQU   *-MPDELD                                                         
         ORG   MPDDATA                                                          
MPDDLNG  DS    XL8                                                              
MPDLNLQ  EQU   *-MPDELD                                                         
                                                                                
*&&UK                                                                           
       ++INCLUDE DDBDESNDD                                                      
*&&                                                                             
*ACREPRLWRK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPRLWRK                                                     
         PRINT ON                                                               
*DDCTRYEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'082ACREPRL04 03/02/21'                                      
         END                                                                    
