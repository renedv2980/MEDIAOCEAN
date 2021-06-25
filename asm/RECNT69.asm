*          DATA SET RECNT69    AT LEVEL 063 AS OF 12/29/04                      
*PHASE T80269A,+0                                                               
         TITLE 'T80269 - RADIO FORMAT'                                          
*                                                                               
***********************************************************************         
*                                                                     *         
*        RECNT69 (T80269) --- RADIO FORMAT                            *         
*                                                                     *         
* ------------------------------------------------------------------  *         
* REFER TO RECNTHIST FOR PAST HISTORY                                 *         
*                                                                     *         
* 29DEC04 SKU ADD PAGE NUMBER TO RADIO WORKSHEET                      *         
* 22JAN03 HQ  CHANGE C/O FORMAT TO ADV NAME C/O AGY NAME              *         
* 30JAN01 RHV SETTIME BUG OF PARSING WRONG STRING (AGAIN!)            *         
* 09JAN01 RHV SPORTS BUYS                                             *         
* 27JUL00 BU  TRADE PROCESSING                                        *         
* 28MAR00 RHV POINT PERSON                                            *         
* 06JUL98 SKU FIX SETTIME BUG OF PARSING WRONG STRING                 *         
* 19DEC97 JRD CARE OF AGENCIES                                                  
* 11AUG97 RHV MARK 'REVISION' WORKSHEETS                              *         
* 17JUN97 RHV REP NAME IN HEADER                                      *         
* 21OCT96 RHV PRINT K ORD CMT AFTER BUYLINES & CONTROL PAGE BREAKING  *         
* 06MAY96 RHV CONTYPE RECORD CONTROLLED WORKSHEET FORMATTING          *         
* 10APR96 RHV SUPPORT 34 BYTE AGY ADDRESS FIELDS FOR ALL USERS        *         
* 02APR96 RHV NEW DISPLAY OF LAST VER# & MOD#                         *         
* 06FEB96 RHV SUPPRESS PRINTING OF EI FIELDS IF EMPTY                 *         
* 05JAN96 SKU PROFILE 24 TO PRINT PTP OVER SALESP FOR TYPE D          *         
* 13DEC95 SKU 2K CONTRACT SUPPORT                                     *         
* 12DEC95 SKU PRINT POINT PERSON PH# (PROFILE CTRLD)                  *         
* 08NOV95 BU  ADJUST FOR KATZ AGENCY/ADV/POINT PERSON                 *         
* 25OCT95 SKU CONTRACT PROFILE TO PRINT POINT PERSON OVER             *         
*             SALESPERSON NAME FOR TYPE N/X                           *         
*                                                                     *         
***********************************************************************         
*                                                                               
T80269   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80269,R9                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     R7,4(R1)                                                         
         USING MYD,R7                                                           
         ST    RB,MYRB                                                          
         ST    R9,MYR9                                                          
         L     RA,VTWA                                                          
         USING TWAD,RA                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    RF,HOOK                                                          
         ST    RF,HEADHOOK                                                      
         LA    RF,HEDSPECS                                                      
         ST    RF,SPECS                                                         
         SPACE 2                                                                
         CLI   8(R1),1             ARE WE SETTING ADDRESSES ONLY                
         BE    EXXMOD                                                           
         SPACE 2                                                                
         CLI   FORMAT,C'A'         RADIO FORMAT                                 
         BE    RADIO                                                            
         DC    H'0'                                                             
NO       LA    R1,1                                                             
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
XIT      B     EXXMOD                                                           
         EJECT                                                                  
*           ROUTINE TO FORMAT BUY LINE TO RADIO SPECIFICATIONS                  
         SPACE 1                                                                
RADIO    DS    0H                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'40'      K ORD COMMENT PRINTED YET?                   
         BO    G1G                 YES - SKIP                                   
         OI    TWAWSFLG,X'40'      NO - IT IS NOW                               
*                                                                               
         L     R4,ASVRCOC          REP CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         SPACE 1                                                                
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    G1D                                                              
         MVC   P(19),=C'*REP ORDER COMMENT*'                                    
         BAS   RE,GOSPOOL                                                       
*                                                                               
G1B      OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    G1C                                                              
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         GOTO1 PSTCMT,DMCB,(3,0(R4))  PRINT ORD CMTS                            
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,G1B                                                           
*                                                                               
G1C      BAS   RE,GOSPOOL                                                       
         SPACE 1                                                                
*                                                                               
G1D      LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         L     R4,ASVSCOC          STA CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    G1G                                                              
         MVC   P(23),=C'*STATION ORDER COMMENT*'                                
         BAS   RE,GOSPOOL                                                       
*                                                                               
G1E      OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    G1F                                                              
*                                                                               
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         MVC   P(60),0(R4)                                                      
         OC    P(60),SPACES                                                     
         BAS   RE,GOSPOOL                                                       
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,G1E                                                           
*                                                                               
G1F      BAS   RE,GOSPOOL                                                       
*                                                                               
G1G      DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'20'                                             
         L     R4,AWORK4           POINT TO OUTPUT                              
         USING PD,R4                                                            
         LA    R3,P                POINT TO PRINT LINE                          
         USING PGRAFD,R3                                                        
         ZIC   R5,BUYLIN                                                        
         LR    R2,R5               SET UP ALLOWLIN                              
         CLI   SVSTAOP2,C'Y'       SINGLE SPACING FOR RADIO                     
         BE    G2                                                               
         SLL   R2,1                DOUBLE SPACING                               
G2       DS    0H                                                               
         TM    TWAWSFLG,X'02'      PRINT BUYLIN HEADER ALSO?                    
         BZ    *+8                 NO                                           
         AH    R2,=H'4'            YES - ALLOW ANOTHER 4 LINES                  
         STC   R2,ALLOWLIN                                                      
         DROP  RF                                                               
G5       DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'02'      DO WE NEED TO PRINT BUYLINE HEADER?          
         BZ    G5A                 NO - SKIP                                    
         CLC   CONACT,=C'CONF'     FOR CONFIRM, NEVER PRINT HEADER              
         BE    G5A                                                              
         DROP  RF                                                               
         LA    R6,P                PUT HEADER ON P                              
         BAS   RE,BLH              GENERATE BUYLINE HEADER                      
         BAS   RE,GOSPOOL          AND PRINT IT                                 
         SPACE 1                                                                
G5A      EQU   *                                                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF          HAVE WE BEEN CALLED JUST TO FORCE            
         TM    TWAWSFLG,X'01'      HEADERS AND ORD CMT TO PRINT?                
         BO    XIT                 YES - WE'RE DONE FOR NOW                     
         DROP  RF                                                               
*                                                                               
         CLC   =C'ROC',PDAY+6      SKIP IF COMMENT                              
         BE    G5B                                                              
         OC    PDAY(5),PDAY        IT'S A COMMENT                               
         BZ    G5B                                                              
*                                                                               
         GOTO1 SETTIME,DMCB,(R4)   SET TIME CHARACTER                           
G5B      CLC   =C'ALT',PDAT        ALTERNATE WEEKS LINE                         
         BNE   G5D                                                              
         MVC   PGDAT-1(13),=C'ALTERNATE WKS'                                    
         MVC   PGDAY,PDAY          DAY                                          
         MVC   PGTIM,PTIM          TIME                                         
         B     G50                                                              
         SPACE 1                                                                
G5D      CLC   =C'WEEKLY RATE',PDAT+1                                           
         BNE   *+14                                                             
         MVC   PGRAT-28(39),PDAT+1    'WEEKLY RATE FOR PLAN XXX IS'             
         B     G50                                                              
         SPACE 1                                                                
         OC    PDAY,PDAY           IF NOTHING THERE, IT'S MORE DATES            
         BNZ   G5G                                                              
         MVC   PGDAT-1(1),PDAT-1   * FOR ALTERNATE WEEKS                        
         MVC   PGDAT,PDAT          DATE                                         
         MVC   PGCLS,PCLS          CLASS                                        
         MVC   PGNPW,PNPW          NUMBER PER WEEK                              
         MVC   PGTRADE,PNPW+3      POSSIBLE TRADE FLAG                          
         MVC   PGTSPOT,PTOT        TOTAL SPOTS                                  
         B     G50                                                              
         SPACE 1                                                                
G5G      DS    0H                                                               
         CLI   PDAY,X'FF'         SPORTS BUY DESCRIPTIVE?                       
         BNE   G5H                                                              
         MVC   PGDAY(L'PSDESC),PSDESC   SPORTS DESCRIPTIVE                      
         B     G50                                                              
G5H      OC    PDAY(5),PDAY       IF NOTHING HERE, IT'S A COMMENT               
         BNZ   G6                                                               
         CLC   =C'ROC',PDAY+6      REP ORDER COMMENT                            
         BNE   G5S                                                              
         MVC   PGR(9),=C'*REP CMT-'                                             
         MVC   9(60,R3),PDAY+10                                                 
         B     G50                                                              
         SPACE 1                                                                
*   IT'S A BUY COMMENT, BUT THEY DON'T WANT TO SEE THE LABEL                    
G5S      DS    0H                                                               
         CLC   =C'P=',PDAY+6                                                    
         BNE   G5T                                                              
         MVC   9(12,R3),=C'PROGRAMMING='                                        
         MVC   21(64,R3),PDAY+8                                                 
         B     G50                                                              
*                                                                               
G5T      MVC   9(66,R3),PDAY+6                                                  
         B     G50                                                              
         SPACE 1                                                                
G6       MVC   PGCHG,PCHG          REVISION CODE                                
         OC    PGCHG,PGCHG         ONLY PRINT REV CODE LISTING                  
         BZ    *+8                 AT BOTTOM IF THERE ARE REVISIONS             
         MVI   CODESW,C'Y'                                                      
         SPACE 1                                                                
         MVC   PGLIN,PLIN          LINE NUMBER                                  
*                                                                               
         L     RF,AIO2             BUYREC                                       
         USING RBUYREC,RF                                                       
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BZ    G7                  NO                                           
         DROP  RF                                                               
         MVC   PGDAY(L'PSTYPE),PSTYPE   SPORTS TYPE                             
         B     G8                                                               
G7       MVC   PGDAY,PDAY          DAY                                          
         MVC   PGTIM,PTIM          TIME                                         
         MVC   PGCLS,PCLS          CLASS                                        
         MVC   PGLEN,PLEN          LEN                                          
         MVC   PGDAT-1(1),PDAT-1   * FOR ALTERNATE WEEKS                        
G8       MVC   PGDAT,PDAT          DATES                                        
         MVC   PGNPW,PNPW          NUMBER PER WEEK                              
         MVC   PGTRADE,PNPW+3      POSSIBLE TRADE FLAG                          
         MVC   PGRAT,PRAT          RATE                                         
         MVC   PGTSPOT,PTOT        TOTAL SPOTS                                  
         SPACE 1                                                                
         OC    PSEC(7),SPACES                                                   
         CLC   PSEC(7),SPACES      NO SECTION OR PLAN                           
         BE    G50                                                              
*                                  IF THERE'S SECTION OR PLAN                   
         BAS   RE,GOSPOOL          PRINT 1ST DATA LINE                          
         LA    R6,P                AND FILL P WITH 2ND DATA LINE                
         USING PGRAF2D,R6                                                       
         MVC   PGSEC,PSEC          SECTION                                      
         MVC   PGPLN,PPLN          PLAN                                         
         DROP  R6                                                               
         SPACE 1                                                                
G50      BAS   RE,GOSPOOL                                                       
         LA    R4,L'PRTLN(R4)                                                   
         BCT   R5,G5                                                            
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*   ROUTINE TO INSERT A/P CHARACTER IN FIRST TIME SEGMENT                       
*                                                                               
SETTIME  NTR1                                                                   
         L     R4,0(R1)                                                         
         USING PD,R4                                                            
         LA    R3,PTIM+10          A(LAST CHAR OF TIME FIELD)                   
         LA    R0,11               PTIM IS 11 CHARS LONG                        
*                                                                               
STIM0020 EQU   *                                                                
         CLI   0(R3),0             CHARACTER = BINARY ZERO?                     
         BE    STIM0030            YES - SKIP IT                                
         CLI   0(R3),C' '          FIND LAST NON-SPACE CHAR                     
         BNE   STIM0040            FOUND                                        
STIM0030 EQU   *                                                                
         BCTR  R3,0                NOT FOUND: GO BACK ONE SPACE                 
         BCT   R0,STIM0020         GO BACK AND CHECK NEXT                       
         B     STIM0900            NO NON-SPACE?  EXIT ROUTINE                  
STIM0040 EQU   *                                                                
         LR    R1,R3               SAVE A(LAST NON-SPACE CHAR)                  
STIM0050 EQU   *                                                                
         CLI   0(R3),C'-'          FIND SEPARATOR                               
         BE    STIM0060            FOUND                                        
         BCTR  R3,0                NOT FOUND: GO BACK ONE SPACE                 
         BCT   R0,STIM0050         GO BACK AND CHECK NEXT                       
         B     STIM0900            NO '-' SEPARATOR?  EXIT ROUTINE              
STIM0060 EQU   *                                                                
         LR    R2,R3               SAVE A(SEPARATOR)                            
         BCTR  R3,0                BUMP BACK 1 POSITION                         
         CLI   0(R3),C'0'          IS CHARACTER 0 THRU 9?                       
         BL    STIM0900            NO  - MUST BE A,P,N, OR M                    
*                                     EXIT ROUTINE                              
STIM0080 EQU   *                   MOVE EVERYTHING DOWN 1 FROM                  
*                                     '-' SEPARATOR TO END                      
         LR    RF,R1               SAVE A(LAST NON-SPACE)                       
         LA    RF,1(RF)            ADD 1 TO NEW END                             
STIM0100 EQU   *                                                                
         MVC   1(1,R1),0(R1)                                                    
         CR    R1,R2               SEPARATOR MOVED?                             
         BE    STIM0120            YES - FINISHED MOVE                          
         BCTR  R1,0                NO  - BUMP BACK 1 POSITION                   
         B     STIM0100            GO BACK FOR NEXT                             
STIM0120 EQU   *                                                                
         MVC   0(1,R2),0(RF)       OVERLAY OLD SEPARATOR WITH END               
*                                     TIME CHARACTER                            
STIM0900 EQU   *                                                                
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO HANDLE SPOOL INTERFACE                                
         SPACE 1                                                                
GOSPOOL  NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVI   SPACING,1                                                        
         TM    TWAWSFLG,X'20'                                                   
         BZ    GOSPL2                                                           
         MVI   SPACING,1                                                        
GOSPL2   GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  RF                                                               
         CLI   MYHEDSW,C'Y'        DID WE JUST PRINT HEADLINES                  
         BNE   GOSPLX                                                           
         LA    R4,H16                                                           
         ZIC   R3,XTRHED                                                        
GOSPL5   MVC   P,0(R4)             PRINT THE EXTRA HEADLINES                    
         MVI   SPACING,1           DON'T DOUBLE SPACE HEADLINES                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,132(R4)                                                       
         BCT   R3,GOSPL5                                                        
         SPACE 1                                                                
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE FOR SPACING                 
         SPACE 1                                                                
         MVC   P,SVPRNT            NOW PRINT THE LINE OF DATA                   
         MVI   SPACING,1           TRIPLE SPACING                               
GOSPL20  GOTO1 SPOOL,DMCB,(R8)                                                  
GOSPLX   MVI   MYHEDSW,C'N'                                                     
         B     XIT                                                              
         EJECT                                                                  
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
PSTCMT   NTR1                                                                   
         L     R3,0(R1)                                                         
*                                                                               
         MVC   WORK2X(L'KEY),KEY     SAVE OFF KEY FOR RESTORE                   
         LA    R2,IOAREA                                                        
         GOTO1 VREGENSC,DMCB,(3,0(R3)),(R2),DATAMGR,RCONREC,GETTXT              
         BNZ   PSTCMTX             COMMENT NOT FOUND, PRINT NOTHING             
         CLI   0(R2),X'FF'         IF X'FF', PRINT NOTHING                      
         BE    PSTCMTX                                                          
         CLI   0(R2),0             IF NULL, PRINT FREE FORM COMMENT             
         BE    PSTCMT20                                                         
*                                                                               
PSTCMT10 ZIC   R4,0(R2)            GET LENGTH OF COMMENT                        
         BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),1(R2)                                                       
         BAS   RE,GOSPOOL                                                       
*                                                                               
         ZIC   R4,0(R2)            BUMP TO NEXT COMMENT ENTRY                   
         AR    R2,R4                                                            
         CLI   0(R2),X'FF'         IF X'FF', DONE                               
         BE    PSTCMTX                                                          
*                                                                               
         LR    R4,RC               BOUNDARY CHECK FOR R2                        
         A     R4,=AL4(IOAREA-GENOLD+1001)                                      
         CR    R4,R2                                                            
         BH    PSTCMT10                                                         
         B     PSTCMTX                                                          
*                                                                               
PSTCMT20 DS    0H                  PRINT FREE FORM COMMENTS                     
         MVC   P(60),0(R3)                                                      
         OC    P(60),SPACES                                                     
*                                                                               
         BAS   RE,GOSPOOL                                                       
*                                                                               
PSTCMTX  DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'04'           NEED TO RESTORE SEQ LOOP?               
         BZ    XIT                                                              
         DROP  RF                                                               
         MVC   KEY,WORK2X               RESTORE BYREC SEQ LOOP                  
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RBUYKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GENERATE HEADLINES (HEADHOOK)                         
         SPACE 2                                                                
HOOK     NTR1  BASE=MYRB                                                        
         L     R9,MYR9                                                          
         CLI   FORMAT,C'A'         RADIO FORMAT                                 
         BE    HD10                                                             
         DC    H'0'                                                             
         SPACE 1                                                                
HD10     DS    0H                                                               
         CLI   RCSUBPRG,1                                                       
         BNE   *+8                                                              
         MVI   RCSUBPRG,2          FOR CONTINUATION PAGES                       
*                                  ONLY INCLUDE A FEW HEADLINES                 
         CLI   RCSUBPRG,5                                                       
         BNE   *+8                                                              
         MVI   RCSUBPRG,6                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(8,H1+6)                                       
         UNPK  DUB,SENDTIME        TIME                                         
         MVC   H1+20(2),DUB+1                                                   
         MVI   H1+22,C'.'                                                       
         MVC   H1+23(2),DUB+3                                                   
*                                                                               
         TM    PROFILES+CNTREVIB,CNTREVIA   MARK AS 'REVISION'?                 
         BZ    HD15                         NO                                  
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWASTAOP,X'40'      OVERRIDE W/STATION OPT?                      
         BO    HD15                                                             
         DROP  RF                                                               
         CLI   SVVER,1             VERSION > 1?                                 
         BNH   HD15                NO - SKIP MARKING AS REVISION                
         TM    SVCONF,X'80'        CONFIRMED?                                   
         BZ    HD15                YES - SKIP MARKING AS REVISION               
         MVC   H1+40(40),REVISION                                               
*                                                                               
HD15     MVC   H2+9(8),CONCNUM                                                  
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0                                                       
         BE    HD20                                                             
         MVC   H2+9(8),=C'*COMBO* '                                             
         DROP  RF                                                               
HD20     DS    0H                                                               
*                                                                               
*   CALL ROUTINE TO DISPLAY MOD & VERSION NUMBER                                
         GOTO1 VGENDMV,DMCB,RCONREC,H2+25,GENOLD                                
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   H3+9(L'TWAREPNM),TWAREPNM   REP NAME                             
*                                                                               
         CLC   PAGE,=X'0001'       FIRST PAGE?                                  
         BE    HD30                YES                                          
*        MVC   H2+55(11),=C'(CONTINUED)'                                        
         NI    SPOOLIND,X'7F'      TURN OFF 'SKIP LINE' INDICATOR               
         TM    TWAWSFLG,X'80'           SHOULD WE PRINT BUYLINE HEADER?         
         BZ    XIT                                                              
         MVI   H4,0                                                             
         LA    R6,H5                                                            
         BAS   RE,BLH                                                           
         B     XIT                                                              
*                                                                               
HD30     DS    0H                                                               
         CLC   =C'RSND',CONACT     FOR ACTION RESEND                            
         BNE   *+10                IDENTIFY AS A DUPLICATE COPY                 
         MVC   H4+55(6),=C'RESENT'                                              
*                                                                               
         MVI   MYHEDSW,C'Y'        MORE THAN 14 HEADLINES                       
         MVC   SVPRNT,P            SAVE PRINT LINE                              
         MVC   P,SPACES            BLANK OUT PRINT LINE                         
         MVI   XTRHED,1            MAXIMUM NUMBER OF EXTRA HEADLINES            
         OI    SPOOLIND,X'80'      DON'T SKIP LINE AFTER 1ST HEADS              
*                                                                               
         MVC   H4+9(5),ACTSTAT     STATION                                      
*                                                                               
         CLI   ACTSTAT+4,C' '                                                   
         BNE   *+14                                                             
         MVC   H4+13(3),=C'-TV'                                                 
         B     HD70                                                             
*                                                                               
         CLI   ACTSTAT+4,C'A'                                                   
         BNE   *+14                                                             
         MVC   H4+13(3),=C'-AM'                                                 
         B     HD70                                                             
*                                                                               
         CLI   ACTSTAT+4,C'F'                                                   
         BNE   *+14                                                             
         MVC   H4+13(3),=C'-FM'                                                 
         B     HD70                                                             
*                                                                               
         CLI   ACTSTAT+4,C'C'                                                   
         BNE   HD70                                                             
         MVC   H4+9(4),SVCAM                                                    
         MVC   H4+13(3),=C'-AM'                                                 
         MVC   H4+17(4),SVCFM                                                   
         MVC   H4+21(3),=C'-FM'                                                 
* GET STATION RECORD COMBO ELEM & PRINT -AM & -FM STATIONS.                     
*                                                                               
HD70     DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0                                                       
         BE    HD80                                                             
         DROP  RF                                                               
         BAS   RE,PRTCALL                                                       
*                                                                               
HD80     CLI   SVVER,1                                                          
         BE    HD90                NO "RECAP" IF VERSION ONE                    
         TM    PROFILES+CNTBDELB,CNTBDELA IF RECAP, SAY SO                      
         BZ    *+10                                                             
         MVC   H4+55(6),=CL6'RECAP'                                             
*                                                                               
HD90     DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H5+9(20),TWASALNM                                                
         TM    PROFILES+CNTSFONB,CNTSFONA PRINT SALESPERSON PHONE ON            
         BZ    HD180                      ALL VER??                             
         MVC   H5+30(12),TWASALTL YES, PRINT PHONE                              
         DROP  RF                                                               
*                                                                               
HD170    CLC   SVSASST,SPACES      FILL IN SALES ASST IF POSSIBLE               
         BE    HD190                                                            
         MVI   H5+43,C'/'                                                       
         MVC   H5+44(9),SVSASST                                                 
         B     HD190                                                            
*                                                                               
HD180    CLC   SVSASST,SPACES      FILL IN SALES ASST IF POSSIBLE               
         BE    HD190                                                            
         MVI   H5+30,C'/'                                                       
         MVC   H5+31(9),SVSASST                                                 
*                                                                               
HD190    MVC   H6+9(16),CONOFFN                                                 
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         OC    TWASALFX,TWASALFX   SALESPERSON FAX NUMBER                       
         BZ    HD200                                                            
         MVC   H6+30(16),=C'SALESPERSON FAX#'                                   
         MVC   H6+47(L'TWASALFX),TWASALFX                                       
         B     HD210                                                            
*                                                                               
HD200    DS    0H                  -OR-                                         
         OC    TWAOFFFX,TWAOFFFX   OFFICE FAX NUMBER                            
         BZ    HD210                                                            
         MVC   H6+30(12),=C'OFFICE FAX# '                                       
         MVC   H6+42(3),TWAOFFFX                                                
         MVI   H6+45,C'-'                                                       
         MVC   H6+46(3),TWAOFFFX+3                                              
         MVI   H6+49,C'-'                                                       
         MVC   H6+50(4),TWAOFFFX+6                                              
*                                                                               
HD210    DS    0H                                                               
         TM    TWAPRFW,X'20'               CONTYPE FORMAT OPTION #3?            
         BZ    HD214                       NO                                   
         MVC   H7(3),=C'AOR'               YES - REPL AGY W/AOR                 
         B     *+10                                                             
HD214    MVC   H7(3),=C'AGY'                                                    
         TM    TWAFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    HD216               NO                                           
*                                                                               
         LA    RE,H7+9                                                          
         MVC   0(L'TWAADVNM,RE),TWAADVNM                                        
         LA    RE,L'TWAADVNM+1(RE)                                              
*                                                                               
         CLI   0(RE),X'40'         CHOP SPACE                                   
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         LA    RE,2(RE)                                                         
         MVC   0(4,RE),=C'C/O '                                                 
         LA    RE,4(RE)                                                         
         MVC   0(L'TWAAGNM2,RE),TWAAGNM2  AGENCY NAME FROM SCREEN               
         B     HD218                                                            
*                                                                               
HD216    DS    0H                                                               
         MVC   H7+9(33),TWAAGNM2   AGENCY NAME FOR CONTRACT                     
*                                                                               
HD218    DS    0H                                                               
         CLI   SVVER,1                                                          
         BE    HD250                                                            
         TM    PROFILES+CNTAADDB,CNTAADDA   PRINT AGY ADD ON ALL VER??          
         BZ    HD390                                                            
*                                                                               
HD250    DS    0H                                                               
         OC    TWAAGYPH,TWAAGYPH                                                
         BZ    HD260                                                            
         MVC   H8+47(4),=C'PH #'  AGENCY PHONE NUMBER                           
         MVC   H8+52(3),TWAAGYPH                                                
         MVI   H8+55,C'-'                                                       
         MVC   H8+56(3),TWAAGYPH+3                                              
         MVI   H8+59,C'-'                                                       
         MVC   H8+60(4),TWAAGYPH+6                                              
*                                                                               
HD260    DS    0H                                                               
         OC    TWAAFAX,TWAAFAX                                                  
         BZ    HD270                                                            
         MVC   H9+47(4),=C'FAX#' AGENCY FAX NUMBER                              
         MVC   H9+52(3),TWAAFAX                                                 
         MVI   H9+55,C'-'                                                       
         MVC   H9+56(3),TWAAFAX+3                                               
         MVI   H9+59,C'-'                                                       
         MVC   H9+60(4),TWAAFAX+6                                               
*                                                                               
HD270    DS    0H                                                               
         MVC   H8+9(34),TWAAGAD1   ADDRESS                                      
         MVC   H9+9(36),TWAAGAD2                                                
         MVC   H10+9(36),TWAAGAD3                                               
*                                                                               
* BUYER, ADVERTISER, EI CODES, PRODUCT                                          
         MVC   H11+9(20),TWABUYER                                               
         MVC   H12+9(20),TWAADVNM                                               
         MVC   H13+9(20),CONPRD                                                 
         CLC   SVCONPRD,SPACES                                                  
         BE    *+10                                                             
         MVC   H13+9(20),TWAPRDNM                                               
         DROP  RF                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD360                                                            
         USING RCONIEL,R6                                                       
         MVC   H12+34(2),=C'EI'                                                 
         MVC   H12+38(3),=C'ADV'                                                
         MVC   H12+47(3),=C'PRD'                                                
         MVC   H12+56(3),=C'EST'                                                
         MVC   H13+47(3),=C'PRD'                                                
         MVC   H12+42(4),RCONIADV                                               
         MVC   H12+51(4),RCONIPRD                                               
         MVC   H12+60(10),RCONXEST                                              
         OC    H12+60(10),MYSPACES                                              
         CLC   H12+60(10),MYSPACES                                              
         BNE   *+10                                                             
         MVC   H12+60(4),RCONIEST                                               
         MVC   H13+51(4),RCONIPR2                                               
         DROP  R6                                                               
*                                                                               
HD360    DS    0H                  POINT PERSON                                 
         LA    R4,H14                                                           
         TM    PROFILES+CNTPTPRB,CNTPTPRA                                       
         BZ    HD370               IF OFF SKIP POINT PERSON                     
         OC    WPTPEXP,SPACES                                                   
         CLC   WPTPEXP,SPACES                                                   
         BE    HD370                                                            
         MVC   0(3,R4),=C'PTP'                                                  
         MVC   9(20,R4),WPTPEXP     PRINT OUT POINT PERSON NAME                 
         LA    R4,H15                                                           
*                                                                               
HD370    DS    0H                   FLIGHT DATES                                
         MVC   0(3,R4),=C'FLT'                                                  
         GOTO1 DATCON,DMCB,(3,SVCONDTE),(5,9(R4))                               
         MVI   18(R4),C'-'                                                      
         GOTO1 DATCON,DMCB,(3,SVCONDTE+3),(5,20(R4))                            
         B     XIT                                                              
*                                                                               
* HERE IF NOT VER 1 (NO ADDR LINES)                                             
HD390    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   H8+9(20),TWABUYER                                                
         MVC   H9+9(20),TWAADVNM                                                
         MVC   H11+9(20),CONPRD                                                 
         CLC   SVCONPRD,SPACES                                                  
         BE    *+10                                                             
         MVC   H11+9(20),TWAPRDNM                                               
         DROP  RF                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD420                                                            
         USING RCONIEL,R6                                                       
         MVC   H9+35(2),=C'EI'                                                  
         MVC   H9+39(3),=C'ADV'                                                 
         MVC   H9+48(3),=C'PRD'                                                 
         MVC   H9+57(3),=C'EST'                                                 
         MVC   H11+48(3),=C'PRD'                                                
         MVC   H9+42(4),RCONIADV                                                
         MVC   H9+51(4),RCONIPRD                                                
         MVC   H9+60(10),RCONXEST                                               
         OC    H9+60(10),MYSPACES                                               
         CLC   H9+60(10),MYSPACES                                               
         BNE   *+10                                                             
         MVC   H9+60(4),RCONIEST                                                
         MVC   H11+51(4),RCONIPR2                                               
         DROP  R6                                                               
*                                                                               
HD420    DS    0H                  POINT PERSON                                 
         LA    R4,H12                                                           
         TM    PROFILES+CNTPTPRB,CNTPTPRA                                       
         BZ    HD430               IF OFF SKIP POINT PERSON                     
         OC    WPTPEXP,SPACES                                                   
         CLC   WPTPEXP,SPACES                                                   
         BE    HD430                                                            
         MVC   0(3,R4),=C'PTP'                                                  
         MVC   9(20,R4),WPTPEXP     PRINT OUT POINT PERSON NAME                 
         LA    R4,H13                                                           
*                                                                               
HD430    DS    0H                   FLIGHT DATES                                
         MVC   0(3,R4),=C'FLT'                                                  
         GOTO1 DATCON,DMCB,(3,SVCONDTE),(5,9(R4))                               
         MVI   18(R4),C'-'                                                      
         GOTO1 DATCON,DMCB,(3,SVCONDTE+3),(5,20(R4))                            
         B     XIT                                                              
         EJECT                                                                  
BLH      NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'02'  TURN OFF FLAG FOR FIRST TIME HEADR         
         DROP  RF                                                               
         MVC   0(24,R6),=C'MC LN DAYS         TIMES'                            
         MVC   30(13,R6),=C'LEN EFF DATES'                                      
         MVC   47(22,R6),=C'CLS NPW RATE       TOT'                             
         LA    R6,132(R6)                                                       
         MVC   47(22,R6),=C'SEC PLA            SPT'                             
         LA    R6,132(R6)                                                       
         MVC   0(29,R6),=C'-- -- ------------ ----------'                       
         MVC   30(16,R6),=C'--- ------------'                                   
         MVC   47(22,R6),=C'--- --- ---------- ---'                             
BLHX     B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
* PRINT COMPONENT STATION CALL LETTERS FOR COMBO ORDERS                         
*********************************************************************           
PRTCALL  NTR1                                                                   
         LA    R6,RCONREC          PRINT CALL LETTERS IN ORDER AS               
         USING RCONCBEL,R6         FOUND IN THE COMBO ELEMENT                   
         MVI   ELCODE,X'17'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,RCONCBST         POINT TO FIRST STATION TO PRINT              
         DROP  R6                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         ZIC   R2,TWACOMBO         NUMBER OF STATIONS TO PRINT                  
         DROP  RF                                                               
*                                                                               
         LA    R1,H4+9             POINT TO OUTPUT LINE                         
*                                                                               
PRTC10   OC    0(L'RCONCBST,R3),0(R3)                                           
         BZ    PRTCX                                                            
         MVC   0(4,R1),0(R3)       CALL LETTER                                  
         MVC   4(3,R1),=C'- M'                                                  
         MVC   5(1,R1),4(R3)       BAND                                         
*                                                                               
         LA    R3,9(R3)                                                         
         LA    R1,9(R1)                                                         
         BCT   R2,PRTC10                                                        
*                                                                               
PRTCX    B     XIT                                                              
         EJECT                                                                  
*        CONSTANTS, LITERAL POOL, ETC.                                          
         SPACE 1                                                                
DASH     DC    51C'-'                                                           
REVISION DC    C'*************** REVISION ***************'                      
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*          HEADLINE SPECS FOR CONTRACTS                                         
         SPACE 2                                                                
HEDSPECS DS    0D                                                               
         SPACE 1                                                                
         SPROG 1,2,5,6        ***RADIO - ALL PAGES***                           
         PSPEC H2,55,PAGE                                                       
         PSPEC H2,1,C'CON #'                                                    
         PSPEC H3,1,C'REP'                                                      
         SPACE 1                                                                
         SPROG 1              ***RADIO - PAGE 1 ONLY***                         
         PSPEC H4,1,C'TO'                                                       
         PSPEC H5,1,C'FM'                                                       
         PSPEC H6,1,C'OFF'                                                      
         PSPEC H8,1,C'BYR'                                                      
         PSPEC H9,1,C'ADV'                                                      
         PSPEC H11,1,C'PDT'                                                     
***>     PSPEC H12,1,C'FLT'                                                     
         EJECT                                                                  
         SPROG 5              ***RADIO - PAGE 1 VERSION 1 ONLY ***              
         PSPEC H4,1,C'TO'                                                       
         PSPEC H5,1,C'FM'                                                       
         PSPEC H6,1,C'OFF'                                                      
         PSPEC H8,1,C'ADDR'                                                     
         PSPEC H11,1,C'BYR'                                                     
         PSPEC H12,1,C'ADV'                                                     
         PSPEC H13,1,C'PDT'                                                     
***>     PSPEC H14,1,C'FLT'                                                     
         EJECT                                                                  
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
       ++INCLUDE RECNTFMTD                                                      
       ++INCLUDE REGENPBYD                                                      
         EJECT                                                                  
*    FORMAT FOR RADIO WORKSHEET BUYLINES                                        
         SPACE 1                                                                
PGRAFD   DSECT                                                                  
PGR      DS    0CL110                                                           
PGCHG    DS    CL2                 CHANGE CODE                                  
PGLIN    DS    CL3                 DDS LINE NUMBER                              
         DS    CL1                                                              
PGDAY    DS    CL12                DAY                                          
         DS    CL1                                                              
PGTIM    DS    CL10                TIME                                         
         DS    CL1                                                              
PGLEN    DS    CL3                 LENGTH                                       
         DS    CL1                                                              
PGDAT    DS    CL12                DATE                                         
         DS    CL1                                                              
PGCLS    DS    CL3                 CLASS                                        
         DS    CL1                                                              
PGNPW    DS    CL3                 NUMBER PER WEEK                              
PGTRADE  DS    CL1                                                              
PGRAT    DS    CL10                RATE                                         
         DS    CL1                                                              
PGTSPOT  DS    CL3                 TOTAL SPOTS                                  
         DS    CL41                                                             
         SPACE 2                                                                
PGRAF2D  DSECT                                                                  
         DS    CL47                                                             
PGSEC    DS    CL3                 SECTION                                      
         DS    CL1                                                              
PGPLN    DS    CL3                 PLAN                                         
         DS    CL56                                                             
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063RECNT69   12/29/04'                                      
         END                                                                    
