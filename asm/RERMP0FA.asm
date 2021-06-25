*          DATA SET RERMP0FA   AT LEVEL 009 AS OF 03/12/97                      
*PHASE T8100FA                                                                  
T8100F   TITLE 'RERMP0F - INVENTORY FILE FILEFIX'                               
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* Mar12/97 009 GLEE - Count number of NOV96 tracks on file            *         
*                                                                     *         
* Jan31/97 008 GLEE - Fucked up online transfers                      *         
*                                                                     *         
* Jan14/97 007 GLEE - Fix weighting problem for recs touched by ROVER *         
*                                                                     *         
* Jan14/97 006 GLEE - Modify DEMUP call to pass in 2-char REP code    *         
*                                                                     *         
* Jan07/97 005 GLEE - Filefix for impression-based demos              *         
*                                                                     *         
* Nov26/96 003 GLEE - Added SELNY and UTSNY to REPLIST                *         
*                                                                     *         
* Nov14/96 001 GLEE - Add program title footnotes to PAV transfers    *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
RMP0F    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 IUNWRKL,T8100F**,R9,RR=RE                                        
                                                                                
         LR    R0,RC               HOLD ONTO ADDRESS OF IUN WORK AREA           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASYSD                                                         
         USING SYSD,R8                                                          
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
                                                                                
         ST    RE,RELO                                                          
         ST    RB,MYBASE1                                                       
         ST    R9,MYBASE2                                                       
         ST    R0,AIUNWRK                                                       
*                                                                               
         BAS   RE,MYINIT           INITIALIZATION                               
*                                                                               
         DS    0H                  CHECK GENCON MODES                           
         CLI   MODE,VALREC                                                      
         BE    VREC                                                             
         CLI   MODE,PRINTREP                                                    
         BE    PREP                                                             
         B     XIT                                                              
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*========================= MY INITIALIZATION =========================*         
         DS    0H                                                               
MYINIT   NTR1                                                                   
*                                                                               
         DS    0H                  SET UP ADCONS OF TABLES & ROUTINES           
         LH    R2,=Y(DISPTAB-RMP0F)                                             
         LA    R2,RMP0F(R2)                                                     
         LA    R0,DISPTABQ                                                      
                                                                                
MI10     DS    0H                                                               
         ZICM  R1,0(R2),(3)                                                     
         LA    RE,RMP0F            RE = BASE OF TABLE/ROUTINE                   
         ZICM  RF,2(R2),(3)                                                     
         BZ    *+12                                                             
         LA    RF,GEND(RF)                                                      
         L     RE,0(RF)                                                         
         AR    R1,RE               R1 = A(TABLE OR ROUTINE)                     
         ZICM  RF,4(R2),(3)                                                     
         A     RF,ASYSD            RF-->PLACE TO STORE ADDRESS                  
         ST    R1,0(RF)                                                         
         LA    R2,L'DISPTAB(R2)                                                 
         BCT   R0,MI10                                                          
*                                                                               
         DS    0H                  SET UP LABELS IN BIGAREA                     
         LH    R2,=Y(LBLTAB-RMP0F)                                              
         LA    R2,RMP0F(R2)                                                     
         LA    R0,LBLTABQ                                                       
                                                                                
MI20     DS    0H                                                               
         ZICM  R1,0(R2),(3)                                                     
         A     R1,ATIA             R1 = A(TO PUT LABEL)                         
         MVC   0(8,R1),2(R2)        AND MOVE LABEL IN                           
         LA    R2,L'LBLTAB(R2)                                                  
         BCT   R0,MI20                                                          
*                                                                               
MI40     DS    0H                                                               
         MVC   AIO,AIO1                                                         
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
         OI    CONSERVH+6,X80+X01  FOR PFKEYS TO WORK                           
         OI    GENSTAT1,RDUPAPPL   WE CONTROL READ FOR UPDATE                   
*                                                                               
         DS    0H                  MOVE PROFILE TO LOCAL WORKNG STORAGE         
         LR    RF,RA                                                            
         AH    RF,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,RF                                                       
         MVC   RMPPROFS,SVPGPBIT                                                
         DROP  RF                                                               
*                                                                               
         DS    0H                  TRANSLATE DATA DICT TERMS                    
         XC    DMCB(6*4),DMCB                                                   
         LA    R1,DMCB                                                          
         USING DICTATED,R1                                                      
         MVI   DDACTN,DDACTNL                                                   
         MVI   DDRETN,DDCASEL                                                   
         MVI   DDRETN,C'M'         (NO IDEA WHY THIS YIELDS LOWER CASE)         
         MVI   DDSYS,8                                                          
         MVI   DDLANG,C' '                                                      
         LH    RE,=Y(DCLIST-RMP0F)                                              
         A     RE,MYBASE1                                                       
         STCM  RE,7,DDIADR                                                      
         LA    RE,DSLIST                                                        
         STCM  RE,7,DDOADR                                                      
         GOTO1 DICTATE,(R1)                                                     
         DROP  R1                                                               
                                                                                
         NI    RE@OPTS,XFF-X40     CHANGE FROM C'O' TO C'o'                     
*                                                                               
         XC    ACURFORC,ACURFORC                                                
*                                                                               
         MVC   DSP1EL,=Y(RINVPEL-RINVREC)                                       
*                                                                               
         MVC   DCFILTP,=C'TP '                                                  
         MVC   DCFILPAV,=C'PAV'                                                 
         MVC   DCFILIUN,=C'IUN'                                                 
         MVC   DCFILINV,=C'INV'                                                 
         MVC   DCFILPT,=C'PT '                                                  
*                                                                               
         DS    0H                  GET TYPE OF DEMO CALC PRECISION              
         MVI   MYTAPEP,0            ASSUME BOOK (RTG) BASED                     
         TM    RMPPROFS+RMPIMPSB,RMPIMPSA                                       
         BZ    *+8                                                              
         MVI   MYTAPEP,C'Y'         TAPE (IMP) BASED, AS PER PROFILE            
*                                                                               
MIX      DS    0H                                                               
         B     XIT                                                              
***********************************************************************         
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (VALREC)'                      
***********************************************************************         
*====================== VALIDATE RECORD ROUTINE ======================*         
VREC     DS    0H                                                               
                                                                                
*                                                                               
*---------------------------- VALIDATE REP ---------------------------*         
*                                                                               
         LA    R2,FFXREPH                                                       
         GOTO1 ANY                                                              
                                                                                
         CLC   TWAAGY,WORK                                                      
         BNE   INVLREP                                                          
                                                                                
         MVC   IPREP,WORK                                                       
                                                                                
*                                                                               
*--------------------------- VALIDATE BOOK ---------------------------*         
*                                                                               
         XC    IPBOOK,IPBOOK                                                    
         MVC   IPBOOK_P,SPACES                                                  
         LA    R2,FFXBOOKH                                                      
         GOTO1 ANY                                                              
                                                                                
*                                                                               
         CLI   5(R2),3             CHECK IF INPUT IS "ALL"                      
         BNE   VR042X                                                           
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   VR042X                                                           
         MVC   IPBOOK_P+((L'IPBOOK_P-3)/2)(3),=C'ALL'                           
         B     VR049                                                            
VR042X   EQU   *                                                                
                                                                                
*                                                                               
         GOTO1 BOOKVAL,DMCB,(C'N',(R2)),(1,DUB),(0,SCANNER),0                   
         CLI   DMCB+4,1                                                         
         BNE   INVLBK                                                           
         TM    DUB,X'BE'           NO FUNNY STUFF                               
         BNZ   INVLBK                                                           
                                                                                
         MVC   IPBOOK,DUB+1                                                     
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   (DUB+1)+2,0                                                      
         GOTO1 DATCON,DMCB,(3,DUB+1),(6,WORK),0                                 
         MVC   IPBOOK_P,WORK                                                    
VR049    EQU   *                                                                
                                                                                
*                                                                               
*---------------------------- VALREC EXITS ---------------------------*         
*                                                                               
VRX      DS    0H                                                               
         B     XIT                                                              
***********************************************************************         
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (VALIDATION ERRORS)'           
***********************************************************************         
*========================= VALIDATION ERRORS =========================*         
                                                                                
MISSFLD  DS    0H                  MISSING INPUT FIELD ERROR                    
         MVI   OURERRCD,MFLDQ                                                   
         B     OURERROR                                                         
                                                                                
INVLFLD  DS    0H                  INVALID FIELD ERROR                          
         MVI   OURERRCD,IFLDQ                                                   
         B     OURERROR                                                         
                                                                                
INVLREP  DS    0H                  INVALID REP                                  
         MVI   OURERRCD,IREPQ                                                   
         B     OURERROR                                                         
                                                                                
INVLBK   DS    0H                  INVALID BOOK                                 
         MVI   OURERRCD,IBKQ                                                    
         B     OURERROR                                                         
                                                                                
***********************************************************************         
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (PRINTREP)'                    
***********************************************************************         
*======================== PRINT REPORT ROUTINE =======================*         
                                                                                
* Will re-generate actual tracks in impression-based format                     
                                                                                
PREP     DS    0H                                                               
         CLI   OFFLINE,C'Y'        MUST BE OFFLINE                              
         BNE   PRX                                                              
                                                                                
*                                                                               
         MVI   GOSUBN,VREP#        VALIDATE REP                                 
         GOTO1 AGOSUB                                                           
         BE    PR029                                                            
         MVC   P(23),=C'REP  ??  IS NOT IN LIST'                                
         MVC   P+5(2),IPREP                                                     
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     PRX                  EXIT IF REP IS NOT IN LIST                  
PR029    EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  REPORT INITIALIZATION                        
         LA    R0,HEDSPECS                                                      
         ST    R0,SPECS                                                         
         LA    R0,HDHOOK                                                        
         ST    R0,HEADHOOK                                                      
*                                                                               
         XC    CNTFLDS(CNTFLDSL),CNTFLDS  INITIALIZE COUNTERS                   
*                                                                               
         XC    PVFXSTTN,PVFXSTTN   CLEAR OUT PREVIOUS STATION                   
                                                                                
*                                                                               
** READ REPDIR FOR INVENTORY RECORDS **                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RINVREC,R6                                                       
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,IPREP                                                   
         DROP  R6                                                               
                                                                                
*                                                                               
         DS    0H                  READ FIRST INVENTORY RECORD                  
         GOTO1 HIGH                                                             
         B     PR065                                                            
*                                                                               
PR062    DS    0H                  READ SUBSEQUENT INVENTORY RECORDS            
         GOTO1 SEQ                                                              
         B     PR065                                                            
*                                                                               
PR065    DS    0H                                                               
         LA    R0,1                                                             
         A     R0,CNTDREAD                                                      
         ST    R0,CNTDREAD         UPDATE # OF DIRECTORY READS                  
                                                                                
*                                                                               
         CLC   KEY(IKYREPL),KEYSAVE                                             
         BNE   PR600                                                            
         EJECT                                                                  
*                                                                               
** CHECK IF RECORD NEEDS FIXING **                                              
*                                                                               
         LA    R6,KEY                                                           
         USING RINVREC,R6                                                       
                                                                                
*                                                                               
         DS    0H                  CHECK IF STATION IS ALPHANUMERIC             
         MVI   BYTE,L'RINVKSTA                                                  
         MVC   WORK(L'RINVKSTA),RINVKSTA                                        
         MVI   GOSUBN,CAN#                                                      
         GOTO1 AGOSUB                                                           
         BNE   PR062                                                            
                                                                                
*                                                                               
         DS    0H                  CHECK IF INV # IS ALPHANUMERIC               
         MVI   BYTE,L'RINVKINV                                                  
         MVC   WORK(L'RINVKINV),RINVKINV                                        
         MVI   GOSUBN,CAN#                                                      
         GOTO1 AGOSUB                                                           
         BNE   PR062                                                            
                                                                                
         PRINT OFF                                                              
*&&DO                                                                           
*                                                                               
         DS    0H                  CHECK FOR KCONYR STATIONS                    
         CLC   IPREP,=C'CQ'                                                     
         BNE   PRCQSTNX                                                         
         CLC   RINVKSTA,=C'WPTAT'   WPTA                                        
         BE    PR062                                                            
         CLC   RINVKSTA,=C'WSBTT'   WSBT                                        
         BE    PR062                                                            
         CLC   RINVKSTA,=C'WTHIT'   WTHI                                        
         BE    PR062                                                            
PRCQSTNX EQU   *                                                                
                                                                                
*&&                                                                             
         PRINT ON                                                               
*                                                                               
         DS    0H                  CHECK FOR CORRECT INVNTRY REC TYPE           
         CLI   RINVKSRC,0           SKIP HEADER       RECORDS                   
         BNE   PR076                                                            
         MVI   GOSUBN,GHI#           BUT STILL NEED TO GET HDR INFO             
         GOTO1 AGOSUB                                                           
         B     PR062                 READ NEXT RECORD                           
*                                                                               
PR076    DS    0H                                                               
         XC    GKSIPARM,GKSIPARM                                                
         XC    GKSOPARM,GKSOPARM                                                
                                                                                
         DS    0H                  GET RTG SVCE, QLFYR, & BKTYPE                
         LA    R3,GKSIPARM                                                      
         USING GKSPARMD,R3                                                      
         MVC   GKSPKSRC,RINVKSRC                                                
         DROP  R3                                                               
         GOTO1 VGETKSRC,DMCB,(C'K',GKSIPARM),GKSOPARM                           
         CLI   DMCB+4,0                                                         
         BNE   PR062                                                            
                                                                                
         LA    R3,GKSOPARM                                                      
         USING GKSPARMD,R3                                                      
         MVC   TMPSRC(1),GKSPRSVC   RETURN: RATING SERVICE                      
         MVC   TMPQLFY,GKSPQLFY     RETURN: TRACK QUALIFIER                     
         MVC   TMPBTYP,GKSPBTYP     RETURN: BOOK TYPE                           
         DROP  R3                                                               
*                                                                               
         MVC   TRKKQLFY,TMPQLFY     GET TRACK QUALIFIER                         
         MVC   TRKKBTYP,TMPBTYP     GET BOOK TYPE                               
*                                                                               
         MVC   FDSRC,=C'NSI'                                                    
         CLI   TMPSRC,C'N'                                                      
         BE    PR089                                                            
         MVC   FDSRC,=C'MFX'                                                    
         CLI   TMPSRC,C'M'                                                      
         BE    PR089                                                            
         MVC   FDSRC,=C'SRC'                                                    
         CLI   TMPSRC,C'S'                                                      
         BE    PR089                                                            
         DC    H'0'                                                             
PR089    EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  CHECK IF TRACK MATCHES REQUEST               
         OC    IPBOOK,IPBOOK                                                    
         BZ    *+14                                                             
         CLC   RINVKBK,IPBOOK                                                   
         BNE   PR062                                                            
         DROP  R6                                                               
         EJECT                                                                  
         PRINT OFF                                                              
*&&DO                                                                           
*                                                                               
*** GET TRACK RECORD UNDER SCRUTINY ***                                         
*                                                                               
         DS    0H                  GET INVENTORY RECORD                         
         GOTO1 GETREC                                                           
*                                                                               
         LA    R0,1                                                             
         A     R0,CNTFREAD                                                      
         ST    R0,CNTFREAD         UPDATE # OF FILE READS                       
*                                                                               
         DS    0H                  GET D/A IN EBCDIC FORMAT                     
         GOTO1 HEXOUT,DMCB,KEY+(L'RINVKEY+1),PRNT_DA,4,=C'TOG',0,0              
*                                                                               
         DS    0H                  RE-INITIALIZE TRACK INFORMATION              
         MVC   BYTE,TRKKQLFY        DON'T INITIALIZE THIS THOUGH                
         XC    TRKINFO(TRKINFOL),TRKINFO                                        
         MVC   TRKKQLFY,BYTE                                                    
*                                                                               
         MVC   TRK_DA,KEY+(L'RINVKEY+1)   REMEMBER D/A OF TRACK                 
                                                                                
*                                                                               
*** CONTINUE TO CHECK IF RECORD NEEDS FIXING ***                                
*                                                                               
         DS    0H                  CHECK LAST CHANGE DATE                       
         L     R3,AIO                                                           
         MVI   ELCODE,X'EF'                                                     
         BAS   RE,GETEL                                                         
         BNE   PR062                                                            
                                                                                
         USING RINVAEL,R3                                                       
         CLC   RINVALST,BGNDATE     WANT THOSE BETWEEN BEGIN                    
         BL    PR062                                                            
         CLC   RINVALST,ENDDATE      AND END DATES, INCLUSIVE                   
         BH    PR062                                                            
         DROP  R3                                                               
                                                                                
*                                                                               
         DS    0H                  SKIP THOSE W/ DEMO HISTORY ELEMS             
         L     R3,AIO                                                           
         MVI   ELCODE,X'CF'                                                     
         BAS   RE,GETEL                                                         
         BE    PR062                                                            
                                                                                
*                                                                               
         DS    0H                  DO ONLY ONLINE TAPE TRANSFERS                
         L     R3,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         MVC   DATADISP,DSP1EL                                                  
         BAS   RE,GETEL                                                         
         BNE   PR062                READ NEXT RECORD IF ELEM NOT FOUND          
                                                                                
         USING RINVFREL,R3                                                      
         CLI   RINVFRPR,C'A'        GET ONLY ONLINE TRANSFERS                   
         BNE   PR062                                                            
         CLI   RINVFRTY,C'P'        TAPE TRANSFER INDICATOR                     
         BNE   PR062                                                            
         DROP  R3                                                               
                                                                                
*                                                                               
         DS    0H                  SKIP "PROGRAM RAN"                           
         L     R3,AIO                                                           
         MVI   ELCODE,X'CD'                                                     
         BAS   RE,GETEL                                                         
         BNE   PR062                READ NEXT RECORD IF ELEM NOT FOUND          
                                                                                
         USING RINVCEL,R3                                                       
         CLC   RINVCODE,=C'PR'      PROGRAM RAN NOT OKAY                        
         BE    PR062                                                            
         MVC   TRKXFRCD,RINVCODE                                                
         DROP  R3                                                               
         EJECT                                                                  
*&&                                                                             
         PRINT ON                                                               
*                                                                               
** TRACK NEEDS TO BE RE-TRANSFERRED **                                          
*                                                                               
         B     PR300                                                            
                                                                                
*                                                                               
         MVI   BADRCNUM,(BRTTRGT-BADRECTB)/(L'BADRECTB)                         
         MVI   GOSUBN,PBR#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
         DS    0H                  GET TRACK'S UPGRADE ELEMENT                  
         L     R3,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   TRKUPGEL,0(R3)                                                   
                                                                                
*                                                                               
         DS    0H                  BUILD INVENTORY LIST                         
         MVI   GOSUBN,BIL#                                                      
         GOTO1 AGOSUB                                                           
         BE    PR207X                                                           
                                                                                
         MVI   BADRCNUM,(BRTFRDTL-BADRECTB)/(L'BADRECTB)                        
         MVI   GOSUBN,PBR#                                                      
         GOTO1 (RF)                                                             
         B     PR062               SOMETHING WRONG W/ RECORD, READ NEXT         
PR207X   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  BUILD INV RECD IN IUN-FMT (IN BUFF)          
         MVI   GOSUBN,BIR#                                                      
         GOTO1 AGOSUB                                                           
         BE    PR219                                                            
                                                                                
         MVI   BADRCNUM,(BRTTRKNR-BADRECTB)/(L'BADRECTB)                        
         MVI   GOSUBN,PBR#                                                      
         GOTO1 (RF)                                                             
         B     PR062                                                            
PR219    EQU   *                                                                
         PRINT OFF                                                              
*&&DO                                                                           
                                                                                
*                                                                               
*^^GYL                                                                          
         CLC   TRK_DA,TESTDA                                                    
         BE    PR234                                                            
*^^EOGYL                                                                        
         LA    RF,1                                                             
         A     RF,CNTRFIX                                                       
         SR    RE,RE                                                            
         D     RE,=F'201'          PRINT EVERY 201ST RECORD                     
         OR    RE,RE                                                            
         BNZ   PR239                                                            
*                                                                               
PR234    DS    0H                                                               
         MVI   GOSUBN,COR#         COPY ORIG INV RECD INTO SORTED ORDER         
         GOTO1 AGOSUB                                                           
*                                                                               
         DS    0H                  COMPARE DEMO ELEMENTS                        
         L     RE,AIO2                                                          
         LR    R0,RE                                                            
         ZICM  R1,(RINVLEN-RINVREC)(RE),(3)                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    RE,BUFF                                                          
         ZICM  RF,(RINVLEN-RINVREC)(RE),(3)                                     
                                                                                
         CLCL  R0,RE               ARE RECORDS EQUAL?                           
         BNE   *+8                                                              
         L     R0,AIO2                                                          
*&&DO                                                                           
         BE    PR239                YES                                         
*&&                                                                             
         L     R1,AIO2              NO, RECORDS DON'T AGREE                     
         SR    R0,R1                R0 = DISPLACEMENT TO MISMATCH               
         STH   R0,HALF                                                          
         MVI   GOSUBN,PMM#          REPORT MISMATCHING RECORDS                  
         GOTO1 AGOSUB                                                           
         OC    BUFF(L'RINVKEY),BUFF                                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
PR239    EQU   *                                                                
*&&                                                                             
         PRINT ON                                                               
                                                                                
*                                                                               
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
* WAIT UNTIL THE VERY END BEFORE ACTIVATING THE "PUTREC" INSTRUCTION            
                                                                                
         MVI   ACTELOPT,C'N'       DON'T MONITOR ACTIVITY                       
         L     R0,AIO              REMEMBER ADDRESS IN AIO                      
         LA    R3,BUFF                                                          
         ST    R3,AIO               AND PUT A(RE-XFERRED REC) IN IT             
         GOTO1 PUTREC                                                           
         ST    R0,AIO              RESTORE ORIGINAL ADDRESS INTO AIO            
PR259    EQU   *                                                                
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
                                                                                
*                                                                               
PR300    DS    0H                                                               
         CLI   TRKKQLFY,C' '       DON'T PRINT KEYS OF ACTUAL TRACKS            
         BE    PR305                                                            
                                                                                
         LA    R6,KEY                                                           
         ST    R6,FULL                                                          
         MVI   GOSUBN,FIK#                                                      
         GOTO1 AGOSUB                                                           
         ZIC   R1,BYTE                                                          
         BCTR  R1,0                                                             
         EXMVC R1,P1,WORK                                                       
         GOTO1 AGOSPOOL                                                         
*                                                                               
PR305    DS    0H                                                               
         LA    R0,TRKQTABQ                                                      
         L     R4,ATRKQTAB                                                      
                                                                                
PR305B   DS    0H                                                               
         CLC   TRKKQLFY,23(R4)                                                  
         BE    PR305D                                                           
         LA    R4,L'TRKQTAB(R4)                                                 
         BCT   R0,PR305B                                                        
         B     PR320                                                            
                                                                                
PR305D   DS    0H                                                               
         ZICM  RF,23+1(R4),(3)                                                  
         LA    RF,SYSD(RF)                                                      
         LA    R0,1                                                             
         A     R0,0(RF)                                                         
         ST    R0,0(RF)                                                         
         LA    R0,1                                                             
         A     R0,CNTTRKS                                                       
         ST    R0,CNTTRKS                                                       
                                                                                
*                                                                               
PR320    DS    0H                                                               
         LA    R0,1                                                             
         A     R0,CNTRFIX                                                       
         ST    R0,CNTRFIX                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         B     PR062               GO READ NEXT RECORD                          
         EJECT                                                                  
*                                                                               
** FINISHED READING FILE **                                                     
*                                                                               
PR600    DS    0H                                                               
         MVI   P1,0                                                             
         MVI   P2,0                                                             
         MVI   P3,0                                                             
         GOTO1 SPOOL,DMCB,SPOOLD                                                
                                                                                
*                                                                               
         L     R4,ATRKQTAB                                                      
         LA    R2,L'TRKQTAB                                                     
         LA    R3,(TRKQTABX-1)(R4)                                              
*                                                                               
PR612    DS    0H                                                               
         MVC   P(23),0(R4)                                                      
                                                                                
         CLI   23(R4),C'-'                                                      
         BNE   PR616                                                            
         MVC   P+24(10),=CL10'----------'                                       
         B     PR618                                                            
                                                                                
PR616    DS    0H                                                               
         ZICM  RF,23+1(R4),(3)                                                  
         LA    RF,SYSD(RF)                                                      
         L     R1,0(RF)                                                         
         EDIT  (R1),(10,P+24),COMMAS=YES,ZERO=NOBLANK                           
                                                                                
PR618    DS    0H                                                               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
*                                                                               
         BXLE  R4,R2,PR612                                                      
                                                                                
*                                                                               
         PRINT OFF                                                              
*&&DO                                                                           
         L     R4,ASTTMSTB                                                      
         LA    R2,L'STTMSGTB                                                    
         LA    R3,(STTMSTBX-1)(R4)                                              
*                                                                               
PR612    DS    0H                                                               
         MVC   P(25),0(R4)                                                      
                                                                                
         ZICM  RF,25(R4),(3)                                                    
         LA    RF,SYSD(RF)                                                      
         L     R1,0(RF)                                                         
         EDIT  (R1),(10,P+26),COMMAS=YES,ZERO=NOBLANK                           
                                                                                
         GOTO1 SPOOL,DMCB,SPOOLD                                                
*                                                                               
         BXLE  R4,R2,PR612                                                      
                                                                                
*                                                                               
*&&                                                                             
         PRINT ON                                                               
PRX      DS    0H                                                               
         B     XIT                                                              
                                                                                
                                                                                
* Little routine to insert a comma into the print line.                         
*  R2-->next output location in the print line.                                 
                                                                                
INSCOMMA DS    0H                                                               
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         BR    RE                                                               
                                                                                
                                                                                
* Little routine to go and print bad record.                                    
*  BADRCNUM = bad record message code                                           
                                                                                
PRPBR    NTR1                                                                   
         MVI   GOSUBN,PBR#                                                      
         GOTO1 AGOSUB                                                           
         B     XIT                                                              
***********************************************************************         
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (REPORT SPECS)'                
***********************************************************************         
*============================ REPORT SPECS ===========================*         
                                                                                
HEDSPECS SSPEC H1,1,C'INVENTORY FILE-FIX'                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C'FILEFIX REPORT'                                          
         SSPEC H2,52,C'--------------'                                          
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,01,C'IMPRESSION-BASED PRECISION FILEFIX FOR  ??  FILE+        
                 ???/??  BOOK'                                                  
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H5,112,PAGE                                                      
         DC    X'00'                                                            
***********************************************************************         
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (REPORT HOOK)'                 
***********************************************************************         
*========================== HEADLINE ROUTINE =========================*         
                                                                                
HDHOOK   NTR1                                                                   
         MVC   H4+40(2),IPREP                                                   
         MVC   H4+50(6),IPBOOK_P                                                
                                                                                
HDHOOKX  B     XIT                                                              
***********************************************************************         
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (MISCELLANEOUS)'               
***********************************************************************         
*===================== SUBROUTINE POOL INTERFACE =====================*         
                                                                                
         DS    0H                                                               
GOSUB    NTR1  BASE=MYBASE1,LABEL=N                                             
         L     R9,MYBASE2                                                       
                                                                                
         L     RE,4(RD)            PUT EYECATCHER IN MYSELF                     
         MVC   0(3,RE),=C'+GO'                                                  
         MVC   3(1,RE),GOSUBN                                                   
         SR    RE,RE                AND CLEAR  RE  JUST TO BE SAFE              
                                                                                
         MVC   ASUBRTN,ASUBR01                                                  
         CLI   GOSUBN,R01#                                                      
         BNH   GOSUBGO                                                          
         MVC   ASUBRTN,ASUBR02                                                  
         CLI   GOSUBN,R02#                                                      
         BNH   GOSUBGO                                                          
         MVC   ASUBRTN,ASUBR03                                                  
         CLI   GOSUBN,R03#                                                      
         BNH   GOSUBGO                                                          
         DC    H'0'                                                             
*                                                                               
GOSUBGO  DS    0H                                                               
         GOTO1 ASUBRTN,(RC)                                                     
*                                                                               
         DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*========================== SPOOL INTERFACE ==========================*         
                                                                                
         DS    0H                                                               
GOSPOOL  NTR1  BASE=MYBASE1,LABEL=N                                             
         L     R9,MYBASE2                                                       
                                                                                
         GOTO1 SPOOL,DMCB,SPOOLD                                                
                                                                                
         XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== MISCELLANEOUS STUFF ========================*         
                                                                                
*----------------- EXIT AND DISPLAY MESSAGE ROUTINES -----------------*         
                                                                                
OURERROR DS    0H                                                               
         MVI   BYTE,C'E'                                                        
         B     XMSGGO                                                           
                                                                                
OURWARN  DS    0H                                                               
         MVI   BYTE,C'W'                                                        
         B     XMSGGO                                                           
                                                                                
OURINFO  DS    0H                                                               
         MVI   BYTE,C'I'                                                        
         B     XMSGGO                                                           
                                                                                
XMSGGO   DS    0H                                                               
         GOTO1 AXMSGRTN,DMCB,(BYTE,(RC))                                        
         B     XIT                                                              
                                                                                
*------------------------------- GETEL -------------------------------*         
                                                                                
         DS    0H                                                               
         GETEL R3,DATADISP,ELCODE                                               
                                                                                
***********************************************************************         
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (LTORG && CONSTANTS)'          
***********************************************************************         
*========================= LTORG & CONSTANTS =========================*         
         LTORG                                                                  
                                                                                
                                                                                
MAXGOPMM DC    F'50'                     MAX # OF TIMES TO PMM# RTN             
DDSTEST  DC    C'Y'                                                             
                                                                                
BGNDATE  DC    XL3'610114'         JAN20/97                                     
ENDDATE  DC    XL3'61011A'         JAN26/97                                     
                                                                                
TESTDA   DC    XL4'1309C504'                                                    
                                                                                
                                                                                
MAINL    EQU   *-RMP0F                                                          
         DS    0CL(X'2000'-MAINL+1)                                             
***********************************************************************         
                                                                                
                                                                                
         DROP  R7,R8,R9,RA,RB,RC                                                
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR01)'                      
***********************************************************************         
*======================== SUBROUTINE POOL ONE ========================*         
                                                                                
* At entry,                                                                     
*   RA-->TWA,                                                                   
*   R8-->SYSD,                                                                  
*   R7-->SPOOLD,                                                                
*   R1-->GEND,                                                                  
*   GOSUBN = equated sub-routine number.                                        
                                                                                
         ORG   RMP0F+X'2000'                                                    
         ORG                                                                    
SUBR01Q  EQU   (((*-RMP0F+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP0F+SUBR01Q                                                    
SUBR01   NMOD1 0,**0F01**                                                       
         LR    RC,R1                                                            
         USING GEND,RC             RC=A(GENCON WORK AREA)                       
         USING CONHEADH-64,RA      RA=A(TWA)                                    
         USING SYSD,R8             R8=A(SYSD)                                   
         USING SPOOLD,R7           R7=A(SPOOL WORK AREA)                        
                                                                                
         ZIC   R1,GOSUBN                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R01_00(R1)                                                       
                                                                                
VSRC#    EQU   (R01_01-*)/4+1      VALIDATE SOURCE                              
TBK#     EQU   (R01_02-*)/4+1      TRANSLATE BOOK                               
VREP#    EQU   (R01_03-*)/4+1      VALIDATE REP CODE                            
BIL#     EQU   (R01_04-*)/4+1      BUILD INVENTORY LIST                         
CALF#    EQU   (R01_05-*)/4+1      CHECK ALPHABETICS                            
CNUM#    EQU   (R01_06-*)/4+1      CHECK NUMERICS                               
CAN#     EQU   (R01_07-*)/4+1      CHECK ALPHANUMERICS                          
DIV#     EQU   (R01_08-*)/4+1      DIVIDE LOGIC                                 
PMM#     EQU   (R01_09-*)/4+1      PRINT MISMATCH                               
                                                                                
R01_00   DS    0H                                                               
R01_01   B     VALSRC              VALIDATE SOURCE                              
R01_02   B     TRSLTBK             TRANSLATE BOOK                               
R01_03   B     VALREPCD            VALIDATE REP CODE                            
R01_04   B     BLDIVLST            BUILD INVENTORY LIST                         
R01_05   B     CHKALPH             CHECK ALPHABETICS                            
R01_06   B     CHKNUMRC            CHECK NUMERICS                               
R01_07   B     CHKALPH             CHECK ALPHANUMERICS                          
R01_08   B     DIVIDE              DIVIDE LOGIC                                 
R01_09   B     PMISMTCH            PRINT MISMATCH                               
R01#     EQU   (*-R01_00)/4                                                     
         DC    H'0'                                                             
                                                                                
YES_01   SR    RC,RC                                                            
NO_01    LTR   RC,RC                                                            
XIT_01   XIT1                                                                   
         TITLE 'RERMP0F - S(TATION) COPY REPORT (SUBR01--VSRC#)'                
*------------------- VALIDATE SOURCE (RATING SERVICE) ----------------*         
                                                                                
* Validates a field containing a variable-length source (rating srvce).         
* At entry,                                                                     
*   OURBYTE = length of data to validate,                                       
*   R0      = A(test specimen).                                                 
* At exit,                                                                      
*   TMPSRC = 3-char rating service code,                                        
*   CC set to equal if valid source,                                            
*   CC set to not equal if invalid source.                                      
                                                                                
VALSRC   DS    0H                                                               
         CLI   OURBYTE,3           IF LENGTH > MAX,                             
         BNH   *+6                                                              
         DC    H'0'                 SOMETHING'S INTERNALLY WRONG                
                                                                                
*                                                                               
         ZIC   R1,OURBYTE                                                       
         BCTR  R1,0                R1 = LEN FOR EX COMPARE                      
         LR    RE,R0               RE = A(TEST SPECIMEN)                        
                                                                                
         MVC   TMPSRC,=C'NSI'      ASSUME NIELSEN                               
         EX    R1,VSRCCLC                                                       
         BE    VSRCXY                                                           
         MVC   TMPSRC,=C'MFX'      ASSUME MEDIAFAX                              
         EX    R1,VSRCCLC                                                       
         BE    VSRCXY                                                           
         MVC   TMPSRC,=C'SRC'      ASSUME STRATEGY                              
         EX    R1,VSRCCLC                                                       
         BE    VSRCXY                                                           
         MVC   TMPSRC,=C'ARB'      ASSUME ARBITRON                              
         EX    R1,VSRCCLC                                                       
         BE    VSRCXY                                                           
*                                                                               
         XC    TMPSRC,TMPSRC       NONE OF THE ABOVE                            
         B     VSRCXN               RETURN INVALID FLAG                         
                                                                                
*                                                                               
VSRCXY   DS    0H                                                               
         B     YES_01                                                           
                                                                                
VSRCXN   DS    0H                                                               
         B     NO_01                                                            
                                                                                
                                                                                
* Should not fall through and execute this                                      
                                                                                
VSRCCLC  CLC   0(0,RE),TMPSRC                                                   
         TITLE 'RERMP0F - S(TATION) COPY REPORT (SUBR01--TBK#)'                 
*--------------------------- TRANSLATE BOOK --------------------------*         
                                                                                
* At entry,                                                                     
*   TMPBOOK = book to translate.                                                
*   TMPBTYP = book type, if applicable.                                         
*   TMPWKN  = week number, if applicable.                                       
* At exit,                                                                      
*   OURBYTE = L(converted book),                                                
*   WORK    = converted book.                                                   
                                                                                
TRSLTBK  DS    0H                                                               
                                                                                
         MVC   WORK,SPACES                                                      
         OC    TMPBOOK,TMPBOOK     IF BOOK IS NULLS,                            
         BZ    TBKX                 EXIT NOW                                    
                                                                                
*                                                                               
         LA    R2,WORK                                                          
         XC    DUB,DUB                                                          
         MVC   DUB(L'TMPBOOK),TMPBOOK                                           
                                                                                
         GOTO1 DATCON,DMCB,(X'83',DUB),(6,(R2))                                 
         MVC   3(2,R2),4(R2)       REMOVE SLASH                                 
         LA    R2,5(R2)                                                         
         MVI   0(R2),C' '                                                       
                                                                                
         CLI   TMPBTYP,0           IF THERE IS A BOOKTYPE,                      
         BE    TBK029                                                           
         MVI   0(R2),C'('           TACK IT ON                                  
         MVC   1(1,R2),TMPBTYP                                                  
         MVI   2(R2),C')'                                                       
         LA    R2,3(R2)                                                         
TBK029   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  FORMAT WEEK NUMBER                           
         CLI   TMPWKN,0                                                         
         BE    TBK039                                                           
         MVI   0(R2),C'-'                                                       
         MVC   1(1,R2),TMPWKN                                                   
         OI    1(R2),X'F0'                                                      
         LA    R2,2(R2)                                                         
TBK039   EQU   *                                                                
                                                                                
*                                                                               
         LA    R0,WORK                                                          
         SR    R2,R0                                                            
         STC   R2,OURBYTE                                                       
                                                                                
*                                                                               
TBKX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR01--VREP#)'               
*------------------------ VALIDATE REP AGENCY ------------------------*         
                                                                                
VALREPCD DS    0H                                                               
                                                                                
         L     RF,AREPLIST                                                      
VREP022  CLI   0(RF),EOT                                                        
         BE    VREPXN                                                           
         CLC   IPREP,0(RF)                                                      
         BE    VREPXY                                                           
         LA    RF,L'REPLIST(RF)                                                 
         B     VREP022                                                          
                                                                                
*                                                                               
VREPXY   DS    0H                                                               
         B     YES_01                                                           
*                                                                               
VREPXN   DS    0H                                                               
         B     NO_01                                                            
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR01--BIL#)'                
*------------------------ BUILD INVENTORY LIST -----------------------*         
                                                                                
* Builds a so-called "inventory list" (as referred to in RERMP11) from          
*  the RINVFRDT field of the RINVFREL element.                                  
* At entry,                                                                     
*   AIO = A(inventory record)                                                   
                                                                                
BLDIVLST DS    0H                                                               
         L     R0,AINVLIST                                                      
         LA    R1,INVLISTX-INVLIST                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR INVENTORY LIST AREA                    
                                                                                
*                                                                               
** GET FILE AND PRECISION **                                                    
*                                                                               
         DS    0H                  GET RINVCEL IN RECORD                        
         L     R3,AIO                                                           
         MVI   ELCODE,X'CD'                                                     
         MVC   DATADISP,DSP1EL                                                  
         BAS   RE,GETEL1                                                        
         BNE   BILXN                                                            
                                                                                
         USING RINVCEL,R3                                                       
         MVC   FDFILE,DCFILPT                                                   
         MVI   FDTPTT,0                                                         
         CLC   RINVCODE,=C'PT'                                                  
         BE    BIL010X                                                          
         MVC   FDFILE,DCFILPAV                                                  
         CLC   RINVCODE,SPACES                                                  
         BE    BIL010X                                                          
         CLC   RINVCODE,=C'ES'                                                  
         BE    BIL010X                                                          
         CLC   RINVCODE,=C'PJ'                                                  
         BE    BIL010X                                                          
         MVC   FDFILE,DCFILTP                                                   
         MVI   FDTPTT,C'P'                                                      
         CLC   RINVCODE,=C'TP'                                                  
         BE    BIL010X                                                          
         MVI   FDTPTT,C'T'                                                      
         CLC   RINVCODE,=C'TT'                                                  
         BE    BIL010X                                                          
                                                                                
         L     R1,AMONTAB                                                       
         LA    R0,MONTABQ                                                       
         CLC   0(1,R1),RINVCODE                                                 
         BE    *+16                                                             
         LA    R1,L'MONTAB(R1)                                                  
         BCT   R0,*-14                                                          
         B     BILXN                                                            
                                                                                
         CLI   RINVCODE+1,C'0'                                                  
         BL    BILXN                                                            
         CLI   RINVCODE+1,C'9'                                                  
         BH    BILXN                                                            
         MVC   FDFILE,DCFILPAV                                                  
         MVI   FDTPTT,0                                                         
         DROP  R3                                                               
BIL010X  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  GET PRECISION                                
         XC    ORIGBK,ORIGBK                                                    
         L     R3,AIO                                                           
         MVI   ELCODE,X'5E'                                                     
         BAS   RE,GETEL1                                                        
         BNE   BILXN                                                            
         MVC   ORIGBK,5(R3)                                                     
                                                                                
*                                                                               
** GET STATION, BOOK, AND BOOKTYPE **                                           
*                                                                               
         DS    0H                  GET RINVFREL IN RECORD                       
         L     R3,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         MVC   DATADISP,DSP1EL                                                  
         BAS   RE,GETEL1                                                        
         BNE   BILXN                                                            
                                                                                
         USING RINVFREL,R3                                                      
         MVC   FDSTTN,RINVFRST     STATION                                      
         MVC   FDFRBK,RINVFRBK     BOOK                                         
         MVC   FDBTYP,RINVFRBT     BOOK TYPE                                    
         MVC   FDFTYP,RINVFRTY     FROM TYPE                                    
         MVC   FDFRPR,RINVFRPR     FROM FUNCTION                                
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
** BUILD INVENTORY LIST FROM RINVFRDT **                                        
*                                                                               
         DS    0H                  CHECK IF ANY RINVFRDT DATA                   
         USING RINVFREL,R3                                                      
         CLI   RINVFRLN,RINVFRDT-RINVFREL                                       
         BE    BIL200                                                           
         CLI   RINVFRLN,RINVFRDT-RINVFREL+1   THIS IS JUST                      
         BH    BIL020                                                           
         CLI   RINVFRDT,0                      TO PROTECT MYSELF                
         BE    BIL200                                                           
         DROP  R3                                                               
*                                                                               
BIL020   DS    0H                  BUILD DUMMY TWA FIELD W/ RINVFRDT            
         XC    BLOCK(256),BLOCK                                                 
         USING RINVFREL,R3                                                      
         LA    R0,RINVFRDT-RINVFREL                                             
         ZIC   R1,RINVFRLN                                                      
         SR    R1,R0                                                            
         STC   R1,BLOCK+5                                                       
         BCTR  R1,0                                                             
         EXMVC R1,BLOCK+8,RINVFRDT                                              
         LA    R1,8+1(R1)                                                       
         STC   R1,BLOCK                                                         
         DROP  R3                                                               
                                                                                
         DS    0H                  RUN SCANNER THROUGH RINVFRDT DATA            
         MVI   SCANLNTH,10                                                      
         GOTO1 SCANNER,DMCB,(SCANLNTH,BLOCK),BUFF,0                             
         ZICM  R0,DMCB+4,(1)                                                    
         BNZ   BIL030                                                           
         DC    H'0'                                                             
                                                                                
BIL030   DS    0H                                                               
         STC   R0,COUNTER                                                       
         LA    R4,BUFF                                                          
         L     R2,AINVLIST                                                      
         USING INVLD,R2                                                         
         BAS   RE,BILFLLFD                                                      
         MVI   INVLQ(R2),EOT       ASSUME THIS IS LAST ENTRY IN INV LST         
                                                                                
                                                                                
BILPFX   DS    0H                  ASSUME PREFIX                                
         CLI   0(R4),0              MAKE SURE THERE IS INPUT                    
         BH    BILPFX10                                                         
         BAS   RE,BILBSCAN          SEE IF NEXT ENTRY HAS INPUT                 
         BP    BILPFX                                                           
         B     BILXN                EXIT IF THERE SEEMS TO BE NO INPUT          
                                                                                
BILPFX10 DS    0H                                                               
         CLI   12(R4),C'+'                                                      
         BE    BILPFX20                                                         
         CLI   12(R4),C'/'                                                      
         BE    BILPFX30                                                         
         B     BILSTA              NO PREFIX FOUND                              
                                                                                
BILPFX20 DS    0H                                                               
         OI    ILTYP,ILTADD        FLAG FOR "ADD" EXPRESSION                    
         B     BILPFX50                                                         
                                                                                
BILPFX30 DS    0H                                                               
         B     BILPFX50            FOOTNOTE SUPPRESSION--LET IT PASS            
                                                                                
BILPFX50 DS    0H                  REMOVE PREFIX & DECREMENT LENGTH             
         CLI   0(R4),1              PREFIX SHOULDN'T BE ALONE                   
         BH    *+6                                                              
         DC    H'0'                                                             
         ZIC   RF,0(R4)                                                         
         BCTR  RF,0                                                             
         STC   RF,0(R4)             NEW LENGTH                                  
         BCTR  RF,0                                                             
         EXMVC RF,12(R4),13(R4)     SHIFT DATA 1 BYTE TO LEFT                   
         LA    RF,12+1(RF,R4)                                                   
         MVI   0(RF),C' '           BLANK OUT LAST CHAR                         
*                                                                               
BILSTA   DS    0H                  ASSUME STATION                               
         ZIC   R0,0(R4)             CHECK IF 1ST IS A DAY                       
         GOTO1 VINVDAY,DMCB,((R0),12(R4)),DUB,DUB+1,DAYVAL                      
         CLI   DUB,0                                                            
         BNE   BILDYT05              YEP, 1ST ENTRY IS A DAY EXPRESSION         
                                                                                
         DS    0H                   SEE IF 1ST IS A STATION                     
         GOTO1 VPAVSTA,DMCB,12(R4),DUB                                          
         CLI   DMCB+4,XFF                                                       
         BE    BILNUM                NOPE, 1ST ENTRY NOT A STATION              
         MVC   FDSTTN,DUB            YEP, SAVE STATION                          
         MVC   FDMED,DUB+5            AND MEDIA                                 
         CLI   COUNTER,2            STATION MUST BE FOLLOWED                    
         BNL   BILSKIP               BY SOMETHING ELSE                          
         DC    H'0'                                                             
*                                                                               
BILNUM   DS    0H                  ASSUME INVENTORY/PURE NUMBER                 
         SR    R0,R0                                                            
         BAS   RE,BILFLLFD                                                      
         MVI   INVLQ(R2),EOT       ASSUME THIS IS LAST ENTRY IN INV LST         
                                                                                
         TM    RMPPROFS,X80        IS THIS A SELF-DEFINED INVENTORY?            
         BZ    BILNUM10                                                         
         CLI   12(R4),C'*'                                                      
         BNE   BILNUM10                                                         
         MVC   12(4,R4),13(R4)                                                  
         B     BILNUM20                                                         
                                                                                
BILNUM10 DS    0H                                                               
         CLI   0(R4),3             INVENTORY NUMBER MUST BE 3                   
         BL    BILDYT                                                           
         CLI   0(R4),4              OR 4 CHARACTERS LONG                        
         BH    BILDYT                                                           
                                                                                
         CLI   12(R4),C'0'         QTR HOUR NUMBER MUST BE 00 - 99              
         BL    BILDYT                                                           
         CLI   12(R4),C'9'                                                      
         BH    BILDYT                                                           
         CLI   13(R4),C'0'                                                      
         BL    BILDYT                                                           
         CLI   13(R4),C'9'                                                      
         BH    BILDYT                                                           
                                                                                
BILNUM20 DS    0H                                                               
         CLI   FDFTYP,C'I'                                                      
         BNE   BILNUM25                                                         
         MVC   ILINVN,12(R4)       MOVE INVENTORY NUMBER OUT                    
         OC    ILINVN,SPACES                                                    
         B     BILNUM60                                                         
                                                                                
BILNUM25 DS    0H                                                               
         PACK  DUB,12(2,R4)                                                     
         CVB   R1,DUB                                                           
         STC   R1,ILPURE           QUARTER HOUR NUMBER                          
                                                                                
         CLI   14(R4),C'D'         TYPICAL                                      
         BE    BILNUM30                                                         
         CLI   14(R4),C'E'         WEEKEND                                      
         BE    BILNUM30                                                         
         CLI   14(R4),C'0'                                                      
         BL    *+16                                                             
         CLI   14(R4),C'9'                                                      
         BH    *+8                                                              
         B     BILNUM30                                                         
         DC    H'0'                                                             
                                                                                
BILNUM30 DS    0H                                                               
         MVC   ILPURE+1(1),14(R4)                                               
         GOTO1 VINVDAY,DMCB,1,(0,14(R4)),(0,ILPURE+1)  PAV DAY CODE             
                                                                                
         CLI   0(R4),3             PURE 3 CHAR CODE                             
         BE    BILNUM60             IS OK                                       
         ZIC   RF,15(R4)           START WEEK (4TH CHAR)                        
         SR    RE,RE                                                            
         SLDL  RE,28               RE HAS 4 HIGH ORDER BITS                     
         SRL   RF,28               RF HAS 4 LOW  ORDER BITS                     
         CH    RF,=H'7'            0-7 , A-G                                    
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,1                SHIFT TO BIT POSITIONS 4, 5, & 6             
         EX    RF,*+8                                                           
         B     *+8                                                              
         OI    ILPURE+1,0                                                       
                                                                                
         CH    RE,=H'15'           IF RE <> X'F0',                              
         BNE   BILNUM44             THEN IT'S NOT A DIGIT                       
         LTR   RF,RF               IF THE DIGIT IS 0,                           
         BZ    BILNUM46             HANDLE IT DIFFERENTLY                       
         B     BILNUM60                                                         
                                                                                
BILNUM44 DS    0H                  IT'S NOT A DIGIT                             
         CH    RE,=H'12'            THEN IT BETTER BE A X'C0'                   
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
BILNUM46 DS    0H                                                               
         OI    ILPURE+1,X01        LOW ORDER ON FOR ZERO & A-G                  
         B     BILNUM60                                                         
                                                                                
BILNUM60 DS    0H                                                               
         OI    ILTYP,ILTIVN        IT IS AN INVENTORY NUMBER                    
         MVI   ILWT,1              WEIGHT                                       
                                                                                
         CLI   1(R4),0             ANY OVERRIDING WEIGHT?                       
         BE    BILDAT               NOPE                                        
         OC    8(4,R4),8(R4)                                                    
         BNZ   *+6                                                              
         DC    H'0'                WEIGHT S/B NUMBERIC                          
         CLC   8(4,R4),=F'99'      99 SHOULD BE ENOUGH                          
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   ILWT,11(R4)         MOVE WEIGHT TO LIST                          
         OI    ILTYP,ILTWOV         AND INDICATE WEIGHT OVERRIDE                
*                                                                               
BILDAT   DS    0H                  A (EFFCTVE) DATE MAY FOLLOW INV #            
         CLI   ILFLE,C'P'                                                       
         BE    BILNEXT                                                          
         CLI   COUNTER,2            IS THERE ANOTHER ENTRY LEFT?                
         BL    BILNEXT               NOPE                                       
                                                                                
         ZIC   R5,SCANLNTH           YES, CHECK IF IT'S A DATE                  
         LA    R5,22(R5,R4)                                                     
         GOTO1 DATVAL,DMCB,(0,12(R5)),DUB                                       
         OC    DMCB,DMCB            IS IT A VALID DATE?                         
         BZ    BILNEXT                                                          
         CLI   1(R5),0               YES, THEN IT SHOULDN'T HAVE                
         BE    *+6                    A SECOND HALF                             
         DC    H'0'                                                             
                                                                                
         LR    R4,R5               BUMP TO SCANNER ENTRY W/ DATE                
         ZIC   R1,COUNTER                                                       
         BCTR  R1,0                                                             
         STC   R1,COUNTER          DECREMENT LOOP COUNTER                       
         GOTO1 DATCON,DMCB,(0,DUB),(2,ILEFFDT)                                  
         B     BILNEXT                                                          
*                                                                               
BILDYT   DS    0H                  DAY & TIME EXPRESSIONS                       
         ZIC   R0,0(R4)                                                         
         GOTO1 VINVDAY,DMCB,((R0),12(R4)),DUB,DUB+1,DAYVAL                      
         CLI   DUB,0                                                            
         BNE   BILDYT05                                                         
         B     BILBAD              IF STILL INVALID, FILE IS MESSED UP          
                                                                                
BILDYT05 DS    0H                                                               
         CLI   1(R4),0             SHOULD NOT HAVE 2ND HALF OF FIELD            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   ILFLE,C'I'                                                       
         BE    BILDYT30                                                         
         OI    ILTYP,ILTFDT+ILTLDT                                              
                                                                                
BILDYT20 DS    0H                  CHECK FOR MULTIPLE DAYS                      
         ZIC   R0,0(R4)                                                         
         GOTO1 DAYVAL,DMCB,((R0),12(R4)),DUB,DUB+1                              
         CLI   DUB,0                                                            
         BE    BILDYT35            NOT A DAY EXPRESSION ANYMORE                 
         OC    ILIDAY,DUB                                                       
                                                                                
         DS    0H                  CHECKING FOR MULTIPLE DAYS                   
         BAS   RE,BILBSCAN                                                      
         BP    BILDYT20             YES THERE IS MORE                           
         DC    H'0'                                                             
                                                                                
BILDYT25 DS    0H                                                               
         CLI   ILIDAY,0            ANY DAY EXPRESSIONS FOUND?                   
         BNE   BILDYT35             YES (R0 = L'INPUT OF NEXT ENTRY)            
         DC    H'0'                                                             
                                                                                
BILDYT30 DS    0H                  EXPECT NEXT ENTRY TO BE A TIME EXP           
         BAS   RE,BILBSCAN          BUMP TO NEXT SCANNER ENTRY                  
         BP    *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,0(R4)            R0 = L(TIME EXPRESSION)                      
                                                                                
BILDYT35 DS    0H                  MAKE SURE R0 = L(TIME EXP)                   
         GOTO1 TIMVAL,DMCB,((R0),12(R4)),DUB+4                                  
         CLI   DMCB,XFF                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DUB+6(2),=H'2400'                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   ILFLE,C'P'                                                       
         BNE   BILDYT33                                                         
         MVC   ILTIME,DUB+4                                                     
         B     BILNEXT                                                          
*                                                                               
BILDYT33 DS    0H                                                               
         GOTO1 VHRTOQH,DMCB,DUB+4,FULL                                          
         ZIC   R0,FULL                                                          
         STC   R0,ILINVN           QUARTER-HOUR CODE                            
         MVC   ILINVN+1(1),DUB     INV DAY CODE                                 
         MVI   ILINVN+2,C'0'       PROGRAM LENGTH                               
                                                                                
         MVI   ILWT,1              WEIGHT                                       
         OI    ILTYP,ILTFDT+ILTLDT  FIRST & LAST                                
                                                                                
         OC    DUB+6(2),DUB+6      ANY END TIME?                                
         BZ    BILNEXT              NOPE                                        
                                                                                
         GOTO1 VHRTOQH,DMCB,DUB+6,FULL                                          
         ZIC   RF,FULL                                                          
         LR    RE,R0                                                            
         SR    RF,RE               END QH MINUS START QH                        
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BNP   BILNEXT                                                          
         NI    ILTYP,XFF-ILTIVN-ILTLDT                                          
                                                                                
BILDYT36 DS    0H                                                               
         MVC   INVLQ(INVLQ,R2),0(R2)  SET UP NEXT OUTPUT AREA                   
         LA    R2,INVLQ(R2)                                                     
         MVI   INVLQ(R2),EOT          ASSME THIS LAST NTRY IN INV LST           
         LA    RE,1(RE)               NEXT QUARTER HOUR                         
         STC   RE,ILINVN                                                        
         NI    ILTYP,XFF-ILTIVN-ILTFDT-ILTLDT                                   
         BCT   RF,BILDYT36                                                      
         OI    ILTYP,ILTLDT           LAST IN LIST                              
                                                                                
                                                                                
BILBAD   DS    0H                  FILE IS MESSED UP                            
         B     BILNEXT              IGNORE IT FOR NOW                           
                                                                                
                                                                                
BILNEXT  DS    0H                  BUMP TO NEXT OUTPUT AREA                     
         LA    R2,INVLQ(R2)                                                     
*                                                                               
BILSKIP  DS    0H                  BUMP TO NEXT SCANNER ENTRY                   
         BAS   RE,BILBSCAN                                                      
         BNP   BIL200               NO MORE ENTRIES LEFT                        
         CLI   0(R4),0              IF THIS ENTRY HAS NO DATA,                  
         BE    BILSKIP               GET NEXT ENTRY                             
         B     BILNUM              MORE "FROM DETAILS" TO GET                   
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
** GET DAYS/TIMES FROM TRACK INTO INVENTORY LIST **                             
*                                                                               
BIL200   DS    0H                                                               
         L     R2,AINVLIST                                                      
         USING INVLD,R2                                                         
         CLI   0(R2),EOT                                                        
         BNE   BIL249                                                           
                                                                                
*                                                                               
         DS    0H                  ASSUME NO X'CE' ELEM IN TRACK                
         BAS   RE,BILFLLFD                                                      
         MVI   INVLQ(R2),EOT                                                    
         MVC   ILTIME,HDRIDAY       USE DAY &                                   
         MVC   ILIDAY,HDRTIME        TIME FROM HEADER                           
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'CE'                                                     
         MVC   DATADISP,DSP1EL                                                  
         BAS   RE,GETEL1                                                        
         BNE   BIL249                                                           
                                                                                
BIL212   DS    0H                                                               
         USING RINVZEL,R3                                                       
         BAS   RE,BILFLLFD                                                      
         MVI   INVLQ(R2),EOT                                                    
         MVC   ILTIME,RINVZTIM                                                  
         MVC   ILIDAY,RINVZDAY                                                  
         DROP  R2,R3                                                            
                                                                                
         BAS   RE,NEXTEL1                                                       
         BNE   BIL249                                                           
         LA    R2,INVLQ(R2)                                                     
         B     BIL212                                                           
                                                                                
*                                                                               
BIL249   EQU   *                                                                
                                                                                
*                                                                               
** FINE-TUNE INVLIST ENTRIES **                                                 
*                                                                               
         DS    0H                                                               
         L     R2,AINVLIST                                                      
         USING INVLD,R2                                                         
                                                                                
BIL262   DS    0H                                                               
         CLI   0(R2),EOT           MAKE R3-->END OF INVLIST                     
         BE    BIL269                                                           
                                                                                
*                                                                               
         CLC   ILDFILE,DCFILPT      MIXED PAV/TP FILE?                          
         BNE   BIL268                NO, CHECK NEXT ENTRY                       
                                                                                
         TM    ILTYP,ILTIVN                                                     
         BNO   *+14                                                             
         MVC   ILDFILE,DCFILPAV                                                 
         MVI   ILBEST,C'B'                                                      
                                                                                
         TM    ILTYP,ILTIVN                                                     
         BO    *+14                                                             
         MVC   ILDFILE,DCFILTP                                                  
         MVI   ILTPTT,C'T'                                                      
                                                                                
         B     BIL268                                                           
                                                                                
*                                                                               
BIL268   DS    0H                                                               
         LA    R2,INVLQ(R2)                                                     
         B     BIL262                                                           
         DROP  R2                                                               
*                                                                               
BIL269   EQU   *                                                                
         B     BILXY                                                            
         EJECT                                                                  
*                                                                               
** BUILD INVENTORY LIST EXITS **                                                
*                                                                               
BILXN    DS    0H                                                               
         B     NO_01                                                            
*                                                                               
BILXY    DS    0H                                                               
         B     YES_01                                                           
                                                                                
                                                                                
* Little helper routine to bump R4 to next scanner entry and decrement          
*  COUNTER, the number of scanner entries left.                                 
* NOTE: R1 will get clobbered here!                                             
                                                                                
BILBSCAN DS    0H                  BUMP TO NEXT SCANNER ENTRY                   
         ZIC   R1,SCANLNTH                                                      
         LA    R4,22(R1,R4)                                                     
         IC    R1,COUNTER                                                       
         BCTR  R1,0                                                             
         STC   R1,COUNTER                                                       
         LTR   R1,R1               RETURN CONDITION CODE                        
         BR    RE                                                               
                                                                                
                                                                                
* Little helper routine to fill the From Detail information (fields             
*  prefixed w/ c'FD') into an inventory list entry.                             
* At entry,                                                                     
*  R2-->an inventory list entry                                                 
                                                                                
BILFLLFD DS    0H                  FILL INVLIST ENTRY                           
         USING INVLD,R2                                                         
         MVC   ILFLE,FDFTYP                                                     
         MVC   ILDFILE,FDFILE                                                   
         CLC   ILDFILE,DCFILPAV                                                 
         BNE   *+8                                                              
         MVI   ILBEST,C'B'                                                      
         MVC   ILTPTT,FDTPTT                                                    
         MVC   ILSRC,FDSRC                                                      
         MVC   ILSTTN,FDSTTN                                                    
         MVC   ILBOOK,FDBOOK                                                    
         MVC   ILBTYP,FDBTYP                                                    
         DROP  R2                                                               
*                                                                               
         BR    RE                                                               
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR01--CALF#, CNUM#,+        
                && CAN#)'                                                       
*-------------------------- CHECK ALPHABETIC -------------------------*         
                                                                                
* At entry,                                                                     
*   BYTE = L(specimen)                                                          
*   WORK contains specimen                                                      
                                                                                
CHKALPH  DS    0H                                                               
         ZIC   R0,BYTE                                                          
         LA    RF,WORK                                                          
*                                                                               
CALF010  DS    0H                                                               
         CLI   0(RF),C'A'                                                       
         BL    CALFXN                                                           
         CLI   0(RF),C'I'                                                       
         BNH   CALF020                                                          
                                                                                
         CLI   0(RF),C'J'                                                       
         BL    CALFXN                                                           
         CLI   0(RF),C'R'                                                       
         BNH   CALF020                                                          
                                                                                
         CLI   0(RF),C'S'                                                       
         BL    CALFXN                                                           
         CLI   0(RF),C'Z'                                                       
         BH    CALF030                                                          
*                                                                               
CALF020  DS    0H                  THIS CHARACTER IS AN ALPHABET                
         LA    RF,1(RF)                                                         
         BCT   R0,CALF010                                                       
         B     CALFXY                                                           
*                                                                               
CALF030  DS    0H                  NON-ALPHABETIC CHARACTER FOUND               
         CLI   GOSUBN,CAN#          IF TO CHECK ALPHANUMERICS,                  
         BE    CHKNUMRC              GO CHECK NUMERICS NOW                      
         B     CALFXN               ELSE, IT'S AN ERROR                         
                                                                                
*                                                                               
CALFXY   DS    0H                                                               
         B     YES_01                                                           
*                                                                               
CALFXN   DS    0H                                                               
         B     NO_01                                                            
         EJECT                                                                  
*--------------------------- CHECK NUMERICS --------------------------*         
                                                                                
* At entry,                                                                     
*   BYTE = L(specimen)                                                          
*   WORK contains specimen                                                      
                                                                                
CHKNUMRC DS    0H                                                               
         ZIC   R0,BYTE                                                          
         LA    RF,WORK                                                          
*                                                                               
CNUM010  DS    0H                                                               
         CLI   0(RF),C'0'                                                       
         BL    CNUMXN                                                           
         CLI   0(RF),C'9'                                                       
         BH    CNUMXN                                                           
*                                                                               
         DS    0H                  THIS CHARACTER IS A NUMERIC                  
         LA    RF,1(RF)                                                         
         BCT   R0,CNUM010                                                       
         B     CNUMXY                                                           
                                                                                
*                                                                               
CNUMXY   DS    0H                                                               
         B     YES_01                                                           
*                                                                               
CNUMXN   DS    0H                                                               
         B     NO_01                                                            
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR01--DIV#)'                
*------------------------ GENERIC DIVIDE LOGIC -----------------------*         
                                                                                
* At entry, DIVIDEND, DIVISOR are set.                                          
* At exit, QUOTIENT and REMAINDR are set.                                       
                                                                                
DIVIDE   DS    0H                                                               
         XC    QUOTIENT,QUOTIENT                                                
         XC    REMAINDR,REMAINDR                                                
                                                                                
         ICM   RF,15,DIVISOR       IF DIVISOR IS ZERO,                          
         BZ    XIT_01               CALLER GETS ZERO BACK                       
         OC    DIVIDEND,DIVIDEND   IF DIVIDEND IS ZERO                          
         BZ    XIT_01               CALLER GETS ZERO BACK ALSO                  
                                                                                
         DS    0H                  CALCULATE QUOTIENT                           
         LM    R0,R1,DIVIDEND                                                   
         SLDA  R0,1                DOUBLE DIVIDEND                              
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,QUOTIENT                                                      
                                                                                
         DS    0H                  CALCULATE REMAINDER                          
         AH    R0,=H'1'                                                         
         SRA   R0,1                                                             
         ST    R0,REMAINDR                                                      
                                                                                
         B     XIT_01                                                           
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR01--PMM#)'                
*--------------------------- PRINT MISMATCH --------------------------*         
                                                                                
* Prints the original record on the left side of the page, and the              
*  created record on the right side of the page.  This is so that they          
*  can be compared side-to-side to find the mismatch(es).                       
* At entry,                                                                     
*   KEY  = key of original record                                               
*   AIO2 = A(copy of original record)                                           
*   BUFF = created record                                                       
*   HALF = displacement to mismatch                                             
* In routine,                                                                   
*   R4 --> area containing hexed-out data from original record                  
*   R5 -->  "       "        "    "   "    "   created    "                     
                                                                                
PMISMTCH DS    0H                                                               
         XC    PMMWORK(PMMWORKL),PMMWORK                                        
                                                                                
*                                                                               
         L     R4,AIO3             R4-->HEXED-OUT DATA FROM ORIG REC            
         LA    R5,1000(R4)         R5-->     "     "    "   CREATED REC         
                                                                                
*                                                                               
** PRINT HEADING **                                                             
*                                                                               
         MVI   P1,0                                                             
         MVI   P2,0                                                             
         MVC   P3(16),=C'RECORD MISMATCH:'                                      
         OC    HALF,HALF                                                        
         BNZ   *+10                                                             
         MVC   P3(16),=C'RECORDS   MATCH:'                                      
         MVC   ((P4+1)+20)(14),=C'ORIGINAL (D/A='                               
         MVC   ((P4+1)+34)(8),PRNT_DA                                           
         MVI   ((P4+1)+42),C')'                                                 
         MVC   ((P4+67)+29)(7),=C'CREATED'                                      
                                                                                
*                                                                               
         DS    0H                  TELL LOCATION OF MISMATCH                    
         OC    HALF,HALF                                                        
         BNZ   *+12                                                             
         LA    R2,P3+17                                                         
         B     PMM037                                                           
                                                                                
         L     R1,AIO2                                                          
         LR    RE,R1                                                            
         LA    RF,(RINVPEL-RINVREC)(RE)                                         
         AH    R1,HALF                                                          
*                                                                               
         DS    0H                   CHECK KEY AND CONTROL BYTES                 
         CR    R1,RF                 IS MISMATCH IN HERE?                       
         BNL   PMM024                 NO, CHECK ELEMENTS                        
         MVI   MMELCODE,0                                                       
         B     PMM028                                                           
*                                                                               
PMM024   DS    0H                   CHECK ELEMENTS                              
         CLI   0(RF),0               ARE WE AT END OF RECORD?                   
         BE    PMM029                 YES, JUST PRINT WHAT WE HAVE              
         LR    RE,RF                 BUMP START POINTER                         
         ZIC   R0,1(RF)                                                         
         AR    RF,R0                 BUMP END POINTER                           
         CR    R1,RF                 IS MISMATCH IN THIS ELEMENT?               
         BNL   PMM024                 NO, BUMP TO NEXT ELEMENT                  
         MVC   MMELCODE,0(RE)         YES, THIS IS ELEMENT WE WANT              
         B     PMM028                                                           
*                                                                               
PMM028   DS    0H                                                               
         SR    R1,RE                R1 = DISPL INTO SEGMENT OF RECORD           
         BAS   RE,PMMFDSP           FORMAT DISPLACEMENT MESSAGE                 
         ZIC   R1,BYTE              R1 = L(LOCATION MESSAGE)                    
         BCTR  R1,0                                                             
         EXMVC R1,P3+17,WORK        WORK = LOCATION MESSAGE                     
                                                                                
*                                                                               
PMM029   DS    0H                                                               
         LA    R2,P3+17+1(R1)                                                   
                                                                                
*                                                                               
         DS    0H                  R2-->NXT AVAILABLE AREA ON PRNTLINE          
         MVC   1(2,R2),=C'OF'                                                   
         LA    R2,1+2+2(R2)         (SKIP 2 SPACES AFTER "OF")                  
                                                                                
*                                                                               
PMM037   DS    0H                                                               
         MVC   FULL,AIO2                                                        
         MVI   GOSUBN,FIK#                                                      
         GOTO1 AGOSUB                                                           
         ZIC   R1,BYTE                                                          
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),WORK                                                    
                                                                                
*                                                                               
PMM039   DS    0H                                                               
         GOTO1 AGOSPOOL                                                         
                                                                                
*                                                                               
** PRINT KEYS **                                                                
*                                                                               
         L     R2,AIO2             R2-->ORIGINAL RECORD                         
         LA    R3,BUFF             R3-->CREATED RECORD                          
                                                                                
         DS    0H                  FORMAT KEY OF ORIGINAL RECORD                
         LA    R0,(RINVLEN-RINVREC)+L'RINVLEN                                   
         LH    R0,DSP1EL                                                        
         GOTO1 HEXOUT,DMCB,(R2),(R4),(R0),=C'TOG',0,0                           
         ST    R4,FULL              SET A(DATA TO PRINT)                        
         L     R0,DMCB+16           SET L(DATA TO PRINT)                        
         MVI   PMMINDT,0                                                        
         BAS   RE,PMMFMTL                                                       
                                                                                
         DS    0H                  FORMAT KEY OF CREATED  RECORD                
         LA    R0,(RINVLEN-RINVREC)+L'RINVLEN                                   
         LH    R0,DSP1EL                                                        
         GOTO1 HEXOUT,DMCB,(R3),(R5),(R0),=C'TOG',0,0                           
         ST    R5,FULL              SET A(DATA TO PRINT)                        
         L     R0,DMCB+16           SET L(DATA TO PRINT)                        
         BAS   RE,PMMFMTR                                                       
                                                                                
         DS    0H                  PRINT KEYS OF RECORDS                        
         BAS   RE,PMMPRT                                                        
                                                                                
*                                                                               
** PRINT DEMO ELEMENTS **                                                       
*                                                                               
         LA    R2,(RINVPEL-RINVREC)(R2)  R2-->1ST ELEM OF ORIG RECD             
         LA    R3,(RINVPEL-RINVREC)(R3)  R3-->1ST ELEM OF CREATED RECD          
         MVI   LPMMBUMP,C'N'                                                    
         MVI   RPMMBUMP,C'N'                                                    
*                                                                               
         B     PMM090                                                           
                                                                                
*                                                                               
PMM085   DS    0H                                                               
         MVC   ELCODE,MMELCODE     GET ELEMENT W/ MISMATCH                      
                                                                                
         CLI   LPMMBUMP,C'Y'                                                    
         BNE   *+20                                                             
         MVI   LPMMBUMP,C'N'                                                    
         ST    R2,FULL                                                          
         BAS   RE,PMMBUMP                                                       
         L     R2,FULL                                                          
                                                                                
         CLI   RPMMBUMP,C'Y'                                                    
         BNE   *+20                                                             
         MVI   RPMMBUMP,C'N'                                                    
         ST    R3,FULL                                                          
         BAS   RE,PMMBUMP                                                       
         L     R3,FULL                                                          
*                                                                               
PMM090   DS    0H                  HANDLE END-OF-RECD CONDITIONS                
         CLI   0(R2),0              ARE WE AT END OF ORIGINAL RECD?             
         BNE   PMM092                NOPE                                       
         CLI   0(R3),0               YEP, IF AT END OF CRTD RECD ALSO,          
         BE    PMMX                   EXIT PMM# ROUTINE NOW                     
         B     PMM210                ELSE, PRNT DATA FROM CRTD REC ONLY         
                                                                                
*                                                                               
PMM092   DS    0H                   NOT AT END OF ORIGINAL RECORD               
         CLI   0(R3),0               ARE WE AT END OF CREATED RECORD?           
         BE    PMM110                 YES, PRNT DATA FROM ORIG REC ONLY         
                                                                                
*                                                                               
*** FORMAT DATA FROM ORIGINAL RECORD ***                                        
*                                                                               
PMM100   DS    0H                  DATA FROM ORIGINAL RECORD                    
         CLC   0(1,R2),0(R3)        COMPARE ELEM CODES                          
         BH    PMM199                IF ORIG > CREATED, DON'T PRNT ORIG         
*                                                                               
PMM110   DS    0H                                                               
         ZIC   R0,1(R2)             R0 = L(ELEMENT)                             
         GOTO1 HEXOUT,DMCB,(R2),(R4),(R0),=C'TOG',0,0                           
         ST    R4,FULL              SET A(DATA TO PRINT)                        
         L     R0,DMCB+16           SET L(DATA TO PRINT)                        
         MVI   PMMINDT,2                                                        
         BAS   RE,PMMFMTL                                                       
         MVI   LPMMBUMP,C'Y'                                                    
*                                                                               
PMM199   EQU   *                                                                
                                                                                
*                                                                               
*** FORMAT DATA FROM CREATED RECORD ***                                         
*                                                                               
PMM200   DS    0H                  DATA FROM CREATED  RECORD                    
         CLC   0(1,R2),0(R3)        COMPARE ELEM CODES                          
         BL    PMM299                IF ORIG < CREATED, DON'T PRNT CRTD         
*                                                                               
PMM210   DS    0H                                                               
         ZIC   R0,1(R3)             R0 = L(ELEMENT)                             
         GOTO1 HEXOUT,DMCB,(R3),(R5),(R0),=C'TOG',0,0                           
         ST    R5,FULL              SET A(DATA TO PRINT)                        
         L     R0,DMCB+16           SET L(DATA TO PRINT)                        
         MVI   PMMINDT,2                                                        
         BAS   RE,PMMFMTR                                                       
         MVI   RPMMBUMP,C'Y'                                                    
*                                                                               
PMM299   EQU   *                                                                
                                                                                
*                                                                               
PMM300   DS    0H                  PRINT ELEMENT DATA OF RECORDS                
         BAS   RE,PMMPRT                                                        
         B     PMM085                                                           
         EJECT                                                                  
*                                                                               
** EXIT PRINT MISMATCH RECORDS ROUTINE **                                       
*                                                                               
PMMX     DS    0H                                                               
         LA    R0,1                                                             
         A     R0,CNTGOPMM                                                      
         ST    R0,CNTGOPMM                                                      
*                                                                               
         B     XIT_01                                                           
         EJECT                                                                  
* Helper routine to set parameters to format LHS print area.                    
* !!Warning!! Do NOT clobber RE                                                 
* At entry,                                                                     
*   FULL    = A(stuff to print)                                                 
*   R0      = L(stuff to print)                                                 
*   PMMINDT = # of spaces to indent                                             
                                                                                
PMMFMTL  DS    0H                                                               
         MVC   LPRTADDR,FULL                                                    
         STH   R0,LPRTLEN                                                       
         MVC   LPRTINDT,PMMINDT                                                 
         BR    RE                                                               
                                                                                
                                                                                
* Helper routine to set parameters to format RHS print area.                    
* !!Warning!! Do NOT clobber RE                                                 
* At entry,                                                                     
*   FULL    = A(stuff to print)                                                 
*   R0      = L(stuff to print)                                                 
*   PMMINDT = # of spaces to indent                                             
                                                                                
PMMFMTR  DS    0H                                                               
         MVC   RPRTADDR,FULL                                                    
         STH   R0,RPRTLEN                                                       
         MVC   RPRTINDT,PMMINDT                                                 
         BR    RE                                                               
                                                                                
                                                                                
* Helper routine to bump pointers to the next element.  The contents            
*  of R0 & RF will be clobbered.                                                
* !!Warning!! Do NOT clobber RE                                                 
* At entry,                                                                     
*   FULL    = A(starting point of pointer)                                      
*   ELCODE  = element code (if 0, pass back all elements)                       
*   ELCDHI  = high element code                                                 
* At exit,                                                                      
*   FULL    = A(ending point of pointer)                                        
                                                                                
PMMBUMP  DS    0H                                                               
         L     RF,FULL                                                          
                                                                                
PMMBUMP2 DS    0H                                                               
         CLI   0(RF),0                                                          
         BE    PMMBUMP4                                                         
         ZICM  R0,1(RF),(1)                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RF,R0                                                            
                                                                                
*&&DO                                                                           
         CLI   ELCODE,0                                                         
         BE    PMMBUMP4                                                         
         CLI   0(RF),X'03'                                                      
         BE    PMMBUMP4                                                         
         CLI   0(RF),X'CD'                                                      
         BE    PMMBUMP4                                                         
         CLI   0(RF),X'CE'                                                      
         BE    PMMBUMP4                                                         
         CLI   0(RF),X'CF'                                                      
         BE    PMMBUMP4                                                         
         CLI   0(RF),X'EF'                                                      
         BE    PMMBUMP4                                                         
         CLC   ELCODE,0(RF)                                                     
         BE    PMMBUMP4                                                         
         B     PMMBUMP2                                                         
                                                                                
*&&                                                                             
PMMBUMP4 DS    0H                                                               
         ST    RF,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
* Routine formats the location message (telling where mismatch                  
*  occurred) into WORK.                                                         
* At entry,                                                                     
*  R1     = displacement into element or record                                 
*  ELCODE = indicates element code or key of record                             
* At exit,                                                                      
*  BYTE = L(message)                                                            
*  WORK = message                                                               
                                                                                
PMMFDSP  NTR1                                                                   
         MVC   WORK,SPACES                                                      
         LA    R2,WORK                                                          
                                                                                
*                                                                               
         LA    R1,1(R1)            R1-->POSITION OF MISMATCHED BYTE             
         EDIT (R1),(3,(R2)),ALIGN=LEFT,ZERO=NOBLANK,WRK=MYWORK                  
         AR    R2,R0                                                            
         BCTR  R2,0                                                             
         MVC   1(2,R2),=C'st'                                                   
         CLI   0(R2),C'1'                                                       
         BE    PMMFDSP3                                                         
         MVC   1(2,R2),=C'nd'                                                   
         CLI   0(R2),C'2'                                                       
         BE    PMMFDSP3                                                         
         MVC   1(2,R2),=C'rd'                                                   
         CLI   0(R2),C'3'                                                       
         BE    PMMFDSP3                                                         
         MVC   1(2,R2),=C'th'                                                   
PMMFDSP3 EQU   *                                                                
         LA    R2,1+2+1(R2)                                                     
                                                                                
*                                                                               
         MVC   0(7,R2),=C'BYTE IN'                                              
         LA    R2,7+1(R2)                                                       
                                                                                
*                                                                               
         CLI   MMELCODE,0                                                       
         BNE   PMMFDSP6                                                         
         MVC   0(6,R2),=C'RECORD'                                               
         LA    R2,6(R2)                                                         
         B     PMMFDSP8                                                         
*                                                                               
PMMFDSP6 DS    0H                                                               
         MVC   0(2,R2),=C'x'''                                                  
         GOTO1 HEXOUT,DMCB,MMELCODE,2(R2),1,=C'TOG',0,0                         
         MVI   4(R2),C''''                                                      
         LA    R2,5(R2)                                                         
         B     PMMFDSP8                                                         
                                                                                
*                                                                               
PMMFDSP8 DS    0H                                                               
         LA    R0,WORK                                                          
         SR    R2,R0                                                            
         STC   R2,BYTE                                                          
                                                                                
*                                                                               
         B     XIT_01                                                           
         EJECT                                                                  
* Routine to format LHS and RHS data onto the print line and then               
*  print the line.                                                              
* OURBYTE and formatting parameters will be clobbered in the routine.           
* At entry, RHS & LHS parameters are set                                        
                                                                                
PMMPRT   NTR1                                                                   
         MVI   OURBYTE,0           TURN FLAG OFF                                
                                                                                
*                                                                               
PMMPRT10 DS    0H                  FORMAT LHS DATA                              
         L     RE,LPRTADDR                                                      
         ZICM  RF,LPRTLEN,(3)       ANYTHING FOR LHS?                           
         BZ    PMMPRT20              NO, DO RHS                                 
*                                                                               
         LA    R0,P+1               R0-->DESTINATION                            
         CLI   OURBYTE,0                                                        
         BE    *+8                                                              
         LA    R0,P+3                ADJUST DESTINATION IF NECESSARY            
*                                                                               
         ZIC   R1,LPRTINDT                                                      
         AR    R0,R1                ADJUST DESTINATION FOR INDENTING            
         LA    R1,P+1+MXPRTL                                                    
         SR    R1,R0                R1 = L(AVAILABLE PRINT SPACE)               
*                                                                               
         ICM   RF,8,SPACES         SET PADDING CHARACTER                        
         MVCL  R0,RE               MOVE STUFF ONTO PRINT LINE                   
         ST    RE,LPRTADDR         UPDATE PARAMETERS                            
         STCM  RF,3,LPRTLEN                                                     
                                                                                
*                                                                               
PMMPRT20 DS    0H                  FORMAT RHS DATA                              
         L     RE,RPRTADDR                                                      
         ZICM  RF,RPRTLEN,(3)       ANYTHING FOR RHS?                           
         BZ    PMMPRT30              NO, GO PRINT DATA ON PRINT LINE            
*                                                                               
         LA    R0,(P+(L'P/2))+1     R0-->DESTINATION                            
         CLI   OURBYTE,0                                                        
         BE    *+8                                                              
         LA    R0,(P+(L'P/2))+3      ADJUST DESTINATION IF NECESSARY            
*                                                                               
         ZIC   R1,RPRTINDT                                                      
         AR    R0,R1                ADJUST DESTINATION FOR INDENTING            
         LA    R1,((P+(L'P/2))+1)+MXPRTL                                        
         SR    R1,R0                R1 = L(AVAILABLE PRINT SPACE)               
*                                                                               
         ICM   RF,8,SPACES         SET PADDING CHARACTER                        
         MVCL  R0,RE               MOVE STUFF ONTO PRINT LINE                   
         ST    RE,RPRTADDR         UPDATE PARAMETERS                            
         STCM  RF,3,RPRTLEN                                                     
                                                                                
*                                                                               
PMMPRT30 DS    0H                  PRINT LINE                                   
         GOTO1 AGOSPOOL                                                         
         MVI   OURBYTE,1            ONE LINE PRINTED, TURN FLAG ON              
                                                                                
         OC    LPRTLEN,LPRTLEN                                                  
         BNZ   PMMPRT10                                                         
         OC    RPRTLEN,RPRTLEN                                                  
         BNZ   PMMPRT20                                                         
                                                                                
*                                                                               
         B     XIT_01                                                           
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR01--LTORG && CONS+        
               TANTS)'                                                          
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR01--MISC STUFF)'          
*--------------------- SUBR01 MISCELLANEOUS STUFF --------------------*         
                                                                                
GETEL1   DS    0H                  "GETEL1  R3,DATADISP,ELCODE"                 
         PRINT OFF                                                              
         AH    R3,DATADISP                                                      
         PRINT ON                                                               
FIRSTEL1 DS    0H                                                               
         PRINT OFF                                                              
         CLI   0(R3),0                                                          
         BNE   *+10                                                             
         CLI   0(R3),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R3)                                                     
         BCR   8,RE                                                             
         PRINT ON                                                               
NEXTEL1  DS    0H                                                               
         PRINT OFF                                                              
         SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,RF                                                            
         B     FIRSTEL1                                                         
         PRINT ON                                                               
                                                                                
                                                                                
SUBR01L  EQU   *-SUBR01                                                         
         DS    0CL(X'1000'-SUBR01L+1)                                           
***********************************************************************         
                                                                                
         DROP  R7,R8,RA,RB,RC                                                   
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR02)'                      
***********************************************************************         
*======================== SUBROUTINE POOL TWO ========================*         
                                                                                
* At entry,                                                                     
*   RA-->TWA,                                                                   
*   R8-->SYSD,                                                                  
*   R7-->SPOOLD,                                                                
*   R1-->GEND,                                                                  
*   GOSUBN = equated sub-routine number.                                        
                                                                                
SUBR02Q  EQU   (((*-RMP0F+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP0F+SUBR02Q                                                    
SUBR02   NMOD1 0,**0F02**                                                       
         LR    RC,R1                                                            
         USING GEND,RC             RC=A(GENCON WORK AREA)                       
         USING CONHEADH-64,RA      RA=A(TWA)                                    
         USING SYSD,R8             R8=A(SYSD)                                   
         USING SPOOLD,R7           R7=A(SPOOL WORK AREA)                        
                                                                                
         ZIC   R1,GOSUBN                                                        
         SH    R1,=Y(R01#)         SUBTRACT FOR SUB-RTN # 2                     
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R02_00(R1)                                                       
                                                                                
BIR#     EQU   ((R02_01-*)/4+1)+R01#  BUILD INVENTORY RECORD                    
BDB#     EQU   ((R02_02-*)/4+1)+R01#  BUILD DBLOCK                              
CDBK#    EQU   ((R02_03-*)/4+1)+R01#  CLEAR DBLOCK                              
BUE#     EQU   ((R02_04-*)/4+1)+R01#  BUILD UPGRADE ELEMENT                     
                                                                                
R02_00   DS    0H                                                               
R02_01   B     BLDINVRC            BUILD INVENTORY RECORD                       
R02_02   B     BLDBLOCK            BUILD DBLOCK                                 
R02_03   B     CLRDBLK             CLEAR DBLOCK                                 
R02_04   B     BLDUPGEL            BUILD UPGRADE ELEMENT                        
R02#     EQU   ((*-R02_00)/4+1)+R01#                                            
         DC    H'0'                                                             
                                                                                
YES_02   SR    RC,RC                                                            
NO_02    LTR   RC,RC                                                            
XIT_02   XIT1                                                                   
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR02--BIR#)'                
*----------------------- BUILD INVENTORY RECORD ----------------------*         
                                                                                
* Builds an inventory record given a set of parameters.                         
* At entry,                                                                     
*   fields prefixed w/ 'FD' are set                                             
*   INVLIST table is set                                                        
* At exit,                                                                      
*   BUFF = built inventory record                                               
                                                                                
BLDINVRC DS    0H                                                               
         XC    FNWORK(FNWORKL),FNWORK                                           
         XC    CUMSHR(CUMSHRL),CUMSHR                                           
         XC    CUMFCTR,CUMFCTR                                                  
*                                                                               
         LA    R0,BUFF                                                          
         LA    R1,2000                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR OUTPUT AREA FOR INVENTORY RECD         
                                                                                
*                                                                               
         DS    0H                  BUILD INV RECD W/ ALL INVLIST NTRIES         
         L     R2,AINVLIST                                                      
         USING INVLD,R2                                                         
         CLI   0(R2),EOT                                                        
         BE    BIRXN                                                            
*                                                                               
BIR020   DS    0H                  START OF LOOP                                
                                                                                
*                                                                               
** CONVERT INVLIST ENTRY TO IUN-FORMAT RECORD **                                
*                                                                               
*** SET UP DBLOCK ***                                                           
*                                                                               
         MVI   GOSUBN,BDB#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
         L     R5,ADBLOCK                                                       
         USING DBLOCKD,R5                                                       
                                                                                
*                                                                               
*** CALL DEMAND ***                                                             
*                                                                               
         L     R0,AIO3             IO3 USED AS INTERIM RECORD AREA              
         LA    R1,L'IO                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                CLEAR IT PRIOR TO DEMAND CALLS              
*                                                                               
         XC    TOTSHR(TOTSHRL),TOTSHR   CLEAR SHARE ACCUMULATOR AREA            
                                                                                
*                                                                               
         XC    CNTDHK1,CNTDHK1                                                  
         XC    DMCB(6*4),DMCB                                                   
         GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOK1                                      
         OC    CNTDHK1,CNTDHK1                                                  
         BZ    BIRXN                                                            
                                                                                
*                                                                               
*** UNWEIGH INTERIM RECORD & TOTAL SHARES ***                                   
*                                                                               
         DS    0H                  AIO3=A(ACCUMULATED INTERIM RECD)             
         ST    R5,MTHCFACS          SET A(DBLOCK) IN MATHBLOCK                  
         MVC   MTHIFIL,DBFILE        "  INPT-FILE  "     "                      
         MVC   MTHOFIL,DBFILE        "  OUPT-FILE  "     "                      
         MVC   MTHOSRC,FDSRC         "  SOURCE     "     "                      
         ZICM  R0,DBDIVSOR,(3)                                                  
         ST    R0,MTHFCTR            "  WEIGHT     "     "                      
                                                                                
         L     RF,DBCOMFCS                                                      
         L     RF,(CDEMOMTH-COMFACSD)(RF)                                       
         GOTO1 (RF),DMCB,=C'DIVIDE',AIO3,AIO3,MATHFAC                           
                                                                                
*                                                                               
         DS    0H                  UNWEIGH SHARES                               
         ST    R0,DIVISOR                                                       
         LA    R3,TOTSHR            SET A(ACCUMULATED SHARES)                   
         LA    R4,HOMSHR            SET A(SHARES OUTPUT AREA)                   
         BAS   RE,DIVSHR                                                        
                                                                                
*                                                                               
*** GET H/P/T LINE FOR INTERIM RECORD ***                                       
*                                                                               
         DS    0H                                                               
         L     RF,AIO3                                                          
         MVC   0(1,RF),DBFILE                                                   
         TM    ILTYP,ILTIVN                                                     
         BNO   *+16                                                             
         MVC   DBSELDAY,ILIDAY                                                  
         MVC   DBSELTIM,ILTIME                                                  
                                                                                
*                                                                               
         DS    0H                  BUILD FORCED UPGRADE ELEMENT                 
         XC    DUMUPGD,DUMUPGD                                                  
         LA    RF,DUMUPGD                                                       
         USING RAVLNEL,RF                                                       
         MVI   RAVLNCOD,X'05'       ELEMENT CODE                                
         MVI   RAVLNLEN,UPGDLNQ        "    LENGTH                              
         MVC   RAVLNBKS,DBFILE      FILE                                        
         MVI   RAVLNTYP,4           UPGRADE TYPE                                
         MVI   RAVLNOP1,XFF         INDEX FF CALL                               
         MVI   RAVLNOP2+1,1         UNWEIGHTED RECORD                           
         STCM  R5,15,RAVLNOP3       DBLOCK                                      
         MVI   RAVLNCAT,C'P'        PURE NUMBER TRANSFER                        
         TM    ILTYP,ILTIVN                                                     
         BO    *+8                                                              
         MVI   RAVLNCAT,C'D'        DAY/TIME TRANSFER                           
         DROP  RF                                                               
                                                                                
         XC    DEMODUB,DEMODUB                                                  
         MVC   DEMODUB+0(4),=C'RID='  PASS IN                                   
         MVC   DEMODUB+4(2),IPREP      2-CHAR REP CODE                          
         MVI   OURBYTE,0                                                        
         CLI   MYTAPEP,C'Y'                                                     
         BNE   *+8                                                              
         MVI   OURBYTE,C'I'                                                     
         L     RF,AIO3                                                          
         LA    R0,23(RF)           POINT R0 TO 1ST ELEMENT                      
         GOTO1 DEMUP,DMCB,(R0),(OURBYTE,DUMUPGD),ACOMFACS,DEMODUB,     +        
               HOMSHR                                                           
                                                                                
*                                                                               
** ADD INTERIM RECORD TO ACCUMULATING AREA **                                   
*                                                                               
         DS    0H                  AIO3 = A(INTERIM RECORD)                     
         MVC   MTHIFIL,=C'PAV'                                                  
         MVC   MTHOFIL,=C'INV'                                                  
         MVC   MTHOSRC,=C'NSI'                                                  
                                                                                
         TM    ILTYP,ILTWOV        CHECK FOR ANY WEIGHT OVERRIDES               
         BZ    BIR172X                                                          
         ZIC   R0,ILWT              IF THERE ARE,                               
         ST    R0,MTHFCTR            USE WEIGHT OVERRIDE IN MATHFACTOR          
BIR172X  EQU   *                                                                
                                                                                
         L     RF,DBCOMFCS                                                      
         L     RF,(CDEMOMTH-COMFACSD)(RF)                                       
         GOTO1 (RF),DMCB,=C'MAD',AIO3,BUFF,MATHFAC                              
                                                                                
         L     R0,MTHFCTR                                                       
         A     R0,CUMFCTR                                                       
         ST    R0,CUMFCTR          UPDATE CUMULATIVE FACTOR                     
         DROP  R5                                                               
                                                                                
*                                                                               
** REPEAT LOOP **                                                               
*                                                                               
         CLI   INVLQ(R2),EOT       AT END OF INV LIST TABLE?                    
         BE    BIR199               YES, EXIT LOOP                              
         LA    R2,INVLQ(R2)                                                     
         B     BIR020                                                           
         DROP  R2                                                               
                                                                                
*                                                                               
BIR199   EQU   *                                                                
         EJECT                                                                  
*                                                                               
** FINISHED ACCUMULATING INVLIST ENTRIES **                                     
*                                                                               
*** UNWEIGH ACCUMULATED RECORD & TOTAL SHARES ***                               
*                                                                               
         MVC   MTHCFACS,ADBLOCK                                                 
         MVC   MTHIFIL,DCFILINV                                                 
         MVC   MTHOFIL,DCFILINV                                                 
         MVC   MTHOSRC,=C'NSI'                                                  
         MVC   MTHFCTR,CUMFCTR                                                  
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,(CDEMOMTH-COMFACSD)(RF)                                       
         GOTO1 (RF),DMCB,=C'DIVIDE',BUFF,BUFF,MATHFAC                           
                                                                                
*                                                                               
         DS    0H                  UNWEIGH TOTAL SHARES                         
         MVC   DIVISOR,CUMFCTR                                                  
         LA    R3,CUMSHR            POINT TO ACCUMULATED SHARES                 
         LA    R4,HOMSHR            POINT TO SHARES OUTPUT AREA                 
         BAS   RE,DIVSHR                                                        
                                                                                
*                                                                               
*** INVENTORY-IZE CUMULATIVE RECORD ***                                         
*                                                                               
         DS    0H                                                               
         L     R3,AIO1             R3-->ORIGINAL INVENTORY RECORD               
         LA    R6,BUFF                                                          
         USING RINVREC,R6                                                       
         MVC   RINVKEY,0(R3)       COPY KEY OVER TO CUMULATIVE RECD             
*                                                                               
         DS    0H                  COPY ELEMENTS OVER TO CUMULATIVE REC         
         LA    R3,(RINVPEL-RINVREC)(R3)                                         
BIR212   DS    0H                                                               
         CLI   0(R3),0                                                          
         BE    BIR219                                                           
                                                                                
         L     RF,ATBELCPY         CHECK IF ELEM SHOULD BE COPIED OVER          
BIR214   DS    0H                                                               
         CLI   0(RF),EOT            IF END OF TABLE,                            
         BE    BIR218                ELEMENT IS NOT TO BE COPIED                
         CLC   0(1,R3),0(RF)                                                    
         BE    BIR216                                                           
         LA    RF,L'TABELCPY(RF)                                                
         B     BIR214                                                           
                                                                                
BIR216   DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R6),(R3),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
                                                                                
BIR218   DS    0H                  BUMP TO NEXT ELEMENT IN ORIG TRACK           
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     BIR212                                                           
*                                                                               
BIR219   EQU   *                                                                
                                                                                
*                                                                               
*** GET H/P/T LINE FOR CUMULATIVE RECORD ***                                    
*                                                                               
         DS    0H                  BUILD FORCED UPGRADE ELEMENT                 
         MVI   GOSUBN,BUE#                                                      
         GOTO1 AGOSUB               DUMUPGD HAS UPGRADE ELEMENT                 
                                                                                
*                                                                               
         LA    R6,BUFF                                                          
         USING RINVREC,R6                                                       
         LA    R0,RINVPEL                                                       
         MVI   OURBYTE,0                                                        
         CLI   MYTAPEP,C'Y'                                                     
         BNE   *+8                                                              
         MVI   OURBYTE,C'I'                                                     
         XC    DEMODUB,DEMODUB                                                  
         MVC   DEMODUB+0(4),=C'RID='    PASS IN                                 
         MVC   DEMODUB+4(2),RINVKREP     2-CHAR REP CODE                        
         MVC   BYTE,RINVREC+DMKIVRQ     REMEMBER DATA @ RINVREC+DMKIVRQ         
         MVI   RINVREC+DMKIVRQ,0        PREVENT UT/TP PROBLEM                   
         GOTO1 DEMUP,DMCB,(R0),(OURBYTE,DUMUPGD),ACOMFACS,DEMODUB,     +        
               HOMSHR                                                           
         MVC   RINVREC+DMKIVRQ(1),BYTE  RESTORE  DATA @ RINVREC+DMKIVRQ         
         DROP  R6                                                               
                                                                                
*                                                                               
         DS    0H                  CORRECT RECD LENGTH                          
         LA    RE,BUFF                                                          
         LA    RF,(RINVPEL-RINVREC)(RE)                                         
         ZICM  R1,(RINVLEN-RINVREC)(RE),(3)                                     
         AR    R1,RE                                                            
         MVI   0(R1),0                                                          
*                                                                               
BIR242   DS    0H                   LOCATE LAST BYTE OF RECORD                  
         CLI   0(RF),0                                                          
         BE    BIR245                                                           
         ZICM  R0,1(RF),(1)          IF NOT FOUND,                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RF,R0                  BUMP TO NEXT ELEMENT                      
         B     BIR242                                                           
*                                                                               
BIR245   DS    0H                   RF-->LAST BYTE OF RECORD                    
         LA    RF,1(RF)              BUMP PAST LAST BYTE                        
         SR    RF,RE                 RF = L(RECORD)                             
         STCM  RF,3,(RINVLEN-RINVREC)(RE)                                       
                                                                                
*                                                                               
         DS    0H                  PUT UPGRADE ELEMENT INTO TRACK               
         OC    TRKUPGEL,TRKUPGEL                                                
         BZ    BIR247X                                                          
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),BUFF,TRKUPGEL,0                    
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
BIR247X  EQU   *                                                                
                                                                                
*                                                                               
*** COPY DEMO OVERRIDE ELEMENTS OVER ***                                        
*                                                                               
         L     R3,AIO1                                                          
         LA    R6,BUFF                                                          
         MVI   ELCODE,X'DE'                                                     
                                                                                
         BAS   RE,GETEL2                                                        
         B     BIR254B                                                          
BIR254A  BAS   RE,NEXTEL2                                                       
BIR254B  BNE   BIR259                                                           
                                                                                
         DS    0H                  SEE IF CREATED TRACK HAS IT YET              
         ZIC   R0,1(R3)                                                         
         SH    R0,=H'2'             L(SEARCH ARGUMENT)                          
         GOTO1 HELLO,DMCB,(C'G',=C'REPFILE'),(X'DE',(R6)),             +        
               ((R0),2(R3)),0                                                   
         CLI   DMCB+12,0            YES, IT'S THERE ALREADY                     
         BE    BIR254A                                                          
         CLI   DMCB+12,6            NOPE, ADD IT TO TRACK                       
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R6),(R3),0                        
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         B     BIR254A                                                          
BIR259   EQU   *                                                                
                                                                                
*                                                                               
*** PUT PROGRAM-TITLE FOOTNOTE ELEM INTO TRACK ***                              
*                                                                               
         MVI   GOSUBN,PFN#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
         B     BIRXY                                                            
                                                                                
*                                                                               
BIRXN    DS    0H                                                               
         B     NO_02                                                            
*                                                                               
BIRXY    DS    0H                                                               
         B     YES_02                                                           
         EJECT                                                                  
*                                                                               
** DEMAND HOOK #1 **                                                            
*                                                                               
* At entry,                                                                     
*   R2-->current INVLIST entry                                                  
*   R5-->DBLOCK                                                                 
                                                                                
DEMHOOK1 DS    0H                                                               
DHK1     NTR1                                                                   
         DS    0XL(1-(DHK1-DEMHOOK1))                                           
                                                                                
*                                                                               
         USING INVLD,R2                                                         
         USING DBLOCKD,R5                                                       
                                                                                
*                                                                               
         MVC   SVDBVALS(8),DBAREC  SAVE DBAREC & DBAQUART                       
*                                                                               
         LA    R0,1                                                             
         A     R0,CNTDHK1                                                       
         ST    R0,CNTDHK1          NUMBER OF TIMES ENTERRING HOOK               
         EJECT                                                                  
                                                                                
*                                                                               
*** PROGRAM-TITLE FOOTNOTE PROCESSING ***                                       
*                                                                               
         DS    0H                                                               
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         GOTO1 DEFINE,MYDMCB,=C'PROGRA',DBLOCK,WORK                             
         MVC   FNWPROG2,WORK                                                    
         OC    FNWPROG1,FNWPROG1                                                
         BNZ   *+10                                                             
         MVC   FNWPROG1,WORK                                                    
         MVC   FNWB1BK,DBACTBK                                                  
                                                                                
*                                                                               
*** SET MATHFAC BLOCK ***                                                       
*                                                                               
         DS    0H                                                               
         XC    MATHFAC,MATHFAC                                                  
         ST    R5,MTHCFACS                                                      
         MVC   MTHFCTR+2(2),DBFACTOR                                            
         MVC   MTHIFIL,DBFILE       KEEP FILE FORMAT OF INPUT RECORD            
         MVC   MTHOFIL,DBFILE                                                   
         MVC   MTHOSRC(1),DBSELSRC                                              
                                                                                
*                                                                               
*** GET SHARES FROM INPUT RECD AND WEIGH ***                                    
*                                                                               
         DS    0H                                                               
         LA    R3,TOTSHR            POINT R3 TO OUTPUT ACCUM AREA               
         BAS   RE,GETSHR                                                        
         LA    R3,CUMSHR            UPDATE CUMULATIVE SHARES ALSO               
         BAS   RE,GETSHR                                                        
                                                                                
*                                                                               
*** IUN-IZE INPUT RECORD ***                                                    
*                                                                               
         DS    0H                                                               
         CLI   TRKNHELS,0          IF TRACK CHANGED BY ROVER,                   
         BH    DHK1150              IUN-IZE RECORD FROM DEMAND                  
         TM    ILTYP,ILTIVN        WAS INPUT A PURE NUMBER?                     
         BO    DHK1200              YES, DON'T IUN-IZE RECORD                   
                                                                                
*                                                                               
DHK1150  DS    0H                                                               
         L     R3,AIUNWRK                                                       
         LR    R0,R3                                                            
         LA    R1,IUNWRKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR IUN WORK AREA                          
                                                                                
*                                                                               
         MVC   0(1,R3),DBFILE                                                   
         MVC   20(2,R3),=H'24'     DUMMY RECD LENGTH                            
                                                                                
*                                                                               
         DS    0H                                                               
         LA    R3,1000(R3)                                                      
         USING IUNRECD,R3                                                       
*                                                                               
         GOTO1 REGETIUN,MYDMCB,(9,DBLOCKD),IUNRECD                              
         LA    R0,IUNNEW                                                        
         LA    R1,IUNNEWX-IUNNEW                                                
         LA    RE,IUNOLD                                                        
         LA    RF,IUNOLDX-IUNOLD                                                
         MVCL  R0,RE               COPY OLD VALUES TO NEW                       
*                                                                               
         GOTO1 DEMOUT,MYDMCB,(C'L',ASHARES),DBLOCKD,ISHOMES,0                   
                                                                                
*                                                                               
         DS    0H                  USE DEMAINT TO PUT INTO RECD FORMAT          
         L     RE,AIUNWRK                                                       
         ST    RE,DBAREC                                                        
         LA    RE,23(RE)                                                        
         ST    RE,DBAQUART                                                      
         LA    RE,IUNRECL/4                                                     
         STCM  RE,3,DBNUMVLS                                                    
         MVC   OFORMAT+0(7),=C'IUNUIUN'                                         
         MVC   OFORMAT+7(2),=X'530B'                                            
         CLI   DBTAPEP,C'Y'                                                     
         BNE   *+10                                                             
         MVC   OFORMAT+7(2),=X'5A0B'                                            
         MVI   OFORMAT+9,0                                                      
                                                                                
         L     RF,DBCOMFCS                                                      
         L     RF,(CDEMAINT-COMFACSD)(RF)                                       
         GOTO1 (RF),MYDMCB,=C'PUT',DBLOCKD,IUNRECD,OFORMAT                      
         DROP  R3                                                               
                                                                                
*                                                                               
*** ACCUMULATE TO INTERIM RECORD ***                                            
*                                                                               
DHK1200  DS    0H                                                               
         MVC   MTHOSRC,=C'NSI'                                                  
         L     RF,DBCOMFCS                                                      
         L     RF,(CDEMOMTH-COMFACSD)(RF)                                       
         GOTO1 (RF),MYDMCB,=C'MAD',DBAREC,AIO3,MATHFAC                          
                                                                                
*                                                                               
         DS    0H                  RESTORE DBLOCK VALUES                        
         MVC   DBAREC(8),SVDBVALS   DBAREC & DBAQUART                           
                                                                                
*                                                                               
         DS    0H                  SET KEY & QHR ELEM INTO INTERIM REC          
         L     RE,AIO3                                                          
         L     RF,DBAREC                                                        
         MVC   0(L'PRKMAJOR+L'PRKMINOR,RE),0(RF)                                
                                                                                
         GOTO1 HELLO,MYDMCB,(C'D',DBFILNAM),(X'20',AIO3),0,0                    
         CLI   MYDMCB+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R3,DBAQUART                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,FIRSTEL2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 HELLO,MYDMCB,(C'P',DBFILNAM),AIO3,(R3),0                         
         CLI   MYDMCB+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
*** EXIT FOR DEMHOOK1 ***                                                       
*                                                                               
         DS    0H                                                               
         B     XIT_02                                                           
         DROP  R2,R5                                                            
         EJECT                                                                  
                                                                                
* Routine to get shares and update shares accumulators.                         
* At entry,                                                                     
*   R3-->output area                                                            
*   R5-->DBLOCK.                                                                
                                                                                
         DS    0H                                                               
GETSHR   NTR1                                                                   
         USING DBLOCKD,R5                                                       
         MVC   HALF,DBACTBK        SAVE ACTUAL BOOK                             
         XC    HOMSHR(HOMSHRL),HOMSHR                                           
         GOTO1 DEMOUT,MYDMCB,(C'P',ADEMOSHR),DBLOCKD,HOMSHR                     
         MVC   DBACTBK,HALF        RESTORE ACTUAL BOOK VALUE                    
         DROP  R5                                                               
*                                                                               
         DS    0H                  ACCUMULATE THE SHARES                        
         LA    R0,3                 R0 = COUNTER                                
         SR    R1,R1                R1 = INDEX INTO VALUES                      
                                                                                
GETSHR10 DS    0H                                                               
         L     RE,HOMSHR(R1)                                                    
         MH    RE,MTHFCTR+2                                                     
         A     RE,0(R1,R3)                                                      
         ST    RE,0(R1,R3)         UPDATE SHARES                                
         LA    R1,4(R1)                                                         
         BCT   R0,GETSHR10                                                      
*                                                                               
         B     XIT_02                                                           
         EJECT                                                                  
                                                                                
* Routine to divide accumulated shares by accumulated weighting factor.         
* At entry,                                                                     
*   R3      = A(accumulated shares),                                            
*   R4      = A(output area),                                                   
*   DIVISOR = accumulated factor.                                               
                                                                                
         DS    0H                                                               
DIVSHR   NTR1                                                                   
         LA    R0,3                R0 = COUNTER                                 
         MVI   GOSUBN,DIV#         DIVIDE ROUTINE                               
                                                                                
DIVSHR10 DS    0H                                                               
         SR    RE,RE                                                            
         L     RF,0(R3)                                                         
         STM   RE,RF,DIVIDEND      SET TOTAL WEIGHTED SHARE IN DIVIDEND         
         GOTO1 AGOSUB                                                           
         MVC   0(4,R4),QUOTIENT    QUOTIENT HAS UNWEIGHTED SHARE                
                                                                                
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,DIVSHR10                                                      
*                                                                               
         B     XIT_02                                                           
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR02--BDB#)'                
*---------------------------- BUILD DBLOCK ---------------------------*         
                                                                                
* Builds the demo block (DBLOCK) and extended DBLOCK areas for the              
*  DEMAND calls made in this program.  AIO2 is used for DEMAND i/o,             
*  and the media is always TV (for now).                                        
* At entry,                                                                     
*   fields with prefix 'FD' are set                                             
*   R2-->INVLIST entry                                                          
* At exit,                                                                      
*   FULL = A(next INVLIST entry)                                                
                                                                                
BLDBLOCK DS    0H                                                               
         USING INVLD,R2                                                         
*                                                                               
         L     R5,ADBLOCK                                                       
         USING DBLOCKD,R5                                                       
                                                                                
*                                                                               
** CLEAR DBLOCK AND EXTENSION AREAS **                                          
*                                                                               
         MVI   GOSUBN,CDBK#                                                     
         GOTO1 AGOSUB                                                           
                                                                                
         L     R0,ADBXTND                                                       
         LA    R1,DBXTND1X-DBXTND1                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
*                                                                               
** SET UP DBLOCK **                                                             
*                                                                               
*** NON FILE-SPECIFIC INFORMATION ***                                           
*                                                                               
         MVC   DBFILE,ILDFILE                                                   
         MVC   DBAREC,AIO2                                                      
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBBTYPE,ILBTYP                                                   
         MVC   DBTAPEP,MYTAPEP                                                  
         MVC   DBSELSRC,ILSRC                                                   
         MVC   DBSELBK,ILBOOK                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELSTA,ILSTTN                                                  
         MVC   DBSELAGY,IPREP                                                   
         EJECT                                                                  
*                                                                               
*** FILE-SPECIFIC INFORMATION ***                                               
*                                                                               
         DS    0H                  FILE SPECIFIC PARAMETERS                     
         CLC   DBFILE,DCFILPAV                                                  
         BE    BDBP                                                             
         CLC   DBFILE,DCFILTP                                                   
         BE    BDBT                                                             
         DC    H'0'                                                             
                                                                                
                                                                                
                                                                                
BDBP     DS    0H                  PROGRAM AVG SPECIFIC PARAMETERS              
         TM    ILTYP,ILTIVN         IS THIS GOING TO BE A PURE NUMBER?          
         BNO   *+14                                                             
         MVI   DBFUNCT,DBGETPUR      YEP,                                       
         MVC   DBSELPUR,ILPURE        MOVE IN PURE NUMBER                       
         TM    ILTYP,ILTIVN         IS THIS GOING TO BE A PURE NUMBER?          
         BO    *+16                                                             
         MVC   DBSELDAY,ILIDAY       NOPE, MOVE IN DAY                          
         MVC   DBSELTIM,ILTIME        AND TIMES                                 
                                                                                
*                                                                               
         MVC   DBBEST,ILBEST        GET BEST RECORD                             
                                                                                
*                                                                               
         B     BDB100                                                           
                                                                                
                                                                                
                                                                                
BDBT     DS    0H                  TIME PERIOD SPECIFIC PARAMETERS              
         MVC   DBSELDAY,ILIDAY      DAY(S)                                      
         MVC   DBSELTIM,ILTIME      TIME(S)                                     
         MVC   DBTPTT,FDTPTT        4 WK/TYPICAL TIME                           
                                                                                
*                                                                               
         B     BDB100                                                           
                                                                                
                                                                                
                                                                                
BDB100   DS    0H                                                               
         B     XIT_02                                                           
         DROP  R2,R5                                                            
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR02--CDBK#)'               
*------------------------- CLEAR DBLOCK AREA -------------------------*         
                                                                                
* Clears the area pointed to by ADBLOCK.                                        
                                                                                
CLRDBLK  DS    0H                                                               
         L     R0,ADBLOCK                                                       
         LA    R1,DBLOCK1X-DBLOCK1                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         B     XIT_02                                                           
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR02--BUE#)'                
*----------------------- BUILD UPGRADE ELEMENT -----------------------*         
                                                                                
BLDUPGEL DS    0H                                                               
         XC    DUMUPGD,DUMUPGD     BUILD UPGRADE ELEMENT HERE                   
                                                                                
*                                                                               
** USE UPGRADE ELEMENT IN TRACK **                                              
*                                                                               
         OC    TRKUPGEL,TRKUPGEL   IF TRACK HAD ONE ALREADY,                    
         BZ    BUE019                                                           
         CLI   TRKUPGEL+(RAVLNTYP-RAVLNEL),7  AND IT'S NOT "MIN VALUE"          
         BE    BUE019                                                           
         MVC   DUMUPGD,TRKUPGEL               USE IT                            
         B     BUEX                            AND EXIT NOW                     
BUE019   EQU   *                                                                
                                                                                
*                                                                               
** RE-CONSTRUCT UPGRADE ELEMENT **                                              
*                                                                               
         LA    RF,DUMUPGD                                                       
         USING RAVLNEL,RF                                                       
         MVI   RAVLNCOD,X'05'       ELEMENT CODE                                
         MVI   RAVLNLEN,UPGDLNQ        "    LENGTH                              
         MVI   RAVLNTYP,4           UPGRADE TYPE                                
                                                                                
*                                                                               
         CLI   FDFRPR,C'A'          CHECK IF ONLINE TRANSFER                    
         BE    BUE_OL                                                           
         DC    H'0'                                                             
*                                                                               
BUE_OL   DS    0H                   TRACK TRANSFERRED ONLINE                    
         OI    RAVLNBKS,X80                                                     
         MVC   RAVLNOP1,=H'100'      INDEX 100 CALL                             
         B     BUE049                                                           
*                                                                               
BUE049   EQU   *                                                                
         DROP  RF                                                               
                                                                                
*                                                                               
BUEX     DS    0H                                                               
         B     XIT_02                                                           
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR02--MISC STUFF)'          
*--------------------- SUBR02 MISCELLANEOUS STUFF --------------------*         
                                                                                
GETEL2   DS    0H                  "GETEL2  R3,DATADISP,ELCODE"                 
         PRINT OFF                                                              
         AH    R3,DATADISP                                                      
         PRINT ON                                                               
FIRSTEL2 DS    0H                                                               
         PRINT OFF                                                              
         CLI   0(R3),0                                                          
         BNE   *+10                                                             
         CLI   0(R3),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R3)                                                     
         BCR   8,RE                                                             
         PRINT ON                                                               
NEXTEL2  DS    0H                                                               
         PRINT OFF                                                              
         SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,RF                                                            
         B     FIRSTEL2                                                         
         PRINT ON                                                               
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR02--LTORG && CONS+        
               TANTS)'                                                          
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
SUBR02L  EQU   *-SUBR02                                                         
         DS    0CL(X'1000'-SUBR02L+1)                                           
***********************************************************************         
                                                                                
         DROP  R7,R8,RA,RB,RC                                                   
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR03)'                      
***********************************************************************         
*======================== SUBROUTINE POOL TWO ========================*         
                                                                                
* At entry,                                                                     
*   RA-->TWA,                                                                   
*   R8-->SYSD,                                                                  
*   R7-->SPOOLD,                                                                
*   R1-->GEND,                                                                  
*   GOSUBN = equated sub-routine number.                                        
                                                                                
SUBR03Q  EQU   (((*-RMP0F+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP0F+SUBR03Q                                                    
SUBR03   NMOD1 0,**0F03**                                                       
         LR    RC,R1                                                            
         USING GEND,RC             RC=A(GENCON WORK AREA)                       
         USING CONHEADH-64,RA      RA=A(TWA)                                    
         USING SYSD,R8             R8=A(SYSD)                                   
         USING SPOOLD,R7           R7=A(SPOOL WORK AREA)                        
                                                                                
         ZIC   R1,GOSUBN                                                        
         SH    R1,=Y(R02#)         SUBTRACT FOR SUB-RTN # 2                     
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R03_00(R1)                                                       
                                                                                
COR#     EQU   ((R03_01-*)/4+1)+R02#  COPY ORIGINAL RECORD                      
PBR#     EQU   ((R03_02-*)/4+1)+R02#  PRINT "BAD" RECORD                        
GHI#     EQU   ((R03_03-*)/4+1)+R02#  GET INV HEADER INFO                       
FIK#     EQU   ((R03_04-*)/4+1)+R02#  FORMAT INV KEY                            
PFN#     EQU   ((R03_05-*)/4+1)+R02#  PUT FOOTNOTE ELEMENT                      
                                                                                
R03_00   DS    0H                                                               
R03_01   B     CPYOREC                COPY ORIGINAL RECORD                      
R03_02   B     PRTBADRC               PRINT "BAD" RECORD                        
R03_03   B     GTHDRINF               GET INV HEADER INFO                       
R03_04   B     FMTINVKY               FORMAT INV KEY                            
R03_05   B     PUTFTNT                PUT FOOTNOTE ELEMENT                      
R03#     EQU   ((*-R03_00)/4+1)+R02#                                            
         DC    H'0'                                                             
                                                                                
YES_03   SR    RC,RC                                                            
NO_03    LTR   RC,RC                                                            
XIT_03   XIT1                                                                   
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR03--COR#)'                
*------------------------ COPY ORIGINAL RECORD -----------------------*         
                                                                                
* Copy the original record in I/O1 to I/O2 with the elements in                 
*  sorted order.                                                                
                                                                                
CPYOREC  DS    0H                                                               
         L     R4,AIO2                                                          
         LR    R0,R4                                                            
         LA    R1,L'IO                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR OUTPUT AREA FOR INVENTORY RECD         
                                                                                
*                                                                               
         DS    0H                                                               
         L     R3,AIO1                                                          
         MVC   0(L'RINVKEY,R4),0(R3)                                            
         MVC   (RINVLEN-RINVREC)(2,R4),=H'35'                                   
                                                                                
*                                                                               
         DS    0H                  COPY ELEMENTS TO I/O2                        
         LA    R3,(RINVPEL-RINVREC)(R3)                                         
*                                                                               
COR022   DS    0H                   START OF LOOP                               
         CLI   0(R3),0               AT END OF RECORD?                          
         BE    COR030                 YEP, DONE COPYING                         
                                                                                
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R4),(R3),0                             
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                 BUMP TO NEXT ELEMENT                       
         B     COR022                                                           
                                                                                
*                                                                               
COR030   DS    0H                                                               
         L     R3,AIO1                                                          
         CLC   (RINVLEN-RINVREC)(2,R3),(RINVLEN-RINVREC)(R4)                    
         BE    COR039                                                           
*                                                                               
         ZICM  R1,(RINVLEN-RINVREC)(R3),(3)                                     
         LA    R1,1(R1)                                                         
         CLM   R1,3,(RINVLEN-RINVREC)(R4)                                       
         BE    COR039                                                           
*                                                                               
         DC    H'0'                                                             
*                                                                               
COR039   EQU   *                                                                
                                                                                
*                                                                               
         B     XIT_03                                                           
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR03--PBR#)'                
*------------------------- PRINT "BAD" RECORD ------------------------*         
                                                                                
* Reports the reason for and the key of the bad record.                         
* At entry,                                                                     
*   BADRCNUM = reason number for BADRECTB                                       
*   AIO      = A("bad" record)                                                  
*   KEY      = key of "bad" record                                              
                                                                                
PRTBADRC DS    0H                                                               
         MVI   P1,0                                                             
         LA    R2,P2                                                            
                                                                                
*                                                                               
         DS    0H                                                               
         ZIC   R1,BADRCNUM                                                      
         MH    R1,=Y(L'BADRECTB)                                                
         A     R1,ABADRCTB                                                      
                                                                                
*                                                                               
         ZICM  RE,30(R1),(3)                                                    
         LA    RE,SYSD(RE)         RE-->COUNTER                                 
         LA    R0,1                                                             
         A     R0,0(RE)                                                         
         ST    R0,0(RE)                                                         
                                                                                
*                                                                               
         MVC   0(30,R2),0(R1)                                                   
         MVI   30(R2),C':'                                                      
         LA    R2,30+1+1(R2)                                                    
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   FULL,AIO                                                         
         MVI   GOSUBN,FIK#         FORMAT TRACK KEY INTO WORK                   
         GOTO1 AGOSUB                                                           
         ZIC   R1,BYTE                                                          
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),WORK                                                    
                                                                                
*                                                                               
*^^GYL                                                                          
         CLI   BADRCNUM,(BRTTRKNR-BADRECTB)/(L'BADRECTB)                        
         BNE   *+10                                                             
         MVC   P2+20(1),TRKKQLFY                                                
*^^EOGYL                                                                        
         DS    0H                                                               
         GOTO1 AGOSPOOL                                                         
                                                                                
*                                                                               
         DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR03--GHI#)'                
*--------------------- GET INVENTORY HEADER INFO ---------------------*         
                                                                                
* Gets pertinent info from inventory header record.                             
* At entry,                                                                     
*   KEY      = key of inventory header                                          
                                                                                
GTHDRINF DS    0H                                                               
         XC    HDRINFO(HDRINFOL),HDRINFO  CLEAR HEADER INFORMATION              
*                                                                               
         L     R0,AINVHDR                                                       
         LA    R1,INVHDRX-INVHDR                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                      CLEAR AREA FOR HEADER RECD            
                                                                                
*                                                                               
         MVC   HDRKEY,KEY          REMEMBER KEY OF CURRENT INV HDR              
*                                                                               
         GOTO1 GETREC              GET INVENTORY HEADER RECORD                  
                                                                                
*                                                                               
         DS    0H                  GET DAYS & TIME                              
         L     R3,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         MVC   DATADISP,DSP1EL                                                  
         BAS   RE,GETEL3                                                        
         BNE   GHI019                                                           
                                                                                
         USING RIDTELEM,R3                                                      
         MVC   HDRIDAY,RIDTDAY      DAY                                         
         MVC   HDRTIME,RIDTTIME     TIME                                        
         DROP  R3                                                               
GHI019   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  HOLD ONTO HEADER RECORD                      
         L     RE,AIO                        A(SOURCE)                          
         ZICM  RF,(RINVLEN-RINVREC)(RE),(3)  L(SOURCE)                          
         L     R0,AINVHDR                    A(DESTINATION)                     
         LR    R1,RF                         L(TO COPY)                         
         MVCL  R0,RE                                                            
                                                                                
*                                                                               
         DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR03--FIK#)'                
*----------------------- FORMAT INVENTORY KEY ------------------------*         
                                                                                
* Formats key of an inventory track record.                                     
* At entry,                                                                     
*   FULL     = A(inventory track record)                                        
* At exit,                                                                      
*   WORK     = formatted key                                                    
*   BYTE     = L(formatted data)                                                
                                                                                
FMTINVKY DS    0H                                                               
         L     R6,FULL                                                          
         USING RINVREC,R6                                                       
                                                                                
*                                                                               
         LA    R2,WORK                                                          
         XC    WORK,WORK                                                        
         MVI   BYTE,0                                                           
                                                                                
*                                                                               
** START FORMATTING **                                                          
*                                                                               
         DS    0H                  STATION                                      
         MVC   0(L'RINVKSTA,R2),RINVKSTA                                        
         LA    R2,L'RINVKSTA(R2)                                                
                                                                                
*                                                                               
         DS    0H                  INVENTORY NUMBER                             
         BAS   RE,FIKICMMA                                                      
         MVC   0(L'RINVKINV,R2),RINVKINV                                        
         LA    R2,L'RINVKINV-1(R2)                                              
         CLI   0(R2),0                                                          
         BE    *+8                                                              
         LA    R2,1(R2)                                                         
                                                                                
*                                                                               
         DS    0H                  EFFECTIVE DATE                               
         BAS   RE,FIKICMMA                                                      
         GOTO1 DATCON,DMCB,(X'83',RINVKSTD),(5,MYWORK),0                        
         ZICM  R1,DMCB+4,(1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),MYWORK                                                  
         LA    R2,1(R1,R2)                                                      
                                                                                
*                                                                               
         DS    0H                  RATING SERVICE                               
         BAS   RE,FIKICMMA                                                      
         MVC   0(L'FDSRC,R2),FDSRC                                              
         LA    R2,L'FDSRC(R2)                                                   
                                                                                
*                                                                               
         DS    0H                  BOOK                                         
         BAS   RE,FIKICMMA                                                      
                                                                                
         CLI   TRKKQLFY,C' '                                                    
         BE    *+14                                                             
         MVC   0(1,R2),TRKKQLFY                                                 
         LA    R2,1(R2)                                                         
                                                                                
         XC    DUB,DUB                                                          
         MVC   DUB(L'RINVKBK),RINVKBK                                           
         GOTO1 DATCON,DMCB,(X'83',DUB),(6,MYWORK),0                             
         ZIC   R1,DMCB+4                                                        
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),MYWORK                                                  
         LA    R2,1(R2,R1)                                                      
                                                                                
         CLI   TRKKBTYP,0                                                       
         BE    *+22                                                             
         MVI   0(R2),C'('                                                       
         MVC   1(1,R2),TRKKBTYP                                                 
         MVI   2(R2),C')'                                                       
         LA    R2,3(R2)                                                         
                                                                                
*                                                                               
         PRINT OFF                                                              
*&&DO                                                                           
         DS    0H                  DAYS/TIMES                                   
         LR    R3,R6                                                            
         MVI   ELCODE,X'CE'                                                     
         MVC   DATADISP,DSP1EL                                                  
         BAS   RE,GETEL3                                                        
         BNE   FIK049                                                           
         USING RINVZEL,R3                                                       
         MVC   TMPIDAY,RINVZDAY                                                 
         MVC   TMPSETM,RINVZTIM                                                 
         DROP  R3                                                               
*                                                                               
         MVC   0(2,R2),=C'--'                                                   
         LA    R2,2(R2)                                                         
         GOTO1 UNDAY,DMCB,TMPIDAY,(X'07',MYWORK)                                
         MVC   0(7,R2),MYWORK                                                   
         MVI   7(R2),C'/'                                                       
         LA    R2,7+1(R2)                                                       
*                                                                               
         GOTO1 UNTIME,DMCB,(0,TMPSETM),(R2)                                     
                                                                                
         CLI   0(R2),C' '          BUMP R2 TO FIRST BLANK                       
         BNH   *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
FIK049   EQU   *                                                                
                                                                                
*                                                                               
*&&                                                                             
         PRINT ON                                                               
         DROP  R6                                                               
                                                                                
*                                                                               
** DONE FORMATTING **                                                           
*                                                                               
         DS    0H                                                               
         LA    R0,WORK                                                          
         SR    R2,R0                                                            
         STC   R2,BYTE                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         B     XIT_03                                                           
                                                                                
                                                                                
* Little routine to insert comma.                                               
                                                                                
FIKICMMA DS    0H                                                               
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         BR    RE                                                               
                                                                                
                                                                                
* Little routine to print out military time.                                    
                                                                                
FIKEDTIM NTR1                                                                   
         EDIT  (R1),(4,(R2)),WRK=MYWORK,ZERO=NOBLANK,FILL=0                     
         B     XIT_03                                                           
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR03--PFN#)'                
*--------- PUT FOOTNOTE ELEMENT INTO CREATED INVENTORY RECORD --------*         
                                                                                
* At entry,                                                                     
*   BUFF contains created track.                                                
                                                                                
PUTFTNT  DS    0H                                                               
         LA    R2,ELEM                                                          
         USING RPGMELM,R2                                                       
         XC    RPGMELM(RPGMELML),RPGMELM                                        
         MVI   RPGMELM,X'01'                                                    
         MVI   RPGMELLN,RPGMELML                                                
         MVI   RPGMLIN,1                                                        
                                                                                
         MVC   DUB(2),FNWB1BK                                                   
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(3,DUB),(6,WORK)                                     
         MVC   RPGMMON,WORK                                                     
         MVC   RPGMYR,WORK+4                                                    
                                                                                
         MVC   RPGMNAME,FNWPROG1                                                
         CLC   FNWPROG1,FNWPROG2                                                
         BE    PFN039                                                           
         MVI   RPGMNAME+7,C'/'                                                  
         MVC   RPGMNAME+8(7),FNWPROG2                                           
         MVI   RPGMNAME+15,C' '                                                 
PFN039   EQU   *                                                                
                                                                                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),BUFF,RPGMELM,0                     
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
                                                                                
*                                                                               
PFNX     DS    0H                                                               
         B     XIT_03                                                           
***********************************************************************         
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR03--MISC STUFF)'          
*--------------------- SUBR02 MISCELLANEOUS STUFF --------------------*         
                                                                                
GETEL3   DS    0H                  "GETEL3  R3,DATADISP,ELCODE"                 
         PRINT OFF                                                              
         AH    R3,DATADISP                                                      
         PRINT ON                                                               
FIRSTEL3 DS    0H                                                               
         PRINT OFF                                                              
         CLI   0(R3),0                                                          
         BNE   *+10                                                             
         CLI   0(R3),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R3)                                                     
         BCR   8,RE                                                             
         PRINT ON                                                               
NEXTEL3  DS    0H                                                               
         PRINT OFF                                                              
         SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,RF                                                            
         B     FIRSTEL3                                                         
         PRINT ON                                                               
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBR03--LTORG && CONS+        
               TANTS)'                                                          
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
SUBR03L  EQU   *-SUBR03                                                         
         DS    0CL(X'1000'-SUBR03L+1)                                           
***********************************************************************         
                                                                                
         DROP  R7,R8,RA,RB,RC                                                   
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBRXM)'                      
***********************************************************************         
*==================== EXIT WITH MESSAGE ROUTINES =====================*         
                                                                                
* Exits which leave RMP0F entirely and displays a message go through            
*  this routine.                                                                
* At entry,                                                                     
*   MYTEXT has length and text of text-replace.                                 
                                                                                
SUBRXMQ  EQU   (((*-RMP0F+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   RMP0F+SUBRXMQ                                                    
XMSGRTN  NMOD1 0,**18XM**                                                       
         SR    RC,RC                                                            
         ICM   RC,7,1(R1)                                                       
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASYSD                                                         
         USING SYSD,R8                                                          
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
                                                                                
         MVC   MSGTYPE,0(R1)       GET MESSAGE TYPE                             
         XC    CONHEAD,CONHEAD     CLEAR THE WAY FOR THE MESSAGE                
                                                                                
         CLI   MSGTYPE,C'E'        EXIT W/ AN ERROR MSG                         
         BE    XMERR                                                            
         CLI   MSGTYPE,C'W'        EXIT W/ A WARNING MSG                        
         BE    XMWRN                                                            
         CLI   MSGTYPE,C'I'        EXIT W/ AN INFO  MSG                         
         BE    XMINF                                                            
         DC    H'0'                                                             
                                                                                
                                                                                
ALLMSGX  DS    0H                                                               
         DS    0H                                                               
         GOTO1 AERREX                                                           
         B     XIT_XM                                                           
                                                                                
                                                                                
XIT_XM   XIT1                                                                   
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBRXM--ERR MSGS)'            
*--------------------------- ERROR MESSAGES --------------------------*         
                                                                                
* These messages require the user to re-input (the correct) data for a          
*  response.  Previous values are restored, if the key did not change,          
*  and fields modified this time around will appear modified in the             
*  next transaction.                                                            
* At entry, R2-->field to put cursor.                                           
                                                                                
XMERR    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         OI    MISCFLG1,MF1ERRQ                                                 
                                                                                
         DS    0H                                                               
         MVC   AERREX,ERREX        SET ADDRESS OF ERREX ROUTINE                 
         CLI   OURERRCD,ERRX#                                                   
         BNH   XMERRGO                                                          
         MVC   AERREX,ERREX2        TO GO OFF TO                                
         MVC   CONHEAD(9),=C'**ERROR**'                                         
         LA    R1,CONHEAD+10                                                    
         CLI   OURERRCD,ERRX2#                                                  
         BNH   XMERRGO                                                          
         DC    H'0'                                                             
                                                                                
XMERRGO  DS    0H                                                               
         CLI   OURERRCD,0                                                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   OURERRCD,XMERRQ                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZIC   RF,OURERRCD         BRANCH OFF TO SET ERROR MESSAGE              
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     XMERR00(RF)                                                      
                                                                                
MFLDQ    EQU   ((XMERR01-XMERR00)/4)+1                                          
IFLDQ    EQU   ((XMERR02-XMERR00)/4)+1                                          
IREPQ    EQU   ((XMERR03-XMERR00)/4)+1                                          
IBKQ     EQU   ((XMERR06-XMERR00)/4)+1                                          
                                                                                
*                                                                               
XMERR00  DS    0H                                                               
XMERR01  B     MFLD                MISSING INPUT FIELD                          
XMERR02  B     IFLD                INVALID INPUT FIELD                          
XMERR03  B     IREP                INVALID REP                                  
XMERR06  B     IBOOK               INVALID BOOK                                 
ERRX#    EQU   ((*-XMERR00)/4)+1                                                
                                                                                
ERRX2#   EQU   ((*-XMERR00)/4)+1                                                
                                                                                
XMERRQ   EQU   ((*-XMERR00)/4)+1                                                
         EJECT                                                                  
                                                                                
MFLD     DS    0H                  MISSING INPUT FIELD                          
         MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
IFLD     DS    0H                  INVALID INPUT FIELD                          
         MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
*                                                                               
IREP     DS    0H                  INVALID REP                                  
         MVC   MSGNUM2,=AL2(RR#IREP)                                            
         B     ERRGTXT                                                          
*                                                                               
IBOOK    DS    0H                  INVALID BOOK                                 
         MVC   MSGNUM2,=H'91'                                                   
         MVI   MSGSYS,15                                                        
         B     ERRGTXT                                                          
         EJECT                                                                  
ERRGTXT  DS    0H                  TELL GENCON TO GOTO GETTXT                   
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMTYP,GTMERR        ERROR TYPE MSG                              
         MVC   GTMSGNO,MSGNUM2      ERROR #                                     
         MVC   GTMSYS,MSGSYS        IN CASE CONNECTED SYS OVERRIDED             
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT          A(OUTPUT AREA)                              
         CLI   MYTEXT,0            ANY REPLACE TEXT?                            
         BE    *+18                                                             
         MVC   GTLTXT,MYTEXT        YES, PUT LENGTH IN                          
         LA    RF,MYTEXT+1                                                      
         STCM  RF,7,GTATXT           AS WELL AS THE ADDR OF THE TEXT            
         DROP  R1                                                               
         MVI   ERROR,0                                                          
         B     ERREXIT                                                          
                                                                                
                                                                                
ERREXIT  DS    0H                                                               
         LR    R0,R2               SAVE DISPL OF FIELD                          
         S     R0,ATWA                                                          
         STH   R0,PRVFDSP                                                       
         OC    ACURFORC,ACURFORC   WANT TO FORCE CURSOR ELSEWHERE?              
         BZ    *+8                                                              
         L     R2,ACURFORC          YEP                                         
                                                                                
         B     ALLMSGX                                                          
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBRXM--WRN MSGS)'            
*-------------------------- WARNING MESSAGES -------------------------*         
                                                                                
* These are messages where the user needs to hit <Enter> only for a             
*  response (for acknowledgment).  Previous values are restored,                
*  except in the case when the key changed in the same transaction.             
                                                                                
XMWRN    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         MVC   AERREX,ERREX                                                     
         LA    R2,CONKEYH          FORCE CURSOR TO KEY                          
                                                                                
         DS    0H                                                               
         CLI   OURWRNCD,0                                                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   OURWRNCD,XMWRNQ                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZIC   RF,OURWRNCD         BRANCH OFF TO SET WARNING MESSAGE            
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     XMWRN00(RF)                                                      
                                                                                
WTOPQ    EQU   ((XMWRN01-XMWRN00)/4)+1                                          
WBOTQ    EQU   ((XMWRN02-XMWRN00)/4)+1                                          
                                                                                
XMWRN00  DS    0H                                                               
XMWRN01  B     WTOP                TOP OF LIST                                  
XMWRN02  B     WBOT                BOTTOM OF LIST                               
WRNX#    EQU   ((*-XMWRN00)/4)+1                                                
                                                                                
XMWRNQ   EQU   ((*-XMWRN00)/4)+1                                                
         EJECT                                                                  
WTOP     DS    0H                       TOP OF LIST                             
         MVC   MSGNUM2,=AL2(RW#TOPLI)                                           
         B     WRNGTXT                                                          
*                                                                               
WBOT     DS    0H                       BOTTOM OF LIST                          
         MVC   MSGNUM2,=AL2(RW#BOTLI)                                           
         B     WRNGTXT                                                          
         EJECT                                                                  
WRNGTXT  DS    0H                  TELL GENCON TO GOTO GETTXT                   
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMTYP,GTMWRN        ERROR TYPE MSG                              
         MVC   GTMSGNO,MSGNUM2      ERROR #                                     
         MVC   GTMSYS,MSGSYS        IN CASE CONNECTED SYS OVERRIDED             
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT          A(OUTPUT AREA)                              
         CLI   MYTEXT,0            ANY REPLACE TEXT?                            
         BE    *+18                                                             
         MVC   GTLTXT,MYTEXT        YES, PUT LENGTH IN                          
         LA    RF,MYTEXT+1                                                      
         STCM  RF,7,GTATXT           AS WELL AS THE ADDR OF THE TEXT            
         DROP  R1                                                               
         B     WRNEXIT                                                          
                                                                                
                                                                                
WRNEXIT  DS    0H                                                               
         MVI   ERROR,0                                                          
         ICM   R0,15,ACURFORC      OVERRIDE CURSOR POSITION                     
         BZ    *+6                                                              
         LR    R2,R0                YEP                                         
                                                                                
         B     ALLMSGX                                                          
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBRXM--INF MSGS)'            
*---------------------------- INFO MESSAGES --------------------------*         
                                                                                
* At entry, R2-->appropriate field to put cursor on.                            
                                                                                
XMINF    DS    0H                                                               
         MVI   MSGSYS,0            ASSUME CONNECTED SYS NOT OVERRIDED           
         XC    ACURFORC,ACURFORC                                                
         LA    R1,CONHEAD                                                       
                                                                                
         DS    0H                                                               
         MVC   AERREX,ERREX        SET ADDRESS OF ERREX ROUTINE                 
         CLI   OURINFCD,INFX#                                                   
         BL    XMINFGO                                                          
         MVC   AERREX,ERREX2        TO GO OFF TO                                
         CLI   OURINFCD,INFX2#                                                  
         BL    XMINFGO                                                          
         DC    H'0'                                                             
                                                                                
XMINFGO  DS    0H                                                               
         CLI   OURINFCD,0                                                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   OURINFCD,XMINFQ                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZIC   RF,OURINFCD         BRANCH OFF TO SET INFO MESSAGE               
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     XMINF00(RF)                                                      
                                                                                
NDPLYQ   EQU   ((XMINF01-XMINF00)/4)+1  NO DATA TO DISPLAY                      
RECSVQ   EQU   ((XMINF02-XMINF00)/4)+1  RECORD SAVED - ENTER NEXT               
NTRDELQ  EQU   ((XMINF03-XMINF00)/4)+1  REC DSPLYED - PRESS ENTR TO DEL         
RDELQ    EQU   ((XMINF04-XMINF00)/4)+1  REC DISPLAYED IS DELETED                
NTRRSTQ  EQU   ((XMINF05-XMINF00)/4)+1  REC DSPLYED - PRESS ENTR TO RST         
RRSTQ    EQU   ((XMINF06-XMINF00)/4)+1  REC RESTORED - ENTER NXT RQST           
                                                                                
XMINF00  DS    0H                                                               
XMINF01  B     NDPLY                                                            
XMINF02  B     RECSV               RECORD SAVED - ENTER NEXT                    
XMINF03  B     NTRDEL              RCRD DISPLAYED - PRESS ENTER TO DEL          
XMINF04  B     RDEL                RECORD DISPLAYED IS DELETED                  
XMINF05  B     NTRRST              RCRD DISPLAYED - PRESS ENTER TO RSTR         
XMINF06  B     RRST                RECORD RESTORED - ENTER NEXT REQUEST         
INFX#    EQU   ((*-XMINF00)/4)+1                                                
                                                                                
INFX2#   EQU   ((*-XMINF00)/4)+1                                                
                                                                                
XMINFQ   EQU   ((*-XMINF00)/4)+1                                                
         EJECT                                                                  
NDPLY    DS    0H                       NO DATA TO DISPLAY                      
         MVC   MSGNUM2,=AL2(RI#NDPLY)                                           
         MVC   ACURFORC,AFRSTKEY                                                
         B     INFGTXT                                                          
*                                                                               
RECSV    DS    0H                       RECORD SAVED - ENTER NEXT               
         MVC   MSGNUM2,=AL2(2045)                                               
         MVI   MSGSYS,GTGENSYS                                                  
         B     INFGTXT                                                          
*                                                                               
NTRDEL   DS    0H                       REC DSPLYED - PRESS NTR TO DEL          
         MVC   MSGNUM2,=AL2(24)                                                 
         MVC   ACURFORC,AFRSTKEY                                                
         MVI   MSGSYS,GTGENSYS                                                  
         B     INFGTXT                                                          
*                                                                               
RDEL     DS    0H                       RECORD DISPLAYED IS DELETED             
         MVC   MSGNUM2,=AL2(RI#RDIDL)                                           
         MVC   ACURFORC,AFRSTKEY                                                
         B     INFGTXT                                                          
*                                                                               
NTRRST   DS    0H                       REC DSPLYED - PRESS NTR TO RSTR         
         MVC   MSGNUM2,=AL2(25)                                                 
         MVC   ACURFORC,AFRSTKEY                                                
         MVI   MSGSYS,GTGENSYS                                                  
         B     INFGTXT                                                          
*                                                                               
RRST     DS    0H                       RECORD RSTORED - ENTER NXT RQST         
         MVC   MSGNUM2,=AL2(8)                                                  
         MVC   ACURFORC,AFRSTKEY                                                
         MVI   MSGSYS,GTGENSYS                                                  
         B     INFGTXT                                                          
*                                                                               
         EJECT                                                                  
INFGTXT  DS    0H                  TELL GENCON TO GOTO GETTXT                   
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMTYP,GTMINF        ERROR TYPE MSG                              
         MVC   GTMSGNO,MSGNUM2      ERROR #                                     
         MVC   GTMSYS,MSGSYS        IN CASE CONNECTED SYS OVERRIDED             
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT          A(OUTPUT AREA)                              
         CLI   MYTEXT,0            ANY REPLACE TEXT?                            
         BE    *+18                                                             
         MVC   GTLTXT,MYTEXT        YES, PUT LENGTH IN                          
         LA    RF,MYTEXT+1                                                      
         STCM  RF,7,GTATXT           AS WELL AS THE ADDR OF THE TEXT            
         DROP  R1                                                               
         B     INFEXIT                                                          
                                                                                
                                                                                
INFEXIT  DS    0H                                                               
         OC    ACURFORC,ACURFORC   NEED TO SET CURSOR?                          
         BNZ   INFEXITX             NOPE                                        
         MVC   ACURFORC,AFRSTKEY   PLACE CURSOR ON 1ST KEY FIELD,               
                                                                                
INFEXITX DS    0H                                                               
         MVI   ERROR,0                                                          
         B     ALLMSGX                                                          
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBRXM--LTORG && CONS+        
               TANTS)'                                                          
*-------------------------- LTORG & CONSTANTS ------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SUBRXM--MISC STUFF)'          
*--------------------- SUBRXM MISCELLANEOUS STUFF --------------------*         
                                                                                
SUBRXML  EQU   *-XMSGRTN                                                        
         DS    0CL(X'1000'-SUBRXML+1)                                           
***********************************************************************         
         DROP  R7,R8,RA,RB,RC                                                   
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX'                               
***********************************************************************         
*========================== RMP0F's EQUATES ==========================*         
                                                                                
EOT      EQU   X'00'               END OF TABLE MARKER                          
                                                                                
PHFFXRPT EQU   X'0F'               PHASE NUMBER FOR FILEFIX REPORT              
SCFFXRPT EQU   X'F0'               SCREEN NUMBER FOR FILEFIX REPORT             
                                                                                
PAGEQ    EQU   2                   TWA PAGE # FOR TEMPSTR                       
UPGDLNQ  EQU   14                  LENGTH OF UPGRADE ELEMENT                    
MXFTNTRY EQU   200                 MAX # OF FROM TABLE ENTRIES                  
IKYREPL  EQU   RINVKREP-RINVREC+L'RINVKREP                                      
IKYSTAL  EQU   RINVKSTA-RINVREC+L'RINVKSTA                                      
IKYINVL  EQU   RINVKINV-RINVREC+L'RINVKINV                                      
IKYSTDL  EQU   RINVKSTD-RINVREC+L'RINVKSTD                                      
DMKIVRQ  EQU   (RINVPEL-RINVREC)-(DRFRSTEL-DRKEY)                               
TIASVLEN EQU   BIGENDSV-BIGAREA                                                 
IUNWRKL  EQU   IUNRECL+1000                                                     
MXPRTL   EQU   ((L'P)/2)-2         MAX LEN FOR HALF-OF-A-PRINT-AREA             
MXILNTRY EQU   60                  MAX ENTRIES FOR INVENTORY LIST TABLE         
                                                                                
*                                 ********* BIT MANIPULATIONS *********         
XFF      EQU   X'FF'                                                            
X80      EQU   X'80'                                                            
X40      EQU   X'40'                                                            
X20      EQU   X'20'                                                            
X10      EQU   X'10'                                                            
X08      EQU   X'08'                                                            
X04      EQU   X'04'                                                            
X02      EQU   X'02'                                                            
X01      EQU   X'01'                                                            
X00      EQU   X'00'                                                            
                                                                                
*                                 ************* BOOK BITS *************         
BBARB    EQU   X00                 ARBITRON                                     
BBMFX    EQU   BBARB               MEDIAFAX                                     
BBNSI    EQU   X40                 NIELSEN                                      
BBSRC    EQU   X40+X01             STRATEGY                                     
                                                                                
BBPBK    EQU   X04                 PROJECTED BOOK                               
BBTP     EQU   X08                 TIME PERIOD                                  
BBSS     EQU   X02                 SPECIAL SURVEY                               
BBEBK    EQU   X20                 ESTIMATED BOOK                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=============================== TABLES ==============================*         
                                                                                
*                                  TABLE OF DISPLACEMENTS                       
DISPTAB  DS    0XL(2+2+2)                                                       
         DC    AL2(GOSUB-RMP0F,0,AGOSUB-SYSD)                                   
         DC    AL2(GOSPOOL-RMP0F,0,AGOSPOOL-SYSD)                               
         DC    AL2(SUBR01-RMP0F,0,ASUBR01-SYSD)                                 
         DC    AL2(SUBR02-RMP0F,0,ASUBR02-SYSD)                                 
         DC    AL2(SUBR03-RMP0F,0,ASUBR03-SYSD)                                 
         DC    AL2(XMSGRTN-RMP0F,0,AXMSGRTN-SYSD)                               
         DC    AL2(TABELCPY-RMP0F,0,ATBELCPY-SYSD)                              
         DC    AL2(SVCLST-RMP0F,0,ASVCTAB-SYSD)                                 
         DC    AL2(REPLIST-RMP0F,0,AREPLIST-SYSD)                               
         DC    AL2(SHARES-RMP0F,0,ASHARES-SYSD)                                 
         DC    AL2(DEMOSHR-RMP0F,0,ADEMOSHR-SYSD)                               
         DC    AL2(MONTAB-RMP0F,0,AMONTAB-SYSD)                                 
         DC    AL2(BADRECTB-RMP0F,0,ABADRCTB-SYSD)                              
         DC    AL2(STTMSGTB-RMP0F,0,ASTTMSTB-SYSD)                              
         DC    AL2(TRKQTAB-RMP0F,0,ATRKQTAB-SYSD)                               
         DC    AL2(INVLIST-BIGAREA,ATIA-GEND,AINVLIST-SYSD)                     
         DC    AL2(INVHDR-BIGAREA,ATIA-GEND,AINVHDR-SYSD)                       
         DC    AL2(DBLOCK1-BIGAREA,ATIA-GEND,ADBLOCK-SYSD)                      
         DC    AL2(DBXTND1-BIGAREA,ATIA-GEND,ADBXTND-SYSD)                      
         DC    AL2(DBXTRINV-BIGAREA,ATIA-GEND,ADBXTRIN-SYSD)                    
         DC    AL2(DBXTDYTM-BIGAREA,ATIA-GEND,ADBXTDT-SYSD)                     
DISPTABQ EQU   (*-DISPTAB)/(L'DISPTAB)                                          
                                                                                
                                                                                
LBLTAB   DS    0XL(2+8)            TABLE OF LABELS FOR BIG STORAGE              
         DC    AL2(INVLLABL-BIGAREA),CL8'*INVLST*'                              
         DC    AL2(IHDRLABL-BIGAREA),CL8'*INVHDR*'                              
         DC    AL2(DBLKLABL-BIGAREA),CL8'*DBLOCK*'                              
LBLTABQ  EQU   (*-LBLTAB)/(L'LBLTAB)                                            
                                                                                
                                                                                
TABCLR   DS    0XL(1+2+2)          STORAGES TO CLEAR                            
         DC    C'S',AL2(MYTEXT-SYSD),AL2(MYTEXTX-MYTEXT)                        
TABCLRQ  EQU   (*-TABCLR)/(L'TABCLR)                                            
                                                                                
                                                                                
TABELCPY DS    0X                  TABLE OF ELEMENTS TO COPY                    
         DC     XL1'03'             TRANSFER FROM ELEMENT                       
         DC     XL1'CD'             CODE ELEMENT                                
         DC     XL1'CE'             DAY/TIME ELEMENT                            
         DC     XL1'EF'             ACTIVITY ELEMENT                            
         DC     XL1'F1'             GENCON'S ACTIVITY ELEMENT                   
         DC     AL1(EOT)                                                        
         EJECT                                                                  
DCLIST   DS    0C                  DATA DICTIONARY ESCAPE SEQUENCES             
         DCDDL RE#10PF,L'RE@10PF,L                                              
         DCDDL RE#PF10S,L'RE@PF10S,L                                            
         DCDDL RE#PF12R,L'RE@PF12R,L                                            
         DCDDL RE#PF12N,L'RE@PF12N,L                                            
         DCDDL RE#CHA,L'RE@CHA,L                                                
         DCDDL RE#OPTS,L'RE@OPTS,L                                              
         DCDDL RE#SEL,L'RE@SEL,L                                                
         DCDDL RE#LP,L'RE@LP,L                                                  
DCLISTX  EQU   *                                                                
                                                                                
                                                                                
REPLIST  DS    0CL(2+6)                                                         
         DC     CL2'AM',CL6'KAMNYR'                                             
         DC     CL2'BL',CL6'BLRNY '                                             
         DC     CL2'CQ',CL6'KCONYR'                                             
         DC     CL2'KH',CL6'REPDEM'                                             
         DC     CL2'NK',CL6'KNANYR'                                             
         DC     CL2'PV',CL6'PETNY '                                             
         DC     CL2'SJ',CL6'SJR   '                                             
         DC     CL2'SZ',CL6'SELNY '                                             
         DC     CL2'UT',CL6'UTSNY '                                             
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
SHARES   DS    0XL3                DEMOLIST FOR SHARES                          
         DC     X'00',C'S',AL1(1)                                               
         DC     X'00',C'S',AL1(2)                                               
         DC     X'00',C'S',AL1(3)                                               
         DC    X'FF'                                                            
                                                                                
                                                                                
DEMOSHR  DS    0XL3                ANOTHER DEMOLIST FOR SHARES                  
         DC     X'81',C'S',AL1(1)                                               
         DC     X'81',C'S',AL1(2)                                               
         DC     X'81',C'S',AL1(3)                                               
         DC    X'FF'                                                            
                                                                                
                                                                                
MONTAB   DS    0CL1                MONTH TABLE                                  
         DC     C'NFMAYJO'                                                      
MONTABQ  EQU   ((*-MONTAB)/L'MONTAB)                                            
         EJECT                                                                  
*-------------------------- REPORT MESSAGES --------------------------*         
                                                                                
TRKQTAB  DS    0XL(23+1+2)         TABLE OF TRACK COUNTERS                      
         DC     CL23'NUMBER OF ACTUAL TRACKS',C' ',AL2(CNTATRK-SYSD)            
         DC     CL23'NUMBER OF PRJCTD TRACKS',C'P',AL2(CNTPTRK-SYSD)            
         DC     CL23'NUMBER OF TIME . TRACKS',C'T',AL2(CNTTTRK-SYSD)            
         DC     CL23'NUMBER OF SPCIAL TRACKS',C'S',AL2(CNTSTRK-SYSD)            
         DC     CL23'NUMBER OF ESTMTD TRACKS',C'E',AL2(CNTETRK-SYSD)            
         DC     CL23'                       ',C'-',AL2(0)                       
         DC     CL23'TOTAL NUMBER OF TRACKS ',X'00',AL2(CNTTRKS-SYSD)           
TRKQTABX EQU   *-TRKQTAB                                                        
TRKQTABQ EQU   TRKQTABX/L'TRKQTAB                                               
                                                                                
                                                                                
BADRECTB DS    0XL(30+2)           TABLE OF "BAD" RECORD REASONS                
BRTIVTRY DC     CL30'TRACK X-FERRED FROM INV FILE',AL2(CNTIVXFR-SYSD)           
BRTIMPRC DC     CL30'TRACK ALREADY IN IMP-BASED',AL2(CNTIMPRC-SYSD)             
BRTDIFDT DC     CL30'TRACK HAS DIFFERENT DAY/TIME',AL2(CNTDIFDT-SYSD)           
BRTTRKNR DC     CL30'TRACK NOT RE-CREATED',AL2(CNTTRKNR-SYSD)                   
BRTFRDTL DC     CL30'TRACK HAS BAD "FROM DETAILS"',AL2(CNTBADFD-SYSD)           
BRTMINFO DC     CL30'TRACK IS MISSING INFORMATION',AL2(CNTMINFO-SYSD)           
BRTUNKCD DC     CL30'TRACK HAS AN UNKNOWN CODE',AL2(CNTUNKCD-SYSD)              
BRTUPGD  DC     CL30'TRACK HAS AN UPGRADE FORMULA',AL2(CNTUPGD-SYSD)            
BRTTRGT  DC     CL30'TRACK XFER ONLINE 1/20-26/97',AL2(CNTTRGT-SYSD)            
BADRCTBX EQU   *-BADRECTB                                                       
BADRCTBQ EQU   BADRCTBX/L'BADRECTB                                              
         DS    0XL((BADRCTBQ*(L'BADRECTB))-BADRCTBX+1)                          
         DS    0XL(BADRCTBX-(BADRCTBQ*(L'BADRECTB))+1)                          
GLEETEST EQU   (BADRCTBX-(BADRCTBQ*(L'BADRECTB)))                               
                                                                                
                                                                                
STTMSGTB DS    0XL(25+2)           JOB STATISTICS MESSAGES                      
         DC     CL25'NUMBER OF DIRECTORY READS',AL2(CNTDREAD-SYSD)              
         DC     CL25'NUMBER OF FILE      READS',AL2(CNTFREAD-SYSD)              
         DC     CL25'NUMBER OF RECORDS FIXED  ',AL2(CNTRFIX-SYSD)               
         DC     CL25'# OF RECORDS MISSING INFO',AL2(CNTMINFO-SYSD)              
         DC     CL25'# OF RECS W/ BAD FRM DTLS',AL2(CNTBADFD-SYSD)              
         DC     CL25'# OF TRKS NOT RE-CREATED ',AL2(CNTTRKNR-SYSD)              
         DC     CL25'# OF TARGET RECDS FOUND  ',AL2(CNTTRGT-SYSD)               
STTMSTBX EQU   *-STTMSGTB                                                       
STTMSTBQ EQU   STTMSTBX/L'STTMSGTB                                              
         DS    0XL((STTMSTBQ*L'STTMSGTB)-STTMSTBX+1)                            
         DS    0XL(STTMSTBX-(STTMSTBQ*L'STTMSGTB)+1)                            
         EJECT                                                                  
*---------------------------- SOURCE TABLE ---------------------------*         
                                                                                
       ++INCLUDE RESVCTAB                                                       
***********************************************************************         
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (RERMPWORKD)'                  
***********************************************************************         
*============================= RERMPWORKD ============================*         
       ++INCLUDE RERMPWORKD                                                     
***********************************************************************         
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (SYSD)'                        
***********************************************************************         
*========================== SYSD'S SYSSPARE ==========================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
*----------------------- OWNED BY RERMP0F ONLY -----------------------*         
                                                                                
*                                 ************** WORK AREA ************         
DEMODUB  DS    D                   EXTRA STORAGE FOR DEMUP                      
DIVIDEND DS    D                                                                
DIVISOR  DS    F                                                                
REMAINDR DS    F                                                                
QUOTIENT DS    F                                                                
MYDMCB   DS    6F                                                               
MYWORK   DS    XL(L'WORK)                                                       
RELO     DS    F                                                                
                                                                                
*                                 ************** ADDRESSES ************         
MYBASE1  DS    A                   A(1ST 4096 BYTES OF THIS PROGRAM)            
MYBASE2  DS    A                   A(2ND 4096 BYTES OF THIS PROGRAM)            
ASUBRTN  DS    A                   A(SUBROUTINE POOL TO USE)                    
AERREX   DS    A                   A(ERREX ROUTINE) TO USE                      
PRVFDSP  DS    H                   FIELD DISP FROM PREV TRANSACTION             
DSP1EL   DS    H                   DISPL TO 1ST ELEM OF INV RECORD              
                                                                                
         DS    0A                  ADDRESSES OF TABLES/ROUTINES                 
AGOSUB   DS    A                    A(SUBROUTINE POOL INTERFACE)                
AGOSPOOL DS    A                    A(SPOOL INTERFACE)                          
ASUBR01  DS    A                    A(SUBROUTINE POOL #1)                       
ASUBR02  DS    A                    A(SUBROUTINE POOL #2)                       
ASUBR03  DS    A                    A(SUBROUTINE POOL #3)                       
AXMSGRTN DS    A                    A(EXIT W/ MSG ROUTINE)                      
ATBELCPY DS    A                    A(TABELCPY)                                 
ASVCTAB  DS    A                    A(SVCLST)                                   
AREPLIST DS    A                    A(REPLIST)                                  
ASHARES  DS    A                    A(SHARES)                                   
ADEMOSHR DS    A                    A(DEMOSHR)                                  
AMONTAB  DS    A                    A(MONTAB)                                   
ABADRCTB DS    A                    A(BADRECTB)                                 
ASTTMSTB DS    A                    A(STTMSGTB)                                 
ATRKQTAB DS    A                    A(TRKQTAB)                                  
AINVLIST DS    A                    A(INVENTORY LIST)                           
AINVHDR  DS    A                    A(INVENTORY HEADER)                         
ADBLOCK  DS    A                    A(DBLOCK)                                   
ADBXTND  DS    A                    A(DBLOCK EXTENSION AREA)                    
ADBXTRIN DS    A                    A(C'RINV' DBLOCK EXTENSION AREA)            
ADBXTDT  DS    A                    A(C'DYTM' DBLOCK EXTENSION AREA)            
AIUNWRK  DS    A                    A(IUN WORK AREA)                            
                                                                                
*                                 ************** COUNTERS *************         
CNTFLDS  DS    0F                                                               
CNTDREAD DS     F                  COUNT # OF DIRECTORY READS                   
CNTFREAD DS     F                  COUNT # OF FILE READS                        
CNTRFIX  DS     F                  COUNT # OF RECORDS FIXED                     
CNTGOPMM DS     F                  COUNT # OF TIMES WENT TO PMM# RTN            
CNTIVXFR DS     F                  COUNT # OF TRACKS XFERRED FROM INV           
CNTIMPRC DS     F                  COUNT # OF TRACKS IN ALRDY IMP-BASED         
CNTDIFDT DS     F                  COUNT # OF TRACKS W/ DFFRNT DAY/TIME         
CNTTRKNR DS     F                  COUNT # OF TRACKS NOT RECREATED              
CNTBADFD DS     F                  COUNT # OF TRACKS W/ BAD FROM DTAILS         
CNTMINFO DS     F                  COUNT # OF TRACKS W/ MISSING INFO            
CNTUNKCD DS     F                  COUNT # OF TRACKS W/ UNKNOWN CODE            
CNTUPGD  DS     F                  COUNT # OF TRACKS W/ UPGRADE ELEM            
CNTTRGT  DS     F                  COUNT # OF TRACKS FIT TARGET                 
CNTDHK1  DS     F                  COUNT # OF TIMES INTO DEMHOOK1               
CNTATRK  DS     F                  COUNT # OF ACTUAL      TRACKS                
CNTPTRK  DS     F                  COUNT # OF PROJECTED   TRACKS                
CNTTTRK  DS     F                  COUNT # OF TIME PERIOD TRACKS                
CNTSTRK  DS     F                  COUNT # OF SPECIAL     TRACKS                
CNTETRK  DS     F                  COUNT # OF ESTIMATED   TRACKS                
CNTTRKS  DS     F                  COUNT # OF TOTAL       TRACKS                
CNTFLDSX EQU   *                                                                
CNTFLDSL EQU   CNTFLDSX-CNTFLDS                                                 
                                                                                
*                                 ********** INPUT PARAMETERS *********         
INPTPRMS DS    0X                  INPUT PARAMETERS                             
IPREP    DS     CL2                 REP CODE                                    
IPBOOK   DS     XL2                 BOOK                                        
IPBOOK_P DS     CL6                 BOOK IN PRINTABLE FORMAT                    
                                                                                
*                                 ************ HEADER INFO ************         
HDRINFO  DS    0X                                                               
HDRKEY   DS     XL(IKYSTDL)         KEY OF INV HEADER (W/O KSRC & TRK)          
HDRIDAY  DS     XL1                 (INTERNAL) DAY                              
HDRTIME  DS     0XL4                TIMES                                       
HDRSTIM  DS      XL2                 START                                      
HDRETIM  DS      XL2                 END                                        
HDRPRGNA DS     CL27                PROGRAM NAME                                
HDRINFOX EQU   *                                                                
HDRINFOL EQU   HDRINFOX-HDRINFO                                                 
                                                                                
*                                 ************* TRACK INFO ************         
TRKINFO  DS    0X                                                               
TRKKQLFY DS     CL1                 TRACK KEY: QUALIFIER                        
TRKKBTYP DS     CL1                 TRACK KEY: BOOK TYPE                        
TRKFTNT  DS     CL16                FOOTNOTE                                    
TRKNHELS DS     XL1                 NUMBER OF HISTORY ELEMENTS                  
TRKCFPAV DS     XL1                 NUMBER OF HISTORY ELEMENTS FROM PAV         
TRKXFRCD DS     CL2                 TRANSFER CODE                               
TRKUPGEL DS     XL14                UPGRADE ELEMENT                             
TRKFLAG1 DS     XL1                 FLAGS                                       
TRK_DA   DS     A                   DISK ADDRESS OF TRACK                       
TRKINFOX EQU   *                                                                
TRKINFOL EQU   TRKINFOX-TRKINFO                                                 
                                                                                
*                                 ********* FROM DETAILS DATA *********         
FROMDATA DS    0X                                                               
FDFILE   DS     CL3                 FILE                                        
FDTPTT   DS     CL1                 4 WK AVG OR TYPICAL TIME TP INFO            
FDSRC    DS     CL3                 RATING SERVICE                              
FDMED    DS     CL1                 MEDIA                                       
FDSTTN   DS     CL5                 STATION                                     
FDFRBK   DS     0XL3                BOOK                                        
FDBITS   DS      XL1                 BITS                                       
FDBOOK   DS      XL2                 BOOK                                       
FDBTYP   DS     CL1                 BOOK TYPE                                   
FDFTYP   DS     CL1                 FROM TYPE (P/I)                             
FDFRPR   DS     CL1                 FROM FUNCTION (RINVFRPR)                    
FROMDATX EQU   *                                                                
FROMDATL EQU   FROMDATX-FROMDATA                                                
                                                                                
*                                 ******* PROGRAM TITLE FOOTNOTE ******         
FNWORK   DS    0X                                                               
FNWB1    DS     0XL(2+1)            FIRST BOOK INFO                             
FNWB1BK  DS      XL2                                                            
FNWB1BT  DS      CL1                                                            
FNWB2    DS     0XL(2+1)            SECOND BOOK INFO                            
FNWB2BK  DS      XL2                                                            
FNWB2BT  DS      CL1                                                            
FNWPROG1 DS     CL16                NAME OF FIRST PROGRAM                       
FNWPROG2 DS     CL16                NAME OF SECOND PROGRAM                      
FNWORKX  EQU   *                                                                
FNWORKL  EQU   FNWORKX-FNWORK                                                   
                                                                                
*                                 ************ TEMP STORAGE ***********         
TMPS     DS   0X                                                                
TMPFIL   DS    CL3                 TEMP STORAGE FOR FILE                        
TMPTPTT  DS    CL1                  "      "     "  TYPE OF TP INFO             
TMPSRC   DS    CL3                  "      "     "  SOURCE                      
TMPKSRC  DS    CL1                  "      "     "  KEY SOURCE                  
TMPFRBK  DS    0XL3                 "      "     "  FROM BOOK                   
TMPFRBTS DS     XL1                 "      "     "   BITS                       
TMPBOOK  DS     XL2                 "      "     "   BOOK                       
TMPCODE  DS    CL2                  "      "     "  TRANSFER CODE               
TMPSTTN  DS    CL5                  "      "     "  STATION                     
TMPINVN  DS    CL4                  "      "     "  INVENTORY NUMBER            
TMPPURE  DS    CL4                  "      "     "  PURE NUMBER                 
TMPBTYP  DS    CL1                  "      "     "  BOOK TYPE                   
TMPWKN   DS    XL1                  "      "     "  WEEK NUMBER (1-4)           
TMPIDAY  DS    XL1                  "      "     "  INTERNAL DAY                
TMPKDAY  DS    XL1                  "      "     "  KEY DAY                     
TMPSETM  DS    0XL4                 "      "     "  TIMES                       
TMPSTIM  DS     XL2                 "      "     "   START TIME                 
TMPETIM  DS     XL2                 "      "     "   END TIME                   
TMPNOR   DS    CL1                  "      "     "  NOR PROGRAM FLAG            
TMPNQHR  DS    XL1                  "      "     "  # OF QUARTER HOURS          
TMPWKS   DS    XL1                  "      "     "  WEEKS                       
TMPBEST  DS    CL1                  "      "     "  BEST/ALL                    
TMPPGNAM DS    CL13                 "      "     "  PROGRAM NAME                
TMPMED   DS    CL1                  "      "     "  MEDIA                       
TMPQLFY  DS    CL1                  "      "     "  QUALIFIER                   
TMPFLAG  DS    XL1                  "      "     "  FLAGS                       
TMPSX    EQU  *                                                                 
TMPSL    EQU  TMPSX-TMPS                                                        
                                                                                
PVFXSTTN DS    CL(L'RINVKSTA)      PREVIOUS STATION FIXED                       
PRNT_DA  DS    CL8                 DISK ADDRESS IN EBCDIC                       
MYTAPEP  DS    CL1                 MY FLAG FOR TAPE PRECISION                   
OURBYTE  DS    XL1                 ANOTHER 1-BYTE WORKING STORAGE               
GOSUBN   DS    XL1                 SUBROUTINE NUMBER                            
MYRDUPDT DS    XL1                 MY VERSION OF RDUPDATE                       
MSGNUM2  DS    XL2                                                              
MSGSYS   DS    XL1                                                              
MSGTYPE  DS    CL1                 (E)RROR, (W)ARNING, OR (I)NFO                
OURERRCD DS    XL1                 MY ERROR   CODE                              
OURWRNCD DS    XL1                 MY WARNING CODE                              
OURINFCD DS    XL1                 MY INFO    CODE                              
BADRCNUM DS    XL1                 REASON # FOR BAD RECORD                      
NTIMES   DS    XL1                                                              
COUNTER  DS    XL1                                                              
FLDDSPL  DS    XL1                 DISPL OF SUB-FIELD INTO FIELD                
SCANLNTH DS    XL1                 L'RHS OF SCANNER BLOCK ENTRY                 
ELCDLO   DS    XL1                 LOW  ELEMENT CODE                            
ELCDHI   DS    XL1                 HIGH ELEMENT CODE                            
MMELCODE DS    XL1                 ELCODE OF ELEM W/ MISMATCH                   
                                                                                
*                                 *********** PMM# WORK AREA **********         
PMMWORK  DS    0F                                                               
LPRTADDR DS     A                  A(LHS SOURCE)                                
LPRTLEN  DS     H                  L(LHS SOURCE)                                
LPRTINDT DS     XL1                AMOUNT TO INDENT ON LHS                      
LPMMBUMP DS     CL1                BUMP TO NEXT ELEM ON LHS                     
RPRTADDR DS     A                  A(RHS SOURCE)                                
RPRTLEN  DS     H                  L(RHS SOURCE)                                
RPRTINDT DS     XL1                AMOUNT TO INDENT ON RHS                      
RPMMBUMP DS     CL1                BUMP TO NEXT ELEM ON RHS                     
PMMINDT  DS     XL1                # SPACES TO INDENT                           
PMMWORKX EQU   *                                                                
PMMWORKL EQU   PMMWORKX-PMMWORK                                                 
                                                                                
*                                 *************** FLAGS ***************         
MISCFLG1 DS    XL1                 MISC FLAG #1                                 
MF1GDPLY EQU    X80                 GO TO DISPLAY LOGIC                         
MF1ADCMB EQU    X40                 ADD DATA UP FOR COMBOS                      
MF1RDDEL EQU    X20                 CAN READ DELETED RECORDS AS WELL            
MF1ERRQ  EQU    X01                                                             
MF1RSTKC EQU   0                   RESET THESE ON KEY CHANGE                    
                                                                                
*                                 *********** MISCELLANEOUS ***********         
GKSIPARM DS    CL5                 REGETKSRC INPUT  BLOCK                       
GKSOPARM DS    CL5                 REGETKSRC OUTPUT BLOCK                       
                                                                                
*                                 ************** PROFILES *************         
RMPPROFS DS    CL8                 PROFILE SETTINGS                             
                                                                                
*                                 ************* CONSTANTS *************         
DCFILTP  DS    CL3                 C'TP '                                       
DCFILPAV DS    CL3                 C'PAV'                                       
DCFILIUN DS    CL3                 C'IUN'                                       
DCFILINV DS    CL3                 C'INV'                                       
DCFILPT  DS    CL3                 C'PT '                                       
                                                                                
*                                 ******* DATA DICTIONARY TERMS *******         
DSLIST   DS    0C                                                               
RE@10PF  DS     CL49               PF5=Top  6=Bottom  7=Up  8=Down              
RE@PF10S DS     CL7                10=Save                                      
RE@PF12R DS     CL7                12=Rtrn                                      
RE@PF12N DS     CL7                12=Next                                      
RE@CHA   DS     CL6                change                                       
RE@OPTS  DS     CL7                options                                      
RE@SEL   DS     CL6                select                                       
RE@LP    DS     CL10               list parms                                   
DSLISTX  EQU   *                                                                
                                                                                
         DS    0XL(L'RE@PF12R-L'RE@PF12N+1)                                     
         DS    0XL(L'RE@PF12N-L'RE@PF12R+1)                                     
                                                                                
*                                 ************* MATH BLOCK ************         
HOMSHR   DS    3F                  SHARE VALUES                                 
HOMSHRL  EQU   *-HOMSHR                                                         
TOTSHR   DS    3F                  SHARE ACCUMULATORS                           
TOTSHRL  EQU   *-TOTSHR                                                         
CUMSHR   DS    3F                  CUMULATIVE SHARE                             
CUMSHRL  EQU   *-CUMSHR                                                         
                                                                                
CUMFCTR  DS    F                   CUMULATIVE FACTOR                            
                                                                                
         DS    0F                                                               
MATHFAC  DS    0XL17                                                            
MTHCFACS DS     A                   A(DBLOCK)                                   
MTHFCTR  DS     F                   WEIGHTING FACTOR FOR MULT & DIVIDE          
MTHIFIL  DS     CL3                 INPUT FILE                                  
MTHOFIL  DS     CL3                 OUTPUT FILE                                 
MTHOSRC  DS     CL3                 OUTPUT SOURCE                               
                                                                                
OFORMAT  DS    XL10                OUTPUT FORMAT FOR DEMAINT CALLS              
                                                                                
ORIGBK   DS    XL2                 DEFAULT BOOK (IUN) FOR DEMO BK ELEM          
                                                                                
*                                 ************** BUFFERS **************         
STARTKEY DS    XL(L'RINVKEY)                                                    
                                                                                
SVDBVALS DS    2XL4                SAVED DBAREC & DBAQUART                      
                                                                                
MYTEXT   DS    0X                  MISCELLANEOUS TEXT FIELD                     
         DS    XL1                  L'TEXT                                      
         DS    CL20                 THE TEXT ITSELF                             
MYTEXTX  EQU   *                                                                
MYTEXTL  EQU   MYTEXTX-MYTEXT                                                   
                                                                                
DUMUPGD  DS    XL(UPGDLNQ)         WORK AREA FOR UPGRADE ELEMENT                
                                                                                
MYFLD    DS    0X                  MY TWA FIELD WORK AREA                       
MYFLDH   DS     XL8                 MY WORKING FIELD HEADER                     
MYFLDD   DS     XL80                MY WORKING FIELD DATA                       
MYFLDL   EQU   *-MYFLD                                                          
         EJECT                                                                  
                                                                                
*                                                                               
MYSSPREL EQU   *-SYSSPARE                                                       
SYSPREMN EQU   L'SYSSPARE-MYSSPREL   AMOUNT LEFT IN SYSSPARE                    
         DS    0XL(SYSPREMN+1)       CHECK AGAINST SYSSPARE LIMIT               
***********************************************************************         
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (TWA DSECTS)'                  
***********************************************************************         
*================================ TWA ================================*         
                                                                                
*---------------------------- BASE SCREEN ----------------------------*         
       ++INCLUDE RERMPFFD                                                       
         EJECT                                                                  
*--------------------------- FILEFIX SCREEN --------------------------*         
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPF0D                                                       
         EJECT                                                                  
*------------------------- SAVED STORAGE AREA ------------------------*         
         ORG                                                                    
* WARNING: This is not a good place to save information around.  This           
*           area may coincide with the field  SVLIST  defined in                
*           RERMP00 and may eventually clobber what was there.                  
*                                                                               
MYTWAL   EQU   *-CONHEADH                                                       
         DS    0CL(3520-L'SFMPROFS-MYTWAL)  GENCON & RERMP'S TWA LIMIT          
                                                                                
                                                                                
*------------------------------ DDGENTWA -----------------------------*         
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
*----------------------------- RERMPWTWA -----------------------------*         
       ++INCLUDE RERMPWTWA                                                      
***********************************************************************         
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (OTHER DSECTS)'                
***********************************************************************         
*============================ OTHER DSECTS ===========================*         
                                                                                
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* FAGETTXTD                                                                     
* DDCOMFACS                                                                     
* DDGLOBEQUS                                                                    
* DDCOREQUS                                                                     
* DDDICTATED                                                                    
* REMSGEQUS                                                                     
* REDDEQUS                                                                      
* DEDEMFILE                                                                     
* RERMPPROF                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDDICTATED                                                     
       ++INCLUDE REMSGEQUS                                                      
       ++INCLUDE REDDEQUS                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE RERMPPROF                                                      
         PRINT ON                                                               
***********************************************************************         
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (REGEN DSECTS)'                
***********************************************************************         
*============================ REGEN DSECTS ===========================*         
                                                                                
*------------------------------ REGENINV -----------------------------*         
REINVRCD DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
         EJECT                                                                  
*------------------------------ REGENSTA -----------------------------*         
RSTARECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENSTA                                                       
         PRINT ON                                                               
                                                                                
                                                                                
*------------------------------ REGENAVL -----------------------------*         
RAVLRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENAVL                                                       
         PRINT ON                                                               
***********************************************************************         
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (DBLOCKD)'                     
***********************************************************************         
*============================= DEMO BLOCK ============================*         
                                                                                
* DEDBLOCK *                                                                    
DBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
                                                                                
                                                                                
* DEDBEXTRAD *                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDBEXTRAD                                                     
         PRINT ON                                                               
***********************************************************************         
         TITLE 'RERMP0F - INVENTORY FILE FILEFIX (MISC DSECTS)'                 
***********************************************************************         
*======================== MISCELLANEOUS DSECTS =======================*         
                                                                                
                                                                                
*------------------------ INVENTORY LIST ENTRY -----------------------*         
                                                                                
INVLD    DSECT                                                                  
ILFLE    DS    CL1                 P=TAPE, I=INVENTORY                          
ILDFILE  DS    CL3                 DEMO FILE                                    
ILTPTT   DS    CL1                 4 WK AVG OR TYPICAL TIME TP INFO             
ILSRC    DS    CL1                 RATING SERVICE                               
ILSTTN   DS    CL5                 STATION                                      
ILBOOK   DS    XL2                 BOOK                                         
ILBTYP   DS    CL1                 BOOK TYPE                                    
ILPURE   DS    XL2                 PURE NUMBER                                  
ILINVN   DS    CL4                 INVENTORY NUMBER                             
ILIDAY   DS    XL1                 INTERNAL DAY                                 
ILTIME   DS    0XL4                MILITARY START & END TIMES                   
ILSTIM   DS     XL2                 START TIME                                  
ILETIM   DS     XL2                 END   TIME                                  
ILNOR    DS    CL1                 NOR PROGRAMMING (PAV ONLY)                   
ILBEST   DS    CL1                 BEST/ALL PROGRAMMING                         
ILEFFDT  DS    XL2                 EFFECTIVE START DATE (COMPRESSED)            
ILTYP    DS    XL1                                                              
ILTIVN   EQU    X80                 INVENTORY/PURE NUMBER                       
ILTFDT   EQU    X40                 FIRST IN DAY/TIME EXP.                      
ILTLDT   EQU    X20                 LAST IN DAY/TIME EXP.                       
ILTADD   EQU    X08                 ADD EXPRESSION                              
ILTWOV   EQU    X04                 USER WEIGHTING OVERRIDE                     
ILWT     DS    XL1                 WEIGHT (BINARY)                              
INVLX    EQU   *                                                                
INVLQ    EQU   INVLX-INVLD                                                      
                                                                                
*----------------------------- PRINT LINE ----------------------------*         
                                                                                
PRTLINED DSECT                                                                  
PLNDREAD DS    CL7                 Nth DIRECTORY READ                           
PLCLN1   DS    CL1                 C':'                                         
                                                                                
         DS    CL2                                                              
PLKEY    DS    0Cl30               KEY OF INVENTORY RECORD                      
PLKSTTN  DS     CL6                 STATION CALL LETTERS                        
         DS     CL1                                                             
PLKINVN  DS     CL4                 INVENTORY NUMBER                            
         DS     CL1                                                             
PLKEFFD  DS     CL8                 EFFECTIVE DATE (MMMDD/YY)                   
         DS     CL1                                                             
PLKRSVC  DS     CL3                 RATING SERVICE                              
         DS     CL1                                                             
PLKBK    DS     CL5                 BOOK/TRACK                                  
                                                                                
         DS    CL1                                                              
PLSIGN   DS    CL3                 '==>'                                        
         DS    CL1                                                              
PLFTNOTE DS    CL(L'RPGMNAME)      FOOTNOTE                                     
                                                                                
PRTLINEL EQU   *-PRTLINED                                                       
         DS    0XL(L'P-PRTLINEL+1)                                              
         EJECT                                                                  
*-------------------------- REGETKSRC PARMS --------------------------*         
                                                                                
GKSPARMD DSECT                                                                  
GKSPRSVC DS    CL1                 RATING SERVICE                               
GKSPQLFY DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GKSPKSRC DS    XL1                 RINVKSRC FOR KEY                             
GKSPBKBT DS    XL1                 BOOKVAL BITS                                 
GKSPBTYP DS    CL1                 BOOK TYPE                                    
GKSPARMX EQU   *                                                                
GKSPARML EQU   GKSPARMX-GKSPARMD                                                
         DS    0XL(L'GKSIPARM-GKSPARML+1)                                       
         DS    0XL(GKSPARML-L'GKSIPARM+1)                                       
         DS    0XL(L'GKSOPARM-GKSPARML+1)                                       
         DS    0XL(GKSPARML-L'GKSOPARM+1)                                       
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================== IUN RECORD DSECT =========================*         
                                                                                
IUNRECD  DSECT                                                                  
                                                                                
                                                                                
IUNIVS   DS    (IUNNVALS)F        UNIVERSES                                     
                                                                                
                                                                                
IUNOLD   DS    0F                 ORIGINAL (OLD) BOOK VALUES                    
*                                                                               
IOLDRTG  DS    (IUNNVALS)F         RATINGS                                      
         ORG   IOLDRTG+IUNHMDSP                                                 
IUORHOME DS    F                                                                
*                                                                               
         ORG                                                                    
IOLDIMP  DS    (IUNNVALS)F         IMPRESSIONS                                  
*                                                                               
IOLDHPT  DS    (IUNNVALS)F         HUTS/PUTS                                    
         ORG   IOLDHPT+IUNHMDSP                                                 
IUOPHOME DS    F                                                                
*                                                                               
         ORG                                                                    
IOLDTOT  DS    (IUNNVALS)F         TSA TOTALS                                   
         ORG   IOLDTOT+IUNHMDSP                                                 
IUOQHOME DS    F                                                                
*                                                                               
         ORG                                                                    
IUNOLDX  EQU   *                                                                
                                                                                
                                                                                
IUNNEW   DS    0F                 NEW VALUES                                    
*                                                                               
INEWRTG  DS    (IUNNVALS)F         RATINGS                                      
         ORG   INEWRTG+IUNHMDSP                                                 
IUNRHOME DS    F                                                                
*                                                                               
         ORG                                                                    
INEWIMP  DS    (IUNNVALS)F         IMPRESSIONS                                  
*                                                                               
INEWHPT  DS    (IUNNVALS)F         HUTS/PUTS                                    
         ORG   INEWHPT+IUNHMDSP                                                 
IUNPHOME DS    F                                                                
*                                                                               
         ORG                                                                    
INEWTOT  DS    (IUNNVALS)F         TSA TOTALS                                   
         ORG   INEWTOT+IUNHMDSP                                                 
IUNQHOME DS    F                                                                
*                                                                               
         ORG                                                                    
IUNNEWX  EQU   *                                                                
                                                                                
                                                                                
         DS    0CL((IUNOLDX-IUNOLD)-(IUNNEWX-IUNNEW)+1)                         
         DS    0CL((IUNNEWX-IUNNEW)-(IUNOLDX-IUNOLD)+1)                         
                                                                                
                                                                                
IUNOTH   DS    0F                 OTHER VALUES                                  
ISHOMES  DS    F                                                                
ISMETA   DS    F                                                                
ISMETB   DS    F                                                                
*                                                                               
ILUNVS   DS    (IUNNVALS)F         LOONEYVERSES                                 
ILUNVX   EQU   *                                                                
                                                                                
                                                                                
IUNRECL  EQU   *-IUNRECD                                                        
                                                                                
                                                                                
IUNNVALS EQU   32                  # OF IUN VALUES                              
IUNLVALS EQU   IUNNVALS*4          LENGTH OF IUN VALUES                         
IUNHMNDX EQU   20                  INDEX TO HOMES RTGS IN IUN RTGS AREA         
IUNHMDSP EQU   IUNHMNDX*4          DISPL TO HOMES RTGS IN IUN RTGS AREA         
***********************************************************************         
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
*========================== BIG STORAGE AREA =========================*         
                                                                                
BIGAREA  DSECT                                                                  
                                                                                
                                                                                
INVLLABL DS    D                   *INVLST*                                     
INVLIST  DS    (MXILNTRY)XL(INVLQ)                                              
         DS    XL1                                                              
INVLISTX EQU   *                                                                
                                                                                
                                                                                
IHDRLABL DS    D                   *INVHDR*                                     
INVHDR   DS    XL(L'IO)                                                         
INVHDRX  EQU   *                                                                
                                                                                
                                                                                
BIGENDSV EQU   *                   SAVE UP TO HERE INTO TEMPSTR                 
                                                                                
                                                                                
DBLKLABL DS    D                   *DBLOCK*                                     
DBLOCK1  DS    XL(L'DBLOCK)                                                     
         DS    XL14                (RESERVED)                                   
DBLOCK1X DS    0X                                                               
                                                                                
DBXTLABL DS    D                   *DBXTND*                                     
DBXTND1  DS    0XL256                                                           
DBXTRINV DS     XL(DBXINVWL)        C'RINV' (DBXINVWK)                          
DBXTDYTM DS     XL((DBXTLIST-DBXTLD)+(10*(L'DBXTLIST))+1)                       
*                                   C'DYTM' (DBXTLD)                            
         DS    0XL(L'DBXTND1-(*-DBXTND1))                                       
DBXTND1X DS    0X                                                               
                                                                                
                                                                                
MYTIALEN EQU   *-BIGAREA                                                        
         DS    0CL((X'4800'-MYTIALEN)+1)                                        
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009RERMP0FA  03/12/97'                                      
         END                                                                    
