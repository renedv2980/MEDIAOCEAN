*          DATA SET TAGEN51    AT LEVEL 202 AS OF 06/27/16                      
*PHASE T70251A,*                                                                
         TITLE 'T70251 - PAY CONTROL'                                           
T70251   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T70251,R7,R5,CLEAR=YES                                    
         LR    R6,RC               R6=A(TEMPORARY STORAGE)                      
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         LA    R8,TWAHOLE          R8=A(PAY STORAGE)                            
         USING PAYD,R8                                                          
         ST    R6,ATMPPAYD         SAVE R6 IN WORKING STORAGE                   
         USING TMPPAYD,R6                                                       
         SPACE 1                                                                
         LA    RE,HNWBUFF          SAVE A(CNET/MKT/CSYS TABLE)                  
         AHI   RE,L'HNWBUFF                                                     
         ST    RE,AMKTTABL                                                      
         SPACE 1                                                                
         AHI   RE,L'MKTTABL        SAVE A(INTERNET/NEW MEDIA TABLE)             
         ST    RE,AMEDTABL                                                      
         SPACE 1                                                                
         AHI   RE,L'MEDTABL        SAVE A(INPUTTED VERSIONS (VRE/VNR))          
         ST    RE,APAYVERS                                                      
         SPACE 1                                                                
         AHI   RE,L'PAYVERS        SAVE A(VERSIONS TO PAY (VRE/VNR))            
         ST    RE,ACSTVERS                                                      
         SPACE 1                                                                
         TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         BZ    *+8                 INPUT IS ALREADY PRESENT                     
         NI    SCRSTAT,X'FF'-SCRCHG                                             
         EJECT                                                                  
*              MAIN PROCESSING CONTROL                                          
         SPACE 2                                                                
         BRAS  RE,INIT             INITIALIZE                                   
         SPACE 1                                                                
         BAS   RE,VSCREEN          HANDLE BASE SCREEN                           
         SPACE 1                                                                
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         BE    ABORTPFK            A WEB TRANSACATION, ABORT NOW                
         SPACE 1                                                                
         OC    SVTAUHEL,SVTAUHEL   IF HAVEN'T SAVED THIS TAUHEL YET             
         BNZ   *+10                                                             
         MVC   SVTAUHEL,TCTAUHEL   SAVE COMM'L TAUHEL IN CASE VERSIONS          
         SPACE 1                                                                
         OC    SVTCUNIT(3),SVTCUNIT  IF HAVEN'T SAVED UNITS/MAJORS YET          
         BNZ   *+10                                                             
         MVC   SVTCUNIT(3),TCUNITS   SAVE COMM'L INFO IN CASE VERSIONS          
         SPACE 1                                                                
         TM    LCLSTAT7,SNW0UNIT     IF PAYING SNW UPGRADE FOR 0 UNITS          
         BZ    *+10                                                             
         XC    SVTCUNIT,SVTCUNIT     SAVED UNITS MUST BE ZERO                   
         SPACE 1                                                                
         TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         BO    MN10                ALWAYS PAY ALL CAST AT ONCE                  
         SPACE                                                                  
         TM    PAYSTAT1,DETAIL     IF DISPLAY DETAILS REQUESTED                 
         BO    *+12                                                             
         TM    PAYMODE,DTLONBSE    OR IF DETAIL ON BASE SCREEN                  
         BZ    MN10                                                             
         BAS   RE,DTLCNTL          GO TO DETAIL PAYMENT CONTROL                 
         SPACE 1                                                                
         TM    LCLSTAT,FINISHED    TEST NOT FINISHED YET                        
         BO    MNX                                                              
         SPACE 1                                                                
MN10     BAS   RE,ALLCNTL          PAY ALL(/REMAINING) CAST                     
         SPACE 1                                                                
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         BE    ABORTPFK            A WEB TRANSACATION, ABORT NOW                
         SPACE 1                                                                
MNX      MVC   SAVTGROS,TGROSS     PRESERVE TGROSS BEFORE ACTRA FUND            
         BAS   RE,COMPLETE         COMPLETE THE PAYMENT                         
         SPACE 1                                                                
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         BE    ABORTPFK            A WEB TRANSACATION, ABORT NOW                
         SPACE 1                                                                
         GOTO1 PAYDONE             FINISH UP                                    
         SPACE 1                                                                
         TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         BO    XIT                 RETURN TO TAGENFA NOW                        
         DC    H'0'                ELSE, SHOULD NEVER RET FROM PAYDONE          
         EJECT                                                                  
*              ROUTINES TO HANDLE BASE SCREEN                                   
         SPACE 1                                                                
VSCREEN  NTR1                                                                   
         TM    AGYSTAT,TAAYSCOD    IF COD AGENCY                                
         BZ    *+14                                                             
         MVC   CONHED2(12),CODMSG  DISPLAY IT                                   
         B     VSC1                                                             
         TM    CLTSTAT,TACISCOD    IF COD CLIENT                                
         BZ    *+10                                                             
         MVC   CONHED2(12),CODMSG2 DISPLAY IT                                   
         SPACE                                                                  
VSC1     TM    PAYSTAT1,CASTSELD   IF CAST HAS BEEN SELECTED                    
         BZ    VSC2                                                             
         SPACE 1                                                                
         CLI   PFAID,24            TEST ABORT PFKEY PRESSED                     
         BNE   *+12                                                             
         BAS   RE,RESTSCRN         RESTORE SCREEN FOR NEXT PAYMENT              
         B     ABORTPFK                                                         
         SPACE                                                                  
         OI    CONRECH+1,X'20'     PROTECT REC/ACT                              
         NI    CONRECH+6,X'FE'     TURN OFF MODIFIED                            
         OI    CONRECH+6,X'80'     (00 UNPROTECTS EACH TIME IN CASE             
         OI    CONACTH+1,X'20'      GLOBAL PF KEY HIT)                          
         NI    CONACTH+6,X'FE'                                                  
         OI    CONACTH+6,X'80'                                                  
         GOTO1 FLDVAL,DMCB,(X'08',PAYAGYH),PAYLFTH  PROTECT KEY FIELDS          
         B     VSC3                                                             
         SPACE 1                                                                
VSC2     L     R2,EFHREC                                                        
         GOTO1 USEVAL,DMCB,(X'40',8(R2))  VALIDATE USE CODE ONLY                
         BNE   FLDINV                                                           
         SPACE 1                                                                
         BRAS  RE,SETHEADS         FILL IN HEADERS                              
         SPACE 1                                                                
VSC3     CLI   TGUSEQU,UGRT        IF GRT PAYMENT                               
         BE    VSC3A                                                            
         CLI   TGUSEQU,UPNH        OR PNH PAYMENT                               
         BNE   VSC3D                                                            
VSC3A    GOTO1 TSTLCKT,DMCB,=C'TAL_CHECKS'                                      
         BE    VSC3C               AND CHECK LOCKOUT IS ACTIVE                  
         GOTO1 TSTLCKT,DMCB,=C'TAL_PRCHKS'                                      
         BE    VSC3C               OR PRINT CHECK LOCKOUT IS ACTIVE             
         GOTO1 TSTLCKT,DMCB,=C'TAL_P+CHKS'                                      
         BNE   VSC3D               OR P+ CHECKS LOCKOUT IS ACTIVE               
VSC3C    BAS   RE,RESTSCRN         RESTORE SCREEN                               
         B     GRTLOCK             PAYMENT NOT ALLOWED                          
         SPACE 1                                                                
VSC3D    NI    LCLSTAT,ALL-TOPOK                                                
         MVI   KFLDCHA,0           CLEAR KEY FIELD CHANGED STATUS               
         SPACE 1                                                                
         GOTO1 VALAGY              VALIDATE AGENCY                              
         SPACE 1                                                                
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         BE    XIT                 A WEB TRANSACTION, EXIT NOW                  
         SPACE 1                                                                
         CLI   TGUSEQU,UEVE        UNLESS MAKING EVENT PAYMENT                  
         BE    VSC4                                                             
         CLI   PAYCIDH+5,0         IF COMMERCIAL INPUT                          
         BNE   *+14                                                             
         OC    TGCID,TGCID         OR THERE'S A GLOBAL COMMERCIAL               
         BZ    VSC4                                                             
         GOTO1 VALCOM              VALIDATE COMMERCIAL FIRST                    
         TM    LCLSTAT8,VGCONCHG   GCON CHANGED DURING PROCESSING?              
         BO    ERFROM50                                                         
         SPACE 1                                                                
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         BE    XIT                 A WEB TRANSACTION, EXIT NOW                  
         SPACE 1                                                                
         MVI   MYMODE,XVALCOM                                                   
         BAS   RE,GOOVLY           GIVE OVERLAY A SHOT                          
         GOTO1 VALINV              VALIDATE INVOICE NUMBER                      
         TM    LCLSTAT8,VINVNFND+VINVPAID                                       
         BNZ   ERFROM50                                                         
         B     VSC6                                                             
         SPACE 1                                                                
VSC4     GOTO1 VALINV              VALIDATE INVOICE NUMBER FIRST                
         TM    LCLSTAT8,VINVNFND+VINVPAID                                       
         BNZ   ERFROM50                                                         
         GOTO1 VALCOM              VALIDATE COMMERCIAL                          
         TM    LCLSTAT8,VGCONCHG   GCON CHANGED DURING PROCESSING?              
         BO    ERFROM50                                                         
         MVI   MYMODE,XVALCOM                                                   
         BAS   RE,GOOVLY           GIVE OVERLAY A SHOT                          
         SPACE 1                                                                
VSC6     TM    PAYMODE,DTLDSPLD    ONLY IF DETAIL NOT DISPLAYED YET             
         BO    VSC6A                                                            
         TM    PAYMODE,DRAFT       AND IF NOT A DRAFT PAYMENT                   
         BO    VSC6A                                                            
         CLI   PAYINVH+5,0         AND THERE'S INV INPUT (TGINV=INPUT)          
         BE    VSC6A                                                            
         BRAS  RE,CHKPHIST         CHECK NO PAYMENT HISTORY                     
         BE    VSC6A                                                            
         TM    PAYSTAT1,CASTSELD   ELSE IF CAST HAS BEEN SELECTED               
         BZ    *+8                                                              
         BAS   RE,RESTSCRN         NEED TO RESTORE SCREEN                       
         B     BADINVER            GIVE ERROR                                   
         SPACE 1                                                                
VSC6A    OI    LCLSTAT,TOPOK       WE'VE VALIDATED TOP KEY FIELDS               
         SPACE 1                                                                
         TM    SCRSTAT,SCRCHG      IF FIRST TIME FOR THIS SCREEN                
         BO    ENTERFLD            GIVE USER A CHANCE BEFORE CONTINUING         
         SPACE 1                                                                
         BRAS  RE,TESTADV          TEST IF PAY ADVICE LINK                      
         SPACE 1                                                                
         MVI   MYMODE,VALISCRN     HAVE OVERLAY VALIDATE ITS SCREEN             
         BAS   RE,GOOVLY           (REQ'D FOR CYCLE DATE VALIDATION)            
         SPACE 1                                                                
         GOTO1 USEVAL,DMCB,CONREC,TYPE  VALIDATE USE CODE AND TYPE              
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         BRAS  RE,SAVEINPS         SAVE INPUTTED PAY VARIABLES                  
         SPACE 1                                                                
         GOTO1 VALMID,DMCB,0       VALIDATE MIDDLE FIELDS                       
         SPACE 1                                                                
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         BE    XIT                 A WEB TRANSACTION, EXIT NOW                  
         SPACE 1                                                                
         BRAS  RE,CNTWBERS         R0 = # OF WEB ERRORS BEFORE XVALMID          
         SPACE 1                                                                
         MVI   MYMODE,XVALMID                                                   
         BAS   RE,GOOVLY           GIVE OVERLAY ANOTHER SHOT                    
         SPACE 1                                                                
         LR    R2,R0               R2 = # OF WEB ERRORS BEFORE XVALMID          
         BRAS  RE,CNTWBERS         R0 = # OF WEB ERRORS AFTER XVALMID           
         CR    R2,R0               IF NOT THE SAME, SET USE DETAILS             
         JE    *+8                 ERROR ENCOUNTERED STATUS                     
         OI    TGFASTAT,TGERUSDT                                                
         SPACE 1                                                                
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         BE    XIT                 A WEB TRANSACTION, EXIT NOW                  
         SPACE 1                                                                
         BRAS  RE,SAVEINPS                                                      
         SPACE 1                                                                
         OC    DUEDATE,DUEDATE     IF WE DON'T HAVE DUE DATE                    
         BNZ   VSC6B                                                            
         GOTO1 GETDUE              GET IT                                       
         SPACE                                                                  
VSC6B    LH    R2,DSPDUE           SET TO DISPLAY DUE DATE                      
         AR    R2,RA                                                            
         TM    PAYMODE,DTLONSCR                                                 
         BZ    *+8                                                              
         L     R2,ADTLDUEH         DIFF. AREA IF DETAIL SCREEN LOADED           
         GOTO1 DATCON,DMCB,(1,DUEDATE),(8,13(R2))  DISPLAY IT                   
         NI    1(R2),X'FB'                                                      
         OI    6(R2),X'80'                                                      
         SPACE                                                                  
         CLC   PREVDUE,DUEDATE                                                  
         BNH   VSC8                                                             
         CLC   PREVDUE,TGNXTBUS                                                 
         BNH   VSC8                                                             
         TM    LCLSTAT3,OVCLADER                                                
         BO    VSC8                                                             
         TM    LCLSTAT3,CLADUEER                                                
         BO    VSC7                                                             
         OI    LCLSTAT3,CLADUEER                                                
         B     CLADUEPF                                                         
VSC7     CLI   PFAID,20                                                         
         BNE   CLADUEPF                                                         
         OI    LCLSTAT3,OVCLADER                                                
         NI    LCLSTAT3,ALL-CLADUEER                                            
VSC8     B     XIT                                                              
                                                                                
ERFROM50 BAS   RE,RESTSCRN                                                      
         TM    LCLSTAT8,VINVNFND                                                
         BO    INVNFND                                                          
         TM    LCLSTAT8,VINVPAID                                                
         BO    INVPAID                                                          
         TM    LCLSTAT8,VGCONCHG                                                
         BO    GCONCHG                                                          
         DC    H'00'                                                            
         EJECT                                                                  
*              ROUTINE TO CONTROL PAYING ENTIRE CAST AT ONCE                    
         SPACE 1                                                                
ALLCNTL  NTR1                                                                   
         ZIC   R1,LPAGE            LAST SCREEN PAGE WRITTEN SO FAR              
         LA    R1,1(R1)            + 1                                          
         STC   R1,FPAGE            = PAGE OF 1ST NON-CAST SELECT SCRN           
         SPACE                                                                  
         USING TLSCD,R3                                                         
         LA    R3,KEY              BUILD KEY                                    
         XC    TLSCKEY,TLSCKEY                                                  
         MVI   TLSCCD,TLSCCDQ      SCREEN RECORD CODE                           
         MVC   TLSCAGY,TGAGY       AGENCY                                       
         MVC   TLSCINV,SCRINVNO    INVOICE NUMBER                               
         MVC   TLSCPG,FPAGE        PAGE NUMBER                                  
         MVC   TLSCSCR,TWASCR      SCREEN NUMBER                                
         MVI   RDUPDATE,C'Y'       READ IT FOR UPDATE                           
         GOTO1 HIGH                TO PREVENT DEADLY EMBRACE                    
         SPACE                                                                  
         TM    PAYSTAT1,CASTSELD   IF CAST HAS BEEN SELECTED                    
         BZ    ALLC10                                                           
         NI    CONRECH+1,X'DF'     UNPROTECT REC/ACT                            
         OI    CONRECH+6,X'80'                                                  
         NI    CONACTH+1,X'DF'                                                  
         OI    CONACTH+6,X'80'                                                  
         GOTO1 FLDVAL,DMCB,(X'04',PAYAGYH),PAYLFTH UNPROTECT KEY FIELDS         
         SPACE                                                                  
ALLC10   BRAS  RE,RESETINP         RESET INPUTTED PAY VARIABLES                 
         SPACE                                                                  
         GOTO1 GETCAST,DMCB,0      GET (1ST/)NEXT CAST MEMBER                   
         BNE   ALLC15                                                           
         SPACE                                                                  
         TM    LCLSTAT3,W4LOCKED   W4 RECORD CANNOT BE LOCKED                   
         BNO   ALLC11                                                           
         BRAS  RE,W4LCK                                                         
         BNE   DOABEND                                                          
         SPACE                                                                  
ALLC11   TM    LCLSTAT9,CRPLCKED   W4 CORP CANNOT BE LOCKED                     
         BNO   ALLC13                                                           
         BRAS  RE,CRPLCK                                                        
         BNE   DOABEND                                                          
         SPACE                                                                  
ALLC13   BRAS  RE,CHKDERRS         CHECK FOR DEALER ERRORS                      
         JNE   DOABEND                                                          
         SPACE 1                                                                
         BRAS  RE,CHKGERRS         CHECK FOR GUARANTEE ERRORS                   
         JNE   DOABEND                                                          
         SPACE 1                                                                
         MVC   AIO,AIO2            DON'T CREAM CAST RECORD IN AIO1              
         BAS   RE,GETINV           GET INVOICE NUMBER IF NECESSARY              
         SPACE                                                                  
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         BE    XIT                 A WEB TRANSACATION, ABORT NOW                
         SPACE                                                                  
         MVC   AIO,AIO1                                                         
         TM    PAYMODE,DRAFT       IF NOT DRAFT                                 
         BO    *+8                                                              
         OI    TRNSTAT,PYINPROG    SET PAY UPDATE IN PROGRESS                   
         SPACE                                                                  
         BAS   RE,RATECALC         CALCULATE RATES                              
         SPACE 1                                                                
         TM    TCRTRN,TCRTDLR      IF PAYMENT NOT COVERED BY DEALER             
         BO    *+8                                                              
         OI    LCLSTAT6,NTDLRCVR   SET AT LEAST 1 UNCOVERED PERFORMER           
         SPACE                                                                  
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         BE    XIT                 A WEB TRANSACATION, ABORT NOW                
         SPACE                                                                  
         BRAS  RE,CSTMTCH          ENSURE CAST-LEVEL PAYMENT MATCHES            
         BNE   XIT                 CERNO'S EXPECTATIONS                         
         SPACE                                                                  
         CLI   TCERROR,0           TEST NO ERRORS                               
         BNE   ALLC10                                                           
         BAS   RE,PROCCHK          PROCESS CHECK RECORD                         
         B     ALLC10              GET NEXT CAST MEMBER                         
         SPACE                                                                  
ALLC15   BRAS  RE,TOTMTCH          ENSURE TOTAL PAYMENT MATCHES                 
         BNE   XIT                 WEB APPLICATION'S EXPECTATIONS               
         SPACE                                                                  
         TM    LCLSTAT,PERFPAID    ENSURE SOMEONE IS ELIGIBLE FOR               
         BZ    NONEELIG            PAYMENT                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL PAYING WITH DETAIL OPTION                     
         SPACE 1                                                                
DTLCNTL  NTR1                                                                   
         MVC   GCWRKBAL,GCBAL                                                   
         TM    PAYSTAT1,DETAIL     IF DISPLAY DETAILS REQUESTED                 
         BZ    *+16                                                             
         TM    PAYMODE,DTLONSCR    AND DETAIL SCREEN HASN'T BEEN LOADED         
         BO    *+8                                                              
         BAS   RE,LOADDTL          LOAD IT NOW                                  
         SPACE 1                                                                
         OI    CONRECH+1,X'20'     PROTECT REC/ACT                              
         NI    CONRECH+6,X'FE'     TURN OFF MODIFIED                            
         OI    CONRECH+6,X'80'     (00 UNPROTECTS EACH TIME IN CASE             
         OI    CONACTH+1,X'20'      GLOBAL PF KEY HIT)                          
         NI    CONACTH+6,X'FE'                                                  
         OI    CONACTH+6,X'80'                                                  
         SPACE 1                                                                
         TM    LCLSTAT,DTLFIRST    TEST FIRST TIME IN                           
         BO    DTLC10              NOTHING TO VALIDATE YET-DISP 1ST PG          
         SPACE 1                                                                
DTLC5    L     R2,ADTLFSTH         R2=A(FIRST DETAIL LINE)                      
         SPACE 1                                                                
         GOTO1 FLDVAL,DMCB,(X'40',(R2)),ADTLLSTH  IF ALL FIELDS VALID           
         BE    DTLC8                              SKIP VALIDATION               
         OI    PAYMODE,VALSCRN     SET TO ONLY VALIDATE FIELDS                  
         GOTO1 VALCAST,DMCB,(R2)   VALIDATE USER INPUT                          
         B     CHGDMSG             USER MUST'VE CHG'D SOMETHING-RETURN          
         SPACE 1                                                                
DTLC8    BAS   RE,SCRNCTL          HANDLE SCREEN RECORDS / PAGING               
         SPACE 1                                                                
         TM    PAYMODE,DRAFT       IF THIS IS A DRAFT PAYMENT                   
         BZ    DTLC10                                                           
         NI    PAYMODE,ALL-VALSCRN SET TO DO IT FOR REAL THIS TIME              
         GOTO1 VALCAST,DMCB,(R2)   SIMULATE ADDING CHECK RECORDS NOW            
         SPACE 1                                                                
DTLC10   TM    LCLSTAT,FINISHED    IF WE'RE NOT FINISHED YET                    
         BO    DTLC30                                                           
         TM    PAYMODE,DTLONBSE    TEST DETAIL ON BASE SCREEN                   
         BO    *+12                                                             
         TM    PAYSTAT1,DETAIL     OR IF DETAIL (STILL) REQUIRED                
         BZ    DTLC30                                                           
         BAS   RE,DISPLAY          DISPLAY (FIRST/)NEXT SCREEN                  
         BE    DISPMSG             CAST DISPLAYED                               
         SPACE 3                                                                
*                                  *** FINISHED DISPLAYING CAST ***             
         SPACE 1                                                                
DTLC30   BAS   RE,PAYCAST          PAY ENTIRE CAST BASED ON SAVED SCRNS         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY CAST                                          
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         BRAS  RE,CLRSCRN          CLEAR SCREEN                                 
         SPACE 1                                                                
         XR    R2,R2               R2=N'CAST ON THIS SCREEN                     
         L     R3,ADTLFSTH         R3=A(FIRST LINE)                             
         LA    R4,PAGETBL          R4=A(SAVED CAST SORT KEYS AREA)              
         SPACE 1                                                                
         MVI   DMCB,X'80'          FORCE GETCAST TO READ HIGH 1ST TIME          
         B     *+8                                                              
DISP2    MVI   DMCB,0              CLEAR PARAMETER TO GETCAST                   
         SPACE 1                                                                
         GOTO1 GETCAST,DMCB        GET (1ST/)NEXT CAST MEMBER                   
         BE    DISP4                                                            
         TM    LCLSTAT,DTLFIRST    IF THIS IS FIRST TIME FOR DETAIL             
         BZ    YES                                                              
         LTR   R2,R2               AND NO ONE WAS DISPLAYED                     
         BNZ   YES                                                              
         B     NO                  RETURN CC NE                                 
         SPACE 1                                                                
DISP4    L     RF,ADTLLSTH         TEST PAST LAST LINE                          
         CR    R3,RF                                                            
         BH    YES                 YES - RETURN CC EQ                           
         SPACE 1                                                                
         CLI   TGUSEQU,UGRT        GRT PAYMENT?                                 
         JE    *+12                                                             
         CLI   TGUSEQU,UPNH        PNH PAYMENT?                                 
         JNE   DISP4A                                                           
         LA    RF,DTLFRSTX-DTLFRSTH(R3)  A(EXT HDR OF 1ST FLD FOR CAST)         
         MVI   1(RF),0                                                          
         CLC   TGUNI,=C'SAG'             SET CAST MEMBER'S UNION                
         JE    *+14                      ONLY IF SAG OR AFT                     
         CLC   TGUNI,=C'AFT'                                                    
         JNE   *+8                                                              
         OI    1(RF),X'80'                                                      
         SPACE 1                                                                
DISP4A   TM    LCLSTAT3,W4LOCKED   W4 RECORD CANNOT BE LOCKED                   
         BNO   DISP4B                                                           
         BRAS  RE,W4LCK                                                         
         BNE   DOABEND                                                          
         SPACE 1                                                                
DISP4B   TM    LCLSTAT9,CRPLCKED   W4 CORP CANNOT BE LOCKED                     
         BNO   DISP5                                                            
         BRAS  RE,CRPLCK                                                        
         BNE   DOABEND                                                          
         SPACE                                                                  
DISP5    BRAS  RE,CHKDERRS         CHECK FOR DEALER ERRORS                      
         JNE   DOABEND                                                          
         SPACE 1                                                                
         BRAS  RE,CHKGERRS         CHECK FOR GUARANTEE ERRORS                   
         JNE   DOABEND                                                          
         SPACE 1                                                                
         TM    LCLSTAT9,EVEMSCAN   TEST NO CANADIAN TAXES FOR                   
         BO    DISPERRC            EVE PAYMENT ERROR                            
         TM    LCLSTAT2,GUARERR    TEST NO GUARANTEE ERROR                      
         BO    DISPERRG                                                         
         TM    LCLSTAT4,NOAGT      TEST NOT MISSING AGENT                       
         BO    DISPERRA                                                         
         ST    R3,ATHSLINE         SAVE A(THIS LINE)                            
         SPACE 1                                                                
         BRAS  RE,UNPROTMD         UNPROTECT MISC. DED FIELD                    
         SPACE 1                                                                
         BRAS  RE,PROTRET          PROTECT FIELDS FOR RETROS                    
         SPACE 1                                                                
         LH    R1,DSPAPPLI         SET DISP. OF 1ST FLD TO PROTECT              
**NO-OP  TM    LCLSTAT3,SOAPRES    IF SOAP RESIDUAL TO BE APPLIED CODE          
**JUN09  BO    DISP6                                                            
         LH    R1,DSPREIMI         ELSE SET DISP TO REIMB EXP INDICATOR         
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF TYPE IS CLIENT              
         BO    DISP6                                                            
         CLI   TGUSMEDS,PRINT      OR IF PRINT PAYMENT                          
         BNE   DISP7                                                            
         OC    ELTADL,ELTADL       AND THERE'S NO DEAL ELEMENT                  
         BNZ   DISP7B                                                           
DISP6    BRAS  RE,PROTDTL          PROTECT PART OF DETAIL LINE                  
         B     DISP7B                                                           
         SPACE 1                                                                
DISP7    BRAS  RE,PROTMDED         PROTECT MISC. DED FIELD IF NEEDED            
         SPACE 1                                                                
DISP7B   TM    CASTSTAT,CSTNPAY    IF NOT PAYING THIS PERSON                    
         BZ    DISP10                                                           
         CLC   TCCAONOF(2),=C'ON'  AND HE'S OFF CAMERA                          
         BE    DISP8                                                            
         XC    TCPAY,TCPAY         SET PAYMENT = 0                              
         OI    TCINPUT,TCINPAY     SET HAVE PAYMENT AMOUNT                      
         B     DISP10                                                           
         SPACE                                                                  
DISP8    CLI   DSPNP,X'FF'         IF ON CAMERA AND HAVE NP FIELD               
         BE    DISP10                                                           
         LH    R1,DSPNP                                                         
         AR    R1,R3                                                            
         MVI   8(R1),C'Y'          MOVE Y INTO NP FIELD                         
         B     DISP12              DON'T BOTHER CALCULATING RATES               
         SPACE 1                                                                
DISP10   BAS   RE,SETNUSES         SET NUMBER OF USES PAID PREVIOUSLY           
         GOTO1 ARATECLC,DMCB,(RC),TCD,SYSCOMM  CALCULATE RATES                  
******** BRAS  RE,RETSPNH          GET RETRO SUBJ P&H IF NECESSARY              
         BRAS  RE,RESETINP         RESET INPUTTED PAY VARIABLES                 
         SPACE 1                                                                
         BRAS  RE,CHKMINOR         CHECK THAT MINOR IS SET UP                   
         BNE   DOABEND             APPROPORATELY FOR P+                         
         SPACE                                                                  
         TM    LCLSTAT3,SOAPRES    IF SOAP RESIDUAL                             
         BZ    DISP12                                                           
         MVI   MYTSACTN,TSAPUT     WRITE BACK TSAR RECORD                       
         BRAS  RE,CALLTSAR                                                      
         BNE   TOOBIG                                                           
         SPACE 1                                                                
DISP12   MVI   MYMODE,DISPDTL                                                   
         BAS   RE,GOOVLY           DISPLAY DETAIL                               
         SPACE 1                                                                
         BRAS  RE,DISOPT           DISPLAY OPTIONAL INFORMATION                 
         SPACE 1                                                                
         GOTO1 DISRHS              DISPLAY CALCULATED AMOUNTS                   
         SPACE 1                                                                
         USING PAGED,R4                                                         
         PACK  DUB,TGSSN           SAVE S/S NUMBER IN BINARY                    
         CVB   R1,DUB                                                           
         ST    R1,PAGESSN                                                       
         MVC   PAGESKEY,TGCSORT    THIS SORT KEY                                
         SPACE                                                                  
         MVC   PAGECAUH,ACAUHREC   A(CAST UH REC)                               
         MVC   PAGELOCL,TGLCL      RESIDENCE LOCAL                              
         MVC   PAGENCW,NTACWS      N'CHECK WITHHOLDING ELS. REQ'D               
         MVC   PAGEW4TY,TCW4TYPE   W4 TYPE                                      
         AHI   R2,1                BUMP CAST COUNT                              
         STC   R2,NCASTPG                                                       
         TM    TCCASTST,TCCADUES   IF NEED TO WITHHOLD DUES, SET BIT            
         BZ    *+8                                                              
         OI    PAGENCW,X'20'                                                    
         BRAS  RE,SETHWBUF         SET H&W TABLE AND FIXED RATE STATUS          
         SPACE 1                                                                
         NI    LCLSTAT,ALL-DTLFIRST TURN OFF FIRST TIME INDICATOR               
         OI    PAYMODE,DTLDSPLD     SET DETAIL DISPLAYED                        
         SPACE 1                                                                
         LA    R4,PAGENEXT                                                      
         AH    R3,DSPNEXT          BUMP TO NEXT LINE                            
         B     DISP2                                                            
         SPACE 1                                                                
DISPERRC MVC   MYMSGNO,=Y(ERRETCAN) MISSING CAN TAXES FOR CANADIAN              
         B     DISPERRS                                                         
DISPERRG MVI   MYMSGNO1,ERGRTDET   BAD GUARANTEE RECORD                         
DISPERRS XC    BLOCK+1(9),BLOCK+1                                               
         GOTO1 SSNPACK,DMCB,GRTERSSN,BLOCK+1                                    
         B     DISPERR                                                          
         SPACE 1                                                                
DISPERRA MVC   BLOCK+1(9),TGSSN    SHOW SSN                                     
         MVI   BLOCK+10,4                                                       
         MVC   BLOCK+11(3),TGCAT   AND CATEGORY                                 
         MVI   BLOCK+14,0                                                       
         MVI   MYMSGNO1,ERNOAGT    MISSING AGENT                                
         SPACE 1                                                                
DISPERR  MVI   BLOCK,10            BUILD SUBSTITUTION BLOCK FOR GETTXT          
         BAS   RE,RESTSCRN         RESTORE SCREEN FOR NEXT PAYMENT              
         B     ERRSUBS             GIVE ERROR                                   
         EJECT                                                                  
*              ROUTINE TO PAY CAST BASED ON DETAIL SCREENS                      
         SPACE 1                                                                
PAYCAST  NTR1                                                                   
         BAS   RE,RESTSCRN         RESTORE SCREEN FOR NEXT PAYMENT              
         SPACE 1                                                                
         TM    LCLSTAT,DTLFIRST    ENSURE SOMEONE IS ELIGIBLE FOR               
         BO    NONEELIG            PAYMENT                                      
         SPACE 1                                                                
         TM    PAYMODE,DTLONBSE    TEST DETAIL ON BASE SCREEN                   
         BZ    *+8                                                              
         BRAS  RE,CLRSCRN          CLEAR LOWER HALF OF SCREEN                   
         SPACE 1                                                                
         NI    PAYMODE,ALL-VALSCRN SET TO DO IT FOR REAL THIS TIME              
         SPACE 1                                                                
         TM    PAYMODE,DRAFT       DON'T BOTHER ON DRAFT PAYMENTS               
         BO    PAYCX               (DIDN'T WRITE SCREEN RECORDS)                
         SPACE 1                                                                
         L     RA,ATIA             PROCESS CHECK REC. WRITES FROM TIA           
         L     R2,ADTLFSTH         R2=A(FIRST DETAIL LINE)                      
         S     R2,ATWA             ADJUST TO POINT TO CORRESPONDING             
         AR    R2,RA               AREA IN TIA                                  
         SPACE 1                                                                
         BRAS  RE,VALGRT           VALIDATE GRT/PAY INPUT                       
         SPACE 1                                                                
         BRAS  RE,ANYACT04         IF PAYING 2404, ANALYZE CAST                 
         SPACE 1                                                                
******** BRAS  RE,ANYCON16         ANALYZE CAST FOR 2016 PERFS                  
         SPACE 1                                                                
         MVC   GPAGE,DPAGE         START AT FIRST DETAIL PAGE                   
         LA    R3,X'80'            SET TO FORCE THIS PAGE                       
         B     *+6                                                              
PAYC40   XR    R3,R3               SET TO BUMP TO NEXT PAGE                     
         SPACE 1                                                                
         GOTO1 GETSCRN,DMCB,((R3),GPAGE),(RA) RESTORE SCREEN REC TO TIA         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VALCAST,DMCB,(R2)   VALIDATE INPUT / ADD CHECK RECORDS           
         SPACE 1                                                                
         CLC   GPAGE,LPAGE         TEST REACHED LAST PAGE                       
         BL    PAYC40              NO - GET NEXT                                
         SPACE 1                                                                
         TM    LCLSTAT,PERFPAID    TEST SOMEONE PAID                            
         BZ    NONEPAID            NO - GIVE NONE PAID MESSAGE                  
PAYCX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE DETAIL CAST INPUT                            
         SPACE 1                                                                
*                                  P1=A(FIRST LINE)                             
VALCAST  NTR1                                                                   
         L     R3,0(R1)            R3=A(FIRST LINE)                             
         LA    R4,PAGETBL          R4=A(SAVED CAST SORT KEYS AREA)              
         USING PAGED,R4                                                         
         ZIC   R2,NCASTPG          R2=N'CAST ON THIS PAGE                       
         SPACE 1                                                                
VC2      XC    TCCAST(TCCSTLNQ),TCCAST  INITIALIZE INDIV.CAST AREAS             
         XC    TCCSTBRK,TCCSTBRK                                                
         XC    PYCAST(PYCSTLNQ),PYCAST                                          
         SPACE 1                                                                
         L     R1,PAGESSN          CONVERT S/S NUMBER TO EBCDIC                 
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TGSSN,DUB+3(5)      AND SAVE IN GLOBAL STORAGE                   
         MVC   TGCSORT,PAGESKEY    CAST SORT KEY                                
         SPACE                                                                  
         TM    LCLSTAT3,SOAPRES    IF USE IS A SOAP RESIDUAL                    
         BZ    *+10                                                             
         MVC   MYTSRNUM,TGCSORT+4  SAVE TSAR RECORD NUM                         
         SPACE                                                                  
         MVC   NTACWS,PAGENCW      N'CHECK WITHHOLDING ELS. REQ'D               
         NI    NTACWS,X'0F'        TURN OFF ALL HIGH ORDER BITS                 
         MVC   TCW4TYPE,PAGEW4TY   W4 TYPE                                      
         SPACE                                                                  
         TM    PAGENCW,X'20'       SET BIT IF NEED TO WITHHOLD DUES             
         BZ    *+8                                                              
         OI    TCCASTST,TCCADUES                                                
         TM    PAGENCW,X'80'       TEST CAST GETTING FIXED RATE 2X              
         BO    VC3                 IF YES, DON'T NEED TO SET ANYTHING           
         TM    PAGENCW,X'40'       ELSE TEST GETTING IT 1X                      
         BZ    *+12                                                             
         OI    TCCASTST,TCCA1XFX   IF YES, SET BIT FOR 60 TO ADD 1X             
         B     *+8                                                              
         OI    TCCASTST,TCCANOFX   ELSE SET BIT FOR 60 TO NOT ADD FIXED         
         SPACE 1                                                                
VC3      ST    R3,ATHSLINE         SAVE A(THIS LINE)                            
         MVI   MYMODE,VALIDTL                                                   
         BAS   RE,GOOVLY           VALIDATE DETAIL                              
         SPACE 1                                                                
         GOTO1 VALRHS              VALIDATE RHS                                 
         SPACE 1                                                                
         TM    PAYMODE,VALSCRN     IF ONLY VALIDATING INPUT                     
         BZ    VC8                                                              
         LH    R0,DSPFRST          SET A(FIRST APPLICABLE FLD)                  
         AR    R0,R3                                                            
         LH    RF,DSPAGT           AND A(LAST)                                  
         AR    RF,R3                                                            
         GOTO1 FLDVAL,DMCB,(R0),(X'02',(RF))  HIGHLIGHT CHANGED FIELDS          
         BNE   VC6                                                              
         L     RE,8(R1)                                                         
         SR    RE,R3               RE=DISPL INTO LINE OF 1ST CHNGD FLD          
         LH    RF,DSPMDED          SET RF TO LAST FIELD                         
         CLI   TGUSEQU,UPNH                                                     
         BNE   *+8                                                              
         LH    RF,DSPSPNH                                                       
         CR    RE,RF               TEST IF CHANGE AFFECTS PAYMENT               
         BH    VC6                                                              
         CLI   DSPNP,X'FF'         IF HAVE NP FIELD                             
         BE    VC4                                                              
         BAS   RE,VALNP            VALIDATE IT                                  
         BE    VC6                 SKIP IF NP=Y                                 
         B     VC5                                                              
VC4      BRAS  RE,ANYINPUT         SKIP IF NO INPUT AT ALL                      
         BE    VC6                                                              
VC5      BAS   RE,CALC             YES - RE-READ CAST RECORD/CALC RATES         
         BRAS  RE,RESETINP         RESET INPUTTED PAY VARIABLES                 
         GOTO1 DISRHS              RE-DISPLAY AMOUNTS                           
VC6      B     VC10                BUMP TO NEXT PERFORMER                       
         SPACE 1                                                                
*                                  *** PROCESS PAYMENT NOW ***                  
VC8      BRAS  RE,ANYINPUT         SKIP IF NO INPUT AT ALL                      
         BNE   VC8C                                                             
         CLI   TGUSEQU,UVNR        UNLESS MAKING A VNR PAYMENT                  
         BNE   VC10                                                             
         TM    PAYMODE,DRAFT                                                    
         BO    VC10                                                             
         LH    RE,DSPNP                                                         
         AR    RE,R3                                                            
         CLI   8(RE),C'Y'                                                       
         JE    VC10                                                             
         BAS   RE,CALC                                                          
         BRAS  RE,VNRNODOL         WHERE WE MAY NEED TO ADD VERSIONS            
         B     VC10                TO CAST RECORD/USAGE HISTORY                 
         SPACE 1                                                                
                                                                                
VC8C     BAS   RE,GETINV           GET INVOICE NUMBER IF NECESSARY              
         SPACE                                                                  
         TM    PAYMODE,DRAFT       IF NOT DRAFT                                 
         BO    *+8                                                              
         OI    TRNSTAT,PYINPROG    SET PAY UPDATE IN PROGRESS                   
         SPACE                                                                  
         OI    LCLSTAT,PERFPAID    SET SOMEONE PAID                             
         BAS   RE,CALC             READ CAST RECORD / CALCULATE RATES           
         SPACE 1                                                                
         CLI   TGUNEQU,ACT                IF UNION IS ACTRA                     
         BE    VC8D                                                             
         CLI   ELTACA+TACACORP-TACAD,C' ' OR IF CORP# INDICATED ON CAST         
         BNH   VC9                                                              
VC8D     GOTO1 W4DET               EXTRACT W4 DETAILS (INCL. CORP ID#)          
         SPACE 1                                                                
VC9      LH    R0,DSPFRST          SET A(FIRST APPLICABLE FLD)                  
         AR    R0,R3                                                            
         LH    RF,DSPAGT           AND A(LAST)                                  
         AR    RF,R3                                                            
         GOTO1 FLDVAL,DMCB,(R0),(X'01',(RF)),(X'80',0)  IF HIGHLIGHTED          
         BE    *+12                                                             
         OI    CASTSTAT,CSTMAN     SET MANUAL OVERRIDE FOR THIS CHECK           
         OI    LCLSTAT,MANOVER     AND FOR INVOICE                              
         BAS   RE,PROCCHK          PROCESS CHECK RECORD                         
         BRAS  RE,RESETINP         RESET INPUTTED PAY VARIABLES                 
         SPACE 2                                                                
VC10     AH    R3,DSPNEXT          BUMP TO NEXT LINE                            
         LA    R4,PAGENEXT                                                      
         BCT   R2,VC2                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TESTS IF VALID NP INPUT FOR CAST MEMBER                  
*              SETS CC EQUAL IF NP=Y                                            
*              SETS CC NOT EQUAL IF NP IS BLANK                                 
*              ELSE GIVES ERROR                                                 
         SPACE 1                                                                
*                                  R3=A(FIRST FIELD)                            
VALNP    NTR1                                                                   
         LH    R2,DSPNP                                                         
         AR    R2,R3               SET R2=A(NP FIELD HEADER)                    
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VALNP5                                                           
         TM    4(R2),X'20'         AND IF NOT VALID                             
         OI    1(R2),X'08'         SET HIGH INTENSITY                           
         NI    1(R2),X'FB'                                                      
         B     NO                  RETURN CC NOT EQUAL                          
VALNP5   CLI   8(R2),C'Y'                                                       
         BNE   FLDINV              ERROR IF INPUT NOT Y                         
         SPACE 1                                                                
         USING TACOD,RE                                                         
         LA    RE,ELTACO           IF PAYING ACTRA TYPE 2404A COMM'L            
         CLI   TACOCTYP,CCTY04A    Y IN INVALID                                 
         BE    NP04AERR                                                         
         DROP  RE                                                               
         SPACE 1                                                                
         LH    R4,DSPAGT           SET A(LAST FIELD)                            
         AR    R4,R3                                                            
         GOTO1 FLDVAL,DMCB,(X'01',(R3)),(R4) CLEAR LINE                         
         MVI   8(R2),C'Y'          PUT Y BACK IN NP FIELD                       
         MVI   5(R2),1                                                          
         OI    4(R2),X'20'         SET PREV VALIDATED                           
         B     YES                 RETURN CC EQUAL                              
         EJECT                                                                  
*              ROUTINE GETS THE INVOICE NUMBER TO PAY AND UPDATES THE           
*              NEXT INVOICE NUMBER IN THE AGENCY RECORD                         
         SPACE                                                                  
GETINV   NTR1                                                                   
         TM    PAYMODE,DRAFT       DON'T NEED INVOICE NUMBER FOR DRAFT          
         BO    XIT                                                              
         TM    LCLSTAT2,GOTINV     ONLY DO 1 TIME AT BEGINNING                  
         BO    XIT                                                              
         SPACE                                                                  
         XC    INV04A,INV04A       CLEAR CANADIAN INVOICE FOR 2404A             
         SPACE                                                                  
         USING WEBREQD,RE                                                       
         TM    TGCTSTST,TGCTSCLI   IF PAYMENT BEING MADE BY CLIENT              
         BO    GETI05                                                           
         TM    TGFASTAT,TGFROMFA   OR IF PAYMENT COMING FROM WEB                
         BZ    GETI30                                                           
         L     RE,TGAFAREQ                                                      
         OC    WBPYINV,WBPYINV     AND INVOICE NUMBER IS NOT PROVIDED           
         BNZ   GETI30                                                           
         DROP  RE                                                               
         SPACE                                                                  
GETI05   L     RA,ATWA             INSURE RA=A(TWA)                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'30',0) READ AGENCY REC FOR UPDATE         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE                                                                  
         GOTO1 CHNINV,DMCB,=X'001C',DSPLYINV  UPDATE IT                         
         BE    GETI10              BRANCH IF OK                                 
         BAS   RE,RESTSCRN                                                      
         B     AGYERR              ELSE ABORT PAYMENT FOR AGENCY ERROR          
         SPACE                                                                  
GETI10   MVC   PAYINV,DSPLYINV     DISPLAY INVOICE TO PAY                       
         OI    PAYINVH+6,X'80'                                                  
         BRAS  RE,CHKPHIST         CHECK NO PAYMENT HISTORY                     
         BE    GETI20                                                           
         OI    TRNSTAT,TRNABEND    ELSE SET BIT FOR CONTROLLER TO ABEND         
         BAS   RE,RESTSCRN                                                      
         B     BADINVER            GIVE ERROR                                   
         SPACE                                                                  
GETI20   MVC   INVNO,TGINV                                                      
         XC    INVNO,HEXFFS        SET INVNO                                    
         SPACE                                                                  
         USING TACOD,RE                                                         
GETI30   LA    RE,ELTACO                                                        
         CLI   TACOCTYP,CCTY04A    IF PAYING ACTRA TYPE 2404A COMM'L            
         BE    GETI35                                                           
         CLI   TACOCTYP,CCTY2404   OR ACTRA TYPE 2404                           
         BNE   GETI60                                                           
GETI35   TM    LCLSTAT4,NONACTON+ACTON  AND BOTH US AND CANADIAN CAST           
         BNO   GETI60                   ARE ON THIS PAYMENT                     
         CLI   TACOCTYP,CCTY2404                                                
         BE    GETI36                                                           
         TM    TGUSSTAT,SESSION    AND NOT PAYING A SESSION                     
         BO    GETI60                                                           
         TM    TGUSSTA2,HLDTYPE    OR HOLDING FEE                               
         BO    GETI60                                                           
         DROP  RE                                                               
         SPACE                                                                  
GETI36   GOTO1 RECVAL,DMCB,TLAYCDQ,(X'30',0) READ AGENCY REC FOR UPDATE         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE                                                                  
         MVC   WORK2,TGINV         SAVE US$ INVOICE                             
         SPACE                                                                  
         GOTO1 CHNINV,DMCB,=X'001C',DSPLYINV  UPDATE IT                         
         BE    GETI40              BRANCH IF OK                                 
         MVC   TGINV,WORK2                                                      
         BAS   RE,RESTSCRN                                                      
         B     AGYERR              ELSE ABORT PAYMENT FOR AGENCY ERROR          
         SPACE                                                                  
GETI40   BRAS  RE,CHKPHIST         CHECK NO PAYMENT HISTORY                     
         BE    GETI50                                                           
         OI    TRNSTAT,TRNABEND    ELSE SET BIT FOR CONTROLLER TO ABEND         
         MVC   TGINV,WORK2                                                      
         BAS   RE,RESTSCRN                                                      
         B     BADINVER            GIVE ERROR                                   
         SPACE                                                                  
GETI50   MVC   INV04A,TGINV        SET INV NUMBER FOR CANADIANS                 
         MVC   TGINV04A,TGINV      SAVE GLOBAL 2404A CANADIAN INVOICE           
         MVC   TGINV,WORK2         RESTORE US$ INVOICE                          
         SPACE                                                                  
GETI60   OI    LCLSTAT2,GOTINV     SET ALREADY GOT INVOICE NUMBER               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE RESTORE THE SCREEN FOR THE NEXT PAYMENT                  
         SPACE                                                                  
RESTSCRN NTR1                                                                   
*                                            UNPROTECT ALL INPUT FIELDS         
         GOTO1 FLDVAL,DMCB,(X'04',CONRECH),ADTLLSTH                             
         SPACE 1                                                                
         CLI   TGUSEQU,UEVE                                                     
         BNE   RS10                                                             
         GOTO1 FLDVAL,DMCB,(8,PAYCIDH),PAYLFTH                                  
         LH    R2,DSPPD                                                         
         LTR   R2,R2                                                            
         JNZ   *+6                                                              
         DC    H'00'                                                            
         A     R2,ATWA                                                          
         OI    1(R2),X'20'                                                      
         LH    R2,DSPCYC                                                        
         LTR   R2,R2                                                            
         JNZ   *+6                                                              
         DC    H'00'                                                            
         A     R2,ATWA                                                          
         OI    1(R2),X'20'                                                      
         SPACE 1                                                                
RS10     NI    PAYMODE,ALL-DTLDSPLD TURN OFF DETAIL DISPLAYED INDICATOR         
         SPACE 1                                                                
         TM    PAYMODE,DTLONSCR    IF DETAIL SCREEN LOADED                      
         BZ    *+12                                                             
         BAS   RE,RESTLWR          RESTORE LOWER HALF OF SCREEN                 
         B     XIT                                                              
         TM    PAYMODE,DTLONBSE    TEST DETAIL ON BASE SCREEN                   
         BZ    XIT                                                              
         SPACE 1                   CLEAR/VALIDATE/SET NORM INT FOR ALL          
         GOTO1 FLDVAL,DMCB,(X'21',ADTLFSTH),(X'10',ADTLLSTH)                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO READ CAST RECORD AND CALCULATE RATES                  
         SPACE 1                                                                
         USING PAGED,R4            R4=A(ENTRY IN PAGETBL FOR THIS CAST)         
CALC     NTR1                                                                   
         L     RA,ATWA             INSURE RA=A(TWA)                             
         SPACE 1                                                                
         TM    LCLSTAT3,SOAPRES    IF USE IS A SOAP RESIDUAL                    
         BZ    CLC30                                                            
         MVI   MYTSACTN,TSAGET                                                  
         BRAS  RE,CALLTSAR         GET TSAR REC, VALCAST SET MYTSRNUM           
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE                                                                  
         USING ECASTABD,R3                                                      
         L     R3,TCATSAR          R3=A(TSAR REC)                               
         TM    ECSTSTAT,ECSTS2ND   IF CAST FROM 2ND COMM'L                      
         BZ    *+10                                                             
         MVC   TGCOM,COM2          SET TGCOM OF 2ND COMM'L                      
         ZIC   RF,ECSTNEPI                                                      
         STC   RF,TCNEPIS          SET NUMBER OF EPISODES FOR RATECALC          
         BCTR  RF,0                NUMBER OF EPISODES - 1                       
         MH    RF,=AL2(EPINUMLN)   * L'EACH ENTRY=DISP. TO LAST ENTRY           
         LA    R3,ECSTEPIS(RF)     ADD DISPLACEMENT TO FIRST ENTRY              
         USING EPISD,R3                                                         
         LH    R1,EPINUM           GET LAST EPISODE NUM                         
         CVD   R1,DUB                                                           
         UNPK  TGEPI,DUB+5(3)      SET TGEPI FOR RECVAL                         
         OI    TGEPI+4,X'F0'                                                    
         XC    TGINV,TGINV         CLEAR TGINV FOR RECVAL                       
         SPACE                                                                  
         GOTO1 RECVAL,DMCB,TLECCCDQ,0  READ PASSIVE ECAST KEY                   
         BE    CLC20                                                            
         LA    R3,KEY              R3=A(KEY)                                    
         USING TLECPD,R3                                                        
         CLC   TGCAT,=C'W  '       IF KEY NOT FOUND, AND IF WRITER CAT          
         BNE   CLC10                                                            
         CLI   TLECCCAT,C'W'       TAKE ANY CATEGORY THAT STARTS WITH W         
         BNE   MISSCAST                                                         
         CLC   TLECCCAT(2),=C'W2'  BUT NOT W2                                   
         BE    MISSCAST                                                         
         B     CLC20                                                            
CLC10    CLC   TGCAT,=C'W2 '                                                    
         BNE   MISSCAST                                                         
         CLC   TLECCCAT(2),=C'W2'  TAKE ANY CAT THAT STARTS WITH W2             
         BNE   MISSCAST                                                         
CLC20    GOTO1 GETREC              GET ECAST RECORD                             
         MVC   TGINV,INVNO         MOVE INVOICE BACK TO GLOBAL STORAGE          
         XC    TGINV,HEXFFS        AND COMPLEMENT IT                            
         MVC   TGCOM,SVTGCOM       RESTORE TGCOM                                
         B     CLC40                                                            
         SPACE 1                                                                
CLC30    GOTO1 RECVAL,DMCB,TLCACDQ,(X'A0',0) GET CAST RECORD                    
         BE    CLC40                                                            
         BRAS  RE,FNDCSEQ          IF NOT FOUND, TRY TO FIND BY CAST            
         BNE   MISSCAST            SEQUENCE NUMBER                              
         BRAS  RE,CRDCAST          ADJUST TO CREDITED CAST STATUS               
         SPACE 1                                                                
CLC40    MVC   SVSVCKEY,SVCASTKY   SAVE SAVED CAST KEY                          
         XC    SVCASTKY,SVCASTKY   CLEAR IT SO CASTDET WON'T RE-READ            
         SPACE 1                                                                
         GOTO1 CASTDET             EXTRACT CAST DETAILS                         
         SPACE 1                                                                
         CLI   TGUSEQU,UEVE        IF MAKING AN EVENT PAYMENT                   
         BNE   CLC45                                                            
         GOTO1 W4DET               EXTRACT W4 DETAILS                           
         SPACE 1                                                                
CLC45    GOTO1 GETCAUH             SET CAST USAGE HISTORY                       
         SPACE                                                                  
         CLI   TGUNEQU,ACT            IF UNION ACT, TURN OFF INPUT BITS         
         BNE   CLC50                  SO SYSCALC WILL RECALCULATE               
         NI    TCINPUT2,ALL-TCINMDED                                            
         XC    TCMDED,TCMDED          AND CLEAR TCMDED IN CASE 0                
         NI    TCINPUT2,ALL-TCINDUES                                            
         XC    TCDUES,TCDUES          AND CLEAR TCDUES IN CASE 0                
         SPACE                                                                  
CLC50    MVC   TGLCL,PAGELOCL      SET LOCAL AFTER CASTDET FOR RATECLC          
         BRAS  RE,SETPNH           SET TCPENS AND TCHLTH FOR UNION WGA          
         SPACE 1                                                                
         MVC   SVCASTKY,SVSVCKEY   RESTORE FOR DISPLAY IN CASE DRAFT            
         SPACE 1                                                                
         USING TLUHD,R3                                                         
CLC60    XC    TCTAUHEL,TCTAUHEL   DO NOT CLEAR CSYS COUNT                      
         OC    PAGECAUH,PAGECAUH   IF HAVE A(CAST UH REC)                       
         BZ    CLC70                                                            
         XC    KEY,KEY                                                          
         MVC   KEY+(TLDRDA-TLDRD)(4),PAGECAUH  SET D/A IN KEY                   
         MVC   AIO,AIO2            DON'T CREAM CAST REC IN AIO1                 
         L     R3,AIO                                                           
         GOTO1 GETREC              GET RECORD                                   
         CLI   DMCB+8,0            TEST NO ERROR CONDITIONS SET                 
         BNE   NOCASTUH                                                         
         TM    TLUHSTAT,X'80'      TEST NOT DELETED (ERROR NOT SET)             
         BO    NOCASTUH                                                         
         MVI   ELCODE,TAUHELQ                                                   
         LR    R4,R3                                                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TCTAUHEL(0),0(R4)   SAVE TAUHEL                                  
         SPACE 1                                                                
         GOTO1 LCBUHFX             OLD FORMAT LCB UH REQUIRES FIX               
         GOTO1 LCB2CBL             CBL/SCB UPGRADE TO LCB REQUIRES FIX          
         MVC   AIO,AIO1            RESET AIO                                    
         SPACE 1                                                                
         USING TAUHD,R3                                                         
CLC70    TM    PAYOPTS2,OPRVTUS    IF TU OPTION INPUT                           
         BZ    CLC80                                                            
         LA    R3,TCTAUHEL                                                      
         MVC   TAUHUSN,OPTPRVTU    REPLACE LAST TOTAL USE NUMBER                
         XC    TAUHUSNL,TAUHUSNL   INIT LAST LIFT USE NUM TO 0                  
         TM    PAYOPTS2,OPRVLUS    IF LU OPTION INPUT                           
         BZ    CLC100                                                           
         MVC   TAUHUSNL,OPTPRVLU   REPLACE LAST LIFT USE NUMBER                 
         B     CLC100                                                           
         SPACE 1                                                                
CLC80    TM    LCLSTAT3,FORCEUPG   IF FORCED UPGRADE                            
         BZ    CLC90                                                            
         MVC   TCTAUHEL(TAUHCSUB-TAUHD),SVTAUHEL                                
         TM    TGUSSTA3,CBLUSE     SET CAST UH TO BE SAME AS FORCED             
         BNZ   CLC90               (EXCEPT FOR CBL,SCB AND LCB)                 
         MVC   TCTAUHEL(TAUHCSYS-TAUHD),SVTAUHEL                                
         MVC   TCTAUHEL+TAUHFRTY-TAUHD(14),SVTAUHEL+TAUHFRTY-TAUHD              
         CLI   TGUSEQU,ULCB                                                     
         BE    CLC90                                                            
         MVC   TCTAUHEL,SVTAUHEL                                                
         SPACE 1                                                                
CLC90    TM    TGUSTYST,UPGRADE    IF UPGRADE TYPE                              
         BZ    CLC100                                                           
         LA    R3,TCTAUHEL                                                      
         MVC   TCMAJORS,SVINPMAJ   GET MAJORS INPUT                             
         OC    TCMAJORS,TAUHMAJ    MERGE WITH PREV MAJORS                       
         SPACE 1                                                                
         TM    TGUSSTA3,USEMTAB    FOR CABLES AND WILDSPOTS                     
         BNZ   CLC100              UNITS/SUBSCRIBERS SET IN TAGEN50             
         SPACE 1                                                                
         LH    R1,SVINPUNT         ELSE                                         
         AH    R1,TAUHUNT          ADD INPUT UNITS & PREV UNITS                 
         STH   R1,TCUNITS                                                       
CLC100   BAS   RE,RATECALC         CALCULATE RATES                              
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE CALCULATES RATES AND WRITES BACK                         
*              CAST RECORD IF NECESSARY                                         
         SPACE                                                                  
RATECALC NTR1                                                                   
         BAS   RE,SETNUSES         SET NUMBER OF USES PAID PREVIOUSLY           
         SPACE 1                                                                
         BAS   RE,HGRTPNH          HANDLE P&H FOR GRT PAYMENTS                  
         SPACE                                                                  
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         BE    RATECX              A WEB TRANSACATION, ABORT NOW                
         SPACE 1                                                                
         BRAS  RE,GETTSHT          GET (TRADITIONAL) TIMESHEET IN AIO3          
         BRAS  RE,GETWBTS          GET WEB TIMESHEET IN AIO3                    
         BRAS  RE,BLDWBSD          BUILD WEB SESSION DETAILS ELEMENT            
         SPACE                                                                  
         BRAS  RE,SETADAM          ADD WEB APP ADDITIONAL AMOUNTS               
         SPACE                                                                  
         TM    TCPAYST2,TCRETRO                                                 
         BZ    RATEC5                                                           
         TM    TRNSTAT,PYINPROG                                                 
         BZ    RATEC5                                                           
         NI    TCPAYST2,X'FF'-TCRETRO                                           
         MVI   TGYREQU,CN13                                                     
         GOTO1 ARATECLC,DMCB,(RC),TCD,SYSCOMM                                   
         MVC   TCRAPPLC,TCAPPLIC                                                
         XC    TCTACREL,TCTACREL                                                
         OI    TCPAYST2,TCRETRO                                                 
         MVI   TGYREQU,CN16                                                     
         L     R0,AIO                                                           
         MVC   AIO,TCACAST                                                      
         GOTO1 RECVAL,DMCB,TLCACDQ,(X'A0',0)                                    
         JE    *+6                                                              
         DC    H'00'                                                            
         ST    R0,AIO                                                           
         SPACE                                                                  
RATEC5   GOTO1 ARATECLC,DMCB,(RC),TCD,SYSCOMM  CALCULATE RATES                  
         BRAS  RE,RETSPNH          GET RETRO SUBJ P&H IF NECESSARY              
         SPACE                                                                  
         BRAS  RE,SVWEBDET         SAVE WEB PAYMENT DETAILS                     
         BRAS  RE,ADJDISC          ADJUST BREAKDOWN TABLE FOR DISCOUNTS         
******** BRAS  RE,RESETINP         RESET INPUTTED PAY VARIABLES                 
         BRAS  RE,SETMINOR         SET MINOR FOR P+                             
         BRAS  RE,SETWAHI          SET HAWAII/WASHINGTON FOR P+                 
         SPACE                                                                  
         TM    TGUSSTA3,CBLUSE     IF CABLE PAYMENT                             
         BZ    *+10                                                             
         MVC   TGUSTYST,SVUSTYST   RESTORE USE STATUS                           
         SPACE                                                                  
         TM    PAYOPTS3,ODUMMY     IF DUMMY PYMT, TURN OFF SOME BITS TO         
         BZ    *+8                                                              
         NI    TCRTRN,ALL-TCRTDUM  PREVENT WRITING FTRACK & CAST RECS           
         SPACE                                                                  
         TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         BO    RATEC6                                                           
         TM    PAYSTAT1,DETAIL     OR NOT PAYING WITH DETAILS                   
         BO    RATEC7                                                           
         TM    PAYMODE,DTLONBSE    AND IF DETAIL NOT ON BASE SCREEN             
         BO    RATEC7                                                           
RATEC6   CLI   TCERROR,0           TEST NO ERROR FROM CALCULATING RATES         
         BNE   *+8                                                              
         OI    LCLSTAT,PERFPAID    SET SOMEONE PAID                             
         SPACE 1                                                                
RATEC7   TM    PAYMODE,VALSCRN     IF NOT JUST VALIDATING                       
         BO    RATECX                                                           
         SPACE 1                                                                
         BAS   RE,PROCGRT          PROCESS GUARANTEE RECORD                     
         SPACE 1                                                                
         BRAS  RE,PROCGCON         PROCESS GUARANTEE CONTRACT RECORD            
         SPACE 1                                                                
         TM    LCLSTAT3,SOAPRES    IF USE IS A SOAP RESIDUAL                    
         BZ    RATEC9                                                           
         OC    TCAPPLCR,TCAPPLCR   AND IF APPLIED CREDITS                       
         BZ    *+8                                                              
         BAS   RE,UPDECAST         UPDATE ECAST RECORDS                         
         B     RATECX                                                           
         SPACE 1                                                                
RATEC9   TM    TCRTRN,TCRTCAST     TEST CAST RECORD CHANGED                     
         BZ    RATECX                                                           
         L     R3,AIO1             CHANGED CAST RECORD IN AIO1                  
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM                        
         MVC   KEY,0(R3)                                                        
         GOTO1 HIGH                NEED TO READ AGAIN BEFORE WRITE              
         CLC   KEY(L'TLCAKEY),KEYSAVE                                           
         BE    RATEC10                                                          
         BRAS  RE,FNDCSEQ          IF NOT FOUND, TRY TO FIND BY CAST            
         BNE   MISSCAST            SEQUENCE NUMBER                              
RATEC10  MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         GOTO1 SAVPTRS,DMCB,CASTPTRS  SAVE PASSIVE PTRS                         
         SPACE 1                                                                
         USING TLCAD,R3                                                         
         MVC   AIO,AIO1               RESTORE AIO FOR CAST RECORD               
         L     R3,AIO                                                           
         TM    CAGUARST,TAGUSPAY      IF GRT RECORD ADDED BY PAY                
         BZ    *+14                                                             
         NI    TLCASORT,X'FF'-X'02'   SET MEMBER IS ON GUAR                     
         MVC   TGCSORT,TLCASORT       SET GLOBAL CAST SORT KEY FOR CHKS         
         SPACE 1                                                                
         GOTO1 MYPUTREC               WRITE IT BACK                             
         SPACE 1                                                                
         TM    PAYMODE,DRAFT                                                    
         BO    RATECX                                                           
         GOTO1 ADDPTRS,DMCB,(X'80',CASTPTRS)  UPDATE ALL PTRS                   
         SPACE 1                                                                
RATECX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE USES TSAR RECORD AT TCATSAR TO UPDATE TACRBAL            
*              ON ECAST RECORDS WHERE NECESSARY                                 
         SPACE                                                                  
         USING ECASTABD,R3                                                      
UPDECAST NTR1                                                                   
         L     R3,TCATSAR          R3=A(TSAR REC)                               
         TM    ECSTSTAT,ECSTS2ND   IF CAST FROM 2ND COMM'L                      
         BZ    *+10                                                             
         MVC   TGCOM,COM2          SET TGCOM OF 2ND COMM'L                      
         ZIC   R0,ECSTNEPI         R0=N'EPISODES                                
         LA    R3,ECSTEPIS         R3=A(EPISODE ENTRY)                          
         XC    TGINV,TGINV         CLEAR TGINV FOR RECVAL                       
         USING EPISD,R3                                                         
UPDEC5   OC    EPIAPPL,EPIAPPL     TEST APPL CREDIT NOT 0 FOT THIS EPI          
         BZ    UPDEC15                                                          
         LH    R1,EPINUM           GET EPISODE NUM                              
         CVD   R1,DUB                                                           
         UNPK  TGEPI,DUB+5(3)      SET TGEPI FOR RECVAL                         
         OI    TGEPI+4,X'F0'                                                    
         GOTO1 RECVAL,DMCB,TLECCDQ,(X'B0',0) READ ECAST REC FOR UPDATE          
         BNE   MISSCAST                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACRELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE                                                                  
         USING TACRD,R4                                                         
         XR    R1,R1                                                            
         ICM   R1,7,EPIAPPL        R1=APPLIED CREDIT                            
         TM    TCPAYST,TCCREDIT                                                 
         BO    *+6                                                              
         LCR   R1,R1               REVERSE SIGN IF NOT CREDIT PAYMENT           
         A     R1,TACRBAL          AND ADD TO BALANCE                           
         BNM   *+6                                                              
         XR    R1,R1               INSURE IT DOESN'T GO NEGATIVE                
         ST    R1,TACRBAL          AND SAVE NEW BALANCE IN TACREL               
         STCM  R1,7,EPIBAL         AND IN TSAR REC FOR ETRACK REC LATER         
         SPACE                                                                  
         USING TLECD,R2                                                         
         LTR   R1,R1               IF BALANCE BECOMES 0                         
         BNZ   UPDEC10                                                          
         L     R2,AIO                                                           
         NI    TLECSTAT,ALL-TLECSBAL           TURN OFF CREDIT BAL BIT          
         MVC   KEY+TLDRSTAT-TLDRD(1),TLECSTAT  ALSO DO IN ACTV PTR              
         SPACE                                                                  
         GOTO1 MYWRITE             WRITE BACK ACTIVE PTR                        
UPDEC10  GOTO1 MYPUTREC            WRITE BACK RECORD                            
         SPACE                                                                  
UPDEC15  LA    R3,L'ECSTEPIS(R3)   BUMP TO NEXT EPISODE ENTRY                   
         BCT   R0,UPDEC5                                                        
         SPACE                                                                  
UPDECX   MVC   TGINV,INVNO         MOVE COMPL. INVOICE BACK TO GLOBAL           
         XC    TGINV,HEXFFS                                                     
         MVC   TGCOM,SVTGCOM       RESTORE TGCOM                                
         MVI   MYTSACTN,TSAPUT     WRITE BACK TSAR REC WITH NEW EPIBAL          
         BRAS  RE,CALLTSAR                                                      
         BNE   TOOBIG                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET NUMBER OF USES PAID PREVIOUSLY                    
         SPACE                                                                  
SETNUSES NTR1                                                                   
         TM    TGUSSTA3,NWKUSE                                                  
         BZ    XIT                 DON'T BOTHER IF NOT NWK USE PYMNT            
         SPACE                                                                  
         USING TAUHD,R3                                                         
         USING TACAD,R4                                                         
         LA    R3,TCTAUHEL         R3=A(USAGE HISTORY ELEMENT)                  
         LA    R4,ELTACA           R4=A(CAST DETAILS ELEMENT)                   
         CLI   VERSION,0           TEST IF PAYING A VERSION                     
         BNE   SETNU5                                                           
         TM    TACASTAT,TACASTLO                                                
         BO    SETNU10             BRANCH TO SETNU10 IF ONLY ON LIFT            
         TM    TACASTAT,TACASTLF                                                
         BZ    SETNU20             BRANCH TO SETNU20 IF NOT ON LIFT             
         SPACE                                                                  
SETNU5   MVC   TCNUSES,TAUHUSN     TOTAL PREV N'USES PAID=LAST USE NUM          
         MVC   TCNUSESL,TAUHUSNL   PREV N'USES PAID TO LFT=LAST LIFT #          
         TM    PAYOPTS2,OPRVTUS                                                 
         BO    SETNX                                                            
         MVC   TCNUSESP,TAUHUSNP   PREV N'PAX USES EXCLUDED FR CLA PAY          
         MVC   TCLUSESP,TAUHLUSP   PREV N' LFT PAX USES EX FR CLA PAY           
         B     SETNX                                                            
         SPACE                                                                  
SETNU10  MVC   TCNUSES,TAUHUSNL    TOTAL PREV N'USES PAID=LAST LIFT #           
         MVC   TCNUSESL,TAUHUSNL   PREV N'USES PAID TO LFT=LAST LIFT #          
         TM    PAYOPTS2,OPRVTUS                                                 
         BO    SETNX                                                            
         MVC   TCNUSESP,TAUHUSNP   PREV N'PAX USES EXCLUDED FR CLA PAY          
         MVC   TCLUSESP,TAUHLUSP   PREV N' LFT PAX USES EX FR CLA PAY           
         B     SETNX                                                            
         SPACE                                                                  
SETNU20  LH    R1,TAUHUSN          LAST USE NUMBER                              
         SH    R1,TAUHUSNL         - LAST USE # FOR LIFT                        
         STH   R1,TCNUSES          = TOTAL PREV N'USES PAID                     
         XC    TCNUSESL,TCNUSESL   NO PREV USES PAID TO LFT                     
         TM    PAYOPTS2,OPRVTUS                                                 
         BO    SETNX                                                            
         MVC   TCNUSESP,TAUHUSNP                                                
         MVC   TCLUSESP,TAUHLUSP   PREV N' LFT PAX USES EX FR CLA PAY           
         SPACE                                                                  
SETNX    CLI   TGUSEQU,UCLA        IF NOT CLA                                   
         BE    XIT                                                              
         CLI   TGUSEQU,UPAX        OR IF NOT PAX                                
         BE    XIT                                                              
         OC    TCNUSES,TCNUSES     IF TOTAL PREV N'USES PAID NOT 0              
         BZ    XIT                                                              
         OI    TCINPUT,TCINPAY     SET HAVE PYMNT ALREADY TO GET 0 CHK          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES SETTING TCINPNH BIT FOR GRT PAYMENTS             
         SPACE                                                                  
HGRTPNH  NTR1                                                                   
         CLI   TGUSEQU,UGRT        TEST GRT PAYMENT                             
         BNE   XIT                                                              
         OC    TGGUA,TGGUA         IF NOT INSTALLMENT PAYMENT                   
         BNZ   *+14                                                             
         MVC   CAGUARST,GUARSTAT   USE GUARANTEE STATUS FROM PAY SCREEN         
         B     HGRTP10                                                          
         SPACE                                                                  
         MVC   AIO,AIO2            ELSE DON'T CREAM CAST RECORD IN AIO1         
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'24',0)  GET GUARANTEE RECORD              
         BNE   NOGRT                                                            
         SPACE                                                                  
         L     R4,AIO                                                           
         MVC   AIO,AIO1            RESTORE AIO                                  
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TAGUD,R4                                                         
         MVC   CAGUARST,TAGUSTAT   SAVE STATUS FOR INDIV. CAST MEMBER           
         SPACE                                                                  
HGRTP10  TM    CAGUARST,PAYPNH     IF PAY P&H ON USE BIT ON                     
         BZ    *+8                                                              
         OI    TCINPUT,TCINPNH     SET SO WILL BE 0 UNLESS OVERRIDEN            
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO PROCESS GUARANTEE RECORD                              
         SPACE 1                                                                
PROCGRT  NTR1                                                                   
         TM    PAYOPTS3,ODUMMY     DON'T DO IF DUMMY PAYMENT                    
         BO    XIT                                                              
         SPACE                                                                  
         CLI   TGUSEQU,UGRT        ONLY FOR GUARANTEE USES                      
         BNE   XIT                                                              
         SPACE                                                                  
         TM    PAYSTAT1,CREDIT                                                  
         BO    XIT                 SKIP IF CREDIT PAYMENT                       
         SPACE                                                                  
         L     RA,ATWA             INSURE RA=A(TWA)                             
         SPACE 1                                                                
         GOTO1 GUARNTEE            ADD OR UPDATE GUARANTEE RECORD               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS CHECK RECORD                                  
         SPACE 1                                                                
PROCCHK  NTR1                                                                   
         L     RA,ATWA             INSURE RA=A(TWA)                             
         SPACE 1                                                                
         CLI   TGUNEQU,DGA                                                      
         BE    PROCC2              TEST UNION DGA                               
         CLI   TGUNEQU,WGA                                                      
         BNE   PROCC3              OR WGA                                       
         SPACE 1                                                                
PROCC2   MVC   SVTCPNH,TCPNH       SAVE TCPNH FOR AUTO LOCAL CHECKS             
         XC    TCPNH,TCPNH         CLEAR SO WON'T BE ON CHECK OR INV            
         SPACE 1                                                                
         USING TACMD,R4                                                         
PROCC3   CLI   TGUSEQU,USOP        IF SOP USE                                   
         BNE   PROCC5                                                           
         XC    SOPCMTEL,SOPCMTEL                                                
         MVI   ELCODE,TACMELQ                GET CHECK COMMENT FROM             
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPC))  CAST RECORD IN AIO                 
         BNE   PROCC5                                                           
         L     R4,TGELEM                                                        
         ZIC   R1,TACMLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SOPCMTEL(0),TACMEL  SAVE IT FOR CHECK REC LATER                  
         SPACE                                                                  
PROCC5   GOTO1 CHECK               BUILD CHECK RECORD                           
         SPACE 1                                                                
         BRAS  RE,BLDUPG           BUILD UPGR DETAILS EL. FOR VERSIONS          
         SPACE 1                                                                
         MVI   MYMODE,BLDCHK       GIVE OVERLAY A SHOT                          
         BAS   RE,GOOVLY                                                        
         SPACE 1                                                                
         MVC   SYSFIL,=CL8'CHKFIL' SET SYSFIL AND SYSDIR FOR CHECKS             
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         GOTO1 MYADDREC            AND ADD IT BACK                              
         SPACE 1                                                                
         TM    PAYMODE,DRAFT                                                    
         BO    PROCC10                                                          
         GOTO1 ADDPTRS,DMCB,CHKPTRS  ADD PASSIVE PTRS                           
         SPACE 1                                                                
PROCC10  MVC   SYSFIL,SVSYSFIL     SET SYSFIL AND SYSDIR TO DEFAULTS            
         MVC   SYSDIR,SVSYSDIR                                                  
         SPACE 1                                                                
         BRAS  RE,CASTUH           ADD CAST UH REC                              
         SPACE 1                                                                
         TM    LCLSTAT3,SOAPRES    IF USE IS A SOAP RESIDUAL                    
         BZ    *+12                                                             
         BAS   RE,ETRACK           BUILD ECAST TRACKING RECORDS                 
         B     *+8                                                              
         BAS   RE,FTRACK           ELSE BUILD TRACKING RECORD                   
         SPACE 1                                                                
         BAS   RE,ADDUP            ADD TO INVOICE TOTALS                        
         BRAS  RE,CASTVEL          ADD VERSION ELEMENTS TO CAST RECORD          
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*              ROUTINE TO ADD FIXED CYCLE TRACKING RECORDS                      
         SPACE 1                                                                
FTRACK   NTR1                                                                   
         TM    TCRTRN,TCRTTACR     TEST APPLIED CREDIT EL. CHANGED              
         BZ    FTX                                                              
         LA    R4,TCTACREL         R4=A(NEW/UPDATED APPLIED CREDIT EL.)         
         USING TACRD,R4                                                         
**NO-OP  TM    TACRSTAT,TACRSTRK   TEST TRACKING ENABLED                        
**8/8    BZ    FTX                                                              
         XC    KEY,KEY                                                          
         LA    R3,KEY              BUILD BASIC KEY                              
         USING TLFTD,R3                                                         
         MVI   TLFTCD,TLFTCDQ      RECORD CODE                                  
         MVC   TLFTSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         MVC   TLFTCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLFTCAST,TGCSORT+4  CAST INPUT SEQUENCE NUMBER                   
         MVC   TLFTSTRT(6),TACRSTRT  CYCLE DATES                                
         XC    TLFTSTRT(6),HEXFFS    (COMPLEMENTED)                             
         SPACE 1                                                                
         L     R1,TCAPPLIC         AMOUNT TO BE APPLIED THIS TIME               
         S     R1,TCAPPLCR         LESS APPLIED CREDITS                         
         ST    R1,TGDUB            IS APPLIED AMOUNT FOR BLDTRK                 
         MVC   TGDUB+4(4),TACRBAL  SET NEW BALANCE AS WELL                      
         SPACE 1                                                                
         MVC   TGINV,INVNO                     SET UNCOMP. INV IN GLB           
         MVC   AIO,AIO2                        SET TO ADD FROM I/O 2            
         GOTO1 BLDTRK,DMCB,TLFTTRK-TLFTD,AIO1  BUILD TRACKING RECORD            
         XC    TGINV,HEXFFS                                                     
         SPACE 1                                                                
         L     R3,AIO              R3=A(RECORD)                                 
         MVC   TLFTINV,INVNO       ADD INVOICE NUMBER TO KEY                    
         SPACE 1                                                                
         OC    DSPHCOM,DSPHCOM     ADD HISTORY COMMENT TO RECORD                
         BZ    FT8                                                              
         LH    R2,DSPHCOM                                                       
         AR    R2,RA                                                            
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',(R2)),TACMTYPH                         
         SPACE 1                                                                
FT8      TM    PAYSTAT2,PCYCCOML   IF PER CYCLE COMMERCIAL ...                  
         BZ    FT9                                                              
         SPACE 1                                                                
         LR    R0,R4                                                            
         SPACE 1                                                                
         USING TAGTD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAGTELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         XC    TAGTPNH,TAGTPNH     CLEAR PENSION AND HEALTH                     
         DROP  R4                                                               
         SPACE 1                                                                
         LR    R4,R0                                                            
         SPACE 1                                                                
FT9      GOTO1 MYADDREC            ADD TRACKING RECORD TO FILE                  
         MVC   AIO,AIO1            RESTORE DEFAULT I/O AREA                     
         SPACE 1                                                                
         BAS   RE,FQREQ            ADD TRACKING REQUEST TO TABLE                
         SPACE 1                                                                
FTX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE USES TSAR RECORD AT TCATSAR TO ADD ECAST                 
*              TRACKING RECORDS WHEN NECESSARY                                  
         SPACE                                                                  
         USING ECASTABD,R3                                                      
ETRACK   NTR1                                                                   
         OC    TCAPPLCR,TCAPPLCR   TEST APPLIED CREDITS                         
         BZ    ETX                                                              
         L     R3,TCATSAR          R3=A(TSAR REC)                               
         ZIC   R0,ECSTNEPI         R0=N'EPISODES                                
         LA    R3,ECSTEPIS         R3=A(EPISODE ENTRY)                          
         MVC   TGINV,INVNO         SET UNCOMP. INV IN GLB FOR BLDTRK            
         MVC   AIO,AIO2            SET TO ADD ETRACK FROM I/O 2                 
         SPACE                                                                  
         USING TLETD,R2                                                         
         USING EPISD,R3                                                         
         XC    KEY,KEY             BUILD BASIC KEY                              
         LA    R2,KEY              R2=A(KEY)                                    
         MVI   TLETCD,TLETCDQ      RECORD CODE                                  
         MVC   TLETSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         MVC   TLETCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLETCAT,TGCAT       CATEGORY                                     
         SPACE                                                                  
ET5      OC    EPIAPPL,EPIAPPL     TEST APPL CREDIT NOT 0 FOR THIS EPI          
         BZ    ET15                                                             
         SPACE                                                                  
         LH    R1,EPINUM                                                        
         CVD   R1,DUB                                                           
         UNPK  TLETEPI,DUB+5(3)    EPISODE NUMBER                               
         OI    TLETEPI+4,X'F0'                                                  
         XC    TLETEPI,HEXFFS      (COMPLEMENTED)                               
         SPACE 1                                                                
         XC    TGDUB(8),TGDUB                                                   
         SR    R1,R1                                                            
         ICM   R1,7,EPIAPPL        APPLIED AMOUNT                               
         LCR   R1,R1               COMPLEMENTED                                 
         ST    R1,TGDUB            IS APPLIED AMOUNT FOR BLDTRK                 
         MVC   TGDUB+5(3),EPIBAL   SET NEW BALANCE AS WELL                      
         SPACE 1                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO1             CHANGE TAPD IN CHECK REC AT AIO1             
         MVI   ELCODE,TAPDELQ      TO HAVE EPISODE INFO FOR BLDTRK              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TAPDAPPL,TGDUB       APPLIED AMOUNT                              
         XC    TAPDPNH,TAPDPNH                                                  
         MVC   TAPDPNH+2(2),EPIPNH  P&H                                         
         XC    TAPDPAYI,TAPDPAYI                                                
         MVC   TAPDPAYI+1(3),EPIPAY PAYMENT                                     
         XC    TAPDPAYC,TAPDPAYC                                                
         SPACE                                                                  
         TM    PAYSTAT1,CREDIT     IF CREDIT PAYMENT                            
         BZ    *+8                                                              
         BAS   RE,REVSIGN          REVERSE SIGN ON AMOUNTS                      
         SPACE 1                                                                
         GOTO1 BLDTRK,DMCB,TLETTRK-TLETD,AIO1  BUILD TRACKING RECORD            
         SPACE 1                                                                
         L     R2,AIO              R2=A(ETRACK RECORD)                          
         MVC   TLETINV,INVNO       ADD INVOICE NUMBER TO KEY                    
         LA    R2,KEY              RESET R2=A(KEY)                              
         SPACE 1                                                                
         OC    DSPHCOM,DSPHCOM     ADD HISTORY COMMENT TO RECORD                
         BZ    ET8                                                              
         LH    R4,DSPHCOM                                                       
         AR    R4,RA                                                            
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',(R4)),TACMTYPH                         
         SPACE 1                                                                
ET8      GOTO1 MYADDREC            ADD TRACKING RECORD TO FILE                  
         SPACE 1                                                                
         XC    KEY,KEY                     ADDREC PUTS ADDRESS IN KEY           
         MVC   KEY(TLETTRK-TLETD),KEYSAVE  SO RESTORE KEY                       
         SPACE 1                                                                
*ASK IRV BAS   RE,FQREQ            ADD TRACKING REQUEST TO TABLE                
         SPACE                                                                  
ET15     LA    R3,L'ECSTEPIS(R3)   BUMP TO NEXT EPISODE ENTRY                   
         BCT   R0,ET5                                                           
         XC    TGINV,HEXFFS        COMPLEMENT GLOBAL INVOICE                    
         SPACE                                                                  
ETX      MVC   AIO,AIO1            RESTORE DEFAULT I/O AREA                     
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE REVERSES SIGN ON AMOUNTS FOR ETRACK REC                  
*              R4=A(TAPD ELEMENT)                                               
         SPACE                                                                  
         USING TAPDD,R4                                                         
REVSIGN  DS    0H                                                               
         L     RF,TAPDPNH          COMPLEMENT P&H AMOUNT                        
         LCR   RF,RF                                                            
         ST    RF,TAPDPNH                                                       
         L     RF,TAPDPAYI                    PAYMENT AMOUNT                    
         LCR   RF,RF                                                            
         ST    RF,TAPDPAYI                                                      
         L     RF,TAPDAPPL                    APPLIED AMOUNT                    
         LCR   RF,RF                                                            
         ST    RF,TAPDAPPL                                                      
         L     RF,TGDUB                       APPLIED AMOUNT IN TGDUB           
         LCR   RF,RF                                                            
         ST    RF,TGDUB                                                         
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO GENERATE A FIXED CYCLE TRACKING REQUEST               
         SPACE 1                                                                
         USING TACRD,R4            R4=A(NEW/UPDATED APPLIED CREDIT EL.)         
FQREQ    NTR1                                                                   
         TM    AGYSTAT,TAAYSNFT    GET OUT IF SUPPRESSED AT AGY LEVEL           
         BO    FQX                                                              
         TM    TCCASTAT,TACASTNF   OR CAST LEVEL                                
         BO    FQX                                                              
         TM    TACRSTAT,TACRSGUA   ONLY IF IT'S A GUARANTEE                     
         BZ    FQX                                                              
         LA    R3,FQTABLE          R3=A(TABLE OF REQUESTS)                      
         USING TAFQSBEL,R3                                                      
         LA    R0,NFQTAB           R0=N'ENTRIES IN TABLE                        
         SPACE                                                                  
FQ5      CLI   0(R3),0             IF ENTRY NOT EMPTY                           
         BE    FQ10                                                             
         LA    R3,L'TAFQSBEL(R3)   BUMP TO NEXT ENTRY                           
         BCT   R0,FQ5              KEEP LOOPING TILL FIND AVAILABLE ONE         
         DC    H'0'                DIE IF NO ROOM IN TABLE                      
         SPACE 1                                                                
FQ10     MVC   TAFQOFF,TGOFF       TP OFFICE                                    
         PACK  DUB,TGSSN                                                        
         CVB   R1,DUB                                                           
         ST    R1,TAFQSSN          SS NUMBER IN BINARY                          
         MVC   TAFQCAST,TGCSORT+4  CAST INPUT SEQUENCE NUMBER                   
         MVC   TAFQSTRT,TACRSTRT   CYCLE START                                  
         SPACE 1                                                                
FQX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD CHECK TOTALS TO INVOICE TOTALS                    
*              AND UPDATE LOCAL TABLES                                          
         SPACE 1                                                                
ADDUP    NTR1                                                                   
         LA    R1,TCTOTS           R1=A(CHECK TOTALS)                           
         LA    RF,ITOTS            RF=A(INVOICE TOTALS)                         
         LA    R0,NTCTOTS          R0=N'ACCUMS                                  
         SPACE 1                                                                
ADD2     L     RE,0(RF)            CURRENT INVOICE TOTAL                        
         A     RE,0(R1)            PLUS THIS CHECK TOTAL                        
         ST    RE,0(RF)            IS NEW INVOICE TOTAL                         
         SPACE 1                                                                
         LA    R1,4(R1)            BUMP TO NEXT CHECK TOTAL                     
         LA    RF,4(RF)            AND TO NEXT INVOICE TOTAL                    
         BCT   R0,ADD2                                                          
         SPACE                                                                  
         BRAS  RE,SV04ACAN         SAVE CANADIAN AMOUNTS FOR 2404A              
         SPACE                                                                  
         LA    R4,ELTACA           R4=A(CAST DETAILS ELEM)                      
         USING TACAD,R4                                                         
         MVC   FULL(3),TACALOCL    FULL = LOCAL                                 
         CLC   FULL(3),=C'047'     IF LOCAL IS 047, USE 47 INSTEAD              
         BNE   *+10                                                             
         MVC   FULL(3),=C'47 '                                                  
         LA    RF,UNLCLLST         RF=A(ENTRY IN UNION/LOCAL LIST)              
         LA    R0,NUNLCL           R0=N'ENTRIES                                 
         SPACE                                                                  
ADD2A    OC    0(L'UNLCLLST,RF),0(RF)     IF END OF ENTRIES                     
         BNZ   ADD2B                                                            
         MVC   0(L'TACAUN,RF),TACAUN      ADD ONE FOR THIS UNION/LOCAL          
         MVC   3(L'TACALOCL,RF),FULL                                            
         B     ADD2X                                                            
         SPACE                                                                  
ADD2B    CLC   TACAUN,0(RF)        ELSE IF NO MATCH ON UNION                    
         BNE   ADD2C                                                            
         CLC   FULL(3),3(RF)       AND LOCAL                                    
         BE    ADD2X                                                            
ADD2C    LA    RF,L'UNLCLLST(RF)   BUMP TO NEXT ENTRY                           
         BCT   R0,ADD2A                                                         
         DC    H'0'                NOT ENOUGH ROOM IN TABLE                     
         SPACE                                                                  
ADD2X    OC    TCAGTFEE,TCAGTFEE   IF HAVE AGENT FEE, ACCUM BY AGENT            
         BZ    ADD6                                                             
         BRAS  RE,ADDUPAGT                                                      
         B     ADDX                DONE, NO P&H, H&W, OR I&R FOR PRINT          
         SPACE                                                                  
ADD6     OC    TCHNW,TCHNW         IF HAVE H&W AMT, ACCUMULATE BY LOCAL         
         BZ    ADD7                                                             
         L     R3,TCHNW            R3=AMOUNT                                    
         SPACE                                                                  
         BAS   RE,ADDUPAFM         ADD TO LCL ACCUMS                            
         B     ADDX                                                             
         SPACE                                                                  
ADD7     XR    R4,R4               INIT R4 TO 0 FOR ADDUPLCL                    
         OC    TCINR,TCINR         IF HAVE I&R AMT, ACCUMULATE BY LOCAL         
         BZ    ADD8                                                             
         L     R3,TCINR            R3=AMOUNT FOR LOCAL                          
         L     R1,TCMDED           R1=MISC. DED AMOUNT                          
         L     R4,TCDUES           R4=UNION DUES AMOUNT                         
         SPACE                                                                  
         LA    R2,LCLACT           R2=A(ACT TABLE)                              
         LA    R0,LCLNACT          R0=N'ACCUMS                                  
         CLI   TGUNEQU,ACT                                                      
         BE    ADD10               TEST UNION ACT                               
         SPACE                                                                  
         LA    R2,LCLUDA           R2=A(UDA TABLE)                              
         LA    R0,LCLNUDA          R0=N'ACCUMS                                  
         CLI   TGUNEQU,UDA                                                      
         BE    ADD10               TEST UNION UDA                               
         B     ADDX                                                             
         SPACE                                                                  
ADD8     OC    SVTCPNH,SVTCPNH     IF HAVE P&H AMT, ACCUMULATE BY LOCAL         
         BZ    ADDX                                                             
         SPACE                                                                  
         CLI   TGUNEQU,DGA                                                      
         BNE   ADD9                TEST UNION DGA                               
         LA    R2,LCLDGA           R2=A(DGA TABLE)                              
         LA    R0,LCLNDGA          R0=N'ACCUMS                                  
         ICM   R3,15,SVTCPNH       R3=AMOUNT FOR LOCAL                          
         L     R1,TCMDED           R1=MISC. DED AMOUNT                          
         B     ADD10                                                            
         SPACE                                                                  
ADD9     CLI   TGUNEQU,WGA                                                      
         BNE   ADDX                TEST UNION WGA                               
         LA    R2,LCLWGA           R2=A(WGA TABLE)                              
         LA    R0,LCLNWGA          R0=N'ACCUMS                                  
         L     R3,TCPENS           R3=PENSION AMOUNT FOR LOCAL                  
         L     R1,TCHLTH           R1=HEALTH AMOUNT                             
         SPACE                                                                  
ADD10    BAS   RE,ADDUPLCL         ADD TO LCL ACCUMS                            
ADDX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ACCUMULATE AMOUNTS IN R3, R1, AND R4                  
*              INTO TABLE AT R2.  R0=N'ACCUMS IN TABLE                          
         SPACE                                                                  
         USING LCLTABD,R2                                                       
ADDUPLCL DS    0H                                                               
ADDL3    OC    LCLTFST(LCLTLNQ),LCLTFST  IF END OF ENTRIES                      
         BNZ   ADDL4                                                            
         MVC   LCLLOCAL,TGLCL        ADD ONE FOR THIS LOCAL                     
         ST    R3,LCLTFST            INIT FIRST                                 
         ST    R1,LCLT2ND            AND SECOND                                 
         ST    R4,LCLT3RD            AND THIRD AMOUNTS                          
         MVI   LCLTNCHK,1            SET ONE CHECK                              
         BR    RE                                                               
         SPACE                                                                  
ADDL4    CLC   TGLCL,LCLLOCAL      ELSE IF NO MATCH ON LOCAL                    
         BE    ADDL5                                                            
         LA    R2,LCLTLNQ(R2)      BUMP TO NEXT ENTRY                           
         BCT   R0,ADDL3                                                         
         DC    H'0'                NOT ENOUGH ROOM IN ACCUMS                    
         SPACE                                                                  
ADDL5    A     R3,LCLTFST          THIS CHECK'S FIRST AMOUNT                    
         ST    R3,LCLTFST          + CURRENT TOTAL IS NEW TOTAL                 
         A     R1,LCLT2ND          THIS CHECK'S SECOND AMOUNT                   
         ST    R1,LCLT2ND          + CURRENT TOTAL IS NEW TOTAL                 
         A     R4,LCLT3RD          THIS CHECK'S THIRD AMOUNT                    
         ST    R4,LCLT3RD          + CURRENT TOTAL IS NEW TOTAL                 
         ZIC   R1,LCLTNCHK         INCREMENT NUMBER OF CHECKS                   
         LA    R1,1(R1)                                                         
         STC   R1,LCLTNCHK                                                      
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO ACCUMULATE AMOUNT IN R3 INTO LCLAFM TABLE             
         SPACE                                                                  
ADDUPAFM DS    0H                                                               
         CLC   TGLCL,=C'047'                                                    
         BNE   *+10                                                             
         MVC   TGLCL,=C'47 '                                                    
         SPACE                                                                  
         LA    RF,LCLAFM                                                        
         LA    R0,LCLNAFM                                                       
ADDA3    OC    0(L'LCLAFM,RF),0(RF)  IF END OF ENTRIES                          
         BNZ   ADDA4                                                            
         MVC   4(L'TGLCL,RF),TGLCL   ADD ONE FOR THIS LOCAL                     
         ST    R3,0(RF)                   INIT AMOUNT                           
         BR    RE                                                               
         SPACE                                                                  
ADDA4    CLC   TGLCL,4(RF)         ELSE IF NO MATCH ON LOCAL                    
         BE    ADDA5                                                            
         LA    RF,L'LCLAFM(RF)     BUMP TO NEXT ENTRY                           
         BCT   R0,ADDA3                                                         
         DC    H'0'                NOT ENOUGH ROOM IN ACCUMS                    
         SPACE                                                                  
ADDA5    A     R3,0(RF)            THIS CHECK'S AMT + CURRENT TOTAL             
         ST    R3,0(RF)            IS NEW TOTAL                                 
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO CONTROL DETAIL SCREEN RECORD HANDLING                 
         SPACE 1                                                                
SCRNCTL  NTR1                                                                   
         XR    R3,R3               SET TO START AT TOP OF SCREEN                
         TM    PAYMODE,DTLONBSE    IF DETAIL NOT ON BASE SCREEN                 
         BO    SCRNC1                                                           
         LH    R3,DSPOVLY          SET TO START AT BEGINNING OF DETAIL          
         AR    R3,RA                                                            
SCRNC1   XR    R2,R2               SET TO ADD NEW PAGE                          
         SPACE 1                                                                
         CLI   GPAGE,X'FF'         IF WE HAVE PAGE NUMBER ALREADY               
         BE    *+12                                                             
         LA    R2,GPAGE            MUST BE OLD - SET ITS PAGE NUMBER            
         B     SCRNC3                                                           
         SPACE 1                                                                
         CLI   FPAGE,X'FF'         IF THIS IS FIRST SCREEN RECORD               
         BNE   SCRNC3                                                           
         ZIC   RE,LPAGE            NEXT PAGE NUM SHOULD BE LAST PAGE            
         AHI   RE,1                + 1                                          
         STC   RE,FPAGE            = FIRST NON-SELECT SCREEN PAGE               
         TM    PAYMODE,DTLONBSE    IF DETAIL NOT ON BASE SCREEN                 
         BO    *+8                                                              
         AHI   RE,1                SKIP A PAGE FOR BASE SCREEN                  
         STC   RE,DPAGE            SAVE FIRST DETAIL SCREEN PAGE                
         LA    R2,DPAGE            SET PAGE NUMBER TO WRITE                     
         SPACE                                                                  
SCRNC3   GOTO1 PUTSCRN,DMCB,(R2),(R3),0  WRITE THIS SCREEN RECORD               
         SPACE 1                                                                
         MVC   GPAGE,PPAGE         SAVE MOST RECENT WRITE IN GPAGE ALSO         
         SPACE 1                                                                
         CLI   PFAID,0             TEST PFKEY PRESSED                           
         BE    SCRNC10                                                          
         CLI   PFAID,24            TEST ABORT PFKEY PRESSED                     
         BNE   SCRNC5                                                           
         BAS   RE,RESTSCRN         RESTORE SCREEN FOR NEXT PAYMENT              
         B     ABORTPFK                                                         
         SPACE                                                                  
SCRNC5   GOTO1 VALPFK,DMCB,(2,0)   TEST WHETHER LEVEL 2 PFKEY PRESSED           
         BE    ALLMSG              CC EQ ==> THEY HIT PFKEY - GIVE MSG          
         B     PFERR               ELSE ERROR                                   
         SPACE 1                                                                
SCRNC10  CLC   PPAGE,LPAGE         IF PAGE JUST WRITTEN NE LAST PAGE            
         BE    SCRNCX                                                           
         MVC   GPAGE,PPAGE         SET TO GET NEXT                              
         GOTO1 GETSCRN,DMCB,GPAGE,(RA)                                          
         BE    RDISPMSG            GIVE RE-DISPLAY MESSAGE                      
         DC    H'0'                                                             
         SPACE 1                                                                
SCRNCX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO LOAD DETAIL SCREEN                                    
         SPACE 1                                                                
LOADDTL  NTR1                                                                   
         LH    R3,DSPOVLY                                                       
         AR    R3,RA               R3=A(DETAIL SCREEN LOAD ADDRESS)             
         SPACE 1                                                                
         GOTO1 GETTWA,DMCB,(X'84',(R3)) SAVE STARTING HERE IN TEMPSTR 4         
         SPACE 1                                                                
         MVI   OVERLAY,SCR95       SET DETAIL SCREEN PHASE NUMBER               
         GOTO1 LOADSOPH,DMCB,1     LOAD SCREEN                                  
         BRAS  RE,PROTFLDS         PROTECT SOME FIELDS                          
         BAS   RE,TRANSMIT         TRANSMIT / RE-BUILD                          
         SPACE 1                                                                
         OI    PAYMODE,DTLONSCR    SET DETAIL SCREEN LOADED                     
         OI    LCLSTAT,DTLFIRST    AND 1ST TIME                                 
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO RESTORE LOWER HALF OF ORIGINAL PAY SCREEN             
         SPACE 1                                                                
RESTLWR  NTR1                                                                   
         LH    R3,DSPOVLY                                                       
         AR    R3,RA               R3=A(DETAIL SCREEN LOAD ADDRESS)             
         SPACE 1                                                                
         GOTO1 GETTWA,DMCB,(X'44',(R3))  RESTORE X'900' BYTES TO SCREEN         
*                                   (THIS IMPLIES DSPOVLY MUSN'T EXCEED         
*                                    X'500' (X'E00'-X'900') BYTES)              
         SPACE 1                                                                
         BAS   RE,TRANSMIT         TRANSMIT / RE-BUILD                          
         NI    PAYMODE,ALL-DTLONSCR                                             
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TRANSMITS / RE-BUILDS SCREENS                            
         SPACE 1                                                                
TRANSMIT DS    0H                  R3=A(FIRST FIELD TO TRANSMIT)                
         XR    R0,R0                                                            
TRNS2    OI    6(R3),X'80'         TRANSMIT                                     
         OI    7(R3),X'80'         AND RE-BUILD                                 
         ICM   R0,1,0(R3)                                                       
         BZR   RE                                                               
         AR    R3,R0                                                            
         B     TRNS2                                                            
         EJECT                                                                  
*              ROUTINES TO COMPLETE PAYMENT                                     
         SPACE 1                                                                
COMPLETE NTR1                                                                   
         XC    SVTCPNH,SVTCPNH     CLEAR SVTCPNH LEFT FROM CAST CHECKS          
         XC    TCAGTFEE,TCAGTFEE         TCAGTFEE                               
         MVC   TCTAUHEL,SVTAUHEL   RESTORE COMM'L TAUHEL                        
         MVC   TCUNITS(3),SVTCUNIT                UNITS AND MAJORS              
         SPACE 1                                                                
         TM    PAYMODE,DTLONBSE    IF DETAIL NOT ON BASE SCREEN                 
         BO    COMPL5                                                           
         MVI   NCASTPG,0           SET NO PAGE TABLE                            
         SPACE                                                                  
         CLI   FPAGE,X'FF'         IF WE DON'T HAVE WHAT PAGE NO. OF            
         BNE   COMPL2              BASE SCREEN SHOULD BE                        
         ZIC   R1,LPAGE            THEN SET IT BY ADDING 1 TO LAST              
         LA    R1,1(R1)            SCREEN PAGE WRITTEN SO FAR                   
         STC   R1,FPAGE                                                         
         SPACE                                                                  
COMPL2   LH    R2,DSPUSNME                                                      
         LTR   R2,R2                                                            
         BZ    COMPL4                                                           
         AR    R2,RA                                                            
         MVC   8(L'TGUSNAME,R2),TGUSNAME  USE NAME FOR APPROVE                  
         NI    1(R2),X'DF'                UNPROT SO FIELD WILL BE RED           
         OI    6(R2),X'80'                                                      
         SPACE                                                                  
COMPL4   GOTO1 PUTSCRN,DMCB,FPAGE,0,0     WRITE BASE SCREEN                     
         LTR   R2,R2                                                            
         BZ    *+8                                                              
         OI    1(R2),X'20'         PROTECT USE NAME FIELD AGAIN                 
         SPACE 1                                                                
COMPL5   BAS   RE,PROCLCL          PROCESS LOCAL ACCUMS FOR CHECKS              
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         BE    XIT                 A WEB TRANSACATION, ABORT NOW                
         SPACE 1                                                                
         BAS   RE,PROCAGT          PROCESS AGENT ACCUMS FOR CHECKS              
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         BE    XIT                 A WEB TRANSACATION, ABORT NOW                
         SPACE 1                                                                
         BAS   RE,PROCCSF          PROCESS CONTRACT SERVICE FEE                 
         SPACE 1                                                                
         USING TAUHD,R3                                                         
         LA    R3,TCTAUHEL         BUILD NEW USAGE HISTORY ELEMENT              
         SPACE 1                                                                
         CLI   TGUSEQU,UISU        IF MAKING INDUSTRIAL SUPPLEMENTAL            
         BNE   COMPL6              PAYMENT                                      
         MVC   TAUHISU1,ISUTYPE1   SAVE TYPE 1                                  
         MVC   TAUHISU2,ISUTYPE2   AND TYPE 2                                   
         B     COMPL10                                                          
         SPACE 1                                                                
COMPL6   CLI   TGUSEQU,USNW        IF MAKING SPANISH NETWORK COMBINED           
         BNE   COMPL7                                                           
         TM    TGUSTYST,UPGRADE    UPGRADE PAYMENT                              
         BZ    COMPL7                                                           
         CLI   VERSION,0           TO A COMMERCIAL WITH VERSIONS                
         BE    COMPL7                                                           
         LH    R1,TCUNITS          SAVE CORRECT UNIT COUNT NOW                  
         AH    R1,TAUHUNT                                                       
         STH   R1,TCUNITS                                                       
         SPACE                                                                  
COMPL7   TM    TGUSSTA3,NWKUSE     IF NWK USE                                   
         BO    COMPL10                                                          
         TM    TGUSTYST,UPGRADE    OR UPGRADES,                                 
         BZ    COMPL9                                                           
         CLI   TGUSEQU,ULCB        IF LOCAL CABLE PAYMENT                       
         BNE   COMPL8                                                           
         GOTO1 LCBUHFX             ADJUST OLD LCB TYPES TO NEW FORMAT           
COMPL8   CLI   TGUSEQU,UCAB        EXCEPT FOR CAB,                              
         BNE   COMPL10             KEEP OLD USE INFO FOR UPDATES                
COMPL9   XC    TCTAUHEL,TCTAUHEL                                                
COMPL10  MVI   TAUHEL,TAUHELQ      ELEMENT CODE                                 
         MVI   TAUHLEN,TAUHLNQ     LENGTH                                       
         MVC   TAUHTYPE,TGUSTYP    USE TYPE                                     
         MVC   TAUHSTRT(6),TCPCYCS CYCLE START/END                              
         MVC   TAUHLFT,LIFT        LFT INPUT                                    
         SPACE 1                                                                
         BRAS  RE,VERADJ           SPECIAL PROCESSING FOR VERSIONS              
         SPACE 1                                                                
         USING WEBREQD,RE                                                       
         TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         BZ    COMPL10A                                                         
         TM    TGFASTAT,TGCRNOPY                                                
         BO    COMPL10A                                                         
         L     RE,TGAFAREQ         AND INVOICE IS NOT PROVIDED                  
         OC    WBPYINV,WBPYINV                                                  
         BZ    COMPL10A                                                         
         MVC   SCRINVNO,TGINV      SAVE INVOICE NUMBER                          
         XC    SCRINVNO,HEXFFS     AND WRITE BASE SCREEN                        
         GOTO1 PUTSCRN,DMCB,FPAGE,0,0                                           
         SPACE 1                                                                
COMPL10A GOTO1 INVOICE             BUILD INVOICE RECORD                         
         SPACE 1                                                                
         MVI   MYMODE,BLDINV       GIVE OVERLAY A SHOT                          
         BAS   RE,GOOVLY                                                        
         SPACE 1                                                                
         OC    SVADVDA,SVADVDA     IF HAVE ADVICE RECORD                        
         BZ    COMPL10D                                                         
         MVI   OVERLAY,X'54'       LOAD ADVICE EXTRACT MODULE                   
         GOTO1 LOADSOPH,DMCB,0     AND PASS CONTROL TO IT 2ND TIME              
         GOTO1 (R3),DMCB,(RC)      ADVICE STILL IN AIO2 FOR BLDUH               
*                                  ROUTINE                                      
         L     RE,AIO                                                           
         CLI   0(RE),TLINCDQ       ENSURE AIO CONTAINS THE ADVICE               
         BE    *+10                RECORD                                       
         MVC   AIO,AIO3                                                         
         SPACE 1                                                                
         MVI   WORK,L'TGADV+8         USE WORK TO SIMULATE SCREEN FIELD         
         MVI   WORK+5,L'TGADV                                                   
         MVC   WORK+8(L'TGADV),TGADV                                            
         GOTO1 NAMIN,DMCB,TAFNELQ,(X'80',WORK),TAFNTADV  ADVICE PAID            
         SPACE 1                                                                
COMPL10D DS    0H                                                               
******** BRAS  RE,TRAPEST          TRAP MISSING ESTIMATE NUM BUG                
         SPACE 1                                                                
         USING TACOD,RE                                                         
         LA    RE,ELTACO                                                        
         CLI   TACOCTYP,CCTY04A    IF COMMERCIAL IS ACTRA TYPE 2404A            
         BE    *+12                                                             
         CLI   TACOCTYP,CCTY2404   OR ACTRA TYPE 2404                           
         BNE   COMPL10E                                                         
         TM    LCLSTAT4,NONACTON   AND THERE ARE NO NON-ACTRA CAST              
         BO    COMPL10E            ON PAYMENT                                   
         L     R4,AIO                                                           
         BRAS  RE,STCANDOL         SET PAYMENT AS CANADIAN DOLLARS              
         DROP  RE                                                               
         SPACE 1                                                                
         USING WEBREQD,RE                                                       
COMPL10E TM    TGCTSTST,TGCTSCLI   IF PAYMENT BEING MADE BY CLIENT              
         BO    COMPL10F                                                         
         TM    TGFASTAT,TGFROMFA   OR IF PAYMENT COMING FROM WEB                
         BZ    COMPL11                                                          
         L     RE,TGAFAREQ                                                      
         OC    WBPYINV,WBPYINV     AND INVOICE NUMBER IS NOT PROVIDED           
         BNZ   COMPL11                                                          
COMPL10F GOTO1 MYADDREC            ADD INVOICE RECORD                           
         B     COMPL11B                                                         
         DROP  RE                                                               
         SPACE 1                                                                
COMPL11  GOTO1 MYPUTREC            ELSE WRITE BACK INVOICE RECORD               
         SPACE 1                                                                
COMPL11B TM    PAYMODE,DRAFT                                                    
         BO    COMPL13                                                          
         SPACE 1                                                                
         L     R2,AIO3             INITIALIZE PASSIVE POINTER BLOCK             
         CLC   AIO,AIO3            IN CASE INVOICE IS NEW                       
         BNE   *+8                                                              
         L     R2,AIO2                                                          
         XC    0(255,R2),0(R2)                                                  
         SPACE 1                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN,=C'SVIP'                                                
         MVI   FAWSACTN,FAWSARST   RECALL ORIGINAL PASSIVES FOR                 
         ST    R2,FAWSADR          INVOICE RECORD                               
         GOTO1 WSSVR,(R1)                                                       
         DROP  R1                                                               
         SPACE 1                                                                
         GOTO1 ADDPTRS,DMCB,(R2)   UPDATE PASSIVE POINTERS                      
         SPACE 1                                                                
         BRAS  RE,ADD04AIN         ADD CANADIAN INVOICE FOR 2404A               
         SPACE 1                                                                
         BRAS  RE,ADDWTR           ADD WEB TRANSACTION RECORD                   
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'30',0)  READ COMM REC FOR UPDATE         
         BNE   COMPL13                                                          
         SPACE 1                                                                
         OI    TGGNSTAT,TGJSTCOE   SET TO ONLY GENERATE TLCOECDQ PTR            
         GOTO1 SAVPTRS,DMCB,BLOCK                                               
         SPACE 1                                                                
         TM    TGCTSTST,TGCTSCLI   IF CLIENT                                    
         BZ    COMPL12                                                          
         USING TACOD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   COMPL12                                                          
         CLC   TACOPDTE,TGTODAY1   IF LAST PAID DATE NOT SET TO TODAY           
         BE    COMPL12                                                          
         MVC   TACOPDTE,TGTODAY1   SET LAST PAID DATE                           
         DROP  R4                                                               
         SPACE 1                                                                
COMPL12  MVC   TGBYTE4,TWASCR      SAVE SCREEN                                  
         MVI   TWASCR,SCR50        ADD ACTIVITY ELEMENT WITH SCREEN 50          
         GOTO1 ACTVIN,DMCB,(X'80',0)                                            
         MVC   TWASCR,TGBYTE4      AND RESET SCREEN                             
         SPACE 1                                                                
         GOTO1 MYPUTREC            WRITE BACK CHANGED RECORD                    
         GOTO1 ADDPTRS,DMCB,(X'08',BLOCK),BLOCK+200                             
         NI    TGGNSTAT,X'FF'-TGJSTCOE                                          
         SPACE                                                                  
COMPL13  TM    PAYOPTS3,ODUMMY     DONE IF DUMMY PAYMENT                        
         BO    XIT                                                              
         SPACE 1                                                                
         TM    PAYOPTS3,ORETRO     IF NOT MAKING RETRO PAYMENT                  
         BO    COMPL13B                                                         
         BRAS  RE,VERUH            SPECIAL UH PROCESSING FOR VERSIONS           
         BRAS  RE,BLDUH            BUILD NEW UH REC IN AIO W/DV IN AIO2         
         SPACE 1                                                                
         TM    TGUSSTA3,USEMTAB                                                 
         BZ    COMPL13A                                                         
         L     RE,AIO3                       ADD MKT/CNET/CSYS ELEMENTS         
         XC    0(L'CASTHEAD,RE),0(RE)                      TO UH RECORD         
         BRAS  RE,MKTBLDUH                                                      
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
COMPL13A GOTO1 MYADDREC            ADD RECORD TO FILE                           
         SPACE 1                                                                
         TM    PAYMODE,DRAFT       DONE IF DRAFT - DIDN'T WRITE SCREEN          
         BO    XIT                 RECS AND DON'T WRITE TO REQUEST FILE         
         SPACE 1                                                                
         XC    BLOCK(200),BLOCK                                                 
         GOTO1 ADDPTRS,DMCB,(X'08',BLOCK),UHMTPTRS                              
         SPACE 1                                                                
         USING TLUHD,RE                                                         
         OC    INV04A,INV04A       IF GENERATING TWO INVOICES FOR               
         BZ    COMPL13B            ACTRA TYPE 2404A COMMERCIAL                  
         L     RE,AIO              ADD USAGE HISTORY RECORD FOR                 
         MVC   TLUHINV,INV04A      CANADIAN INVOICE TOO                         
         BRAS  RE,ADJUHCYE         ADJUST CYCLE END DATE FOR CAN INV            
         GOTO1 MYADDREC                                                         
         XC    BLOCK(200),BLOCK                                                 
         GOTO1 ADDPTRS,DMCB,(X'08',BLOCK),UHMTPTRS                              
         DROP  RE                                                               
         SPACE 1                                                                
COMPL13B TM    PAYMODE,DRAFT       DONE IF DRAFT - DIDN'T WRITE SCREEN          
         BO    XIT                 RECS AND DON'T WRITE TO REQUEST FILE         
         BRAS  RE,COMPVNR          COMPLETE VNR PAYMENT                         
         SPACE 1                                                                
         TM    PAYMODE,DTLONBSE    IF DETAIL ON BASE SCREEN                     
         BZ    COMPL14                                                          
         TM    TGFASTAT,TGFROMFA   AND PAYMENT IS NOT COMING FROM WEB           
         BO    COMPL14                                                          
         BRAS  RE,ADDGQFQ          ADD GQ AND FQ ELS TO BASE SCREEN REC         
         SPACE                                                                  
COMPL14  CLI   TGCTSTTY,TASTTYPC   IF CLIENT WITH AUTO APPROVE,                 
         BNE   XIT                 ADD GRT TRACKING REQUESTS                    
         LA    R2,GQTABLE                                                       
         LA    R0,NGQTAB           R0=MAX N'ENTRIES IN GQTABLE                  
COMPL15  CLI   0(R2),0             TEST IF END OF ENTRIES                       
         BE    XIT                                                              
         SPACE                                                                  
         GOTO1 GQEXT,DMCB,(1,(R2))  ADD REQUESTS ONE AT A TIME                  
         SPACE                                                                  
         LA    R2,L'TAGQSBEL(R2)    BUMP TO NEXT ENTRY                          
         BCT   R0,COMPL15                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS LOCAL ACCUMS FOR CHECK RECORDS                
         SPACE 1                                                                
PROCLCL  NTR1                                                                   
         MVI   TCW4TYPE,TAW4TYCO   SET TYPE TO BE CORPORATION                   
         MVC   TGCSORT,HEXFFS          SORT KEY                                 
         MVC   TGCAT,=C'ZZZ'           CATEGORY                                 
         SPACE                                                                  
         TM    TCSTAT2,TCSTCAN$    IF NOT CAN$                                  
         BO    *+8                                                              
         BRAS  RE,ADJHNW           ADJUST H&W AMOUNTS                           
         SPACE                                                                  
         OC    THNW,THNW           IF HAVE H&W FOR THIS INVOICE                 
         BZ    PRCL1                                                            
         MVC   TGUNI,=C'AFM'       SET GLOBAL UNION AFM                         
         MVI   TGUNEQU,AFM         AND UNION EQUATE                             
         MVI   TGUNEQU1,AFM                                                     
         BAS   RE,PROCLAFM         PROCESS LOCAL CHECK RECORD FOR AFM           
         B     PRCLX                                                            
         SPACE                                                                  
PRCL1    CLI   TGUNEQU,ACT                                                      
         BE    PRCL3                                                            
         CLI   TGUNEQU,UDA                                                      
         BE    PRCL3                                                            
         CLI   TGUNEQU,SAG                                                      
         BE    PRCL3                                                            
*RCL1    TM    TGUSXUNI,DGA        IF SOAP DIRECTOR PAYMENT                     
         GOTO1 UNITEST,DMCB,TGUSXUNS,DGA,0,0,0                                  
         BO    PRCL2                                                            
         MVC   TGUNI,=C'DGA'       SET GLOBAL UNION DGA                         
         MVI   TGUNEQU,DGA         AND UNION EQUATE                             
         MVI   TGUNEQU1,DGA                                                     
         LA    R2,LCLDGA           R2=A(ENTRY IN ACCUMS)                        
         LA    R0,LCLNDGA          R0=N'ACCUMS                                  
         BAS   RE,PROCLCHK         PROCESS LOCAL CHECK RECORD                   
         B     PRCLX                                                            
         SPACE                                                                  
*RCL2    TM    TGUSXUNI,WGA        IF SOAP WRITER PAYMENT                       
PRCL2    GOTO1 UNITEST,DMCB,TGUSXUNS,WGA,0,0,0                                  
         BO    PRCL3                                                            
         MVC   TGUNI,=C'WGA'       ELSE SET GLOBAL UNION WGA                    
         MVI   TGUNEQU,WGA         AND UNION EQUATE                             
         MVI   TGUNEQU1,WGA                                                     
         BAS   RE,PROCLWGA         PROCESS LOCAL CHECK RECORDS FOR WGA          
         B     PRCLX                                                            
         SPACE                                                                  
PRCL3    OC    TINR,TINR           IF HAVE I&R FOR THIS INVOICE                 
         BZ    PRCL5                                                            
         MVC   TGUNI,=C'ACT'       SET GLOBAL UNION ACT                         
         MVI   TGUNEQU,ACT         AND UNION EQUATE                             
         MVI   TGUNEQU1,ACT                                                     
         LA    R2,LCLACT           R2=A(ENTRY IN ACCUMS)                        
         LA    R0,LCLNACT          R0=N'ACCUMS                                  
         BAS   RE,PROCLCHK         PROCESS LOCAL CHECK RECORD                   
         SPACE                                                                  
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         BE    PRCLX               A WEB TRANSACATION, ABORT NOW                
         SPACE 1                                                                
         MVC   TGUNI,=C'UDA'       SET GLOBAL UNION UDA                         
         MVI   TGUNEQU,UDA         AND UNION EQUATE                             
         MVI   TGUNEQU1,UDA                                                     
         LA    R2,LCLUDA           R2=A(ENTRY IN ACCUMS)                        
         LA    R0,LCLNUDA          R0=N'ACCUMS                                  
         BAS   RE,PROCLCHK         PROCESS LOCAL CHECK RECORD                   
         SPACE                                                                  
         BRAS  RE,WEBERRS          IF THERE ARE ERRORS THAT TERMINATE           
         BE    PRCLX               A WEB TRANSACATION, ABORT NOW                
         SPACE 1                                                                
PRCL5    OC    TACTWRK,TACTWRK     IF HAVE ACTRAWRKS CONTR FOR THIS INV         
         BZ    PRCL7                                                            
         BAS   RE,PROCACTR         PROCESS CHECK RECORD TO ACTRAWORKS           
*                                                                               
PRCL7    OC    TACTAOS,TACTAOS     IF HAVE ACTRA AOS CONTR FOR THIS INV         
         BZ    PRCLX                                                            
         BAS   RE,PROCAOS          PROCESS CHECK RECORD TO ACTRA AOS            
PRCLX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS CHECK REC FOR ACTRAWORKS                      
         SPACE 1                                                                
PROCACTR NTR1                                                                   
         MVC   TGUNI,=C'ACT'       SET GLOBAL UNION ACT                         
         MVI   TGUNEQU,ACT         AND UNION EQUATE                             
         MVI   TGUNEQU1,ACT                                                     
         MVC   TGSSN,=C'000006816' SET GLOBAL SSN                               
         L     R3,TACTWRK          R3=ACTRAWORKS CONTRIBUTION                   
         XR    R4,R4               R4=MISC DED AMOUNT                           
         GOTO1 LCLCHECK,DMCB,(R3),(R4),0  BUILD CHECK RECORD                    
         SPACE 1                                                                
         BRAS  RE,BLDUPG           BUILD UPGR DETAILS EL. FOR VERSIONS          
         SPACE 1                                                                
         MVC   SYSFIL,=CL8'CHKFIL' SET SYSFIL AND SYSDIR FOR CHECKS             
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         GOTO1 MYADDREC            AND ADD IT TO FILE                           
         BAS   RE,ADDUP            ADD TO INVOICE TOTALS                        
         SPACE 1                                                                
         TM    PAYMODE,DRAFT                                                    
         BO    PROCAC10                                                         
         GOTO1 ADDPTRS,DMCB,CHKPTRS  ADD PASSIVE PTRS                           
         SPACE 1                                                                
PROCAC10 MVC   SYSFIL,SVSYSFIL     SET SYSFIL AND SYSDIR TO DEFAULTS            
         MVC   SYSDIR,SVSYSDIR                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS LOCAL CHECK REC FOR ACT, UDA AND DGA          
*              R2=A(LOCAL TABLE), R0=N'ACCUMS IN TABLE                          
         SPACE 1                                                                
         USING LCLTABD,R2                                                       
PROCLCHK NTR1                                                                   
PROCL5   OC    0(LCLTLNQ,R2),0(R2)  IF NOT END OF ENTRIES                       
         BZ    XIT                                                              
         OC    LCLTFST(8),LCLTFST   AND AMOUNTS NOT 0                           
         BZ    PROCL17                                                          
         GOTO1 RECVAL,DMCB,TLLOCDQ,(X'A0',LCLLOCAL) GET LOCAL RECORD            
         BNE   NOLOCAL                                                          
         SPACE                                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TALOELQ      GET LOCAL ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    PROCL6                                                           
         CLC   TGUNI,=C'DGA'                                                    
         BNE   PROCL15                                                          
         DC    H'0'                MUST HAVE ELEMENT FOR DGA                    
         SPACE                                                                  
         USING TALOD,R4                                                         
PROCL6   OC    TALOSSN,TALOSSN     TEST HAVE SS NUMBER FOR FUND                 
         BNZ   PROCL7                                                           
         CLC   TGUNI,=C'DGA'                                                    
         BNE   PROCL15                                                          
         DC    H'0'                MUST HAVE FUND FOR DGA                       
         SPACE                                                                  
PROCL7   MVC   TGSSN,TALOSSN       SET GLOBAL SSN                               
         SPACE 1                                                                
         CLC   TGUNI,=C'ACT'                    IF ACTRA                        
         BNE   PROCL8                                                           
         GOTO1 CHAROUT,DMCB,TANUELQ,0,TANUT2ND  GET 2ND SS NUMBER               
         BE    *+6                                                              
         DC    H'0'                             MUST HAVE IT                    
         MVC   WORK2(L'TGSSN),TGNAME            SAVE IT IN WORK2                
         SPACE 1                                                                
PROCL8   L     R3,LCLTFST          R3=I&R OR P&H AMOUNT                         
         L     R4,LCLMDED          R4=MISC. DED AMOUNT                          
         LCR   R4,R4               COMPLEMENT IT                                
         SPACE 1                                                                
         GOTO1 LCLCHECK,DMCB,(R3),(R4),0  BUILD CHECK RECORD                    
         SPACE 1                                                                
         BRAS  RE,BLDUPG           BUILD UPGR DETAILS EL. FOR VERSIONS          
         SPACE 1                                                                
         MVC   SYSFIL,=CL8'CHKFIL' SET SYSFIL AND SYSDIR FOR CHECKS             
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         GOTO1 MYADDREC            AND ADD IT TO FILE                           
         BAS   RE,ADDUP            ADD TO INVOICE TOTALS                        
         SPACE 1                                                                
         TM    PAYMODE,DRAFT                                                    
         BO    PROCL10                                                          
         GOTO1 ADDPTRS,DMCB,CHKPTRS  ADD PASSIVE PTRS                           
         SPACE 1                                                                
PROCL10  MVC   SYSFIL,SVSYSFIL     SET SYSFIL AND SYSDIR TO DEFAULTS            
         MVC   SYSDIR,SVSYSDIR                                                  
         SPACE                                                                  
         L     R4,LCLDUES          R4=UNION DUES AMOUNT                         
         LTR   R4,R4               TEST NOT 0                                   
         BZ    PROCL17                                                          
         LCR   R4,R4               COMPLEMENT IT                                
         MVC   TGSSN,WORK2         SET GLOBAL SSN                               
         SPACE 1                                                                
         GOTO1 LCLCHECK,DMCB,0,(R4),(X'80',0)  BUILD CHECK RECORD               
         SPACE 1                                                                
         BRAS  RE,BLDUPG           BUILD UPGR DETAILS EL. FOR VERSIONS          
         SPACE 1                                                                
         MVC   SYSFIL,=CL8'CHKFIL' SET SYSFIL AND SYSDIR FOR CHECKS             
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         GOTO1 MYADDREC            AND ADD IT TO FILE                           
         BAS   RE,ADDUP            ADD TO INVOICE TOTALS                        
         SPACE 1                                                                
         TM    PAYMODE,DRAFT                                                    
         BO    PROCL12                                                          
         GOTO1 ADDPTRS,DMCB,CHKPTRS  ADD PASSIVE PTRS                           
         SPACE 1                                                                
PROCL12  MVC   SYSFIL,SVSYSFIL     SET SYSFIL AND SYSDIR TO DEFAULTS            
         MVC   SYSDIR,SVSYSDIR                                                  
         B     PROCL17                                                          
         SPACE                                                                  
PROCL15  OI    LCLSTAT2,NOFUND     IF NO FUND FOR I&R LOCAL, SET BIT            
         SPACE                                                                  
PROCL17  LA    R2,LCLTLNQ(R2)      BUMP TO NEXT ENTRY                           
         BCT   R0,PROCL5                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS LOCAL CHECK REC FOR WGA                       
         SPACE 1                                                                
         USING LCLTABD,R2                                                       
PROCLWGA NTR1                                                                   
         LA    R2,LCLWGA            R2=A(ENTRY IN ACCUMS)                       
         LA    R0,LCLNWGA           R0=N'ACCUMS                                 
PRLW5    OC    0(LCLTLNQ,R2),0(R2)  IF NOT END OF ENTRIES                       
         BZ    XIT                                                              
         OC    LCLTFST(8),LCLTFST   AND AMOUNTS NOT 0                           
         BZ    PRLW17                                                           
         GOTO1 RECVAL,DMCB,TLLOCDQ,(X'A0',LCLLOCAL) GET LOCAL RECORD            
         BNE   NOLOCAL                                                          
         SPACE                                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TALOELQ      GET LOCAL ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE                                                                  
         USING TALOD,R4                                                         
PRLW6    OC    TALOSSN,TALOSSN     TEST HAVE SS NUMBER FOR FUND                 
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE IT                                 
         SPACE                                                                  
         MVC   TGSSN,TALOSSN       SET GLOBAL SSN                               
         SPACE                                                                  
         GOTO1 CHAROUT,DMCB,TANUELQ,0,TANUT2ND  GET 2ND SS NUMBER               
         BE    *+6                                                              
         DC    H'0'                             MUST HAVE IT                    
         MVC   WORK2(L'TGSSN),TGNAME            SAVE IT IN WORK2                
         SPACE 1                                                                
         L     R3,LCLPENS          R3=PENSION AMOUNT                            
         LTR   R3,R3                                                            
         BZ    PRLW8                                                            
         SPACE 1                                                                
         GOTO1 LCLCHECK,DMCB,(R3),0,0  BUILD CHECK RECORD                       
         SPACE 1                                                                
         MVC   SYSFIL,=CL8'CHKFIL' SET SYSFIL AND SYSDIR FOR CHECKS             
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         GOTO1 MYADDREC            AND ADD IT TO FILE                           
         SPACE 1                                                                
         BAS   RE,ADDUP            ADD TO INVOICE TOTALS                        
         SPACE 1                                                                
         TM    PAYMODE,DRAFT                                                    
         BO    PRLW7                                                            
         GOTO1 ADDPTRS,DMCB,CHKPTRS  ADD PASSIVE PTRS                           
         SPACE 1                                                                
PRLW7    MVC   SYSFIL,SVSYSFIL     SET SYSFIL AND SYSDIR TO DEFAULTS            
         MVC   SYSDIR,SVSYSDIR                                                  
         SPACE 1                                                                
PRLW8    L     R3,LCLHLTH          R3=HEALTH AMOUNT                             
         LTR   R3,R3                                                            
         BZ    PRLW17                                                           
         MVC   TGSSN,WORK2         SET GLOBAL SSN                               
         SPACE 1                                                                
         GOTO1 LCLCHECK,DMCB,(R3),0,0   BUILD CHECK RECORD                      
         SPACE 1                                                                
         MVC   SYSFIL,=CL8'CHKFIL' SET SYSFIL AND SYSDIR FOR CHECKS             
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         GOTO1 MYADDREC            AND ADD IT TO FILE                           
         SPACE 1                                                                
         BAS   RE,ADDUP            ADD TO INVOICE TOTALS                        
         SPACE 1                                                                
         TM    PAYMODE,DRAFT                                                    
         BO    PRLW10                                                           
         GOTO1 ADDPTRS,DMCB,CHKPTRS  ADD PASSIVE PTRS                           
         SPACE 1                                                                
PRLW10   MVC   SYSFIL,SVSYSFIL     SET SYSFIL AND SYSDIR TO DEFAULTS            
         MVC   SYSDIR,SVSYSDIR                                                  
         SPACE                                                                  
PRLW17   LA    R2,LCLTLNQ(R2)      BUMP TO NEXT ENTRY                           
         BCT   R0,PRLW5                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS LOCAL CHECK RECORD FOR AFM                    
         SPACE 1                                                                
PROCLAFM NTR1                                                                   
         LA    R2,LCLAFM           R2=A(ENTRY IN ACCUMS)                        
         LA    R0,LCLNAFM          R0=N'ACCUMS                                  
PRLA5    OC    0(L'LCLAFM,R2),0(R2) IF NOT END OF ENTRIES                       
         BZ    XIT                                                              
         OC    0(4,R2),0(R2)        AND AMOUNT NOT 0                            
         BZ    PRLA17                                                           
         GOTO1 RECVAL,DMCB,TLLOCDQ,(X'A0',4(R2)) GET LOCAL RECORD               
         BNE   NOLOCAL                                                          
         SPACE                                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TALOELQ      GET LOCAL ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    PRLA6                                                            
         TM    TCSTAT2,TCSTCAN$    IF NOT CAN$ PAYMENT - MUST BE H&W            
         BO    PRLA15                 (ELSE I&R)                                
         DC    H'0'                MUST HAVE ELEMENT                            
         SPACE                                                                  
         USING TALOD,R4                                                         
PRLA6    OC    TALOSSN,TALOSSN     TEST HAVE SS NUMBER FOR FUND                 
         BNZ   PRLA7                                                            
         TM    TCSTAT2,TCSTCAN$    IF NOT CAN$ PAYMENT - MUST BE H&W            
         BO    PRLA15              ELSE I&R                                     
         DC    H'0'                MUST HAVE FUND FOR H&W                       
         SPACE                                                                  
PRLA7    MVC   TGSSN,TALOSSN       SET GLOBAL SSN                               
         SPACE 1                                                                
         L     R3,0(R2)                R3=AMOUNT                                
         GOTO1 LCLCHECK,DMCB,(R3),0,0  BUILD CHECK RECORD                       
         SPACE 1                                                                
         USING TAPDD,R4                                                         
         TM    PAYOPTS1,OCANTAX    IF USING C=Y OPTION FOR CANADIAN TAX         
         BZ    PRLA8                                                            
         L     R4,AIO              GET CHECK RECORD                             
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL            GET PAYMENT DETAILS ELEMENT                  
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    TAPDOPT1,ALL-TAPDOCAN  TURN OFF BIT TO TAKE CANADIAN TAX         
         SPACE 1                                                                
PRLA8    MVC   SYSFIL,=CL8'CHKFIL' SET SYSFIL AND SYSDIR FOR CHECKS             
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         GOTO1 MYADDREC            AND ADD IT TO FILE                           
         SPACE 1                                                                
         TM    PAYMODE,DRAFT                                                    
         BO    PRLA10                                                           
         GOTO1 ADDPTRS,DMCB,CHKPTRS  ADD PASSIVE PTRS                           
         SPACE 1                                                                
PRLA10   MVC   SYSFIL,SVSYSFIL     SET SYSFIL AND SYSDIR TO DEFAULTS            
         MVC   SYSDIR,SVSYSDIR                                                  
         SPACE 1                                                                
         BAS   RE,ADDUP            ADD TO INVOICE TOTALS                        
         B     PRLA17                                                           
         SPACE                                                                  
PRLA15   OI    LCLSTAT2,NOFUND     IF NO FUND FOR I&R LOCAL, SET BIT            
         SPACE                                                                  
PRLA17   LA    R2,L'LCLAFM(R2)     BUMP TO NEXT ENTRY                           
         BCT   R0,PRLA5                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS AGENT ACCUMS FOR CHECK RECORDS                
         SPACE 1                                                                
         USING AGTTABD,R2                                                       
PROCAGT  NTR1                                                                   
         CLI   TGUSMEDS,PRINT      ONLY FOR PRINT                               
         BNE   XIT                                                              
         LA    R2,AGTTAB           R2=A(ENTRY IN AGENT TABLE)                   
         LA    R0,MAXAGT           R0=MAX N'AGENTS                              
PROCAG5  OC    AGTTAGT,AGTTAGT     IF NOT END OF ENTRIES                        
         BZ    XIT                                                              
         OC    AGTTAMT,AGTTAMT     AND AMOUNT NOT 0                             
         BZ    PROCAG17                                                         
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'A0',AGTTAGT) GET AGENT RECORD             
         BNE   AGTMIS                                                           
         SPACE                                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TAANELQ      GET AGENT ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE                                                                  
         USING TAAND,R4                                                         
         OC    TAANSSN,TAANSSN     TEST HAVE SS NUMBER FOR AGENT                
         BZ    ASSMIS                                                           
         MVC   TGSSN,TAANSSN       SET GLOBAL SSN                               
         SPACE 1                                                                
         L     R3,AGTTAMT          R3=AGENT FEE AMOUNT                          
         SPACE 1                                                                
         GOTO1 LCLCHECK,DMCB,(R3),0,0  BUILD CHECK RECORD                       
         SPACE 1                                                                
         USING TANUD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     INIT ELEMENT                                 
         MVI   TANUEL,TANUELQ      BUILD TANUEL                                 
         MVI   TANULEN,12          SET LENGTH 3+9                               
         MVI   TANUTYPE,TANUTCST   SET TYPE                                     
         SPACE                                                                  
         LA    R4,AGTTSSN          R4=A(SSN ENTRY)                              
         LA    R3,MAXAGSSN         R3=MAX N'SSN PER AGENT                       
         SPACE 1                                                                
PROCAG7  OC    0(L'AGTTSSN,R4),0(R4)       TEST NOT END OF SSN ENTRIES          
         BZ    PROCAG9                                                          
         MVC   ELEMENT+3(L'AGTTSSN),0(R4)  PUT SSN IN ELEMENT                   
         GOTO1 ADDL                                                             
         SPACE                                                                  
         LA    R4,L'AGTTSSN(R4)    BUMP TO NEXT SSN                             
         BCT   R3,PROCAG7                                                       
         SPACE 1                                                                
PROCAG9  MVC   SYSFIL,=CL8'CHKFIL' SET SYSFIL AND SYSDIR FOR CHECKS             
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         GOTO1 MYADDREC            AND ADD IT TO FILE                           
         BAS   RE,ADDUP            ADD TO INVOICE TOTALS                        
         SPACE 1                                                                
         TM    PAYMODE,DRAFT                                                    
         BO    PROCAG10                                                         
         GOTO1 ADDPTRS,DMCB,CHKPTRS  ADD PASSIVE PTRS                           
         SPACE 1                                                                
PROCAG10 MVC   SYSFIL,SVSYSFIL     SET SYSFIL AND SYSDIR TO DEFAULTS            
         MVC   SYSDIR,SVSYSDIR                                                  
         SPACE                                                                  
PROCAG17 LA    R2,AGTTNXT          BUMP TO NEXT ENTRY                           
         BCT   R0,PROCAG5                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS CONTRACT SERVICE FEE                          
*              GENERATES CSF CHECK FOR LOCAL WITH THE MOST CAST MEMBERS         
         SPACE 1                                                                
PROCCSF  NTR1                                                                   
         TM    PAYOPTS4,OGENCSF    IF CSF OPTION USED FOR BSC PYMT              
         BZ    PROCCSFX                                                         
         MVC   TGUNI,=C'ACT'       SET GLOBAL UNION ACT                         
         MVI   TGUNEQU,ACT         AND UNION EQUATE                             
         MVI   TGUNEQU1,ACT                                                     
         SPACE 1                                                                
         USING LCLTABD,R2                                                       
         LA    R2,LCLACT           R2=A(ACT TABLE)                              
         LA    R0,LCLNACT          R0=N'ACCUMS                                  
         XR    R1,R1               R1=N'CHECKS GENERATED PER LOCAL              
         SPACE 1                                                                
PROCCSF1 OC    LCLTFST(LCLTLNQ),LCLTFST                                         
         BZ    PROCCSF4                                                         
         CLM   R1,1,LCLTNCHK       CHK THIS LOCAL'S COUNT AGAINST PREV          
         BH    PROCCSF2            PREVIOUS HIGHER - KEEP IT                    
         BL    *+14                PREVIOUS LOWER - REPLACE IT                  
         CLC   =C'TOR',LCLLOCAL    EQUAL TO PREVIOUS - TORONTO WINS             
         BNE   PROCCSF2                                                         
         ZIC   R1,LCLTNCHK         SET TO USE THIS ONE FOR CSF CHECK            
         BRAS  RE,SETGSSN          SET CORRECT SSN FOR LOCAL                    
PROCCSF2 LA    R2,LCLTLNQ(R2)      BUMP TO NEXT ENTRY                           
         BCT   R0,PROCCSF1         AND KEEP SEARCHING THROUGH LOCALS            
         SPACE 1                                                                
PROCCSF4 LTR   R1,R1               IF LOCAL FOUND                               
         BZ    PROCCSFX                                                         
         L     R3,OPTCSF$          AND IF OVERRIDE AMOUNT SPECIFIED             
         LTR   R3,R3                                                            
         BNZ   PROCCSF8            USE IT                                       
         SPACE 1                                                                
         L     R3,SVCSFMAX         R3=MAX CSF AMOUNT                            
         LA    R0,2500             SET TV CSF PERCENTAGE                        
         TM    TGMEEQU,RADIO       IF IT IS A RADIO COMM'L                      
         BZ    *+8                                                              
         LA    R0,2000             SET RADIO CSF PERCENTAGE                     
         LTR   R3,R3               IF CONTRACT SERVICE FEE AMOUNT               
         BZ    PROCCSFX                                                         
         TM    PAYSTAT1,CREDIT     IF CREDIT PAYMENT                            
         BZ    PROCCSF6                                                         
         LCR   R3,R3               REVERSE SIGN                                 
         SPACE 1                                                                
PROCCSF6 L     R1,SAVTGROS         R1=GROSS OF PERF WAGES                       
         BAS   RE,MULTR0           COMPUTE CSF PERCENTAGE OF GROSS              
         TM    PAYSTAT1,CREDIT     IF CREDIT PAYMENT                            
         BZ    PROCCSF7                                                         
         CR    R3,R1               IF MAX CSF >= COMPUTED CSF                   
         BNL   PROCCSF8            GO SAVE MAX CSF                              
         B     *+10                                                             
PROCCSF7 CR    R3,R1               IF MAX CSF <= COMPUTED CSF                   
         BNH   *+6                 GO SAVE MAX CSF                              
         LR    R3,R1               ELSE R3=COMPUTED CSF                         
PROCCSF8 ST    R3,CSFAMT           SAVE CSF AMOUNT FOR TABDD                    
         GOTO1 LCLCHECK,DMCB,(R3),0,0  BUILD CHECK RECORD                       
         MVC   SYSFIL,=CL8'CHKFIL' SET SYSFIL AND SYSDIR FOR CHECKS             
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         GOTO1 MYADDREC            AND ADD IT TO FILE                           
         BAS   RE,ADDUP            ADD TO INVOICE TOTALS                        
         TM    PAYMODE,DRAFT                                                    
         BO    PROCCSF9                                                         
         GOTO1 ADDPTRS,DMCB,CHKPTRS  ADD PASSIVE PTRS                           
PROCCSF9 MVC   SYSFIL,SVSYSFIL     SET SYSFIL AND SYSDIR TO DEFAULTS            
         MVC   SYSDIR,SVSYSDIR                                                  
         SPACE 1                                                                
PROCCSFX B     XIT                                                              
         SPACE 2                                                                
MULTR0   MR    R0,R0               USE R0                                       
*                                                                               
MULTALL  D     R0,=F'5000'                                                      
MULTALLX LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         BR    RE                  RETURN ANSWER IN R1                          
         EJECT                                                                  
*              ROUTINE TO CALL PAYMENT OVERLAYS                                 
         SPACE 1                                                                
GOOVLY   NTR1                                                                   
         GOTO1 APAYOVLY                                                         
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO PROCESS CHECK REC FOR ACTRA AOS                       
*---------------------------------------------------------------------          
PROCAOS  NTR1                                                                   
         MVC   TGUNI,=C'ACT'       SET GLOBAL UNION ACT                         
         MVI   TGUNEQU,ACT         AND UNION EQUATE                             
         MVI   TGUNEQU1,ACT                                                     
         MVC   TGSSN,=C'000112843' SET GLOBAL SSN DEFAULT ONTARIO               
         OC    TCAOSSSN,TCAOSSSN   CONVERT SAVED SSN TO CHAR                    
         BZ    PROCAS05                                                         
         EDIT  (B4,TCAOSSSN),(9,TGSSN),0,FILL=0                                 
PROCAS05 L     R0,TACTAOS          CALC 1% FOR AOS                              
         CVD   R0,DUB                                                           
         SRP   DUB,62,5                                                         
         CVB   R0,DUB                                                           
         LR    R3,R0               R3=ACTRA AOS                                 
         XR    R4,R4               R4=MISC DED AMOUNT                           
         GOTO1 LCLCHECK,DMCB,(R3),(R4),0  BUILD CHECK RECORD                    
*                                                                               
         BRAS  RE,BLDTAMA          BUILD TAMA EL. FOR AOS                       
         BRAS  RE,BLDUPG           BUILD UPGR DETAILS EL. FOR VERSIONS          
*                                                                               
         MVC   SYSFIL,=CL8'CHKFIL' SET SYSFIL AND SYSDIR FOR CHECKS             
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         GOTO1 MYADDREC            AND ADD IT TO FILE                           
         BAS   RE,ADDUP            ADD TO INVOICE TOTALS                        
*                                                                               
         TM    PAYMODE,DRAFT                                                    
         BO    PROCAS10                                                         
         GOTO1 ADDPTRS,DMCB,CHKPTRS  ADD PASSIVE PTRS                           
*                                                                               
PROCAS10 MVC   SYSFIL,SVSYSFIL     SET SYSFIL AND SYSDIR TO DEFAULTS            
         MVC   SYSDIR,SVSYSDIR                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS/EXITS                                                     
         SPACE 1                                                                
ENTERFLD MVI   MYMSGNO1,2          PLEASE ENTER FIELDS AS REQUIRED              
         MVI   MYMSYS,X'FF'                                                     
         L     R2,AFRSTREC                                                      
         B     INFOEND                                                          
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
AMTINV   MVI   ERROR,ERINVAMT      INVALID AMOUNT                               
         B     THEEND                                                           
         SPACE 1                                                                
TOOBIG   MVI   ERROR,ERTOOBIG      PAYMENT TOO BIG                              
         B     THEEND                                                           
         SPACE 1                                                                
CLADUEPF MVI   ERROR,ERCLADUE      DUE DATE BEFORE PREV USES - PF20             
         LH    R2,CURDISP                                                       
         AR    R2,RA               RETURN CURSOR TO SAME SPOT                   
         B     THEEND                                                           
         SPACE 1                                                                
PFERR    MVI   ERROR,ERINVPFK      INVALID PFKEY FOR THIS SCREEN                
         L     R2,ADTLCURH                                                      
         B     THEEND                                                           
         SPACE 1                                                                
NP04AERR MVC   MYMSGNO,=Y(ERRNP04A) NP INVALID FOR 2404A COMMERCIALS            
         B     NTHEEND                                                          
         SPACE 1                                                                
NTHEEND  MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     INFOEND                                                          
         SPACE 1                                                                
NONEPAID MVI   ERROR,ERNONEPD      NO ONE PAID                                  
NONEEROR TM    TRNSTAT,PYINPROG    IF PAY UPDATE IN PROGRESS                    
         BZ    *+8                                                              
         OI    TRNSTAT,TRNABEND    SET BIT FOR CONTROLLER TO ABEND              
         B     STRTOVER                                                         
NOCASTUH MVI   ERROR,NOTFOUND      CAST UH REC DISAPPEARED                      
         BAS   RE,RESTSCRN                                                      
         OI    TRNSTAT,TRNABEND    SET BIT FOR CONTROLLER TO ABEND              
         B     STRTOVER                                                         
ABORTPFK MVI   ERROR,ERPABORT      PAYMENT ABORTED - INV NOT PROCESSED          
         B     STRTOVER                                                         
BADINVER MVI   ERROR,ERBADINV      BAD INVOICE NUMBER, DUPL. TLINH KEY          
         B     STRTOVER                                                         
GRTLOCK  MVI   ERROR,ERCKLOCK      ACTION NOT ALLOWED - CHECK LOCKOUT           
         B     STRTOVER                                                         
AGYERR   MVI   ERROR,ERAGYERR      CAN'T ASSIGN NEW NUMBER - AGY ERROR          
         B     STRTOVER                                                         
INVNFND  MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     STRTOVER                                                         
INVPAID  MVI   ERROR,ERINVPD       INVOICE ALREADY PAID                         
         B     STRTOVER                                                         
GCONCHG  MVC   MYMSGNO,=Y(ERMUS519)  GCON CHANGED DURING PROCESSING             
         J     NTHEEND                                                          
                                                                                
         SPACE 1                                                                
***********************************************************************         
*        CODE TO HANDLE "NO ONE ELIGIBLE FOR PAYMENT' ERROR           *         
***********************************************************************         
                                                                                
         USING WEBRESD,R1                                                       
NONEELIG TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         BZ    NELIG3                                                           
         L     R1,TGAFARES                                                      
NELIG1   CLI   0(R1),X'FF'         AND THE REASON THAT A PERFORMER              
         BE    NELIG2              IS NOT ELIGIBLE FOR PAYMENT HAS              
         TM    WRSSTAT,WRSSNERE    BEEN IDENTIFIED, JUST EXIT                   
         BO    XIT                                                              
         LA    R1,WRSLNQ(R1)       ELSE RETURN NO ONE ELIGIBLE                  
         B     NELIG1              FOR PAYMENT ERROR                            
                                                                                
         USING WEBREQD,RE                                                       
NELIG2   CLI   TGUSEQU,UPRM                                                     
         JE    NELIG2A                                                          
         TM    TGUSSTA2,NORATES                                                 
         JZ    NELIG2B                                                          
NELIG2A  L     RE,TGAFAREQ                                                      
         CLI   WBMODE,WBMDEXE                                                   
         JNE   XIT                                                              
NELIG2B  GOTO1 ADDERROR,DMCB,ERNONELI,TGAERTAB                                  
         B     XIT                                                              
         DROP  R1,RE                                                            
                                                                                
NELIG3   MVI   ERROR,ERNONEEL      NO ONE ELIGIBLE FOR PAYMENT                  
         B     NONEEROR                                                         
                                                                                
ERNONELI DC    AL1(ENONELIX-*),AL2(ERNONEEL),AL1(ERRCATY3),AL1(D#PYCST)         
         DC    AL2(0)                                                           
         DC    CL60'No one eligible for payment'                                
ENONELIX EQU   *                                                                
                                                                                
***********************************************************************         
*        CODE TO HANDLE "MISSING CAST RECORD" ERROR                   *         
***********************************************************************         
                                                                                
MISSCAST MVI   MYMSGNO1,ERABCAST   MISSING CAST RECORD                          
         MVI   BLOCK,10            BUILD SUBST BLOCK FOR GETTXT                 
         MVC   BLOCK+1(9),TGSSN    SHOW SSN                                     
         MVI   BLOCK+10,4                                                       
         MVC   BLOCK+11(3),TGCAT   AND CATEGORY                                 
         MVI   BLOCK+14,0                                                       
         B     DOABEND                                                          
                                                                                
***********************************************************************         
*        CODE TO HANDLE "MISSING GUARANTEE RECORD" ERROR              *         
***********************************************************************         
                                                                                
NOGRT    TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         BZ    NOGRT1              ADD TO ERROR BLOCK                           
         GOTO1 ADDERROR,DMCB,ERNOGRT                                            
         B     XIT                                                              
                                                                                
NOGRT1   MVI   MYMSGNO1,ERABGRT    MISSING GRT RECORD                           
         MVI   BLOCK,5             BUILD SUBST BLOCK FOR GETTXT                 
         MVC   BLOCK+1(4),TGGUA    SHOW GRT CODE                                
         XC    BLOCK+1(4),HEXFFS                                                
         MVI   BLOCK+5,10                                                       
         MVC   BLOCK+6(9),TGSSN    AND SSN                                      
         MVI   BLOCK+15,0                                                       
         B     DOABEND                                                          
                                                                                
ERNOGRT  DC    AL1(ENOGRTX-*),AL2(ERABGRT),AL1(ERRCATY1),AL1(D#PYCST)           
         DC    AL2(0)                                                           
         DC    C'Missing guarantee record'                                      
ENOGRTX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CODE TO HANDLE "MISSING LOCAL RECORD" ERROR                  *         
***********************************************************************         
                                                                                
NOLOCAL  TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         BZ    NOLOCAL1            ADD TO ERROR BLOCK                           
         GOTO1 ADDERROR,DMCB,ERLCLMIS                                           
         B     XIT                                                              
                                                                                
NOLOCAL1 MVI   MYMSGNO1,ERABLOCL   MISSING UNION LCL RECORD                     
         MVI   BLOCK,8             BUILD SUBST BLOCK FOR GETTXT                 
         MVC   BLOCK+1(3),TGUNI    SHOW UNION CODE                              
         MVI   BLOCK+4,C' '                                                     
         MVC   BLOCK+5(3),TGLCL    AND LOCAL                                    
         MVI   BLOCK+8,0                                                        
         B     DOABEND                                                          
                                                                                
ERLCLMIS DC    AL1(ELCLMISX-*),AL2(ERABLOCL),AL1(ERRCATY1),AL1(D#PYCST)         
         DC    AL2(0)                                                           
         DC    C'The "Local" record indicated does not exist on the mai+        
               nframe'                                                          
ELCLMISX EQU   *                                                                
                                                                                
***********************************************************************         
*        CODE TO HANDLE "MISSING AGENT RECORD" ERROR                  *         
***********************************************************************         
                                                                                
AGTMIS   TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         BZ    AGTMIS1             ADD TO ERROR BLOCK                           
         GOTO1 ADDERROR,DMCB,ERAGTMIS                                           
         B     XIT                                                              
                                                                                
AGTMIS1  MVI   MYMSGNO1,ERABAGT    MISSING AGENT RECORD                         
         MVI   BLOCK,5             BUILD SUBST BLOCK FOR GETTXT                 
         MVC   BLOCK+1(4),TGAGT    SHOW AGENT CODE                              
         MVI   BLOCK+5,0                                                        
         B     DOABEND                                                          
                                                                                
ERAGTMIS DC    AL1(EAGTMISX-*),AL2(ERABAGT),AL1(ERRCATY1),AL1(D#PYCST)          
         DC    AL2(0)                                                           
         DC    C'The "Agent" record indicated does not exist on the mai+        
               nframe'                                                          
EAGTMISX EQU   *                                                                
                                                                                
***********************************************************************         
*        CODE TO HANDLE "MISSING SS# FOR AGENT" ERROR                 *         
***********************************************************************         
                                                                                
ASSMIS   TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         BZ    ASSMIS1             ADD TO ERROR BLOCK                           
         GOTO1 ADDERROR,DMCB,ERASSMIS                                           
         B     XIT                                                              
                                                                                
ASSMIS1  MVI   MYMSGNO1,ERABAGSS   MISSING SSN FOR AGENT                        
         MVI   BLOCK,5             BUILD SUBST BLOCK FOR GETTXT                 
         MVC   BLOCK+1(4),TGAGT    SHOW AGENT CODE                              
         MVI   BLOCK+5,0                                                        
         B     DOABEND                                                          
                                                                                
ERASSMIS DC    AL1(EASSMISX-*),AL2(ERABAGSS),AL1(ERRCATY1),AL1(D#PYCST)         
         DC    AL2(0)                                                           
         DC    C'The SS# for the agent does not exist on the mainframe'         
EASSMISX EQU   *                                                                
                                                                                
***********************************************************************         
*                                                                               
DOABEND  BAS   RE,RESTSCRN         RESTORE SCREEN                               
         OI    TRNSTAT,TRNABEND    SET BIT FOR CONTROLLER TO ABEND              
         B     ERRSUBS                                                          
         SPACE                                                                  
ERRSUBS  MVI   MYMTYP,GTMERR       SET ERROR MESSAGE TYPE                       
         OI    GENSTAT2,USGETTXT   USE GETTXT FOR SUBSTITUTION PARAMS           
         B     STRTOVER                                                         
         SPACE 1                                                                
DISPMSG  CLI   TGUSEQU,UEVE                                                     
         BE    DISPMSGE                                                         
         MVI   MYMSGNO1,4          CAST DISPLAYED - MORE TO COME                
         TM    LCLSTAT,FINISHED    UNLESS THIS IS LAST PAGE                     
         BZ    *+8                                                              
         MVI   MYMSGNO1,5          CAST DISPLAYED - LAST PAGE                   
         B     DTLEND                                                           
         SPACE 1                                                                
DISPMSGE MVC   MYMSGNO,=H'277'     EMPLOYEES DISPLAYED - MORE TO COME           
         TM    LCLSTAT,FINISHED    UNLESS THIS IS LAST PAGE                     
         BZ    *+10                                                             
         MVC   MYMSGNO,=H'278'     EMPLOYEES DISPLAYED - LAST PAGE              
         MVI   MYMTYP,GTMINF                                                    
         MVI   BLOCK,0                                                          
         B     DTLEND                                                           
         SPACE 1                                                                
CHGDMSG  MVI   MYMSGNO1,8          CHANGES ACCEPTED - MORE TO COME              
         TM    LCLSTAT,FINISHED    UNLESS THIS IS LAST PAGE                     
         BZ    *+8                                                              
         MVI   MYMSGNO1,9          CHANGES ACCEPTED - LAST PAGE                 
         B     ALLMSG                                                           
         SPACE 1                                                                
RDISPMSG MVI   MYMSGNO1,21         NEXT SCREEN RE-DISPLAYED - MORE ...          
         CLC   GPAGE,LPAGE         IF THIS IS LAST PAGE                         
         BL    *+16                                                             
         TM    LCLSTAT,FINISHED    AND WE PROCESSED ENTIRE CAST                 
         BZ    *+8                                                              
         MVI   MYMSGNO1,22         LAST SCREEN - ENTER TO COMPLETE              
         SPACE 1                                                                
ALLMSG   GOTO1 FLDVAL,DMCB,(X'20',ADTLFSTH),999 MARK ALL FIELDS VALID           
         B     DTLEND                                                           
         SPACE 1                                                                
STRTOVER TM    TRNSTAT,TRNABEND      IF NOT ABENDING                            
         BO    STRTO5                                                           
         GOTO1 DELSCRS               DELETE ANY SCREEN RECORDS WRITTEN          
STRTO5   TM    PRGSTAT,INITSARD      IF TSAR INITIALIZED (IN USE)               
         BZ    STRTO6                                                           
         GOTO1 TSARCNTL,DMCB,0       INDICATE NO LONGER USING TSAR              
STRTO6   NI    PRGSTAT,ALL-PAYINITD  SET TO START ALL OVER NEXT TIME            
         NI    TRNSTAT,ALL-PYINPROG  SET PAY UPDATE NOT IN PROGRESS             
         L     R2,EFHREC                                                        
         TM    TGFASTAT,TGFROMFA                                                
         BZ    THEEND                                                           
         B     XIT                                                              
DTLEND   L     R2,ADTLCURH                                                      
INFOEND  OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
HEXFFS   DC    6X'FF'                                                           
CODMSG   DC    C'(PUR',X'42',C'AGENCY)'                                         
CODMSG2  DC    C'(PUR',X'42',C'CLIENT)'                                         
         SPACE 1                                                                
IRNHD    DC    CL(L'IRNFHED)'Internet'                                          
NMRHD    DC    CL(L'IRNFHED)'New Media'                                         
         SPACE 1                                                                
CASTHEAD DC    C'CASTHEAD'                                                      
CNETHD   DC    CL(L'CBLHD1)'CNet'                                               
MKTHD    DC    CL(L'CBLHD1)'Mkt'                                                
CSYSHD   DC    CL(L'CBLHD1)'CSys'                                               
UNITHD   DC    CL(L'CBLUNS)'^Units^'                                            
SUBSHD   DC    CL(L'CBLUNS)'^  Subs'                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              READ SUBJ PNH FOR RETROS                                         
RETSPNH  NTR1  BASE=*,LABEL=*                                                   
         XC    CSTRSPNH,CSTRSPNH   CLEAR VARIABLES                              
         XC    CSTRPNHD,CSTRPNHD                                                
                                                                                
         TM    PAYOPTS3,ORETRO     IF RETRO PAYMENT                             
         JZ    XIT                                                              
         TM    PAYMODE,VALSCRN     AND NOT JUST VALIDATING ...                  
         JO    XIT                                                              
                                                                                
         CLI   TGUSEQU,UPNH        IF PNH PAYMENT                               
         JNE   RETSPNHA                                                         
         XC    TCSUBPNH,TCSUBPNH   ONLY COMPENSATE FOR ORIGINAL                 
         XC    TCPNH,TCPNH         P&H INCREAASE                                
                                                                                
RETSPNHA MVC   SVSYSFIL,SYSFIL     SAVE DIRECTORY                               
         MVC   SVSYSDIR,SYSDIR     FILE                                         
         L     R2,AIO              AND AIO                                      
                                                                                
         MVC   SYSFIL,=CL8'CHKFIL' SET SYSFIL AND SYSDIR FOR CHECKS             
         MVC   SYSDIR,=CL8'CHKDIR'                                              
                                                                                
         USING TLCKD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCKCD,TLCKCDQ      CHECK ACTIVE POINTER                         
         MVC   TLCKAGY,TGAGY       AGENCY                                       
         MVC   TLCKINV,RET4INV                                                  
         GOTO1 HIGH                                                             
         B     RETSPNH3                                                         
RETSPNH1 GOTO1 SEQ                                                              
RETSPNH3 CLC   TLCKKEY(TLCKSORT-TLCKKEY),KEYSAVE  HAS TO FIND ORIG INV          
         BNE   RETSPNHX                                                         
         CLC   TLCKSORT+4(2),TGCSORT+4                                          
         BNE   RETSPNH1                                                         
         CLC   TLCKSSN,TGSSN                      SAME SSN                      
         BNE   RETSPNH1                                                         
                                                                                
RETSPNH5 MVC   AIO,AIO3            READ CHECK INTO AIO3                         
         GOTO1 GETREC                                                           
                                                                                
         L     R4,AIO                                                           
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   RETSPNHX                                                         
         L     R1,TAPDSPNH                                                      
         TM    PAYSTAT1,CREDIT     IF CREDIT PAYMENT                            
         BZ    *+6                 NO                                           
         LNR   R1,R1               NEGATE R1                                    
         STCM  R1,15,CSTRSPNH                                                   
                                                                                
         LHI   R0,120              P&H RATE CHANGE (1800-1680)                  
         BAS   RE,MULTR0_2                                                      
         STCM  R1,15,CSTRPNHD                                                   
                                                                                
RETSPNHX MVC   SYSFIL,SVSYSFIL                                                  
         MVC   SYSDIR,SVSYSDIR                                                  
         ST    R2,AIO                                                           
         J     XIT                                                              
                                                                                
MULTR0_2 MR    R0,R0               USE R0                                       
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         BR    RE                  RETURN ANSWER IN R1                          
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE BUILDS AND ADDS UPGRADE DETAILS EL FOR VERSIONS          
         SPACE                                                                  
         USING TAUHD,R3                                                         
         USING TAUPD,R4                                                         
BLDUPG   NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,0           ONLY IF PAYING A VERSION                     
         JE    XIT                                                              
         TM    TGUSTYST,UPGRADE    AND IF UPGRADE USE TYPE                      
         JZ    XIT                                                              
         LA    R3,TCTAUHEL         R3=A(USAGE HISTORY ELEMENT)                  
         LA    R4,ELEMENT          R4=A(UPGRADE DETAILS ELEMENT)                
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAUPEL,TAUPELQ      ELEMENT CODE                                 
         MVI   TAUPLEN,TAUPLNQ             LENGTH                               
         SPACE 1                                                                
         TM    TGUSSTA3,CBLUSE     IF CABLE                                     
         JZ    BLDUP5                                                           
         MVC   TAUPICBU,TAUHCBUN   SAVE INITIAL UNITS                           
         J     BLDUPX                                                           
         SPACE 1                                                                
BLDUP5   MVC   TAUPIMAJ,TAUHMAJ    USE ORIGINAL VALUES BECAUSE                  
         MVC   TAUPIUNT,TAUHUNT    UH EL NOT UPDATED YET BY CASTUH              
         SPACE                                                                  
BLDUPX   GOTO1 ADDL                ADD ELEMENT TO CHECK RECORD                  
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE BUILDS TAMA FOR AOS ELEMENT IN CHECK                     
         SPACE                                                                  
         USING TAMAD,R4                                                         
BLDTAMA  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ELEMENT          R4=A(UPGRADE DETAILS ELEMENT)                
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAMAEL,TAMAELQ      ELEMENT CODE                                 
         MVI   TAMALEN,TAMALN2Q            LENGTH                               
         MVI   TAMATYPE,TAMATYPA           AOS AMOUNTS                          
*                                                                               
         STCM  R3,15,TAMAASAM      STORE AOS AMOUNT                             
         MVC   TAMAASBS,TACTAOS    STORE BASE AMOUNT FOR AOS                    
         CLC   TGSSN,=C'000112843' SET GLOBAL SSN                               
         BNE   BLDTM3                                                           
         LA    R1,800              8% PST                                       
         B     BLDTM8                                                           
BLDTM3   CLC   TGSSN,=C'000117358' SET GLOBAL SSN                               
         BNE   BLDTMX                                                           
         LA    R1,900              9% QST                                       
BLDTM8   STCM  R1,15,TAMAASPR                                                   
         XR    R2,R2                                                            
         MR    R2,R1               MULTIPLY BY PST/QST RATE                     
         ZAP   DUB(8),=P'0'                                                     
         CVD   R3,DUB                                                           
         SRP   DUB,60,5                                                         
         CVB   R0,DUB                                                           
         STCM  R0,15,TAMAASPA      SAVE PST/QST AMOUNT                          
*                                                                               
BLDTMX   GOTO1 ADDL                ADD ELEMENT TO CHECK RECORD                  
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
         SPACE 2                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
         TM    PAYMODE,DTLONSCR+DTLONBSE  IF NOT PAYING WITH DETAIL             
         JNZ   XIT                                                              
         GOTO1 VALPFK,DMCB,0       TEST MENU PFKEY PRESSED                      
         SPACE 1                                                                
         XC    SVCASTKY,SVCASTKY   INITIALIZE CAST KEY                          
         SPACE 1                                                                
         TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         JZ    XIT                 IN DRAFT MODE                                
         CLC   PAYINV,=CL6'DRAFT'                                               
         JNE   XIT                                                              
         OI    PAYMODE,DRAFT       SET AS DRAFT PAYMENT NOW                     
         J     XIT                                                              
         EJECT                                                                  
*              ROUTINE CHECKS FOR PAY ADVICE LINK                               
         SPACE                                                                  
TESTADV  NTR1  BASE=*,LABEL=*                                                   
         LH    R2,DSPAUTH          R2=DISP. TO AUTH/PO FIELD                    
         LTR   R2,R2                                                            
         JZ    XIT                                                              
         AR    R2,RA               R2=A(FIELD)                                  
         CLC   =C'A=',8(R2)        IF AUTH STARTS WITH 'A='                     
         JNE   XIT                                                              
         XC    SVADVDA,SVADVDA     PRE-CLEAR ADVICE DISK ADDRESS                
         MVI   OVERLAY,X'54'       LOAD ADVICE EXTRACT MODULE                   
         GOTO1 LOADSOPH,DMCB,0                                                  
         GOTO1 (R3),DMCB,(RC)      AND PASS CONTROL TO IT                       
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PROTECT PART OF DETAIL LINE FOR RETROS                
*              R1=A(DISPL. TO 1ST FIELD TO PROTECT)                             
         SPACE                                                                  
PROTRET  NTR1  BASE=*,LABEL=*                                                   
         TM    PAYOPTS3,ORETRO                                                  
         JZ    XIT                                                              
         CLI   TGUSEQU,UPNH                                                     
         JE    XIT                                                              
                                                                                
         CLC   DSPAPPLI,=X'FFFF'                                                
         BE    PRET10                                                           
         LH    R2,DSPAPPLI                                                      
         A     R2,ATHSLINE                                                      
         OI    1(R2),X'20'                                                      
                                                                                
         LH    R2,DSPAPPL                                                       
         A     R2,ATHSLINE                                                      
         OI    1(R2),X'20'                                                      
                                                                                
PRET10   LH    R2,DSPSPNH                                                       
         A     R2,ATHSLINE                                                      
         OI    1(R2),X'20'                                                      
         OI    1(R2),X'0C'                                                      
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PROTECT PART OF DETAIL LINE                           
*              R1=A(DISPL. TO 1ST FIELD TO PROTECT)                             
         SPACE                                                                  
PROTDTL  NTR1  BASE=*,LABEL=*                                                   
         LTR   R2,R1               COPY R1 TO R2 TO USE IN GOTO1                
         JZ    XIT                 XIT IF DON'T HAVE FIELD                      
         A     R2,ATHSLINE         ADD A(THIS LINE)                             
         LH    R3,DSPAGT                                                        
         A     R3,ATHSLINE                                                      
         GOTO1 FLDVAL,DMCB,(X'08',(R2)),(R3)  PROTECT PART OF DTL LINE          
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PROTECT MISC. DED FIELD IF UNION IS ACT               
         SPACE                                                                  
PROTMDED NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUNEQU,ACT         IF UNION IS ACT                              
         JE    PRMDED                                                           
         TM    PAYSTAT1,CREDIT     OR IF CREDIT INVOICE                         
         JZ    XIT                                                              
         TM    PAYSTAT1,BNP        AND NOT BNP                                  
         JO    XIT                                                              
PRMDED   LH    R2,DSPMDED          GET DISPLACEMENT TO FIELD                    
         LTR   R2,R2                                                            
         JZ    XIT                 DON'T BOTHER IF DON'T HAVE FIELD             
         A     R2,ATHSLINE                                                      
         OI    1(R2),X'20'         PROTECT IT                                   
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         SPACE 3                                                                
*              ROUTINE TO UNPROTECT MISC. DED FIELD IN CASE IT WAS              
*              PROTECTED LAST PAGE                                              
         SPACE                                                                  
UNPROTMD NTR1  BASE=*,LABEL=*                                                   
         LH    R2,DSPMDED          GET DISPLACEMENT TO FIELD                    
         LTR   R2,R2                                                            
         JZ    XIT                 DON'T BOTHER IF DON'T HAVE FIELD             
         A     R2,ATHSLINE                                                      
         NI    1(R2),X'DF'         UNPROTECT IT                                 
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE HANDLES THE H&W BUFFER TABLE.  TURNS ON X'80' IN         
*              PAGENCW IF CAST MEMBER SHOULD GET FIXED SESSION RATE 2X.         
*              TURNS ON X'40' IF HE SHOULD GET IT ONLY 1X.                      
*              R4=A(PAGETBL ENTRY)                                              
         SPACE                                                                  
         USING PAGED,R4                                                         
SETHWBUF NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UBSM        ONLY VALID FOR BSM OR                        
         BE    SETHB2                                                           
         CLI   TGUSEQU,UIMS        IMS PAYMENTS                                 
         JNE   XIT                                                              
*                                                                               
SETHB2   LA    R2,HNWBUFF          R2=A(HNWBUFF ENTRY)                          
         LA    R0,MAXHNW           R0=MAX NO. OF ENTRIES                        
*                                                                               
SETHB5   OC    0(4,R2),0(R2)       TEST NOT END OF TABLE                        
         BZ    SETHB15                                                          
         MVC   DUB(4),0(R2)                                                     
         NI    DUB,X'7F'           TURN OFF X'80' BIT                           
         L     R1,DUB                                                           
         CVD   R1,DUB                                                           
         UNPK  WORK(9),DUB         CONVERT SSN TO EBCDIC                        
         OI    WORK+8,X'F0'                                                     
         CLC   TGSSN,WORK          IF NOT SAME AS TGSSN                         
         BE    SETHB10                                                          
         LA    R2,4(R2)            BUMP TO NEXT ENTRY                           
         BCT   R0,SETHB5                                                        
         DC    H'0'                NOT ENOUGH ROOM IN TABLE                     
         SPACE                                                                  
*                                  PERF IS IN H&W TABLE                         
SETHB10  TM    0(R2),X'80'         TEST ALREADY GETTING FIXED RATE 2X           
         JO    XIT                 IF YES, WE'RE DONE HERE                      
         OI    PAGENCW,X'40'       ELSE SET CAST MEMBER GETS FIXED 1X           
         OI    0(R2),X'80'         SET PERF ALREADY GETTING FIXED 2X            
         J     XIT                                                              
         SPACE                                                                  
SETHB15  PACK  DUB,TGSSN           ADD NEW ENTRY IN H&W TABLE                   
         CVB   R1,DUB              WITH BINARY SSN                              
         ST    R1,0(R2)                                                         
         XC    4(4,R2),4(R2)       CLEAR NEXT ENTRY TO MARK END                 
         SPACE                                                                  
         TM    TGCASTAT,HNW2X      IF THIS CATEGORY GETS FIXED RATE 2X          
         BZ    SETHB17                                                          
         OI    PAGENCW,X'80'       SET CAST MEMBER GETS FIXED 2X                
         OI    0(R2),X'80'         AND PERF ALREADY GETTING FIXED 2X            
         J     XIT                                                              
SETHB17  OI    PAGENCW,X'40'       ELSE SET CAST MEMBER GETS FIXED 1X           
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE ACCUMULATES ALL THE EPIPENS AND EPIHLTH IN TSAR          
*              REC AT TCATSAR AND SETS TCPENS AND TCHLTH FOR UNION WGA          
         SPACE                                                                  
         USING ECASTABD,R3                                                      
SETPNH   NTR1  BASE=*,LABEL=*                                                   
         TM    LCLSTAT3,SOAPRES    IF SOAP RESIDUAL                             
         JZ    XIT                                                              
         CLI   TGUNEQU,WGA         ONLY IF UNION IS WGA                         
         JNE   XIT                                                              
         L     R3,TCATSAR                                                       
         ZIC   R0,ECSTNEPI         R0=N'EPISODES                                
         LA    R3,ECSTEPIS         R3=A(EPISODE ENTRY)                          
         SPACE                                                                  
         USING EPISD,R3                                                         
         XR    R1,R1               R1=TOTAL PENSION AMOUNT                      
         XR    R2,R2               R2=TOTAL HEALTH AMOUNT                       
         SPACE                                                                  
SETPNH5  AH    R1,EPIPENS          ADD PENSION AMOUNT FOR THIS EPISODE          
         AH    R2,EPIHLTH              HEALTH                                   
         LA    R3,EPINUMLN(R3)                                                  
         BCT   R0,SETPNH5          LOOP FOR N'EPISODES                          
         SPACE                                                                  
         ST    R1,TCPENS           SAVE TCPENS                                  
         ST    R2,TCHLTH           SAVE TCHLTH                                  
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CALLS TSAR USING TSARAREA FOR TSAR RECORD                
*              MYTSRNUM AND MYTSACTN ARE SET                                    
*              RETURNS CONDITION CODE                                           
CALLTSAR NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TSARAREA                                                      
         ST    R1,TCATSAR          SET A(TSAR RECORD) FOR RATECALC              
         LA    R1,ECSTRLNQ                                                      
         STCM  R1,3,MYTSRECL       REC LENGTH                                   
         MVI   MYTSKEYL,ECSTKLNQ   KEY LENGTH                                   
         MVI   MYTSPAGN,6          SET 6 14K PAGES - MAX 192 TSAR RECS          
         MVI   MYTSINDS,TSIALLOC+TSIRTNAF  SET FOR TEMPEST                      
         GOTO1 TSARCNTL,DMCB,TCATSAR                                            
         JE    YES                                                              
         J     NO                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DISPLAY OPTIONAL INFORMATION                          
         SPACE 1                                                                
DISOPT   NTR1  BASE=*,LABEL=*                                                   
         OC    DSPOINF,DSPOINF     TEST HAVE OPTIONAL INFO AREA                 
         JZ    XIT                                                              
         XC    TGNAME,TGNAME                                                    
         LA    R4,TGNAME           FIRST PUT INFO IN TGNAME                     
* NO OPT CLI   TGUSEQU,USOP                                                     
* INFO   BE    DOPT1                                                            
         CLI   TGYRCDE+2,C' '      IF YEAR IS 3 BYTES                           
         BNH   *+8                                                              
         LA    R4,1(R4)            START OPTIONS 1 FARTHER                      
         SPACE 1                                                                
DOPT1    LA    R3,ELTACA           R3=A(CAST DETAILS ELEMENT)                   
         USING TACAD,R3                                                         
         CLI   TACACORP,C' '       IF CORP# INDICATED ON CAST                   
         BNH   DOPT2                                                            
         OC    ELTATI,ELTATI       AND WE DIDN'T FIND CORP TAX ID#              
         BNZ   DOPT2                                                            
         MVC   0(14,R4),=C'>>>BAD CORP<<<'  DISPLAY WARNING                     
         B     DOPTX                                                            
         DROP  R3                                                               
         SPACE 1                                                                
DOPT2    CLI   TCW4TYPE,TAW4TYIN   IF THIS ISN'T AN INDIVIDUAL                  
         BE    DOPT4                                                            
         MVC   0(1,R4),TCW4TYPE    DISPLAY TYPE                                 
         MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
         SPACE 1                                                                
DOPT4    CLC   TGUNI,=C'SAG'                                                    
         JE    *+14                                                             
         CLC   TGUNI,=C'AFT'                                                    
         JNE   DOPT5                                                            
         OC    GUARCONT,GUARCONT   IF ON A GUARANTEE CONTRACT                   
         BZ    DOPT5                                                            
         MVC   0(3,R4),=C'GC,'     FLAG IT                                      
         LA    R4,3(R4)                                                         
         B     DOPT5A                                                           
         SPACE 1                                                                
DOPT5    OC    TGGUA,TGGUA         IF ON A GUARANTEE                            
         BZ    *+14                                                             
         MVC   0(2,R4),=C'G,'      FLAG IT                                      
         LA    R4,2(R4)                                                         
         SPACE 1                                                                
DOPT5A   CLI   TCCADBL,C' '        IF CAST MEMBER DOUBLED                       
         BNH   DOPT6                                                            
         MVC   0(1,R4),TCCADBL     DISPLAY N'DOUBLES                            
         MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
         SPACE 1                                                                
DOPT6    OC    TCOV1(8),TCOV1      ANY OVERSCALE                                
         BZ    DOPT8                                                            
         MVC   0(2,R4),=C'O='                                                   
         LA    R4,2(R4)                                                         
         SPACE 1                                                                
         XR    RE,RE               HANDLE 1ST OVERSCALE                         
         L     RF,TCOV1                                                         
         TM    TCOV1,X'80'         PERCENT SCALE?                               
         BZ    DOPT6A                                                           
         MVI   0(R4),C'%'                                                       
         LA    R4,1(R4)                                                         
         N     RF,=X'7FFFFFFF'                                                  
DOPT6A   D     RE,=F'100'          PUSH OUT PENNIES                             
         BAS   RE,EDITOV                                                        
         SPACE 1                                                                
         OC    TCOV2,TCOV2         DO WE HAVE 2ND OVERSCALE                     
         BZ    DOPT8                                                            
         XR    RE,RE                                                            
         L     RF,TCOV2                                                         
         D     RE,=F'100'          PUSH OUT PENNIES                             
         BAS   RE,EDITOV                                                        
         SPACE 1                                                                
DOPT8    BCTR  R4,0                BUMP BACK ONE                                
         CLI   0(R4),C','                                                       
         BNE   *+8                                                              
         MVI   0(R4),C' '          ERASE TRAILING COMMA                         
         SPACE 1                                                                
DOPTX    LH    R4,DSPOINF          R4=DISP. TO OPTIONAL INFO AREA               
         A     R4,ATHSLINE                                                      
         LA    R1,15               SET (L'OPTIONAL INFO)-1                      
         SPACE 1                                                                
         TM    LCLSTAT3,SOAPRES    IF USE IS A SOAP RESIDUAL                    
         BZ    *+8                                                              
         LA    R1,7                SET DIFFERENT LENGTH FOR OPT. INFO           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),TGNAME      MOVE OPTIONAL INFO FOR L'FIELD               
         J     XIT                                                              
         SPACE 3                                                                
EDITOV   DS    0H                                                               
         EDIT  (RF),(4,(R4)),ALIGN=LEFT                                         
         AR    R4,R0               BUMP PAST AMOUNT                             
         MVI   0(R4),C','          ADD COMMA                                    
         LA    R4,1(R4)                                                         
         BR    RE                  RETURN R4=A(NEXT SLOT)                       
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
*              ROUTINE MAKES SURE THERE'S AN ESTIMATE NUM ON THE                
*              INVOICE REC IN AIO IF IT'S REQUIRED                              
         SPACE                                                                  
TRAPEST  NTR1  BASE=*,LABEL=*                                                   
         TM    PAYMODE,DRAFT       DON'T BOTHER IF DRAFT PAYMENT                
         JO    XIT                                                              
         TM    LCLSTAT3,SPLIT      IF NOT SPLIT BILL                            
         JO    XIT                                                              
         CLI   INTER,C'Y'          AND IF ON INTERFACE                          
         BE    *+12                                                             
         TM    AGYSTAT,TAAYSEST    OR IF ESTIMATE REQUIRED FOR AGENCY           
         JZ    XIT                                                              
         TM    PAYOPTS4,OPNOINT    OR INTERFACING                               
         JO    XIT                                                              
         MVI   ELCODE,TANUELQ      MAKE SURE ESTIMATE NUM IS ON INVOICE         
         GOTO1 GETL,DMCB,(1,=AL1(TANUTEST))                                     
         JE    XIT                                                              
         DC    H'0'                DIE IF NOT FOUND - BUG                       
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*              ROUTINE TO TEST IF PERFORMER ON 2404A COMMERCIALS                
*              BELONGS ON THE CANADIAN INVOICE                                  
         SPACE 1                                                                
TEST04A  NTR1  BASE=*,LABEL=*                                                   
         OC    INV04A,INV04A       IF GENERATING TWO INVOICES FOR               
         JZ    NO                  ACTRA TYPE 2404A COMMERCIAL                  
         SPACE 1                                                                
         GOTOR ONCAINV,DMCB,ELTACA                                              
         J     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SAVE OFF CANADIAN PORTION OF PAYMENT                  
*              FOR ACTRA TYPE 2404A COMMERCIALS                                 
         SPACE 1                                                                
SV04ACAN NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,TEST04A          IF 2404A PERFORMER BELONGS ON                
         JNE   XIT                 CANADIAN INVOICE                             
         SPACE 1                                                                
         LA    R1,TCTOTS           R1=A(CHECK TOTALS)                           
         LA    RF,CTOTS            RF=A(CANADIAN TOTALS)                        
         LA    R0,NTCTOTS          R0=N'ACCUMS                                  
         SPACE 1                                                                
S04AC10  L     RE,0(RF)            CURRENT INVOICE TOTAL                        
         A     RE,0(R1)            PLUS THIS CHECK TOTAL                        
         ST    RE,0(RF)            IS NEW INVOICE TOTAL                         
         SPACE 1                                                                
         LA    R1,4(R1)            BUMP TO NEXT CHECK TOTAL                     
         LA    RF,4(RF)            AND TO NEXT INVOICE TOTAL                    
         BCT   R0,S04AC10                                                       
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD CANADIAN INVOICE FOR PAYMENTS TO                  
*              ACTRA TYPE 2404A COMMERCIALS                                     
         SPACE 1                                                                
ADD04AIN NTR1  BASE=*,LABEL=*                                                   
         USING TLIND,R4                                                         
         OC    INV04A,INV04A       IF NEED TO ADD CANADIAN INVOICE              
         JZ    XIT                 FOR ACTRA TYPE 2404A                         
         L     R4,AIO                                                           
         MVC   TLININV,INV04A      PUT CAN INV INTO RECORD KEY                  
         DROP  R4                                                               
         SPACE 1                                                                
         BRAS  RE,STCANDOL         SET PAYMENT AS CANADIAN DOLLARS              
         SPACE 1                                                                
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   TAPDINV,INV04A      SET INVOICE NUMBER                           
         XC    TAPDINV,HEXFFS                                                   
         OC    ACTCYCE,ACTCYCE     IF CANADIAN CYCLE END DATE                   
         BZ    *+10                DIFFERS FROM US INVOICE                      
         MVC   TAPDCYCE,ACTCYCE    USE CANADIAN CYCLE END DATE                  
         SPACE 1                                                                
         MVC   TAPDGRS,CGROSS      SET CANADIAN GROSS                           
         L     R0,CAPPLCR          CANADIAN APPLIED CREDITS                     
         LCR   R0,R0               SAVE COMPLEMENTED AMOUNT                     
         ST    R0,TAPDAPPL                                                      
         L     R0,CGUAR            CANADIAN GUARANTEE CREDITS                   
         LCR   R0,R0               SAVE COMPLEMENTED AMOUNT                     
         ST    R0,TAPDGUAR                                                      
         MVC   TAPDPAYI,CPAYI      CANADIAN INDIVIDUAL PAYMENT AMOUNT           
         MVC   TAPDPAYC,CPAYC      CANADIAN CORPORATE PAYMENT AMOUNT            
         MVC   TAPDREXP,CEXPENSE   CANADIAN REIMBURSED EXPENSES                 
         MVC   TAPDSPNH,CSUBJPNH   CANADIAN SUBJECT TO P&H                      
         MVC   TAPDMDED,CMISCDED   CANADIAN MISCELLANEOUS DEDUCTION             
         MVC   TAPDDUES,CDUES      CANADIAN UNION DUES                          
         MVC   TAPDPNH,CPNH        CANADIAN PENSION & HEALTH                    
         MVC   TAPDINR,CINR        CANADIAN INSURANCE & RETIREMENT              
         MVC   TAPDHNW,CHNW        CANADIAN HEALTH & WELFARE                    
         MVC   TAPDTXNW,CTXNW                                                   
         L     RE,CEXPENSE                                                      
         ICM   RF,15,CTXNW                                                      
         SR    RE,RF                                                            
         STCM  RE,15,TAPDNTNW                                                   
         SPACE 1                                                                
         NI    TAPDSTA3,X'FF'-TAPDSC16                                          
         DROP  R4                                                               
         SPACE 1                                                                
         GOTO1 MYADDREC            ADD INVOICE RECORD                           
         XC    WORK,WORK                                                        
         GOTO1 ADDPTRS,DMCB,WORK                                                
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET PAYMENT AS BEING IN CANADIAN DOLLARS              
*              ON ENTRY ... R4=A(INVOICE RECORD)                                
         SPACE 1                                                                
STCANDOL NTR1  BASE=*,LABEL=*                                                   
         TM    PAYMODE,DRAFT       EXIT IF MAKING DRAFT PAYMENT                 
         JO    XIT                                                              
         SPACE 1                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ      SET COMMERCIAL AS CANADIAN DOLLARS           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         OI    TACOSTAT,TACOSCAN                                                
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         OI    TAPDSTAT,TAPDSCAN   AND SET PAYMENT AS CANADIAN DOLLARS          
         J     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CLEAR LOWER HALF OF SCREEN                            
         SPACE 1                                                                
CLRSCRN  NTR1  BASE=*,LABEL=*                                                   
         TM    LCLSTAT,DTLFIRST    IF FIRST TIME FOR DETAIL                     
         BZ    CLR5                                                             
         LH    R2,DSPEINV                                                       
         AR    R2,RA               R2=A(CR OF INV# FLD HEADER)                  
         SPACE                                                                  
         GOTO1 FLDVAL,DMCB,(X'08',PAYAGYH),(R2)  PROTECT FROM AGENCY TO         
*                                                CR OF INV# FIELD               
         LH    R2,DSPOPTS                                                       
         AR    R2,RA               R2=A(OPTIONS FIELD HEADER)                   
         OI    1(R2),X'20'         PROTECT IT                                   
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
         CLI   TGUSEQU,UPAX        IF PAX USE                                   
         BE    *+12                                                             
         CLI   TGUSEQU,UCLA        IF CLA USE                                   
         BNE   CLR2                                                             
         OI    CLAG13H+1,X'20'     PROTECT GUAR13? FIELD                        
         OI    CLAG13H+6,X'80'                                                  
         B     CLR5                                                             
         SPACE 1                                                                
CLR2     CLI   TGUSEQU,UADO        IF ADO                                       
         BE    *+12                                                             
         CLI   TGUSEQU,UARR        OR ARR                                       
         BNE   CLR3                                                             
         GOTO1 FLDVAL,DMCB,(X'08',ADOTTY1H),ADOTTY5H PROTECT THIS: INFO         
         B     CLR5                                                             
         SPACE 1                                                                
CLR3     CLI   TGUSEQU,UADT        IF ADT                                       
         BE    *+12                                                             
         CLI   TGUSEQU,UARS        OR ARS                                       
         BNE   CLR5                                                             
         GOTO1 FLDVAL,DMCB,(X'08',ADTTTY1H),ADTTTY5H PROTECT THIS: INFO         
         SPACE 1                   CLEAR/VALIDATE/SET NORM INT FOR ALL          
CLR5     GOTO1 FLDVAL,DMCB,(X'21',ADTLFSTH),(X'10',ADTLLSTH)                    
         SPACE 1                                                                
         XC    DSPLMDED,DSPLMDED   CLEAR DISP. TO LAST MISC. DED. INPUT         
         SPACE 1                                                                
         MVI   GPAGE,X'FF'         CLEAR PREV. PAGE INDICATOR                   
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CHECKS THE PAYMENT HISTORY POINTER TO SEE                
*              IF THERE'S ALREADY A PAYMENT FOR THIS COMMERCIAL                 
*              AND INVOICE IN CASE OF AGENCY CHANGE FROM COMML COPY             
*              SETS CC EQUAL IF NONE YET                                        
         SPACE                                                                  
         USING TLINPD,R3                                                        
CHKPHIST NTR1  BASE=*,LABEL=*                                                   
         GOTO1 RECVAL,DMCB,(X'80',TLINHCDQ),(X'C0',0)  BUILD TLINH PTR          
         LA    R3,KEY                                                           
         MVI   TLINHSEQ,X'FF'      SET SEQUENCE FOR REGULAR INVOICE             
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLDRKEY),KEYSAVE                                           
         JNE   YES                                                              
         J     NO                  OK IF DON'T ALREADY HAVE                     
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
SETGSSN  NTR1  BASE=*,LABEL=*      SET TGSSN FOR LOCAL                          
         CLC   =C'TOR',LCLLOCAL    IF LOCAL TORONTO                             
         JNE   *+14                                                             
         MVC   TGSSN,=C'000001553' SET GLOBAL SSN FOR CHECK                     
         J     XIT                                                              
         CLC   =C'ALB',LCLLOCAL    IF LOCAL ALBERTA                             
         JNE   *+14                                                             
         MVC   TGSSN,=C'000000202' SET GLOBAL SSN FOR CHECK                     
         J     XIT                                                              
         CLC   =C'MON',LCLLOCAL    IF LOCAL MONTREAL                            
         JNE   *+14                                                             
         MVC   TGSSN,=C'000004137' SET GLOBAL SSN FOR CHECK                     
         J     XIT                                                              
         CLC   =C'VBC',LCLLOCAL    IF LOCAL VANCOUVER                           
         JNE   *+14                                                             
         MVC   TGSSN,=C'000002533' SET GLOBAL SSN FOR CHECK                     
         J     XIT                                                              
         CLC   =C'NFL',LCLLOCAL    IF LOCAL NEWFOUNDLAND                        
         JNE   *+14                                                             
         MVC   TGSSN,=C'000007108' SET GLOBAL SSN FOR CHECK                     
         J     XIT                                                              
         CLC   =C'CAL',LCLLOCAL    IF LOCAL CALAGORY                            
         JNE   *+14                                                             
         MVC   TGSSN,=C'000000202' SET GLOBAL SSN FOR CHECK                     
         J     XIT                                                              
         CLC   =C'MAR',LCLLOCAL    IF LOCAL MARITIME                            
         JNE   *+14                                                             
         MVC   TGSSN,=C'000115472' SET GLOBAL SSN FOR CHECK                     
         J     XIT                                                              
         CLC   =C'WIN',LCLLOCAL    IF LOCAL WINNIPEG                            
         JNE   *+14                                                             
         MVC   TGSSN,=C'000006777' SET GLOBAL SSN FOR CHECK                     
         J     XIT                                                              
         CLC   =C'EDM',LCLLOCAL    IF LOCAL EDMONTON                            
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   TGSSN,=C'000008213' SET GLOBAL SSN FOR CHECK                     
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE GETS TIMESHEET IN AIO3 FOR SYSCALC IF IT EXISTS          
         SPACE 1                                                                
GETTSHT  NTR1  BASE=*,LABEL=*                                                   
         TM    TGFASTAT,TGFROMFA                                                
         JO    XIT                                                              
         CLI   HASTIME,C'Y'                                                     
         JNE   XIT                                                              
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLTMCDQ,(X'A4',0)  DOES PERF HAVE TIMESHT?           
         JE    GTSHT00                                                          
         OI    TGFASTAT,TGRDWBTS                                                
         GOTO1 RECVAL,DMCB,TLTMCDQ,(X'A4',0)                                    
         JNE   XIT                                                              
GTSHT00  MVC   AIO,AIO1                                                         
         L     R4,AIO3                                                          
         USING TATTD,R4                                                         
         MVI   ELCODE,TATTELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
GTSHT10  BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         CLC   TATTDATE,=X'FFFFF0'  FIND 1ST SUBTOTAL ELEMENT                   
         JL    GTSHT10              X'F9',x'FA',x'FB',X'FC',X'FD',X'FE'         
         CLC   TATTDATE,=X'FFFFFF'  TOTAL ELEMENT                               
         JL    *+6                                                              
         DC    H'00'                SHOULD HAVE AT LEAST ONE SUBTOTAL           
         ST    R4,TCATMTOT          STORE ADDRESS OF 1ST TOTAL ELEMENT          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE ADDS TAGQ AND TAFQ ELS TO THE BASE SCREEN REC            
         SPACE                                                                  
         USING TLSCD,R3                                                         
ADDGQFQ  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,KEY              BUILD KEY                                    
         XC    TLSCKEY,TLSCKEY                                                  
         MVI   TLSCCD,TLSCCDQ      SCREEN RECORD CODE                           
         MVC   TLSCAGY,TGAGY       AGENCY                                       
         MVC   TLSCINV,SCRINVNO    INVOICE NUMBER                               
         MVC   TLSCPG,FPAGE        BASE SCREEN PAGE NUMBER                      
         SPACE 1                                                                
         GOTO1 HIGH                READ DIRECTORY                               
         CLC   TLSCKEY(TLSCSCR-TLSCKEY),KEYSAVE  INSURE RECORD IS VALID         
         JE    *+6                                                              
         DC    H'0'                MISSING SCREEN RECORD                        
         SPACE                                                                  
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC              GET THE RECORD FOR UPDATE                    
         GOTO1 ADDREQEL            ADD TAGQ AND TAFQ ELEMENTS                   
         GOTO1 MYPUTREC            WRITE BACK CHANGED RECORD                    
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO HANDLE "LOCKED W4 RECORD" ERROR                   *         
***********************************************************************         
                                                                                
W4LCK    NTR1  BASE=*,LABEL=*                                                   
         TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB ...               
         JZ    W4LCK40                                                          
                                                                                
         TM    TGFASTAT,TGCRNOPY   ... AND APPLICATION IS CERNO                 
         JZ    W4LCK30             ONLY ADD ERROR MESSAGE TO BLOCK              
         L     RE,TGAERTAB         ONCE                                         
W4LCK10  CLI   0(RE),X'FF'                                                      
         JE    W4LCK20                                                          
         CLI   2(RE),ERW4LOCK                                                   
         JE    XIT                                                              
         ZIC   R0,0(RE)                                                         
         AR    RE,R0                                                            
         J     W4LCK10                                                          
W4LCK20  GOTO1 ADDERROR,DMCB,ERW4LCK                                            
         J     YES                                                              
                                                                                
W4LCK30  GOTOR ADDEAP,DMCB,0,ERW4LCK2,AWEBRES                                   
         NI    LCLSTAT3,X'FF'-W4LOCKED                                          
         J     YES                                                              
                                                                                
W4LCK40  MVI   MYMSGNO1,ERW4LOCK   ELSE, SEND ERROR MESSAGE                     
         MVI   BLOCK,10            TO SCREEN                                    
         MVC   BLOCK+1(9),TGSSN                                                 
         MVI   BLOCK+10,0                                                       
         J     NO                                                               
                                                                                
***********************************************************************         
*        W4 LOCKED ERRORS                                             *         
***********************************************************************         
                                                                                
ERW4LCK  DC    AL1(EW4LCKX-*),AL2(ERW4LOCK),AL1(ERRCATY1),AL1(D#PYCST)          
         DC    AL2(0)                                                           
         DC    C'Cast includes a locked W4 record'                              
EW4LCKX  EQU   *                                                                
                                                                                
ERW4LCK2 DC    AL1(EW4LCK2X-*),AL2(ERW4LOCK),AL1(ERRCATY1),AL1(D#PYCST)         
         DC    AL2(0)                                                           
         DC    C'W4 record is locked'                                           
EW4LCK2X EQU  *                                                                 
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO HANDLE "LOCKED CORP RECORD" ERROR                 *         
***********************************************************************         
                                                                                
CRPLCK   NTR1  BASE=*,LABEL=*                                                   
         TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB ...               
         JZ    CRPLCK40                                                         
                                                                                
         USING ERRENTD,RE                                                       
         TM    TGFASTAT,TGCRNOPY   ... AND APPLICATION IS CERNO                 
         JZ    CRPLCK30            ONLY ADD ERROR MESSAGE TO BLOCK              
         L     RE,TGAERTAB         ONCE                                         
CRPLCK10 CLI   0(RE),X'FF'                                                      
         JE    CRPLCK20                                                         
         CLC   EENUMB,=Y(ERCRPLCK)                                              
         JE    XIT                                                              
         ZIC   R0,0(RE)                                                         
         AR    RE,R0                                                            
         J     CRPLCK10                                                         
CRPLCK20 GOTO1 ADDERROR,DMCB,ERCRPLK                                            
         J     YES                                                              
         DROP  RE                                                               
                                                                                
CRPLCK30 GOTOR ADDEAP,DMCB,0,ERCRPLK2,AWEBRES                                   
         NI    LCLSTAT9,X'FF'-CRPLCKED                                          
         J     YES                                                              
                                                                                
CRPLCK40 MVC   MYMSGNO,=Y(ERCRPLCK)    ELSE, SEND ERROR MESSAGE                 
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVI   BLOCK,7                                                          
         MVC   BLOCK+1(6),TGPID        TO SCREEN                                
         MVI   BLOCK+7,0                                                        
         J     NO                                                               
                                                                                
***********************************************************************         
*        CORP LOCKED ERRORS                                           *         
***********************************************************************         
                                                                                
ERCRPLK  DC    AL1(ECRPLKX-*),AL2(ERCRPLCK),AL1(ERRCATY1),AL1(D#PYCST)          
         DC    AL2(0)                                                           
         DC    C'Cast includes a locked Corp record'                            
ECRPLKX  EQU   *                                                                
                                                                                
ERCRPLK2 DC    AL1(ECRPLK2X-*),AL2(ERCRPLCK),AL1(ERRCATY1),AL1(D#PYCST)         
         DC    AL2(0)                                                           
         DC    C'Corp record is locked'                                         
ECRPLK2X EQU  *                                                                 
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CHECK FOR DEALER ERRORS                           *         
***********************************************************************         
                                                                                
CHKDERRS NTR1  BASE=*,LABEL=*                                                   
         TM    LCLSTAT7,HLDPENDG   HLD PAYMENT IS PENDING                       
         JZ    XIT                                                              
*                                                                               
***********************************************************************         
*        HLD PENDING ERROR                                            *         
***********************************************************************         
                                                                                
HLDPND   MVC   MYMSGNO,=Y(ERRHLDPN)                                             
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         MVI   BLOCK,7                                                          
         MVC   BLOCK+1(6),TGPID                                                 
         MVI   BLOCK+7,0                                                        
         J     NO                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CHECK FOR GUARANTEE ERRORS                        *         
***********************************************************************         
                                                                                
CHKGERRS NTR1  BASE=*,LABEL=*                                                   
         TM    LCLSTAT6,GRRPENDG   GRR PAYMENT IS PENDING                       
         JO    GRRPND                                                           
                                                                                
         TM    LCLSTAT7,NGRTABAL   GUARANTEE HAS ASCENDING BALANCE              
         JO    NGABAL                                                           
         TM    LCLSTAT7,GACNMTCH   GRT/COMM'L AGY/CLI MISMATCH                  
         JO    GACMIS                                                           
                                                                                
         TM    LCLSTAT6,PCYPENDG   PER CYCLE PAYMENT IS PENDING                 
         JO    PCYPND                                                           
                                                                                
         TM    TGGRTSTA,TGGRTEAC   AGENCY/CLIENT NOT ON GUARANTEE               
         JO    GRTAGCL                                                          
         TM    TGGRTSTA,TGGRTEPD   PAYMENT DOES NOT FIT WITHIN PERIOD           
         JO    GRTPERD                                                          
         TM    TGGRTSTA,TGGRTECP   CORPORATION CODE DOES NOT MATCH              
         JO    GRTCORP                                                          
                                                                                
         TM    LCLSTAT6,GRTINERI   GRT PAYMENT IN ERROR STATUS (A/I)            
         JO    GRTIERAI                                                         
         TM    LCLSTAT6,PCYINERI   PCY PAYMENT IN ERROR STATUS (A/I)            
         JO    PCYIERAI                                                         
                                                                                
         TM    LCLSTAT6,GRTNTAPI   GRT PAYMENT IS NOT APPROVED (A/I)            
         JO    GRTNAPAI                                                         
         TM    LCLSTAT6,PCYNTAPI   PCY PAYMENT IS NOT APPROVED (A/I)            
         JO    PCYNAPAI                                                         
                                                                                
         TM    LCLSTAT8,CNNOTCAN   CN USED FOR SESSION PAYMENT                  
         JO    CNNOTVAL                                                         
                                                                                
         OC    GRTDUEDT,GRTDUEDT   IF UNPROCESSED GUARANTEE PAYMENT             
         JZ    YES                 WAS FOUND                                    
         LA    R1,GRTDUEDT                                                      
         CLC   GRTDUEDT,TGTODAY1                                                
         JNL   CGE10                                                            
         LA    R1,TGTODAY1                                                      
CGE10    CLC   0(3,R1),DUEDATE     AND DUE DATE IS NOT EARLIER THAN             
         JL    YES                 CURRENT PAYMENT'S DUE DATE                   
         JH    CGE20                                                            
         TM    PAYOPTS2,OPURGENT   OR IF DUE DATE IS THE SAME                   
         JO    CGE20               AND CURRENT PAYMENT IS URGENT                
         CLC   DUEDATE,TGTODAY1    OR IF DUE DATE IS TODAY                      
         JH    YES                                                              
         TM    TGUSSTA2,HLDTYPE    AND PAYMENT IS HOLDING FEE                   
         JO    CGE20                                                            
         TM    LCLSTAT6,AFTRPAID   OR CONTAINTS AFTRA CAST                      
         JZ    YES                                                              
CGE20    BAS   RE,GRTMPRO1         RETURN ERROR BECAUSE THIS PAYMENT            
         J     XIT                 WILL PROCESS BEFORE GUARANTEE                
                                                                                
***********************************************************************         
*        GRR PENDING ERROR                                            *         
***********************************************************************         
                                                                                
GRRPND   MVC   MYMSGNO,=Y(ERRGRRPN)                                             
         J     CGEPIDX                                                          
                                                                                
***********************************************************************         
*        GRT/PAY ERRORS                                               *         
***********************************************************************         
                                                                                
NGABAL   MVC   MYMSGNO,=Y(ERRNGABL)  GRT INELIGIBLE FOR INSTALLMENT             
         J     CGEPIDX                                                          
                                                                                
GACMIS   MVC   MYMSGNO,=Y(ERRGACMS)  GRT/COMM'L AGY/CLI MISMATCH                
         J     CGEPIDX                                                          
                                                                                
***********************************************************************         
*        PER CYCLE PENDING ERROR                                      *         
***********************************************************************         
                                                                                
PCYPND   MVC   MYMSGNO,=Y(ERRPCYPN)                                             
         J     CGEPIDX                                                          
                                                                                
***********************************************************************         
*        GUARANTEE/PER CYCLE SETUP PROBLEMS                           *         
***********************************************************************         
                                                                                
GRTAGCL  MVC   MYMSGNO,=Y(ERRGAGCL)  AGENCY/CLIENT NOT ON GUARANTEE             
         J     CGEPIDX                                                          
                                                                                
GRTPERD  MVC   MYMSGNO,=Y(ERRGPERD)  PAYMENT DOES NOT FIT WITHIN PERIOD         
         J     CGEPIDX                                                          
                                                                                
GRTCORP  MVC   MYMSGNO,=Y(ERRGCORP)  CORPORATION CODE DOES NOT MATCH            
         J     CGEPIDX                                                          
                                                                                
***********************************************************************         
*        GUARANTEE/PER CYCLE IN ERROR STATUS                          *         
***********************************************************************         
                                                                                
GRTIERAI MVC   MYMSGNO,=Y(ERRGIEAI)  GUARANTEE IN ERROR STATUS (A/I)            
         J     CGEAIX                                                           
                                                                                
PCYIERAI MVC   MYMSGNO,=Y(ERRPIEAI)  PER CYCLE IN ERROR STATUS (A/I)            
         J     CGEAIX                                                           
                                                                                
***********************************************************************         
*        GUARANTEE/PER CYCLE MUST BE APPROVED                         *         
***********************************************************************         
                                                                                
GRTNAPAI MVC   MYMSGNO,=Y(ERRGNAAI)  GUARANTEE MUST BE APPROVED (A/I)           
         J     CGEAIX                                                           
                                                                                
PCYNAPAI MVC   MYMSGNO,=Y(ERRPNAAI)  PER CYCLE MUST BE APPROVED (A/I)           
         J     CGEAIX                                                           
                                                                                
CNNOTVAL MVC   MYMSGNO,=Y(ERRCNNOT)  CN NO LONGER VALID FOR CANADA              
         J     CGEAIX                                                           
                                                                                
***********************************************************************         
*        GUARANTEE MUST PROCESS BEFORE THIS PAYMENT ERROR             *         
***********************************************************************         
                                                                                
GRTMPRO1 NTR1                                                                   
         MVC   TGPID,GRTDUPID      SAVE PID                                     
                                                                                
         TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         JZ    GRTMPRO7                                                         
                                                                                
         OC    GRTDUAGY,GRTDUAGY   CHECK IF USER HAS ACCESS TO INVOICE          
         JZ    GRTMPRO5            CAUSING ERROR                                
                                                                                
         LA    R2,GRTP1AI          IF YES, RETURN GUARANTEE/PER CYCLE           
         TM    LCLSTAT7,GTLRGOVS   AGY/INV THAT MUST BE MADE URGENT             
         JO    GRTMPRO2                                                         
         LA    R2,PCYC1AI                                                       
GRTMPRO2 MVC   0(L'GRTDUAGY,R2),GRTDUAGY                                        
         XR    R3,R3                                                            
GRTMPRO3 CLI   0(R2),C' '                                                       
         JE    GRTMPRO4                                                         
         LA    R2,1(R2)                                                         
         AHI   R3,1                                                             
         J     GRTMPRO3                                                         
GRTMPRO4 MVI   0(R2),C'/'                                                       
         GOTO1 TINVCON,DMCB,GRTDUINV,1(R2),DATCON                               
                                                                                
GRTMPRO5 TM    LCLSTAT7,GTLRGOVS                                                
         JZ    GRTMPRO6                                                         
         MVC   GRTP1PID,TGPID                                                   
         GOTOR ADDEAP,DMCB,ERGRTPR1,ERGRTPRF,AWEBRES                            
         J     YES                                                              
GRTMPRO6 MVC   PCYP1PID,TGPID                                                   
         GOTOR ADDEAP,DMCB,ERPCYPR1,ERPCYPRF,AWEBRES                            
         J     YES                                                              
                                                                                
         USING SVAID,R4                                                         
GRTMPRO7 LA    R4,WORK2                                                         
         MVC   SVAGY,GRTDUAGY                                                   
         MVC   SVINV,GRTDUINV                                                   
         MVC   MYMSGNO,=Y(ERRGP1AI)                                             
         TM    LCLSTAT7,GTLRGOVS                                                
         JO    CGEAIX                                                           
         MVC   MYMSGNO,=Y(ERRPP1AI)                                             
         J     CGEAIX                                                           
         DROP  R4                                                               
                                                                                
ERGRTPR1 DC    AL1(EGRTPR1X-*),AL2(ERRGP1AI),AL1(ERRCATY1),AL1(D#PYCST)         
         DC    AL2(0)                                                           
         DC    CL20'Guarantee pymt. for '                                       
GRTP1PID DC    CL6' '                                                           
         DC    CL21' must be made urgent '                                      
GRTP1AI  DC    CL13' '                                                          
EGRTPR1X EQU   *                                                                
                                                                                
ERGRTPRF DC    AL1(EGRTPRFX-*),AL2(ERRGP1AI),AL1(ERRCATY1),AL1(D#PYCST)         
         DC    AL2(0)                                                           
         DC    C'Guarantee payment must be made urgent'                         
EGRTPRFX EQU   *                                                                
                                                                                
ERPCYPR1 DC    AL1(EPCYPR1X-*),AL2(ERRPP1AI),AL1(ERRCATY1),AL1(D#PYCST)         
         DC    AL2(0)                                                           
         DC    CL20'Per cycle pymt. for '                                       
PCYP1PID DC    CL6' '                                                           
         DC    CL21' must be made urgent '                                      
PCYC1AI  DC    CL13' '                                                          
EPCYPR1X EQU   *                                                                
                                                                                
ERPCYPRF DC    AL1(EPCYPRFX-*),AL2(ERRPP1AI),AL1(ERRCATY1),AL1(D#PYCST)         
         DC    AL2(0)                                                           
         DC    CL37'Per cycle payment must be made urgent'                      
EPCYPRFX EQU   *                                                                
                                                                                
***********************************************************************         
*        ERROR EXIT INCLUDING PID                                     *         
***********************************************************************         
                                                                                
CGEPIDX  GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         MVI   BLOCK,7                                                          
         MVC   BLOCK+1(6),TGPID                                                 
         MVI   BLOCK+7,0                                                        
         J     NO                                                               
                                                                                
***********************************************************************         
*        ERROR EXIT INCLUDING AGENCY/INVOICE                          *         
***********************************************************************         
                                                                                
CGEAIX   MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
                                                                                
         USING SVAID,R4                                                         
         LA    R4,WORK2                                                         
         MVC   BLOCK(25),SPACES                                                 
         MVC   BLOCK+1(6),SVAGY                                                 
                                                                                
         LA    R2,BLOCK+2                                                       
         LHI   R3,2                                                             
GEEAIXA  CLI   0(R2),C' '                                                       
         JE    CGEAIXB                                                          
         LA    R2,1(R2)                                                         
         AHI   R3,1                                                             
         J     GEEAIXA                                                          
                                                                                
CGEAIXB  MVI   0(R2),C'/'                                                       
         GOTO1 TINVCON,DMCB,SVINV,1(R2),DATCON                                  
         MVI   7(R2),0                                                          
         AHI   R3,7                                                             
         STC   R3,BLOCK                                                         
         J     NO                                                               
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO SET APPROPRIATE CNET/MKT/CSYS HEADERS                 
*              BASED ON USE TYPE (FOR TAPYS5F)                                  
         SPACE 1                                                                
SETHEADS NTR1  BASE=*,LABEL=*                                                   
         TM    PAYMODE,DTLDSPLD    IF DETAIL NOT ALREADY DISPAYED               
         JO    XIT                                                              
         CLI   TWASCR,SCR11        AND ON INTERNET/NEW MEDIA SCREEN             
         BE    SH20                                                             
         CLI   TWASCR,SCR12        OR MOVED TO SCREENS                          
         BE    SH30                                                             
         CLI   TWASCR,SCR1D        OR NEW INTERNET/NEW MEDIA SCREEN             
         BE    SH40                                                             
         CLI   TWASCR,SCR5F        OR CBL/SCB/SWS/WSC/LCB SCREEN                
         BNE   XIT                                                              
         SPACE 1                                                                
         OI    CBLUNSH+6,X'80'                                                  
         SPACE 1                                                                
         LA    R2,CBLHD1H                                                       
         LHI   RF,4                                                             
         LA    R1,CNETHD                                                        
         MVC   CBLUNS,UNITHD                                                    
         TM    TGUSSTA3,CBLUSE     FILL IN THE CNET HEADERS                     
         BNZ   SH10                                                             
         SPACE 1                                                                
         LA    R1,CSYSHD           OR THE CSYS HEADERS                          
         MVC   CBLUNS,SUBSHD                                                    
         CLI   TGUSEQU,ULCB                                                     
         BE    SH10                                                             
         SPACE 1                                                                
         LA    R1,MKTHD            OR THE MKT HEADERS                           
         MVC   CBLUNS,UNITHD                                                    
         SPACE 1                                                                
SH10     MVC   8(L'CBLHD1,R2),0(R1)                                             
         OI    6(R2),X'80'                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   RF,SH10                                                          
         J     XIT                                                              
         SPACE 2                                                                
SH20     LA    R2,IRNFHEDH         SET FIRST INTERNET/NEWMEDIA FIELD            
         LA    R3,IRNLHEDH         AND LAST INTERNET/NEWMEDIA FIELD             
         B     SH50                                                             
SH30     LA    R2,MOVFHEDH                                                      
         LA    R3,MOVLHEDH                                                      
         B     SH50                                                             
SH40     LA    R2,INUFHEDH                                                      
         LA    R3,INULHEDH                                                      
         SPACE 1                                                                
SH50     LA    R1,IRNHD            FILL IN THE INTERNET                         
         CLI   TGUSEQU,UIRN                                                     
         BE    SH60                                                             
         CLI   TGUSEQU,UINU                                                     
         BE    SH60                                                             
         CLI   TGUSEQU,UMVI                                                     
         BE    SH60                                                             
         CLI   TGUSEQU,USIU                                                     
         BE    SH60                                                             
         CLI   TGUSEQU,USIR                                                     
         BE    SH60                                                             
         CLI   TGUSEQU,USMI                                                     
         BE    SH60                                                             
         LA    R1,NMRHD            OR NEW MEDIA HEADERS                         
         SPACE 1                                                                
SH60     CR    R2,R3                                                            
         JH    XIT                                                              
         MVC   8(L'IRNFHED,R2),0(R1)                                            
         OI    6(R2),X'80'                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     SH60                                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SAVE INPUTTED PAY VARABLES                            
         SPACE 1                                                                
SAVEINPS NTR1  BASE=*,LABEL=*                                                   
         OC    SVINPUNT(3),SVINPUNT IF HAVEN'T SAVED INPUT UNITS/MAJORS         
         BNZ   *+10                                                             
         MVC   SVINPUNT(3),TCUNITS  SAVE IT                                     
         SPACE 1                                                                
         OC    SVINPSUB,SVINPSUB    IF HAVEN'T SAVED INPUT SUBSCRIBERS          
         BNZ   *+10                                                             
         MVC   SVINPSUB,TCSUBS      SAVE IT                                     
         SPACE 1                                                                
         OC    SVUPTOTY,SVUPTOTY    IF HAVEN'T SAVED UPG "TO" TYPE              
         BNZ   *+10                                                             
         MVC   SVUPTOTY,TGUPTOTY    SAVE IT                                     
         SPACE 1                                                                
         OC    SVUPFRTY,SVUPFRTY    IF HAVEN'T SAVED UPG "FROM" TYPE            
         BNZ   *+10                                                             
         MVC   SVUPFRTY,TGUPFRTY    SAVE IT                                     
         SPACE 1                                                                
         OC    SVUSTYP,SVUSTYP      IF HAVEN'T SAVED USE TYPE                   
         BNZ   *+10                                                             
         MVC   SVUSTYP,TGUSTYP      SAVE IT                                     
         SPACE 1                                                                
         OC    SVUSTYST,SVUSTYST    IF HAVEN'T SAVED USE TYPE STATUS            
         JNZ   XIT                                                              
         MVC   SVUSTYST,TGUSTYST    SAVE IT                                     
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO RESET THE SAVED PAY VARIABLES                         
         SPACE 1                                                                
RESETINP NTR1  BASE=*,LABEL=*                                                   
         MVC   TCUNITS,SVINPUNT    RESTORE UNITS                                
         MVC   TCSUBS,SVINPSUB     RESTORE SUBSCRIBERS                          
         SPACE 1                                                                
         TM    TGUSSTA3,CBLUSE     IF CABLE PAYMENT                             
         BZ    RINPS10                                                          
         MVC   TGUSTYP,SVUSTYP     RESTORE USE TYPE                             
         SPACE 1                                                                
RINPS10  CLI   TGUSEQU,ULCB        IF LOCAL CABLE PAYMENT                       
         BNE   RINPS20                                                          
         MVC   TGUPTOTY,SVUPTOTY   RESTORE UPGRADE "TO" TYPE                    
         MVC   TGUPFRTY,SVUPFRTY           UPGRADE "FROM" TYPE                  
         MVC   TGUSTYP,SVUSTYP             USE TYPE                             
         MVC   TGUSTYST,SVUSTYST       AND USE STATUS                           
         SPACE 1                                                                
RINPS20  TM    TGUSSTA3,CBLUSE     IF CABLE PAYMENT                             
         JZ    XIT                                                              
         MVC   TGUSTYST,SVUSTYST   RESTORE USE STATUS                           
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE BUILDS KEY OF UH REC IN AIO AND ADDS                     
*              SOME ELEMENTS TO IT                                              
         SPACE                                                                  
         USING TLUHD,R3                                                         
BLDUH    NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO                                                           
         XC    TLUHKEY,TLUHKEY                                                  
         MVI   TLUHCD,TLUHCDQ      RECORD CODE                                  
         MVC   TLUHCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLUHUSE,TGUSCDE     USE CODE                                     
         MVC   TLUHINV,TGINV       INVOICE NUMBER (COMPLEMENTED)                
         MVC   TLUHLEN,DATADISP                                                 
         XC    TLUHSTAT(10),TLUHSTAT                                            
         SPACE                                                                  
         MVC   ELEMENT(L'TCTAUHEL),TCTAUHEL                                     
         GOTO1 ADDL                ADD NEW USAGE HISTORY ELEMENT                
         SPACE 1                                                                
         OC    ELTAVR,ELTAVR       IF HAVE VERSIONS ELEMENT                     
         BZ    BLDU5                                                            
         MVC   ELEMENT(L'ELTAVR),ELTAVR                                         
         GOTO1 ADDL                ADD IT TO RECORD                             
         SPACE 1                                                                
         USING TADDD,R4                                                         
BLDU5    XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         MVI   TADDEL,TADDELQ                                                   
         MVI   TADDLEN,TADDLNQ                                                  
         MVC   TADDDATE,DUEDATE                                                 
         MVC   TADDOVRD,DUEDATE2                                                
         GOTO1 ADDL                ADD DUE DATE EL. TO RECORD                   
         SPACE 1                                                                
         BRAS  RE,UHVEREL          BUILD TAFNTVRE ON USAGE RECORD               
         OC    SVADVDA,SVADVDA     IF HAVE ADVICE RECORD                        
         JZ    XIT                                                              
         L     R4,AIO2             AND IT IS IN AIO2                            
         CLI   0(R4),TLDVCDQ                                                    
         JNE   XIT                                                              
         MVI   ELCODE,TAUNELQ      COPY UNIT TOTALS FOR CONTRACT YRS            
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         MVC   ELEMENT,0(R4)                                                    
         GOTO1 ADDL                                                             
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  ROUTINE TO BUILD TAFNTVRE,TAFNTVNR ELEMENT FOR VNR AND VRE                   
*  FOR CAST USAGE HISTORY RECORD                                                
UHVEREL  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ELEMENT                                                       
         USING TAFND,R4                                                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAFNEL,TAFNELQ                                                   
         LA    R0,3                                                             
         MVI   TAFNTYPE,TAFNTVRE                                                
         CLI   TGUSEQU,UVRE                                                     
         BE    *+16                                                             
         CLI   TGUSEQU,UVNR                                                     
         JNE   XIT                                                              
         MVI   TAFNTYPE,TAFNTVNR                                                
*                                                                               
         LA    RF,TAFNNAME                                                      
         L     RE,ACSTVERS                                                      
UHVERE10 CLI   0(RE),X'FF'                                                      
         BE    UHVERE20                                                         
         MVC   0(1,RF),0(RE)                                                    
         AHI   R0,1                                                             
         AHI   RF,1                                                             
         AHI   RE,1                                                             
         B     UHVERE10                                                         
*                                                                               
                                                                                
UHVERE20 STC   R0,TAFNLEN                                                       
         GOTO1 ADDELEM                                                          
         J     XIT                                                              
                                                                                
******************************************************************              
*  ROUTINE TO BUILD TAFNTVRE,TAFNTVNR, TAFNTVN1                                 
*  ELEMENT FOR VNR AND VRE                                                      
*  FOR CAST RECORD                                                              
******************************************************************              
CASTVEL  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UVRE                                                     
         BE    *+8                                                              
         CLI   TGUSEQU,UVNR                                                     
         JNE   XIT                                                              
         L     RE,ACSTVERS          CHECK ENTIRE CASTVER                        
         CLI   0(RE),X'FF'          EXIT IF NO VERSIONS TO ADD                  
         JE    XIT                                                              
         MVC   AIO,AIO2                                                         
         LA    R4,KEY                                                           
         USING TLCAD,R4                                                         
         MVC   KEY(TLCASEQ-TLCAD),SVSVCKEY        RE-READ SAVED KEY             
         MVC   TLCASORT,TGCSORT                                                 
         GOTO1 HIGH                                                             
         CLC   TLCAKEY(TLCASEQ+L'TLCASEQ-TLCAD),KEYSAVE                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         XC    BYTE,BYTE           USE BYTE AS FLAG                             
*                                                                               
         L     R4,AIO2                                                          
         USING TAFND,R4                                                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELCODE,TAFNELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    CASTVE04                                                         
CASTVE02 BRAS  RE,NEXTEL                                                        
         JNE   CASTVE40                                                         
CASTVE04 CLI   TAFNTYPE,TAFNTVN1   FOUND VN1 NAME ELEMENT - SKIP                
         BNE   CASTVE4H                                                         
* FOUND EXISTING TAFNTVN1 ELEMENT                                               
         TM    BYTE,PROC_VN1       TEST-   PROCESSED VN1 ELEMENT                
         BO    CASTVE02                                                         
         OI    BYTE,PROC_VN1       ALREADY PROCESSED TAFNTVN1 ELEMENT           
* IF CREDIT PAYMENT DELETE TAFNTVN1 IF 1ST VARIATION IS IN CSTVERS              
* AND RE-ADD WITH NEW FIRST VNR VERSION                                         
         TM    PAYSTAT1,CREDIT     IF CREDIT PAYMENT                            
         BZ    CASTVE02                                                         
         CLI   TGUSEQU,UVNR                                                     
         BNE   CASTVE02                                                         
         L     RE,ACSTVERS          CHECK ENTIRE CASTVER                        
CASTVE4A CLI   0(RE),X'FF'          TO SEE IF WE ARE CREDITING EXISTING         
         BE    CASTVE02             1ST VARIATION                               
         CLC   TAFNNAME(1),0(RE)                                                
         BE    CASTVE4D            IF CREDITING 1ST VARIATION                   
         AHI   RE,1                                                             
         B     CASTVE4A                                                         
CASTVE4D MVI   TAFNEL,X'FF'        DELETE ORIGINAL TAFN ELEMENT                 
         B     CASTVE02                                                         
*                                                                               
CASTVE4H CLI   TAFNTYPE,TAFNTVRE                                                
         BNE   CASTVE4K                                                         
         CLI   TGUSEQU,UVNR        VRE PAYMENT -FOUND VNR ELEMENT-              
         BE    CASTVE02            IGNORE SKIP                                  
         CLI   TGUSEQU,UVRE        VRE PAYMENT -FOUND VRE ELEMENT               
         BE    CASTVE30            MERGE EXISTING ELEMENT W CSTVERS             
         DC    H'0'                                                             
*                                                                               
CASTVE4K CLI   TAFNTYPE,TAFNTVNR                                                
         BNE   CASTVE4J                                                         
         CLI   TGUSEQU,UVNR        VNR PAYMENT -FOUND VNR ELEMENT-              
         BE    CASTVE30            MERGE WITH CASTVER                           
         CLI   TGUSEQU,UVRE        VNR PAYMENT -FOUND VRE ELEMENT               
         BE    CASTVE02            IGNORE AND SKIP                              
*                                                                               
CASTVE4J B     CASTVE02                                                         
*                                                                               
* FOUND  EXISTING TAFN ELEMENT- MERGE WITH ACSTVERS                             
* REGULAR PAYMENT-MERGE WITH VERSION IN CSTVERS                                 
* CREDIT PAYMENT- DELETE VERSION IN CSTVERS FROM TAFN                           
*                 IF VERSION IS 1ST VARIATION ALSO DELETE TAFNTV1               
CASTVE30 L     RE,ACSTVERS                                                      
         ZIC   RF,TAFNLEN                                                       
                                                                                
         CLI   TGUSEQU,UVNR                                                     
         BNE   *+8                                                              
         OI    BYTE,PROC_VNR           ALREADY PROCESSED VNR ELEMENT            
         CLI   TGUSEQU,UVRE                                                     
         BNE   *+8                                                              
         OI    BYTE,PROC_VRE           ALREADY PROCESSED VRE ELEMENT            
         TM    PAYSTAT1,CREDIT     IF CREDIT PAYMENT                            
         BO    CASTVE35                                                         
*  REGULAR PAYMENTS - MERGE TAFN VERSIONS WITH ACSTVERS                         
         XC    ELEMENT,ELEMENT                                                  
         SHI   RF,1                    COPY ORIGINAL TAFN VERSIONS              
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R4)                                                 
         MVI   TAFNEL,X'FF'            DELETE ORIGINAL TAFN ELEMENT             
         LA    RF,ELEMENT                                                       
         ZIC   R0,TAFNLEN                                                       
         AR    RF,R0                                                            
CASTVE34 CLI   0(RE),X'FF'             MERGE IN CSTVERS VERSIONS                
         BE    CASTVE39                                                         
         MVC   0(1,RF),0(RE)                                                    
         AHI   RF,1                                                             
         AHI   RE,1                                                             
         AHI   R0,1                                                             
         B     CASTVE34                                                         
*  CREDIT PAYMENTS - ONLY KEEP VERSIONS NOT IN CSTVERS                          
CASTVE35 LA    RF,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         L     RE,ACSTVERS                                                      
         LA    R3,TAFNNAME                                                      
         MVC   ELEMENT(3),0(R4)                                                 
         MVI   TAFNEL,X'FF'       DELETE ORGINAL TAFN ELEMENT                   
         AHI   RF,3                                                             
         ZIC   R1,TAFNLEN                                                       
         SHI   R1,3               R1=NUM OF VERSIONS IN TAFN ELEMENT            
         LA    R0,3               ELEMENT LENGTH COUNTER                        
CASTVE36 CLI   0(RE),X'FF'        VERSION IN TAFNNAME IS NOT IN                 
         BE    CASTVE37           CSTVER SO WE CAN KEEP IT                      
         CLC   0(1,RE),0(R3)      IF TAFN VERSION IS IN CSTVER                  
         BE    CASTVE38                                                         
         AHI   RE,1               THEN SKIP IT                                  
         B     CASTVE36                                                         
*                                                                               
CASTVE37 MVC   0(1,RF),0(R3)      COPY VERSION IN NEW ELEMENT                   
         AHI   R0,1                                                             
         AHI   RF,1               NEXT OUTPUT AREA                              
CASTVE38 L     RE,ACSTVERS        RESET RE BACK TO ACSTVERS                     
         AHI   R3,1               NEXT TAFN VERSION TO PROCESS                  
         BCT   R1,CASTVE36                                                      
****     MVC   WORK(1),ELEMENT+3  SAVE NEW FIRST VNR VERSION                    
*                                                                               
CASTVE39 STC   R0,ELEMENT+1                                                     
         CHI   R0,3               DONT ADD TAFN ELEMENT IF NO                   
         BH    *+12               VERSIONS ARE ON ELEMENT                       
         MVI   WORK,0             INDICATE IN WORK NO 1ST VERSION               
         B     CASTVE02                                                         
         BAS   RE,SORTTFN         SORT VERSIONS IN ELEMENT                      
         GOTO1 ADDELEM                                                          
         MVC   WORK(1),ELEMENT+3  SAVE NEW FIRST VNR VERSION                    
         B     CASTVE02                                                         
                                                                                
* DONT HAVE TAFNTVN1(1ST NON RENDERED VERSION) - ADD AS NEW                     
                                                                                
CASTVE40 LA    R4,ELEMENT                                                       
         USING TAFND,R4                                                         
         TM    PAYSTAT1,CREDIT     NEVER ADD A VN1 TAFN FOR CREDIT              
         BO    CASTVE50            ONLY CAN ADD FOR PAY                         
         TM    BYTE,PROC_VN1       ONLY ADD TAFNTVN1 ONCE                       
         BO    CASTVE50            FOR VNR PAYMENT ONLY                         
         CLI   TGUSEQU,UVNR                                                     
         BNE   CASTVE50                                                         
         L     RE,ACSTVERS                                                      
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNTYPE,TAFNTVN1                                                
         MVC   TAFNNAME(1),0(RE)                                                
         MVI   TAFNLEN,4                                                        
         MVC   AIO,AIO2                                                         
         GOTO1 ADDELEM                                                          
                                                                                
* DONT HAVE TAFNTVRE OR TAFNTVNR ELEMENT - ADD AS NEW  FROM CASTVERS            
                                                                                
CASTVE50 LA    R4,ELEMENT                                                       
         USING TAFND,R4                                                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAFNEL,TAFNELQ                                                   
         LA    R0,3                                                             
         CLI   TGUSEQU,UVRE           IF PAYING VRE AND PROCESSED               
         BNE   CASTVE56               VRE TAFN ELEMENT THEN EXIT                
         MVI   TAFNTYPE,TAFNTVRE                                                
         TM    BYTE,PROC_VRE                                                    
         BO    CASTVE80                                                         
         B     CASTVE58                                                         
CASTVE56 CLI   TGUSEQU,UVNR           IF PAYING VNR AND PROCESSED               
         JNE   CASTVE80               VRE TAFN ELEMENT THEN EXIT                
         MVI   TAFNTYPE,TAFNTVNR                                                
         TM    BYTE,PROC_VNR                                                    
         BO    CASTVE80                                                         
*                                                                               
* CREATE NEW TAFNTVRE OR TAFNTVNR ELEMENTS FROM ACSTVERS                        
*                                                                               
CASTVE58 LA    RF,TAFNNAME                                                      
         L     RE,ACSTVERS                                                      
CASTVE60 CLI   0(RE),X'FF'                                                      
         BE    CASTVE70                                                         
         MVC   0(1,RF),0(RE)                                                    
         AHI   R0,1                                                             
         AHI   RF,1                                                             
         AHI   RE,1                                                             
         B     CASTVE60                                                         
CASTVE70 STC   R0,TAFNLEN                                                       
         MVC   AIO,AIO2                                                         
         GOTO1 ADDELEM                                                          
                                                                                
CASTVE80 MVI   ELCODE,X'FF'        DELETE OLD ELEMENTS                          
         GOTO1 REMELEM                                                          
         GOTO1 MYPUTREC            WRITE BACK CHANGED RECORD                    
         J     XIT                                                              
                                                                                
PROC_VN1 EQU   X'80'                                                            
PROC_VNR EQU   X'40'                                                            
PROC_VRE EQU   X'20'                                                            
********************************************                                    
*CODE TO SORT THE VERSION LIST IN ELEMENT  *                                    
********************************************                                    
SORTTFN  NTR1                                                                   
         LA    R1,BLOCK          SORT OUTPUT AREA                               
SORT180  LA    RE,ELEMENT+3                                                     
*                                                                               
SORT190  ZIC   RF,ELEMENT+1                                                     
         LA    R3,ELEMENT                                                       
         AR    R3,RF              EOL?                                          
         CR    RE,R3                                                            
         BNL   SORT230                                                          
         CLI   0(RE),0                                                          
         BNE   SORT200                                                          
         LA    RE,1(RE)                                                         
         B     SORT190                                                          
*                                                                               
SORT200  LR    RF,RE             RE=A OF 1ST MEANINGFUL VALUE                   
SORT210  LA    RF,1(RF)          COMPARE IT TO NEXT MEANINGFUL VAL              
         CR    RF,R3             EOL?                                           
         BNL   SORT220                                                          
         CLI   0(RF),0           ZERO VALUE MEANS ALREADY PROCESSED             
         BE    SORT210                                                          
         CLC   0(1,RE),0(RF)                                                    
         BL    SORT210                                                          
         LR    RE,RF                                                            
         B     SORT210                                                          
* ADD THE LOWEST VERSION TO OUTPUT AREA  RE=LOWEST VALUE                        
SORT220  AHI   R0,1              ADD VERSIONS TO INPUT TABLE FROM               
         MVC   0(1,R1),0(RE)     SMALLEST TO LARGEST                            
         LA    R1,1(R1)                                                         
         MVI   0(RE),0                                                          
         B     SORT180                                                          
SORT230  MVI   0(R1),X'FF'                                                      
* MOVE BACK SORTED VERSIONS INTO ELEMENT                                        
         ZIC   RE,ELEMENT+1                                                     
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT+3(0),BLOCK                                               
         J     XIT                                                              
********************************************                                    
*                                                                               
*              ROUTINE BUILDS CNET/MKT ELEMENTS AND USAGE HISTORY               
*              ELEMENT FOR A PARTICULAR CAST MEMBER ON A CBL,SCB,               
*              SWS OR WSC PAYMENT TO A VERSION                                  
         SPACE 1                                                                
MKTBLDUH NTR1  BASE=*,LABEL=*                                                   
         USING TAMTD,R4                                                         
         TM    TGUSSTA3,USEMTAB    IF WILDSPOT OR CABLE                         
         JZ    XIT                                                              
         SPACE 1                                                                
         L     R3,AMKTTABL         R3=A(CNETS/MKTS/CSYS ON PAYMENT)             
         L     RE,AIO3                                                          
         CLC   0(L'CASTHEAD,RE),CASTHEAD                                        
         BNE   MBU20                                                            
         AHI   RE,L'CASTHEAD                                                    
         LR    R3,RE                                                            
         SPACE 1                                                                
         USING MKTTABLD,R3                                                      
MBU20    LA    R4,ELEMENT                                                       
MBU30    CLI   MTCODE,X'FF'        ADD MKT/CNET EL'S TO USAGE HISTORY           
         BE    MBU50                                                            
         TM    MTSTAT,MTNOCALC                                                  
         BNZ   MBU40                                                            
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAMTEL,TAMTELQ                                                   
         MVI   TAMTLEN,TAMTULNQ                                                 
         MVC   TAMTCODE,MTCODE                                                  
         MVC   TAMTCYCS,MT1DATE                                                 
         MVC   TAMTCYCE,MTCYCEND                                                
         MVC   TAMTINUM,MTINUM                                                  
         GOTO1 ADDL                                                             
MBU40    LA    R3,MKTLNQ(R3)                                                    
         B     MBU30                                                            
         DROP  R3                                                               
         SPACE 1                                                                
MBU50    L     RE,AIO3                     IF BUILDING USAGE HISTORY            
         CLC   0(L'CASTHEAD,RE),CASTHEAD   FOR CAST MEMBER ON A VERSION         
         BNE   MBU330                                                           
         SPACE 1                                                                
         USING TAUHD,R4                                                         
         L     R4,AIO            GET USAGE HISTORY ELEMENT                      
         MVI   ELCODE,TAUHELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         SPACE 1                                                                
         TM    TGUSSTA3,CBLUSE   IF CABLE OR SPANISH CABLE                      
         BZ    MBU70                                                            
         LA    R2,TAUHCBUN       R2=A(WHERE TO STORE CURRENT UNITS)             
         MVC   TAUHICBU,TAUHCBUN SAVE PREVIOUS UNITS                            
         MVC   TAUHCAUT,TAUHCBUN SET START FOR AUTOMATIC UNITS                  
         MVC   TCSUBS,TAUHCSUB   SET START FOR SUBSCRIBERS                      
         B     MBU90                                                            
         SPACE 1                                                                
MBU70    CLI   TGUSEQU,ULCB      IF LOCAL CABLE                                 
         BNE   MBU80                                                            
         LA    R2,TAUHSUBS       R2=A(WHERE TO STORE CURRENT SUBS)              
         MVC   TAUHISUB,TCSUBS   SAVE PREVIOUS SUBSCRIBERS                      
         MVC   TAUHSUBS,TCSUBS   SET START FOR SUBSCRIBERS                      
         MVC   TAUHASUB,TCSUBS   SET START FOR AUTOMATIC SUBSCRIBERS            
         MVC   TAUHFRTY,TGUPFRTY SET UPGRADE "FROM" TYPE                        
         B     MBU90                                                            
         SPACE 1                                                                
MBU80    LA    R2,TAUHUNT         ELSE, R2=A(WHERE TO STORE CUR UNITS)          
         MVC   TAUHIUNT,TAUHUNT   SET PREVIOUS UNITS                            
         MVC   TAUHWAUT,TAUHUNT   SET START FOR AUTOMATIC UNITS                 
         SPACE 1                                                                
MBU90    TM    PAYSTAT1,OVERMKTS  IF OVERRIDING                                 
         BZ    MBU130                                                           
         TM    TGUSSTA3,CBLUSE    A CABLE OR SPANISH CABLE PAYMENT              
         BZ    MBU110                                                           
         LH    RE,TAUHCBUN        PREVIOUS UNITS                                
         AH    RE,SVINPUNT        PLUS THE OVERRIDE UNITS                       
         STH   RE,TAUHCBUN        EQUALS THE CURRENT UNITS                      
         LA    R2,TAUHCAUT        GO CALC AUTOMATIC UNITS INSTEAD               
         B     MBU130                                                           
         SPACE 1                                                                
MBU110   CLI   TGUSEQU,ULCB       IF OVERRIDING A LOCAL CABLE PAYMENT           
         BNE   MBU120                                                           
         ZICM  RE,SVINPSUB,4      OVERRIDE SUBSCRIBERS                          
         A     RE,TAUHSUBS        PLUS THE PREVIOUS SUBSCRIBERS                 
         STCM  RE,15,TAUHSUBS     EQUALS THE CURRENT SUBSCRIBERS                
         LA    R2,TAUHASUB        GO CALC AUTOMATIC SUBS INSTEAD                
         B     MBU130                                                           
         SPACE 1                                                                
MBU120   LH    RE,TAUHUNT         IF OVERRIDING ANOTHER PAY TYPE                
         AH    RE,SVINPUNT        PREV UNITS PLUS THE OVERRIDE UNITS            
         STH   RE,TAUHUNT         EQUALS THE CURRENT UNITS                      
         LA    R2,TAUHWAUT        GO CALCULATE AUTOMATIC UNITS INSTEAD          
         SPACE 1                                                                
MBU130   LA    RE,TAUHLCST        RE=A(USAGE HISTORY STATUS BYTE)               
         CLI   TGUSEQU,ULCB                                                     
         BE    MBU135                                                           
         LA    RE,TAUHCSTA                                                      
         TM    TGUSSTA3,CBLUSE                                                  
         BNZ   MBU135                                                           
         LA    RE,TAUHSTAT                                                      
MBU135   NI    0(RE),X'FF'-TAUHWCRD-TAUHSFUP                                    
         TM    PAYSTAT1,CREDIT   IF CREDIT PAYMENT                              
         BZ    *+8                                                              
         OI    0(RE),TAUHWCRD    TURN ON CREDIT BIT                             
         TM    LCLSTAT3,FORCEUPG IF FORCING UPGRADE                             
         BZ    MBU140                                                           
         OI    0(RE),TAUHSFUP    TURN ON FORCING UPGRADE BIT                    
         DROP  R4                                                               
         SPACE 1                                                                
MBU140   MVC   AIO,AIO3          BASED ON PAY TYPE                              
         MVI   TGMTTYPE,TANPNET  SET TO READ CNET/TMKT/RMKT OR CSYS             
         TM    TGUSSTA3,CBLUSE                                                  
         BNZ   MBU150                                                           
         MVI   TGMTTYPE,C'S'                                                    
         CLI   TGUSEQU,ULCB                                                     
         BE    MBU150                                                           
         MVI   TGMTTYPE,C'T'                                                    
         CLI   TGMEEQU,RADIO                                                    
         BNE   MBU150                                                           
         MVI   TGMTTYPE,C'R'                                                    
         SPACE 1                                                                
         USING TAUHD,R4                                                         
MBU150   L     R4,AIO2                                                          
         MVI   ELCODE,TAMTELQ    IF NO CNET/TMKT/RMKT/CSYS ENTERED              
         BRAS  RE,GETEL          FOR THIS PAYMENT                               
         BE    MBU190                                                           
         L     R4,AIO2                                                          
         MVI   ELCODE,TAUHELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   MBU280                                                           
         TM    TGUSSTA3,CBLUSE   FOR CABLE AND SPANISH CABLE                    
         BZ    MBU170            AUTOMATIC UNITS ARE SAME AS INITIAL            
         MVC   TAUHCAUT,TAUHICBU UNITS                                          
         B     MBU280                                                           
MBU170   CLI   TGUSEQU,ULCB      FOR LOCAL CABLE                                
         BNE   MBU180            AUTOMATIC SUBSCRIBERS ARE SAME AS              
         MVC   TAUHASUB,TAUHISUB INITIAL SUBSCRIBERS                            
         B     MBU280                                                           
MBU180   MVC   TAUHWAUT,TAUHIUNT FOR EVERYTHING ELSE                            
         B     MBU280            AUTOMATIC UNITS ARE SAME AS INITIAL            
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAMTD,R3                                                         
MBU190   LR    R3,R4             IF CNET/TMKT/RMKT/CSYS ENTERED                 
MBU200   CLI   0(R3),TAMTELQ     BUMP THROUGH ELEMENTS WITH R3                  
         BNE   MBU280            GET THE CNET/MKT/CSYS RECORD                   
         LHI   RF,TLMTCDQ                                                       
         BRAS  RE,USEALPH                                                       
         BNE   *+8                                                              
         LHI   RF,TLMTALDQ                                                      
         GOTO1 RECVAL,DMCB,(RF),(X'A4',TAMTCODE)                                
         BE    MBU210                                                           
         CLI   TGMTTYPE,TANPNET                                                 
         BNE   MBU270                                                           
         MVI   TGMTTYPE,C'S'                                                    
         GOTO1 RECVAL,DMCB,TLMTCDQ,(X'A4',TAMTCODE)                             
         MVI   TGMTTYPE,TANPNET                                                 
         BNE   MBU270                                                           
         B     MBU260                                                           
         SPACE 1                                                                
         USING TAMSD,R4                                                         
MBU210   L     R4,AIO            GET CNET/MKT/CSYS WEIGHT ELEMENT               
         MVI   ELCODE,TAMSELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
MBU220   BRAS  RE,NEXTEL                                                        
         BE    MBU230                                                           
         SPACE 1                                                                
         CLI   TGUSEQU,USWS      FOR SPANISH WILDSPOT,                          
         BNE   MBU270            GET THE SPANISH WEIGHT                         
         LH    RE,0(R2)                                                         
         AHI   RE,1              IF NO SPANISH WEIGHT,                          
         STH   RE,0(R2)          DEFAULT TO WEIGHT OF ONE                       
         B     MBU270                                                           
MBU230   CLI   TGUSEQU,USWS                                                     
         BNE   MBU240                                                           
         CLI   TAMSTYPE,C'S'                                                    
         BNE   MBU220                                                           
         SPACE 1                                                                
MBU240   CLI   TGUSEQU,ULCB      ADD WEIGHT OF CNET/MKT/CSYS                    
         BE    MBU250            TO CURRENT UNITS/SUBSCRIBERS                   
         LH    RE,0(R2)                                                         
         A     RE,TAMSWGHT                                                      
         STH   RE,0(R2)                                                         
         B     MBU270                                                           
MBU250   L     RE,0(R2)                                                         
         A     RE,TAMSWGHT                                                      
         ST    RE,0(R2)                                                         
         B     MBU270                                                           
         SPACE 1                                                                
MBU260   LH    RE,0(R2)          IF CABLE OR SPANISH CABLE PAYMENT              
         AHI   RE,1              AND PAYING A CSYS                              
         STH   RE,0(R2)          ADD ONE UNIT                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAMSELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   MBU270                                                           
         ZICM  RF,TCSUBS,4       AND ADD WEIGHT TO SUBSCRIBERS                  
         A     RF,TAMSWGHT                                                      
         STCM  RF,15,TCSUBS                                                     
         DROP  R4                                                               
         SPACE 1                                                                
MBU270   ZIC   RE,TAMTLEN        AND BUMP TO NEXT CNET/MKT/CSYS                 
         AR    R3,RE             ELEMENT                                        
         B     MBU200                                                           
         DROP  R3                                                               
         SPACE 1                                                                
MBU280   TM    TGUSSTA3,CBLUSE   WHEN DONE PROCESSING ALL INPUT                 
         BZ    MBU290            AND IF PAYING CABLE OR SPANISH CABLE           
         OC    TCSUBS,TCSUBS     AND CSYS CODES WERE ENTERED                    
         BZ    MBU290                                                           
         XR    RE,RE                                                            
         ZICM  RF,TCSUBS,4       DIVIDE CURRENT SUBSCRIBERS                     
         D     RE,=F'350000'     BY 350,000                                     
         LR    R1,RF                                                            
         XR    RE,RE                                                            
         ZICM  RF,12(R2),4       THEN DIVIDE PREVIOUS SUBSCRIBERS               
         TM    PAYSTAT1,OVERMKTS BY 350,000                                     
         BZ    *+8                                                              
         ZICM  RF,8(R2),4                                                       
         D     RE,=F'350000'                                                    
         SR    R1,RF                                                            
         AH    R1,0(R2)                                                         
         STH   R1,0(R2)          ADD THE DIFFERENCE BETWEEN THE                 
         LA    RE,12(R2)         QUOTIENTS TO CURRENT UNITS                     
         TM    PAYSTAT1,OVERMKTS                                                
         BZ    *+8                                                              
         LA    RE,8(R2)                                                         
         MVC   0(L'TAUHCSUB,RE),TCSUBS                                          
         SPACE 1                                                                
MBU290   MVC   AIO,AIO2                                                         
         SPACE 1                                                                
         TM    PAYSTAT1,OVERMKTS IF NOT AN OVERRIDE PAYMENT                     
         BNZ   MBU320                                                           
         CLI   TWASCR,SCR5F      COPY NUMBER OF AUTOMATIC                       
         BNE   MBU300            UNITS/SUBSCRIBERS INTO UNITS/                  
         CLI   TGUSEQU,ULCB      SUBSCRIBERS PAID                               
         BE    MBU310                                                           
         MVC   4(L'TAUHCAUT,R2),0(R2)                                           
         J     XIT                                                              
MBU300   SHI   R2,2                                                             
         MVC   0(L'TAUHCAUT,R2),2(R2)                                           
         J     XIT                                                              
MBU310   MVC   8(L'TAUHASUB,R2),0(R2)                                           
         SPACE 1                                                                
MBU320   L     RE,AIO3                                                          
         MVC   0(L'CASTHEAD,RE),CASTHEAD                                        
         SPACE 1                                                                
         USING TAUHD,R4                                                         
MBU330   CLI   TGUSEQU,ULCB      IF LOCAL CABLE PAYMENT                         
         JNE   XIT                                                              
         L     R4,AIO            GET USAGE HISTORY ELEMENT                      
         MVI   ELCODE,TAUHELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         L     RE,AIO3                                                          
         CLC   0(L'CASTHEAD,RE),CASTHEAD                                        
         BNE   MBU360                                                           
         GOTO1 STLCBTY           AND, BASED ON SUBSCRIBERS, SET TYPE            
         SPACE 1                                                                
MBU360   LA    R2,TAUHCSYS                                                      
         DROP  R4                                                               
         L     R4,AIO            FINALLY, COUNT THE NUMBER OF                   
         MVI   ELCODE,TAMTELQ    CSYS CODES ON THE PAYMENT                      
         BRAS  RE,GETEL          AND SAVE THAT NUMBER                           
         B     *+8                                                              
MBU370   BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         LH    RE,0(R2)                                                         
         AHI   RE,1                                                             
         STH   RE,0(R2)                                                         
         B     MBU370                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET LCB TYPES FOR VERSION PAYMENTS                    
         SPACE 1                                                                
STLCBTYP NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,ULCB        IF USE IS LOCAL CABLE                        
         JNE   XIT                                                              
         SPACE 1                                                                
         MVI   TGUPFRTY,0          CLEAR UPGRADE "FROM" TYPE                    
         XC    TCSUBS,TCSUBS       N'SUBSCRIBERS AND UPGRADE STATUS             
         NI    TGUSTYST,X'FF'-UPGRADE                                           
         SPACE 1                                                                
         MVI   TGBYTE,UHCUR        SET TRIED THIS USE FOR CURR AGY/COM          
         MVC   AIO,AIO3                                                         
         SPACE 1                                                                
         USING TLUHD,R3                                                         
         LA    R3,KEY              BUILD PARTIAL KEY FOR RECORD                 
         XC    TLUHKEY,TLUHKEY                                                  
         MVI   TLUHCD,TLUHCDQ      RECORD CODE                                  
         MVC   TLUHCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLUHCSEQ,TGCSORT+4  CAST INPUT SEQ NUMBER                        
         MVC   TLUHUSE,TGUPFRCD    "FROM" USE CODE                              
         GOTO1 HIGH                GET DIRECTORY RECORD                         
         B     SLCBT20                                                          
SLCBT10  GOTO1 SEQ                 GET NEXT DIRECTORY RECORD                    
SLCBT20  CLC   TLUHKEY(TLUHINV-TLUHD),KEYSAVE                                   
         BE    SLCBT30                                                          
         SPACE 1                                                                
         GOTO1 TPRELCB             CHECK USAGE HISTORY FOR PREVIOUS             
         BNE   SLCBT70             AGY/COMM'L AND (POSSIBLY) LCB                
         SPACE 1                                                                
SLCBT30  GOTO1 GETREC              GET THE RECORD                               
         SPACE 1                                                                
         USING TAUHD,R4                                                         
         MVI   ELCODE,TAUHELQ      GET USAGE HISTORY ELEMENT                    
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   SLCBT10                                                          
         CLC   TCPCYCS,TAUHSTRT    FOR SAME CYCLE START                         
         BNE   SLCBT10                                                          
         SPACE 1                                                                
         MVC   TGUPFRTY,TAUHTYPE   SET THE UPGRADE "FROM" TYPE                  
         OI    TGUSTYST,UPGRADE    SET UPGRADE STATUS AS ON                     
         SPACE 1                                                                
         CLI   TAUHLEN,TAUHLNQ     SAVE PREVIOUS SUBSCRIBERS                    
         BL    SLCBT40                                                          
         MVC   TCSUBS,TAUHSUBS                                                  
         B     SLCBT70                                                          
SLCBT40  GOTO1 STLCBSB                                                          
         DROP  R4                                                               
         SPACE 1                                                                
SLCBT70  MVC   AIO,AIO2                                                         
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO RESET LCB TYPES FOR VERSION PAYMENTS                  
         SPACE 1                                                                
RSLCBTYP NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,ULCB                                                     
         JNE   XIT                                                              
         MVC   TGUPFRTY,SVUPFRTY                                                
         MVC   TGUSTYST,SVUSTYST                                                
         MVC   TCSUBS,SVINPSUB                                                  
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DETERMINE IF ALPHA CODE SHOULD BE USED TO             
*              READ FOR TMKT RECORDS                                            
         SPACE 1                                                                
USEALPH  NTR1  BASE=*,LABEL=*                                                   
         TM    TGMEEQU,LIKETV                                                   
         JZ    NO                                                               
         TM    TGUSSTA3,CBLUSE                                                  
         JNZ   NO                                                               
         CLI   TGUSEQU,ULCB                                                     
         JNE   YES                                                              
         J     NO                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DOES SOME SPECIAL PROCESSING FOR VERSION PAYMENTS    *         
***********************************************************************         
                                                                                
VERADJ   NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,0           IF COMMERCIAL IS A VERSION                   
         JE    XIT                                                              
         TM    TGUSSTA3,USEMTAB    AND USE IS CABLE OR WILDSPOT ...             
         JZ    XIT                                                              
                                                                                
         XC    TCSUBS,TCSUBS       INITIALIZE VARIABLES                         
         XC    TGFULL,TGFULL                                                    
         XC    TCUNITS,TCUNITS                                                  
         XC    TCMAJORS,TCMAJORS                                                
         XC    TCTUSES,TCTUSES                                                  
                                                                                
         USING TLUHD,R3                                                         
         LA    R3,KEY              READ ALL CAST LEVEL USAGE HISTORY            
         XC    KEY,KEY             FOR THIS COMMERCIAL/USE                      
         MVI   TLUHCD,TLUHCDQ                                                   
         MVC   TLUHCOM,TGCOM                                                    
         MVC   TLUHCSEQ,=H'1'                                                   
         MVC   TLUHUSE,TGUSCDE                                                  
         GOTO1 HIGH                                                             
         J     VA20                                                             
VA10     GOTO1 SEQ                                                              
VA20     CLC   KEY(TLUHCSEQ-TLUHD),KEYSAVE                                      
         JNE   VA70                                                             
         CLC   TLUHUSE,TGUSCDE                                                  
         JNE   VA10                                                             
         GOTO1 GETREC                                                           
                                                                                
         USING TAUHD,R4                                                         
         L     R4,AIO              GET USAGE HISTORY ELEMENT                    
         MVI   ELCODE,TAUHELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   VA10                                                             
         CLC   TAUHSTRT,TCPCYCS    ONLY CONSIDER THOSE WITH SAME                
         JNE   VA10                CYCLE START DATE                             
                                                                                
         CLI   TGUSEQU,ULCB        SAVE HIGHEST SUBSCRIBER/UNIT/USE             
         JNE   VA30                COUNT ENCOUNTERED                            
         CLC   TAUHSUBS,TCSUBS                                                  
         JNH   *+10                                                             
         MVC   TCSUBS,TAUHSUBS                                                  
         CLC   TLUHINV,TGINV                                                    
         JE    VA10                                                             
         CLC   TAUHSUBS,TGFULL                                                  
         JNH   VA10                                                             
         MVC   TGFULL,TAUHSUBS                                                  
         J     VA10                                                             
         DROP  R3                                                               
                                                                                
VA30     TM    TGUSSTA3,CBLUSE                                                  
         JZ    VA40                                                             
         CLC   TAUHUSN,TCTUSES                                                  
         JNH   VA10                                                             
         MVC   TCTUSES,TAUHUSN                                                  
         MVC   TCUNITS,TAUHUSN                                                  
         J     VA10                                                             
                                                                                
VA40     OC    TCUNITS,TCUNITS                                                  
         BZ    VA40A                                                            
         CLC   TAUHUNT,TCUNITS                                                  
         JL    VA10                                                             
         JE    VA50                                                             
VA40A    MVC   TCUNITS,TAUHUNT                                                  
         J     VA60                                                             
                                                                                
VA50     XR    R0,R0                                                            
         TM    TCMAJORS,NY                                                      
         BZ    *+8                                                              
         AHI   R0,1                                                             
         TM    TCMAJORS,CHI                                                     
         BZ    *+8                                                              
         AHI   R0,1                                                             
         TM    TCMAJORS,LA                                                      
         BZ    *+8                                                              
         AHI   R0,1                                                             
                                                                                
         XR    R1,R1                                                            
         TM    TAUHMAJ,NY                                                       
         BZ    *+8                                                              
         AHI   R1,1                                                             
         TM    TAUHMAJ,CHI                                                      
         BZ    *+8                                                              
         AHI   R1,1                                                             
         TM    TAUHMAJ,LA                                                       
         BZ    *+8                                                              
         AHI   R1,1                                                             
                                                                                
         CR    R0,R1                                                            
         BNL   VA10                                                             
                                                                                
VA60     OC    TCMAJORS,TAUHMAJ                                                 
         J     VA10                                                             
         DROP  R4                                                               
                                                                                
VA70     CLI   TGUSEQU,ULCB      IF USE IS LOCAL CABLE                          
         JNE   VA100                                                            
         LA    RE,SUBSTAB        FIND NEW NUMBER OF SUBSCRIBERS IN              
VA80     CLI   0(RE),X'FF'       TABLE                                          
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLC   TCSUBS,0(RE)                                                     
         JNH   VA90                                                             
         LA    RE,L'SUBSTAB(RE)                                                 
         J     VA80                                                             
VA90     MVC   TGUSTYP,4(RE)     AND SET USE TYPE                               
                                                                                
         USING TLUHD,R3                                                         
VA100    XC    KEY,KEY           READ ALL CAST LEVEL USAGE HISTORY              
         MVI   TLUHCD,TLUHCDQ    FOR THIS INVOICE                               
         MVC   TLUHCOM,TGCOM                                                    
         MVC   TLUHCSEQ,=H'1'                                                   
         MVC   TLUHUSE,TGUSCDE                                                  
         GOTO1 HIGH                                                             
         J     VA120                                                            
VA110    GOTO1 SEQ                                                              
VA120    CLC   KEY(TLUHCSEQ-TLUHD),KEYSAVE                                      
         JNE   VA150                                                            
         CLC   TLUHINV,TGINV                                                    
         JNE   VA110                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TAUHD,R4                                                         
         L     R4,AIO            GET USAGE HISTORY ELEMENT                      
         MVI   ELCODE,TAUHELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   VA110                                                            
                                                                                
         CLI   TGUSEQU,ULCB      IF ANYONE ON INVOICE DOES NOT                  
         JNE   VA130             MATCH THIS HIGHEST COUNT                       
         CLC   TGUSTYP,TAUHTYPE  SET DIFFERING UH COUNTS STATUS                 
         JE    VA110                                                            
         OI    PAYSTAT1,DIFFERUH                                                
         J     VA150                                                            
                                                                                
VA130    TM    TGUSSTA3,CBLUSE                                                  
         JZ    VA140                                                            
         CLC   TCTUSES,TAUHUSN                                                  
         JE    VA110                                                            
         OI    PAYSTAT1,DIFFERUH                                                
         J     XIT                                                              
                                                                                
VA140    CLC   TAUHUNT,TCUNITS                                                  
         JE    VA110                                                            
         OI    PAYSTAT1,DIFFERUH                                                
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
VA150    CLI   TGUSEQU,ULCB      IF USE IS LOCAL CABLE                          
         JNE   XIT                                                              
         OC    TGFULL,TGFULL                                                    
         JZ    XIT                                                              
         MVC   TGUPTOTY,TGUSTYP                                                 
                                                                                
         LA    RE,SUBSTAB        FIND INITIAL NUMBER OF SUBSCRIBERS IN          
VA160    CLI   0(RE),X'FF'       TABLE                                          
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLC   TGFULL,0(RE)                                                     
         JNH   VA170                                                            
         LA    RE,L'SUBSTAB(RE)                                                 
         J     VA160                                                            
VA170    MVC   TGUPFRTY,4(RE)                                                   
                                                                                
         GOTO1 UPGRVAL,DMCB,(X'80',TGUSCDE),TGUPFRTY,TGUSCDE,TGUPTOTY           
         MVC   TGUSTYP,TGUPTYP                                                  
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DOES SOME SPECIAL UH PROCESSING FOR VERSION PAYMENTS *         
***********************************************************************         
                                                                                
VERUH    NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,0           IF COMMERCIAL IS A VERSION                   
         JE    XIT                                                              
         TM    TGUSSTA3,USEMTAB    AND USE IS CABLE OR WILDSPOT ...             
         JZ    XIT                                                              
                                                                                
         USING TAUHD,R4                                                         
         LA    R4,TCTAUHEL         INITIALIZE USAGE HISTORY ELEMENT             
                                                                                
         CLI   TGUSEQU,ULCB                                                     
         JNE   VU10                                                             
         MVI   TAUHCSYS,0                                                       
         MVI   TAUHFRTY,0                                                       
         XC    TAUHSUBS,TAUHSUBS                                                
         XC    TAUHISUB,TAUHISUB                                                
         J     VU30                                                             
                                                                                
VU10     TM    TGUSSTA3,CBLUSE                                                  
         JZ    VU20                                                             
         XC    TAUHUSN,TAUHUSN                                                  
         J     VU30                                                             
                                                                                
VU20     XC    TAUHUNT,TAUHUNT                                                  
         DROP  R4                                                               
                                                                                
         USING TLUHD,R3                                                         
VU30     LA    R3,KEY              READ ALL CAST LEVEL USAGE HISTORY            
         XC    KEY,KEY             FOR THIS COMMERCIAL/USE                      
         MVI   TLUHCD,TLUHCDQ                                                   
         MVC   TLUHCOM,TGCOM                                                    
         MVC   TLUHCSEQ,=H'1'                                                   
         MVC   TLUHUSE,TGUSCDE                                                  
         GOTO1 HIGH                                                             
         J     VU50                                                             
VU40     GOTO1 SEQ                                                              
VU50     CLC   KEY(TLUHCSEQ-TLUHD),KEYSAVE                                      
         JNE   VU80                                                             
         CLC   TLUHUSE,TGUSCDE                                                  
         JNE   VU40                                                             
         GOTO1 GETREC                                                           
                                                                                
         USING TAUHD,R4                                                         
         L     R4,AIO              GET USAGE HISTORY ELEMENT                    
         MVI   ELCODE,TAUHELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   VU40                                                             
         CLC   TAUHSTRT,TCPCYCS    ONLY CONSIDER THOSE WITH SAME                
         JNE   VU40                CYCLE START DATE                             
                                                                                
         CLI   TGUSEQU,ULCB        SAVE HIGHEST SUBSCRIBER/USE/UNIT             
         JNE   VU60                COUNT ENCOUNTERED                            
         CLC   TAUHCSYS,TAUHCSYS+TAUHSUBS-TAUHD                                 
         JNH   *+10                                                             
         MVC   TCTAUHEL+TAUHCSYS-TAUHD(L'TAUHCSYS),TAUHCSYS                     
         CLC   TAUHSUBS,TCTAUHEL+TAUHSUBS-TAUHD                                 
         JNH   *+10                                                             
         MVC   TCTAUHEL+TAUHSUBS-TAUHD(L'TAUHSUBS),TAUHSUBS                     
         CLC   TLUHINV,TGINV                                                    
         JE    VU40                                                             
         CLC   TAUHSUBS,TCTAUHEL+TAUHISUB-TAUHD                                 
         JNH   VU40                                                             
         MVC   TCTAUHEL+TAUHISUB-TAUHD(L'TAUHISUB),TAUHSUBS                     
         J     VU40                                                             
         DROP  R3                                                               
                                                                                
VU60     TM    TGUSSTA3,CBLUSE                                                  
         JZ    VU70                                                             
         CLC   TAUHUSN,TCTAUHEL+TAUHUSN-TAUHD  ENCOUNTERED                      
         JNH   VU40                                                             
         MVC   TCTAUHEL+TAUHUSN-TAUHD(L'TAUHUSN),TAUHUSN                        
         J     VU40                                                             
                                                                                
VU70     OC    TCTAUHEL+TAUHUNT-TAUHD(L'TAUHUNT),TCTAUHEL+TAUHUNT-TAUHD         
         JZ    *+14                                                             
         CLC   TAUHUNT,TCTAUHEL+TAUHUNT-TAUHD                                   
         JNH   VU40                                                             
         MVC   TCTAUHEL+TAUHUNT-TAUHD(L'TAUHUNT),TAUHUNT                        
         OC    TCTAUHEL+TAUHMAJ-TAUHD(L'TAUHMAJ),TAUHMAJ                        
         J     VU40                                                             
         DROP  R4                                                               
                                                                                
VU80     CLI   TGUSEQU,ULCB      IF MAKING LCB PAYMENT                          
         JNE   XIT                                                              
                                                                                
         USING TAUHD,R4                                                         
         LA    R4,TCTAUHEL                                                      
                                                                                
         LA    RE,SUBSTAB        FIND NEW NUMBER OF SUBSCRIBERS IN              
VU90     CLI   0(RE),X'FF'       TABLE                                          
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLC   TAUHSUBS,0(RE)                                                   
         JNH   VU100                                                            
         LA    RE,L'SUBSTAB(RE)                                                 
         J     VU90                                                             
VU100    MVC   TAUHTYPE,4(RE)                                                   
                                                                                
         OC    TAUHISUB,TAUHISUB                                                
         JZ    XIT                                                              
         MVC   TGUPTOTY,4(RE)                                                   
                                                                                
         LA    RE,SUBSTAB        FIND INITIAL NUMBER OF SUBSCRIBERS IN          
VU110    CLI   0(RE),X'FF'       TABLE                                          
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLC   TAUHISUB,0(RE)                                                   
         JNH   VU120                                                            
         LA    RE,L'SUBSTAB(RE)                                                 
         J     VU110                                                            
VU120    MVC   TGUPFRTY,4(RE)                                                   
                                                                                
         GOTO1 UPGRVAL,DMCB,(X'80',TGUSCDE),TGUPFRTY,TGUSCDE,TGUPTOTY           
         MVC   TAUHTYPE,TGUPTYP                                                 
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SUBSCRIBER COUNT/USE EQUATE TABLE                            *         
***********************************************************************         
                                                                                
         DC    0F                                                               
SUBSTAB  DC    0XL8                                                             
         DC    F'50000',AL1(ULCB50)                                             
         DC    F'100000',AL1(ULCB100)                                           
         DC    F'150000',AL1(ULCB150)                                           
         DC    F'200000',AL1(ULCB200)                                           
         DC    F'250000',AL1(ULCB250)                                           
         DC    F'500000',AL1(ULCB500)                                           
         DC    F'750000',AL1(ULCB750)                                           
         DC    F'1000000',AL1(ULCB1M)                                           
         DC    F'99999999',AL1(ULCBMAX)                                         
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATE GRT/PAY CAST DETAILS INPUT                  *         
*        ON ENTRY .... R2=A(FIRST LINE)                               *         
***********************************************************************         
                                                                                
VALGRT   NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UGRT        IF MAKING GRT PAYMENT                        
         JNE   XIT                                                              
         TM    PAYSTAT1,BNP        AND NOT USING BNP OPTION                     
         JO    XIT                                                              
                                                                                
         MVC   GPAGE,DPAGE         INITIALLY SET TO READ FIRST PAGE             
         LHI   RF,X'80'                                                         
         J     VGRT20                                                           
VGRT10   XR    RF,RF               THEN BUMP TO EACH FOLLOWING PAGE             
                                                                                
VGRT20   GOTO1 GETSCRN,DMCB,((RF),GPAGE),(RA) RESTORE SCREEN REC TO TIA         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZIC   R0,NCASTPG          R0=N'CAST ON THIS PAGE                       
                                                                                
VGRT30   LH    R4,DSPPAY                                                        
         AR    R4,R2               R4=A(PAYMENT AMOUNT FIELD)                   
                                                                                
         CLI   5(R4),0             IF PAYMENT AMOUNT IS PRESENT                 
         JE    VGRT40              OK TO MAKE THIS GRT PAYMENT                  
         ZIC   RF,5(R4)                                                         
         GOTO1 CASHVAL,DMCB,8(R4),(RF)                                          
         OC    4(4,R1),4(R1)                                                    
         JNZ   XIT                                                              
                                                                                
VGRT40   AH    R2,DSPNEXT          ELSE, BUMP TO NEXT LINE                      
         BCT   R0,VGRT30                                                        
                                                                                
         CLC   GPAGE,LPAGE         IF WE HAVEN'T REACHED LAST PAGE              
         JL    VGRT10              GO GET THE NEXT ONE                          
                                                                                
         J     NONEPAID            ELSE, RETURN ERROR                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE FORMATS WEB INITIATED TIMESHEET INTO AIO3            *         
***********************************************************************         
                                                                                
GETWBTS  NTR1  BASE=*,LABEL=*                                                   
         TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         JZ    XIT                                                              
         TM    TGUSSTAT,USETIME    AND MAKING A TIMESHEET PAYMENT               
         JZ    XIT                                                              
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN(2),TGCSORT+4                                            
         MVI   FAWSACTN,FAWSARST   RECALL TIMESHEET RECORD FOR THIS             
         MVC   FAWSADR,AIO3        CAST SEQUENCE NUMBER INTO AIO3               
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
         L     R4,AIO3                                                          
         MVI   ELCODE,TASDELQ      GET SESSION DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         ST    R4,TCATASD                                                       
                                                                                
         USING TASDD,R3                                                         
         LA    R3,ELTASD           COPY SESSION DETAILS ELEMENT                 
         MVC   TASDEL(TASDLNQ),0(R4)         TO WORKING STORAGE                 
         MVC   TASDEQU,TGUSEQU                                                  
         DROP  R3                                                               
                                                                                
         USING TATTD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TATTELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         MVI   HASTIME,C'Y'         SET TIMESHEET STATUS                        
                                                                                
GWT10    CLC   TATTDATE,=X'FFFFF0'                                              
         JL    GWT20                                                            
         ST    R4,TCATMTOT          STORE ADDRESS OF 1ST TOTAL ELEMENT          
         J     XIT                                                              
                                                                                
GWT20    BRAS  RE,NEXTEL                                                        
         JE    GWT10                                                            
         DC    H'00'                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE BUILD WEB INITIATED SESSION DETAILS ELEMENT          *         
***********************************************************************         
                                                                                
BLDWBSD  NTR1  BASE=*,LABEL=*                                                   
         TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         JZ    XIT                                                              
         CLI   TGUSEQU,UBSR        AND MAKING A BSR PAYMENT                     
         JE    BWSD10                                                           
         CLI   TGUSEQU,URRR        OR RRR PAYMENT                               
         JE    BWSD10                                                           
         CLI   TGUSEQU,UADO        OR ADO PAYMENT                               
         JNE   XIT                                                              
                                                                                
         USING FAWSSVRD,R1                                                      
BWSD10   LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN(2),TGCSORT+4                                            
         MVI   FAWSACTN,FAWSARST   RECALL SESSION DETAILS ELEMENT FOR           
         MVC   FAWSADR,AIO3        CAST SEQUENCE NUMBER INTO AIO3               
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
         L     R4,AIO3                                                          
         ST    R4,TCATASD                                                       
                                                                                
         USING TASDD,R3                                                         
         LA    R3,ELTASD           COPY SESSION DETAILS ELEMENT                 
         MVC   TASDEL(TASDLNQ),0(R4)         TO WORKING STORAGE                 
         MVC   TASDEQU,TGUSEQU                                                  
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS CERNO-INITIATED ADDITIONAL AMOUNT TO A          *         
*        PERFORMER'S PAYMENT                                          *         
***********************************************************************         
                                                                                
SETADAM  NTR1  BASE=*,LABEL=*                                                   
         TM    TGFASTAT,TGFROMFA   EXIT IF PAYMENT IS NOT COMING                
         JZ    XIT                 FROM WEB                                     
                                                                                
         USING TLCAD,R3                                                         
         LA    R3,SVCASTKY         R3=A(CAST KEY)                               
                                                                                
***********************************************************************         
                                                                                
         TM    TGFASTAT,TGCRNOPY   IF PAYMENT COMING FROM CERNO                 
         JZ    SADAM30                                                          
                                                                                
         GOTOR GTHFCDET,DMCB,TLCASEQ                                            
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING WBPYCSD,RF                                                       
         L     RF,TGFULL                                                        
         OC    WPCSADAX,WPCSADAX   IF THERE IS AN ADDITONAL AMOUNT              
         JZ    XIT                 SAVE IT                                      
         LA    R1,TCCAADPH                                                      
         TM    WPCSSTAT,WPCSADSP                                                
         JO    *+8                                                              
         LA    R1,TCCAADNS                                                      
         MVC   0(L'TCCAADPH,R1),WPCSADAX                                        
         J     XIT                                                              
         DROP  RF                                                               
                                                                                
***********************************************************************         
                                                                                
         USING WEBRESD,RF                                                       
SADAM30  L     RF,TGAFARES                                                      
SADAM40  CLC   TLCASEQ,WRSSEQ      SEARCH WEB RESPONSE DETAILS AREA             
         JE    SADAM50             FOR CAST SEQUENCE NUMBER                     
         CLI   0(RF),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'00'                                                            
         LA    RF,WRSLNQ(RF)                                                    
         J     SADAM40                                                          
         DROP  R3                                                               
                                                                                
SADAM50  MVC   TCCAADPH,WRSADJPH   SAVE ADJUSTMENT AMOUNTS                      
         MVC   TCCAADNS,WRSADJNS                                                
         J     XIT                                                              
         DROP  RF                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SAVES CAST LEVEL DETAILS OF WEB INITIATED PAYMENT    *         
***********************************************************************         
                                                                                
SVWEBDET NTR1  BASE=*,LABEL=*                                                   
         TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         JZ    XIT                                                              
         TM    TGFASTAT,TGCRNOPY   (BUT NOT CERNO)                              
         JO    XIT                                                              
                                                                                
         USING WEBRESD,R1                                                       
         L     R1,AWEBRES          R1=A(CAST ENTRY IN WEB RES DET AREA)         
         MVC   WRSAPCOD,TCAPPLCD   APPLIED CREDIT CODE                          
         MVC   WRSAPAMT,TCAPPLCR   APPLIED CREDIT AMOUNT                        
         MVC   WRSRECOD,TCEXPICD   REIMBURSED EXPENSE CODE                      
         MVC   WRSREAMT,TCEXP      REIMBURSED EXPENSE AMOUNT                    
         L     RE,TCPAYI                                                        
         A     RE,TCPAYC                                                        
         STCM  RE,15,WRSAMAMT      PAYMENT AMOUNT                               
         MVC   WRSPHAMT,TCSUBPNH   SUBJECT TO P&H AMOUNT                        
         MVC   WRSMDAMT,TCMDED     AND MISCELLANEOUS DEDUCTION AMOUNT           
         DROP  R1                                                               
                                                                                
         OC    TCCSTBRK,TCCSTBRK   IF PAYMENT BREAKDOWN BLOCK IS                
         JZ    XIT                 POPULATED ...                                
                                                                                
         XR    R0,R0               INITIALIZE WSSVR AREA LENGTH                 
                                                                                
         L     R3,AIO3                                                          
         XC    0(250,R3),0(R3)                                                  
                                                                                
         TM    TGUSSTAT,USETIME    IF MAKING A TIMESHEET PAYMENT ...            
         JO    SWB10                                                            
         CLI   TGUSEQU,UBSR        OR BSR PAYMENT                               
         JE    SWB10                                                            
         CLI   TGUSEQU,URRR        OR RRR PAYMENT                               
         JE    SWB10                                                            
         CLI   TGUSEQU,UADO        OR ADO PAYMENT                               
         JNE   SWB30                                                            
                                                                                
         USING FAWSSVRD,R1                                                      
SWB10    LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN(2),TGCSORT+4                                            
         MVI   FAWSACTN,FAWSARST   RECALL CAST SEQUENCE NUMBER KEYED            
         ST    R3,FAWSADR          WSSVR AREA                                   
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
         USING TLRCD,R3                                                         
         TM    TGUSSTAT,USETIME    IF MAKING A TIMESHEET PAYMENT ...            
         JZ    SWB20                                                            
         ZICM  R0,TLRCLEN,2        BUMP PAST THE TIMESHEET                      
         AR    R3,R0                                                            
         DROP  R3                                                               
                                                                                
         USING TASDD,R3                                                         
SWB20    CLI   TGUSEQU,UBSR        IF MAKING A BSR PAYMENT                      
         JE    SWB25                                                            
         CLI   TGUSEQU,URRR        OR RRR PAYMENT                               
         JE    SWB25                                                            
         CLI   TGUSEQU,UADO        OR ADO PAYMENT                               
         JNE   SWB30                                                            
SWB25    ZIC   R0,TASDLEN          BUMP PAST THE SESSION DETAILS                
         AR    R3,R0               ELEMENT                                      
         DROP  R3                                                               
                                                                                
         USING WEBBRKD,R3                                                       
SWB30    XC    WBRKGUA,WBRKGUA     SAVE GUARANTEE CODE                          
         MVC   WBRKOV1,TCOV1       OVERSCALE RATE                               
         MVC   WBRKOV2,TCOV2       SECOND OVERSCALE RATE                        
         MVC   WBRKBRK,TCCSTBRK    AND PAYMENT BREAKDOWN BLOCK                  
                                                                                
         USING PAYBRKDD,R4                                                      
         LA    R4,WBRKBRK                                                       
         LA    R4,PBBRKDAT         R4=A(PAYMENT BREAKDOWN DETAILS)              
                                                                                
         USING PBDATAD,R4                                                       
SWB40    OC    0(PBUNTLNQ,R4),0(R4)                                             
         JZ    SWB80                                                            
                                                                                
         LHI   RF,100              SET NUMBER OF UNITS AS 1.00                  
         CLI   PBCODE,PBCOVP       IF CODE IS OVERSCALE PERCENTAGE              
         JE    SWB70                                                            
         CLI   PBCODE,PBCPAY       OR PAYMENT                                   
         JE    SWB70                                                            
         CLI   PBCODE,PBCPLY       OR PENALTY                                   
         JE    SWB70                                                            
         CLI   PBCODE,PVCNPR       OR NIGHT PREMIUM                             
         JE    SWB70                                                            
         CLI   PBCODE,PVCMEA       OR MEAL PENALTY                              
         JE    SWB70                                                            
         CLI   PBCODE,PVCSMK       OR SMOKE PAY                                 
         JE    SWB70                                                            
         CLI   PBCODE,PVCADJ       OR ADJUSTMENT                                
         JE    SWB70                                                            
                                                                                
         ZICM  RF,PBUNITUN,4                                                    
         CLI   PBCODE,PVCTRV       IF CODE IS TRAVEL TIME                       
         JE    SWB70               ALREADY HAVE FOUR DECIMAL PLACES             
         CLI   PBCODE,PVCPDW       OR PRIOR DAY WARDROBE                        
         JE    SWB70               ALREADY HAVE TWO DECIMAL PLACES              
         MHI   RF,100              OTHERWISE ADD TWO DECIMAL PLACES NOW         
                                                                                
         CLI   PBCODE,PBCDAY       IF CODE IS DAYS ...                          
         JNE   SWB70                                                            
         TM    TCCAD150,X'01'      ... AND AND OFF NUMBER OF DAYS               
         JZ    SWB50               HAVE BEEN PAID AT 1.50 X RATE                
         AHI   RF,50               ADD TO THE NUMBER OF UNITS                   
SWB50    CLI   TCCAD050,0          ... AND DAYS HAVE BEEN PAID AT               
         JE    SWB60               0.50 X RATE                                  
         ZIC   RE,TCCAD050         SUBTRACT FROM THE NUMBER OF UNITS            
         MHI   RE,50                                                            
         SR    RF,RE                                                            
SWB60    CLI   TCCAD075,0          ... AND 1 DAY HAS BEEN PAID AT               
         JE    SWB70               0.75 X RATE                                  
         ZIC   R1,TCCAD075         SUBTRACT FROM THE NUMBER OF UNITS            
         MHI   R1,100              THEN ADD BACK ON                             
         SR    RF,R1                                                            
         ZIC   R1,TCCAD075                                                      
         MHI   R1,75                                                            
         AR    RF,R1                                                            
                                                                                
SWB70    STCM  RF,15,PBUNITUN      SAVE THE ADJUSTED NUMBER OF UNITS            
                                                                                
         LA    R4,PBUNTLNQ(R4)     BUMP TO NEXT BREADOWN ENTRY                  
         J     SWB40                                                            
                                                                                
SWB80    OC    TCOVAMT,TCOVAMT     IF PAYMENT WAS MADE WITH AN                  
         JZ    SWB90               OVERSCALE AMOUNT                             
         MVI   PBCODE,PBCOVA                                                    
         MVC   PBUNITUN,=F'100'                                                 
         MVC   PBUNITRT,TCOVAMT    SAVE OVERSCALE AMOUNT                        
         DROP  R4                                                               
                                                                                
         USING FAWSSVRD,R1                                                      
SWB90    LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN(2),TGCSORT+4                                            
         MVI   FAWSACTN,FAWSASVE                                                
         AHI   R0,WBRKEND-WEBBRKD                                               
         STCM  R0,3,FAWSLEN                                                     
         MVC   FAWSADR,AIO3        RESAVE CAST SEQUENCE NUMBER KEYED            
         GOTO1 WSSVR,(R1)          WSSVR AREA                                   
         CLI   FAWSRTN,0                                                        
         JE    XIT                                                              
         DC    H'00'                                                            
         DROP  R1,R3                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ENSURES THAT PERFORMER PAYMENT MATCHES CERNO'S       *         
*        EXPECTATIONS                                                 *         
***********************************************************************         
                                                                                
CSTMTCH  NTR1  BASE=*,LABEL=*                                                   
         TM    TGFASTAT,TGCRNOPY   IF PAYMENT COMING FROM CERNO                 
         JZ    YES                                                              
                                                                                
         TM    TCRTRN,TCRTDLR      IF PAYMENT IS COVERED BY DEALER              
         JO    YES                 WE'LL CATCH IT LATER                         
                                                                                
         GOTOR GTHFCDET,DMCB,TGCSORT+4                                          
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING WBPYCSD,RF                                                       
         L     RF,TGFULL                                                        
                                                                                
         ZICM  RE,TCGROSS,4                                                     
         TM    TCINPUT,TCINPAY+TCINOVSC+TCINPRI                                 
         JNO   CMTCH30                                                          
         ZICM  RE,TCPAYI,4                                                      
         A     RE,TCPAYC                                                        
                                                                                
CMTCH30  S     RE,WPCSPAYX         CAST'S EXPECTED PAYMENT AMOUNT               
         S     RE,WPCSADAX         MUST MATCH THE ACTUAL PAYMENT AMOUNT         
         LTR   RE,RE                                                            
         JZ    YES                                                              
         GOTO1 ADDERROR,DMCB,ERCNMTCH                                           
         J     NO                                                               
         DROP  RF                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
ERCNMTCH DC    AL1(ECNMTCHX-*),AL2(ERNCNMAT),AL1(ERRCATY3),AL1(D#PYCST)         
         DC    AL2(0)                                                           
         DC    C'Cast payment amount does not match notice'                     
ECNMTCHX EQU   *                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ENSURES THAT TOTAL PAYMENT MATCHES WEB APPLICATION'S *         
*        EXPECTATIONS                                                 *         
***********************************************************************         
                                                                                
TOTMTCH  NTR1  BASE=*,LABEL=*                                                   
         TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB ...               
         JZ    YES                                                              
                                                                                
         USING WEBRESD,R4                                                       
         TM    TGFASTAT,TGVITSES+TGVITCMP                                       
         JZ    TMTCH30             IF PAYMENT COMING FROM VITA                  
         L     R4,TGAFARES                                                      
TMTCH10  CLI   0(R4),X'FF'         ENSURE CAST RECORDS WERE FOUND               
         JE    TMTCH30             FOR ALL PERFORMERS                           
         TM    WRSSTAT,WRSSEXST                                                 
         JO    TMTCH20                                                          
         GOTOR ADDEAP,DMCB,0,ERCSTMIS,0(R4)                                     
         OI    WRSSTAT,WRSSNERE                                                 
TMTCH20  LA    R4,WRSLNQ(R4)                                                    
         J     TMTCH10                                                          
                                                                                
TMTCH30  XR    R2,R2               R2=# OF PERFROMERS WITH OVERLAP              
         XR    R3,R3               R3=# OF PERFORMERS WITHOUT OVERLAP           
                                                                                
         USING WEBRESD,R4                                                       
         L     R4,TGAFARES         ENSURE ALL PERFORMERS THAT WEB               
TMTCH40  CLI   0(R4),X'FF'         EXPECTS TO PROCESS HAVE BEEN                 
         JE    TMTCH80             PROCESSED                                    
         TM    WRSSTAT,WRSSELIG+WRSSNERE                                        
         JNZ   TMTCH50                                                          
         GOTOR ADDEAP,DMCB,ERRINCST,ERRINCS2,0(R4)                              
         TM    TGFASTAT,TGCRNOPY   IF NOT, RETURN INELIGIBLE CAST               
         JO    NO                  ERROR                                        
                                                                                
TMTCH50  TM    WRSSTAT,WRSSOVLP    IF PERFORMER HAS ALREADY BEEN                
         JZ    TMTCH60             PAID FOR THIS CYCLE                          
         AHI   R2,1                ADD ONE TO PERFORMERS WITH OVERLAP           
         J     TMTCH70                                                          
TMTCH60  AHI   R3,1                ELSE ADD ONE TO PERFORMERS WITHOUT           
                                                                                
TMTCH70  LA    R4,WRSLNQ(R4)       BUMP TO NEXT PERFORMER                       
         J     TMTCH40                                                          
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
TMTCH80  TM    TGFASTAT,TGCRNOPY   IF PAYMENT COMING FROM CERNO                 
         JZ    TMTCHX                                                           
         LTR   R2,R2               IF ANY PERFORMERS HAVE AN OVERLAP            
         JZ    TMTCH100            RETURN THE APPROPRATE ERROR MESSAGE          
         OI    LCLSTAT6,NTDLRCVR                                                
         LTR   R3,R3                                                            
         JZ    TMTCH90                                                          
         OI    LCLSTAT5,CSTNPPRE                                                
TMTCH90  J     CSTAPD10                                                         
                                                                                
TMTCH100 TM    LCLSTAT,PERFPAID    IF SOMEONE IS ELIGIBLE FOR                   
         BZ    YES                 PAYMENT                                      
                                                                                
         TM    LCLSTAT5,CSTMSNOT   PAYMENT CANNOT BE MISSING CAST               
         JZ    TMTCH110            THAT SHOULD BE ON HOLDING FEE                
         GOTO1 ADDERROR,DMCB,ERRISPND                                           
         J     NO                                                               
                                                                                
TMTCH110 TM    LCLSTAT5,CSTPDPRE   PAYMENT CANNOT INCLUDE CAST THAT HAS         
         JO    CSTAPD10            ALREADY BEEN PAID AN OVERLAPPING HF          
                                                                                
         USING WEBREQD,R1                                                       
         L     R1,TGAFAREQ                                                      
         ZICM  RE,WBPYTOT,4        TOTAL EXPECTED PAYMENT AMOUNT                
         S     RE,TPAYI            MUST MATCH THE ACTUAL PAYMENT AMOUNT         
         S     RE,TPAYC                                                         
         LTR   RE,RE                                                            
         JZ    TMTCH120                                                         
         GOTO1 ADDERROR,DMCB,ERTNMTCH                                           
         J     NO                                                               
                                                                                
         USING TACOD,R4                                                         
TMTCH120 LA    R4,ELTACO                                                        
         TM    TACOSTA2,TACOCHHF   CHECK IF COMM'L HAS REISSUE PENDING          
         JZ    YES                                                              
         GOTO1 ADDERROR,DMCB,ERRISPND                                           
         J     NO                                                               
         DROP  R1,R4                                                            
                                                                                
***********************************************************************         
                                                                                
TMTCHX   L     R4,TGAERTAB                                                      
         CLI   0(R4),X'FF'         IF AN ERRORS HAVE BEEN ENCOUNTERED           
         JE    YES                 SET NEGATIVE CONDITION CODE                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        HANDLE "CAST ALREADY PAID FOR THIS CYCLE" ERRORS             *         
***********************************************************************         
                                                                                
CSTAPD10 TM    LCLSTAT5,CSTNPPRE                                                
         JO    CSTAPD30                                                         
         TM    LCLSTAT6,NTDLRCVR                                                
         JZ    CSTAPD20                                                         
         GOTO1 ADDERROR,DMCB,ERACSTPD                                           
         J     NO                                                               
CSTAPD20 GOTO1 ADDERROR,DMCB,ERTNMDLR                                           
         J     NO                                                               
                                                                                
CSTAPD30 GOTO1 ADDERROR,DMCB,ERSCSTPD                                           
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
ERCSTMIS DC    AL1(ECSTMISX-*),AL2(ERABCAST),AL1(ERRCATY1),AL1(D#PYCST)         
         DC    AL2(0)                                                           
         DC    C'Missing cast record'                                           
ECSTMISX EQU   *                                                                
                                                                                
ERRINCST DC    AL1(ERINCSTX-*),AL2(ERNONEEL),AL1(ERRCATY1),AL1(D#PYCST)         
         DC    AL2(0)                                                           
         DC    C'Request contains ineligible cast members'                      
ERINCSTX EQU   *                                                                
                                                                                
ERRINCS2 DC    AL1(ERINCS2X-*),AL2(ERNONEEL),AL1(ERRCATY3),AL1(D#PYCST)         
         DC    AL2(0)                                                           
         DC    C'Performer is not eligible for payment'                         
ERINCS2X EQU   *                                                                
                                                                                
ERACSTPD DC    AL1(EACSTPDX-*),AL2(ERAPDCST),AL1(ERRCATY3),AL1(D#PYCID)         
         DC    AL2(0)                                                           
         DC    C'All cast is already paid for this cycle'                       
EACSTPDX EQU   *                                                                
                                                                                
ERTNMDLR DC    AL1(ETNMDLRX-*),AL2(ERRHFCDL),AL1(ERRCATY3),AL1(D#PYCST)         
         DC    AL2(0)                                                           
         DC    C'Holding Fee Payment covered by Dealer cycle'                   
ETNMDLRX EQU   *                                                                
                                                                                
ERSCSTPD DC    AL1(ESCSTPDX-*),AL2(ERSPDCST),AL1(ERRCATY3),AL1(D#PYCID)         
         DC    AL2(0)                                                           
         DC    C'Some cast is already paid for this cycle'                      
ESCSTPDX EQU   *                                                                
                                                                                
ERTNMTCH DC    AL1(ETNMTCHX-*),AL2(ERNINMAT),AL1(ERRCATY3),AL1(D#PYCST)         
         DC    AL2(0)                                                           
         DC    C'Total payment amount does not match notice'                    
ETNMTCHX EQU   *                                                                
                                                                                
ERRISPND DC    AL1(ERISPNDX-*),AL2(ERREIPND),AL1(ERRCATY3),AL1(D#PYCID)         
         DC    AL2(0)                                                           
         DC    C'Commercial has a reissue pending'                              
ERISPNDX EQU   *                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS WEB TRANSACTION RECORD                          *         
***********************************************************************         
                                                                                
ADDWTR   NTR1  BASE=*,LABEL=*                                                   
         TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         JZ    XIT                                                              
                                                                                
         USING TLWTD,R3                                                         
         L     R3,AIO              INITIALIZE WEB TRANSACTION RECORD            
         XC    0(255,R3),0(R3)                                                  
         MVI   TLWTCD,TLWTCDQ                                                   
         MVC   TLWTDATE,TGTODAY1                                                
         TIME  DEC                                                              
         STCM  R0,12,TLWTTIME                                                   
         MVC   TLWTLEN,DATADISP                                                 
                                                                                
         USING TAWTD,R4                                                         
         LA    R4,ELEMENT          INITIALIZE WEB TRANSACTION ELEMENT           
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAWTEL,TAWTELQ                                                   
         MVC   TAWTSTAF,TGCTSTAF                                                
                                                                                
***********************************************************************         
                                                                                
         TM    TGFASTAT,TGCRNOPY   IF PAYMENT COMING FROM CERNO                 
         JZ    AWTR10                                                           
                                                                                
         MVI   TLWTWBAP,TLWTWACO   FINISH BUILDING WEB TRANSACTION              
         MVI   TLWTACTN,TLWTAHLD   RECORD'S KEY FIELDS                          
         GOTO1 HEXOUT,DMCB,TGCOM,TLWTWBID,L'TGCOM,0                             
         GOTO1 DATCON,DMCB,(1,TCPCYCS),(20,TLWTWBID+8)                          
                                                                                
         MVI   TAWTLEN,TAWT2LNQ                                                 
         GOTO1 TINVCON,DMCB,INVNO,TAWTHCIN,DATCON                               
         L     RE,TPAYI                                                         
         A     RE,TPAYC            FINISH BUILDING WEB TRANSACTION              
         S     RE,TINR             ELEMENT                                      
         EDIT  (RE),(10,TAWTHCDO),2,ZERO=NOBLANK                                
         OC    TAWTHCDO(7),=7X'F0'                                              
         OC    TAWTHCDO+8(2),=2X'F0'                                            
         J     AWTR20                                                           
                                                                                
***********************************************************************         
                                                                                
         USING WEBREQD,R2                                                       
AWTR10   L     R2,TGAFAREQ                                                      
                                                                                
         MVI   TLWTWBAP,TLWTWAVI   FINISH BUILDING WEB TRANSACTION              
         MVI   TLWTACTN,TLWTAPAY   RECORD'S KEY FIELDS                          
         MVC   TLWTWBID,WBWAPID                                                 
         GOTO1 TINVCON,DMCB,INVNO,TLWTUINV,DATCON                               
         OC    TLWTUNIQ,SPACES                                                  
                                                                                
         MVI   TAWTLEN,TAWT7LNQ                                                 
         MVC   TAWTCOAY,TGAGY                                                   
         MVC   TAWTCOID,ELTACO+TACOCID-TACOD                                    
         MVC   TAWTCOTI,COMNAME                                                 
         MVC   TAWTPYUS,WBPYUSE                                                 
         L     RE,TPAYI                                                         
         A     RE,TPAYC            FINISH BUILDING WEB TRANSACTION              
         S     RE,TINR             ELEMENT                                      
         EDIT  (RE),(10,TAWTPYDO),2,ZERO=NOBLANK                                
         OC    TAWTPYDO(7),=7X'F0'                                              
         OC    TAWTPYDO+8(2),=2X'F0'                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
                                                                                
AWTR20   GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         GOTO1 MYADDREC            ADD THE RECORD                               
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE DETERMINES IF ANY ERRORS HAVE BEEN ENCOUNTERED       *         
*        THAT SHOULD ABORT A WEB TRANSACTION IMMEDIATELY              *         
***********************************************************************         
                                                                                
WEBERRS  NTR1  BASE=*,LABEL=*                                                   
         TM    TGFASTAT,TGFROMFA   IF PAYMENT COMING FROM WEB                   
         JZ    NO                                                               
                                                                                
         USING ERRENTD,RE                                                       
         L     RE,TGAERTAB                                                      
         CLI   EECATY,ERRCATY3     AND A CATEGORY 3 WAS FOUND                   
         J     XIT                 TERMINATE THIS TRANSACTION                   
         DROP  RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PROTECTS SOME FIELDS FOR GRT AND GRR PAYMENTS        *         
***********************************************************************         
                                                                                
PROTFLDS NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UGRT        IF MAKING A GRT PAYMENT                      
         JE    PFLDS10                                                          
         CLI   TGUSEQU,UGRR        OR GRR PAYMENT                               
         JNE   XIT                                                              
PFLDS10  OI    DTLAC1H+1,X'20'     PROTECT ALL APPLIED CODE                     
         OI    DTLAA1H+1,X'20'     AND APPLIED AMOUNT FIELDS                    
         OI    DTLAC2H+1,X'20'                                                  
         OI    DTLAA2H+1,X'20'                                                  
         OI    DTLAC3H+1,X'20'                                                  
         OI    DTLAA3H+1,X'20'                                                  
         OI    DTLAC4H+1,X'20'                                                  
         OI    DTLAA4H+1,X'20'                                                  
         OI    DTLAC5H+1,X'20'                                                  
         OI    DTLAA5H+1,X'20'                                                  
         OI    DTLAC6H+1,X'20'                                                  
         OI    DTLAA6H+1,X'20'                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAADDEAR                                                       
***********************************************************************         
*        ROUTINE TO ACCUMULATE AGENT FEE FOR EACH AGENT AND           *         
*        CAST MEMBERS CONTRIBUTING TO IT                              *         
***********************************************************************         
                                                                                
         USING AGTTABD,R2                                                       
ADDUPAGT NTR1  BASE=*,LABEL=*                                                   
         LA    R0,MAXAGT           R0=MAX N'AGENTS                              
         LA    R2,AGTTAB           R2=ENTRY IN AGENT TOTAL TABLE                
ADDAG5   OC    AGTTAGT,AGTTAGT     IF END OF ENTRIES                            
         JNZ   ADDAG8                                                           
         MVC   AGTTAGT,TGAGT       ADD ONE FOR THIS AGENT                       
         MVC   AGTTAMT,TCAGTFEE    INIT AMOUNT                                  
         MVC   AGTTSSN,TGSSN       SAVE 1ST CONTRIBUTING CAST SSN               
         J     XIT                                                              
                                                                                
ADDAG8   CLC   AGTTAGT,TGAGT       TEST AGENT MATCHES                           
         JNE   ADDAG30                                                          
         L     RE,AGTTAMT          ACCUMULATE AGENT FEE                         
         A     RE,TCAGTFEE                                                      
         ST    RE,AGTTAMT                                                       
                                                                                
         LA    R1,AGTTSSN             SET R1=A(SSN INFO IN THIS ENTRY)          
         LA    RE,MAXAGSSN            RF=MAX NUMBER OF SSN PER AGENT            
ADDAG20  OC    0(L'AGTTSSN,R1),0(R1)  IF END OF SSN ENTRIES                     
         JNZ   ADDAG22                                                          
         MVC   0(L'AGTTSSN,R1),TGSSN  ADD ONE FOR THIS SSN                      
         J     XIT                                                              
ADDAG22  CLC   TGSSN,0(R1)         TEST ALREADY HAVE THIS CAST SSN              
         JE    XIT                                                              
         LA    R1,L'AGTTSSN(R1)                                                 
         BCT   RE,ADDAG20                                                       
         DC    H'0'                NO MORE ROOM FOR THIS SSN                    
                                                                                
ADDAG30  LA    R2,AGTTNXT          BUMP TO NEXT AGENT ENTRY                     
         BCT   R0,ADDAG5                                                        
         DC    H'0'                NO MORE ROOM FOR THIS AGENT                  
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADJUST USAGE HISTORY RECORD'S CYCLE END DATE         *         
*        WITH THE CANADIAN CYCLE END DATE                             *         
*        ON ENTRY ... AIO = A(USAGE HISTORY RECORD)                   *         
***********************************************************************         
                                                                                
ADJUHCYE NTR1  BASE=*,LABEL=*                                                   
         OC    ACTCYCE,ACTCYCE     IF CANADIAN CYCLE END DATE                   
         JZ    XIT                 DIFFERS FROM US INVOICE ...                  
                                                                                
         USING TAUHD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAUHELQ      GET USAGE HISTORY ELEMENT                    
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TAUHEND,ACTCYCE     AND USE CANADIAN CYCLE END DATE              
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PROCESS GUARANTEE CONTRACT RECORD AND TRACKING    *         
***********************************************************************         
                                                                                
PROCGCON NTR1  BASE=*,LABEL=*                                                   
         OC    GUARCONT,GUARCONT   IF PAYMENT'S SUBJECT TO P&H                  
         JZ    XIT                 IS APPLYING TO A GUARANTEE CONTRACT          
**NOOP   OC    TCSUBPNH,TCSUBPNH                                                
**NOOP   JZ    XIT                                                              
         TM    PAYMODE,DRAFT       AND THIS IS NOT A DRAFT PAYMENT              
         JO    XIT                                                              
         TM    PAYOPTS3,ODUMMY     OR DUMMY PAYMENT                             
         JO    XIT                                                              
         CLC   TGUNI,=C'SAG'       AND PERFORMER'S UNION IS SAG                 
         JE    PGC00                                                            
         CLC   TGUNI,=C'AFT'       OR AFTRA                                     
         JNE   XIT                                                              
                                                                                
         USING TLGCD,R3                                                         
PGC00    LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLGCCD,TLGCCDQ                                                   
         MVC   TLGCSSN,TGSSN                                                    
         MVC   TLGCGCNT,GUARCONT                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLGCKEY),KEYSAVE                                           
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R3                                                               
                                                                                
         L     R0,AIO                                                           
                                                                                
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         USING TAGCD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAGCELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
PGC10    BRAS  RE,NEXTEL                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TAGCPD,GCCYEAR                                                   
         JNE   PGC10                                                            
         ICM   R2,15,TAGCBAL                                                    
         TM    PAYSTAT1,CREDIT     IF THIS IS A CREDIT PAYMENT                  
         JZ    PGC20               INCREMENT CONTRACT YEAR'S BALANCE            
         A     R2,TCSUBPNH                                                      
         J     PGC30                                                            
PGC20    S     R2,TCSUBPNH         IF THIS IS A NON-CREDIT PAYMENT              
PGC30    STCM  R2,15,TAGCBAL       DECREMENT CONTRACT YEAR'S BALANCE            
         DROP  R4                                                               
                                                                                
         GOTO1 PUTREC              PUT GUARANTEE CONTRACT TO FILE               
                                                                                
PGC35    MVC   TGTHREE(2),=X'FFFF'                                              
                                                                                
         USING TLOTD,R3                                                         
         XC    KEY,KEY                                                          
         MVI   TLOTCD,TLOTCDQ      GCON TRACKING RECORD                         
         MVC   TLOTSSN,TGSSN                                                    
         MVC   TLOTGCNT,GUARCONT   GCONTRACT CODE                               
         MVC   TLOTSTRT,GCCYEARS   CONTRACT YEAR                                
         MVC   TLOTAGY,TGAGY                                                    
         MVC   TLOTINV,INVNO                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLOTSEQ-TLOTCD),KEYSAVE                                      
         JNE   PGC40                                                            
                                                                                
         ZICM  R1,TLOTSEQ,2                                                     
         SHI   R1,1                                                             
         STCM  R1,3,TGTHREE                                                     
         DROP  R3                                                               
                                                                                
         USING TLOTD,R4                                                         
PGC40    L     R4,AIO3             BUILD KEY IN RECORD                          
         XC    0(250,R4),0(R4)                                                  
         MVC   0(TLOTSEQ-TLOTD,R4),KEYSAVE                                      
         MVC   TLOTSEQ,TGTHREE                                                  
         MVC   TLOTAGY,TGAGY                                                    
         MVC   TLOTINV,INVNO                                                    
         DROP  R4                                                               
                                                                                
         USING TAGCD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAGCEL,TAGCELQ      GRT CONTRACT YEAR ELEM                       
         MVI   TAGCLEN,TAGCLNQ                                                  
         MVC   TAGCPD,GCCYEAR      CONTRACT YEAR                                
                                                                                
         L     RE,TCSUBPNH                                                      
         TM    PAYSTAT1,CREDIT                                                  
         JZ    PGC50                                                            
         LCR   RE,RE                                                            
PGC50    STCM  RE,15,TAGCAMT                                                    
                                                                                
         STCM  R2,15,TAGCBAL       NEW BALANCE                                  
         MVC   TAGCGRT,TGGUA       GUARANTEE CODE                               
         MVC   TAGCUNI,TGUNI       UNION                                        
         MVC   TAGCUSE,TGUSCDE                                                  
         MVC   TAGCCOM,TGCOM                                                    
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         GOTO1 ACTVIN,DMCB,0                                                    
                                                                                
         GOTO1 MYADDREC                                                         
                                                                                
         ST    R0,AIO                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TESTS IF ANY INPUT FOR CAST MEMBER                   *         
*        RETURNS CC EQUAL IF NO INPUT                                 *         
*        ON ENTRY ... R3 = A(FIRST FIELD)                             *         
***********************************************************************         
                                                                                
ANYINPUT NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UEVE        IF MAKING EVENT PAYMENT                      
         JE    NO                  ALWAYS RETURN NO                             
                                                                                
         CLI   DSPNP,X'FF'         IF HAVE NP FIELD                             
         JE    ANYIN10                                                          
         LH    R4,DSPNP                                                         
         AR    R4,R3               SET R4=A(NP FIELD HEADER)                    
         CLI   8(R4),C'Y'                                                       
         JE    YES                 RETURN CC EQUAL IF INPUT=Y                   
                                                                                
ANYIN10  TM    LCLSTAT3,SOAPRES    IF USE IS A SOAP RESIDUAL                    
         JZ    ANYIN11                                                          
         LH    R4,DSPMDED          SET A(LAST FLD) TO BE MISC DED               
         AR    R4,R3               BECAUSE ALWAYS DISPLAY AGENT                 
         J     ANYIN15                                                          
ANYIN11  CLI   TGUSEQU,USOP        TEST SOP USE                                 
         JE    ANYIN12                                                          
         CLI   TGUSEQU,UPRT        OR PRT                                       
         JE    ANYIN14                                                          
         CLI   TGUSEQU,UPRS        OR PRS                                       
         JE    ANYIN14                                                          
         LH    R4,DSPAGT           ELSE SET A(LAST FIELD) TO BE AGNT            
         AR    R4,R3                                                            
         J     ANYIN15                                                          
ANYIN12  LH    R4,DSPMDED          SOP - SET A(LAST FLD) TO BE MISC DED         
         AR    R4,R3               BECAUSE ALWAYS DISPLAY AGENT                 
         LH    R1,DSPREIMI         SET A(FIRST FIELD) TO BE INCL CODE           
         AR    R3,R1               BECAUSE DISPLAY DATA BEFORE IT               
         J     ANYIN15                                                          
                                                                                
ANYIN14  LH    R4,DSPMDED          SET A(LAST FLD) TO BE MISC DED CAUSE         
         AR    R4,R3               ALWAYS SHOW EXP DATE, AGTF% & AGENT          
                                                                                
ANYIN15  GOTO1 FLDVAL,DMCB,(R3),(X'01',(R4)),(X'40',0)  TEST FOR INPUT          
         J     XIT                 FLDVAL RETURNS CC                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        FIND CAST RECORD BY CAST SEQUENCE NUMBER                     *         
***********************************************************************         
                                                                                
FNDCSEQ  NTR1  BASE=*,LABEL=*                                                   
         TM    PAYSTAT2,TAPDPCIB   IF CRED INV HAS SAVED BILL RATES             
         JZ    NO                                                               
                                                                                
         USING TLCAD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCACD,TLCACDQ      READ ALL CAST RECORDS FOR THE                
         MVC   TLCACOM,TGCOM       COMMERCIAL, ATTEMPTING TO FIND               
         GOTO1 HIGH                THE CAST SEQUENCE NUMBER                     
         J     FCS20                                                            
FCS10    GOTO1 SEQ                                                              
FCS20    CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         JNE   NO                                                               
         CLC   TLCASEQ,TGCSORT+4                                                
         JNE   FCS10                                                            
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ADJUST CAST RECORD BACK TO THE STATE IT WAS IN WHEN          *         
*        INVOICE THAT WAS BEING CREDITED WAS PAID                     *         
***********************************************************************         
                                                                                
CRDCAST  NTR1  BASE=*,LABEL=*                                                   
         TM    PAYSTAT2,TAPDPCIB   IF CRED INV HAS SAVED BILL RATES             
         JZ    XIT                                                              
                                                                                
         MVC   BLOCK(L'KEY),KEY    SAVE CAST RECORD KEY                         
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR'                                              
                                                                                
         USING TLCKD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCKCD,TLCKCDQ      FIND CHECK KEY FOR THIS PERFORMER            
         MVC   TLCKAGY,TGAGY       ON THE INVOICE BEING CREDITED                
         MVC   TLCKINV,EINVNO                                                   
         GOTO1 HIGH                                                             
         J     CRDC20                                                           
CRDC10   GOTO1 SEQ                                                              
CRDC20   CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         JNE   CRDC30                                                           
         CLC   TLCKSORT+4(L'TLCASEQ),BLOCK+TLCASEQ-TLCAD                        
         JNE   CRDC10                                                           
                                                                                
         GOTO1 CATVAL,DMCB,TLCKCAT SET GLOBAL CATEGORY VARIABLES                
         DROP  R3                                                               
                                                                                
CRDC30   MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   KEY,BLOCK                                                        
         GOTO1 HIGH                                                             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ENSURES THAT MINOR ON P+ PAYMENT HAS JUST ONE        *         
*        WORK STATE DEFINE ON EVTIME RECORD                           *         
***********************************************************************         
                                                                                
CHKMINOR NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UEVE        IF MAKING AN EVENT PAYMENT                   
         JNE   YES                                                              
         TM    TCCASTST,TCCAMINR   TO A MINOR                                   
         JZ    YES                                                              
                                                                                
         XC    TGTHREE,TGTHREE                                                  
                                                                                
         USING TATDD,R4                                                         
         L     R4,TCAETREC                                                      
         MVI   ELCODE,TATDELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
CMIN10   BRAS  RE,NEXTEL                                                        
         JNE   YES                                                              
         OC    TGTHREE,TGTHREE                                                  
         JNZ   CMIN20                                                           
         GOTOR SETSTATE,DMCB,TGTHREE                                            
         J     CMIN10                                                           
CMIN20   CLC   TGTHREE,TATDUNIT                                                 
         JE    CMIN10                                                           
         GOTOR SETSTATE,DMCB,TGFULL                                             
         CLC   TGTHREE,TGFULL                                                   
         JE    CMIN10                                                           
         DROP  R4                                                               
                                                                                
CMINERR  GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   MYMSGNO,=Y(ERROSMIP)                                             
         MVI   BLOCK,7                                                          
         MVC   BLOCK+1(6),TGPID                                                 
         MVI   BLOCK+7,0                                                        
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS CAST DETAILS STATE FOR P+ PAYMENTS TO MINORS    *         
*        SO THAT CHECK TRUSTEE ROUTINES WILL JUST FALL THROUGH        *         
***********************************************************************         
                                                                                
SETMINOR NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UEVE        IF MAKING AN EVENT PAYMENT                   
         JNE   XIT                                                              
         TM    TCCASTST,TCCAMINR   TO A MINOR                                   
         JZ    XIT                                                              
                                                                                
         USING TATDD,R4                                                         
         L     R4,TCAETREC                                                      
         MVI   ELCODE,TATDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR SETSTATE,DMCB,TGTHREE                                            
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
         LA    R4,ELTACA                                                        
         MVC   TACAUNIT,TGTHREE                                                 
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS PROVIDED STATE VARIABLE BASED ON MTAX VALUE     *         
*        ON ENTRY ... R4=A(EVENT TIME TASK-PAY TYPE DETAILS ELEMENT)  *         
*                     P1=A(STATE TO SET)                              *         
***********************************************************************         
                                                                                
         USING TATDD,R4                                                         
SETSTATE NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
         MVC   0(3,R2),TATDUNIT                                                 
                                                                                
         CLC   =C'DET',TATDUNIT                                                 
         JNE   *+10                                                             
         MVC   0(3,R2),=C'MI '                                                  
                                                                                
         CLC   =C'NYC',TATDUNIT                                                 
         JNE   *+10                                                             
         MVC   0(3,R2),=C'NY '                                                  
                                                                                
         CLC   =C'CIN',TATDUNIT                                                 
         JNE   *+10                                                             
         MVC   0(3,R2),=C'OH '                                                  
                                                                                
         CLC   =C'CLV',TATDUNIT                                                 
         JNE   *+10                                                             
         MVC   0(3,R2),=C'OH '                                                  
                                                                                
         CLC   =C'PHL',TATDUNIT                                                 
         JNE   *+10                                                             
         MVC   0(3,R2),=C'PA '                                                  
         DROP  R4                                                               
                                                                                
         OC    0(3,R2),SPACES                                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS CAST DETAILS STATE FOR P+ PAYMENTS TO WASH      *         
*        OR HAWAII SO THAT BILLING WA/HI SURCHARGE ROUTINES WILL      *         
*        JUST FALL THROUGH                                                      
***********************************************************************         
                                                                                
SETWAHI  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UEVE        IF MAKING AN EVENT PAYMENT                   
         JNE   XIT                                                              
                                                                                
         USING TATDD,R4                                                         
         L     R4,TCAETREC                                                      
         MVI   ELCODE,TATDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TATDUNIT,=C'WA '    FOR WORK IN WASHINGTON                       
         JE    SWH10                                                            
         CLC   TATDUNIT,=C'HI '    OR HAWAII                                    
         JNE   XIT                                                              
                                                                                
         USING TACAD,R3                                                         
SWH10    LA    R3,ELTACA                                                        
         MVC   TACAUNIT,TATDUNIT   SET CAST DETAILS STATE                       
         J     XIT                                                              
         DROP  R3,R4                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO MAKE H&W ADJUSTMENTS TO AFM LCL CHECK RECORDS     *         
***********************************************************************         
                                                                                
ADJHNW   NTR1  BASE=*,LABEL=*                                                   
         TM    PAYOPTS3,OHNWADJ      TEST FOR H&W ADJUSTMENT INPUT              
         JZ    XIT                                                              
         NI    LCLSTAT,ALL-NOHNWADJ                                             
         LA    R2,LCLAFM             R2=A(ENTRY IN LOCAL ACCUMS)                
         LA    R0,LCLNAFM            R0=N'ACCUMS                                
         L     R3,OPTHNW             R3=AMOUNT OF H&W ADJUSTMENT LEFT           
                                                                                
         OC    0(L'LCLAFM,R2),0(R2)  IF NO LOCALS                               
         JNZ   ADJHNW7                                                          
         OI    LCLSTAT,NOHNWADJ      SET NO H&W ADJUSTMENT                      
         J     XIT                                                              
                                                                                
ADJHNW5  OC    0(L'LCLAFM,R2),0(R2)  TEST FOR MORE LOCALS                       
         JZ    XIT                                                              
ADJHNW7  LTR   R3,R3               TEST FOR H&W ADJUSTMENT AMOUNT               
         JZ    XIT                                                              
         L     R1,0(R2)            R1=LOCAL H&W CHECK AMOUNT                    
                                                                                
         TM    PAYSTAT1,CREDIT     IF CREDIT PAYMENT                            
         JZ    *+8                                                              
         LCR   R1,R1               COMPLEMENT CHECK AMOUNT                      
         LCR   R3,R3               AND ADJUSTMENT AMOUNT                        
                                                                                
         AR    R1,R3               ADD H&W ADJUSTMENT AMT TO CHECK AMT          
         XR    R3,R3                                                            
         LTR   R1,R1                                                            
         JNM   ADJHNW10                                                         
         LR    R3,R1               IF RESULT < 0, SAVE H&W ADJ AMT LEFT         
         XR    R1,R1               MAKE LOCAL H&W CHECK AMOUNT 0                
                                                                                
ADJHNW10 TM    PAYSTAT1,CREDIT     IF CREDIT PAYMENT                            
         JZ    *+8                                                              
         LCR   R1,R1               UN-COMPLEMENT AMOUNTS                        
         LCR   R3,R3                                                            
         ST    R1,0(R2)            SAVE RESULTING LOCAL H&W AMOUNT              
         LA    R2,L'LCLAFM(R2)     BUMP TO NEXT ACCUM                           
         J     ADJHNW5                                                          
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO ADD A WSSVR ENTRY TO THE VNR TABLE IN THE EVENT   *         
*        THAT A VERSION IS BEING COVERED BY RESULTING IN NO DOLLARS   *         
*        ON ENTRY ... ACSTVERS = A(VERSIONS TO COVER)                 *         
***********************************************************************         
                                                                                
VNRNODOL NTR1  BASE=*,LABEL=*                                                   
         L     R2,ACSTVERS                                                      
         CLI   0(R2),0                                                          
         JE    XIT                                                              
                                                                                
         USING VNRD,R3                                                          
         L     R3,AIO1                                                          
         MVI   0(R3),X'FF'                                                      
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,PARAS                                                         
         MVC   FAWSTOKN,=CL4'VNR'                                               
         MVI   FAWSACTN,FAWSARST   IF WE ALREADY HAVE SOMEONE IN                
         XC    FAWSLEN,FAWSLEN     THE LIST OF PERFORMERS THAT HAVE             
         ST    R3,FAWSADR          COVERED VERSIONS BUT NO PAYMENT              
         GOTO1 WSSVR,(R1)          AMOUNT ...                                   
         CLI   FAWSRTN,0                                                        
         JNE   VNRND20                                                          
         DROP  R1                                                               
                                                                                
VNRND10  CLI   0(R3),X'FF'         ... THEN BUMP TO THE FIRST EMPTY             
         JE    VNRND20             SLOT                                         
         LA    R3,VNRLNQ(R3)                                                    
         J     VNRND10                                                          
                                                                                
VNRND20  MVC   VNRCSEQ,TGCSORT+4   INSERT CAST SEQUENCE NUMBER                  
         MVC   VNRVER,0(R2)        AND COVERED VERSIONS INTO THE                
         MVI   VNRLNQ(R3),X'FF'    TABLE                                        
         DROP  R3                                                               
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN,=CL4'VNR'                                               
         MVI   FAWSACTN,FAWSASVE                                                
         MVC   FAWSLEN,=H'4000'                                                 
         MVC   FAWSADR,AIO1        AND RESAVE LIST                              
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         JE    XIT                                                              
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE UPDATES CAST RECORD AND ADDS USAGE HISTORY FOR ANY   *         
*        PERFORMERS THAT HAVE VERSIONS COVERED BY A VNR PAYMENT       *         
*        WHICH RESULTS IN NO PAYMENT AMOUNT                           *         
***********************************************************************         
                                                                                
COMPVNR  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UVNR        IF MAKING A VNR PAYMENT                      
         JNE   XIT                                                              
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,PARAS                                                         
         MVC   FAWSTOKN,=CL4'VNR'                                               
         MVI   FAWSACTN,FAWSARST   ... AND WE HAVE ANYONE WHO HAS               
         XC    FAWSLEN,FAWSLEN     COVERED VERSIONS BUT NO PAYMENT              
         MVC   FAWSADR,AIO1        AMOUNT ...                                   
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         JNE   XIT                                                              
         DROP  R1                                                               
                                                                                
         USING VNRD,R2                                                          
         L     R2,AIO1                                                          
CVNR10   CLI   0(R2),X'FF'                                                      
         JE    XIT                                                              
                                                                                
         L     RE,ACSTVERS         SET COVERED VERSIONS ...                     
         MVC   0(L'VNRVER,RE),VNRVER                                            
                                                                                
         USING TLCAD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,TGCOM                                                    
         GOTO1 HIGH                ... AND SET THE CAST KEY                     
         J     CVNR30                                                           
CVNR20   GOTO1 SEQ                                                              
CVNR30   CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         JNE   XIT                                                              
         CLC   TLCASEQ,VNRCSEQ                                                  
         JNE   CVNR20                                                           
         MVC   SVSVCKEY,KEY                                                     
         MVC   TGCSORT,TLCASORT                                                 
         MVC   TGSSN,TLCASSN                                                    
         MVC   TGCAT,TLCACAT                                                    
         DROP  R3                                                               
                                                                                
         L     R0,AIO                                                           
         BRAS  RE,CASTVEL          ADD VERSION ELEMENTS TO CAST RECORD          
         BRAS  RE,CASTUH           ADD CAST UH RECORD                           
         ST    R0,AIO                                                           
                                                                                
         LA    R2,VNRLNQ(R2)       BUMP TO NEXT PERFORMER                       
         J     CVNR10                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS UH REC FOR CAST IF PAYING A VERSION             *         
***********************************************************************         
                                                                                
CASTUH   NTR1  BASE=*,LABEL=*                                                   
         TM    PAYOPTS3,ORETRO     IF NOT MAKING RETRO PAYMENT                  
         JO    XIT                                                              
                                                                                
         USING TAUHD,R3                                                         
         CLI   TGUSEQU,UVRE        IF VRE/VNR WE DONT NEED TO PUT IN            
         JE    CASTUH06            VERSIONS FIELD                               
         CLI   TGUSEQU,UVNR                                                     
         JE    CASTUH06                                                         
                                                                                
CASTUH05 CLI   VERSION,0           ONLY IF PAYING A VERSION                     
         JE    XIT                                                              
CASTUH06 CLI   TGUSEQU,UMUS        SKIP IF MUS PAYMENT                          
         JE    XIT                                                              
                                                                                
         LA    R3,TCTAUHEL         UPDATE USAGE HISTORY ELEMENT                 
         TM    TGUSSTA3,NWKUSE                                                  
         JZ    CASTUH10                                                         
         MVC   TAUHTYPE,TGUSTYP    SET USE TYPE                                 
                                                                                
         LH    R1,TAUHUSN          PREVIOUS N'USES PD TO CAST                   
         AH    R1,TCTUSES          + N'USES PAID                                
         STH   R1,TAUHUSN          = NEW N'USES PAID TO CAST                    
                                                                                
         LH    R1,TAUHUSNL         PREVIOUS N'USES PD TO LIFT VERSIONS          
         AH    R1,TCTUSESL         + N'USES PAID                                
         STH   R1,TAUHUSNL         = NEW N'USES PAID TO LIFT VERSIONS           
                                                                                
         LH    R1,TAUHUSNP         PREVIOUS N'PAX USES EXCL FROM CLA            
         TM    PAYSTAT1,CREDIT                                                  
         JO    *+12                                                             
         AH    R1,TCTXUSES         + N'PAX USES EXCLUDED THIS TIME              
         J     *+8                                                              
         SH    R1,TCTXUSES                                                      
         CHI   R1,0                                                             
         JNL   *+6                                                              
         SR    R1,R1                                                            
         STH   R1,TAUHUSNP         = NEW N'PAX USES EXCL FROM CLA               
                                                                                
         LH    R1,TAUHLUSP         PREV N'PAX USES PD TO LFT VERSIONS           
         TM    PAYSTAT1,CREDIT                                                  
         JO    *+12                                                             
         AH    R1,TCTXUSEL        + N'PAX USES EXCL FROM LFT THIS TIME          
         J     *+8                                                              
         SH    R1,TCTXUSEL                                                      
         CHI   R1,0                                                             
         JNL   *+6                                                              
         SR    R1,R1                                                            
         STH   R1,TAUHLUSP        = NEW N'PAX USES PD TO LFT VERSIONS           
         J     CASTUH50                                                         
                                                                                
CASTUH10 TM    TGUSTYST,UPGRADE    IF UPGRADE USE TYPE                          
         JZ    CASTUH20                                                         
         MVC   TAUHTYPE,TGUPTOTY   SET NEW UPGRADED TYPE                        
                                                                                
         TM    TGUSSTA3,CBLUSE     SKIP AHEAD IF CABLE, SPANISH CABLE           
         JNZ   CASTUH50                                                         
         CLI   TGUSEQU,ULCB        OR LOCAL CABLE PAYMENT                       
         JE    CASTUH50                                                         
                                                                                
         MVC   TAUHIMAJ,TAUHMAJ    SET PREVIOUS MAJORS                          
         MVC   TAUHIUNT,TAUHUNT    SET PREVIOUS UNITS                           
                                                                                
         CLI   TGUSEQU,UWSP        SKIP AHEAD IF WILDSPOT                       
         JE    CASTUH40                                                         
         TM    TGUSSTA3,USEMTAB    OR SWS, WSC OR ADW                           
         JNZ   CASTUH50                                                         
                                                                                
         NI    TAUHSTAT,X'FF'-TAUHSFUP                                          
         TM    LCLSTAT3,FORCEUPG   IF FORCED UPGRADE                            
         JZ    *+8                                                              
         OI    TAUHSTAT,TAUHSFUP   SET BIT                                      
         J     CASTUH30                                                         
                                                                                
CASTUH20 XC    TCTAUHEL,TCTAUHEL   IF NOT UPGRADING, CLEAR UH                   
         MVC   TAUHTYPE,TGUSTYP    AND SAVE USE TYPE                            
                                                                                
CASTUH30 TM    TGUSSTA3,USEMTAB                                                 
         JNZ   CASTUH40                                                         
         MVC   TAUHUNT,TCUNITS     SET NEW UNITS                                
CASTUH40 MVC   TAUHMAJ,TCMAJORS    SET NEW MAJORS                               
                                                                                
CASTUH50 MVI   TAUHEL,TAUHELQ      ELEMENT CODE                                 
         MVI   TAUHLEN,TAUHLNQ     LENGTH                                       
         MVC   TAUHSTRT(6),TCPCYCS CYCLE START/END                              
         MVC   TAUHLFT,LIFT        LFT INPUT                                    
                                                                                
         CLI   TGUSEQU,UDLR        IF CREDITING A DEALER PAYMENT                
         JNE   CASTUH60                                                         
         TM    PAYSTAT1,CREDIT                                                  
         JZ    CASTUH60                                                         
         OI    TAUHDLST,TAUHDLCR   TURN ON CREDIT STATUS                        
                                                                                
CASTUH60 MVC   AIO,AIO2            ADD CAST UH REC FROM AIO2                    
                                                                                
         BRAS  RE,STLCBTYP         FOR LCB PAYMENTS, SET PREV USE INFO          
         GOTO1 ADJTBCST            ADJ CNET/MKT/CSYS TABLE FOR CAST MEM         
                                                                                
         BRAS  RE,TEST04A          IF 2404A PERFORMER BELONGS ON                
         JNE   CASTUH70            CANADIAN INVOICE                             
         MVC   TGINV,INV04A        USE CANADIAN INVOICE NUMBER                  
         BRAS  RE,BLDUH            TO BUILD USAGE HISTORY RECORD                
         MVC   TGINV,INVNO                                                      
         XC    TGINV,HEXFFS                                                     
         BRAS  RE,ADJUHCYE         ADJUST CYCLE END DATE FOR CAN INV            
         J     CASTUH80                                                         
                                                                                
CASTUH70 BRAS  RE,BLDUH            BUILD NEW UH REC IN AIO                      
                                                                                
CASTUH80 BRAS  RE,MKTBLDUH         BUILD MKT ELEMENTS FOR VERS PYMTS            
         BRAS  RE,RSLCBTYP         RESET LCB TYPES                              
                                                                                
         USING TLUHD,R3                                                         
         L     R3,AIO                                                           
         MVC   TLUHCSEQ,TGCSORT+4  CAST INPUT SEQ NUMBER                        
                                                                                
         L     R2,AIO3             (MYADDREC EXPECTS RECORD IN AIO3)            
         LHI   R3,4000                                                          
         L     RE,AIO2                                                          
         LR    RF,R3                                                            
         MVCL  R2,RE                                                            
         MVC   AIO,AIO3                                                         
                                                                                
         GOTO1 MYADDREC            ADD NEW USAGE HISTORY RECORD                 
                                                                                
         TM    PAYMODE,DRAFT                                                    
         JO    CASTUH90                                                         
         XC    BLOCK(200),BLOCK                                                 
         GOTO1 ADDPTRS,DMCB,(X'08',BLOCK),UHMTPTRS                              
                                                                                
CASTUH90 MVC   AIO,AIO1            RESTORE AIO                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ANALYZES CAST BEING PAID ON ACTRA 2404 PAYMENT       *         
***********************************************************************         
                                                                                
ANYACT04 NTR1  BASE=*,LABEL=*                                                   
         USING TACOD,RE                                                         
         LA    RE,ELTACO                                                        
         CLI   TACOCTYP,CCTY2404   IF ACTRA TYPE 2404 COMM'L                    
         JNE   XIT                                                              
         DROP  RE                                                               
                                                                                
         NI    LCLSTAT4,ALL-NONACTON-ACTON                                      
         ST    R2,PARAS                                                         
         MVC   GPAGE,DPAGE         INITIALLY SET TO READ FIRST PAGE             
         LHI   RF,X'80'            OF PAYMENT                                   
         J     AA0420                                                           
AA0410   XR    RF,RF               THEN BUMP TO EACH FOLLOWING PAGE             
         L     R2,PARAS                                                         
AA0420   GOTO1 GETSCRN,DMCB,((RF),GPAGE),(RA)                                   
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING PAGED,R4                                                         
         LA    R4,PAGETBL                                                       
         ZIC   R0,NCASTPG          R0=N'CAST ON THIS PAGE                       
                                                                                
AA0430   CLI   DSPNP,X'FF'                                                      
         JE    AA0440                                                           
         LH    RE,DSPNP                                                         
         AR    RE,R2                                                            
         CLI   8(RE),C'Y'                                                       
         JE    AA0480                                                           
                                                                                
AA0440   GOTOR AMTPRES,DMCB,DSPPAY                                              
         JE    AA0450                                                           
         GOTOR AMTPRES,DMCB,DSPREIM                                             
         JE    AA0450                                                           
         GOTOR AMTPRES,DMCB,DSPAPPL                                             
         JE    AA0450                                                           
         GOTOR AMTPRES,DMCB,DSPSPNH                                             
         JE    AA0450                                                           
         GOTOR AMTPRES,DMCB,DSPMDED                                             
         JNE   AA0480                                                           
                                                                                
AA0450   BRAS  RE,SETCAST                                                       
         GOTOR ONCAINV,DMCB,ELTACA                                              
         JNE   AA0460                                                           
         OI    LCLSTAT4,ACTON                                                   
         J     AA0470                                                           
AA0460   OI    LCLSTAT4,NONACTON                                                
                                                                                
AA0470   TM    LCLSTAT4,ACTON                                                   
         JZ    AA0480                                                           
         TM    LCLSTAT4,NONACTON                                                
         JO    XIT                                                              
                                                                                
AA0480   AH    R2,DSPNEXT          ELSE, BUMP TO NEXT LINE                      
         LA    R4,PAGENEXT                                                      
         BCT   R0,AA0430                                                        
                                                                                
         CLC   GPAGE,LPAGE         IF WE HAVEN'T REACHED LAST PAGE              
         JL    AA0410              GO GET THE NEXT ONE                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS IF ANY CONTRACT YEAR 16 PERFORMERS ARE BEING  *         
*        PAID                                                         *         
***********************************************************************         
*&&DO                                                                           
ANYCON16 NTR1  BASE=*,LABEL=*                                                   
         NI    LCLSTAT9,ALL-CN16INCL                                            
                                                                                
         ST    R2,PARAS                                                         
         MVC   GPAGE,DPAGE         INITIALLY SET TO READ FIRST PAGE             
         LHI   RF,X'80'            OF PAYMENT                                   
         J     AC1620                                                           
AC1610   XR    RF,RF               THEN BUMP TO EACH FOLLOWING PAGE             
         L     R2,PARAS                                                         
AC1620   GOTO1 GETSCRN,DMCB,((RF),GPAGE),(RA)                                   
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING PAGED,R4                                                         
         LA    R4,PAGETBL                                                       
         ZIC   R0,NCASTPG          R0=N'CAST ON THIS PAGE                       
                                                                                
AC1630   CLI   DSPNP,X'FF'                                                      
         JE    AC1640                                                           
         LH    RE,DSPNP                                                         
         AR    RE,R2                                                            
         CLI   8(RE),C'Y'                                                       
         JE    AC1660                                                           
                                                                                
AC1640   GOTOR AMTPRES,DMCB,DSPPAY                                              
         JE    AC1650                                                           
         GOTOR AMTPRES,DMCB,DSPREIM                                             
         JE    AC1650                                                           
         GOTOR AMTPRES,DMCB,DSPAPPL                                             
         JE    AC1650                                                           
         GOTOR AMTPRES,DMCB,DSPSPNH                                             
         JE    AC1650                                                           
         GOTOR AMTPRES,DMCB,DSPMDED                                             
         JNE   AC1660                                                           
                                                                                
         USING TACAD,RE                                                         
AC1650   BRAS  RE,SETCAST                                                       
         LA    RE,ELTACA                                                        
         CLC   TACAYEAR,=C'16 '                                                 
         JNE   AC1660                                                           
         CLC   TACAUN,=C'NON'                                                   
         JE    AC1660                                                           
         OI    LCLSTAT9,CN16INCL                                                
         J     XIT                                                              
         DROP  RE                                                               
                                                                                
AC1660   AH    R2,DSPNEXT          ELSE, BUMP TO NEXT LINE                      
         LA    R4,PAGENEXT                                                      
         BCT   R0,AC1630                                                        
                                                                                
         CLC   GPAGE,LPAGE         IF WE HAVEN'T REACHED LAST PAGE              
         JL    AC1610              GO GET THE NEXT ONE                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS IF PROVIDED FIELDS CONTAINS A VALUE GREATER   *         
*        THAN ZERO AND SETS CONDITION CODE                            *         
*        ON ENTRY ... P1 = A(FIELD TO CHECK)                          *         
*                     R2 = A(CURRENT LINE)                                      
***********************************************************************         
                                                                                
AMTPRES  NTR1  BASE=*,LABEL=*                                                   
         L     R4,0(R1)                                                         
         LH    R4,0(R4)                                                         
         AR    R4,R2                                                            
         ZIC   RF,5(R4)                                                         
         CLI   5(R4),0                                                          
         JNE   AP10                                                             
                                                                                
         TM    1(R4),X'20'                                                      
         JZ    NO                                                               
         ZIC   RF,0(R4)                                                         
         SHI   RF,8                                                             
         TM    1(R2),X'02'                                                      
         JZ    *+8                                                              
         SHI   RF,8                                                             
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   8(0,R4),SPACES                                                   
         JE    NO                                                               
         EX    RE,*+8                                                           
         J     *+10                                                             
         OC    8(0,R4),8(R4)                                                    
         JZ    NO                                                               
                                                                                
AP10     GOTO1 CASHVAL,DMCB,8(R4),(RF)                                          
         OC    4(4,R1),4(R1)                                                    
         JNZ   YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS CAST VARIABLES FOR ONCAINV ROUTINE              *         
***********************************************************************         
                                                                                
SETCAST  NTR1  BASE=*,LABEL=*                                                   
         USING TLCAD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         J     SCAST20                                                          
SCAST10  GOTO1 SEQ                                                              
SCAST20  CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TLCASORT,PAGESKEY                                                
         JNE   SCAST10                                                          
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   ELTACA,TACAEL                                                    
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO ADJUST CHECK BREAKDOWN TABLE IF CLASS A           *         
*        DISCOUNT WAS TAKEN                                           *         
***********************************************************************         
                                                                                
ADJDISC  NTR1  BASE=*,LABEL=*                                                   
         TM    TCSTAT2,TCSTCDIS    IF CLASS A DISCOUNT WAS TAKEN                
         JZ    XIT                                                              
                                                                                
         LA    R2,TCCSTBRK         R2=A(PAYMENT BREAKDOWN BLOCK)                
                                                                                
         USING PBDATAD,R2                                                       
         LA    R2,PBBRKDAT-PAYBRKDD(R2)                                         
ADISC10  OC    PBCODE(PBUNTLNQ),PBCODE                                          
         JZ    XIT                                                              
         XC    PBUNITST,PBUNITST   CLEAR OUT ALL START/END NUMBERS              
         XC    PBUNITEN,PBUNITEN                                                
         LA    R2,PBUNTLNQ(R2)                                                  
         J     ADISC10                                                          
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO COUNT NUMBER OF WEB ERRORS ENCOUNTERED            *         
*        ON EXIT ... R0 = # OF ERRORS ENCOUNTERED                     *         
***********************************************************************         
                                                                                
CNTWBERS NTR1  BASE=*,LABEL=*                                                   
         XR    R0,R0                                                            
                                                                                
         TM    TGFASTAT,TGFROMFA                                                
         JZ    CWEX                                                             
                                                                                
         USING ERRENTD,RE                                                       
         L     RE,TGAERTAB                                                      
CWE10    CLI   0(RE),X'FF'                                                      
         JE    CWEX                                                             
         AHI   R0,1                                                             
         ZIC   RF,EELEN                                                         
         AR    RE,RF                                                            
         J     CWE10                                                            
         DROP  RE                                                               
                                                                                
CWEX     XIT1  REGS=(R0)                                                        
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAONCAIN                                                       
       ++INCLUDE TAGTHFCD                                                       
       ++INCLUDE TAPYS50D                                                       
         EJECT                                                                  
       ++INCLUDE TAPYS78D                                                       
         EJECT                                                                  
       ++INCLUDE TAGENPAYD                                                      
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS11D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS12D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS1DD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS51D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS52D                                                       
         ORG   GRTOVLYH                                                         
       ++INCLUDE TASCR95D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS5BD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS55D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS5FD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS65D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS66D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS64D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS69D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS1AD                                                       
         EJECT                                                                  
* DDGENTWA    *** MUST FOLLOW LAST SCREEN ***                                   
* TAGENWORKD                                                                    
* TASYSEQUS                                                                     
* TAGENEQUS                                                                     
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* DDTSARD                                                                       
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
* TAMAPEQUS                                                                     
* TAWBDSECT                                                                     
* TAWADSECT                                                                     
* FAWSSVRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE TAMAPEQUS                                                      
       ++INCLUDE TAWBDSECT                                                      
       ++INCLUDE TAWADSECT                                                      
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE TASCRE7D                                                       
         PRINT ON                                                               
*              DSECT TO COVER TABLE FOR SAVED ERROR AGENCY/INVOICE              
         SPACE 1                                                                
SVAID    DSECT                                                                  
SVAGY    DS    CL(L'TGAGY)         AGENCY                                       
SVINV    DS    CL(L'TGINV)         INVOICE                                      
*              DSECT TO COVER VNR COVERED VERSIONS TABLE                        
         SPACE 1                                                                
VNRD     DSECT                                                                  
VNRCSEQ  DS    CL(L'TLCASEQ)       CAST SEQUENCE NUMBER                         
VNRVER   DS    XL4                 COVERED VERSIONS                             
VNRLNQ   EQU   *-VNRD                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'202TAGEN51   06/27/16'                                      
         END                                                                    
