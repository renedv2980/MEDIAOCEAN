*          DATA SET TAGENFA    AT LEVEL 004 AS OF 03/09/16                      
*PHASE T702FAA                                                                  
         TITLE 'T702FA - PAY VIA WEB REQUEST'                                   
                                                                                
T702FA   CSECT                                                                  
         LKSVR TYPE=UR,BLOCKS=(LIOBSB1Q,T702FFD,LIOBSB2Q,WEBREQD)               
         PRINT NOGEN                                                            
         NMOD1 WORKLNQ,T702FA,R6,CLEAR=YES                                      
         LR    R7,RC               R7=A(WEB REQUEST DETAILS)                    
         USING WEBREQD,R7                                                       
         LR    R8,R7                                                            
                                                                                
         USING WEBRESD,R8                                                       
         AHI   R8,WBREQLNQ         R8=A(WEB RESPONSE DETAILS)                   
         MVI   0(R8),X'FF'                                                      
                                                                                
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
                                                                                
         L     RE,AIO1             COPY LINKIO CONTROL BLOCK                    
         LA    R0,LIOBAREA         INTO WEB REQUEST DETAILS AREA                
         LHI   R1,LIOBX-LIOB                                                    
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING LIOB,R2                                                          
         LA    R2,LIOBAREA         R2=A(LOCAL LINKIO CONTROL BLOCK)             
         ST    RA,LIOBASB1         SET A(TWA)                                   
         ST    R7,LIOBASB2         SET A(WORKD)                                 
         LA    R1,LINKMAP                                                       
         ST    R1,LIOBAMAP         SET A(RECORD MAP)                            
         MVI   LIOBINDS,LIOBIRET+LIOBISUB+LIOBIGTR+LIOBINRM+LIOBIMRA            
                                                                                
***********************************************************************         
*        PROCESS UPLOAD RECORDS                                       *         
***********************************************************************         
                                                                                
NXTREC   GOTO1 LINKIO,DMCB,('LIOAGET',LIOB)                                     
         JL    NXTREC                                                           
         JH    XIT                                                              
                                                                                
         MVC   WBACTIN,LIOBMAP#    SAVE ACTION                                  
         XC    LVARS(LVLNQ),LVARS  CLEAR VARIABLES                              
                                                                                
         CLC   WBACTIN,=AL2(I#PAYULD)                                           
         JNE   NR10                                                             
         OC    TGAERTAB,TGAERTAB   IF A(ERROR TABLE) HAS NOT                    
         JNZ   NR20                ALREADY BEEN SAVED, DO SO NOW                
NR10     LA    RE,ERRTAB                                                        
         MVI   0(RE),X'FF'                                                      
         ST    RE,TGAERTAB                                                      
                                                                                
NR20     OC    TGAEATAB,TGAEATAB                                                
         JNZ   NR30                IF A (ERROR APPLIES TO TABLE) HAS            
         LA    RE,EATTAB           NOT ALREADY BEEN SAVED, DO SO NOW            
         MVI   0(RE),X'FF'                                                      
         ST    RE,TGAEATAB                                                      
                                                                                
NR30     BRAS  RE,UPLCAST          IF ONLY UPLOADING CAST                       
         JE    NXTREC              DO SO AND GO GET NEXT REQUEST                
                                                                                
         BRAS  RE,DWNCAST          IF ONLY DOWNLOADING CAST BREAKDOWN           
         JE    NXTREC              DO SO AND GO GET NEXT REQUEST                
                                                                                
         BRAS  RE,UPLUDET          IF ONLY UPLOADING USE DETAILS                
         JE    NXTREC              DO SO AND GO GET NEXT REQUEST                
                                                                                
         BRAS  RE,PROTIM           IF UPLOADING TIMESHEET                       
         JE    NXTREC              DO SO AND GO GET NEXT REQUEST                
                                                                                
         BRAS  RE,INIVARS          INITIALIZE VARIABLES                         
                                                                                
         BRAS  RE,ASRTREQ          ASSERT ALL REQUIRED FIELDS PROVIDED          
         JNE   NR50                                                             
         BRAS  RE,ASRTVAL          ASSERT ALL FIELDS HAVE VALID VALUES          
         JNE   NR50                                                             
                                                                                
         BRAS  RE,FMTREQ           FORMAT REQUEST FIELDS                        
         JNE   NR50                                                             
         BRAS  RE,VALREQ           VALIDATE REQUEST FIELDS                      
         JNE   NR50                                                             
                                                                                
         CLC   WBACTIN,=AL2(I#PAYULD)  IF ACTION IS PAY                         
         JNE   NR40                                                             
         BRAS  RE,INITPAY              INITIALIZE PAY PROGRAM                   
                                                                                
         GOTOR BLDSCR,DMCB,WBMDVFY BUILD PAY SCREEN                             
         BRAS  RE,RUNPAY           AND PASS CONTROL TO PAY PROGRAM              
         JNE   NR50                FOR VERIFICATION                             
                                                                                
         CLI   WBMODE,WBMDEXE      IF MODE IS EXECUTE                           
         JNE   NR40                                                             
         BRAS  RE,RSTWRD           RESET WEB RESPONSE DETAILS BLOCK             
         GOTOR BLDSCR,DMCB,WBMDEXE BUILD PAY SCREEN AGAIN                       
         BRAS  RE,RUNPAY           PASS CONTROL FOR EXECUTION                   
         CLI   ERRTAB,X'FF'        (ERRORS SHOULD BE CAUGHT IN                  
         JE    NR40                VERIFICATION MODE)                           
         DC    H'00'                                                            
                                                                                
NR40     BRAS  RE,OUTSDET          OUTPUT SUCCESSFUL DETAILS                    
         J     NR60                                                             
                                                                                
NR50     BRAS  RE,OUTEDET          OUTPUT ERROR DETAILS                         
                                                                                
NR60     CLI   ERRTAB+EECATY-ERRENTD,ERRCATY3                                   
         JE    NXTREC              IF NO CATEGORY 3 ERRORS ENCOUNTERED          
         BRAS  RE,OUTCMT           OUTPUT COMMENT RECORDS                       
         J     NXTREC                                                           
         DROP  R2                                                               
                                                                                
**********************************************************************          
*        RECORD MAP TABLE                                            *          
**********************************************************************          
                                                                                
LINKMAP  DS    0XL(LIORL)                                                       
         DC    AL2(I#PAYULD,I#PAYULD,PAYHDR-LINKMAP)                            
         DC    AL2(I#PJVULD,I#PAYULD,PJVHDR-LINKMAP)                            
         DC    AL2(I#TMULD,O#TMSTA,TIMHDR-LINKMAP)                              
LINKMAPX DC    AL2(0)                                                           
                                                                                
***********************************************************************         
*        EXITS, CONSTANTS, EQUATES AND LITERALS                       *         
***********************************************************************         
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
WBREQLNQ EQU   6000                WEB REQUEST DETAILS                          
WBRESLNQ EQU   (100*WRSLNQ)+1      WEB RESPONSE DETAILS                         
WORKLNQ  EQU   WBREQLNQ+WBRESLNQ   TOTAL NMOD STORAGE GRAB                      
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SAVE PAYABLE CAST INTO WEB RESPONSE DETAILS BLOCK *         
*        ON ENTRY ... R8=A(WEB RESPONSE DETAILS AREA)                 *         
***********************************************************************         
                                                                                
UPLCAST  NTR1  BASE=*,LABEL=*                                                   
         OC    WBCPSEQ,WBCPSEQ     IF UPLOADING PAYABLE CAST                    
         JZ    NO                                                               
                                                                                
         BRAS  RE,UPLHFCST         UPLOAD CAST FOR HOLDING FEE PAYMENT          
         JE    YES                                                              
                                                                                
         BRAS  RE,ASRTREQ          ASSERT ALL REQUIRED FIELDS PROVIDED          
         JNE   YES                                                              
         BRAS  RE,ASRTVAL          ASSERT ALL FIELDS HAVE VALID VALUES          
         JNE   YES                                                              
                                                                                
UC10     CLI   0(R8),X'FF'         FIND NEXT EMPTY SLOT IN                      
         JE    UC20                WEB RESPONSE DETAILS AREA                    
         LA    R8,WRSLNQ(R8)       AND SAVE THIS PERFOMER'S                     
         J     UC10                DETAILS INTO IT                              
UC20     MVC   WRSSEQ,WBCPSEQ                                                   
         MVI   WRSSTAT,0                                                        
         MVC   WRSCKCMT(WBCPMPN-WBCPCHK),WBCPCHK                                
                                                                                
         GOTO1 SETOVSTA,DMCB,(5,WRSAPCOD),('WRSAPOVR',0)                        
         GOTO1 (RF),(R1),(L'WRSAMAMT,WRSAMAMT),('WRSAMOVR',0)                   
         GOTO1 (RF),(R1),(L'WRSPHAMT,WRSPHAMT),('WRSPHOVR',0)                   
         GOTO1 (RF),(R1),(L'WRSMDAMT,WRSMDAMT),('WRSMDOVR',0)                   
                                                                                
         MVI   WRSLNQ(R8),X'FF'                                                 
                                                                                
         BAS   RE,GETTIME          GET TIMESHEET RECORD                         
         JE    YES                                                              
         BAS   RE,BLDTIME          BUILD TIMESHEET RECORD                       
         JE    YES                                                              
         BAS   RE,BLDTASD          BUILD SESSION DETAILS ELEMENT                
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE SAVES OFF CAST-LEVEL INFORMATION FOR HOLDING FEE     *         
*        PAYMENT INTO CAST SEQUENCE NUMBER KEYED WSSVR AREA           *         
***********************************************************************         
                                                                                
UPLHFCST NTR1                                                                   
         CLC   WBPYUSE,=C'HLD'     IF USE IS HOLDING FEE                        
         JE    UHFC10                                                           
         CLC   WBPYUSE,=C'SHL'     SPANISH HOLDING FEE                          
         JE    UHFC10                                                           
         CLC   WBPYUSE,=C'ADH'     OR ADDENDUM HOLDING FEE                      
         JNE   XIT                                                              
UHFC10   OI    TGFASTAT,TGCRNOPY                                                
                                                                                
         USING WBPYCSD,R2                                                       
         L     R2,AIO3             INITIALIZE CAST DETAILS BLOCK                
         XC    0(WBPYCLNQ,R2),0(R2)                                             
                                                                                
         MVC   WPCSSEQX,WBCPSEQ    SET CAST SEQUENCE NUMBER                     
         MVC   WPCSPAYX,WBCPPAM    AND PAYMENT AMOUNT                           
                                                                                
         CLI   WBCAGRT,C'E'        SET GUARANTEE APPLY SETTING                  
         JNE   UHFC20                                                           
         OI    WPCSSTAT,WPCSGEND                                                
                                                                                
UHFC20   MVC   WPCSADAX,WBCPADN    SET ADDITIONAL AMOUNT NOT SUBJ P&H           
                                                                                
         OC    WBCPADJ,WBCPADJ     SET ADDITIONAL AMOUNT SUBJECT P&H            
         JZ    UHFC30                                                           
         MVC   WPCSADAX,WBCPADJ                                                 
         OI    WPCSSTAT,WPCSADSP                                                
                                                                                
UHFC30   ZICM  RE,PAYTOT,4                                                      
         A     RE,WPCSPAYX         ADD THIS CAST'S PAYMENT AMOUNT               
         A     RE,WPCSADAX         AND ADDITIONAL AMOUNT                        
         STCM  RE,15,PAYTOT        TO TOTAL EXPECTED PAYMENT                    
                                                                                
         MVC   WPCSCKC,WBCPCHK     SET CHECK COMMENT                            
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN(2),WBCPSEQ                                              
         MVI   FAWSACTN,FAWSASVE                                                
         MVC   FAWSLEN,=AL2(WBPYCLNQ)                                           
         MVC   FAWSADR,AIO3        SAVE CAST DETAILS INTO                       
         GOTO1 WSSVR,(R1)          CAST SEQUENCE NUMBER-KEYED                   
         CLI   FAWSRTN,0           WSSVR AREA                                   
         JE    YES                                                              
         DC    H'00'                                                            
         DROP  R1,R2                                                            
                                                                                
***********************************************************************         
*        ROUTINE TURNS ON OVERRIDE STATUS IF MODE IS EXECUTE          *         
*        OR AMOUNT IS BEING EXPLICITY OVERRIDEN                       *         
*        ON ENTRY ... P1=A(OVERRIDE AMOUNT FIELD)                     *         
*                        BYTE 0 = L'OVERRIDE AMOUNT FIELD             *         
*                     P2 BYTE 0 = OVERRIDE STATUS BIT EQUATE          *         
***********************************************************************         
                                                                                
SETOVSTA NTR1                                                                   
         L     R2,0(R1)                                                         
         ZIC   RE,0(R1)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         OC    0(0,R2),0(R2)       OR AMOUNT EXPLICITY OVERRIDDEN               
         JZ    XIT                                                              
         OC    WRSSTAT,4(R1)       TURN ON OVERRIDE STATUS BIT                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE GETS TIMESHEET RECORD AND SAVES IT IN WSSVR          *         
***********************************************************************         
                                                                                
GETTIME  NTR1                                                                   
         TM    TGUSSTAT,USETIME    EXIT IF PAY TYPE DOES NOT USE                
         JZ    NO                  TIMESHEETS                                   
         OC    WBPYCOM,WBPYCOM                                                  
         JZ    NO                                                               
                                                                                
         USING TLTMD,R4                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLTMCD,TLTMCDQ                                                   
         MVI   TLTMSTA,TLTMSWEB                                                 
         MVC   TLTMCOM,WBPYCOM                                                  
         GOTO1 TINVCON,DMCB,WBPYINV,TLTMINV,DATCON                              
         XC    TLTMINV,=6X'FF'                                                  
         GOTO1 HIGH                                                             
         J     GTIME20                                                          
GTIME10  GOTO1 SEQ                                                              
GTIME20  CLC   KEY(TLTMSSN-TLTMD),KEYSAVE                                       
         JNE   GTIMERR                                                          
         CLC   WBCPSEQ,TLTMSORT+4                                               
         JNE   GTIME10                                                          
         CLC   WBCPCAT,TLTMCAT                                                  
         JNE   GTIMERR                                                          
         LA    RE,=CL3'ON'                                                      
         TM    TLTMSORT,TLCASRFQ                                                
         JZ    *+8                                                              
         LA    RE,=CL3'OFF'                                                     
         CLC   WBCPONO,0(RE)                                                    
         JNE   GTIMERR                                                          
         DROP  R4                                                               
                                                                                
         L     R0,AIO                                                           
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         ST    R0,AIO                                                           
                                                                                
         CLI   WBCPREC,0           IF REIMBURSED EXPENSES ARE                   
         JNE   GTIME40             NOT SET                                      
                                                                                
         USING TATTD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TATTELQ      GET TIMESHEET TOTALS ELEMENT                 
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
GTIME30  BRAS  RE,NEXTEL                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TATTDATE,=X'FFFFFF'                                              
         JNE   GTIME30                                                          
         EDIT  TATTINCL,WRSRECOD   AND SET REIMBURSED EXPENSE CODE              
         MVC   WRSREAMT,TATTREIM   AND REIMBURSED EXPENSE AMOUNT                
         DROP  R4                                                               
                                                                                
         USING TLTMD,R4                                                         
GTIME40  L     R4,AIO3                                                          
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN(2),WBCPSEQ                                              
         MVI   FAWSACTN,FAWSASVE                                                
         MVC   FAWSLEN,TLTMLEN                                                  
         MVC   FAWSADR,AIO3        SAVE TIMESHEET RECORD INTO                   
         GOTO1 WSSVR,(R1)          CAST SEQUENCE NUMBER-KEYED                   
         CLI   FAWSRTN,0           WSSVR AREA                                   
         JE    YES                                                              
         DC    H'00'                                                            
         DROP  R1,R4                                                            
                                                                                
GTIMERR  GOTOR ADDEAP,DMCB,0,ERTIMNFO,0(R8)                                     
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE BUILDS TIMESHEET RECORD AND SAVES IT IN WSSVR        *         
***********************************************************************         
                                                                                
BLDTIME  NTR1                                                                   
         TM    TGUSSTAT,USETIME    EXIT IF PAY TYPE DOES NOT USE                
         JZ    NO                  TIMESHEETS                                   
                                                                                
         USING TLTMD,R4                                                         
         L     R4,AIO3             INITIALIZE TIMESHEET RECORD                  
         XC    0(250,R4),0(R4)                                                  
         MVI   TLTMLEN+1,41                                                     
                                                                                
         USING TASDD,R2                                                         
         LA    R2,ELTASD                                                        
         MVI   TASDEL,TASDELQ      INITIALIZE SESSION DETAILS ELEMENT           
         MVI   TASDLEN,TASDLNQ                                                  
                                                                                
         BAS   RE,ADDTATP          ADD PRIOR-DAY WARDROBE ELEMENT               
                                                                                
         GOTO1 ADDTATT,DMCB,=X'FFFFFE',WBCPRSP   ADD REGULAR DAY                
         GOTO1 (RF),(R1),=X'FFFFFD',WBCPHSP      HOLIDAY                        
         GOTO1 (RF),(R1),=X'FFFFFC',WBCPASP      SATURDAY                       
         GOTO1 (RF),(R1),=X'FFFFFB',WBCPUSP      SUNDAY                         
         GOTO1 (RF),(R1),=X'FFFFF8',WBCPR1SP     REG WEATHER CXL 1/2            
         GOTO1 (RF),(R1),=X'FFFFF7',WBCPH1SP     HOL WEATHER CXL 1/2            
         GOTO1 (RF),(R1),=X'FFFFF6',WBCPA1SP     SAT WEATHER CXL 1/2            
         GOTO1 (RF),(R1),=X'FFFFF5',WBCPU1SP     SUN WEATHER CXL 1/2            
         GOTO1 (RF),(R1),=X'FFFFF4',WBCPR3SP     REG WEATHER CXL 3/4            
         GOTO1 (RF),(R1),=X'FFFFF3',WBCPH3SP     HOL WEATHER CXL 3/4            
         GOTO1 (RF),(R1),=X'FFFFF2',WBCPA3SP     SAT WEATHER CXL 3/4            
         GOTO1 (RF),(R1),=X'FFFFF1',WBCPU3SP     SUN WEATHER CXL 342            
         GOTO1 (RF),(R1),=X'FFFFF0',WBCPDSP      AND DIS LOC ELEMENTS           
                                                                                
         BAS   RE,ADDATATT         ADD ALL TOTAL ELEMENT                        
         BAS   RE,ADDTASD          ADD SESSION DETAILS ELEMENT                  
         DROP  R2                                                               
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN(2),WBCPSEQ                                              
         MVI   FAWSACTN,FAWSASVE                                                
         MVC   FAWSLEN,TLTMLEN                                                  
         ST    R4,FAWSADR          SAVE TIMESHEET RECORD INTO                   
         GOTO1 WSSVR,(R1)          CAST SEQUENCE NUMBER-KEYED                   
         CLI   FAWSRTN,0           WSSVR AREA                                   
         JE    YES                                                              
         DC    H'00'                                                            
         DROP  R1,R4                                                            
                                                                                
***********************************************************************         
*        ROUTINE ADDS TIMESHEET PRIOR DAY WARDROBE ELEMENT            *         
*        ON ENTRY ... R2=A(SESSION DETAILS ELEMENT)                   *         
***********************************************************************         
                                                                                
         USING TASDD,R2                                                         
ADDTATP  NTR1                                                                   
         ZIC   RF,WBCPRPM          TOTAL UP THE PRIOR DAY WARDROBE              
         ZIC   RE,WBCPHPM          MINUTES                                      
         AR    RF,RE                                                            
         ZIC   RE,WBCPAPM                                                       
         AR    RF,RE                                                            
         ZIC   RE,WBCPUPM                                                       
         AR    RF,RE                                                            
                                                                                
         XR    RE,RE                                                            
         D     RE,=F'60'           DIVIDE BY 60                                 
         LR    R1,RE               REMAINDER MINUTES                            
                                                                                
         ZIC   RE,WBCPRPH          QUOTIENT PLUS PRIOR DAY WARDROBE             
         AR    RF,RE               HOURS IS THE TOTAL HOURS                     
         ZIC   RE,WBCPHPH                                                       
         AR    RF,RE                                                            
         ZIC   RE,WBCPAPH                                                       
         AR    RF,RE                                                            
         ZIC   RE,WBCPUPH                                                       
         AR    RF,RE                                                            
         MHI   RF,100                                                           
         AR    RF,R1                                                            
         STCM  RF,3,TASDPDW        SAVE IN SESSION DETAILS ELEMENT              
         DROP  R2                                                               
                                                                                
         USING TATPD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     BUILD PRIOR DAY WARDROBE ELEMENT             
         MVI   TATPEL,TATPELQ      AND ADD IT TO TIMESHEET RECORD               
         MVI   TATPLEN,TATPLNQ                                                  
         GOTO1 TRNPCTHR,DMCB,WBCPRPH,TATPREG                                    
         GOTO1 (RF),(R1),WBCPHPH,TATPHOL                                        
         GOTO1 (RF),(R1),WBCPAPH,TATPSAT                                        
         GOTO1 (RF),(R1),WBCPUPH,TATPSUN                                        
         OC    TATPREG(8),TATPREG                                               
         JZ    XIT                                                              
         GOTO1 HELLO,DMCB,(C'P',=C'TALFIL'),AIO3,(R4)                           
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE ADDS SPECIFIF TIMESHEET TOTAL ELEMENT                *         
*        ON ENTRY ... P1=A(SPECIFIC TIMESHEET TOTAL CODE)             *         
*                     P2=A(REQUEST MAP SPOTS FIELD)                   *         
***********************************************************************         
                                                                                
ADDTATT  NTR1                                                                   
         L     R2,4(R1)                                                         
                                                                                
         ZIC   RE,TRVLHRS                                                       
         ZIC   RF,WBCPRTH-WBCPRSP(R2)                                           
         AR    RE,RF                                                            
         STC   RE,TRVLHRS                                                       
                                                                                
         ZICM  RE,TRVLMNS,2                                                     
         ZIC   RF,WBCPRTM-WBCPRSP(R2)                                           
         AR    RE,RF                                                            
         STCM  RE,3,TRVLMNS                                                     
                                                                                
         USING TATTD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TATTEL,TATTELQ                                                   
         MVI   TATTLEN,TATTLNQ                                                  
         L     RE,0(R1)                                                         
         MVC   TATTDATE,0(RE)                                                   
         MVC   TATTSPOT(TATTNP10-TATTSPOT),0(R2)                                
         GOTO1 TRNTO1BY,DMCB,WBCPRTH-WBCPRSP(R2),TATTTRVL                       
         GOTO1 TRNPCTHR,DMCB,WBCPRPH-WBCPRSP(R2),TATTPDWD                       
         GOTO1 (RF),(R1),WBCPR1H-WBCPRSP(R2),TATTNP10                           
         GOTO1 (RF),(R1),WBCPR2H-WBCPRSP(R2),TATTNP20                           
         MVC   TATT16HR,WBCPR16-WBCPRSP(R2)                                     
                                                                                
         OC    TATTSPOT(TATTEND-TATTSPOT),TATTSPOT                              
         JZ    XIT                                                              
         GOTO1 HELLO,DMCB,(C'P',=C'TALFIL'),AIO3,(R4)                           
                                                                                
         BAS   RE,ADDTOTS                                                       
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE ADDS CURRENT SPOTS, DAYS, OVERTIME HOURS, DOUBLETIME *         
*        HOURS AND TAGS TO ACCUMULATOR FIELDS IN SESSION DETAILS      *         
*        ELEMENT                                                      *         
*        ON ENTRY ... R4=A(CURRENT TOTALS ELEMENT)                    *         
***********************************************************************         
                                                                                
         USING TATTD,R4                                                         
ADDTOTS  NTR1                                                                   
         USING TASDD,R3                                                         
         LA    R3,ELTASD                                                        
                                                                                
         ZIC   RE,TASDSP                                                        
         ZIC   RF,TATTSPOT                                                      
         AR    RE,RF                                                            
         STC   RE,TASDSP           ADD TO TOTAL SPOTS                           
                                                                                
         ZIC   RE,TASDDAY                                                       
         ZIC   RF,TATTDAYS                                                      
         AR    RE,RF                                                            
         STC   RE,TASDDAY          ADD TO TOTAL DAYS                            
                                                                                
         ZIC   RE,TASDOT                                                        
         ZIC   RF,TATTOVTM                                                      
         AR    RE,RF                                                            
         STC   RE,TASDOT           ADD TO TOTAL OVERTIME HOURS                  
                                                                                
         ZIC   RE,TASDDT                                                        
         ZIC   RF,TATTDBTM                                                      
         AR    RE,RF                                                            
         STC   RE,TASDDT           ADD TO TOTAL DOUBLETIME HOURS                
                                                                                
         ZIC   RE,TASDTAG                                                       
         ZIC   RF,TATTTAG                                                       
         AR    RE,RF                                                            
         STC   RE,TASDTAG          ADD TO TOTAL TAGS                            
         J     XIT                                                              
         DROP  R3,R4                                                            
                                                                                
***********************************************************************         
*        ROUTINE ADDS TIMESHEET ALL TOTALS ELEMENT                    *         
***********************************************************************         
                                                                                
ADDATATT NTR1                                                                   
         USING TATTD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     INITIALIZE ALL TOTALS ELEMENT                
         MVI   TATTEL,TATTELQ                                                   
         MVI   TATTLEN,TATTLNQ                                                  
                                                                                
         OC    WBCPMPN,WBCPMPN     IF MEAL PENALTY AMOUNT PROVIDED              
         JZ    AAT10                                                            
         MVC   TATTDATE,=X'FFFFFF' ADD MEAL PENALTY TO ELEMENT                  
         MVC   TATTNSPH,WBCPMPN                                                 
                                                                                
AAT10    OC    WBCPSMK,WBCPSMK     IF SMOKE PAY AMOUNT PROVIDED                 
         JZ    AAT20                                                            
         MVC   TATTDATE,=X'FFFFFF' ADD SMOKE PAY TO ELEMENT                     
         MVC   TATTPYMT,WBCPSMK                                                 
                                                                                
AAT20    OC    WBCPEXH(2),WBCPEXH  IF EXTRAS FRI TO SAT ADJUSTMENT              
         JZ    AAT30               PROVIDED                                     
         MVC   TATTDATE,=X'FFFFFF' IF YES, ADD EXTRAS FRI TO SAT                
         GOTO1 TRNPCTHR,DMCB,WBCPEXH,TATTXSAT ADJUSTMENT TO ELEMENT             
                                                                                
AAT30    CLI   WBCPRPV,0           IF REST PERIOD VIOLATIONS PROVIDED           
         JE    AAT40                                                            
         MVC   TATTDATE,=X'FFFFFF' IF YES, ADD REST PERIOD VIOLATIONS           
         MVC   TATTRPVL,WBCPRPV    TO ELEMENT                                   
                                                                                
AAT40    CLC   TATTDATE,=X'FFFFFF' IF ALL ELEMENT SHOULD BE ADDED               
         JNE   XIT                 DO SO NOW                                    
         GOTO1 HELLO,DMCB,(C'P',=C'TALFIL'),AIO3,(R4)                           
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE ADDS SESSION DETAILS ELEMENT                         *         
***********************************************************************         
                                                                                
         USING TASDD,R2                                                         
ADDTASD  NTR1                                                                   
         ZICM  RF,TRVLMNS,2                                                     
         XR    RE,RE               DIVIDE ACCUMULATED TRAVEL MINUTES            
         D     RE,=F'60'           BY 60                                        
         STC   RE,HALF+1           REMAINDER IS TOTAL MINUTES                   
         ZIC   RE,TRVLHRS          QUOTIENT PLUS ACCUMULATED TRAVEL             
         AR    RF,RE               HOURS IS THE TOTAL TRAVEL HOURS              
         STC   RF,HALF                                                          
         GOTO1 TRNTO1BY,DMCB,HALF,TASDTRV                                       
                                                                                
         MVC   ELEMENT,ELTASD      ADD SESSION DETAILS ELEMENT                  
         GOTO1 HELLO,DMCB,(C'P',=C'TALFIL'),AIO3,ELEMENT                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ROUTINE TRANSLATES 2-BYTE HOURS/MINUTES TO 1-BYTE            *         
*        HOURS/MINUTES                                                *         
*        ON ENTRY ... R1=A(HOURS FIELD IN REQUEST MAP)                *         
*                     P2=A(HOURS/MINUTES FIELD IN ELEMENT)            *         
***********************************************************************         
                                                                                
TRNTO1BY NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
                                                                                
         ZIC   RE,0(R2)                                                         
         MHI   RE,100                                                           
         ZIC   RF,1(R2)                                                         
         AR    RE,RF                                                            
         STCM  RE,3,0(R3)                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TRANSLATES HOURS/MINUTES TO % OF AN HOUR             *         
*        ON ENTRY ... P1=A(HOURS FIELD IN REQUEST MAP)                *         
*                     P2=A(HOURS FIELD IN ELEMENT)                              
***********************************************************************         
                                                                                
TRNPCTHR NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
                                                                                
         XR    RE,RE                                                            
         ZIC   RF,1(R2)                                                         
         MHI   RF,100                                                           
         D     RE,=F'60'                                                        
                                                                                
         CHI   RE,30                                                            
         JL    *+8                                                              
         AHI   RF,1                                                             
                                                                                
         ZIC   RE,0(R2)                                                         
         MHI   RE,100                                                           
         AR    RE,RF                                                            
         STCM  RE,3,0(R3)                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE BUILDS SESSION DETAILS ELEMENT AND SAVES IN IN WSSVR *         
***********************************************************************         
                                                                                
BLDTASD  NTR1                                                                   
         CLI   TGUSEQU,UBSR        EXIT IF PAY TYPE IS NOT BSR                  
         JE    BTASD10                                                          
         CLI   TGUSEQU,URRR        OR RRR                                       
         JE    BTASD10                                                          
         CLI   TGUSEQU,UADO        OR ADO                                       
         JNE   XIT                                                              
                                                                                
         USING TASDD,R4                                                         
BTASD10  LA    R4,ELTASD                                                        
         MVI   TASDEL,TASDELQ      INITIALIZE SESSION DETAILS ELEMENT           
         MVI   TASDLEN,TASDLNQ                                                  
         MVC   TASDEQU,TGUSEQU                                                  
         MVC   TASDRSP,WBCPRSP                                                  
         GOTO1 TRNTO1BY,DMCB,WBCPRHR,TASDRHM                                    
         MVC   TASDRTG,WBCPRTG                                                  
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN(2),WBCPSEQ                                              
         MVI   FAWSACTN,FAWSASVE                                                
         MVC   FAWSLEN,=AL2(TASDLNQ)                                            
         ST    R4,FAWSADR          SAVE SESSION DETAILS ELEMENT INTO            
         GOTO1 WSSVR,(R1)          CAST SEQUENCE NUMBER-KEYED                   
         CLI   FAWSRTN,0           WSSVR AREA                                   
         JE    XIT                                                              
         DC    H'00'                                                            
         DROP  R1,R4                                                            
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR UPLCAST                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERTIMNFO DC    AL1(ETIMNFOX-*),AL2(ERTSNOFO),AL1(ERRCATY3),AL1(D#PCSEQ)         
         DC    AL2(0)                                                           
         DC    C'Timesheet not on file'                                         
ETIMNFOX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SEND CAST PAYMENT BREAKDOWN DETAILS BACK TO       *         
*        WEB APPLICATION                                              *         
*        ON ENTRY ... R8=A(WEB RESPONSE DETAILS AREA)                 *         
***********************************************************************         
                                                                                
DWNCAST  NTR1  BASE=*,LABEL=*                                                   
         OC    WBCPDSQ,WBCPDSQ     IF DOWNLOADING CAST PAYMENT                  
         JZ    NO                  DETAILS AND NO CATEGORY 3 ERRORS             
         CLI   ERRTAB+EECATY-ERRENTD,ERRCATY3                                   
         JE    YES                                                              
         TM    TGFASTAT,TGERUSDT   OR USE DETAILS ERRORS WERE                   
         JO    YES                 ENCOUNTERED                                  
                                                                                
DC10     CLI   0(R8),X'FF'         FIND CAST SEQUENCE NUMBER IN                 
         JNE   *+6                 WEB RESPONSE DETAILS AREA                    
         DC    H'00'                                                            
         CLC   WBCPDSQ,WRSSEQ                                                   
         JE    DC20                                                             
         LA    R8,WRSLNQ(R8)                                                    
         J     DC10                                                             
                                                                                
DC20     GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTMAP',O#PAYDET)            
                                                                                
         GOTO1 HEXOUT,DMCB,WRSSEQ,OUTPUT,L'WRSSEQ,0                             
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',1),         +        
               ('LD_CHARQ',OUTPUT),(4,0)                                        
                                                                                
         OC    WRSAPAMT,WRSAPAMT                                                
         JZ    DC30                                                             
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',33),          +        
               ('LD_CHARQ',WRSAPCOD),(L'WRSAPCOD,0)                             
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',34),          +        
               ('LD_UBINQ',WRSAPAMT),(L'WRSAPAMT,0)                             
                                                                                
DC30     OC    WRSREAMT,WRSREAMT                                                
         JZ    DC40                                                             
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',35),          +        
               ('LD_CHARQ',WRSRECOD),(L'WRSRECOD,0)                             
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',36),          +        
               ('LD_UBINQ',WRSREAMT),(L'WRSREAMT,0)                             
                                                                                
DC40     OC    WRSAMAMT,WRSAMAMT                                                
         JZ    DC50                                                             
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',37),          +        
               ('LD_UBINQ',WRSAMAMT),(L'WRSAMAMT,0)                             
                                                                                
DC50     OC    WRSPHAMT,WRSPHAMT                                                
         JZ    DC60                                                             
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',38),          +        
               ('LD_UBINQ',WRSPHAMT),(L'WRSPHAMT,0)                             
                                                                                
DC60     OC    WRSMDAMT,WRSMDAMT                                                
         JZ    DC70                                                             
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',39),          +        
               ('LD_UBINQ',WRSMDAMT),(L'WRSMDAMT,0)                             
                                                                                
DC70     L     RE,AIO3                                                          
         LA    RF,4000                                                          
         XCEF                                                                   
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN(L'WBCPDSQ),WBCPDSQ                                      
         MVI   FAWSACTN,FAWSARST   RECALL CAST SEQUENCE NUMBER KEYED            
         MVC   FAWSADR,AIO3        WSSVR AREA                                   
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         JNE   YES                                                              
         DROP  R1                                                               
                                                                                
         USING TLRCD,R3                                                         
         L     R3,AIO3                                                          
         TM    TGUSSTAT,USETIME    IF MAKING A TIMESHEET PAYMENT                
         JZ    DC80                                                             
         ZICM  R0,TLRCLEN,2                                                     
         AR    R3,R0               BUMP PAST THE TIMESHEET                      
         DROP  R3                                                               
                                                                                
         USING TASDD,R3                                                         
DC80     CLI   TGUSEQU,UBSR        IF MAKING A BSR PAYMENT                      
         JE    DC81                                                             
         CLI   TGUSEQU,URRR        OR RRR PAYMENT                               
         JE    DC81                                                             
         CLI   TGUSEQU,UADO        OR ADO PAYMENT                               
         JNE   DC85                                                             
DC81     ZIC   R0,TASDLEN          BUMP PAST THE SESSION DETAILS                
         AR    R3,R0               ELEMENT                                      
         DROP  R3                                                               
                                                                                
         USING WEBBRKD,R3                                                       
DC85     OC    WBRKGUA,WBRKGUA                                                  
         JZ    DC90                                                             
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',50),        +        
               ('LD_CHARQ',WBRKGUA),(L'WBRKGUA,0)                               
                                                                                
         USING PAYBRKDD,R4                                                      
DC90     LA    R4,WBRKBRK                                                       
         LA    R4,PBBRKDAT         R4=A(PAYMENT BREAKDOWN DETAILS)              
                                                                                
         USING PBDATAD,R4                                                       
DC100    OC    0(PBUNTLNQ,R4),0(R4)                                             
         JZ    YES                                                              
                                                                                
         CLI   PBCODE,PBCOVP       SKIP OVERSCALE PERCENTAGE AMOUNT             
         JNE   DC110               IF IT IS NOT BEING SENT BACK                 
         TM    SVPYBRK,WBPYSKVO    SEPARATELY                                   
         JZ    DC140                                                            
                                                                                
DC110    GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTMAP',O#PAYBRK)            
                                                                                
         TM    TGUSSTA3,NWKUSE                                                  
         JO    DC111                                                            
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',1),           +        
               ('LD_UBINQ',PBCODE),(L'PBCODE,0)                                 
         J     DC112                                                            
                                                                                
DC111    GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',2),           +        
               ('LD_UBINQ',PBUNITST),(L'PBUNITST,0)                             
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',3),           +        
               ('LD_UBINQ',PBUNITEN),(L'PBUNITEN,0)                             
                                                                                
DC112    GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',4),         +        
               ('LD_UBINQ',PBUNITUN),(L'PBUNITUN,0)                             
         CLI   PBCODE,PVCTAG                                                    
         JE    DC115                                                            
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',5),           +        
               ('LD_UBINQ',PBUNITRT),(L'PBUNITRT,0)                             
                                                                                
DC115    OC    WBRKOV1,WBRKOV1     IF OVERSCALE RATE IS PRESENT                 
         JZ    DC140                                                            
         CLI   PBCODE,PBCPLY       AND THIS IS NOT PENALTY                      
         JE    DC140                                                            
         CLI   PBCODE,PVCMEA       OR MEAL PENALTY                              
         JE    DC140                                                            
******** CLI   PBCODE,PVCSMK       OR SMOKE PAY                                 
******** JE    DC140                                                            
         CLI   PBCODE,PVCADJ       OR ADJUSTMENT                                
         JE    DC140                                                            
         CLI   PBCODE,PVCRPV       OR REST PERIOD VIOLATION                     
         JE    DC140                                                            
                                                                                
         ZICM  RE,PBUNITUN,4                                                    
         CLI   PBCODE,PVCTRV       RE=NUMBER OF UNITS                           
         JNE   DC120                                                            
         XR    R0,R0                                                            
         LR    R1,RE                                                            
         D     R0,=F'100'                                                       
         CHI   R0,50                                                            
         JL    *+8                                                              
         AHI   R1,1                                                             
         LR    RE,R1                                                            
                                                                                
DC120    ZICM  R1,PBUNITRT,4       MULTIPLY NUMBER OF UNITS                     
         MR    R0,RE               BY SCALE RATE                                
         XR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CHI   R0,50                                                            
         JL    *+8                                                              
         AHI   R1,1                                                             
         ST    R1,FULL                                                          
                                                                                
         ZICM  R0,WBRKOV1,4                                                     
         TM    WBRKOV1,X'80'       THEN MULTIPLY BY FIRST OVERSCALE             
         JZ    *+8                 PERCENTAGE                                   
         N     R0,=X'7FFFFFFF'                                                  
         BAS   RE,MULTR0                                                        
         TM    WBRKOV1,X'80'                                                    
         JO    *+8                                                              
         A     R1,FULL                                                          
         ST    R1,FULL                                                          
                                                                                
         OC    WBRKOV2,WBRKOV2     THEN, IF PRESENT, MULTIPLY BY SECOND         
         JZ    DC130               OVERSCALE PERCENTAGE                         
         ZICM  R0,WBRKOV2,4                                                     
         BAS   RE,MULTR0                                                        
         A     R1,FULL                                                          
         ST    R1,FULL                                                          
                                                                                
DC130    GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',6),         +        
               ('LD_UBINQ',FULL),(L'FULL,0)                                     
         J     DC150                                                            
                                                                                
DC140    CLI   PBCODE,PVCTAG                                                    
         JNE   DC150                                                            
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',6),           +        
               ('LD_UBINQ',PBUNITRT),(L'PBUNITRT,0)                             
                                                                                
DC150    LA    R4,PBUNTLNQ(R4)     BUMP TO NEXT BREAKDOWN IN BLOCK              
         J     DC100                                                            
         DROP  R3,R4                                                            
                                                                                
***********************************************************************         
                                                                                
MULTR0   MR    R0,R0                                                            
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         JM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         BR    RE                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SAVE USE DETAILS INTO WSSVR                       *         
***********************************************************************         
                                                                                
UPLUDET  NTR1  BASE=*,LABEL=*                                                   
         OC    WBPUDAT(WBPULNQ),WBPUDAT                                         
         JZ    NO                                                               
                                                                                
         CLI   WBPUUID,0           IF UPLOADING USE DETAILS                     
         JNE   UUD10               ENSURE UNIQUE ID IS PROVIDED                 
         GOTOR ADDEAR,DMCB,UUDMIS,WBPUUID                                       
         J     YES                                                              
                                                                                
UUD10    L     R2,AIO3             RECALL PREVIOUSLY SAVED USE DETAILS          
         GOTOR REUSEDET,DMCB,(R2)  WSSVR AREA                                   
         JNE   UUD40                                                            
                                                                                
UUD20    CLC   WBPUUID,WBPUUID-WBPUDAT(R2)   ASSERT UNIQUE IDENTIFER            
         JNE   UUD30                         WASN'T SUPPLIED PREVIOUSLY         
         GOTOR ADDEAR,DMCB,UUDDUI,WBPUUID                                       
         J     YES                                                              
                                                                                
UUD30    LA    R2,WBPULNQ(R2)      FIND FIRST EMPTY SLOT                        
         CLI   0(R2),X'FF'         AND SAVE USE DETAILS INTO IT                 
         JNE   UUD20                                                            
UUD40    MVC   0(WBPULNQ,R2),WBPUDAT                                            
         MVI   WBPULNQ(R2),X'FF'                                                
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN,=C'USDT'                                                
         MVI   FAWSACTN,FAWSASVE                                                
         MVC   FAWSLEN,=H'4000'                                                 
         MVC   FAWSADR,AIO3        SAVE USE DETAILS INTO                        
         GOTO1 WSSVR,(R1)          WSSVR AREA                                   
         CLI   FAWSRTN,0                                                        
         JE    YES                                                              
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR UPLUDET                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
UUDMIS   DC    AL1(UUDMISX-*),AL2(502),AL1(ERRCATY3),AL1(0)                     
         DC    AL2(D#PUUID)                                                     
         DC    C'Missing field'                                                 
UUDMISX  EQU   *                                                                
                                                                                
UUDDUI   DC    AL1(UUDDUIX-*),AL2(EDUPUID),AL1(ERRCATY3),AL1(0)                 
         DC    AL2(D#PUUID)                                                     
         DC    C'Duplicated unique identifier'                                  
UUDDUIX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE INITIALIZE VARIABLES                                 *         
***********************************************************************         
                                                                                
INIVARS  NTR1  BASE=*,LABEL=*                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
                                                                                
         MVC   SVSYSFIL,=CL8'TALFIL'                                            
         MVC   SVSYSDIR,=CL8'TALDIR'                                            
                                                                                
         ST    R7,TGAFAREQ         SAVE ADDRESS OF WEB REQUEST DETAILS          
         ST    R8,TGAFARES         SAVE ADDRESS OF WEB RESPONSE DETAILS         
                                                                                
         CLC   =C'VS',WBWAPID      IF PAYMENT COMING FROM OLD VITA TV           
         JNE   *+8                 SESSION                                      
         OI    TGFASTAT,TGVITSES   SET STATUS                                   
                                                                                
         CLC   =C'TS',WBWAPID      IF PAYMENT COMING FROM NEW VITA TV           
         JNE   *+8                 SESSION                                      
         OI    TGFASTAT,TGVITSES   SET STATUS                                   
                                                                                
         CLC   =C'RS',WBWAPID      IF PAYMENT COMING FROM VITA RADIO            
         JNE   *+8                 SESSION                                      
         OI    TGFASTAT,TGVITSES   SET STATUS                                   
                                                                                
         CLC   =C'VC',WBWAPID      IF PAYMENT COMING FROM OLD VITA TV           
         JNE   *+8                 COMPLETIONS                                  
         OI    TGFASTAT,TGVITCMP   SET STATUS                                   
                                                                                
         CLC   =C'TC',WBWAPID      IF PAYMENT COMING FROM NEW VITA TV           
         JNE   *+8                 COMPLETIONS                                  
         OI    TGFASTAT,TGVITCMP   SET STATUS                                   
                                                                                
         CLC   =C'RC',WBWAPID      IF PAYMENT COMING FROM VITA RADIO            
         JNE   *+8                 COMPLETIONS                                  
         OI    TGFASTAT,TGVITCMP   SET STATUS                                   
                                                                                
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'20',0)                                    
         JE    *+6                                                              
         DC    H'00'               READ SYSTEM RECORD                           
                                                                                
         USING TASYD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TASYELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TGTPEMP,TASYEMP     SAVE DEFAULT EMPLOYER                        
         MVC   TGUSBNK,TASYUSBK         US BANK ACCOUNT                         
         MVC   TGCNBNK,TASYCNBK         CAN. BANK ACCOUNT                       
         MVC   TGEUBNK,TASYEUBK         EUR. BANK ACCOUNT                       
         MVC   TGSYSTAT,TASYSTAT        SYSTEM STATUS                           
         MVC   TGSYSTA2,TASYSTA2        SYSTEM STATUS 2                         
         DROP  R4                                                               
                                                                                
         CLC   WBACTIN,=AL2(I#TMULD)    IF UPLOADING TIMESHEET                  
         JNE   IV20                                                             
         NI    PROSTAT,X'FF'-PSTMFRST                                           
         CLC   WBTMCOM,TGCOM            IF THIS IS THE FIRST                    
         JNE   IV10                     TIME WE'VE SEEN THIS                    
         CLC   WBTMINV,TMLINV           TIMESHEET IN THIS TRANSMISSION          
         JNE   IV10                                                             
         CLC   WBTMSEQ,TMLSEQ                                                   
         JE    XIT                                                              
IV10     MVC   TGCOM,WBTMCOM                                                    
         MVC   TMLINV,WBTMINV                                                   
         MVC   TMLSEQ,WBTMSEQ                                                   
         OI    PROSTAT,PSTMFRST         SET STATUS                              
         NI    PROSTAT,X'FF'-PSERWBTS                                           
         J     XIT                                                              
                                                                                
IV20     MVC   SVPYBRK,WBPYBRK     SAVE PAYMENT BREAKDOWN OPTIONS               
         MVI   ACTNUM,ACTPAY       INITIALIZE GLOBAL VARIABLES                  
                                                                                
         CLC   WBPYUSE,=C'HLD'     IF USE IS HOLDING FEE                        
         JE    IV30                                                             
         CLC   WBPYUSE,=C'SHL'     SPANISH HOLDING FEE                          
         JE    IV30                                                             
         CLC   WBPYUSE,=C'ADH'     OR ADDENDUM HOLDING FEE                      
         JNE   XIT                                                              
IV30     OI    TGFASTAT,TGCRNOPY   SET PAY REQUEST FROM CERNO STATUS            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE FORMATS REQUEST FIELDS                               *         
***********************************************************************         
                                                                                
FMTREQ   NTR1  BASE=*,LABEL=*                                                   
         OC    I$CRNOCST,I$CRNOCST IF CERNO CAST FIELDS ARE PRESENT             
         JZ    *+8                                                              
         BAS   RE,FCRNOPY          FORMAT CERNO CAST FIELDS                     
                                                                                
         OC    I$EROV,I$EROV       IF ERROR OVERRIDES ARE PRESENT               
         JZ    *+8                                                              
         BAS   RE,FERROVS          FORMAT THEM                                  
                                                                                
         BAS   RE,FISPLIT          FORMAT ISPLIT FIELDS                         
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE FORMATS CERNO PAY FIELDS                             *         
***********************************************************************         
                                                                                
FCRNOPY  NTR1                                                                   
         ZICM  R4,I$CRNOCST+1,3                                                 
         MVC   WBPYCIN,7(R4)       SET CAST COUNT                               
         ZIC   R3,7(R4)                                                         
                                                                                
         USING WBPYCSD,R4                                                       
         LA    R4,8(R4)                                                         
         STCM  R4,7,AWBPYCS        SET A(CAST DETAILS ARRAY)                    
                                                                                
FCPY10   GOTO1 HEXIN,DMCB,WPCSSEQ,HALF,L'WPCSSEQ                                
         XC    WPCSSEQ,WPCSSEQ                                                  
         MVC   WPCSSEQX,HALF       TRANSLATE CAST SEQ. TO HEX                   
                                                                                
         GOTO1 CASHVAL,DMCB,WPCSPAY,L'WPCSPAY                                   
         CLI   0(R1),0                                                          
         JE    *+6                                                              
         DC    H'00'                                                            
         XC    WPCSPAY,WPCSPAY                                                  
         MVC   WPCSPAYX,4(R1)      TRANSLATE PAYMENT AMOUNT TO HEX              
                                                                                
         MVC   WORK,WPCSADA        IF ADDITONAL AMOUNT WAS NOT                  
         XC    WPCSADA,WPCSADA     ENTERED, CHANGE TO ZEROES                    
         CLC   WORK(L'WPCSADA),SPACES                                           
         JE    FCPY20                                                           
                                                                                
         GOTO1 CASHVAL,DMCB,WORK,L'WPCSADA                                      
         CLI   0(R1),0                                                          
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   WPCSADAX,4(R1)      TRANSLATE ADDITIONAL AMOUNT TO HEX           
                                                                                
FCPY20   ZICM  RE,PAYTOT,4                                                      
         A     RE,WPCSPAYX         ADD THIS CAST'S PAYMENT AMOUNT               
         A     RE,WPCSADAX         AND ADDITIONAL AMOUNT                        
         STCM  RE,15,PAYTOT        TO TOTAL EXPECTED PAYMENT                    
                                                                                
         LA    R4,WBPYCLNQ(R4)     BUMP TO NEXT CAST REC IN ARRAY               
         BCT   R3,FCPY10           AND GO PROCESS IT                            
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE FORMATS ERROR OVERRIDES                              *         
***********************************************************************         
                                                                                
FERROVS  NTR1                                                                   
         ZICM  RF,I$EROV+1,3                                                    
         MVC   WBPYEIN,7(RF)       SET ERROR OVERRIDE COUNT                     
         LA    RF,8(RF)                                                         
         STCM  RF,7,AWBPYEO        SET A(ERROR OVERRIDE ARRAY)                  
         LA    RE,WBPYEIN                                                       
         MVC   TGAFAOER,0(RE)      AND SAVE IN GLOBAL STORAGE                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE FORMATS ISPLIT FIELDS                                *         
***********************************************************************         
                                                                                
FISPLIT  NTR1                                                                   
         CLC   WBACTIN,=AL2(I#TMULD)                                            
         JE    XIT                                                              
                                                                                
         LHI   R3,10               R3=ISPLIT FIELD COUNT                        
         XR    R0,R0                                                            
                                                                                
         USING WBPYSPD,R4                                                       
         LA    R4,WBPYSI1          R4=A(CURRENT ISPLIT FIELD)                   
FISP10   OC    0(WBPYSLNQ,R4),0(R4)                                             
         JZ    FISP20                                                           
         OC    WPSPEST,SPACES      ENSURE FIELD IS ALL CAPS                     
                                                                                
         CLC   WBACTIN,=AL2(I#PAYULD) IF ACTION IS PAY                          
         JNE   FISP20                                                           
         GOTO1 CASHVAL,DMCB,(4,WPSPPCT),L'WPSPPCT                               
         CLI   0(R1),0                                                          
         JE    *+6                                                              
         DC    H'00'                                                            
         XC    WPSPPCT,WPSPPCT     IF CURRENT ISPLIT FIELD POPULATED            
         MVC   WPSPPCTX,4(R1)      TRANSLATE SPLIT PERCENTAGE TO HEX            
         A     R0,4(R1)                                                         
                                                                                
FISP20   LA    R4,WBPYSLNQ(R4)     BUMP TO NEXT SPLIT INVOICE FIELD             
         BCT   R3,FISP10           AND GO PROCESS IT                            
         DROP  R4                                                               
                                                                                
         TM    TGFASTAT,TGCRNOPY   IF PAYMENT IS NOT COMING FROM                
         JO    YES                 CERNO                                        
         LTR   R0,R0               ASSERT THAT PERCENTAGES ADD UP               
         JZ    YES                 TO 100                                       
         L     RF,=F'1000000'                                                   
         CR    R0,RF                                                            
         JE    YES                                                              
         GOTOR ADDGERR,DMCB,('EENINV',0),('D#PYSI1',0)                          
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES REQUEST FIELDS                             *         
***********************************************************************         
                                                                                
VALREQ   NTR1  BASE=*,LABEL=*                                                   
         CLC   WBACTIN,=AL2(I#PAYULD)                                           
         JE    VRPY10                                                           
         CLC   WBACTIN,=AL2(I#PJVULD)                                           
         JE    VRPJ10                                                           
         CLC   WBACTIN,=AL2(I#TMULD)                                            
         JE    VRTM10                                                           
         DC    H'00'                                                            
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES REQUEST FIELDS FOR PAY                     *         
***********************************************************************         
                                                                                
VRPY10   BRAS  RE,VRSTF                      VALIDATE STAFF                     
         JNE   VRPY30                                                           
                                                                                
         GOTO1 USEVAL,DMCB,(X'40',WBPYUSE)   SET USE VARIABLES                  
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         BAS   RE,VRAGY                      VALIDATE AGENCY                    
         JNE   VRPY30                                                           
         BRAS  RE,VRCLI                      VALIDATE CLIENT                    
         JNE   VRPY30                                                           
         BAS   RE,VRPRD                      VALIDATE PRODUCT                   
         JNE   VRPY30                                                           
         BRAS  RE,VRCOM                      VALIDATE COMM'L                    
         JNE   VRPY30                                                           
         BRAS  RE,VRTYP                      VALIDATE PAYMENT TYPE              
         JNE   VRPY30                                                           
         BRAS  RE,VRVER                      VALIDATE VERSION                   
         JNE   VRPY30                                                           
         BAS   RE,VRLFT                      VALIDATE LIFT                      
                                                                                
         MVI   WBPYSKV,0                                                        
         GOTOR VRISP,DMCB,(19,WBPYSI1)       VALIDATE INVOICE SPLITS            
         GOTOR VRISP,DMCB,(20,WBPYSI2)                                          
         GOTOR VRISP,DMCB,(21,WBPYSI3)                                          
         GOTOR VRISP,DMCB,(22,WBPYSI4)                                          
         GOTOR VRISP,DMCB,(23,WBPYSI5)                                          
         GOTOR VRISP,DMCB,(24,WBPYSI6)                                          
         GOTOR VRISP,DMCB,(25,WBPYSI7)                                          
         GOTOR VRISP,DMCB,(26,WBPYSI8)                                          
         GOTOR VRISP,DMCB,(27,WBPYSI9)                                          
         GOTOR VRISP,DMCB,(28,WBPYSIA)                                          
                                                                                
         OC    SVDEFJOB,SVDEFJOB                                                
         JZ    VRPY20                                                           
         GOTOR AVSINDUP,DMCB,WBPYSI1                                            
         JNE   VRPY30                                                           
         GOTOR AVSINDUP,DMCB,WBPYSI2                                            
         JNE   VRPY30                                                           
         GOTOR AVSINDUP,DMCB,WBPYSI3                                            
         JNE   VRPY30                                                           
         GOTOR AVSINDUP,DMCB,WBPYSI4                                            
         JNE   VRPY30                                                           
         GOTOR AVSINDUP,DMCB,WBPYSI5                                            
         JNE   VRPY30                                                           
         GOTOR AVSINDUP,DMCB,WBPYSI6                                            
         JNE   VRPY30                                                           
         GOTOR AVSINDUP,DMCB,WBPYSI7                                            
         JNE   VRPY30                                                           
         GOTOR AVSINDUP,DMCB,WBPYSI8                                            
         JNE   VRPY30                                                           
         GOTOR AVSINDUP,DMCB,WBPYSI9                                            
         JNE   VRPY30                                                           
         GOTOR AVSINDUP,DMCB,WBPYSIA                                            
         JNE   VRPY30                                                           
                                                                                
VRPY20   BAS   RE,VRINV                      VALIDATE INVOICE                   
         BRAS  RE,VRAGN                      VALIDATE OVERRIDE AGENTS           
         BRAS  RE,VRPRT                      VALIDATE PRINT FIELDS              
                                                                                
VRPY30   CLI   ERRTAB+EECATY-ERRENTD,ERRCATY3                                   
         JNE   YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES REQUEST FIELDS FOR AUTH/PO                 *         
*        ESTIMATE/JOB VALIDATION                                      *         
***********************************************************************         
                                                                                
VRPJ10   BAS   RE,VRAGY                      IF VALIDATING AUTH/PO              
         JNE   VRPJ40                        ESTIMATE/JOB, VALIDATE AGY         
                                                                                
         BRAS  RE,VRCLI                      VALIDATE CLIENT                    
         JNE   VRPJ40                                                           
                                                                                
         OC    WBPYPRD,WBPYPRD               VALIDATE PRODUCT                   
         JNZ   VRPJ20                                                           
         TM    SVACBSTA,TABRSINT                                                
         JO    *+12                                                             
         TM    SVAYSTA6,TAAYST10                                                
         JZ    VRPJ30                                                           
         GOTO1 ADDERROR,DMCB,ERMISPRD                                           
         J     VRPJ40                                                           
VRPJ20   BAS   RE,VRPRD                                                         
         JNE   VRPJ40                                                           
                                                                                
VRPJ30   BAS   RE,VRAPO                      VALIDATE AUTH/PO                   
                                                                                
         BRAS  RE,VREJB                      VALIDATE ESTIMATE/JOBS             
         GOTOR VRISP,DMCB,(19,WBPYSI1)                                          
         GOTOR VRISP,DMCB,(20,WBPYSI2)                                          
         GOTOR VRISP,DMCB,(21,WBPYSI3)                                          
         GOTOR VRISP,DMCB,(22,WBPYSI4)                                          
         GOTOR VRISP,DMCB,(23,WBPYSI5)                                          
         GOTOR VRISP,DMCB,(24,WBPYSI6)                                          
         GOTOR VRISP,DMCB,(25,WBPYSI7)                                          
         GOTOR VRISP,DMCB,(26,WBPYSI8)                                          
         GOTOR VRISP,DMCB,(27,WBPYSI9)                                          
         GOTOR VRISP,DMCB,(28,WBPYSIA)                                          
                                                                                
VRPJ40   CLI   ERRTAB,X'FF'                                                     
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES REQUEST FIELDS FOR TIMESHEET UPLOAD        *         
***********************************************************************         
                                                                                
VRTM10   BRAS  RE,VRSTF                      IF UPLOADING TIMESHEET,            
         JNE   VRTM20                        VALIDATE STAFF                     
                                                                                
         OC    WBTMCOM,WBTMCOM               IF UPLOADED A NON-HYPO             
         JZ    VRTM20                        TIMESHEET                          
         BAS   RE,VRTMCOM                    VALIDATE COMMERCIAL                
         JNE   VRTM20                                                           
         BAS   RE,VRTMINV                    VALIDATE INVOICE                   
         JNE   VRTM20                                                           
         BAS   RE,VRTMSEQ                    VALIDATE CAST SEQ NUMBER           
                                                                                
VRTM20   CLI   ERRTAB,X'FF'                                                     
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES AGENCY                                     *         
***********************************************************************         
                                                                                
VRAGY    NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A4',WBPYAGY)   VALIDATE AGENCY            
         JE    VRAGY10                                                          
         LA    RF,ERNLUAGY                                                      
         TM    TGFASTAT,TGCRNOPY                                                
         JO    *+8                                                              
         LA    RF,ERAGYNFO                                                      
         GOTO1 ADDERROR,DMCB,(RF)                                               
         J     NO                                                               
                                                                                
         USING TAAYD,R4                                                         
VRAGY10  L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY ELEMENT                           
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVAYIAGY,TAAYIAGY   SAVE PARENT AGENCY                           
         MVC   SVAYSTAT,TAAYSTAT   SAVE STATUS                                  
         MVC   SVAYSTA2,TAAYSTA2   2ND STATUS                                   
         MVC   SVAYSTA4,TAAYSTA4   4TH STATUS                                   
         MVC   SVAYSTA5,TAAYSTA5   5TH STATUS                                   
         MVC   SVAYSTA6,TAAYSTA6   6TH STATUS                                   
         MVC   SVAYSTA7,TAAYSTA7   7TH STATUS                                   
         MVC   SVAYBUNT,TAAYBUNT   AND BUSINESS UNIT                            
         DROP  R4                                                               
                                                                                
         USING TABRD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TABRELQ      GET BILLING RULES ELEMENT                    
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVACBSTA,TABRSTAT   SAVE STATUS                                  
         DROP  R4                                                               
                                                                                
         USING TANUD,R4                                                         
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTJOB))                                     
         JNE   VRAGY20                                                          
         L     R4,TGELEM                                                        
         MVC   SVAYTJOB,TANUMBER   SAVE JOB SETUP LENGTHS                       
         NC    SVAYTJOB,=X'0F0F0F'                                              
         DROP  R4                                                               
                                                                                
         USING TANUD,R4                                                         
VRAGY20  GOTO1 GETL,DMCB,(1,=AL1(TANUTDJB))                                     
         JNE   VRAGY30                                                          
         L     R4,TGELEM                                                        
         MVC   SVDEFJOB,TANUMBER   SAVE DEFAULT JOB NUMBER                      
         DROP  R4                                                               
                                                                                
VRAGY30  OC    SVAYIAGY,SVAYIAGY   IF PRESENT, VALIDATE PARENT AGENCY           
         JZ    YES                                                              
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A4',SVAYIAGY)                             
         JE    VRAGY40                                                          
         GOTO1 ADDERROR,DMCB,ERERRAGY                                           
         J     VRAGY50                                                          
                                                                                
         USING TAAYD,R4                                                         
VRAGY40  L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ       GET AGENCY ELEMENT                          
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         OC    TAAYIAGY,TAAYIAGY    IF PARENT AGENCY ALSO UNDER                 
         JZ    VRAGY50              A PARENT, RETURN ERROR                      
         GOTO1 ADDERROR,DMCB,ERERRAGY                                           
         DROP  R4                                                               
                                                                                
VRAGY50  MVC   TGAGY,WBPYAGY        RESTORE COMMERCIAL'S AGENCY                 
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES PRODUCT                                    *         
***********************************************************************         
                                                                                
VRPRD    NTR1                                                                   
         OC    WBPYPRD,WBPYPRD     VALIDATE PRODUCT                             
         JZ    YES                                                              
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'A4',WBPYPRD)                              
         JE    VRPRD10                                                          
         TM    TGFASTAT,TGCRNOPY                                                
         JO    YES                                                              
         GOTO1 ADDERROR,DMCB,ERPRDNFO                                           
         J     NO                                                               
                                                                                
VRPRD10  OC    WBPYSI1,WBPYSI1     IF SPLITTING THE INVOICE                     
         JZ    YES                 NEED TO SAVE SOME VARIABLES                  
         MVC   SVPRPICD,TGPRD      SET PRODUCTION PRODUCT                       
                                                                                
         USING TAISD,R4                                                         
         MVI   ELCODE,TAISELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAISTYPP))                                     
         JNE   YES                                                              
         L     R4,TGELEM                                                        
         MVC   SVPRPICD,TAISCDE                                                 
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES LIFT                                       *         
*        ON ENTRY ... AIO=A(COMMERCIAL RECORD)                        *         
***********************************************************************         
                                                                                
VRLFT    NTR1                                                                   
         CLI   WBPYLFT,0           IF LIFT INPUT IS PRESENT                     
         JE    XIT                                                              
         L     R4,AIO                                                           
         MVI   ELCODE,TALFELQ      ENSURE THAT COMMERCIAL HAS A                 
         BRAS  RE,GETEL            LIFT                                         
         JE    XIT                                                              
         GOTO1 ADDERROR,DMCB,ERLIFTNO                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES AUTH/PO FIELD                              *         
***********************************************************************         
                                                                                
VRAPO    NTR1                                                                   
         TM    WBPYSKV,WBPYSKVP    EXIT IF SKIPPING AUTH/PO VALIDATION          
         JO    XIT                                                              
                                                                                
         OC    WBPYAPO,WBPYAPO     IF AUTH/PO IS NOT PRESENT                    
         JNZ   VRAPO20                                                          
         TM    SVAYSTA4,TAAYPOV    AND AGENCY REQUIRES VALIDATION               
         JO    VRAPO10                                                          
         TM    SVAYSTAT,TAAYSAPO   OR AUTH/PO IS REQUIRED                       
         JZ    XIT                 RETURN ERROR                                 
VRAPO10  GOTO1 ADDERROR,DMCB,ERAUTHRQ                                           
         J     XIT                                                              
                                                                                
         USING TLPUD,R3                                                         
VRAPO20  TM    SVAYSTA4,TAAYPOV    IF AGENCY REQUIRES VALIDATION                
         JZ    VRAPO30                                                          
         LA    R3,KEY              ENSURE PURCHASE ORDER EXISTS                 
         XC    KEY,KEY             ON TALENT SYSTEM                             
         MVI   TLPUCD,TLPUCDQ                                                   
         MVI   TLPUSCD,TLPUSCDQ                                                 
         MVC   TLPUAGY,=CL6'OMNY'                                               
         MVC   TLPUPO,=15C'0'                                                   
         MVC   TLPUPO(10),WBPYAPO                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLPUKEY),KEYSAVE                                           
         JE    XIT                                                              
         GOTO1 ADDERROR,DMCB,ERINVPO                                            
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
VRAPO30  TM    SVAYSTA4,TAAYSCPO   IF AGENCY CLOSES PURCHASE ORDERS             
         JZ    XIT                                                              
         XC    KEY,KEY             ENSURE PURCHASE ORDER EXISTS                 
         MVI   KEY,ACOKCODQ        ON ACCOUNTING SYSTEM                         
         MVC   KEY+4(6),WBPYAPO                                                 
         GOTO1 READACC,DMCB,(X'80',0)                                           
         JE    VRAPO40                                                          
         GOTO1 ADDERROR,DMCB,ERACNTFD                                           
         J     XIT                                                              
                                                                                
         USING ACKEYD,R4                                                        
VRAPO40  L     R4,AIO              ENSURE NOT MATCHED OR DELETED                
         TM    ACSTATUS,ACORDDEL+ACORDMCH+ACORDPHS                              
         JZ    VRAPO50                                                          
         GOTO1 ADDERROR,DMCB,ERPOCLOS                                           
         J     XIT                                                              
                                                                                
VRAPO50  OC    WBPYEST,WBPYEST     ENSURE ESTIMATE/JOB IS PROVIDED              
         JNZ   VRAPO60                                                          
         GOTO1 ADDERROR,DMCB,ERPOMJOB                                           
         J     XIT                                                              
                                                                                
VRAPO60  LA    R4,ACRECORD                                                      
         DROP  R4                                                               
                                                                                
         USING ACORDRD,R4                                                       
         MVI   ELCODE,X'67'        ENSURE ESTIMATE/JOB EXISTS                   
         BRAS  RE,FIRSTEL          ON PURCHASE ORDER                            
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   ACORJOB+3(12),WBPYEST                                            
         JE    XIT                                                              
         GOTO1 ADDERROR,DMCB,ERPOJOB                                            
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES INVOICE NUMBER                             *         
***********************************************************************         
                                                                                
VRINV    NTR1                                                                   
         OC    WBPYINV,WBPYINV     IF INVOICE INPUT IS PRESENT                  
         JZ    XIT                                                              
                                                                                
         GOTO1 TINVCON,DMCB,WBPYINV,TGINV,DATCON                                
         CLI   0(R1),X'FF'         ENSURE INVOICE FORMAT IS VALID               
         JNE   VRINV10                                                          
         GOTO1 ADDERROR,DMCB,ERINVIFM                                           
         J     XIT                                                              
                                                                                
VRINV10  XC    TGINV,=6X'FF'       ENSURE INVOICE RECORD EXISTS                 
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A4',0)                                    
         JE    VRINV20                                                          
         GOTO1 ADDERROR,DMCB,ERINVNEX                                           
         J     XIT                                                              
                                                                                
         USING TAIND,R4                                                         
VRINV20  L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS ELEMENT                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    TAINSTAT,TAINSPAY   ENSURE INVOICE HAS NOT BEEN PAID             
         JZ    VRINV30                                                          
         GOTO1 ADDERROR,DMCB,ERINVAPD                                           
         J     XIT                                                              
VRINV30  CLC   =C'TS',WBWAPID      IF WE'RE NOT COMING FROM NEW VITA            
         JE    VRINV40             TV                                           
         OC    TAINTMCO,TAINTMCO   ENSURE NOT RESERVED FOR TIMESHEET            
         JZ    VRINV40                                                          
         GOTO1 ADDERROR,DMCB,ERINVTIM                                           
         J     XIT                                                              
VRINV40  TM    TAINSTA2,TAINSPRM   AND IS NOT ALREADY SPLIT                     
         JZ    VRINV50                                                          
         GOTO1 ADDERROR,DMCB,ERINVSPL                                           
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
VRINV50  L     R4,AIO                                                           
         MVI   ELCODE,TAAIELQ      ENSURE INVOICE DOES NOT HAVE                 
         BRAS  RE,GETEL            AN ATTACHED ADVICE                           
         JNE   XIT                                                              
         GOTO1 ADDERROR,DMCB,ERINVADV                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES TIMESHEET COMMERCIAL                       *         
***********************************************************************         
                                                                                
VRTMCOM  NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',WBTMCOM)  VALIDATE COMM'L            
         JE    VRTC10                                                           
         GOTO1 ADDERROR,DMCB,ERTMCNFO                                           
         J     NO                                                               
                                                                                
         USING TLCOD,R4                                                         
VRTC10   L     R4,AIO                                                           
         MVC   TGAGY,TLCOAGY                                                    
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TGCID,TACOCID                                                    
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES TIMESHEET INVOICE                          *         
***********************************************************************         
                                                                                
VRTMINV  NTR1                                                                   
         GOTO1 TINVCON,DMCB,WBTMINV,TGINV,DATCON                                
         CLI   0(R1),X'FF'         ENSURE INVOICE FORMAT IS VALID               
         JNE   YES                                                              
         GOTO1 ADDERROR,DMCB,ERINVIFM                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES TIMESHEET CAST SEQUENCE NUMBER             *         
***********************************************************************         
                                                                                
VRTMSEQ  NTR1                                                                   
         USING TLCAD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,WBTMCOM                                                  
         GOTO1 HIGH                                                             
         J     VRTS20                                                           
VRTS10   GOTO1 SEQ                                                              
VRTS20   CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         JNE   VRTS30                                                           
         CLC   TLCASEQ,WBTMSEQ                                                  
         JNE   VRTS10                                                           
         MVC   TGCSORT,TLCASORT                                                 
         MVC   TGSSN,TLCASSN                                                    
         MVC   TGCAT,TLCACAT                                                    
         J     YES                                                              
         DROP  R3                                                               
                                                                                
VRTS30   GOTO1 ADDERROR,DMCB,ERTMSQNF                                           
         J     NO                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALREQ                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERNLUAGY DC    AL1(ENLUAGYX-*),AL2(ERCIDNFD),AL1(ERRCATY3),AL1(D#PYAGY)         
         DC    AL2(0)                                                           
         DC    C'Commercial no longer under this Agency'                        
ENLUAGYX EQU   *                                                                
                                                                                
ERAGYNFO DC    AL1(EAGYNFOX-*),AL2(ERRAGYNF),AL1(ERRCATY3),AL1(D#PYAGY)         
         DC    AL2(0)                                                           
         DC    C'Agency record is not on file'                                  
EAGYNFOX EQU   *                                                                
                                                                                
ERMISPRD DC    AL1(EMISPRDX-*),AL2(MISSING),AL1(ERRCATY3),AL1(D#PYPRD)          
         DC    AL2(0)                                                           
         DC    C'Product is required'                                           
EMISPRDX EQU   *                                                                
                                                                                
ERPRDNFO DC    AL1(EPRDNFOX-*),AL2(ERPRDNFD),AL1(ERRCATY3),AL1(D#PYPRD)         
         DC    AL2(0)                                                           
         DC    C'Product record is not on file'                                 
EPRDNFOX EQU   *                                                                
                                                                                
ERERRAGY DC    AL1(EERRAGYX-*),AL2(ERAGYERR),AL1(ERRCATY3),AL1(D#PYAGY)         
         DC    AL2(0)                                                           
         DC    C'Unable to assign a new invoice - check agency record'          
EERRAGYX EQU   *                                                                
                                                                                
ERLIFTNO DC    AL1(ELIFTNOX-*),AL2(ERNOLIFT),AL1(ERRCATY1),AL1(D#PYLFT)         
         DC    AL2(0)                                                           
         DC    C'Commercial does not have a lift'                               
ELIFTNOX EQU   *                                                                
                                                                                
ERAUTHRQ DC    AL1(EAUTHRQX-*),AL2(ERAPOREQ),AL1(ERRCATY1),AL1(D#PYAPO)         
         DC    AL2(0)                                                           
         DC    C'PO# is required'                                               
EAUTHRQX EQU   *                                                                
                                                                                
ERINVPO  DC    AL1(EINVPOX-*),AL2(ERPONTFD),AL1(ERRCATY2),AL1(D#PYAPO)          
         DC    AL2(0)                                                           
         DC    C'PO# not found on Talent System'                                
EINVPOX  EQU   *                                                                
                                                                                
ERACNTFD DC    AL1(EACNTFDX-*),AL2(ERPONOAC),AL1(ERRCATY1),AL1(D#PYAPO)         
         DC    AL2(0)                                                           
         DC    C'PO# does not exist on the accounting system'                   
EACNTFDX EQU   *                                                                
                                                                                
ERPOCLOS DC    AL1(EPOCLOSX-*),AL2(ERPOCLSE),AL1(ERRCATY1),AL1(D#PYAPO)         
         DC    AL2(0)                                                           
         DC    C'PO# already closed/matched'                                    
EPOCLOSX EQU   *                                                                
                                                                                
ERPOMJOB DC    AL1(EPOMJOBX-*),AL2(ERMISJOB),AL1(ERRCATY1),AL1(D#PYSI1)         
         DC    AL2(0)                                                           
         DC    C'Job# is required for this PO#'                                 
EPOMJOBX EQU   *                                                                
                                                                                
ERPOJOB  DC    AL1(EPOJOBX-*),AL2(ERPOIJOB),AL1(ERRCATY1),AL1(D#PYSI1)          
         DC    AL2(0)                                                           
         DC    C'Invalid job# for this PO#'                                     
EPOJOBX  EQU   *                                                                
                                                                                
ERINVIFM DC    AL1(EINVIFMX-*),AL2(ERRIIFMT),AL1(ERRCATY1),AL1(D#PYINV)         
         DC    AL2(0)                                                           
         DC    C'Invalid format'                                                
EINVIFMX EQU   *                                                                
                                                                                
ERINVNEX DC    AL1(EINVNEXX-*),AL2(ERRINNEX),AL1(ERRCATY1),AL1(D#PYINV)         
         DC    AL2(0)                                                           
         DC    C'The invoice number indicated is not on file'                   
EINVNEXX EQU   *                                                                
                                                                                
ERINVAPD DC    AL1(EINVAPDX-*),AL2(ERINVPD),AL1(ERRCATY1),AL1(D#PYINV)          
         DC    AL2(0)                                                           
         DC    C'Invoice number has already been used'                          
EINVAPDX EQU   *                                                                
                                                                                
ERINVTIM DC    AL1(EINVTIMX-*),AL2(ERINVUSD),AL1(ERRCATY1),AL1(D#PYINV)         
         DC    AL2(0)                                                           
         DC    C'Invoice number is reserved for a mainframe timesheet'          
EINVTIMX EQU   *                                                                
                                                                                
ERINVSPL DC    AL1(EINVSPLX-*),AL2(ERRINSPL),AL1(ERRCATY1),AL1(D#PYINV)         
         DC    AL2(0)                                                           
         DC    C'Invoice has been split in PC Talent'                           
EINVSPLX EQU   *                                                                
                                                                                
ERINVADV DC    AL1(EINVADVX-*),AL2(ERRINADV),AL1(ERRCATY1),AL1(D#PYINV)         
         DC    AL2(0)                                                           
         DC    C'Invoice has an attached Advice'                                
EINVADVX EQU   *                                                                
                                                                                
ERTMCNFO DC    AL1(ETMCNFOX-*),AL2(ERCIDNFD),AL1(ERRCATY3),AL1(D#TMCOM)         
         DC    AL2(0)                                                           
         DC    C'Commercial no longer exists'                                   
ETMCNFOX EQU   *                                                                
                                                                                
ERTMSQNF DC    AL1(ETMSQNFX-*),AL2(ERTSSQNF),AL1(ERRCATY3),AL1(D#TMSEQ)         
         DC    AL2(0)                                                           
         DC    C'Performer no longer exists under this Commercial'              
ETMSQNFX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES STAFF CODE                                 *         
***********************************************************************         
                                                                                
VRSTF    NTR1  BASE=*,LABEL=*                                                   
         MVC   TGUSER,TWAORIG                                                   
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'A4',WBSTAFF)                              
         JE    VRSTF10                                                          
         GOTO1 ADDERROR,DMCB,ERSTFNFD                                           
         JNE   XIT                                                              
VRSTF10  MVC   TGCTSTAF,WBSTAFF                                                 
                                                                                
         USING TASTD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TASTELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TGCTSTTY,TASTTYPE   SET STAFF TYPE                               
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VRSTF                                      *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERSTFNFD DC    AL1(ESTFNFDX-*),AL2(ERSTNTFD),AL1(ERRCATY3),AL1(D#PYSTF)         
         DC    AL2(0)                                                           
         DC    C'Staff record is not on file'                                   
ESTFNFDX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES CLIENT                                     *         
***********************************************************************         
                                                                                
VRCLI    NTR1  BASE=*,LABEL=*                                                   
         OC    WBPYCLI,WBPYCLI     IF CLIENT IS PROVIDED                        
         JZ    YES                                                              
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A4',WBPYCLI)   VALIDATE CLIENT            
         JE    VRCLI10                                                          
         TM    TGFASTAT,TGCRNOPY                                                
         JO    YES                                                              
         GOTO1 ADDERROR,DMCB,ERCLINFO                                           
         J     NO                                                               
                                                                                
         USING TACID,R4                                                         
VRCLI10  TM    TGFASTAT,TGCRNOPY   IF REQUEST COMING FROM CERNO                 
         JZ    VRCLI30                                                          
         TM    SVAYSTA5,TAAYCIHR   AGENCY                                       
         JO    VRCLI30                                                          
         L     R4,AIO                                                           
         MVI   ELCODE,TACIELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   VRCLI20                                                          
         TM    TACISTA2,TACICIHR   OR CLIENT MUST BE ELIGIBLE                   
         JO    VRAGY20                                                          
VRCLI20  GOTO1 ADDERROR,DMCB,ERACNELI                                           
         DROP  R4                                                               
                                                                                
VRCLI30  OC    WBPYSI1,WBPYSI1     IF SPLITTING THE INVOICE                     
         JZ    YES                 NEED TO SAVE SOME VARIABLES                  
                                                                                
         USING TABRD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TABRELQ       GET BILLING RULES ELEMENT                   
         BRAS  RE,GETEL                                                         
         JNE   VRCLI40                                                          
         MVC   SVACBSTA,TABRSTAT    SAVE STATUS                                 
         DROP  R4                                                               
                                                                                
VRCLI40  MVC   SVCLPICD,TGCLI       SET PRODUCTION CLIENT                       
                                                                                
         USING TAISD,R4                                                         
         MVI   ELCODE,TAISELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAISTYPC))                                     
         JNE   YES                                                              
         L     R4,TGELEM                                                        
         MVC   SVCLPICD,TAISCDE                                                 
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VRCLI                                      *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCLINFO DC    AL1(ECLINFOX-*),AL2(ERCLINFD),AL1(ERRCATY3),AL1(D#PYCLI)         
         DC    AL2(0)                                                           
         DC    C'Client record is not on file'                                  
ECLINFOX EQU   *                                                                
                                                                                
ERACNELI DC    AL1(EACNELIX-*),AL2(ERNELAGY),AL1(ERRCATY1),AL1(D#PYCLI)         
         DC    AL2(0)                                                           
         DC    C'Direct pay not enabled for this Agency/Client'                 
EACNELIX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES COMMERCIAL                                 *         
***********************************************************************         
                                                                                
VRCOM    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',WBPYCOM)  VALIDATE COMM'L            
         JE    VRCOM10                                                          
         GOTO1 ADDERROR,DMCB,ERCIDNFO                                           
         J     NO                                                               
                                                                                
         USING TAFND,R4                                                         
VRCOM10  TM    TGFASTAT,TGVITSES+TGVITCMP                                       
         JZ    VRCOM30             IF PAYMENT IS COMING FROM VITA               
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         JNE   VRCOM20                                                          
         L     R4,TGELEM                                                        
         ZIC   RE,TAFNLEN                                                       
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   WBWAPID(0),TAFNNAME ENSURE THAT COMMERCIAL WAS LAST              
         JE    VRCOM30             UPDATED FROM THIS VITA SESSION               
         CLC   =C'VC',TAFNNAME     OR FROM A VITA COMPLETION                    
         JE    VRCOM30                                                          
         CLC   =C'TC',TAFNNAME                                                  
         JE    VRCOM30                                                          
         CLC   =C'RC',TAFNNAME                                                  
         JE    VRCOM30                                                          
VRCOM20  GOTO1 ADDERROR,DMCB,ERCOVSNP                                           
         J     NO                                                               
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
VRCOM30  L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TLCOD,R3                                                         
         L     R3,AIO                                                           
         CLC   TLCOAGY,WBPYAGY     ENSURE THAT COMMERCIAL AGENCY                
         JE    VRCOM40             MATCHES REQUEST AGENCY                       
         LA    RF,ERNLUAGC                                                      
         TM    TGFASTAT,TGCRNOPY                                                
         JO    *+8                                                              
         LA    RF,ERREVAGY                                                      
         GOTO1 ADDERROR,DMCB,(RF)                                               
         J     NO                                                               
                                                                                
VRCOM40  OC    WBPYCVD,WBPYCVD     IF COMMERCIAL VERIFICATION DATE              
         JNZ   VRCOM60             IS NOT PROVIDED                              
         CLC   TLCOCLI,WBPYCLI     ENSURE THAT COMMERCIAL CLIENT                
         JE    VRCOM50             MATCHES REQUEST CLIENT                       
         LA    RF,ERNLUCLI                                                      
         TM    TGFASTAT,TGCRNOPY                                                
         JO    *+8                                                              
         LA    RF,ERREVCLI                                                      
         GOTO1 ADDERROR,DMCB,(RF)                                               
         J     NO                                                               
                                                                                
VRCOM50  CLI   TACOMED,TACOMEDP    IF MEDIA IS NOT PRINT                        
         JE    VRCOM60                                                          
         CLC   TLCOPRD,WBPYPRD     ENSURE THAT COMMERCIAL PRODUCT               
         JE    VRCOM60             MATCHES REQUEST PRODUCT                      
         LA    RF,ERNLUPRD                                                      
         TM    TGFASTAT,TGCRNOPY                                                
         JO    *+8                                                              
         LA    RF,ERREVPRD                                                      
         GOTO1 ADDERROR,DMCB,(RF)                                               
         J     NO                                                               
         DROP  R3                                                               
                                                                                
VRCOM60  OC    WBPYCVD,WBPYCVD     IF CAST VERIFICATION DATE IS                 
         JZ    VRCOM65             PROVIDED                                     
         LA    R2,ERNMTCMD                                                      
         OC    TACOVDTE,TACOVDTE   ENSURE IT MATCHES COMMERCIAL                 
         JZ    VRCOM60A                                                         
         GOTO1 DATCON,DMCB,(1,TACOVDTE),(8,PARAS)                               
         CLC   WBPYCVD(8),PARAS                                                 
         JNE   VRCOM60A                                                         
                                                                                
         GOTO1 TIMECON,DMCB,TACOVTIM,TACOVDTE,(8,PARAS)                         
         CLC   WBPYCVT,PARAS                                                    
         JE    VRCOM65                                                          
         LA    R2,ERNMTCMT                                                      
VRCOM60A GOTO1 ADDERROR,DMCB,(R2)                                               
         J     NO                                                               
                                                                                
VRCOM65  TM    TGFASTAT,TGCRNOPY   IF NOT COMING FROM CERNO                     
         JO    VRCOM70                                                          
         TM    TACOSTAT,TACOSTRL   GIVE WARNING IF COMMERCIAL IS                
         JZ    VRCOM70             RELEASED                                     
         GOTO1 ADDERROR,DMCB,ERRELCOM                                           
                                                                                
VRCOM70  MVC   TGCID,TACOCID       SAVE COMMERCIAL ID                           
         MVC   SVCOADST,TACOADST   AND ADDENDUM STATE                           
                                                                                
         CLC   TACOCID,WBPYCID     CHECK IF COMMERCIAL ID MATCHES               
         JE    YES                 REQUEST COMMERCIAL ID                        
         TM    TGFASTAT,TGCRNOPY   IF COMING FROM CERNO, IT MUST                
         JZ    VRCOM80                                                          
         GOTO1 ADDERROR,DMCB,ERCHGCID                                           
         J     NO                                                               
         DROP  R4                                                               
                                                                                
VRCOM80  MVC   PARAS(L'TGCID),TGCID                                             
         GOTO1 RECVAL,DMCB,(X'20',TLCOICDQ),(X'84',WBPYCID)                     
         MVC   TGCID,PARAS                                                      
         JE    VRCOM90             ELSE, CHECK IF IT IS A VERSION ID            
         OC    WBPYCVD,WBPYCVD                                                  
         JNZ   YES                                                              
         GOTO1 ADDERROR,DMCB,ERREVCID                                           
         J     NO                                                               
                                                                                
         USING TLCOPD,R3                                                        
VRCOM90  LA    R3,KEY                                                           
         CLI   WBPYVER,0           IF VERSION HAS NOT BEEN PROVIDED             
         JNE   VRCOM100                                                         
         MVC   WBPYVER,TLCOIVER    SAVE IT NOW                                  
                                                                                
VRCOM100 CLC   TLCOICOM,WBPYCOM    AND ENSURE THAT REQUEST COMM'L ID            
         JE    YES                 IS A VALID VERSION FOR THIS COMM'L           
         GOTO1 ADDERROR,DMCB,ERREVCID                                           
         J     NO                                                               
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALREQ                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCIDNFO DC    AL1(ECIDNFOX-*),AL2(ERCIDNFD),AL1(ERRCATY3),AL1(D#PYCID)         
         DC    AL2(0)                                                           
         DC    C'Commercial no longer exists'                                   
ECIDNFOX EQU   *                                                                
                                                                                
ERCOVSNP DC    AL1(ECOVSNPX-*),AL2(ERCONPVS),AL1(ERRCATY3),AL1(D#PYCID)         
         DC    AL2(0)                                                           
         DC    CL60'Commercial not eligible for payment from this Vita +        
               Session'                                                         
ECOVSNPX EQU   *                                                                
                                                                                
ERNLUAGC DC    AL1(ENLUAGCX-*),AL2(ERCIDNFD),AL1(ERRCATY3),AL1(D#PYAGY)         
         DC    AL2(0)                                                           
         DC    C'Commercial no longer under this Agency'                        
ENLUAGCX EQU   *                                                                
                                                                                
ERREVAGY DC    AL1(EREVAGYX-*),AL2(ERAGYNFD),AL1(ERRCATY3),AL1(D#PYAGY)         
         DC    AL2(0)                                                           
         DC    C'Commercial no longer under this Agency code'                   
EREVAGYX EQU   *                                                                
                                                                                
ERNLUCLI DC    AL1(ENLUCLIX-*),AL2(ERCLICHG),AL1(ERRCATY3),AL1(D#PYCLI)         
         DC    AL2(0)                                                           
         DC    C'Client has changed'                                            
ENLUCLIX EQU   *                                                                
                                                                                
ERREVCLI DC    AL1(EREVCLIX-*),AL2(ERCLICHG),AL1(ERRCATY3),AL1(D#PYCLI)         
         DC    AL2(0)                                                           
         DC    C'Review Client - does not match mainframe'                      
EREVCLIX EQU   *                                                                
                                                                                
ERNLUPRD DC    AL1(ENLUPRDX-*),AL2(ERPRDCHG),AL1(ERRCATY3),AL1(D#PYPRD)         
         DC    AL2(0)                                                           
         DC    C'Product has changed'                                           
ENLUPRDX EQU   *                                                                
                                                                                
ERREVPRD DC    AL1(EREVPRDX-*),AL2(ERPRDCHG),AL1(ERRCATY3),AL1(D#PYPRD)         
         DC    AL2(0)                                                           
         DC    C'Review Product - does not match mainframe'                     
EREVPRDX EQU   *                                                                
                                                                                
ERNMTCMD DC    AL1(ENMTCMDX-*),AL2(ENMTCMF),AL1(ERRCATY3),AL1(D#PYCVD)          
         DC    AL2(0)                                                           
         DC    C'Does not match mainframe'                                      
ENMTCMDX EQU   *                                                                
                                                                                
ERNMTCMT DC    AL1(ENMTCMTX-*),AL2(ENMTCMF),AL1(ERRCATY3),AL1(D#PYCVT)          
         DC    AL2(0)                                                           
         DC    C'Does not match mainframe'                                      
ENMTCMTX EQU   *                                                                
                                                                                
ERRELCOM DC    AL1(ERELCOMX-*),AL2(ERCOMREL),AL1(ERRCATY2),AL1(D#PYCID)         
         DC    AL2(0)                                                           
         DC    C'Commercial is released'                                        
ERELCOMX EQU   *                                                                
                                                                                
ERCHGCID DC    AL1(ECHGCIDX-*),AL2(ERCIDCHG),AL1(ERRCATY3),AL1(D#PYCID)         
         DC    AL2(0)                                                           
         DC    C'Commercial ID has changed'                                     
ECHGCIDX EQU   *                                                                
                                                                                
ERREVCID DC    AL1(EREVCIDX-*),AL2(ERCIDCHG),AL1(ERRCATY3),AL1(D#PYCID)         
         DC    AL2(0)                                                           
         DC    C'Review Commercial ID - does not match mainframe'               
EREVCIDX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES PAYMENT TYPE                               *         
***********************************************************************         
                                                                                
VRTYP    NTR1  BASE=*,LABEL=*                                                   
         CLI   TGUSEQU,UADO                                                     
         JNE   YES                                                              
                                                                                
         CLC   SVCOADST,=C'TX'                                                  
         JNE   VRTYP10                                                          
         CLI   WBPYTYP,0                                                        
         JE    YES                                                              
         GOTOR ADDGERR,DMCB,('EENNAL',0),('D#PYTYP',0)                          
         J     NO                                                               
                                                                                
VRTYP10  CLI   WBPYTYP,0                                                        
         JNE   VRTYP20                                                          
         GOTOR ADDGERR,DMCB,('EENMIS',0),('D#PYTYP',0)                          
         J     NO                                                               
                                                                                
VRTYP20  CLC   SVCOADST,=C'GA'                                                  
         JNE   VRTYP30                                                          
         CLI   WBPYTYP,WBPT3DY                                                  
         JE    YES                                                              
         CLI   WBPYTYP,WBPT1WK                                                  
         JE    YES                                                              
         CLI   WBPYTYP,WBPT4WK                                                  
         JE    YES                                                              
         CLI   WBPYTYP,WBPT13W                                                  
         JE    YES                                                              
         GOTOR ADDGERR,DMCB,('EENINV',0),('D#PYTYP',0)                          
         J     NO                                                               
                                                                                
VRTYP30  CLC   SVCOADST,=C'KS'                                                  
         JNE   VRTYP40                                                          
         CLI   WBPYTYP,WBPT1WK                                                  
         JE    YES                                                              
         CLI   WBPYTYP,WBPT31D                                                  
         JE    YES                                                              
         CLI   WBPYTYP,WBPT13W                                                  
         JE    YES                                                              
         GOTOR ADDGERR,DMCB,('EENINV',0),('D#PYTYP',0)                          
         J     NO                                                               
                                                                                
VRTYP40  CLC   SVCOADST,=C'NW'                                                  
         JNE   NO                                                               
         CLI   WBPYTYP,WBPT2WM                                                  
         JE    YES                                                              
         CLI   WBPYTYP,WBPT2WMU                                                 
         JE    YES                                                              
         CLI   WBPYTYP,WBPT2WS                                                  
         JE    YES                                                              
         CLI   WBPYTYP,WBPT2WSU                                                 
         JE    YES                                                              
         CLI   WBPYTYP,WBPT13M                                                  
         JE    YES                                                              
         CLI   WBPYTYP,WBPT13MU                                                 
         JE    YES                                                              
         CLI   WBPYTYP,WBPT13S                                                  
         JE    YES                                                              
         CLI   WBPYTYP,WBPT13SU                                                 
         JE    YES                                                              
         GOTOR ADDGERR,DMCB,('EENINV',0),('D#PYTYP',0)                          
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES VERSION                                    *         
***********************************************************************         
                                                                                
VRVER    NTR1  BASE=*,LABEL=*                                                   
         CLI   WBPYVER,2           IF PAYING VERSION 2 OR HIGHER                
         JL    VRVER10                                                          
                                                                                
         GOTO1 RECVAL,DMCB,(X'20',TLVRCDQ),(X'A4',WBPYVER)                      
         JE    VRVER10                                                          
         GOTO1 ADDERROR,DMCB,ERVERNFO                                           
         J     NO                  ENSURE THAT VERSION EXISTS                   
                                                                                
VRVER10  CLC   TGCID,WBPYCID       ENSURE THAT PROVIDED COMMERCIAL ID           
         JE    YES                 IS FOR VERSION 1 ...                         
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TACOCID,WBPYCID     ... OR FOR THE PROVIDED VERSION              
         JE    YES                                                              
         GOTO1 ADDERROR,DMCB,ERCVNMTC                                           
         J     NO                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VRVER                                      *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCVNMTC DC    AL1(ECVNMTCX-*),AL2(ECVNMTC),AL1(ERRCATY3),AL1(D#PYVER)          
         DC    AL2(0)                                                           
         DC    C'Version ID mismatch'                                           
ECVNMTCX EQU   *                                                                
                                                                                
ERVERNFO DC    AL1(EVERNFOX-*),AL2(EVERNFO),AL1(ERRCATY3),AL1(D#PYVER)          
         DC    AL2(0)                                                           
         DC    C'Version record is not on file'                                 
EVERNFOX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED FIELDS HAVE BEEN PROVIDED       *         
***********************************************************************         
                                                                                
ASRTREQ  NTR1  BASE=*,LABEL=*                                                   
         TM    TGFASTAT,TGCRNOPY   SKIP IF PAYMENT COMING FROM CERNO            
         JO    YES                                                              
         OC    WBCPDSQ,WBCPDSQ     OR CAST PAYMENT BREAKDOWN REQUEST            
         JNZ   YES                                                              
         CLI   WBPUUID,0           OR USE DETAILS REQUEST                       
         JNE   YES                                                              
                                                                                
         CLC   WBACTIN,=AL2(I#PJVULD) IF AUTH/PO EST/JOB VALIDATION             
         JE    AR00                   SKIP AHEAD                                
                                                                                
         MVI   TGBYTE3,D#PYMOD     ASSERT THAT MODE IS PROVIDED                 
         CLI   WBMODE,0                                                         
         JE    ARMIS                                                            
                                                                                
         CLC   WBACTIN,=AL2(I#TMULD)  IF TIMESHEET UPLOAD                       
         JE    ARTM00                 SKIP AHEAD                                
                                                                                
         MVI   TGBYTE3,D#PYUSE     ASSERT THAT USE IS PROVIDED                  
         OC    WBPYUSE,WBPYUSE                                                  
         JZ    ARMIS                                                            
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED FIELDS HAVE BEEN PROVIDED       *         
*        FOR PAYABLE CAST UPLOAD                                      *         
***********************************************************************         
                                                                                
         OC    WBCPSEQ,WBCPSEQ                                                  
         JZ    AR00                                                             
                                                                                
         MVI   TGBYTE3,D#PYCOM                                                  
         OC    WBPYCOM,WBPYCOM     IF INTERNAL COMMERCIAL NUMBER IS             
         JZ    ARCA00              PROVIDED                                     
         OC    WBPYINV,WBPYINV     ASSERT THAT INVOICE NUMBER IS                
         JZ    ARNAL               PROVIDED                                     
         OC    WBCPCAT,WBCPCAT     ASSERT THAT CATEGORY IS PROVIDED             
         JZ    ARNAL                                                            
         OC    WBCPONO,WBCPONO     ASSERT THAT ON/OFF CAMERA IS                 
         JNZ   ARCA05              PROVIDED                                     
         J     ARNAL                                                            
*                                  IF INTERNAL COMMERCIAL NUMBER IS             
*                                  NOT PROVIDED                                 
ARCA00   OC    WBPYINV,WBPYINV     ASSERT THAT INVOICE NUMBER IS                
         JNZ   ARMIS               NOT PROVIDED                                 
         OC    WBCPCAT,WBCPCAT     ASSERT THAT CATEGORY IS NOT                  
         JNZ   ARMIS               PROVIDED                                     
         OC    WBCPONO,WBCPONO     ASSERT THAT ON/OFF CAMERA IS                 
         JNZ   ARMIS               NOT PROVIDED                                 
                                                                                
ARCA05   CLI   WBCPAPC,0           IF APPLICATION CODE IS NOT                   
         JNE   ARCA10              PROVIDED                                     
         OC    WBCPAPA,WBCPAPA                                                  
         JZ    ARCA20                                                           
         MVI   TGBYTE3,D#PCAPA     ASSERT THAT APPLICATION AMOUNT               
         J     ARNAL               IS NOT PROVIDED                              
                                                                                
ARCA10   OC    WBCPAPA,WBCPAPA     IF APPLICATION AMOUNT IS PROVIDED            
         JZ    ARCA20                                                           
         CLI   WBCPAPC,0           ASSERT THAT APPLICATION CODE                 
         JNE   ARCA20              IS PROVIDED                                  
         MVI   TGBYTE3,D#PCAPC                                                  
         J     ARMIS                                                            
                                                                                
ARCA20   CLI   WBCPREC,0           IF REIMBURSED EXPENSE CODE IS                
         JNE   ARCA30              NOT PROVIDED                                 
         OC    WBCPREA,WBCPREA                                                  
         JZ    ARCA40                                                           
         MVI   TGBYTE3,D#PCREA     ASSERT THAT REIMBURSED EXPENSE               
         J     ARNAL               AMOUNT IS NOT PROVIDED                       
                                                                                
ARCA30   OC    WBCPREA,WBCPREA     IF REIMBURSED EXPENSE AMOUNT IS              
         JZ    ARCA40              PROVIDED                                     
         CLI   WBCPREC,0           ASSERT THAT REIMBURSED EXPENSE               
         JNE   ARCA40              CODE IS PROVIDED                             
         MVI   TGBYTE3,D#PCREC                                                  
         J     ARMIS                                                            
                                                                                
ARCA40   CLC   WBPYUSE,=C'PRT'     IF NOT MAKING A PRT                          
         JE    ARCA50                                                           
         CLC   WBPYUSE,=C'PRS'     OR PRS PAYMENT                               
         JE    ARCA50                                                           
         OC    WBCPPAM,WBCPPAM     AND PAYMENT AMOUNT IS PROVIDED               
         JZ    ARCA50                                                           
         OC    WBCPSPH,WBCPSPH     ASSERT THAT SUBJECT TO P&H AMOUNT            
         JNZ   ARCA50              IS PROVIDED                                  
         MVI   TGBYTE3,D#PCSPH                                                  
         J     ARMIS                                                            
                                                                                
ARCA50   CLI   WBCPRHR,24          IF REGULAR HOURS ARE 24                      
         JNE   ARCA60                                                           
         CLI   WBCPRMN,0           ASSERT THAT REGULAR MINUTES                  
         JE    ARCA60              ARE NOT ALLOWED                              
         MVI   TGBYTE3,D#PCRMN                                                  
         J     ARNAL                                                            
                                                                                
ARCA60   CLC   WBPYUSE,=C'BSR'     IF MAKING BSR PAYMENT                        
         JE    ARCA70                                                           
         CLC   WBPYUSE,=C'RRR'     OR RRR PAYMENT                               
         JE    ARCA70                                                           
         CLC   WBPYUSE,=C'ADO'     OR ADO PAYMENT                               
         JNE   YES                                                              
ARCA70   CLI   WBCPRSP,0           ASSERT THAT REGULAR SPOTS                    
         JNE   YES                                                              
         CLI   WBCPRTG,0           OR REGULAR TAGS                              
         JNE   YES                                                              
         CLI   WBCPRHR,0           OR REGULAR HOURS                             
         JNE   YES                                                              
         CLI   WBCPRMN,0           OR REGULAR MINUTES                           
         JNE   YES                                                              
         OC    WBCPAPA,WBCPAPA     OR APPLICATION AMOUNT                        
         JNZ   YES                                                              
         OC    WBCPAPA,WBCPREA     OR REIMBURSED EXPENSE AMOUNT                 
         JNZ   YES                                                              
         OC    WBCPPAM,WBCPPAM     OR PAYMENT AMOUNT                            
         JNZ   YES                                                              
         OC    WBCPSPH,WBCPSPH     OR SUBJECT TO P&H AMOUNT ARE                 
         JNZ   YES                 PROVIDED                                     
         MVI   TGBYTE3,D#PCRSP                                                  
         J     ARMIS                                                            
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED FIELDS HAVE BEEN PROVIDED       *         
*        FOR PAY UPLOAD AND AUTH/PO ESTIMATE/JOB VALIDATION UPLOAD    *         
***********************************************************************         
                                                                                
AR00     MVI   TGBYTE3,D#PYAGY     ASSERT THAT AGENCY IS PROVIDED               
         OC    WBPYAGY,WBPYAGY                                                  
         JZ    ARMIS                                                            
                                                                                
         OC    WBPYCVD,WBPYCVD     IF CAST VERIFICATION DATE IS                 
         JNZ   AR05                NOT PROVIDED                                 
         MVI   TGBYTE3,D#PYCLI     ASSERT THAT CLIENT IS PROVIDED               
         OC    WBPYCLI,WBPYCLI                                                  
         JZ    ARMIS                                                            
                                                                                
AR05     OC    WBPYSI1,WBPYSI1     IF SPLIT INVOICE 1 DETAILS                   
         JNZ   AR10                ARE NOT PROVIDED                             
         OC    WBPYSI2,WBPYSI2     ASSERT THAT SPLIT INVOICE 2                  
         JZ    AR10                DETAILS ARE NOT PROVIDED                     
         MVI   TGBYTE3,D#PYSI2                                                  
         J     ARNAL                                                            
                                                                                
AR10     OC    WBPYSI2,WBPYSI2     IF SPLIT INVOICE 2 DETAILS                   
         JNZ   AR20                ARE NOT PROVIDED                             
         OC    WBPYSI3,WBPYSI3     ASSERT THAT SPLIT INVOICE 3                  
         JZ    AR20                DETAILS ARE NOT PROVIDED                     
         MVI   TGBYTE3,D#PYSI3                                                  
         J     ARNAL                                                            
                                                                                
AR20     OC    WBPYSI3,WBPYSI3     IF SPLIT INVOICE 3 DETAILS                   
         JNZ   AR30                ARE NOT PROVIDED                             
         OC    WBPYSI4,WBPYSI4     ASSERT THAT SPLIT INVOICE 4                  
         JZ    AR30                DETAILS ARE NOT PROVIDED                     
         MVI   TGBYTE3,D#PYSI4                                                  
         J     ARNAL                                                            
                                                                                
AR30     OC    WBPYSI4,WBPYSI4     IF SPLIT INVOICE 4 DETAILS                   
         JNZ   AR40                ARE NOT PROVIDED                             
         OC    WBPYSI5,WBPYSI5     ASSERT THAT SPLIT INVOICE 5                  
         JZ    AR40                DETAILS ARE NOT PROVIDED                     
         MVI   TGBYTE3,D#PYSI5                                                  
         J     ARNAL                                                            
         J     YES                                                              
                                                                                
AR40     OC    WBPYSI5,WBPYSI5     IF SPLIT INVOICE 5 DETAILS                   
         JNZ   AR50                ARE NOT PROVIDED                             
         OC    WBPYSI6,WBPYSI6     ASSERT THAT SPLIT INVOICE 6                  
         JZ    AR50                DETAILS ARE NOT PROVIDED                     
         MVI   TGBYTE3,D#PYSI6                                                  
         J     ARNAL                                                            
                                                                                
AR50     OC    WBPYSI6,WBPYSI6     IF SPLIT INVOICE 6 DETAILS                   
         JNZ   AR60                ARE NOT PROVIDED                             
         OC    WBPYSI7,WBPYSI7     ASSERT THAT SPLIT INVOICE 7                  
         JZ    AR60                DETAILS ARE NOT PROVIDED                     
         MVI   TGBYTE3,D#PYSI7                                                  
         J     ARNAL                                                            
                                                                                
AR60     OC    WBPYSI7,WBPYSI7     IF SPLIT INVOICE 7 DETAILS                   
         JNZ   AR70                ARE NOT PROVIDED                             
         OC    WBPYSI8,WBPYSI8     ASSERT THAT SPLIT INVOICE 8                  
         JZ    AR70                DETAILS ARE NOT PROVIDED                     
         MVI   TGBYTE3,D#PYSI8                                                  
         J     ARNAL                                                            
                                                                                
AR70     OC    WBPYSI8,WBPYSI8     IF SPLIT INVOICE 8 DETAILS                   
         JNZ   AR80                ARE NOT PROVIDED                             
         OC    WBPYSI9,WBPYSI9     ASSERT THAT SPLIT INVOICE 9                  
         JZ    AR80                DETAILS ARE NOT PROVIDED                     
         MVI   TGBYTE3,D#PYSI9                                                  
         J     ARNAL                                                            
                                                                                
AR80     OC    WBPYSI9,WBPYSI9     IF SPLIT INVOICE 9 DETAILS                   
         JNZ   AR90                ARE NOT PROVIDED                             
         OC    WBPYSIA,WBPYSIA     ASSERT THAT SPLIT INVOICE 10                 
         JZ    AR90                DETAILS ARE NOT PROVIDED                     
         MVI   TGBYTE3,D#PYSIA                                                  
         J     ARNAL                                                            
                                                                                
AR90     CLC   WBACTIN,=AL2(I#PAYULD) FINISH UP ASSERTING REQUIRED FLD          
         JE    ARPY10                 RULES FOR PAY                             
         CLC   WBACTIN,=AL2(I#PJVULD) OR AUTH/PO EST/JOB VALIDATION             
         JE    ARPJ10                                                           
         DC    H'00'                                                            
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED FIELDS HAVE BEEN PROVIDED       *         
*        FOR PAY UPLOAD                                               *         
***********************************************************************         
                                                                                
ARPY10   MVI   TGBYTE3,D#PYSTF     ASSERT THAT STAFF CODE IS PROVIDED           
         OC    WBSTAFF,WBSTAFF                                                  
         JZ    ARMIS                                                            
                                                                                
         MVI   TGBYTE3,D#PYCID     ASSERT THAT COMMERCIAL ID IS                 
         OC    WBPYCID,WBPYCID     PROVIDED                                     
         JZ    ARMIS                                                            
                                                                                
         MVI   TGBYTE3,D#PYCOM     ASSERT THAT INTERNAL COMMERCIAL              
         OC    WBPYCOM,WBPYCOM     NUMBER IS PROVIDED                           
         JZ    ARMIS                                                            
                                                                                
         OC    WBPYCYC,WBPYCYC     IF CYCLE IS NOT PROVIDED                     
         JNZ   ARPY20                                                           
         CLI   TGUSEQU,0           AND WE KNOW THE USE                          
         JE    ARPY20                                                           
         TM    TGUSCYCK,OPTCYC     AND CYCLE IS REQUIRED                        
         JO    ARPY20                                                           
         TM    TGUSCYCK,OKAUTO     ASSERT THAT USE IS SET TO                    
         JO    ARPY20              AUTO-CALCULATE CYCLE                         
         MVI   TGBYTE3,D#PYCYC                                                  
         J     ARMIS                                                            
                                                                                
ARPY20   OC    WBPYEST,WBPYEST     IF ESTIMATE IS PROVIDED                      
         JZ    ARPY30                                                           
         OC    WBPYSI1,WBPYSI1     ASSERT THAT SPLIT INVOICE DETAILS            
         JZ    ARPY30              1 IS NOT PROVIDED                            
         MVI   TGBYTE3,D#PYSI1                                                  
         J     ARNAL                                                            
                                                                                
ARPY30   CLC   WBPYUSE,=C'DEM'     IF MAKING A DEMO                             
         JE    ARPY40                                                           
         CLC   WBPYUSE,=C'SNA'     OR SPANISH DEMO PAYMENT                      
         JNE   ARPY50                                                           
ARPY40   CLI   WBPYDEM,0           ASSERT THAT DEMOS ARE PROVIDED               
         JNE   ARPY50                                                           
         MVI   TGBYTE3,D#PYDEM                                                  
         J     ARMIS                                                            
                                                                                
ARPY50   CLC   WBPYUSE,=C'TAG'     IF MAKING A TAG PAYMENT                      
         JNE   ARPY60                                                           
         MVI   TGBYTE3,D#PYTAG                                                  
         CLI   WBPYTAG,0           ASSERT THAT TAGS ARE PROVIDED                
         JE    ARMIS                                                            
         MVI   TGBYTE3,D#PY1TS                                                  
         CLI   WBPY1TS,0           ASSERT THAT 1ST TAG AT SESSION               
         JE    ARMIS               RATE IS PROVIDED                             
                                                                                
ARPY60   CLC   WBPYUSE,=C'PRT'     IF MAKING A PRT                              
         JE    ARPY70                                                           
         CLC   WBPYUSE,=C'PRS'     OR PRS PAYMENT                               
         JNE   ARPY80                                                           
ARPY70   MVI   TGBYTE3,D#PYARE                                                  
         OC    WBPYARE,WBPYARE     ASSERT THAT PRINT AREA                       
         JZ    ARMIS                                                            
         OC    WBPYPRU,WBPYPRU     AND PRINT USE ARE PROVIDED                   
         JNZ   YES                                                              
         MVI   TGBYTE3,D#PYPRU                                                  
         J     ARMIS                                                            
                                                                                
ARPY80   CLC   WBPYUSE,=C'CLA'     IF MAKING A CLA PAYMENT                      
         JNE   ARPY90                                                           
         CLI   WBPYTYP,0           ASSERT THAT TYPE IS PROVIDED                 
         JNE   ARPY90                                                           
         MVI   TGBYTE3,D#PYTYP                                                  
         J     ARMIS                                                            
                                                                                
ARPY90   CLC   WBPYUSE,=C'LNA'     IF MAKING A LNA                              
         JE    ARPY100                                                          
         CLC   WBPYUSE,=C'LNC'     LNC                                          
         JE    ARPY100                                                          
         CLC   WBPYUSE,=C'LNN'     LNN                                          
         JE    ARPY100                                                          
         CLC   WBPYUSE,=C'PAX'     OR PAX PAYMENT                               
         JNE   ARPY110                                                          
ARPY100  CLI   WBPYTYP,0           ASSERT THAT TYPE IS NOT PROVIDED             
         JE    ARPY110                                                          
         MVI   TGBYTE3,D#PYTYP                                                  
         J     ARNAL                                                            
                                                                                
ARPY110  MVI   TGBYTE3,D#PYCVD                                                  
         OC    WBPYCVD,WBPYCVD     IF VERIFICATION DATE IS PROVIDED             
         JZ    ARPY120                                                          
         CLC   WBPYUSE,=C'PRT'     ASSERT THAT USE IS NOT PRT                   
         JE    ARNAL                                                            
         CLC   WBPYUSE,=C'PRS'     OR PRS                                       
         JE    ARNAL                                                            
         OC    WBPYCVT,WBPYCVT     AND THAT VERIFICATION TIME                   
         JNZ   ARPY130             IS PROVIDED                                  
         MVI   TGBYTE3,D#PYCVT                                                  
         J     ARMIS                                                            
                                                                                
ARPY120  CLC   =C'NP',WBWAPID      IF VERIFICATION DATE IS NOT PROVIDED         
         JE    ARMIS               ASSERT PAYMENT IS NOT FROM NABLE             
         MVI   TGBYTE3,D#PYCVT                                                  
         OC    WBPYCVT,WBPYCVT     AND THAT VERIFICATION TIME                   
         JNZ   ARNAL               IS NOT PROVIDED                              
                                                                                
ARPY130  MVI   TGBYTE3,D#W4WID     ASSERT THAT WEB APPLICATION ID               
         OC    WBWAPID,WBWAPID     IS PROVIDED                                  
         JZ    ARMIS                                                            
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED FIELDS HAVE BEEN PROVIDED       *         
*        FOR AUTH/PO ESTIMATE/JOB VALIDATION UPLOAD                   *         
***********************************************************************         
                                                                                
ARPJ10   J     YES                 NO FURTHER VALIDATION NECESSARY              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED FIELDS HAVE BEEN PROVIDED       *         
*        FOR TIMESHEET UPLOAD                                                   
***********************************************************************         
                                                                                
ARTM00   MVI   TGBYTE3,D#TMSTF     ASSERT THAT STAFF CODE IS PROVIDED           
         OC    WBSTAFF,WBSTAFF                                                  
         JZ    ARMIS                                                            
                                                                                
         MVI   TGBYTE3,D#TMCOM                                                  
         CLI   WBMODE,WBMDEXE      IF MODE IS EXECUTE                           
         JNE   ARTM10                                                           
         OC    WBTMCOM,WBTMCOM     ASSERT THAT INTERNAL COMMERCIAL              
         JZ    ARMIS               NUMBER IS PROVIDED                           
                                                                                
ARTM10   XR    R1,R1                                                            
         OC    WBTMCOM,WBTMCOM     ASSERT THAT ONE (AND ONLY ONE)               
         JZ    *+8                 OF INTERNAL COMMERCIAL NUMBER                
         AHI   R1,1                AND HYPO CATEGORY ARE PROVIDED               
         OC    WBTMCAT,WBTMCAT                                                  
         JZ    *+8                                                              
         AHI   R1,1                                                             
         CHI   R1,1                                                             
         JE    ARTM20                                                           
         JL    ARMIS                                                            
         MVI   TGBYTE3,D#TMCAT                                                  
         J     ARNAL                                                            
                                                                                
ARTM20   OC    WBTMCOM,WBTMCOM     IF INTERNAL COMMERCIAL NUMBER                
         JZ    ARTM30              IS PROVIDED                                  
         MVI   TGBYTE3,D#TMINV                                                  
         OC    WBTMINV,WBTMINV     ASSERT THAT INVOICE NUMBER                   
         JZ    ARMIS               IS PROVIDED                                  
         MVI   TGBYTE3,D#TMSEQ                                                  
         OC    WBTMSEQ,WBTMSEQ     ASSERT THAT CAST SEQUENCE NUMBER             
         JZ    ARMIS               IS PROVIDED                                  
         MVI   TGBYTE3,D#TMHTY                                                  
         CLI   WBTMHTY,0           ASSERT THAT HYPO COMMERCIAL TYPE             
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   TGBYTE3,D#TMHAS                                                  
         OC    WBTMHAS,WBTMHAS     ASSERT THAT HYPO ADDENDUM STATE              
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   TGBYTE3,D#TMHAT                                                  
         CLI   WBTMHAT,0           ASSERT THAT HYPO ACTRA TYPE                  
         JNE   ARNAL               IS NOT PROVIDED                              
         MVI   TGBYTE3,D#TMCAT                                                  
         OC    WBTMCAT,WBTMCAT     ASSERT THAT HYPO CATEGORY                    
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   TGBYTE3,D#TMUNI                                                  
         OC    WBTMUNI,WBTMUNI     ASSERT THAT HYPO UNION                       
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   TGBYTE3,D#TMONO                                                  
         OC    WBTMONO,WBTMONO     ASSERT THAT HYPO ON/OFF CAMERA               
         JNZ   ARNAL               IS NOT PROVIDED                              
         OC    WBTMDOB,WBTMDOB                                                  
         JZ    ARTM40              ASSERT THAT HYPO DATE OF BIRTH               
         MVI   TGBYTE3,D#TMDOB     IS NOT PROVIDED                              
         J     ARNAL                                                            
*                                  IF INTERNAL COMMERCIAL NUMBER                
ARTM30   MVI   TGBYTE3,D#TMINV     IS NOT PROVIDED                              
         OC    WBTMINV,WBTMINV     ASSERT THAT INVOICE NUMBER                   
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   TGBYTE3,D#TMSEQ                                                  
         OC    WBTMSEQ,WBTMSEQ     ASSERT THAT CAST SEQUENCE NUMBER             
         JNZ   ARNAL               IS NOT PROVIDED                              
         MVI   TGBYTE3,D#TMCAT                                                  
         OC    WBTMCAT,WBTMCAT     ASSERT THAT HYPO CATEGORY                    
         JZ    ARMIS               IS PROVIDED                                  
         MVI   TGBYTE3,D#TMUNI                                                  
         OC    WBTMUNI,WBTMUNI     ASSERT THAT HYPO UNION                       
         JZ    ARMIS               IS PROVIDED                                  
         MVI   TGBYTE3,D#TMONO                                                  
         OC    WBTMONO,WBTMONO     ASSERT THAT HYPO ON/OFF CAMERA               
         JZ    ARMIS               IS PROVIDED                                  
                                                                                
ARTM40   MVI   TGBYTE3,D#TMDAT     ASSERT THAT DATE IS PROVIDED                 
         OC    WBTMDAT,WBTMDAT                                                  
         JZ    ARMIS                                                            
                                                                                
         MVI   TGBYTE3,D#TMWET                                                  
         OC    WBTMWST,WBTMWST     IF WORK START TIME IS PROVIDED               
         JZ    ARTM50                                                           
         OC    WBTMWET,WBTMWET     ASSERT THAT WORK END TIME IS                 
         JNZ   ARTM60              PROVIDED                                     
         J     ARMIS                                                            
*                                  IF WORK START TIME IS NOT PROVIDED           
ARTM50   OC    WBTMWET,WBTMWET     ASSERT THAT WORK END TIME IS                 
         JNZ   ARNAL               NOT PROVIDED                                 
                                                                                
ARTM60   MVI   TGBYTE3,D#TMM1E                                                  
         OC    WBTMM1S,WBTMM1S     IF MEAL 1 START TIME IS PROVIDED             
         JZ    ARTM70                                                           
         OC    WBTMM1E,WBTMM1E     ASSERT THAT MEAL 1 END TIME IS               
         JNZ   ARTM80              PROVIDED                                     
         J     ARMIS                                                            
*                                  IF MEAL 1 START TIME IS NOT PROVIDED         
ARTM70   OC    WBTMM1E,WBTMM1E     ASSERT THAT MEAL 1 END TIME IS               
         JNZ   ARNAL               NOT PROVIDED                                 
                                                                                
ARTM80   OC    WBTMM2S,WBTMM2S     IF MEAL 2 START TIME IS PROVIDED             
         JZ    ARTM90                                                           
         MVI   TGBYTE3,D#TMM2S                                                  
         OC    WBTMM1S,WBTMM1S     ASSERT THAT MEAL 1 START TIME                
         JZ    ARNAL               IS PROVIDED                                  
         OC    WBTMM2E,WBTMM2E     ASSERT THAT MEAL 2 END TIME IS               
         JNZ   ARTM100             PROVIDED                                     
         MVI   TGBYTE3,D#TMM2E                                                  
         J     ARMIS                                                            
                                                                                
ARTM90   MVI   TGBYTE3,D#TMM2E     IF MEAL 2 START TIME IS NOT PROVIDED         
         OC    WBTMM2E,WBTMM2E     ASSERT THAT MEAL 2 END TIME IS               
         JNZ   ARNAL               NOT PROVIDED                                 
                                                                                
ARTM100  OC    WBTMM3S,WBTMM3S     IF MEAL 3 START TIME IS PROVIDED             
         JZ    ARTM110                                                          
         MVI   TGBYTE3,D#TMM3S                                                  
         OC    WBTMM2S,WBTMM2S     ASSERT THAT MEAL 2 START TIME                
         JZ    ARNAL               IS PROVIDED                                  
         OC    WBTMM3E,WBTMM3E     ASSERT THAT MEAL 3 END TIME IS               
         JNZ   ARTM120             PROVIDED                                     
         MVI   TGBYTE3,D#TMM3E                                                  
         J     ARMIS                                                            
                                                                                
ARTM110  MVI   TGBYTE3,D#TMM3E     IF MEAL 3 START TIME IS NOT PROVIDED         
         OC    WBTMM3E,WBTMM3E     ASSERT THAT MEAL 3 END TIME IS               
         JNZ   ARNAL               NOT PROVIDED                                 
                                                                                
ARTM120  MVI   TGBYTE3,D#TMPDS                                                  
         OC    WBTMPDD,WBTMPDD     IF PRIOR DAY WARDROBE DATE IS                
         JZ    ARTM130             PROVIDED                                     
         OC    WBTMPDS,WBTMPDS     ASSERT THAT PRIOR DAY WARDROBE               
         JZ    ARMIS               START TIME                                   
         OC    WBTMPDE,WBTMPDE     AND PRIOR DAY WARDROBE END TIME              
         JNZ   ARTM140             ARE PROVIDED                                 
         MVI   TGBYTE3,D#TMPDE                                                  
         J     ARMIS                                                            
                                                                                
ARTM130  OC    WBTMPDS,WBTMPDS     IF PDW DATE IS NOT PROVIDED                  
         JNZ   ARNAL               ASSERT THAT PRIOR DAY WARDROBE               
         MVI   TGBYTE3,D#TMPDE     START TIME                                   
         OC    WBTMPDE,WBTMPDE     AND PRIOR DAY WARDROBE END TIME              
         JNZ   ARNAL               IS NOT PROVIDED                              
                                                                                
ARTM140  MVI   TGBYTE3,D#TMTTA                                                  
         OC    WBTMTTD,WBTMTTD     IF TRAVEL TO DEPART TIME IS PROVIDED         
         JZ    ARTM150                                                          
         OC    WBTMTTA,WBTMTTA     ASSERT THAT TRAVEL TO ARRIVE TIME            
         JNZ   ARTM160             IS PROVIDED                                  
         J     ARMIS                                                            
*                                  IF TRAVEL TO DEPART TIME IS NOT PROV         
ARTM150  OC    WBTMTTA,WBTMTTA     ASSERT THAT TRAVEL TO ARRIVE TIME IS         
         JNZ   ARNAL               NOT PROVIDED                                 
                                                                                
ARTM160  MVI   TGBYTE3,D#TMTFA                                                  
         OC    WBTMTFD,WBTMTFD     IF TRAVEL FROM DEPART TIME IS                
         JZ    ARTM170             PROVIDED                                     
         OC    WBTMTFA,WBTMTFA     ASSERT THAT TRAVEL FROM ARRIVE TIME          
         JNZ   ARTM180             IS PROVIDED                                  
         J     ARMIS                                                            
*                                  IF TRAVEL FROM DEPART TIME IS NOT            
ARTM170  OC    WBTMTFA,WBTMTFA     PROVIDED ASSERT THAT TRAVEL FROM             
         JNZ   ARNAL               ARRIVE TIME IS NOT PROVIDED                  
                                                                                
ARTM180  MVI   TGBYTE3,D#TMTIA                                                  
         OC    WBTMTID,WBTMTID     IF TRAVEL INTV DEPART TIME IS                
         JZ    ARTM190             PROVIDED                                     
         OC    WBTMTIA,WBTMTIA     ASSERT THAT TRAVEL INTV ARRIVE TIME          
         JNZ   ARTM200             IS PROVIDED                                  
         J     ARMIS                                                            
*                                  IF TRAVEL INTV DEPART TIME IS NOT            
ARTM190  OC    WBTMTIA,WBTMTIA     PROVIDED ASSERT THAT TRAVEL INTV             
         JNZ   ARNAL               ARRIVE TIME IS NOT PROVIDED                  
                                                                                
ARTM200  OC    WBTMCOM,WBTMCOM     IF INTERNAL COMMERCIAL NUMBER IS             
         JZ    YES                 PROVIDED                                     
         MVI   TGBYTE3,D#W4WID     ASSERT THAT WEB APPLICATION ID               
         OC    WBWAPID,WBWAPID     IS PROVIDED                                  
         JZ    ARMIS                                                            
         J     YES                                                              
                                                                                
***********************************************************************         
                                                                                
ARMIS    GOTOR ADDGERR,DMCB,('EENMIS',0),(TGBYTE3,0)                            
         J     NO                                                               
                                                                                
ARNAL    GOTOR ADDGERR,DMCB,('EENNAL',0),(TGBYTE3,0)                            
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ASSERTS ALL PROVIDED VALUES ARE VALID                *         
***********************************************************************         
                                                                                
ASRTVAL  NTR1  BASE=*,LABEL=*                                                   
         TM    TGFASTAT,TGCRNOPY   SKIP IF PAYMENT COMING FROM CERNO            
         JO    YES                                                              
         CLC   WBACTIN,=AL2(I#PJVULD)                                           
         JE    AVPJ10                                                           
                                                                                
         CLI   WBMODE,WBMDRTV      ASSERT THAT MODE IS RETRIEVE                 
         JE    AV10                                                             
         CLI   WBMODE,WBMDVFY      VERIFY                                       
         JE    AV10                                                             
         CLI   WBMODE,WBMDEXE      OR EXECUTE                                   
         JE    AV10                                                             
         MVI   TGBYTE3,D#PYMOD                                                  
         J     AVINV                                                            
                                                                                
AV10     CLC   WBACTIN,=AL2(I#TMULD)  IF TIMESHEET UPLOAD                       
         JNE   AV15                                                             
         OC    WBTMCOM,WBTMCOM                                                  
         JNZ   AVPY40                 SKIP AHEAD                                
         J     AVTM10                                                           
                                                                                
AV15     CLI   TGUSEQU,0           IF A PRIOR RECORD HAS ALREADY                
         JE    AV20                DEFINED THE USE                              
         CLC   WBPYUSE,TGUSCDE     ASSERT THAT THIS USE MATCHES                 
         JE    AV20                                                             
         MVI   TGBYTE3,D#PYUSE                                                  
         J     AVINV                                                            
                                                                                
AV20     MVI   TGBYTE3,D#PYUSE                                                  
         GOTO1 USEVAL,DMCB,(X'40',WBPYUSE)                                      
         JNE   AVINV                                                            
         TM    TGUSSTA4,WEBPAYBL                                                
         JZ    AVINV                                                            
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL PROVIDED VALUES ARE VALID FOR PAY UPLOAD *         
***********************************************************************         
                                                                                
         OC    WBCPSEQ,WBCPSEQ                                                  
         JNZ   AVCA10                                                           
                                                                                
         GOTOR AVSINDUP,DMCB,WBPYSI1                                            
         JNE   NO                                                               
         GOTOR AVSINDUP,DMCB,WBPYSI2                                            
         JNE   NO                                                               
         GOTOR AVSINDUP,DMCB,WBPYSI3                                            
         JNE   NO                                                               
         GOTOR AVSINDUP,DMCB,WBPYSI4                                            
         JNE   NO                                                               
         GOTOR AVSINDUP,DMCB,WBPYSI5                                            
         JNE   NO                                                               
         GOTOR AVSINDUP,DMCB,WBPYSI6                                            
         JNE   NO                                                               
         GOTOR AVSINDUP,DMCB,WBPYSI7                                            
         JNE   NO                                                               
         GOTOR AVSINDUP,DMCB,WBPYSI8                                            
         JNE   NO                                                               
         GOTOR AVSINDUP,DMCB,WBPYSI9                                            
         JNE   NO                                                               
         GOTOR AVSINDUP,DMCB,WBPYSIA                                            
         JNE   NO                                                               
                                                                                
         GOTO1 AVSINVAL,DMCB,WBPYSI1                                            
         JNE   AVISI1                                                           
         GOTO1 (RF),(R1),WBPYSI2                                                
         JNE   AVISI2                                                           
         GOTO1 (RF),(R1),WBPYSI3                                                
         JNE   AVISI3                                                           
         GOTO1 (RF),(R1),WBPYSI4                                                
         JNE   AVISI4                                                           
         GOTO1 (RF),(R1),WBPYSI5                                                
         JNE   AVISI5                                                           
         GOTO1 (RF),(R1),WBPYSI6                                                
         JNE   AVISI6                                                           
         GOTO1 (RF),(R1),WBPYSI7                                                
         JNE   AVISI7                                                           
         GOTO1 (RF),(R1),WBPYSI8                                                
         JNE   AVISI8                                                           
         GOTO1 (RF),(R1),WBPYSI9                                                
         JNE   AVISI9                                                           
         GOTO1 (RF),(R1),WBPYSIA                                                
         JNE   AVISIA                                                           
                                                                                
         CLI   WBPYBRK,0           ASSERT THAT BREAKDOWN OPTIONS                
         JE    AVPY20              ARE VALID                                    
         CLI   WBPYBRK,X'80'                                                    
         JE    AVPY20                                                           
         MVI   TGBYTE3,D#PYBRK                                                  
         J     AVINV                                                            
                                                                                
AVPY20   CLC   WBPYUSE,=C'CLA'     IF MAKING CLA PAYMENT                        
         JNE   AVPY30                                                           
         CLI   WBPYTYP,WBPTCLAG    ASSERT THAT TYPE IS VALID                    
         JNH   AVPY30                                                           
         MVI   TGBYTE3,D#PYTYP                                                  
         J     AVINV                                                            
                                                                                
AVPY30   CLC   WBPYUSE,=C'PRT'     IF NOT MAKING PRT                            
         JE    AVPY50                                                           
         CLC   WBPYUSE,=C'PRS'     OR PRS PAYMENT                               
         JE    AVPY50                                                           
         CLC   =C'VS',WBWAPID      ASSERT THAT WEB APPLICATION ID               
         JE    AVPY50              IS VALID                                     
         CLC   =C'RS',WBWAPID                                                   
         JE    AVPY50                                                           
         CLC   =C'VC',WBWAPID                                                   
         JE    AVPY50                                                           
         CLC   =C'NP',WBWAPID                                                   
         JE    AVPY50                                                           
         CLC   =C'RC',WBWAPID                                                   
         JE    AVPY50                                                           
         CLC   =C'TC',WBWAPID                                                   
         JE    AVPY50                                                           
AVPY40   CLC   =C'TS',WBWAPID                                                   
         JE    AVPY50                                                           
         MVI   TGBYTE3,D#W4WID                                                  
         J     AVINV                                                            
                                                                                
AVPY50   CLC   WBACTIN,=AL2(I#TMULD)  IF TIMESHEET UPLOAD                       
         JE    AVTM10                 SKIP AHEAD                                
                                                                                
         CLI   WBPY1TS,0           ASSERT THAT FIRST TAG AT SESSION             
         JE    AVPY60              RATE? IS Y OR N                              
         CLI   WBPY1TS,C'Y'                                                     
         JE    AVPY60                                                           
         CLI   WBPY1TS,C'N'                                                     
         JE    AVPY60                                                           
         MVI   TGBYTE3,D#PY1TS                                                  
         J     AVINV                                                            
                                                                                
AVPY60   CLI   WBPYAAP,0           ASSERT THAT AUTO-APPROVE? IS                 
         JE    YES                 Y OR N                                       
         CLI   WBPYAAP,C'Y'                                                     
         JE    YES                                                              
         CLI   WBPYAAP,C'N'                                                     
         JE    YES                                                              
         MVI   TGBYTE3,D#PYAAP                                                  
         J     AVINV                                                            
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL PROVIDED VALUES ARE VALID FOR PAY        *         
*        CAST UPLOAD                                                  *         
***********************************************************************         
                                                                                
AVCA10   L     R2,=F'2100000000'                                                
                                                                                
         CLI   WBCPAPC,0                                                        
         JE    AVCA20                                                           
         MVI   TGBYTE3,D#PCAPC     ASSERT THAT APPLY CODE IS NOT                
         CLI   WBCPAPC,C'1'        GREATER THAN 9                               
         JL    AVINV                                                            
         CLI   WBCPAPC,C'9'                                                     
         JH    AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#PCAPA     ASSERT THAT APPLICATION AMOUNT               
         ICM   RE,15,WBCPAPA       DOES NOT EXCEED 21M                          
         CR    RE,R2                                                            
         JH    AVINV                                                            
                                                                                
AVCA20   CLI   WBCPREC,0           ASSERT THAT REIMBURSED EXPENSE CODE          
         JE    AVCA30              IS NOT GREATER THAN 9                        
         MVI   TGBYTE3,D#PCREC                                                  
         CLI   WBCPREC,C'1'                                                     
         JL    AVINV                                                            
         CLI   WBCPREC,C'9'                                                     
         JH    AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#PCREA     ASSERT THAT REIMBURSED EXPENSE AMT           
         ICM   RE,15,WBCPREA       DOES NOT EXCEED 21M                          
         CR    RE,R2                                                            
         JH    AVINV                                                            
                                                                                
AVCA30   OC    WBCPOPT,WBCPOPT     IF PERFORMER LEVEL OPTIONS ARE               
         JZ    AVCA40              PROVIDED                                     
         CLC   WBCPOPT,=C'20000000'                                             
         JE    AVCA40              ASSERT THAT ONLY VALID INPUT IS              
         MVI   TGBYTE3,D#PCCOP     2 (G=E)                                      
         J     AVINV                                                            
                                                                                
AVCA40   MVI   TGBYTE3,D#PCPAM     ASSERT THAT PAYMENT AMOUNT                   
         ICM   RE,15,WBCPPAM       DOES NOT EXCEED 21M                          
         CR    RE,R2                                                            
         JH    AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#PCSPH     ASSERT THAT SUBJECT TO P&H AMT               
         ICM   RE,15,WBCPSPH       DOES NOT EXCEED 21M                          
         CR    RE,R2                                                            
         JH    AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#PCMDE     ASSERT THAT MISC DEDUCTION                   
         ICM   RE,15,WBCPMDE       DOES NOT EXCEED 21M                          
         CR    RE,R2                                                            
         JH    AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#PCADJ     ASSERT THAT ADJUSTMENT AMOUNT                
         ICM   RE,15,WBCPADJ       SUBJECT TO P&H DOES NOT EXCEED               
         CR    RE,R2               21M                                          
         JH    AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#PCADN     ASSERT THAT ADJUSTMENT AMOUNT                
         ICM   RE,15,WBCPADN       NOT SUBJECT TO P&H DOES NOT EXCEED           
         CR    RE,R2               21M                                          
         JH    AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#PCRSP     ASSERT THAT REGULAR SPOTS                    
         CLI   WBCPRSP,99          IS NOT GREATER THAN 99                       
         JH    AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#PCRDY     ASSERT THAT REGULAR DAYS                     
         CLI   WBCPRDY,99          IS NOT GREATER THAN 99                       
         JH    AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#PCRHR     ASSERT THAT REGULAR HOURS                    
         CLI   WBCPRHR,24          IS NOT GREATER THAN 24                       
         JH    AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#PCRMN     ASSERT THAT REGULAR MINUTES                  
         CLI   WBCPRMN,59          IS NOT GREATER THAN 59                       
         JH    AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#PCRTH     ASSERT THAT REGULAR TRAVEL TIME              
         CLI   WBCPRTH,99          HOURS IS NOT GREATER THAN 99                 
         JH    AVINV                                                            
                                                                                
         CLI   WBCPRTM,0           ASSERT THAT REGULAR TRAVEL TIME              
         JE    AVCA50              MINUTES IS 0                                 
         CLI   WBCPRTM,15          OR 15                                        
         JE    AVCA50                                                           
         CLI   WBCPRTM,30          OR 30                                        
         JE    AVCA50                                                           
         CLI   WBCPRTM,45          OR 45                                        
         JE    AVCA50                                                           
         MVI   TGBYTE3,D#PCRTM                                                  
         J     AVINV                                                            
                                                                                
AVCA50   MVI   TGBYTE3,D#PCRTG     ASSERT THAT REGULAR TAGS                     
         CLI   WBCPRTG,99          IS NOT GREATER THAN 99                       
         JH    AVINV                                                            
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL PROVIDED VALUES ARE VALID FOR AUTH/PO    *         
*        ESTIMATE/JOB VALIDATION UPLOAD                               *         
***********************************************************************         
                                                                                
AVPJ10   CLI   WBPYSKV,0           IS SPOTS ARE PROVIDED                        
         JE    YES                 ASSERT THAT SPOTS ARE 1-99                   
         CLI   WBPYSKV,WBPYSKVP                                                 
         JE    YES                                                              
         CLI   WBPYSKV,WBPYSKVJ                                                 
         JE    YES                                                              
         CLI   WBPYSKV,WBPYSKVP+WBPYSKVJ                                        
         JNE   AVINV                                                            
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL PROVIDED VALUES ARE VALID FOR TIMESHEET  *         
*        UPLOAD                                                       *         
***********************************************************************         
                                                                                
AVTM10   OC    WBTMSPT,WBTMSPT     ASSERT SPOTS IS A VALID VALUE                
         JZ    AVTM20                                                           
         MVI   TGBYTE3,D#TMSPT                                                  
         GOTO1 AVNUMN0,DMCB,WBTMSPT                                             
         JNE   AVINV                                                            
         GOTO1 AVNUMNR,DMCB,WBTMSPT+1                                           
         JNE   AVINV                                                            
                                                                                
AVTM20   OC    WBTMTAG,WBTMTAG     ASSERT TAGS IS A VALID VALUE                 
         JZ    AVTM30                                                           
         MVI   TGBYTE3,D#TMTAG                                                  
         GOTO1 AVNUMN0,DMCB,WBTMTAG                                             
         JNE   AVINV                                                            
         GOTO1 AVNUMNR,DMCB,WBTMTAG+1                                           
         JNE   AVINV                                                            
                                                                                
AVTM30   OC    WBTMWST,WBTMWST     IF WORK START/END TIMES ARE                  
         JZ    AVTM40              PROVIDED, ASSERT THEY ARE VALID              
         MVI   TGBYTE3,D#TMWST                                                  
         GOTO1 TIMVALL,DMCB,(L'WBTMWST,WBTMWST),DUB                             
         CLI   DMCB,X'FF'                                                       
         JE    AVINV                                                            
         MVI   TGBYTE3,D#TMWET                                                  
         GOTO1 TIMVALL,DMCB,(L'WBTMWET,WBTMWET),DUB                             
         CLI   DMCB,X'FF'                                                       
         JE    AVINV                                                            
                                                                                
AVTM40   MVI   TGBYTE3,D#TMNDB            ASSERT THAT NON-DEDUCTIBLE            
         GOTO1 VALYORN,DMCB,WBTMNDB       BREAKFAST IS A VALID VALUE            
         JNE   AVINV                                                            
                                                                                
         OC    WBTMM1S,WBTMM1S     IF MEAL 1 START/END TIMES ARE                
         JZ    AVTM50              PROVIDED, ASSERT THEY ARE VALID              
         MVI   TGBYTE3,D#TMM1S                                                  
         GOTO1 TIMVALL,DMCB,(L'WBTMM1S,WBTMM1S),DUB                             
         CLI   DMCB,X'FF'                                                       
         JE    AVINV                                                            
         MVI   TGBYTE3,D#TMM1E                                                  
         GOTO1 TIMVALL,DMCB,(L'WBTMM1E,WBTMM1E),DUB                             
         CLI   DMCB,X'FF'                                                       
         JE    AVINV                                                            
                                                                                
AVTM50   OC    WBTMM2S,WBTMM2S     IF MEAL 2 START/END TIMES ARE                
         JZ    AVTM60              PROVIDED, ASSERT THEY ARE VALID              
         MVI   TGBYTE3,D#TMM2S                                                  
         GOTO1 TIMVALL,DMCB,(L'WBTMM2S,WBTMM2S),DUB                             
         CLI   DMCB,X'FF'                                                       
         JE    AVINV                                                            
         MVI   TGBYTE3,D#TMM2E                                                  
         GOTO1 TIMVALL,DMCB,(L'WBTMM2E,WBTMM2E),DUB                             
         CLI   DMCB,X'FF'                                                       
         JE    AVINV                                                            
                                                                                
AVTM60   OC    WBTMM3S,WBTMM3S     IF MEAL 3 START/END TIMES ARE                
         JZ    AVTM70              PROVIDED, ASSERT THEY ARE VALID              
         MVI   TGBYTE3,D#TMM3S                                                  
         GOTO1 TIMVALL,DMCB,(L'WBTMM3S,WBTMM3S),DUB                             
         CLI   DMCB,X'FF'                                                       
         JE    AVINV                                                            
         MVI   TGBYTE3,D#TMM3E                                                  
         GOTO1 TIMVALL,DMCB,(L'WBTMM3E,WBTMM3E),DUB                             
         CLI   DMCB,X'FF'                                                       
         JE    AVINV                                                            
                                                                                
AVTM70   OC    WBTMPDS,WBTMPDS     IF PDW START/END TIMES ARE                   
         JZ    AVTM80              PROVIDED, ASSERT THEY ARE VALID              
         MVI   TGBYTE3,D#TMPDS                                                  
         GOTO1 TIMVALL,DMCB,(L'WBTMPDS,WBTMPDS),DUB                             
         CLI   DMCB,X'FF'                                                       
         JE    AVINV                                                            
         MVI   TGBYTE3,D#TMPDE                                                  
         GOTO1 TIMVALL,DMCB,(L'WBTMPDE,WBTMPDE),DUB                             
         CLI   DMCB,X'FF'                                                       
         JE    AVINV                                                            
                                                                                
AVTM80   MVI   TGBYTE3,D#TMNPR            ASSERT THAT NIGHT PREMIUM             
         GOTO1 VALYORN,DMCB,WBTMNPR       IS A VALID VALUE                      
         JNE   AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#TMMP1            ASSERT THAT MEAL PENALTY 1            
         GOTO1 VALYORN,DMCB,WBTMMP1       IS A VALID VALUE                      
         JNE   AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#TMMP2            ASSERT THAT MEAL PENALTY 2            
         GOTO1 VALYORN,DMCB,WBTMMP2       IS A VALID VALUE                      
         JNE   AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#TMMP3            ASSERT THAT MEAL PENALTY 3            
         GOTO1 VALYORN,DMCB,WBTMMP3       IS A VALID VALUE                      
         JNE   AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#TMSMK            ASSERT THAT SMOKE PAY                 
         GOTO1 VALYORN,DMCB,WBTMSMK       IS A VALID VALUE                      
         JNE   AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#TM16H            ASSERT THAT 16 HOUR RULE              
         GOTO1 VALYORN,DMCB,WBTM16H       IS A VALID VALUE                      
         JNE   AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#TMREH            ASSERT THAT REHEARSAL                 
         GOTO1 VALYORN,DMCB,WBTMREH       IS A VALID VALUE                      
         JNE   AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#TMWCX            ASSERT THAT WEATHER CXL               
         GOTO1 VALYORN,DMCB,WBTMWCX       IS A VALID VALUE                      
         JNE   AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#TMRPV            ASSERT THAT REST PERIOD               
         GOTO1 VALYORN,DMCB,WBTMRPV       VIOLATION IS A VALID VALUE            
         JNE   AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#TMNCD            ASSERT THAT NON CONSECUTIVE           
         GOTO1 VALYORN,DMCB,WBTMNCD       DAY IS A VALID VALUE                  
         JNE   AVINV                                                            
                                                                                
         MVI   TGBYTE3,D#TMTDL            ASSERT THAT TRAVEL TO DIST            
         GOTO1 VALYORN,DMCB,WBTMTDL       LOCATION IS A VALID VALUE             
         JNE   AVINV                                                            
                                                                                
         OC    WBTMTTD,WBTMTTD     IF TRAVEL TO DEPART/ARRIVE TIMES             
         JZ    AVTM90              ARE PROVIDED, ASSERT THEY ARE VALID          
         MVI   TGBYTE3,D#TMTTD                                                  
         GOTO1 TIMVALL,DMCB,(L'WBTMTTD,WBTMTTD),DUB                             
         CLI   DMCB,X'FF'                                                       
         JE    AVINV                                                            
         MVI   TGBYTE3,D#TMTTA                                                  
         GOTO1 TIMVALL,DMCB,(L'WBTMTTA,WBTMTTA),DUB                             
         CLI   DMCB,X'FF'                                                       
         JE    AVINV                                                            
                                                                                
AVTM90   OC    WBTMTFD,WBTMTFD     IF TRAVEL FROM DEPART/ARRIVE TIMES           
         JZ    AVTM100             ARE PROVIDED, ASSERT THEY ARE VALID          
         MVI   TGBYTE3,D#TMTFD                                                  
         GOTO1 TIMVALL,DMCB,(L'WBTMTFD,WBTMTFD),DUB                             
         CLI   DMCB,X'FF'                                                       
         JE    AVINV                                                            
         MVI   TGBYTE3,D#TMTFA                                                  
         GOTO1 TIMVALL,DMCB,(L'WBTMTFA,WBTMTFA),DUB                             
         CLI   DMCB,X'FF'                                                       
         JE    AVINV                                                            
                                                                                
AVTM100  OC    WBTMTID,WBTMTID     IF TRAVEL INTV DEPART/ARRIVE TIMES           
         JZ    AVTM110             ARE PROVIDED, ASSERT THEY ARE VALID          
         MVI   TGBYTE3,D#TMTID                                                  
         GOTO1 TIMVALL,DMCB,(L'WBTMTID,WBTMTID),DUB                             
         CLI   DMCB,X'FF'                                                       
         JE    AVINV                                                            
         MVI   TGBYTE3,D#TMTIA                                                  
         GOTO1 TIMVALL,DMCB,(L'WBTMTIA,WBTMTIA),DUB                             
         CLI   DMCB,X'FF'                                                       
         JE    AVINV                                                            
                                                                                
AVTM110  OC    WBTMNWA,WBTMNWA     ASSERT NON EVENING WARDROBE                  
         JZ    AVTM120             ALLOTMENTS IS A VALID VALUE                  
         MVI   TGBYTE3,D#TMNWA                                                  
         GOTO1 AVNUMN0,DMCB,WBTMNWA                                             
         JNE   AVINV                                                            
         GOTO1 AVNUMNR,DMCB,WBTMNWA+1                                           
         JNE   AVINV                                                            
                                                                                
AVTM120  OC    WBTMEWA,WBTMEWA     ASSERT EVENING WARDROBE                      
         JZ    AVTM130             ALLOTMENTS IS A VALID VALUE                  
         MVI   TGBYTE3,D#TMEWA                                                  
         GOTO1 AVNUMN0,DMCB,WBTMEWA                                             
         JNE   AVINV                                                            
         GOTO1 AVNUMNR,DMCB,WBTMEWA+1                                           
         JNE   AVINV                                                            
                                                                                
AVTM130  MVI   TGBYTE3,D#TMFDY     ASSERT THAT FINAL DAY IS A                   
         GOTO1 VALYORN,DMCB,WBTMFDY               VALID VALUE                   
         JNE   AVINV                                                            
                                                                                
         CLI   WBTMHTY,0           ASSERT THAT HYPO COMMERCIAL                  
         JE    AVTM140             TYPE IS A VALID VALUE                        
         MVI   TGBYTE3,D#TMHTY                                                  
         GOTO1 CTYPVAL,DMCB,WBTMHTY                                             
         JNE   NO                                                               
                                                                                
AVTM140  OC    WBTMHAS,WBTMHAS     ASSERT THAT HYPO ADDENDUM STATE              
         JZ    AVTM150             IS VALID                                     
         MVI   TGBYTE3,D#TMHAS                                                  
         GOTO1 TAXVAL,DMCB,(3,WBTMHAS)                                          
         JNE   AVINV                                                            
         TM    TGTASTAT,TALUOKAD                                                
         JZ    AVINV                                                            
                                                                                
AVTM150  CLI   WBTMHAT,0           ASSERT THAT HYPO ACTRA TYPE                  
         JE    AVTM160             IS VALID                                     
         MVI   TGBYTE3,D#TMHAT                                                  
         GOTO1 CCTYPVAL,DMCB,(X'80',WBTMHAT)                                    
         JNE   AVINV                                                            
                                                                                
AVTM160  OC    WBTMCAT,WBTMCAT     ASSERT THAT HYPO CATEGORY                    
         JZ    AVTM170             IS VALID                                     
         MVI   TGBYTE3,D#TMCAT                                                  
         GOTO1 CATVAL,DMCB,WBTMCAT                                              
         JNE   AVINV                                                            
                                                                                
AVTM170  OC    WBTMUNI,WBTMUNI     ASSERT THAT HYPO UNION                       
         JZ    AVTM180             IS VALID                                     
         MVI   TGBYTE3,D#TMUNI                                                  
         GOTO1 UNIVAL,DMCB,WBTMUNI                                              
         JNE   AVINV                                                            
                                                                                
AVTM180  OC    WBTMONO,WBTMONO                                                  
         JZ    YES                                                              
         MVI   TGBYTE3,D#TMONO                                                  
                                                                                
         CLC   =C'ON ',WBTMONO     ASSERT THAT ON CAMERA SUBMISSION             
         JNE   AVTM190             IS VALID FOR CATEGORY                        
         TM    TGCASTAT,OKON                                                    
         JO    AVTM200                                                          
         J     AVINV                                                            
                                                                                
AVTM190  CLC   =C'OFF',WBTMONO     ASSERT THAT OFF CAMERA SUBMISSION            
         JNE   AVINV               IS VALID FOR CATEGORY                        
         TM    TGCASTAT,OKOFF                                                   
         JZ    AVINV                                                            
                                                                                
AVTM200  OC    WBTMDOB,WBTMDOB     ASSERT THAT DATE OF BIRTH IS VALID           
         JZ    YES                                                              
         LA    R2,WBTMDOB                                                       
         GOTO1 DTVAL,DMCB,(X'40',WORK)                                          
         JNE   AVINV                                                            
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS THAT PROVIDED SPLIT INVOICE ESTIMATE         *         
*        IS VALID                                                     *         
*        ON ENTRY ... P1 = A(SPLIT INVOICE ESTIMATE)                  *         
***********************************************************************         
                                                                                
AVSINVAL NTR1                                                                   
         L     R2,0(R1)            R2=A(SPLIT INVOICE ESTIMATE TO CHK)          
         XR    R0,R0                                                            
                                                                                
         OC    0(L'WBPYSI1,R2),0(R2)                                            
         JZ    YES                                                              
                                                                                
         CLC   0(L'WBPYSI1,R2),SPACES                                           
         JE    NO                                                               
                                                                                
         GOTO1 AVNUM,DMCB,16(R2)                                                
         JNE   NO                                                               
         GOTO1 (RF),(R1),17(R2)                                                 
         JNE   NO                                                               
         CLI   18(R2),C'.'                                                      
         JNE   NO                                                               
         GOTO1 (RF),(R1),19(R2)                                                 
         JNE   NO                                                               
         GOTO1 (RF),(R1),20(R2)                                                 
         JNE   NO                                                               
         GOTO1 (RF),(R1),21(R2)                                                 
         JNE   NO                                                               
         GOTO1 (RF),(R1),22(R2)                                                 
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS THAT PROVIDED CHARACTER IS A BLANK OR NUMBER *         
*        ON ENTRY ... P1 = A(CHARACTER TO CHECK)                      *         
***********************************************************************         
                                                                                
AVNUMNR  NTR1                                                                   
         L     R2,0(R1)            R2=A(CHARACTER TO CHECK)                     
         CLI   0(R2),C' '                                                       
         JE    YES                                                              
         BAS   RE,AVNUM                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS THAT PROVIDED CHARACTER IS A NUMBER          *         
*        ON ENTRY ... P1 = A(CHARACTER TO CHECK)                      *         
***********************************************************************         
                                                                                
AVNUM    NTR1                                                                   
         L     R2,0(R1)            R2=A(CHARACTER TO CHECK)                     
         CLI   0(R2),C'0'                                                       
         JL    NO                                                               
         CLI   0(R2),C'9'                                                       
         JNH   YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS THAT PROVIDED CHARACTER IS A NUMBER (NOT 0)  *         
*        ON ENTRY ... P1 = A(CHARACTER TO CHECK)                      *         
***********************************************************************         
                                                                                
AVNUMN0  NTR1                                                                   
         L     R2,0(R1)            R2=A(CHARACTER TO CHECK)                     
         CLI   0(R2),C'1'                                                       
         JL    NO                                                               
         CLI   0(R2),C'9'                                                       
         JNH   YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS Y OR N               *         
*        ON ENTRY ... P1 = A (FIELD TO VALIDATE)                      *         
***********************************************************************         
                                                                                
VALYORN  NTR1                                                                   
         L     R2,0(R1)                                                         
         CLI   0(R2),0             VALID VALUES ARE BLANK                       
         JE    YES                                                              
         CLI   0(R2),C' '          SPACE                                        
         JE    YES                                                              
         CLI   0(R2),C'Y'          Y                                            
         JE    YES                                                              
         CLI   0(R2),C'N'          AND N                                        
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES TIME FIELD                                 *         
*        ON ENTRY ... P1 BYTE 0 = L'TIME FIELD                        *         
*                     P1        = TIME FIELD                                    
***********************************************************************         
                                                                                
TIMVALL  NTR1                                                                   
         ZIC   RF,0(R1)            RF=L'TIME FIELD                              
         ZICM  R2,1(R1),3          R2=A(TIME FIELD)                             
                                                                                
TV10     LR    R3,R2                                                            
         AR    R3,RF                                                            
         SHI   R3,1                                                             
         CLI   0(R3),C' '                                                       
         JNE   TV20                                                             
         BCT   RF,TV10                                                          
         DC    H'00'                                                            
                                                                                
TV20     GOTO1 TIMVAL,DMCB,((RF),0(R2)),DUB                                     
         J     XIT                                                              
                                                                                
***********************************************************************         
                                                                                
AVISI1   MVI   TGBYTE3,D#PYSI1                                                  
         J     AVINV                                                            
                                                                                
AVISI2   MVI   TGBYTE3,D#PYSI2                                                  
         J     AVINV                                                            
                                                                                
AVISI3   MVI   TGBYTE3,D#PYSI3                                                  
         J     AVINV                                                            
                                                                                
AVISI4   MVI   TGBYTE3,D#PYSI4                                                  
         J     AVINV                                                            
                                                                                
AVISI5   MVI   TGBYTE3,D#PYSI5                                                  
         J     AVINV                                                            
                                                                                
AVISI6   MVI   TGBYTE3,D#PYSI6                                                  
         J     AVINV                                                            
                                                                                
AVISI7   MVI   TGBYTE3,D#PYSI7                                                  
         J     AVINV                                                            
                                                                                
AVISI8   MVI   TGBYTE3,D#PYSI8                                                  
         J     AVINV                                                            
                                                                                
AVISI9   MVI   TGBYTE3,D#PYSI9                                                  
         J     AVINV                                                            
                                                                                
AVISIA   MVI   TGBYTE3,D#PYSIA                                                  
         J     AVINV                                                            
                                                                                
AVINV    GOTOR ADDGERR,DMCB,('EENINV',0),(TGBYTE3,0)                            
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TADDGERR                                                       
***********************************************************************         
*        ROUTINE VALIDATES ESTIMATE/JOB FIELD                         *         
***********************************************************************         
                                                                                
VREJB    NTR1  BASE=*,LABEL=*                                                   
         TM    WBPYSKV,WBPYSKVJ    EXIT IF SKIPPING ESTIMATE/JOB                
         JO    XIT                 VALIDATION                                   
                                                                                
         OC    WBPYSI1,WBPYSI1     IF ESTIMATE/JOB IS NOT PRESENT               
         JNZ   XIT                                                              
         TM    SVAYSTAT,TAAYSEST   AND AGENCY REQUIRES ESTIMATE                 
         JO    VREJB10                                                          
         TM    SVACBSTA,TABRSINT   OR IS ON PRODUCT INTERFACE                   
         JO    VREJB10                                                          
         TM    SVAYSTA6,TAAYST10   OR IS ON TYPE 10 JOB VALIDATION              
         JO    VREJB10                                                          
         OC    SVAYBUNT,SVAYBUNT   OR IS VALIDATING PROJECTS IDS                
         JZ    XIT                 RETURN ERROR                                 
VREJB10  GOTO1 ADDERROR,DMCB,ERJBREQ                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VALREQ                                     *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERJBREQ  DC    AL1(EJBREQX-*),AL2(ERREQJOB),AL1(ERRCATY1),AL1(D#PYSI1)          
         DC    AL2(0)                                                           
         DC    C'Job# is required'                                              
EJBREQX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES ISPLITS                                    *         
*        ON ENTRY ... P1=A(ISPLIT FIELD)                              *         
*                        BYTE 0 = ISPLIT FIELD NUMBER                 *         
***********************************************************************         
                                                                                
VRISP    NTR1  BASE=*,LABEL=*                                                   
         TM    WBPYSKV,WBPYSKVJ    EXIT IF SKIPPING ESTIMATE/JOB                
         JO    XIT                 VALIDATION                                   
                                                                                
         USING WBPYSPD,R2                                                       
         L     R2,0(R1)            R2=A(ISPLIT FIELD)                           
                                                                                
         OC    0(WBPYSLNQ,R2),0(R2)                                             
         JZ    XIT                                                              
         MVC   ISFIELD,0(R1)       SAVE ISPLIT FIELD NUMBER                     
                                                                                
         BAS   RE,VALDJOB          VALIDATE JOB AGAINST DEFAULT                 
         JE    XIT                 JOB FORMAT                                   
         BAS   RE,VALAJOB          OR VALIDATE JOB AGAINST ACCOUNTING           
         JE    XIT                                                              
         BAS   RE,VALTPID          OR TALENT SYSTEM (PROJECT IDS)               
         JE    XIT                                                              
         BAS   RE,VALTJOB          OR TALENT SYSTEM                             
                                                                                
         MVC   TGCLI,WBPYCLI       RESTORE CLIENT                               
         MVC   TGPRD,WBPYPRD       AND PRODUCT CODES                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES JOB AGAINST DEFAULT JOB FORMAT             *         
*        ON ENTRY ... R2=A(ISPLIT JOB FIELD)                          *         
*                        BYTE 0 = ISPLIT FIELD NUMBER                 *         
***********************************************************************         
                                                                                
VALDJOB  NTR1                                                                   
         OC    SVDEFJOB,SVDEFJOB   IF AGENCY HAS DEFAULT JOB                    
         JZ    NO                  VALIDATE AGAINST FORMAT                      
         OC    0(16,R2),SPACES                                                  
         CLC   =CL16'DEFAULT',0(R2)                                             
         JNE   VDJ10                                                            
         MVC   0(L'SVDEFJOB,R2),SVDEFJOB                                        
         MVI   15(R2),C' '                                                      
VDJ10    GOTOR VALPJOB,DMCB,0(R2)                                               
         BNE   VDJERR                                                           
         CLI   15(R2),C' '                                                      
         JE    YES                                                              
VDJERR   GOTO1 ADDISERR,DMCB,ERPUBJOB                                           
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES JOB AGAINST ACCOUNTING SYSTEM              *         
*        ON ENTRY ... R2=A(ISPLIT JOB FIELD)                          *         
*                        BYTE 0 = ISPLIT FIELD NUMBER                 *         
***********************************************************************         
                                                                                
VALAJOB  NTR1                                                                   
         TM    SVACBSTA,TABRSINT   IF ON INTERFACE                              
         JO    *+12                                                             
         TM    SVAYSTA6,TAAYST10   OR TYPE 10 JOB VALIDATION                    
         JZ    NO                                                               
         CLC   WPSPEST+12(4),SPACES                                             
         JE    VAJ20                                                            
         TM    SVAYSTA2,TAAYSNDF   JOB CODE CODE CANNOT EXCEED 12 CHARS         
         JNO   VAJ10                                                            
         GOTO1 ADDISERR,DMCB,ERJOB12                                            
         J     YES                                                              
VAJ10    GOTO1 ADDISERR,DMCB,ERJOB12O                                           
         J     YES                                                              
                                                                                
VAJ20    MVC   KEY,SPACES          BUILD JOB KEY FOR ACCOUNT FILE               
         MVC   KEY+1(2),=C'SJ'     UNIT/LEDGER                                  
         MVC   KEY+3(12),WPSPEST   ASSUME I/P ENTIRE JOB CODE                   
                                                                                
         CLC   WPSPEST+6(10),SPACES                                             
         JNE   VAJ30               IF FIELD IS LE 6 CHARS. (JOB ONLY)           
         MVC   KEY+3(3),SVCLPICD   RE-BUILD JOB - PRODUCTION CLIENT             
         MVC   KEY+6(3),SVPRPICD                             PRODUCT            
         MVC   KEY+9(6),WPSPEST                              JOB                
VAJ30    OC    KEY+3(12),SPACES    INSURE KEY PADDED WITH SPACES                
                                                                                
         GOTO1 READACC,DMCB,(X'80',0) READ ACCOUNT FILE                         
         JE    VAJ70                                                            
                                                                                
         CLI   ERROR,NOTFOUND      IF ERROR IS RECORD NOT FOUND                 
         JNE   VAJ50                                                            
         TM    SVAYSTA2,TAAYSNDF   RETURN ERROR                                 
         JNO   VAJ40                                                            
         GOTO1 ADDISERR,DMCB,ERJBACC                                            
         J     YES                                                              
VAJ40    GOTO1 ADDISERR,DMCB,ERJBACCO                                           
         J     YES                                                              
                                                                                
VAJ50    CLI   ERROR,ERINTER       IF ERROR IS INTERFACE RECORD                 
         JNE   VAJ60               NOT FOUND, RETURN ERROR                      
         GOTO1 ADDISERR,DMCB,ERINTACC                                           
         J     YES                                                              
                                                                                
VAJ60    CLI   ERROR,ERSWACC       IF ERROR IS "CAN'T SWITCH TO ACC             
         JE    *+6                 SYSTEM" RETURN ERROR                         
         DC    H'00'                                                            
         GOTO1 ADDISERR,DMCB,ERSWTACC                                           
         J     YES                                                              
                                                                                
         USING ACKEYD,R4                                                        
VAJ70    L     R4,AIO              R4=A(ACCOUNTING JOB RECORD)                  
         TM    ACSTATUS,X'60'      TEST FOR CLOSED/LOCKED                       
         JZ    VAJ80                                                            
         GOTO1 ADDISERR,DMCB,ERLCKCLS                                           
                                                                                
VAJ80    TM    TGSYSTA2,TASYSPDJ   IF PROHIBITING DRAFT JOBS                    
         JZ    YES                                                              
         TM    ACSTATUS,X'08'      TEST FOR DRAFT                               
         JZ    YES                                                              
         GOTO1 ADDISERR,DMCB,ERDFJOB                                            
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES PROJECT ID AGAINST TALENT SYSTEM           *         
*        ON ENTRY ... R2=A(JOB FIELD)                                 *         
*                        BYTE 0 = ISPLIT FIELD NUMBER                 *         
***********************************************************************         
                                                                                
VALTPID  NTR1                                                                   
         OC    SVAYBUNT,SVAYBUNT      IF AGY USING JWT PROJECT IDS              
         JO    VTP05                                                            
         TM    SVAYSTA7,TAAYSBBD   BBDO JOB?                                    
         JZ    NO                  NO                                           
VTP05    CLC   WBACTIN,=AL2(I#PJVULD) AND ACTION IS AUTH/PO EST/JOB             
         JE    VTP10                  VALIDATION                                
         TM    SVAYSTA5,TAAYISPL      OR ACTION IS PAY AND AGENCY SET           
         JZ    YES                    TO VALIDATE ISPLITS                       
                                                                                
         USING TLJBD,R4                                                         
VTP10    LA    R4,KEY              ENSURE THAT JOB EXISTS                       
         XC    KEY,KEY                                                          
         MVI   TLJBCD,TLJBCDQ                                                   
         MVI   TLJBSPCL,TLJBSPJW                                                
         TM    SVAYSTA7,TAAYSBBD   BBDO JOB?                                    
         JZ    VTP13               NO                                           
         MVI   TLJBSPCL,TLJBSPBD                                                
         MVC   TLJBPROJ,0(R2)                                                   
         OC    TLJBPROJ,SPACES                                                  
         J     VTP15                                                            
                                                                                
VTP13    MVC   TLJBPRJI,0(R2)                                                   
VTP15    GOTO1 HIGH                                                             
         TM    SVAYSTA7,TAAYSBBD   BBDO JOB                                     
         JZ    VTP18                                                            
         CLC   KEY(TLJBDAT-TLJBD),KEYSAVE                                       
         JNE   VTP30                                                            
         J     YES                                                              
                                                                                
VTP18    CLC   KEY(TLJBDATE-TLJBD),KEYSAVE                                      
         JNE   VTP30                                                            
*&&DO                                                                           
         CLC   7(L'TLJBCSTI+L'TLJBPRDI,R2),SPACES                               
         BH    VTP20                                                            
         MVC   7(L'TLJBCSTI,R2),TLJBCSTI                                        
         MVC   13(L'TLJBPRDI,R2),TLJBPRDI                                       
         J     YES                                                              
*&&                                                                             
VTP20    CLC   TLJBCSTI,7(R2)                                                   
         JNE   VTP30                                                            
         CLC   TLJBPRDI,13(R2)                                                  
         JE    YES                                                              
                                                                                
VTP30    LA    RF,ERJOBNEX                                                      
         TM    SVAYSTA7,TAAYSBBD                                                
         JZ    *+8                                                              
         LA    RF,ERJOBNE1                                                      
         GOTO1 ADDISERR,DMCB,0(RF)                                              
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES JOB AGAINST TALENT SYSTEM                  *         
*        ON ENTRY ... R2=A(JOB FIELD)                                 *         
*                        BYTE 0 = ISPLIT FIELD NUMBER                 *         
***********************************************************************         
                                                                                
VALTJOB  NTR1                                                                   
         OC    SVAYTJOB,SVAYTJOB      IF AGENCY HAS JOB SETUP LENGTHS           
         JZ    XIT                                                              
         TM    SVAYSTAT,TAAYSEST      AND WANTS VALIDATION                      
         JZ    XIT                                                              
         CLC   WBACTIN,=AL2(I#PJVULD) AND ACTION IS AUTH/PO EST/JOB             
         JE    VTJ10                  VALIDATION                                
         TM    SVAYSTA5,TAAYISPL      OR ACTION IS PAY AND AGENCY SET           
         JZ    XIT                    TO VALIDATE ISPLITS                       
                                                                                
         USING TLJBD,R4                                                         
VTJ10    LA    R4,KEY                                                           
         BAS   RE,VTJEXIST         JOB RECORDS MUST EXIST UNDER THIS            
         JNE   XIT                 AGENCY                                       
                                                                                
         LA    RE,L'WPSPEST-1(R2)                                               
         LHI   R1,L'WPSPEST        USE R1 TO SAVE INPUTTED JOB LENGTH           
VTJ20    CLI   0(RE),C' '                                                       
         JNE   VTJ30                                                            
         SHI   RE,1                                                             
         SHI   R1,1                                                             
         J     VTJ20                                                            
                                                                                
VTJ30    BAS   RE,VTJFORMT         AUTO-FORMAT JOB FIELD                        
         JE    VTJ40               (CLI/PRD CODES MATCH COMM'L)                 
                                                                                
         BAS   RE,VTJSVCDS         OR SAVE CLI/PRD COES FROM INPUT              
         JNE   XIT                 (CLI/PRD CODES COME FROM HERE)               
                                                                                
VTJ40    MVC   KEY(L'SVJBKEY),SVJBKEY                                           
         MVC   TLJBCLI,TGCLI       JOB RECORD MUST EXIST FOR THIS               
         MVC   TLJBPRD,TGPRD       CLIENT AND PRODUCT                           
         GOTO1 HIGH                                                             
         CLC   KEY(TLJBCLI-TLJBD+3),KEYSAVE                                     
         JE    VTJ50                                                            
         LA    RF,ERJOBNEX                                                      
         TM    SVAYSTA7,TAAYSBBD                                                
         JZ    *+8                                                              
         LA    RF,ERJOBNE1                                                      
         GOTO1 ADDISERR,DMCB,0(RF)                                              
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
VTJ50    MVC   SVJBKEY,KEY                                                      
                                                                                
         BAS   RE,VTJBUNIT         VALIDATE BUSINESS UNITS                      
         JE    XIT                                                              
                                                                                
VTJ60    GOTO1 GETREC              GET JOB RECORD                               
                                                                                
         XC    WORK,WORK           IF GENERAL LIST ELEMENT EXISTS               
         MVC   WORK+1(6),SVJBCODE  FOR THIS JOB                                 
         MVI   ELCODE,TAGLELQ      VALIDATION IS COMPLETE                       
         GOTO1 GETL,DMCB,(7,WORK)                                               
         JE    XIT                                                              
                                                                                
         GOTO1 SEQ                                                              
         CLC   KEY(TLJBSEQ-TLJBKEY),KEYSAVE                                     
         JE    VTJ60                                                            
         GOTO1 ADDISERR,DMCB,ERJOBJNC                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE CHECKS IF JOB RECORDS EXIST FOR AGENCY               *         
*        ON ENTRY ... R4=A(KEY)                                       *         
***********************************************************************         
                                                                                
         USING TLJBD,R4                                                         
VTJEXIST NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   TLJBCD,TLJBCDQ      BUILD KEY TO READ FOR JOB RECORDS            
         MVC   TLJBAGY,WBPYAGY     UNDER THIS AGENCY                            
                                                                                
         CLC   TLJBAGY,=CL6'2071'  (IF AGENCY IS 2071                           
         JNE   VTJE10                                                           
         MVC   TLJBAGY,=C'4937'    LOOK FOR JOB UNDER 4937)                     
                                                                                
VTJE10   CLC   TLJBAGY,=CL6'1073'  (IF AGENCY IS 1073                           
         JNE   VTJE20                                                           
         MVC   TLJBAGY,=C'DWNY'    LOOK FOR JOB UNDER DWNY)                     
                                                                                
VTJE20   CLC   TLJBAGY,=CL6'9325'  (IF AGENCY IS 9325                           
         JNE   VTJE25                                                           
         MVC   TLJBAGY,=C'DWNY'    LOOK FOR JOB UNDER DWLA)                     
                                                                                
VTJE25   GOTO1 HIGH                                                             
         CLC   KEY(TLJBDTE-TLJBD),KEYSAVE                                       
         JE    VTJE50                                                           
                                                                                
         CLC   WBPYAGY,=CL6'DWLA'  (IF AGENCY IS DWLA                           
         JE    VTJE30                                                           
         CLC   WBPYAGY,=CL6'9032'  OR 9032                                      
         JE    VTJE30                                                           
         CLC   WBPYAGY,=CL6'9325'  OR 9325                                      
         JNE   VTJE40                                                           
VTJE30   XC    KEY,KEY                                                          
         MVI   TLJBCD,TLJBCDQ      AND JOB IS NOT FOUND                         
         MVC   TLJBAGY,=C'DWNY'    LOOK FOR JOB UNDER DWNY)                     
         GOTO1 HIGH                                                             
         CLC   KEY(TLJBDTE-TLJBD),KEYSAVE                                       
         JE    VTJE50                                                           
                                                                                
VTJE40   LA    RF,ERJOBNEX                                                      
         TM    SVAYSTA7,TAAYSBBD                                                
         JZ    *+8                                                              
         LA    RF,ERJOBNE1                                                      
         GOTO1 ADDISERR,DMCB,0(RF)                                              
         J     NO                                                               
                                                                                
VTJE50   MVC   SVJBKEY(TLJBCLI-TLJBD),KEY                                       
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        IF INPUT IS JOB ONLY, FORMAT THE ESTIMATE FIELD              *         
*        ON ENTRY ... R1=L'INPUT IN JOB FIELD                         *         
*                     R2=A(JOB FIELD)                                 *         
***********************************************************************         
                                                                                
VTJFORMT NTR1                                                                   
         TM    SVAYSTA5,TAAYBUSU   IF BUSINESS UNITS IN CLIENT                  
         JO    NO                  DO NOT AUTO-FORMAT JOB                       
                                                                                
         ZIC   RE,SVAYTJOB+2                                                    
         CR    R1,RE               IF IS NOT JOB ONLY                           
         JH    NO                  DO NOT AUTO-FORMAT JOB                       
                                                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   SVJBCODE,WPSPEST    SAVE JOB CODE                                
                                                                                
         XC    WPSPEST,WPSPEST     CLEAR JOB FIELD                              
                                                                                
         ZIC   R1,SVAYTJOB         REFORMAT JOB FIELD                           
         BCTR  R1,0                TO START WITH CLIENT CODE                    
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),WBPYCLI                                                  
         LA    R2,1(R1,R2)                                                      
                                                                                
         ZIC   R1,SVAYTJOB+1       TO BE FOLLOWED BY PRODUCT CODE               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),WBPYPRD                                                  
         LA    R2,1(R1,R2)                                                      
                                                                                
         ZIC   R1,SVAYTJOB+2        AND FINISH OFF WITH JOB CODE                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),SVJBCODE                                                 
         J     YES                                                              
                                                                                
***********************************************************************         
*        INF INPUT IS NOT JOB ONLY, VALIDATE JOB INPUT                *         
*        ON ENTRY ... R1=L'INPUT IN JOB FIELD                         *         
*                     R2=A(JOB FIELD)                                 *         
***********************************************************************         
                                                                                
VTJSVCDS NTR1                                                                   
         ZIC   RE,SVAYTJOB         IF JOB IS FULLY INPUT                        
         ZIC   RF,SVAYTJOB+1       ENSURE THAT MAXIMUM JOB LENGTH               
         AR    RE,RF               IS NOT EXCEEDED                              
         ZIC   RF,SVAYTJOB+2                                                    
         AR    RE,RF                                                            
         CR    R1,RE                                                            
         JNH   VTJS10                                                           
         LA    RF,ERJOBNEX                                                      
         TM    SVAYSTA7,TAAYSBBD                                                
         JZ    *+8                                                              
         LA    RF,ERJOBNE1                                                      
         GOTO1 ADDISERR,DMCB,0(RF)                                              
         J     NO                                                               
                                                                                
VTJS10   ZIC   R1,SVAYTJOB         SAVE INPUTTED CLIENT                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   TGCLI(0),0(R2)                                                   
         LA    R2,1(R1,R2)                                                      
                                                                                
         ZIC   R1,SVAYTJOB+1       SAVE INPUTTED PRODUCT                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   TGPRD(0),0(R2)                                                   
         LA    R2,1(R1,R2)                                                      
                                                                                
         ZIC   R1,SVAYTJOB+2       SAVE INPUTTED JOB                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     YES                                                              
         MVC   SVJBCODE(0),0(R2)                                                
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES CLIENT BUSINESS UNITS                      *         
*        ON ENTRY ... R4=A(KEY)                                       *         
***********************************************************************         
                                                                                
VTJBUNIT NTR1                                                                   
         TM    SVAYSTA5,TAAYBUSU   IF BUSINESS UNITS IN CLIENT                  
         JZ    NO                                                               
                                                                                
         USING TLCLD,R4                                                         
         XC    KEY,KEY                                                          
         MVI   TLCLCD,TLCLCDQ      READ CLIENT RECORD                           
         MVC   TLCLAGY,WBPYAGY                                                  
         MVC   TLCLCLI(3),TGCLI+3                                               
         GOTO1 HIGH                                                             
         CLC   TLCLKEY,KEYSAVE                                                  
         JNE   VTJB10                                                           
         GOTO1 GETREC                                                           
         DROP  R4                                                               
                                                                                
         USING TABRD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TABRELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   VTJB10              IF CLIENT EXISTS AND                         
         TM    TABRSTAT,TABRSNIN   IS SET UP TO INTERFACE                       
         JO    YES                 VALIDATION IS COMPLETE                       
         DROP  R4                                                               
                                                                                
VTJB10   MVC   KEY(L'SVJBKEY),SVJBKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(TLJBCLI-TLJBD+3),KEYSAVE                                     
         JE    NO                                                               
         DC    H'00'                                                            
                                                                                
***********************************************************************         
*        ROUTINE TO ADD ISPLIT ERROR DETAILS TO ERROR TABLE           *         
*        ON ENTRY ... P1=A(ERROR ENTRY TO ADD)                        *         
***********************************************************************         
                                                                                
ADDISERR NTR1                                                                   
         USING ERRENTD,R2                                                       
         L     R2,0(R1)                                                         
         ZIC   RE,EELEN            COPY ERROR ENTRY TO BLOCK                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   BLOCK(0),ERRENTD                                                 
         DROP  R2                                                               
                                                                                
         USING ERRENTD,R2                                                       
         LA    R2,BLOCK            REPLACE ERROR FIELD WITH                     
         MVC   EEFIELD,ISFIELD     SAVED VALUE                                  
         GOTO1 ADDERROR,DMCB,(R2)                                               
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VRISP                                      *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERPUBJOB DC    AL1(EPUBJOBX-*),AL2(EPUBJOB),AL1(ERRCATY1),AL1(D#PYSI1)          
         DC    AL2(0)                                                           
         DC    C'Invalid input field. Type ''DEFAULT'' to enter '               
         DC    C'default job'                                                   
EPUBJOBX EQU   *                                                                
                                                                                
ERJOB12  DC    AL1(EJOB12X-*),AL2(ER12JOB),AL1(ERRCATY1),AL1(D#PYSI1)           
         DC    AL2(0)                                                           
         DC    C'Job# cannot exceed 12 characters'                              
EJOB12X  EQU   *                                                                
                                                                                
ERJOB12O DC    AL1(EJOB12OX-*),AL2(ER12JOBO),AL1(ERRCATY2),AL1(D#PYSI1)         
         DC    AL2(0)                                                           
         DC    C'Job# cannot exceed 12 characters'                              
EJOB12OX EQU   *                                                                
                                                                                
ERJBACC  DC    AL1(EJBACCX-*),AL2(ERACCJB),AL1(ERRCATY1),AL1(D#PYSI1)           
         DC    AL2(0)                                                           
         DC    C'Job# does not exist on Accounting system'                      
EJBACCX  EQU   *                                                                
                                                                                
ERJBACCO DC    AL1(EJBACCOX-*),AL2(ERACCJBO),AL1(ERRCATY2),AL1(D#PYSI1)         
         DC    AL2(0)                                                           
         DC    C'Job# does not exist on Accounting system'                      
EJBACCOX EQU   *                                                                
                                                                                
ERINTACC DC    AL1(EINTACCX-*),AL2(ERINTER),AL1(ERRCATY1),AL1(D#PYSI1)          
         DC    AL2(0)                                                           
         DC    C'Interface record does not exist - contact Talent Partn+        
               ers'                                                             
EINTACCX EQU   *                                                                
                                                                                
ERSWTACC DC    AL1(ESWTACCX-*),AL2(ERSWACC),AL1(ERRCATY1),AL1(D#PYSI1)          
         DC    AL2(0)                                                           
         DC    C'DDS system is unavailable, can not validate'                   
ESWTACCX EQU   *                                                                
                                                                                
ERLCKCLS DC    AL1(ELCKCLSX-*),AL2(ERCLSLCK),AL1(ERRCATY1),AL1(D#PYSI1)         
         DC    AL2(0)                                                           
         DC    C'Job# is closed or locked'                                      
ELCKCLSX EQU   *                                                                
                                                                                
ERDFJOB  DC    AL1(EDFJOBX-*),AL2(ERDFTJOB),AL1(ERRCATY1),AL1(D#PYSI1)          
         DC    AL2(0)                                                           
         DC    C'Job is draft'                                                  
EDFJOBX  EQU   *                                                                
                                                                                
ERJOBNEX DC    AL1(EJOBNEXX-*),AL2(ERNEXJOB),AL1(ERRCATY2),AL1(D#PYSI1)         
         DC    AL2(0)                                                           
         DC    C'Job does not exist on Talent System'                           
EJOBNEXX EQU   *                                                                
                                                                                
ERJOBNE1 DC    AL1(EJOBNE1X-*),AL2(ERNEXJOB),AL1(ERRCATY1),AL1(D#PYSI1)         
         DC    AL2(0)                                                           
         DC    C'Job does not exist on Talent System'                           
EJOBNE1X EQU   *                                                                
                                                                                
ERJOBJNC DC    AL1(EJOBJNCX-*),AL2(ERNJCJOB),AL1(ERRCATY2),AL1(D#PYSI1)         
         DC    AL2(0)                                                           
         DC    C'Job # does not exist on Job record'                            
EJOBJNCX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES OVERRIDE AGENTS                            *         
*        ON ENTRY ... R8=A(WEB RESPONSE DETAILS AREA)                 *         
***********************************************************************         
                                                                                
VRAGN    NTR1  BASE=*,LABEL=*                                                   
VRAGN10  CLI   0(R8),X'FF'                                                      
         JE    XIT                                                              
                                                                                
         OC    WRSOVAGT,WRSOVAGT   IF AGENT IS BEING OVERRIDDEN FOR             
         JZ    VRAGN40             PERFORMER                                    
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'A4',WRSOVAGT)                             
         JE    VRAGN20                                                          
         GOTOR ADDEAP,DMCB,0,ERAGNINV,0(R8)                                     
         J     VRAGN40                                                          
                                                                                
         USING TAAND,R4                                                         
VRAGN20  L     R4,AIO                                                           
         MVI   ELCODE,TAANELQ      GET AGENT DETAILS ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   VRAGN30                                                          
         TM    TAANSTAT,TAANSOVR   TEST ALLOWABLE OVERRIDE                      
         JO    VRAGN40                                                          
VRAGN30  GOTOR ADDEAP,DMCB,0,ERAGNNAO,0(R8)                                     
         DROP  R4                                                               
                                                                                
VRAGN40  LA    R8,WRSLNQ(R8)                                                    
         J     VRAGN10                                                          
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VRAGN                                      *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERAGNINV DC    AL1(EAGNINVX-*),AL2(ERRIANCD),AL1(ERRCATY1),AL1(D#PCAGT)         
         DC    AL2(0)                                                           
         DC    C'Agent code is not on file'                                     
EAGNINVX EQU   *                                                                
                                                                                
ERAGNNAO DC    AL1(EAGNNAOX-*),AL2(ERRANNAO),AL1(ERRCATY1),AL1(D#PCAGT)         
         DC    AL2(0)                                                           
         DC    C'Unable to override Agent code'                                 
EAGNNAOX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES PRINT FIELDS                               *         
*        ON ENTRY ... R8=A(WEB RESPONSE DETAILS AREA)                 *         
***********************************************************************         
                                                                                
VRPRT    NTR1  BASE=*,LABEL=*                                                   
         OC    WBPYARE,WBPYARE                                                  
         JZ    XIT                                                              
         OC    WBPYARE,SPACES                                                   
         GOTO1 RECVAL,DMCB,TLARCDQ,(X'84',WBPYARE)                              
         JE    VRPRT10                                                          
         GOTO1 ADDERROR,DMCB,ERPYARE                                            
         J     XIT                                                              
                                                                                
VRPRT10  OC    WBPYPRU,SPACES                                                   
         GOTO1 RECVAL,DMCB,TLUSCDQ,(X'84',WBPYPRU)                              
         JE    XIT                                                              
         GOTO1 ADDERROR,DMCB,ERPYPRU                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR VRPRT                                      *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERPYARE  DC    AL1(EPYAREX-*),AL2(ERRPYARE),AL1(ERRCATY3),AL1(D#PYARE)          
         DC    AL2(0)                                                           
         DC    C'Area is not on file'                                           
EPYAREX  EQU   *                                                                
                                                                                
ERPYPRU  DC    AL1(EPYPRUX-*),AL2(ERRPYPRU),AL1(ERRCATY3),AL1(D#PYPRU)          
         DC    AL2(0)                                                           
         DC    C'Use is not on file'                                            
EPYPRUX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO INITIALIZE PAY PROGRAM                            *         
***********************************************************************         
                                                                                
INITPAY  NTR1  BASE=*,LABEL=*                                                   
         NI    TGUSSTA3,X'FF'-NWKUSE                                            
         MVC   WBPYTOT,PAYTOT                                                   
                                                                                
         MVI   OVERLAY,X'50'                                                    
         GOTO1 LOADSOPH,DMCB,0                                                  
         GOTO1 (R3),DMCB,(RC)                                                   
                                                                                
         MVC   DUB,SPACES                                                       
         MVC   DUB(L'TGUSCDE),TGUSCDE                                           
                                                                                
         USING RACTD,R1                                                         
         L     R1,TGAGRACT         SEARCH FOR PAY TYPE IN GEN'S                 
         ZIC   R2,RACTLTBL         RECORD ACTION TABLE                          
         LA    R1,RACTTBL                                                       
         DROP  R1                                                               
                                                                                
IP10     CLC   DUB,1(R1)                                                        
         JE    IP20                                                             
         AR    R1,R2                                                            
         CLI   0(R1),1                                                          
         JE    IP10                                                             
         DC    H'00'                                                            
IP20     MVC   BYTE,9(R1)                                                       
                                                                                
IP30     CLI   0(R1),3                                                          
         JNE   IP40                                                             
         CLC   BYTE,1(R1)          WHEN FOUND, SAVE SCREEN OVERLAY              
         JE    IP50                                                             
IP40     AR    R1,R2                                                            
         J     IP30                                                             
IP50     MVC   TGFASCRN,3(R1)                                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE LOADS PAY SCREEN AND POPULATES WITH VALUES           *         
*        FROM REQUEST                                                 *         
*        ON ENTRY ... P1=(MODE)                                       *         
***********************************************************************         
                                                                                
BLDSCR   NTR1  BASE=*,LABEL=*                                                   
         MVC   SCRMODE,3(R1)       SET MODE                                     
                                                                                
         L     R3,EFHTAG                                                        
         MVI   ALTPROG,X'F3'                                                    
         MVC   OVERLAY,TGFASCRN                                                 
         GOTO1 LOADSOPH,DMCB,1     LOAD SCREEN                                  
         MVC   TWASCR,OVERLAY                                                   
                                                                                
         MVC   CONREC(3),WBPYUSE   POPULATE RECORD FIELD                        
         GOTOR SETLEN,DMCB,CONRECH,WBPYUSE,L'WBPYUSE                            
                                                                                
         MVC   CONACT,=CL8'PAY'    POPULATE ACTION FIELD                        
         MVI   CONACTH+5,3                                                      
                                                                                
         MVC   PAYAGY,WBPYAGY      POPULATE AGENCY FIELD                        
         GOTOR SETLEN,DMCB,PAYAGYH,WBPYAGY,L'WBPYAGY                            
                                                                                
         CLI   SCRMODE,WBMDVFY     IF PREPARING FOR VERIFICATION                
         JNE   BS20                                                             
         MVC   PAYINV,=CL6'DRAFT'  SET UP AS A DRAFT PAYMENT                    
         MVI   PAYINVH+5,5                                                      
         J     BS30                                                             
                                                                                
BS20     CLI   SCRMODE,WBMDEXE     IF PREPARING FOR EXECUTION                   
         JNE   BS30                                                             
         XC    PAYINV,PAYINV       SET UP FOR INVOICE ASSIGNMENT                
         NI    PAYINVH+4,X'FF'-X'20'                                            
         MVI   PAYINVH+5,0                                                      
         OC    WBPYINV,WBPYINV     OR INSERT PROVIDED INVOICE NUMBER            
         JZ    BS30                                                             
         MVC   PAYINV,WBPYINV                                                   
         GOTOR SETLEN,DMCB,PAYINVH,WBPYINV,L'WBPYINV                            
                                                                                
BS30     MVC   PAYCID,TGCID        POPULATE COMMERCIAL ID FIELD                 
         GOTOR SETLEN,DMCB,PAYCIDH,TGCID,L'TGCID                                
                                                                                
         OC    WBPYVER,WBPYVER     IF VERSION CODE PROVIDED                     
         JZ    BS40                POPULATE VERSION FIELD                       
         EDIT  WBPYVER,PAYLFT,ALIGN=LEFT                                        
         OI    PAYLFTH+4,X'08'                                                  
         STC   R0,PAYLFTH+5                                                     
                                                                                
BS40     CLI   WBPYLFT,0           IF "PAY LIFT?" INPUT PROVIDED                
         JE    BS50                POPULATE LIFT FIELD                          
         MVC   PAYLFT(1),WBPYLFT                                                
         MVI   PAYLFTH+5,1                                                      
                                                                                
BS50     CLI   TGUSEQU,ULFT        IF MAKING LIFT PAYMENT                       
         JE    POPSCR13                                                         
         CLI   TGUSEQU,UALF        OR ADDENDUM LIFT PAYMENT                     
         JE    POPSCR13                                                         
         CLI   TGUSEQU,USLF        OR SPANISH LIFT PAYMENT                      
         JE    POPSCR13            POPULATE LIFT SCREEN                         
                                                                                
         CLI   TGUSEQU,UGRT        IF MAKING GUARANTEE PAYMENT                  
         JE    POPSCR15            POPULATE GUARANTEE SCREEN                    
                                                                                
         CLI   TGUSEQU,USWS        IF MAKING A SPANISH WILDSPOT                 
         JE    POPSCR55            PAYMENT                                      
         CLI   TGUSEQU,UWSC        OR CANADIAN WILDSPOT PAYMENT                 
         JE    POPSCR55                                                         
         CLI   TGUSEQU,UWSP        OR WILDSPOT PAYMENT                          
         JE    POPSCR55            POPULATE WILDSPOT SCREEN                     
                                                                                
         CLI   TGUSEQU,UCLA        IF MAKING A CLASS A PAYMENT                  
         JE    POPSCR5B                                                         
         CLI   TGUSEQU,ULNA        OR LATE NIGHT ABC PAYMENT                    
         JE    POPSCR5B                                                         
         CLI   TGUSEQU,ULNC        OR LATE NIGHT CBS PAYMENT                    
         JE    POPSCR5B                                                         
         CLI   TGUSEQU,ULNN        OR LATE NIGHT NBC PAYMENT                    
         JE    POPSCR5B                                                         
         CLI   TGUSEQU,UPAX        OR PAX PAYMENT                               
         JE    POPSCR5B            POPULATE SCREEN                              
                                                                                
         CLI   TGUSEQU,UCBL        IF MAKING A CABLE PAYMENT                    
         JE    POPSCR5F                                                         
         CLI   TGUSEQU,ULCB        OR LOCAL CABLE PAYMENT                       
         JE    POPSCR5F                                                         
         CLI   TGUSEQU,USCB        OR SPANISH CABLE PAYMENT                     
         JE    POPSCR5F            POPULATE CABLE SCREEN                        
                                                                                
         CLI   TGUSEQU,UADW        IF MAKING AN ADDENDUM WILDSPOT               
         JE    POPSCR9C            PAYMENT                                      
                                                                                
         CLI   TGUSEQU,UEDS        IF MAKING 2ND ALLOW EDIT PAYMENT             
         JE    POPSCRC2            POPULATE 2ND ALLOW EDIT SCREEN               
                                                                                
         CLI   TGUSEQU,UDWN        IF MAKING DOWNGRADE PAYMENT                  
         JE    POPSCRC2            POPULATE DOWNGRADE SCREEN                    
                                                                                
         CLI   TGUSEQU,UADT        IF MAKING ADD TV SESSION PAYMENT             
         JE    POPSCR64            POPULATE ADD TV SESSION SCREEN               
                                                                                
         CLI   TGUSEQU,UADO        IF MAKING ADD RADIO SESSION PAYMENT          
         JE    POPSCR65            POPULATE ADD RADIO SESSION SCREEN            
                                                                                
         CLI   TGUSEQU,UPRT        IF MAKING PRINT PAYMENT                      
         JE    POPSCR69                                                         
         CLI   TGUSEQU,UPRS        OR PRINT SESSION PAYMENT                     
         JE    POPSCR69            POPULATE PRINT SCREEN                        
                                                                                
         CLI   TGUSEQU,UTAG        IF MAKING TAG PAYMENT                        
         JE    POPSCR6A            POPULATE TAG SCREEN                          
                                                                                
         CLI   TGUSEQU,UDEM        IF MAKING DEMO PAYMENT                       
         JE    POPSCR70                                                         
         CLI   TGUSEQU,USNA        OR SPANISH DEMO PAYMENT                      
         JE    POPSCR70            POPULATE DEMO SCREEN                         
                                                                                
         TM    TGUSSTA2,HLDTYPE    IF MAKING HOLDING FEE PAYMENT                
         JO    POPSCRC2            POPULATE HOLDING FEE SCREEN                  
                                                                                
         J     POPSCRC1            ELSE POPULATE COMMON SCREEN                  
                                                                                
***********************************************************************         
*        ROUTINE TO POPULATE LIFT SCREEN WITH VALUES                  *         
*        FROM REQUEST                                                 *         
***********************************************************************         
                                                                                
POPSCR13 LA    R2,LFTFFCH                                                       
         LA    R3,LFTNFFCH                                                      
         CLI   WBPYTYP,WBPTUCF     POPULATE UPDATE FFC                          
         JE    *+12                AND DO NOT UPDATE FFC FIELDS                 
         LA    R2,LFTNFFCH                                                      
         LA    R3,LFTFFCH                                                       
         MVI   8(R2),C'X'                                                       
         MVI   5(R2),1                                                          
         MVI   8(R3),0                                                          
         MVI   5(R3),0                                                          
         J     POPSCRC2                                                         
                                                                                
***********************************************************************         
*        ROUTINE TO POPULATE GUARANTEE SCREEN WITH VALUES             *         
*        FROM REQUEST                                                 *         
***********************************************************************         
                                                                                
POPSCR15 MVC   GT2OVER,WBPYOPD     POPULATE OVERAGE PAID FIELD                  
         MVI   GT2OVERH+5,1                                                     
         J     POPSCRC2                                                         
                                                                                
***********************************************************************         
*        ROUTINE TO POPULATE WILDSPOT SCREEN WITH VALUES              *         
*        FROM REQUEST                                                 *         
***********************************************************************         
                                                                                
POPSCR55 CLI   WBPYTYP,WBPTUPGF    IF 13 WEEK USE                               
         JH    PS5510                                                           
         MVI   WSP13W,C'Y'         POPULATE 13 WEEK FIELD                       
         MVI   WSP13WH+5,1                                                      
         J     PS5520                                                           
                                                                                
PS5510   MVI   WSP8W,C'Y'          IF 8 WEEK USE                                
         MVI   WSP8WH+5,1          POPULATE 13 WEEK FIELD                       
                                                                                
PS5520   CLI   WBPYTYP,WBPTUPG     IF UPGRADE                                   
         JE    PS5530                                                           
         CLI   WBPYTYP,WBPTUPG8                                                 
         JNE   PS5540                                                           
PS5530   MVI   WSPUPGR,C'Y'        POPULATE UPGRADE FIELD                       
         MVI   WSPUPGRH+5,1                                                     
                                                                                
PS5540   CLI   WBPYTYP,WBPTUPGF    IF FORCED UPGRADE                            
         JE    PS5550                                                           
         CLI   WBPYTYP,WBPTUPF8                                                 
         JNE   PS5560                                                           
PS5550   MVI   WSPUPGR,C'F'        IF FORCED UPGRADE FIELD                      
         MVI   WSPUPGRH+5,1        POPULATE UPGRADE FIELD                       
         MVC   WSPLNY,WBPYFNY                                                   
         GOTOR SETLEN,DMCB,WSPLNYH,WBPYFNY,L'WBPYFNY                            
         MVC   WSPLLA,WBPYFLA                                                   
         GOTOR SETLEN,DMCB,WSPLLAH,WBPYFLA,L'WBPYFLA                            
         MVC   WSPLCHI,WBPYFCH                                                  
         GOTOR SETLEN,DMCB,WSPLCHIH,WBPYFCH,L'WBPYFCH                           
         MVC   WSPLUNT,WBPYFIU     AND LAST UNITS FIELD                         
         OI    WSPLUNTH+4,X'08'                                                 
         GOTOR SETLEN,DMCB,WSPLUNTH,WBPYFIU,L'WBPYFIU                           
                                                                                
PS5560   OC    WBPYUNIT,WBPYUNIT   IF OVERRIDE UNITS PROVIDED                   
         JZ    PS5570                                                           
         MVC   WSPUNIT,WBPYUNIT    POPULATE UNITS FIELD                         
         OI    WSPUNITH+4,X'08'                                                 
         GOTOR SETLEN,DMCB,WSPUPGRH,WBPYUNIT,L'WBPYUNIT                         
                                                                                
PS5570   GOTOR REUSEDET,DMCB,AIO3  IF USE DETAILS WERE PROVIDED                 
         JNE   POPSCRC2                                                         
                                                                                
         LA    R2,WSPFRSTH                                                      
                                                                                
         USING WBPUDAT,R3                                                       
         L     R3,AIO3                                                          
                                                                                
PS5580   MVC   8(L'WBPUDAT,R2),WBPUDAT                                          
         GOTOR SETLEN,DMCB,0(R2),WBPUDAT,L'WBPUDAT                              
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   8(L'WBPMMKT,R2),WBPMMKT                                          
         GOTOR SETLEN,DMCB,0(R2),WBPMMKT,L'WBPMMKT                              
                                                                                
         CLI   WBPULNQ(R3),X'FF'                                                
         JE    POPSCRC2                                                         
         LA    R3,WBPULNQ(R3)                                                   
         DROP  R3                                                               
                                                                                
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         J     PS5580                                                           
                                                                                
***********************************************************************         
*        ROUTINE TO POPULATE CLASS A SCREEN WITH VALUES               *         
*        FROM REQUEST                                                 *         
***********************************************************************         
                                                                                
POPSCR5B CLI   WBPYTYP,WBPTCLAG    IF GUARANTEED 13 USE                         
         JNE   PS5B10                                                           
         MVI   CLAG13H+5,1         SET INDICATOR                                
         MVI   CLAG13,C'Y'                                                      
                                                                                
PS5B10   GOTOR REUSEDET,DMCB,AIO3  IF USE DETAILS WERE PROVIDED                 
         JNE   POPSCRC2                                                         
                                                                                
         USING CLAD,R2                                                          
         LA    R2,CLAFRSTH                                                      
                                                                                
         USING WBPUDAT,R3                                                       
         L     R3,AIO3                                                          
                                                                                
PS5B20   MVC   CLAIND,WBPUUID      FILL IN THE SCREEN WITH THEM                 
         MVC   CLADATE,WBPUDAT                                                  
         GOTOR SETLEN,DMCB,CLADATEH,WBPUDAT,L'WBPUDAT                           
         MVC   CLAPROG,WBPUNAM                                                  
         GOTOR SETLEN,DMCB,CLAPROGH,WBPUNAM,L'WBPUNAM                           
         MVC   CLANWK,WBPUNWK                                                   
         GOTOR SETLEN,DMCB,CLANWKH,WBPUNWK,L'WBPUNWK                            
                                                                                
         CLI   WBPULNQ(R3),X'FF'                                                
         JE    POPSCRC2                                                         
         LA    R3,WBPULNQ(R3)                                                   
         DROP  R3                                                               
                                                                                
         LA    R2,CLANEXT                                                       
         J     PS5B20                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO POPULATE CABLE SCREEN WITH VALUES                 *         
*        FROM REQUEST                                                 *         
***********************************************************************         
                                                                                
POPSCR5F OC    WBPYUNIT,WBPYUNIT   IF OVERRIDE UNITS PROVIDED                   
         JZ    PS5F10                                                           
         MVC   CBLUNIT,WBPYUNIT    POPULATE UNITS FIELD                         
         OI    CBLUNITH+4,X'08'                                                 
         GOTOR SETLEN,DMCB,CBLUPGRH,WBPYUNIT,L'WBPYUNIT                         
                                                                                
PS5F10   CLI   WBPYTYP,WBPTUPG     IF UPGRADE                                   
         JNE   PS5F20                                                           
         MVI   CBLUPGR,C'Y'        POPULATE UPGRADE FIELD                       
         MVI   CBLUPGRH+5,1                                                     
         J     PS5F25                                                           
                                                                                
PS5F20   CLI   WBPYTYP,WBPTUPGF                                                 
         JNE   PS5F25                                                           
         MVI   CBLUPGR,C'F'        IF FORCED UPGRADE FIELD                      
         MVI   CBLUPGRH+5,1        POPULATE UPGRADE FIELD                       
         MVC   CBLLUNT,WBPYFIU     AND LAST UNITS FIELD                         
         OI    CBLLUNTH+4,X'08'                                                 
         GOTOR SETLEN,DMCB,CBLLUNTH,WBPYFIU,L'WBPYFIU                           
                                                                                
PS5F25   GOTOR REUSEDET,DMCB,AIO3  IF USE DETAILS WERE PROVIDED                 
         JNE   POPSCRC2                                                         
                                                                                
         LA    R2,CBLFRSTH                                                      
                                                                                
         USING WBPUDAT,R3                                                       
         L     R3,AIO3                                                          
                                                                                
PS5F30   MVC   8(L'WBPUDAT,R2),WBPUDAT                                          
         GOTOR SETLEN,DMCB,0(R2),WBPUDAT,L'WBPUDAT                              
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   8(L'WBPMMKT,R2),WBPMMKT                                          
         GOTOR SETLEN,DMCB,0(R2),WBPMMKT,L'WBPMMKT                              
                                                                                
         CLI   WBPULNQ(R3),X'FF'                                                
         JE    POPSCRC2                                                         
         LA    R3,WBPULNQ(R3)                                                   
         DROP  R3                                                               
                                                                                
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         J     PS5F30                                                           
                                                                                
***********************************************************************         
*        ROUTINE TO POPULATE ADDENDUM WILDSPOT SCREEN WITH VALUES     *         
*        FROM REQUEST                                                 *         
***********************************************************************         
                                                                                
POPSCR9C DS    0H                                                               
******** ADD CODE HERE ********                                                 
         J     POPSCRC2                                                         
                                                                                
***********************************************************************         
*        ROUTINE TO POPULATE ADDENDUM RADIO TV SCREEN WITH            *         
*        VALUES FROM REQUEST                                          *         
***********************************************************************         
                                                                                
POPSCR64 CLC   SVCOADST,=C'TX'     SKIP AHEAD IF ADDENDUM STATE                 
         JE    POPSCRC1            IS TEXAS                                     
                                                                                
         CLI   WBPYTYP,WBPT3DY     IF PAY TYPE IS 3 DAYS                        
         JNE   PS64A                                                            
         MVI   ADTTTY1,C'X'        POPULATE 3 DAY FIELD                         
         MVI   ADTTTY1H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS64A    CLI   WBPYTYP,WBPT1WK     IF PAY TYPE IS 1 WEEK                        
         JNE   PS64C                                                            
         CLC   SVCOADST,=C'KS'                                                  
         JNE   PS64B                                                            
         MVI   ADTTTY1,C'X'        POPULATE 1 WEEK FIELD                        
         MVI   ADTTTY1H+5,1                                                     
         J     POPSCRC1                                                         
PS64B    MVI   ADTTTY2,C'X'                                                     
         MVI   ADTTTY2H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS64C    CLI   WBPYTYP,WBPT2WM     IF PAY TYPE IS 2 WEEKS - MULTI               
         JNE   PS64D                                                            
         MVI   ADTTTY1,C'X'        POPULATE 2 WEEK FIELD                        
         MVI   ADTTTY1H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS64D    CLI   WBPYTYP,WBPT2WMU    IF PAY TYPE IS 2 WEEKS - MULTI               
         JNE   PS64E               UNLIMITED                                    
         MVI   ADTTTY1,C'X'        POPULATE 2 WEEK FIELD                        
         MVI   ADTTTY1H+5,1                                                     
         MVI   ADTTTY3,C'X'        AND UNLIMITED FIELD                          
         MVI   ADTTTY3H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS64E    CLI   WBPYTYP,WBPT2WS     IF PAY TYPE IS 2 WEEKS - SINGLE              
         JNE   PS64F                                                            
         MVI   ADTTTY1,C'X'        POPULATE 2 WEEK FIELD                        
         MVI   ADTTTY1H+5,1                                                     
         MVI   ADTTTY4,C'X'        AND SINGLE MARKET FIELD                      
         MVI   ADTTTY4H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS64F    CLI   WBPYTYP,WBPT2WSU    IF PAY TYPE IS 2 WEEKS - SINGLE              
         JNE   PS64G               UNLIMITED                                    
         MVI   ADTTTY1,C'X'        POPULATE 2 WEEK FIELD                        
         MVI   ADTTTY1H+5,1                                                     
         MVI   ADTTTY3,C'X'        AND UNLIMITED FIELD                          
         MVI   ADTTTY3H+5,1                                                     
         MVI   ADTTTY4,C'X'        AND SINGLE MARKET FIELD                      
         MVI   ADTTTY4H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS64G    CLI   WBPYTYP,WBPT4WK     IF PAY TYPE IS 4 WEEKS                       
         JNE   PS64H                                                            
         MVI   ADTTTY3,C'X'        POPULATE 4 WEEK FIELD                        
         MVI   ADTTTY3H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS64H    CLI   WBPYTYP,WBPT31D     IF PAY TYPE IS 31 DAYS                       
         JNE   PS64I                                                            
         MVI   ADTTTY2,C'X'        POPULATE 31 DAY FIELD                        
         MVI   ADTTTY2H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS64I    CLI   WBPYTYP,WBPT13W     IF PAY TYPE IS 13 WEEKS                      
         JNE   PS64K                                                            
         CLC   SVCOADST,=C'KS'                                                  
         JNE   PS64J                                                            
         MVI   ADTTTY3,C'X'        POPULATE 13 WEEKS FIELD                      
         MVI   ADTTTY3H+5,1                                                     
         J     POPSCRC1                                                         
PS64J    MVI   ADTTTY4,C'X'                                                     
         MVI   ADTTTY4H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS64K    CLI   WBPYTYP,WBPT13M     IF PAY TYPE IS 13 WEEKS - MULTI              
         JNE   PS64L                                                            
         MVI   ADTTTY2,C'X'        POPULATE 13 WEEKS FIELD                      
         MVI   ADTTTY2H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS64L    CLI   WBPYTYP,WBPT13MU    IF PAY TYPE IS 13 WEEKS - MULTI              
         JNE   PS64M               UNLIMITED                                    
         MVI   ADTTTY2,C'X'        POPULATE 13 WEEKS FIELD                      
         MVI   ADTTTY2H+5,1                                                     
         MVI   ADTTTY3,C'X'        AND UNLIMITED FIELD                          
         MVI   ADTTTY3H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS64M    CLI   WBPYTYP,WBPT13S     IF PAY TYPE IS 13 WEEKS - SINGLE             
         JNE   PS64N                                                            
         MVI   ADTTTY2,C'X'        POPULATE 13 WEEKS FIELD                      
         MVI   ADTTTY2H+5,1                                                     
         MVI   ADTTTY4,C'X'        AND SINGLE MARKET FIELD                      
         MVI   ADTTTY4H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS64N    CLI   WBPYTYP,WBPT13SU    IF PAY TYPE IS 13 WEEKS - SINGLE             
         JE    *+6                                                              
         DC    H'00'                                                            
         MVI   ADTTTY2,C'X'        POPULATE 13 WEEKS FIELD                      
         MVI   ADTTTY2H+5,1                                                     
         MVI   ADTTTY3,C'X'        AND UNLIMITED FIELD                          
         MVI   ADTTTY3H+5,1                                                     
         MVI   ADTTTY4,C'X'        AND SINGLE MARKET FIELD                      
         MVI   ADTTTY4H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
***********************************************************************         
*        ROUTINE TO POPULATE ADDENDUM RADIO SESSION SCREEN WITH       *         
*        VALUES FROM REQUEST                                          *         
***********************************************************************         
                                                                                
POPSCR65 CLC   SVCOADST,=C'TX'     SKIP AHEAD IF ADDENDUM STATE                 
         JE    POPSCRC1            IS TEXAS                                     
                                                                                
         CLI   WBPYTYP,WBPT3DY     IF PAY TYPE IS 3 DAYS                        
         JNE   PS65A                                                            
         MVI   ADOTTY1,C'X'        POPULATE 3 DAY FIELD                         
         MVI   ADOTTY1H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS65A    CLI   WBPYTYP,WBPT1WK     IF PAY TYPE IS 1 WEEK                        
         JNE   PS65C                                                            
         CLC   SVCOADST,=C'KS'                                                  
         JNE   PS65B                                                            
         MVI   ADOTTY1,C'X'        POPULATE 1 WEEK FIELD                        
         MVI   ADOTTY1H+5,1                                                     
         J     POPSCRC1                                                         
PS65B    MVI   ADOTTY2,C'X'                                                     
         MVI   ADOTTY2H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS65C    CLI   WBPYTYP,WBPT2WM     IF PAY TYPE IS 2 WEEKS - MULTI               
         JNE   PS65D                                                            
         MVI   ADOTTY1,C'X'        POPULATE 2 WEEK FIELD                        
         MVI   ADOTTY1H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS65D    CLI   WBPYTYP,WBPT2WMU    IF PAY TYPE IS 2 WEEKS - MULTI               
         JNE   PS65E               UNLIMITED                                    
         MVI   ADOTTY1,C'X'        POPULATE 2 WEEK FIELD                        
         MVI   ADOTTY1H+5,1                                                     
         MVI   ADOTTY3,C'X'        AND UNLIMITED FIELD                          
         MVI   ADOTTY3H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS65E    CLI   WBPYTYP,WBPT2WS     IF PAY TYPE IS 2 WEEKS - SINGLE              
         JNE   PS65F                                                            
         MVI   ADOTTY1,C'X'        POPULATE 2 WEEK FIELD                        
         MVI   ADOTTY1H+5,1                                                     
         MVI   ADOTTY4,C'X'        AND SINGLE MARKET FIELD                      
         MVI   ADOTTY4H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS65F    CLI   WBPYTYP,WBPT2WSU    IF PAY TYPE IS 2 WEEKS - SINGLE              
         JNE   PS65G               UNLIMITED                                    
         MVI   ADOTTY1,C'X'        POPULATE 2 WEEK FIELD                        
         MVI   ADOTTY1H+5,1                                                     
         MVI   ADOTTY3,C'X'        AND UNLIMITED FIELD                          
         MVI   ADOTTY3H+5,1                                                     
         MVI   ADOTTY4,C'X'        AND SINGLE MARKET FIELD                      
         MVI   ADOTTY4H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS65G    CLI   WBPYTYP,WBPT4WK     IF PAY TYPE IS 4 WEEKS                       
         JNE   PS65H                                                            
         MVI   ADOTTY3,C'X'        POPULATE 4 WEEK FIELD                        
         MVI   ADOTTY3H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS65H    CLI   WBPYTYP,WBPT31D     IF PAY TYPE IS 31 DAYS                       
         JNE   PS65I                                                            
         MVI   ADOTTY2,C'X'        POPULATE 31 DAY FIELD                        
         MVI   ADOTTY2H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS65I    CLI   WBPYTYP,WBPT13W     IF PAY TYPE IS 13 WEEKS                      
         JNE   PS65K                                                            
         CLC   SVCOADST,=C'KS'                                                  
         JNE   PS65J                                                            
         MVI   ADOTTY3,C'X'        POPULATE 13 WEEKS FIELD                      
         MVI   ADOTTY3H+5,1                                                     
         J     POPSCRC1                                                         
PS65J    MVI   ADOTTY4,C'X'                                                     
         MVI   ADOTTY4H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS65K    CLI   WBPYTYP,WBPT13M     IF PAY TYPE IS 13 WEEKS - MULTI              
         JNE   PS65L                                                            
         MVI   ADOTTY2,C'X'        POPULATE 13 WEEKS FIELD                      
         MVI   ADOTTY2H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS65L    CLI   WBPYTYP,WBPT13MU    IF PAY TYPE IS 13 WEEKS - MULTI              
         JNE   PS65M               UNLIMITED                                    
         MVI   ADOTTY2,C'X'        POPULATE 13 WEEKS FIELD                      
         MVI   ADOTTY2H+5,1                                                     
         MVI   ADOTTY3,C'X'        AND UNLIMITED FIELD                          
         MVI   ADOTTY3H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS65M    CLI   WBPYTYP,WBPT13S     IF PAY TYPE IS 13 WEEKS - SINGLE             
         JNE   PS65N                                                            
         MVI   ADOTTY2,C'X'        POPULATE 13 WEEKS FIELD                      
         MVI   ADOTTY2H+5,1                                                     
         MVI   ADOTTY4,C'X'        AND SINGLE MARKET FIELD                      
         MVI   ADOTTY4H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
PS65N    CLI   WBPYTYP,WBPT13SU    IF PAY TYPE IS 13 WEEKS - SINGLE             
         JE    *+6                                                              
         DC    H'00'                                                            
         MVI   ADOTTY2,C'X'        POPULATE 13 WEEKS FIELD                      
         MVI   ADOTTY2H+5,1                                                     
         MVI   ADOTTY3,C'X'        AND UNLIMITED FIELD                          
         MVI   ADOTTY3H+5,1                                                     
         MVI   ADOTTY4,C'X'        AND SINGLE MARKET FIELD                      
         MVI   ADOTTY4H+5,1                                                     
         J     POPSCRC1                                                         
                                                                                
***********************************************************************         
*        ROUTINE TO POPULATE PRINT SCREEN WITH VALUES FROM REQUEST    *         
***********************************************************************         
                                                                                
POPSCR69 MVC   PRTAREA,WBPYARE                                                  
         OC    PRTAREA,SPACES                                                   
         MVI   PRTAREAH+5,L'PRTAREA                                             
                                                                                
         MVC   PRTUSE,WBPYPRU                                                   
         OC    PRTUSE,SPACES                                                    
         MVI   PRTUSEH+5,L'PRTUSE                                               
                                                                                
         OC    WBPYAPO,WBPYAPO     IF PROVIDED, POPULATE AUTH/PO FIELD          
         JZ    PSC69A                                                           
         MVC   PRTAUTH,WBPYAPO                                                  
         GOTOR SETLEN,DMCB,PRTAUTHH,WBPYAPO,L'WBPYAPO                           
                                                                                
PSC69A   OC    WBPYEST,WBPYEST     IF PROVIDED, POPULATE ESTIMATE FIELD         
         JZ    PSC69B                                                           
         MVC   PRTEST,WBPYEST                                                   
         GOTOR SETLEN,DMCB,PRTESTH,WBPYEST,L'WBPYEST                            
                                                                                
PSC69B   OC    WBPYPER,WBPYPER     IF PROVIDED, POPULATE PERIOD FIELD           
         JZ    PSC69C                                                           
         MVC   PRTPD,WBPYPER                                                    
         GOTOR SETLEN,DMCB,PRTPDH,WBPYPER,L'WBPYPER                             
                                                                                
PSC69C   MVC   PRTCYC,WBPYCYC      POPULATE CYCLE FIELD                         
         GOTOR SETLEN,DMCB,PRTCYCH,WBPYCYC,L'WBPYCYC                            
                                                                                
         OC    WBPYHCM,WBPYHCM                                                  
         JZ    PSC69D                                                           
         MVC   PRTHCOM,WBPYHCM     POPULATE HISTORY COMMENT FIELD               
         GOTOR SETLEN,DMCB,PRTHCOMH,WBPYHCM,L'WBPYHCM                           
                                                                                
PSC69D   OC    WBPYOPT,WBPYOPT                                                  
         JZ    PSC69E                                                           
         MVC   PRTOPT2,WBPYOPT     POPULATE OPTIONS FIELD                       
         GOTOR SETLEN,DMCB,PRTOPT2H,WBPYOPT,L'WBPYOPT                           
                                                                                
PSC69E   OC    WBPYICM,WBPYICM                                                  
         JZ    XIT                                                              
         MVC   PRTICOM,WBPYICM     POPULATE INVOICE COMMENT FIELD               
         GOTOR SETLEN,DMCB,PRTICOMH,WBPYICM,L'WBPYICM                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO POPULATE TAG SCREEN WITH VALUES FROM REQUEST      *         
***********************************************************************         
                                                                                
POPSCR6A EDIT  WBPYTAG,TAGTAG,ALIGN=LEFT                                        
         STC   R0,TAGTAGH+5                                                     
                                                                                
         MVC   TAGSESS,WBPY1TS                                                  
         MVI   TAGSESSH+5,1                                                     
         J     POPSCRC2                                                         
                                                                                
***********************************************************************         
*        ROUTINE TO POPULATE DEMO SCREEN WITH VALUES FROM REQUEST     *         
***********************************************************************         
                                                                                
POPSCR70 EDIT  WBPYDEM,DEMDEMO,ALIGN=LEFT                                       
         STC   R0,DEMDEMOH+5                                                    
         J     POPSCRC2                                                         
                                                                                
***********************************************************************         
*        ROUTINE TO POPULATE COMMON SCREEN 1 WITH VALUES FROM REQUEST *         
***********************************************************************         
                                                                                
POPSCRC1 OC    WBPYAPO,WBPYAPO     IF PROVIDED, POPULATE AUTH/PO FIELD          
         JZ    PSC1A                                                            
         MVC   BSSAUTH,WBPYAPO                                                  
         GOTOR SETLEN,DMCB,BSSAUTHH,WBPYAPO,L'WBPYAPO                           
                                                                                
PSC1A    OC    WBPYEST,WBPYEST     IF PROVIDED, POPULATE ESTIMATE FIELD         
         JZ    PSC1B                                                            
         MVC   BSSEST,WBPYEST                                                   
         GOTOR SETLEN,DMCB,BSSESTH,WBPYEST,L'WBPYEST                            
                                                                                
PSC1B    OC    WBPYPER,WBPYPER     IF PROVIDED, POPULATE PERIOD FIELD           
         JZ    PSC1C                                                            
         MVC   BSSPD,WBPYPER                                                    
         GOTOR SETLEN,DMCB,BSSPDH,WBPYPER,L'WBPYPER                             
                                                                                
PSC1C    MVC   BSSCYC,WBPYCYC      POPULATE CYCLE FIELD                         
         GOTOR SETLEN,DMCB,BSSCYCH,WBPYCYC,L'WBPYCYC                            
                                                                                
         OC    WBPYHCM,WBPYHCM                                                  
         JZ    PSC1D                                                            
         MVC   BSSHCOM,WBPYHCM     POPULATE HISTORY COMMENT FIELD               
         GOTOR SETLEN,DMCB,BSSHCOMH,WBPYHCM,L'WBPYHCM                           
                                                                                
PSC1D    OC    WBPYOPT,WBPYOPT                                                  
         JZ    PSC1E                                                            
         MVC   BSSOPT2,WBPYOPT     POPULATE OPTIONS FIELD                       
         GOTOR SETLEN,DMCB,BSSOPT2H,WBPYOPT,L'WBPYOPT                           
                                                                                
PSC1E    OC    WBPYICM,WBPYICM                                                  
         JZ    XIT                                                              
         MVC   BSSICOM,WBPYICM     POPULATE INVOICE COMMENT FIELD               
         GOTOR SETLEN,DMCB,BSSICOMH,WBPYICM,L'WBPYICM                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO POPULATE COMMON SCREEN 2 WITH VALUES FROM REQUEST *         
***********************************************************************         
                                                                                
POPSCRC2 OC    WBPYAPO,WBPYAPO     IF PROVIDED, POPULATE AUTH/PO FIELD          
         JZ    PSC2A                                                            
         MVC   HLDAUTH,WBPYAPO                                                  
         GOTOR SETLEN,DMCB,DEMAUTHH,WBPYAPO,L'WBPYAPO                           
                                                                                
PSC2A    OC    WBPYEST,WBPYEST     IF PROVIDED, POPULATE ESTIMATE FIELD         
         JZ    PSC2B                                                            
         MVC   HLDEST,WBPYEST                                                   
         GOTOR SETLEN,DMCB,HLDESTH,WBPYEST,L'WBPYEST                            
                                                                                
PSC2B    OC    WBPYPER,WBPYPER     IF PROVIDED, POPULATE PERIOD FIELD           
         JZ    PSC2C                                                            
         MVC   HLDPD,WBPYPER                                                    
         GOTOR SETLEN,DMCB,HLDPDH,WBPYPER,L'WBPYPER                             
                                                                                
PSC2C    MVC   HLDCYC,WBPYCYC      POPULATE CYCLE FIELD                         
         GOTOR SETLEN,DMCB,HLDCYCH,WBPYCYC,L'WBPYCYC                            
                                                                                
         OC    WBPYHCM,WBPYHCM                                                  
         JZ    PSC2D                                                            
         MVC   HLDHCOM,WBPYHCM     POPULATE HISTORY COMMENT FIELD               
         GOTOR SETLEN,DMCB,HLDHCOMH,WBPYHCM,L'WBPYHCM                           
                                                                                
PSC2D    OC    WBPYOPT,WBPYOPT                                                  
         JZ    PSC2E                                                            
         LA    R2,CLAOPTH                                                       
         CLI   TGFASCRN,SCR5B                                                   
         JE    *+8                                                              
         LA    R2,HLDOPTH                                                       
         MVC   8(L'HLDOPT,R2),WBPYOPT      POPULATE OPTIONS FIELD               
         GOTOR SETLEN,DMCB,0(R2),WBPYOPT,L'WBPYOPT                              
                                                                                
PSC2E    OC    WBPYICM,WBPYICM                                                  
         JZ    XIT                                                              
         MVC   HLDICOM,WBPYICM     POPULATE INVOICE COMMENT FIELD               
         GOTOR SETLEN,DMCB,HLDICOMH,WBPYICM,L'WBPYICM                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PASSES CONTROL TO PAY PROGRAM                        *         
***********************************************************************         
                                                                                
RUNPAY   NTR1  BASE=*,LABEL=*                                                   
         MVI   OVERLAY,X'52'       LOAD PAY CONTROL MODULE                      
         GOTO1 LOADSOPH,DMCB,0                                                  
         GOTO1 (R3),DMCB,(RC)      AND PASS CONTROL TO PROGRAM                  
                                                                                
         CLI   ERRTAB,X'FF'        SET CONDITION CODE BASED ON                  
         JE    YES                 WHETHER ANY ERRORS WERE                      
         J     NO                  ENCOUNTERED                                  
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RESETS WEB RESPONSE DETAILS FOR EXECUTE MODE         *         
*        ON ENTRY ... R8=A(WEB RESPONSE DETAILS AREA)                 *         
***********************************************************************         
                                                                                
RSTWRD   NTR1  BASE=*,LABEL=*                                                   
RW10     CLI   0(R8),X'FF'                                                      
         JE    XIT                                                              
         NI    WRSSTAT,WRSAPOVR+WRSAMOVR+WRSPHOVR+WRSMDOVR                      
         LA    R8,WRSLNQ(R8)                                                    
         J     RW10                                                             
         DROP  R8                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO OUTPUT DETAILS OF A SUCCESSFUL MF TRANSACTION     *         
***********************************************************************         
                                                                                
OUTSDET  NTR1  BASE=*,LABEL=*                                                   
         MVI   OUTPUT,PAYSTOK      OUTPUT SUCCESSFUL STATUS                     
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',1),         +        
               ('LD_CHARQ',OUTPUT),(1,0)                                        
                                                                                
         CLC   WBACTIN,=AL2(I#PJVULD) IF ACTION IS AUTH/PO EST/JOB              
         JNE   OSD10                  VALIDATION                                
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',4),         +        
               ('LD_CHARQ',WBPYAGY),(L'WBPYAGY,0)                               
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',5),           +        
               ('LD_CHARQ',WBPYCLI),(L'WBPYCLI,0)                               
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',6),           +        
               ('LD_CHARQ',WBPYPRD),(L'WBPYPRD,0)                               
                                                                                
OSD10    CLC   WBACTIN,=AL2(I#PAYULD) IF ACTION IS PAY                          
         JNE   XIT                                                              
         CLI   WBMODE,WBMDEXE         AND MODE IS EXECUTE                       
         JNE   OSD20                  OUTPUT INVOICE NUMBER                     
                                                                                
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A4',0)                                    
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         XC    TGINV,=6X'FF'                                                    
         GOTO1 TINVCON,DMCB,TGINV,OUTPUT,DATCON                                 
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',2),         +        
               ('LD_CHARQ',OUTPUT),(L'TGINV,0)                                  
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO              OUTPUT STAFF ID                              
         MVI   ELCODE,TAINELQ      AND DATE                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',3),         +        
               ('LD_CHARQ',TAINPST),(L'TAINPST,0)                               
                                                                                
         GOTO1 DATCON,DMCB,(1,TAINPDTE),(8,OUTPUT)                              
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',4),         +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
         DROP  R4                                                               
                                                                                
OSD20    BRAS  RE,OUTURG           OUTPUT URGENT CHECK RUN STATUS               
                                                                                
         TM    TGFASTAT,TGCRNOPY   IF NOT COMING FROM CERNO                     
         JO    XIT                                                              
         BRAS  RE,OUTCYC           OUTPUT PAYMENT CYCLE DATES                   
                                                                                
         CLI   WBMODE,WBMDEXE      IF MODE IS EXECUTE                           
         JNE   OSD30               OUTPUT DUE DATE                              
         GOTO1 DATCON,DMCB,(1,TGPDUEDT),(8,OUTPUT)                              
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',8),         +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
                                                                                
         OC    TGINV04A,TGINV04A   AND 2404A CANADIAN INVOICE                   
         JZ    OSD30                                                            
         GOTO1 TINVCON,DMCB,TGINV04A,OUTPUT,DATCON                              
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',9),         +        
               ('LD_CHARQ',OUTPUT),(6,0)                                        
                                                                                
OSD30    CLI   TGBTYPE,0                                                        
         JE    XIT                                                              
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',10),        +        
               ('LD_UBINQ',TGBTYPE),(L'TGBTYPE,0)                               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
PAYSTOK  EQU   C'1'                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO OUTPUT DETAILS OF A TRANSACTION WITH ERRORS       *         
***********************************************************************         
                                                                                
OUTEDET  NTR1  BASE=*,LABEL=*                                                   
         MVI   OUTPUT,PAYSTER      OUTPUT ERROR STATUS                          
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',1),         +        
               ('LD_CHARQ',OUTPUT),(1,0)                                        
                                                                                
         CLC   WBACTIN,=AL2(I#PJVULD)  IF ACTION IS AUTH/PO EST/JOB             
         JNE   OED10                   VALIDATION                               
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',4),         +        
               ('LD_CHARQ',WBPYAGY),(L'WBPYAGY,0)                               
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',5),           +        
               ('LD_CHARQ',WBPYCLI),(L'WBPYCLI,0)                               
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',6),           +        
               ('LD_CHARQ',WBPYPRD),(L'WBPYPRD,0)                               
                                                                                
OED10    CLC   WBACTIN,=AL2(I#PAYULD)  IF ACTION IS PAY                         
         JNE   OED20                                                            
         BRAS  RE,OUTURG               OUTPUT URGENT CHECK RUN STATUS           
                                                                                
         TM    TGFASTAT,TGCRNOPY   IF NOT COMING FROM CERNO                     
         JO    OED20                                                            
         BRAS  RE,OUTCYC           OUTPUT PAYMENT CYCLE DATES                   
         CLI   TGBTYPE,0           AND BILLING TYPE                             
         JE    OED20                                                            
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',10),        +        
               ('LD_UBINQ',TGBTYPE),(L'TGBTYPE,0)                               
                                                                                
OED20    BRAS  RE,OUTERRS                                                       
                                                                                
         USING EATENTD,R4                                                       
         LA    R4,EATTAB                                                        
OED30    CLI   0(R4),X'FF'                                                      
         JE    XIT                                                              
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTMAP',O#PAYEAT)            
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',1),           +        
               ('LD_UBINQ',EANUMB),(2,0)                                        
                                                                                
         CLI   EASTAT,EASCASQ                                                   
         JNE   OED40                                                            
         GOTO1 HEXOUT,DMCB,EAAPPSTO,OUTPUT,L'EAAPPSTO,0                         
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',2),         +        
               ('LD_CHARQ',OUTPUT),(4,0)                                        
         J     OED50                                                            
                                                                                
OED40    CLI   EASTAT,EASUQID                                                   
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',3),         +        
               ('LD_UBINQ',EAAPPSTO),(L'EAAPPSTO,0)                             
                                                                                
OED50    LA    R4,EALNQ(R4)                                                     
         J     OED30                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
PAYSTER  EQU   C'2'                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO OUTPUT URGENT CHECK RUN STATUS                    *         
***********************************************************************         
                                                                                
OUTURG   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'20',0)                                    
         JE    *+6                                                              
         DC    H'00'               READ SYSTEM RECORD                           
                                                                                
         USING TASYD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TASYELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVI   OUTPUT,C'1'                                                      
         CLC   TGTODAY1,TASYLKDT                                                
         JH    OURG10                                                           
         MVI   OUTPUT,C'2'                                                      
                                                                                
OURG10   GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',5),         +        
               ('LD_CHARQ',OUTPUT),(1,0)                                        
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO OUTPUT PAYMENT CYCLE DATES AND DUE DATE           *         
***********************************************************************         
                                                                                
OUTCYC   NTR1  BASE=*,LABEL=*                                                   
         OC    TGPCYCS,TGPCYCS     OUTPUT CYCLE START DATE                      
         JZ    OCYC10                                                           
         GOTO1 DATCON,DMCB,(1,TGPCYCS),(8,OUTPUT)                               
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',6),         +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
                                                                                
OCYC10   OC    TGPCYCE,TGPCYCE     OUTPUT CYCLE END DATE                        
         JZ    XIT                                                              
         GOTO1 DATCON,DMCB,(1,TGPCYCE),(8,OUTPUT)                               
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',7),         +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO OUTPUT COMMENT RECORDS                            *         
***********************************************************************         
                                                                                
OUTCMT   NTR1  BASE=*,LABEL=*                                                   
         TM    TGFASTAT,TGCRNOPY   IF REQUEST COMING FROM CERNO                 
         JZ    XIT                                                              
         CLI   WBMODE,WBMDRTV      AND MODE IS RETRIEVE                         
         JNE   XIT                 OUTPUT COMMENTS                              
                                                                                
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',WBPYCOM)                             
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TACMD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACMELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
OCMT10   BRAS  RE,NEXTEL                                                        
         JNE   OCMT20                                                           
         CLI   TACMTYPE,TACMTYPG                                                
         JNE   OCMT10                                                           
                                                                                
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTMAP',O#PAYCMT)            
                                                                                
         MVI   WORK,C'1'                                                        
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',1),         +        
               ('LD_CHARQ',WORK),(1,0)                                          
                                                                                
         MVC   OUTPUT,SPACES                                                    
         ZIC   RF,TACMLEN                                                       
         SHI   RF,3                                                             
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   OUTPUT(0),TACMCOMM                                               
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',2),         +        
               ('LD_CHARQ',OUTPUT),((RF),0)                                     
         DROP  R4                                                               
                                                                                
         USING TLCMD,R3                                                         
OCMT20   LA    R3,KEY              R3=A(COMMENT RECORD)                         
         XC    KEY,KEY                                                          
         MVI   TLCMCD,TLCMCDQ                                                   
         MVC   TLCMAGY,WBPYAGY                                                  
         MVI   TLCMTYP,TLCMTCOM                                                 
         MVC   TLCMCID,WBPYCID                                                  
         MVC   TLCMICOM,WBPYCOM                                                 
         GOTO1 HIGH                                                             
         J     OCMT40                                                           
OCMT30   GOTO1 SEQ                                                              
OCMT40   CLC   KEY(TLCMVER-TLCMD),KEYSAVE                                       
         JNE   OCMT60                                                           
         CLI   TLCMVER,1                                                        
         JH    OCMT60                                                           
                                                                                
         MVI   WORK,C'2'                                                        
         CLI   TLCMLEV,TLCMTPC                                                  
         JNE   *+8                                                              
         MVI   WORK,C'3'                                                        
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TAXCD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAXCELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
OCMT50   BRAS  RE,NEXTEL                                                        
         JNE   OCMT30                                                           
                                                                                
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTMAP',O#PAYCMT)            
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',1),           +        
               ('LD_CHARQ',WORK),(1,0)                                          
                                                                                
         MVC   OUTPUT,SPACES                                                    
         ZIC   RF,TAXCLEN                                                       
         SHI   RF,4                                                             
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   OUTPUT(0),TAXCCMNT                                               
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',2),         +        
               ('LD_CHARQ',OUTPUT),((RF),0)                                     
         J     OCMT50                                                           
         DROP  R4                                                               
                                                                                
OCMT60   XR    R5,R5               R5=HISTORY COMMENT COUNTER                   
                                                                                
         USING TLHCD,R3                                                         
         XC    KEY,KEY                                                          
         MVI   TLHCCD,TLHCCDQ                                                   
         MVC   TLHCCOM,WBPYCOM                                                  
         GOTO1 HIGH                                                             
         J     OCMT80                                                           
OCMT70   GOTO1 SEQ                                                              
OCMT80   CLC   KEY(TLHCSPR2-TLHCD),KEYSAVE                                      
         JNE   XIT                                                              
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TACMD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACMELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   OCMT70                                                           
                                                                                
         CLI   TACMCOMM,X'E1'      DO NOT SEND OVER COMMENT WITH                
         JE    OCMT70              UNPRINTABLE CHARACTER                        
                                                                                
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTMAP',O#PAYCMT)            
                                                                                
         MVI   WORK,C'4'                                                        
         GOTOR (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',1),           +        
               ('LD_CHARQ',WORK),(1,0)                                          
                                                                                
         MVC   OUTPUT,SPACES                                                    
         ZIC   RF,TACMLEN                                                       
         SHI   RF,3                                                             
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   OUTPUT(0),TACMCOMM                                               
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',2),         +        
               ('LD_CHARQ',OUTPUT),((RF),0)                                     
                                                                                
         AHI   R5,1                                                             
         CHI   R5,25                                                            
         JL    OCMT70                                                           
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAADDEAR                                                       
***********************************************************************         
*        PROCESS TIMESHEET UPLOAD REQUEST                             *         
***********************************************************************         
                                                                                
PROTIM   NTR1  BASE=*,LABEL=*                                                   
         CLC   WBACTIN,=AL2(I#TMULD)   IF ACTION IS TIMESHEET UPLOAD            
         JNE   XIT                                                              
                                                                                
         BRAS  RE,INIVARS          INITIALIZE VARIABLES                         
                                                                                
         BRAS  RE,ASRTREQ          ASSERT ALL REQUIRED FIELDS PROVIDED          
         JNE   PT10                                                             
         BRAS  RE,ASRTVAL          ASSERT ALL FIELDS HAVE VALID VALUES          
         JNE   PT10                                                             
                                                                                
         BRAS  RE,FMTREQ           FORMAT REQUEST FIELDS                        
         JNE   PT10                                                             
         BRAS  RE,VALREQ           VALIDATE REQUEST FIELDS                      
         JNE   PT10                                                             
                                                                                
         BAS   RE,SETTACT          SET TIMESHEET ACTION                         
         BRAS  RE,BLDTSCR          BUILD TIMESHEET SCREEN                       
         BRAS  RE,RUNTIME          RUN TIMESHEET PROGRAM                        
         JNE   PT10                                                             
                                                                                
         MVI   OUTPUT,TIMSTOK      OUTPUT SUCCESSFUL STATUS                     
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',1),         +        
               ('LD_CHARQ',OUTPUT),(1,0)                                        
         CLI   WBMODE,WBMDEXE                                                   
         JNE   PT20                                                             
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',2),         +        
               ('LD_CHARQ',TGCTSTAF),(L'TGCTSTAF,0)                             
         J     PT20                                                             
                                                                                
PT10     MVI   OUTPUT,PAYSTER      OUTPUT ERROR STATUS                          
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',1),         +        
               ('LD_CHARQ',OUTPUT),(1,0)                                        
         OI    PROSTAT,PSERWBTS                                                 
                                                                                
PT20     GOTO1 HEXOUT,DMCB,WBTMCOM,OUTPUT,L'WBTMCOM,0                           
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',4),         +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',5),         +        
               ('LD_CHARQ',WBTMINV),(L'WBTMINV,0)                               
         GOTO1 HEXOUT,DMCB,WBTMSEQ,OUTPUT,L'WBTMSEQ,0                           
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',6),         +        
               ('LD_CHARQ',OUTPUT),(4,0)                                        
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTRAW',7),         +        
               ('LD_CHARQ',WBTMDAT),(L'WBTMDAT,0)                               
                                                                                
         BRAS  RE,OUTERRS                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
TIMSTOK  EQU   C'1'                                                             
TIMSTER  EQU   C'2'                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SET TIMESHEET ACTION                                         *         
***********************************************************************         
                                                                                
SETTACT  NTR1                                                                   
         MVI   ACTNUM,ACTCHA       DEFAULT TO ACTION CHANGE                     
                                                                                
         USING TLTMD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLTMCD,TLTMCDQ                                                   
         MVI   TLTMSTA,TLTMSWEB                                                 
         MVC   TLTMCOM,WBTMCOM                                                  
         MVC   TLTMINV,TGINV                                                    
         XC    TLTMINV,=6X'FF'                                                  
         MVC   TLTMSSN,TGSSN                                                    
         MVC   TLTMSORT,TGCSORT                                                 
         MVC   TLTMCAT,TGCAT                                                    
         GOTO1 HIGH                                                             
         CLC   TLTMKEY,KEYSAVE     IF TIMESHEET IS NOT FOUND                    
         JE    XIT                                                              
         DROP  R3                                                               
                                                                                
         MVI   ACTNUM,ACTADD       SET TO ACTION ADD                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PROCESS TIMESHEET UPLOAD REQUEST                             *         
***********************************************************************         
                                                                                
BLDTSCR  NTR1  BASE=*,LABEL=*                                                   
         L     R3,EFHTAG                                                        
         MVI   OVERLAY,X'E7'                                                    
         GOTO1 LOADSOPH,DMCB,1     LOAD TIMESHEET SCREEN                        
         MVC   TWASCR,OVERLAY                                                   
                                                                                
         MVC   CONREC(4),=C'TIME'  POPULATE RECORD                              
         MVI   CONRECH+5,4                                                      
                                                                                
         MVC   CONACT,=CL8'CHANGE'                                              
         MVI   CONACTH+5,6                                                      
         CLI   ACTNUM,ACTCHA                                                    
         JE    *+10                                                             
         MVC   CONACT,=CL8'ADD'    POPULATE ACTION                              
         MVI   CONACTH+5,3                                                      
                                                                                
         MVC   STMAGY,TGAGY        POPULATE AGENCY                              
         GOTOR SETLEN,DMCB,STMAGYH,TGAGY,L'TGAGY                                
                                                                                
         MVC   STMINV,WBTMINV      POPULATE INVOICE                             
         GOTOR SETLEN,DMCB,STMINVH,WBTMINV,L'WBTMINV                            
                                                                                
         MVC   STMCID,TGCID        POPULATE COMMERCIAL ID                       
         GOTOR SETLEN,DMCB,STMCIDH,TGCID,L'TGCID                                
                                                                                
         MVC   STMSSN,TGSSN        POPULATE SSN                                 
         GOTOR SETLEN,DMCB,STMSSNH,TGSSN,L'TGSSN                                
                                                                                
         MVC   STMCAT,TGCAT        POPULATE CATEGORY                            
         GOTOR SETLEN,DMCB,STMCATH,TGCAT,L'TGCAT                                
                                                                                
         MVC   STMDATE,WBTMDAT     POPULATE DATE                                
         GOTOR SETLEN,DMCB,STMDATEH,WBTMDAT,L'WBTMDAT                           
                                                                                
         MVC   STMSPOT,WBTMSPT     POPULATE SPOTS                               
         GOTOR SETLEN,DMCB,STMSPOTH,WBTMSPT,L'WBTMSPT                           
                                                                                
         MVC   STMTAGS,WBTMTAG     POPULATE TAGS                                
         GOTOR SETLEN,DMCB,STMTAGSH,WBTMTAG,L'WBTMTAG                           
                                                                                
         MVC   STMWTST,WBTMWST     POPULATE WORK START TIME                     
         GOTOR SETLEN,DMCB,STMWTSTH,WBTMWST,L'WBTMWST                           
                                                                                
         MVC   STMWTNT,WBTMWET     POPULATE WORK END TIME                       
         GOTOR SETLEN,DMCB,STMWTNTH,WBTMWET,L'WBTMWET                           
                                                                                
         MVC   STMM1ND,WBTMNDB     POPULATE NON-DEDUCTIBLE BREAKFAST?           
         GOTOR SETLEN,DMCB,STMM1NDH,WBTMNDB,L'WBTMNDB                           
                                                                                
         MVC   STMM1ST,WBTMM1S     POPULATE MEAL 1 START TIME                   
         GOTOR SETLEN,DMCB,STMM1STH,WBTMM1S,L'WBTMM1S                           
                                                                                
         MVC   STMM1NT,WBTMM1E     POPULATE MEAL 1 END TIME                     
         GOTOR SETLEN,DMCB,STMM1NTH,WBTMM1E,L'WBTMM1E                           
                                                                                
         MVC   STMM2ST,WBTMM2S     POPULATE MEAL 2 START TIME                   
         GOTOR SETLEN,DMCB,STMM2STH,WBTMM2S,L'WBTMM2S                           
                                                                                
         MVC   STMM2NT,WBTMM2E     POPULATE MEAL 2 END TIME                     
         GOTOR SETLEN,DMCB,STMM2NTH,WBTMM2E,L'WBTMM2E                           
                                                                                
         MVC   STMM3ST,WBTMM3S     POPULATE MEAL 3 START TIME                   
         GOTOR SETLEN,DMCB,STMM3STH,WBTMM3S,L'WBTMM3S                           
                                                                                
         MVC   STMM3NT,WBTMM3E     POPULATE MEAL 3 END TIME                     
         GOTOR SETLEN,DMCB,STMM3NTH,WBTMM3E,L'WBTMM3E                           
                                                                                
         MVC   STMPDDT,WBTMPDD     POPULATE PRIOR DAY WARDROBE DATE             
         GOTOR SETLEN,DMCB,STMPDDTH,WBTMPDD,L'WBTMPDD                           
                                                                                
         MVC   STMPDST,WBTMPDS     POPULATE PDW START TIME                      
         GOTOR SETLEN,DMCB,STMPDSTH,WBTMPDS,L'WBTMPDS                           
                                                                                
         MVC   STMPDNT,WBTMPDE     POPULATE PDW END TIME                        
         GOTOR SETLEN,DMCB,STMPDNTH,WBTMPDE,L'WBTMPDE                           
                                                                                
         MVC   STMNHTP,WBTMNPR     POPULATE NIGHT PREMIUM?                      
         GOTOR SETLEN,DMCB,STMNHTPH,WBTMNPR,L'WBTMNPR                           
                                                                                
         MVC   STMMLPH,WBTMMP1     POPULATE MEAL PENALTY 1?                     
         GOTOR SETLEN,DMCB,STMMLPHH,WBTMMP1,L'WBTMMP1                           
                                                                                
         MVC   STMMLP2,WBTMMP2     POPULATE MEAL PENALTY 2?                     
         GOTOR SETLEN,DMCB,STMMLP2H,WBTMMP2,L'WBTMMP2                           
                                                                                
         MVC   STMMLP3,WBTMMP3     POPULATE MEAL PENALTY 3?                     
         GOTOR SETLEN,DMCB,STMMLP3H,WBTMMP3,L'WBTMMP3                           
                                                                                
         MVC   STMSMPY,WBTMSMK     POPULATE SMOKE PAY?                          
         GOTOR SETLEN,DMCB,STMSMPYH,WBTMSMK,L'WBTMSMK                           
                                                                                
         MVC   STM16YN,WBTM16H     POPULATE 16 HOUR RULE?                       
         GOTOR SETLEN,DMCB,STM16YNH,WBTM16H,L'WBTM16H                           
                                                                                
         MVC   STMADJ,WBTMADJ     POPULATE ADJUSTMENT                           
         GOTOR SETLEN,DMCB,STMADJH,WBTMADJ,L'WBTMADJ                            
                                                                                
         MVC   STMRHSL,WBTMREH     POPULATE REHEARSAL?                          
         GOTOR SETLEN,DMCB,STMRHSLH,WBTMREH,L'WBTMREH                           
                                                                                
         MVC   STMWTCN,WBTMWCX     POPULATE WEATHER CANCELLATION?               
         GOTOR SETLEN,DMCB,STMWTCNH,WBTMWCX,L'WBTMWCX                           
                                                                                
         MVC   STMRPVL,WBTMRPV     POPULATE REST PERIOD VIOLATION?              
         GOTOR SETLEN,DMCB,STMRPVLH,WBTMRPV,L'WBTMRPV                           
                                                                                
         MVC   STMNCWD,WBTMNCD     POPULATE NON-CONSECUTIVE DAY?                
         GOTOR SETLEN,DMCB,STMNCWDH,WBTMNCD,L'WBTMNCD                           
                                                                                
         MVC   STMDSLC,WBTMTDL     POPULATE TRAVEL TO DISTANT LOCATION?         
         GOTOR SETLEN,DMCB,STMDSLCH,WBTMTDL,L'WBTMTDL                           
                                                                                
         MVC   STMTTDP,WBTMTTD     POPULATE TRAVEL TO DEPART TIME               
         GOTOR SETLEN,DMCB,STMTTDPH,WBTMTTD,L'WBTMTTD                           
                                                                                
         MVC   STMTTAR,WBTMTTA     POPULATE TRAVEL TO ARRIVE TIME               
         GOTOR SETLEN,DMCB,STMTTARH,WBTMTTA,L'WBTMTTA                           
                                                                                
         MVC   STMTFDP,WBTMTFD     POPULATE TRAVEL FROM DEPART TIME             
         GOTOR SETLEN,DMCB,STMTFDPH,WBTMTFD,L'WBTMTFD                           
                                                                                
         MVC   STMTFAR,WBTMTFA     POPULATE TRAVEL FROM ARRIVE TIME             
         GOTOR SETLEN,DMCB,STMTFARH,WBTMTFA,L'WBTMTFA                           
                                                                                
         MVC   STMTIDP,WBTMTID     POPULATE TRAVEL INTER DEPART TIME            
         GOTOR SETLEN,DMCB,STMTIDPH,WBTMTID,L'WBTMTID                           
                                                                                
         MVC   STMTIAR,WBTMTIA     POPULATE TRAVEL INTER ARRIVE TIME            
         GOTOR SETLEN,DMCB,STMTIARH,WBTMTIA,L'WBTMTIA                           
                                                                                
         MVC   STMWANE,WBTMNWA     POPULATE NON-EVE WARDROBE ALLOTS             
         GOTOR SETLEN,DMCB,STMWANEH,WBTMNWA,L'WBTMNWA                           
                                                                                
         MVC   STMWAEV,WBTMEWA     POPULATE EVE WARDROBE ALLOTS                 
         GOTOR SETLEN,DMCB,STMWAEVH,WBTMEWA,L'WBTMEWA                           
                                                                                
         MVC   STMOTHR,WBTMOWA     POPULATE OTHER WARDROBE ALLOTS               
         GOTOR SETLEN,DMCB,STMOTHRH,WBTMOWA,L'WBTMOWA                           
                                                                                
         MVC   STMCMMT,WBTMCMT     POPULATE COMMENT                             
         GOTOR SETLEN,DMCB,STMCMMTH,WBTMCMT,L'WBTMCMT                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PASSES CONTROL TO TIMSHEET PROGRAM                   *         
***********************************************************************         
                                                                                
RUNTIME  NTR1  BASE=*,LABEL=*                                                   
         MVI   OVERLAY,X'E7'       LOAD TIMESHEET PROGRAM                       
         GOTO1 LOADSOPH,DMCB,0                                                  
         MVI   MODE,VALKEY         AND PASS CONTROL TO IT IN                    
         GOTO1 (R3),DMCB,(RC)      MODE VALKEY                                  
                                                                                
         CLI   ERRTAB,X'FF'        EXIT IF ANY ERRORS WERE                      
         JNE   XIT                 ENCOUNTERED                                  
                                                                                
         BAS   RE,STTMAIO          SET TIMSHEET IN AIO                          
                                                                                
         MVI   MODE,VALREC         PASS CONTROL BACK TO PROGRAM IN              
         GOTO1 (R3),DMCB,(RC)      MODE VALREC                                  
                                                                                
         CLI   ERRTAB,X'FF'        EXIT IF ANY ERRORS WERE                      
         JNE   NO                  ENCOUNTERED                                  
                                                                                
         BAS   RE,ADDTISD          ADD SESSION DETAILS ELEMENT                  
                                                                                
         BAS   RE,SVTIME           IF THIS IS NOT THE FINAL DAY OF THE          
         JE    YES                 TIMESHEET, SAVE IT IN WSSVR AREA             
                                                                                
         CLI   WBMODE,WBMDEXE      IF THIS IS THE FINAL DAY OF THE              
         JNE   YES                 TIMESHEET AND MODE IS EXECUTE                
         TM    PROSTAT,PSERWBTS    AND THERE WERE NO ERRORS FOR ANY             
         JO    YES                 DAYS OF THE TIMESHEET                        
                                                                                
         CLI   ACTNUM,ACTCHA       IF ACTION WAS CHANGE                         
         JNE   RT10                                                             
         GOTO1 PUTREC              PUT THE TIMESHEET                            
         MVI   MODE,XRECPUT        AND SET MODE TO XRECPUT                      
         J     RT20                                                             
                                                                                
RT10     GOTO1 ADDREC              IF ACTION WAS ADD, ADD THE                   
         MVI   MODE,XRECADD        TIMESHEET AND SET MODE TO XRECADD            
                                                                                
RT20     GOTO1 (R3),DMCB,(RC)      PASS CONTROL BACK TO PROGRAM IN              
         J     YES                 MODE XRECPUT/XRECADD                         
                                                                                
***********************************************************************         
*        ROUTINE SETS TIMESHEET IN AIO                                *         
***********************************************************************         
                                                                                
STTMAIO  NTR1                                                                   
         L     R4,AIO                                                           
                                                                                
         USING FAWSSVRD,R2                                                      
         TM    PROSTAT,PSTMFRST    IF THIS IS NOT THE FIRST DAY FOR             
         JO    STA10               THISE PERFORMER                              
         LA    R2,WORK             ATTEMPT TO RECALL PREVIOUSLY SAVED           
         XC    WORK(FAWSSVRL),WORK                          TIMESHEET           
         MVC   FAWSTOKN(L'WBTMSEQ),WBTMSEQ                                      
         OC    WBTMSEQ,WBTMSEQ                                                  
         JNZ   *+10                                                             
         MVC   FAWSTOKN,=C'HYPO'                                                
         MVI   FAWSACTN,FAWSARST                                                
         MVC   FAWSADR,AIO                                                      
         GOTO1 WSSVR,(R2)                                                       
         CLI   FAWSRTN,0                                                        
         JE    STA20                                                            
                                                                                
         USING TLRCD,R4                                                         
STA10    XC    0(250,R4),0(R4)     OTHERWISE                                    
         MVC   TLRCKEY,KEY         COPY KEY INTO AIO                            
         MVC   TLRCLEN,DATADISP    AND SET RECORD LENGTH                        
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
STA20    CLI   WBTMFDY,C'Y'        IF THIS IS THE FINAL DAY OF THE              
         JNE   XIT                 TIMESHEET                                    
         MVI   FAWSACTN,FAWSADEL   DELETE CAST SEQUENCE NUMBER-KEYED            
         GOTO1 WSSVR,(R2)          WSSVR AREA                                   
         CLI   FAWSRTN,0                                                        
         JE    XIT                                                              
         DC    H'00'                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ROUTINE SAVES TIMESHEET INFO INTO WSSVR AREA                 *         
***********************************************************************         
                                                                                
SVTIME   NTR1                                                                   
         CLI   WBTMFDY,C'Y'        EXIT IF THIS IS THE FINAL DAY                
         JE    NO                  OF THE TIMESHEET                             
                                                                                
         USING TLTMD,R4                                                         
         L     R4,AIO                                                           
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN(L'WBTMSEQ),WBTMSEQ                                      
         OC    WBTMSEQ,WBTMSEQ                                                  
         JNZ   *+10                                                             
         MVC   FAWSTOKN,=C'HYPO'                                                
         MVI   FAWSACTN,FAWSASVE                                                
         MVC   FAWSLEN,TLTMLEN                                                  
         ST    R4,FAWSADR          SAVE TIMESHEET RECORD INTO                   
         GOTO1 WSSVR,(R1)          CAST SEQUENCE NUMBER-KEYED                   
         CLI   FAWSRTN,0           WSSVR AREA                                   
         JE    YES                                                              
         DC    H'00'                                                            
         DROP  R1,R4                                                            
                                                                                
***********************************************************************         
*        ROUTINE ADDS SESSION DETAILS ELEMENT TO TIMESHEET            *         
***********************************************************************         
                                                                                
ADDTISD  NTR1                                                                   
         MVI   ELCODE,TASDELQ                                                   
         GOTO1 REMELEM                                                          
                                                                                
         USING TATTD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TATTELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
ATSD10   BRAS  RE,NEXTEL                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TATTDATE,=X'FFFFFF'                                              
         JNE   ATSD10                                                           
                                                                                
         USING TASDD,R2                                                         
         LA    R2,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TASDEL,TASDELQ                                                   
         MVI   TASDLEN,TASDLNQ                                                  
         MVC   TASDSP,TATTSPOT                                                  
         MVC   TASDDAY,TATTDAYS                                                 
         MVC   TASDOT,TATTOVTM                                                  
         MVC   TASDDT,TATTDBTM                                                  
         MVC   TASDTRV,TATTTRVL                                                 
         MVC   TASDPDW,TATTPDWD                                                 
         MVC   TASDTAG,TATTTAG                                                  
         GOTO1 ADDELEM                                                          
         J     XIT                                                              
         DROP  R2,R4                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SET LENGTH OF INPUT IN A SCREEN FIELD             *         
*        ON ENTRY ... P1=A(SCREEN FIELD HEADER)                       *         
*                     P2=A(WEB BLOCK FIELD)                           *         
*                     P3=L'WEB BLOCK FIELD                            *         
***********************************************************************         
                                                                                
SETLEN   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2=A(SCREEN FIELD HEADER)                    
         ZIC   R3,11(R1)           R3=L'WEB BLOCK FIELD                         
                                                                                
         L     RE,4(R1)            RE=A(WEB BLOCK FIELD)                        
                                                                                
         LR    RF,R3               COPY LENGTH                                  
         LA    R4,0(RF,RE)         POINT R4 TO LAST CHAR OF WEB FIELD           
         AHI   R4,-1                                                            
                                                                                
SLEN10   CLI   0(R4),C' '          FINDS LAST NON-BLANK CHAR                    
         BH    SLEN20              THAT'S THE NEW LENGTH                        
         AHI   R4,-1               PREVIOUS CHAR                                
         AHI   RF,-1               DECREMENT LENGTH                             
         BZ    SLEN20                                                           
         B     SLEN10                                                           
                                                                                
SLEN20   STC   RF,5(R2)                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO OUTPUT ERRORS FROM ERROR TABLE                    *         
***********************************************************************         
                                                                                
OUTERRS  NTR1  BASE=*,LABEL=*                                                   
         USING ERRENTD,R4                                                       
         LA    R4,ERRTAB                                                        
OE10     CLI   0(R4),X'FF'                                                      
         JE    XIT                                                              
                                                                                
         GOTO1 LINKIO,DMCB,('LIOAPUT',LIOBAREA),('LIOTMAP',O#PAYERR)            
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',1),           +        
               ('LD_UBINQ',EENUMB),(L'EENUMB,0)                                 
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',2),           +        
               ('LD_UBINQ',EECATY),(L'EECATY,0)                                 
                                                                                
         OC    EEFIELD2,EEFIELD2                                                
         JZ    OE20                                                             
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',3),           +        
               ('LD_UBINQ',EEFIELD2),(L'EEFIELD2,0)                             
         J     OE30                                                             
OE20     GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',3),           +        
               ('LD_UBINQ',EEFIELD),(L'EEFIELD,0)                               
                                                                                
OE30     ZIC   R0,EELEN                                                         
         SHI   R0,7                                                             
         GOTO1 (RF),(R1),('LIOAPUT',LIOBAREA),('LIOTRAW',4),           +        
               ('LD_CHARQ',EEMSG),((R0),0)                                      
                                                                                
         ZIC   R0,EELEN                                                         
         AR    R4,R0                                                            
         J     OE10                                                             
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO RECALL USE DETAILS WSSVR AREA                     *         
*        ON ENTRY ... P1=A(AREA TO RECALL USE DETAILS INTO)           *         
***********************************************************************         
                                                                                
REUSEDET NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN,=C'USDT'                                                
         MVI   FAWSACTN,FAWSARST   RECALL PREVIOUSLY SAVED USE DETAILS          
         ST    R2,FAWSADR          WSSVR AREA                                   
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         J     XIT                                                              
         DROP  R1                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ASSERTS THAT PROVIDED SPLIT INVOICE ESTIMATE         *         
*        NOT DUPLICATED                                               *         
*        ON ENTRY ... P1 = A(SPLIT INVOICE ESTIMATE)                  *         
***********************************************************************         
                                                                                
AVSINDUP NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2=A(SPLIT INVOICE ESTIMATE TO CHK)          
         XR    R0,R0                                                            
                                                                                
         OC    0(L'WBPYSI1,R2),0(R2)                                            
         JZ    YES                                                              
                                                                                
         CLC   WBPYSI1(16),0(R2)                                                
         JNE   *+8                                                              
         AHI   R0,1                                                             
                                                                                
         CLC   WBPYSI2(16),0(R2)                                                
         JNE   *+8                                                              
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AVISI2                                                           
                                                                                
         CLC   WBPYSI3(16),0(R2)                                                
         JNE   *+8                                                              
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AVISI3                                                           
                                                                                
         CLC   WBPYSI4(16),0(R2)                                                
         JNE   *+8                                                              
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AVISI4                                                           
                                                                                
         CLC   WBPYSI5(16),0(R2)                                                
         JNE   *+8                                                              
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AVISI5                                                           
                                                                                
         CLC   WBPYSI6(16),0(R2)                                                
         JNE   *+8                                                              
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AVISI6                                                           
                                                                                
         CLC   WBPYSI7(16),0(R2)                                                
         JNE   *+8                                                              
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AVISI7                                                           
                                                                                
         CLC   WBPYSI8(16),0(R2)                                                
         JNE   *+8                                                              
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AVISI8                                                           
                                                                                
         CLC   WBPYSI9(16),0(R2)                                                
         JNE   *+8                                                              
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AVISI9                                                           
                                                                                
         CLC   WBPYSIA(16),0(R2)                                                
         JNE   *+8                                                              
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JH    AVISIA                                                           
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TALENT PAY UPLOAD MAP CODES                                        *          
**********************************************************************          
                                                                                
       ++INCLUDE TAMAPPAY                                                       
       ++INCLUDE TAMAPPJV                                                       
       ++INCLUDE TAMAPTIM                                                       
       ++INCLUDE TAVALPJB                                                       
         PRINT OFF                                                              
       ++INCLUDE TAGENFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS13D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS15D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS50D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS55D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS5BD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS5FD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS64D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS65D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS66D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS69D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS6AD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS70D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS78D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS90D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRE7D                                                       
         PRINT ON                                                               
         EJECT                                                                  
* DDGENTWA    *** MUST FOLLOW LAST SCREEN ***                                   
* TAGENEQUS                                                                     
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TASYSEQUS                                                                     
* TAGENFILE                                                                     
* TAPAYBRKD                                                                     
* ACGENBOTH                                                                     
* DDCOMFACS                                                                     
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
* DDLINKD                                                                       
* DDLINKIOD                                                                     
* FAWSSVRD                                                                      
* TACLAD                                                                        
* TAMAPEQUS                                                                     
* TAWADSECT                                                                     
* TAWBDSECT                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAPAYBRKD                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDLINKD                                                        
       ++INCLUDE DDLINKIOD                                                      
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE TACLAD                                                         
       ++INCLUDE TAMAPEQUS                                                      
       ++INCLUDE TAWADSECT                                                      
         PRINT ON                                                               
       ++INCLUDE TAWBDSECT                                                      
                                                                                
         LKNDX GEN                 FIELD INDEX LIST                             
                                                                                
         DS    0F                                                               
LIOBAREA DS    XL(L'LIOB)          ** LINKIO CONTROL BLOCK **                   
                                                                                
OUTPUT   DS    CL250               OUTPUT BLOCK FOR LINKIO                      
                                                                                
**********************************************************************          
* LOCAL VARIABLES                                                    *          
**********************************************************************          
                                                                                
LVARS    DS    0X                                                               
SCRMODE  DS    XL1                 MODE FOR SETSCRN ROUTINE                     
                                                                                
SVKEY    DS    XL(L'KEY)           SAVED KEY                                    
                                                                                
*                                  SAVED AGENCY VARIABLES                       
SVAYSTAT DS    XL(L'TAAYSTAT)      STATUS                                       
SVAYSTA2 DS    XL(L'TAAYSTA2)      2ND STATUS                                   
SVAYSTA4 DS    XL(L'TAAYSTA4)      4TH STATUS                                   
SVAYSTA5 DS    XL(L'TAAYSTA5)      5TH STATUS                                   
SVAYSTA6 DS    XL(L'TAAYSTA6)      6TH STATUS                                   
SVAYSTA7 DS    XL(L'TAAYSTA7)      7TH STATUS                                   
SVAYBUNT DS    XL(L'TAAYBUNT)      BUSINESS UNIT                                
SVAYTJOB DS    XL3                 JOB SETUP LENGTH                             
SVAYIAGY DS    XL(L'TAAYIAGY)      PARENT AGENCY                                
SVDEFJOB DS    CL15                SAVED DEFAULT JOB                            
                                                                                
*                                  SAVED CLIENT VARIABLES                       
SVCLPICD DS    XL3                 PRODUCTION INTERFACE CODE                    
                                                                                
*                                  SAVED PRODUCT VARIABLES                      
SVPRPICD DS    XL3                 PRODUCTION INTERFACE CODE                    
                                                                                
*                                  SAVED AGENCY/CLIENT VARIABLES                
SVACBSTA DS    XL(L'TABRSTAT)      BILLING RULES STATUS                         
                                                                                
*                                  SAVED JOB VARIABLES                          
SVJBCODE DS    XL12                SAVED JOB CODE                               
SVJBKEY  DS    XL(L'TLJBKEY)       SAVED JOB KEY                                
                                                                                
*                                  SAVED COMMERCIAL VARIABLES                   
SVCOADST DS    CL2                 SAVED ADDENDUM STATE                         
                                                                                
ELTASD   DS    XL(TASDLNQ)         IN-PROGRESS SESSION DETAILS ELEMENT          
TRVLHRS  DS    XL1                 TOTAL TRAVEL TIME HOURS                      
TRVLMNS  DS    XL2                 TOTAL TRAVEL TIME MINUTES                    
LVLNQ    EQU   *-LVARS                                                          
                                                                                
PROSTAT  DS    X                   FALINK STATUS                                
PSTMFRST EQU   X'80'               FIRST ENCOUNTER WITH THIS TIMESHEET          
PSERWBTS EQU   X'40'               ERROR ENCOUNTERED FOR WEB TIMESHEET          
                                                                                
TMLINV   DS    CL6                 LAST DDLINK TIMESHEET INVOICE                
TMLSEQ   DS    XL2                 LAST DDLINK TIMESHEET CAST SEQUENCE          
                                                                                
SVPYBRK  DS    XL1                 SAVED BREAKDOWN OPTIONS                      
                                                                                
ISFIELD  DS    XL1                 ISPLIT FIELD NUMBER                          
                                                                                
PAYTOT   DS    XL4                 TOTAL EXPECTED DOLLARS                       
                                                                                
SPACES   DS    CL132                                                            
                                                                                
ERRECNUM DS    H                   ERROR RECORD NUMBER                          
                                                                                
ERRTAB   DS    XL1201              ERROR MESSAGE TABLE                          
                                                                                
EATTAB   DS    XL1001              ERROR APPLIES TO TABLE                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004TAGENFA   03/09/16'                                      
         END                                                                    
