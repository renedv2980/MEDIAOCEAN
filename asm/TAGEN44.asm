*          DATA SET TAGEN44    AT LEVEL 165 AS OF 08/16/16                      
*PHASE T70244B,*                                                                
         TITLE 'T70244 - INVOICE REOPEN/CANCEL'                                 
T70244   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70244,R7                                                      
         L     RC,0(R1)            RC=CONTROLLER STORAGE AREA                   
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         SPACE 1                                                                
         LA    RE,TWAHOLE                                                       
         ST    RE,ASVPTRS                                                       
         AHI   RE,(56*L'TLDRREC)+1                                              
         ST    RE,AUPPTRS                                                       
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 2                                                                
         CLI   MODE,VALKEY                                                      
         BNE   *+12                                                             
         BAS   RE,VKEY             VALIDATE KEY                                 
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         MVI   RDUPDATE,C'Y'          SET READ FOR UPDATE ALWAYS ON             
         NI    GENSTAT1,ALL-RDUPAPPL  BY TURNING OFF GENCON STATUS BIT          
         SPACE 1                                                                
         OI    APPHCHGH+1,X'0C'    SET LAST CHANGED FIELLDS TO ZERO INT         
         OI    APPHCHGH+6,X'80'                                                 
         XC    APPLCHG,APPLCHG                                                  
         OI    APPLCHGH+1,X'0C'                                                 
         OI    APPLCHGH+6,X'80'                                                 
         SPACE 1                                                                
         BAS   RE,PROCESS          PROCESS RECORDS                              
         OC    CANINV,CANINV       IF ALSO A CANADIAN INVOICE,                  
         BZ    *+8                                                              
         BAS   RE,PROCCAN          PROCESS CANADIAN INVOICE                     
         SPACE 1                                                                
         LA    R2,APPFSTH          R2=A(FIRST ERROR DISPLAY AREA)               
         L     R3,ATHISERR         R3=A(NEXT AVAILABLE ERROR DISPLAY)           
         CR    R2,R3               IF ANY ERRORS TO DISPLAY                     
         BE    *+16                                                             
         OI    APPHEDH+1,X'08'     MAKE HEADING HIGH INTENSITY                  
         NI    APPHEDH+1,X'FB'                                                  
         OI    APPHEDH+6,X'80'                                                  
         SPACE 1                                                                
         MVI   COMATCHD,NOCMT                                                   
         SPACE 1                                                                
         CLI   ACTNUM,ACTREOP      IF ACTION IS REOPEN                          
         BE    REOPENED            INVOICE REOPENED                             
         B     CANCELED            INVOICE CANCELLED                            
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 1                                                                
VKEY     NTR1                                                                   
         OI    APPHEDH+1,X'0C'     MAKE HEADING LOW INTENSITY                   
         OI    APPHEDH+6,X'80'                                                  
         LA    R2,APPFSTH          CLEAR ERRORS FROM SCREEN                     
         LA    R3,APPLSTH                                                       
         GOTO1 FLDVAL,DMCB,(X'01',(R2)),(R3)                                    
         SPACE                                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',APPAGYH)  VALIDATE AGENCY             
         SPACE                                                                  
         L     R3,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY EL                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TAAYD,R3                                                         
         MVC   AGYSTAT,TAAYSTAT    SAVE AGENCY STATUS                           
         TM    AGYSTAT,TAAYSCOD    IF THIS IS COD AGENCY                        
         BZ    *+10                                                             
         MVC   CONHED2(12),CODMSG  DISPLAY COD MSG                              
         SPACE 1                                                                
VK1      MVC   AIO,AIO1                                                         
         LA    R2,APPINVH          VALIDATE INVOICE NUMBER                      
         GOTO1 ANY                                                              
         GOTO1 TINVCON,DMCB,WORK,INVNO,DATCON                                   
         MVC   SCRINV,INVNO        SAVE FOR SCREEN RECORDS                      
         MVC   TGINV,INVNO                                                      
         XC    TGINV,HEXFFS        COMPLEMENT IT FOR KEY                        
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'20',0)                                    
         BNE   RECNTFND            RECORD NOT FOUND                             
         SPACE 1                                                                
         XC    SVWID,SVWID                                                      
         SPACE 1                                                                
         USING TAFND,R3                                                         
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         BNE   VK1A                                                             
         L     R3,TGELEM           ENSURE INVOICE IS NOT STAMPED                
         MVC   SVWID,TAFNNAME      WITH VITA SESSION ID                         
         DROP  R3                                                               
         SPACE 1                                                                
         USING TAPDD,R3                                                         
VK1A     MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   VK1B                                                             
         MVC   TGCLI,TAPDCLI                                                    
         MVC   TGCOM,TAPDCOM                                                    
         MVC   PAYOPT4,TAPDOPT4                                                 
         BRAS  RE,CHKGRT           ENSURE INVOICE IS ELIGIBLE FOR               
         BRAS  RE,CHKPCY           REOPEN IN REGARDS TO GUARANTEES              
         DROP  R3                                                               
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A0',0)       GET CLIENT RECORD            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   CLTSTAT,0                                                        
         MVI   ELCODE,TACIELQ     GET CLIENT INFO ELEMENT                       
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   VK1B                                                             
         USING TACID,R3                                                         
         MVC   CLTSTAT,TACISTAT                                                 
         TM    CLTSTAT,TACISCOD    IF THIS IS COD CLIENT                        
         BZ    *+10                                                             
         MVC   CONHED2(12),CODMSG2   DISPLAY COD MSG                            
         DROP  R3                                                               
         SPACE 1                                                                
VK1B     GOTO1 RECVAL,DMCB,TLINCDQ,(X'20',0)                                    
         BNE   RECNTFND            RECORD NOT FOUND                             
         SPACE 1                                                                
         MVI   SVTACO3,0                                                        
         L     R3,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   VK1BAA                                                           
         USING TACOD,R3                                                         
         CLI   TACOLEN,TACOLNQ2                                                 
         BL    VK1BAA                                                           
         MVC   SVTACO3,TACOSTA3    SAVE COMMERCIAL STATUS3                      
         DROP  R3                                                               
         SPACE 1                                                                
         USING TACOD,R3                                                         
VK1BAA   CLI   ACTNUM,ACTCAN       IF ACTION IS CANCEL                          
         BNE   VK1BA                                                            
         TM    TGAYSTA7,TAAYSPPL   AND THIS IS A P+ AGENCY                      
         BZ    VK1BA                                                            
         L     R3,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   VK1BA                                                            
         CLI   TACOMED,TACOMEDE    PAYMENT MUST BE TO AN EVENT                  
         BNE   ERPPLSI                                                          
         DROP  R3                                                               
         SPACE 1                                                                
VK1BA    XC    CANINV,CANINV                                                    
         MVI   ELCODE,TAUCELQ      GET US/CANADIAN INVOICE ELEMENT              
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   VK1C                                                             
         USING TAUCD,R3                                                         
         CLC   TGINV,TAUCINU       MUST BE US INVOICE                           
         BNE   NOTAVAIL                                                         
         MVC   CANINV,TAUCINC      SAVE CANADIAN INVOICE NUMBER                 
         DROP  R3                                                               
*                                                                               
VK1C     MVI   ELCODE,TAINELQ      GET INVOICE STATUS ELEMENT                   
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         USING TAIND,R3                                                         
         TM    TGCTSTST,TGCTSCLI   IF CONNECT STAFF TYPE IS CLIENT              
         BZ    VK2                                                              
         TM    TAINSTA2,TAINSSCR   TEST PAYMENT MADE BY A CLIENT                
         BZ    NOTAVAIL                                                         
         SPACE                                                                  
VK2      TM    TAINSTAT,TAINSPAY   INSURE IT'S BEEN PAID                        
         BZ    NOTPAID                                                          
         TM    TAINSTAT,TAINSCHK   DON'T ALLOW IF CHECKS WRITTEN                
         BO    NOTAVAIL                                                         
         SPACE 1                                                                
         MVI   CODPRT,C'N'         IF THIS INVOICE WAS PRINTED (COD)            
         TM    TAINSTA2,TAINSHLP                                                
         BZ    *+8                 SET A FLAG                                   
         MVI   CODPRT,C'Y'                                                      
         SPACE 1                                                                
         CLI   ACTNUM,ACTREOP      IF ACTION IS REOPEN                          
         BNE   VK5                                                              
         TM    TAINSTAT,TAINSBIL   DON'T ALLOW IF BILLED                        
         BO    NOTAVAIL                                                         
         TM    TAINSTAT,TAINSCIN   OR A CANCEL(ER) INVOICE                      
         BO    NOTAVAIL                                                         
         B     VK8                                                              
         SPACE 1                                                                
VK5      CLI   ACTNUM,ACTCAN       IF ACTION IS CANCEL                          
         BNE   VK8                                                              
         TM    TAINSTAT,TAINSCAN+TAINSCIN  DON'T ALLOW IF CANCEL(LED)           
         BNZ   NOTAVAIL                                                         
         TM    TAINSTA2,TAINSHLP   ALLOW IF COD & PRINTED                       
         BO    VK6                                                              
         TM    TAINSTAT,TAINSBIL   DON'T ALLOW IF NOT BILLED                    
         BZ    NOCANCEL                                                         
VK6      TM    TAINSTAT,TAINSHLD   OR IF COD HOLD                               
         BO    CODHOLD                                                          
         SPACE 1                                                                
VK8      TM    TAINSTA2,TAINSSCR   IF SPECIAL SCREEN RECORDS                    
         BZ    *+16                                                             
         MVC   SCRINV(3),TAINIDTE  RESET THE INVOICE NUM FOR SCREENS            
         MVC   SCRINV+3(3),TAINITIM                                             
         SPACE 1                                                                
         MVC   PAYDATE,TAINPDTE    SAVE PAYMENT DATE                            
         SPACE 1                                                                
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R3                                                         
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE  SET GLOBAL USE DETAILS             
         SPACE 1                                                                
         CLI   ACTNUM,ACTCAN       IF ACTION IS CANCEL                          
         BNE   *+12                                                             
         TM    TAPDSTA2,TAPDSSUB   & IF SUBSIDIARY INVOICE                      
         BO    NOTAVAIL            DON'T ALLOW                                  
*                                                                               
** TOM   TM    TAPDOPT3,TAPDODUM   DON'T ALLOW IF DUMMY PAYMENT                 
**9/16/94BO    NOTAVAIL                                                         
         SPACE 1                                                                
         CLI   TGCTSTTY,TASTTYPP   IF NOT A PROGRAMMER                          
         BE    VK9                 &                                            
         CLI   ACTNUM,ACTCAN       IF ACTION IS CANCEL                          
         BNE   *+12                                                             
         TM    TAPDPST1,TAPDPBNP   DON'T ALLOW IF BNP                           
         BO    NOTAVAIL                                                         
         SPACE 1                                                                
VK9      CLI   CODPRT,C'Y'         IF THIS IS A PRINTED COD                     
         BNE   VK20                                                             
         CLI   ACTNUM,ACTREOP      AND ACTION IS REOPEN                         
         BNE   VK10                                                             
         CLI   TGUSEQU,UGRT        AND THIS IS GUARANTEE PAYMENT                
         BE    NOTAVAIL            DON'T ALLOW                                  
         OC    TAPDGUAR,TAPDGUAR   OR IF GUARANTEE CREDITS WERE TAKEN           
         BNZ   NOTAVAIL            DON'T ALLOW                                  
         B     VK20                                                             
         SPACE 1                                                                
VK10     CLI   ACTNUM,ACTCAN       ELSE IF ACTION IS CANCEL                     
         BNE   VK20                                                             
         CLI   TGUSEQU,UGRT        AND THIS IS NOT A GUARANTEE PAYMENT          
         BE    VK20                                                             
         OC    TAPDGUAR,TAPDGUAR   & NO GUARANTEE CREDITS WERE TAKEN            
         BZ    NOTAVAIL            DON'T ALLOW                                  
         SPACE 1                                                                
VK20     MVC   TGCLI,TAPDCLI       SAVE CLIENT                                  
         MVC   TGPRD,TAPDPRD       PRODUCT                                      
         MVC   PAYSTAT,TAPDPST1    PAYMENT STATUS                               
         MVC   PAYSTAT2,TAPDPST2   PAYMENT STATUS 2                             
         MVC   PAYOPT3,TAPDOPT3    PAYMENT OPTION 3                             
         SPACE                                                                  
         BAS   RE,GETADV           GET ADVICE NUMBER PAID AND CID               
         SPACE                                                                  
         TM    PAYOPT3,TAPDORET    IF RETROACTIVE PAYMENT                       
         BZ    *+8                                                              
         BRAS  RE,GETORINV         GET ORIG INV # FROM HISTORY COMMENT          
         SPACE 1                                                                
         BAS   RE,SETSI            SET SUBSIDIARY INVOICE TABLE                 
         SPACE 1                                                                
         CLI   COMATCHD,CMTOK      SKIP AHEAD IF OK THAT COMMENT IS             
         BE    VK30                ATTACHED                                     
         GOTO1 RECVAL,DMCB,TLCMCDQ,(X'C0',TYPEI)                                
         GOTO1 HIGH                                                             
         CLC   KEY(TLCMLEV-TLCMKEY),KEYSAVE                                     
         BNE   VK30                                                             
         CLI   COMATCHD,NOCMT      IF COMMENT EXISTS, USER MUST                 
         BE    CMTATTCH            ENTER PF13 TO INDICATE IT IS                 
         CLI   PFAID,13            OK TO REOPEN/CANCEL                          
         BNE   CMTATTCH                                                         
         MVI   PFAID,0                                                          
         MVI   COMATCHD,CMTOK                                                   
         SPACE 1                                                                
VK30     OC    CANINV,CANINV       IF CANADIAN INVOICE DEFINED                  
         BZ    VK40                                                             
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A4',CANINV)                               
         BE    *+6                                                              
         DC    H'00'               GO GET CANADIAN INVOICE RECORD               
         SPACE 1                                                                
         USING TAIND,R3                                                         
         L     R3,AIO                                                           
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         TM    TAINSTAT,TAINSCHK   DON'T ALLOW IF CHECKS WRITTEN                
         BO    NOTAVAIL                                                         
         SPACE 1                                                                
         CLI   ACTNUM,ACTREOP      IF ACTION IS REOPEN                          
         BNE   VK30A                                                            
         TM    TAINSTAT,TAINSBIL   DON'T ALLOW IF BILLED                        
         BO    NOTAVAIL                                                         
         SPACE 1                                                                
VK30A    CLI   ACTNUM,ACTCAN       IF ACTION IS CANCEL                          
         BNE   VK40                                                             
         TM    TAINSTAT,TAINSCAN+TAINSCIN  DON'T ALLOW IF CANCEL(LED)           
         BNZ   NOTAVAIL                                                         
         TM    TAINSTA2,TAINSHLP   ALLOW IF COD & PRINTED                       
         BO    VK30B                                                            
         TM    TAINSTAT,TAINSBIL   DON'T ALLOW IF NOT BILLED                    
         BZ    NOCANCEL                                                         
VK30B    TM    TAINSTAT,TAINSHLD   OR IF COD HOLD                               
         BO    CODHOLD                                                          
         DROP  R3                                                               
         SPACE 1                                                                
VK40     MVC   TGINV,INVNO                                                      
         XC    TGINV,HEXFFS                                                     
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'20',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         SPACE 1                                                                
         GOTO1 SAVPTRS,DMCB,INVPTRS  BUILD INVOICE POINTER BLOCK                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CREATES A TABLE OF SUBSIDIARY INVOICES THAT              
*              ARE ATTACHED TO THE PRIMARY INVOICE NUM BEING CANCELLED          
         SPACE                                                                  
SETSI    NTR1                                                                   
         LA    R4,SITAB                                                         
         CLI   ACTNUM,ACTCAN       PROCESS SUBSIDIARY RECORDS                   
         BNE   SETSI20             ONLY IF ACTION IS CANCEL                     
         L     R3,AIO              R3=A(PRIMARY INVOICE RECORD)                 
         USING TASID,R3                                                         
         MVI   ELCODE,TASIELQ                                                   
         SPACE                                                                  
         BAS   RE,GETEL            GET SUBSIDIARY INVOICE ELEMENT               
         B     *+8                                                              
SETSI10  BAS   RE,NEXTEL                                                        
         BNE   SETSI20                                                          
         MVC   0(L'SITAB,R4),TASIINV     SET INVOICE NUMBER                     
         LA    R4,L'SITAB(R4)            BUMP TO NEXT TABLE ENTRY               
         B     SETSI10                                                          
*                                                                               
SETSI20  MVI   0(R4),X'FF'         MARK END OF TABLE                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS THE ADVICE NUMBER PAID AND COMMERCIAL ID            
         SPACE                                                                  
GETADV   NTR1                                                                   
         XC    ADVNUM,ADVNUM                                                    
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTADV  ADVICE NUM IN TAFN EL           
         BNE   XIT                                                              
         MVC   ADVNUM,TGNAME       SAVE IT IF FOUND                             
         SPACE                                                                  
         USING TACOD,R3                                                         
         L     R3,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL            GET COMMERCIAL DETAILS ELEMENT               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TGCID,TACOCID       SET TGCID                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PROCESSES RECORDS                                        
         SPACE 1                                                                
*                                  AIO=A(INVOICE RECORD)                        
PROCESS  NTR1                                                                   
         TM    AGYSTAT,TAAYSCOD    IF THIS IS COD AGENCY                        
         BZ    *+14                                                             
         MVC   CONHED2(12),CODMSG  DISPLAY COD MSG                              
         B     PROC05                                                           
         TM    CLTSTAT,TACISCOD    IF THIS IS COD CLIENT                        
         BZ    *+10                                                             
         MVC   CONHED2(12),CODMSG2  DISPLAY COD MSG                             
         SPACE 1                                                                
PROC05   TM    STATUS,PFTOPROC     TEST PF13 TO PROCESS BIT ON                  
         BO    *+12                                                             
         OI    STATUS,PFTOPROC     NO, TURN IT ON                               
         B     PRESSPF             AND GIVE MESSAGE TO USER                     
         SPACE 1                                                                
         CLI   PFAID,13            YES, TEST IT WAS PRESSED                     
         BNE   PRESSPF                                                          
         SPACE 1                                                                
         LA    R2,APPFSTH          GET A(FIRST ERROR DISPLAY FIELD)             
         ST    R2,ATHISERR         SAVE IT                                      
         TIME  DEC                                                              
         STCM  R0,14,ERRTIME       SAVE THE TIME                                
         SPACE 1                                                                
         LA    R4,SITAB            POINT TO SUBSIDIARY INVOICE TABLE            
         MVC   SVINVNO,INVNO       SAVE PRIMARY INVOICE NUMBER                  
*                                                                               
PROC10   BAS   RE,CHECKS           PROCESS CHECK/CAST/GUAR/DUECOMP RECS         
         SPACE 1                                                                
         CLI   ACTNUM,ACTREOP      TEST ACTION REOPEN                           
         BNE   *+16                                                             
         BAS   RE,DSCREENS         DELETE SCREEN RECORDS                        
         BAS   RE,RINVOICE         REOPEN INVOICE RECORD                        
         B     *+8                                                              
         BAS   RE,CINVOICE         CANCEL INVOICE RECORD                        
         SPACE 1                                                                
         GOTO1 ACTVOUT,DMCB,(X'80',APPLCHGH) DISPLAY LAST CHANGED INFO          
         NI    APPHCHGH+1,X'F3'    TURN OFF ZERO INTENSITY FOR HEADER           
         NI    APPLCHGH+1,X'F3'    TURN OFF ZERO INTENSITY FOR DATA             
         SPACE 1                                                                
         XC    HALF,HALF           CLEAR HALF (USED FOR CAST UH)                
         BAS   RE,DUSAGE           DELETE USAGE HISTORY RECORD                  
         BAS   RE,ADVICE           UNMARK ADVICE RECORD                         
         BRAS  RE,PROGCO           PROCESS GUARANTEE CONTRACT RECORDS           
         SPACE 1                                                                
         MVI   STATUS,0                                                         
         SPACE 1                                                                
         CLI   0(R4),X'FF'                                                      
         BE    PROC20                                                           
         MVC   INVNO,0(R4)         SET TO CANCEL THIS NUMBER                    
         MVC   TGINV,INVNO         SET GLOBAL INVOICE NUMBER                    
         XC    TGINV,HEXFFS        COMPLEMENT IT                                
         LA    R4,L'SITAB(R4)      BUMP TO NEXT ENTRY                           
         B     PROC10                                                           
*                                                                               
PROC20   MVC   INVNO,SVINVNO       RESET PRIMARY INVOICE NUMBER                 
         MVC   TGINV,INVNO         RESET GLOBAL INVOICE NUMBER                  
         XC    TGINV,HEXFFS        COMPLEMENT IT                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PROCESSES CANADIAN INVOICES                              
         SPACE 1                                                                
*                                  AIO=A(INVOICE RECORD)                        
PROCCAN  NTR1                                                                   
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',CANINV)  GET CAN INV REC              
         BE    *+6                                                              
         DC    H'00'               MUST EXIST OR DIE                            
*                                                                               
         GOTO1 SAVPTRS,DMCB,INVPTRS   BUILD INVOICE POINTER BLOCK               
*                                                                               
         MVC   TGINV,CANINV                                                     
*                                                                               
         LA    R2,APPFSTH          GET A(FIRST ERROR DISPLAY FIELD)             
         ST    R2,ATHISERR         SAVE IT                                      
         TIME  DEC                                                              
         STCM  R0,14,ERRTIME       SAVE THE TIME                                
*                                                                               
         BAS   RE,SETSI            SET SUBSIDIARY INVOICE TABLE                 
*                                                                               
         LA    R4,SITAB            POINT TO SUBSIDIARY INVOICE TABLE            
         MVC   SVINVNO,CANINV      SAVE PRIMARY CANADIAN INVOICE NUMBER         
         MVC   INVNO,CANINV                                                     
         XC    INVNO,HEXFFS        UNCOMPLEMENT CAN INVOICE NUMBER              
*                                                                               
PROCC10  BAS   RE,CHECKS           PROCESS CHECK/CAST/GUAR/DUECOMP RECS         
         SPACE 1                                                                
         CLI   ACTNUM,ACTREOP      TEST ACTION REOPEN                           
         BNE   *+12                                                             
         BAS   RE,RINVCAN          REOPEN INVOICE RECORD                        
         B     *+8                                                              
         BAS   RE,CINVCAN          CANCEL INVOICE RECORD                        
         SPACE 1                                                                
         GOTO1 ACTVOUT,DMCB,(X'80',APPLCHGH) DISPLAY LAST CHANGED INFO          
         NI    APPHCHGH+1,X'F3'    TURN OFF ZERO INTENSITY FOR HEADER           
         NI    APPLCHGH+1,X'F3'    TURN OFF ZERO INTENSITY FOR DATA             
         SPACE 1                                                                
         XC    HALF,HALF           CLEAR HALF (USED FOR CAST UH)                
         BAS   RE,DUSAGE           DELETE USAGE HISTORY RECORD                  
         MVI   STATUS,0                                                         
         SPACE 1                                                                
         CLI   0(R4),X'FF'                                                      
         BE    PROCC20                                                          
         MVC   INVNO,0(R4)         SET TO CANCEL THIS NUMBER                    
         MVC   TGINV,INVNO         SET GLOBAL INVOICE NUMBER                    
         XC    TGINV,HEXFFS        COMPLEMENT IT                                
         LA    R4,L'SITAB(R4)      BUMP TO NEXT ENTRY                           
         B     PROCC10                                                          
*                                                                               
PROCC20  MVC   INVNO,SVINVNO       RESET PRIMARY INVOICE NUMBER                 
         MVC   TGINV,INVNO         RESET GLOBAL INVOICE NUMBER                  
         XC    TGINV,HEXFFS        COMPLEMENT IT                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DELETE SCREEN RECORDS (REOPEN)                        
         SPACE 1                                                                
DSCREENS NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY              BUILD INITIAL SCREEN RECORD KEY              
         USING TLSCD,R4                                                         
         MVI   TLSCCD,TLSCCDQ      RECORD CODE                                  
         MVC   TLSCAGY,TGAGY       AGENCY                                       
         MVC   TLSCINV,SCRINV      INVOICE NUMBER (UN-COMPLEMENTED)             
         GOTO1 HIGH                GET RECORD                                   
         SPACE 1                                                                
DSC2     CLC   TLSCKEY(TLSCPG-TLSCD),KEYSAVE  IF NO LONGER SAME INV.            
         BNE   XIT                            GET OUT                           
         SPACE 1                                                                
         GOTO1 GETREC              GET FILE RECORD                              
         L     R4,AIO                                                           
         OI    TLSCSTAT,X'80'      TURN ON DELETED BIT                          
         GOTO1 PUTREC              AND WRITE IT BACK                            
         SPACE 1                                                                
         USING TLDRD,R4                                                         
         LA    R4,KEY                                                           
         OI    TLDRSTAT,X'80'      DELETE DIRECTORY RECORD                      
         GOTO1 WRITE                                                            
         SPACE 1                                                                
         OI    DMINBTS,X'08'       SET READ FOR DELETED                         
         GOTO1 HIGH                RE-READ THE RECORD WE JUST WROTE             
         NI    DMINBTS,X'F7'                                                    
         GOTO1 SEQ                 GET NEXT                                     
         B     DSC2                                                             
         EJECT                                                                  
*              ROUTINE TO REOPEN INVOICE RECORD                                 
         SPACE 1                                                                
RINVOICE NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'20',0)  GET INVOICE RECORD                
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE 1                                                                
         BAS   RE,REOPINV                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO REOPEN CANADIAN INVOICE RECORD                        
         SPACE 1                                                                
RINVCAN  NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',CANINV)  GET INVOICE RECORD           
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE 1                                                                
         BAS   RE,REOPINV                                                       
         B     XIT                                                              
         EJECT                                                                  
REOPINV  NTR1                                                                   
         L     R3,AIO              LOOP THROUGH RECORD                          
         MVI   ELCODE,0            AND DELETE ELEMENTS                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
RIN4     BAS   RE,NEXTEL                                                        
         BNE   RIN8                                                             
         SPACE 1                                                                
         USING TARAD,R3                                                         
         CLI   0(R3),TARAELQ       DO NOT DELETE BOVER ELEMENT                  
         BNE   RIN5                                                             
         CLI   TARASTA2,0                                                       
         BNE   RIN4                                                             
         DROP  R3                                                               
         SPACE 1                                                                
         USING TAACD,R3                                                         
RIN5     CLI   0(R3),TAACELQ                                                    
         BNE   RIN6                                                             
         CLI   TAACSCR,SCR60       DO NOT DELETE BOVER ACTIVITY                 
         BE    RIN4                ELEMENT                                      
         DROP  R3                                                               
         SPACE 1                                                                
RIN6     CLI   0(R3),TASIELQ       KEEP SUBSIDIARY INVOICE ELEMENTS             
         BE    RIN4                                                             
         CLI   0(R3),TAAIELQ       KEEP ADVICE ASSIGN ELEMENTS                  
         BE    RIN4                                                             
         CLI   0(R3),TAINELQ       HANDLE INVOICE STATUS EL. SEPARATELY         
         BE    *+12                                                             
         MVI   0(R3),X'FF'         SET TO DELETE THIS ELEMENT                   
         B     RIN4                                                             
         SPACE 1                                                                
         USING TAIND,R3            R3=A(INVOICE STATUS ELEMENT)                 
         XC    TAINPINF,TAINPINF   CLEAR PAYMENT INFO                           
         XC    TAINQINF,TAINQINF   AND QC INFO                                  
         MVI   TAINTERR,0          AND ERROR NUMBER                             
         CLI   TAINLEN,TAINHDTE+L'TAINHDTE-TAIND                                
         BL    *+10                                                             
         XC    TAINHDTE,TAINHDTE   AND COD PRINT DATE                           
         NI    TAINSTAT,ALL-TAINSREO                                            
         NI    TAINSTA2,ALL-TAINS2RE  CLEAR SOME STATUS BITS                    
         B     RIN4                                                             
         SPACE 1                                                                
RIN8     MVI   ELCODE,X'FF'        DELETE ALL REMAINING ELEMENTS                
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         GOTO1 ACTVIN,DMCB,(X'80',0) ADD ACTIVITY INFO BY SCREEN                
         SPACE 1                                                                
         GOTO1 PUTREC                WRITE BACK THE RECORD                      
         GOTO1 ADDPTRS,DMCB,INVPTRS  UPDATE INVOICE PASSIVE POINTERS            
*                                                                               
         BRAS  RE,VREVNR           PROCESS VRE/VNR PAYMENTS                     
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CANCELS INVOICE RECORD AND ADDS REVERSED INVOICE         
         SPACE 1                                                                
CINVOICE NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'20',0)  GET INVOICE RECORD                
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE 1                                                                
         BAS   RE,CANCINV                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CANCELS CAN INV RECORD AND ADDS REVERSED INVOICE         
         SPACE 1                                                                
CINVCAN  NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',CANINV)  GET INVOICE RECORD           
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE 1                                                                
         BAS   RE,CANCINV                                                       
         B     XIT                                                              
         EJECT                                                                  
CANCINV  NTR1                                                                   
         MVI   UPDCSFST,0          UPDATE COMM CSF STATUS                       
*                                                                               
         L     R3,AIO              R3=A(INVOICE RECORD)                         
         USING TLIND,R3                                                         
         MVC   INVKEY,TLINKEY      SAVE ACTIVE KEY                              
         SPACE 1                                                                
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R3                                                         
*                                                                               
         TM    TAPDSTA2,TAPDSSUB   IF SUBSIDIARY INVOICE                        
         BNO   CINV10                                                           
         GOTO1 SAVPTRS,DMCB,INVPTRS  BUILD INVOICE POINTER BLOCK                
CINV10   OI    TAPDSTAT,TAPDSCNL   SET CANCEL BIT HERE                          
         LR    R2,R3               SAVE A(EL.)                                  
         SPACE 1                                                                
         TM    TAPDSTAT,TAPDSCAN   IF US INVOICE,                               
         BNZ   CINV20                                                           
         MVI   ELCODE,TABDELQ      GET BILLING DETAILS ELEMENT                  
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TABDD,R3                                                         
         OC    TABDCSF,TABDCSF     CONTRACT SERVICE FEE PAID?                   
         BZ    CINV20                                                           
         MVI   UPDCSFST,C'Y'       SET STATUS TO UPDATE COMM REC                
         DROP  R3                                                               
         SPACE 1                                                                
CINV20   MVI   ELCODE,TAINELQ      GET INVOICE STATUS ELEMENT                   
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAIND,R3                                                         
         OI    TAINSTAT,TAINSCAN   SET CANCELLED BIT                            
         SPACE 1                                                                
         GOTO1 ACTVIN,DMCB,(X'80',0) ADD ACTIVITY INFO BY SCREEN                
         SPACE 1                                                                
         GOTO1 PUTREC              WRITE BACK INVOICE RECORD                    
         SPACE 1                                                                
         GOTO1 ADDPTRS,DMCB,(X'04',INVPTRS) UPDATE INV PASSIVE POINTERS         
         SPACE 1                                                                
         OI    TAINSTAT,TAINSCIN   NOW SET UP CANCEL INVOICE                    
         ICM   R4,7,TAINBDTE       R4=ORIGINAL BILL DATE                        
         XC    TAINBDTE,TAINBDTE   CLEAR BILL DATE                              
         CLI   TAINLEN,TAINHDTE+L'TAINHDTE-TAIND                                
         BL    *+10                                                             
         XC    TAINHDTE,TAINHDTE         COD PRINT DATE                         
         MVI   TAINTERR,0                ERROR NUMBER                           
         NI    TAINSTAT,ALL-TAINSCNL                                            
         NI    TAINSTA2,ALL-TAINS2CL     CLEAR SOME STATUS BITS                 
         SPACE 1                                                                
         L     R3,AIO              R3=A(RECORD)                                 
         USING TLIND,R3                                                         
         MVC   TLINKEY,INVKEY      RESTORE ACTIVE KEY                           
         NI    TLININV+5,X'7F'     SET CANCEL BIT IN INVOICE NUMBER             
         XC    TLINSTAT,TLINSTAT                                                
         SPACE 1                                                                
         LR    R3,R2               R3=A(PAYMENT DETAILS EL.)                    
         USING TAPDD,R3                                                         
         OI    TAPDINV+5,X'80'     SET CANCEL BIT IN INVOICE NUMBER             
         NI    TAPDSTA2,ALL-TAPDSCPO CLEAR PO CLOSE OK                          
         SPACE 1                                                                
         GOTO1 REVERSE             REVERSE AMOUNT FIELDS                        
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     ADD ORIGINAL BILL DATE ELEMENT               
         LA    R3,ELEMENT                                                       
         USING TAOBD,R3                                                         
         MVI   TAOBEL,TAOBELQ                                                   
         MVI   TAOBLEN,TAOBLNQ                                                  
         STCM  R4,7,TAOBBDTE       SET ORIGINAL BILL DATE                       
         GOTO1 ADDELEM                                                          
*                                                                               
         BAS   RE,ADJPRI           SET CANCEL BIT IN INVOICE NUMBERS            
         BAS   RE,ADJSI            (FOR SPLIT BILLING SITUATIONS)               
         BAS   RE,ADJCSUN          ADJUST US/CAN INVOICE ELEMENT                
*                                                                               
         GOTO1 ADDREC                ADD REVERSED INVOICE RECORD                
         XC    INVPTRS(99),INVPTRS   CLEAR INVOICE POINTER BLOCK                
         GOTO1 ADDPTRS,DMCB,INVPTRS  SO THAT WE ADD ALL NEW ONES                
*                                                                               
         BRAS  RE,VREVNR           PROCESS VRE/VNR PAYMENTS                     
*                                                                               
         CLI   UPDCSFST,C'Y'       UPDATE CSF STATUS?                           
         BNE   XIT                                                              
         BRAS  RE,UPDCOMM          UPDATE CSF STATUS ON COMMERCIAL              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE ADJUSTS SUBSIDIARY INVOICE ELEMENT                       
         SPACE                                                                  
ADJSI    NTR1                                                                   
         L     R3,AIO              R3=A(INVOICE RECORD)                         
         MVI   ELCODE,TASIELQ                                                   
         USING TASID,R3                                                         
         SPACE                                                                  
         BAS   RE,GETEL            GET SUBSIDIARY INVOICE ELEMENT               
         B     *+8                                                              
ADJSI10  BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         OI    TASIINV+5,X'80'     SET CANCEL BIT IN INVOICE NUMBER             
         B     ADJSI10                                                          
         DROP  R3                                                               
         SPACE 2                                                                
*              ROUTINE ADJUSTS TANU PRIMARY INVOICE NUMBER                      
*              IN CHECK OR INVOICE RECORD IN AIO                                
ADJPRI   NTR1                                                                   
         L     R3,AIO              R3=A(SUBSIDIARY RECORD)                      
         MVI   ELCODE,TANUELQ                                                   
         USING TANUD,R3                                                         
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSPL))                                     
         BNE   ADJPRIX                                                          
         L     R3,TGELEM                                                        
         OI    TANUMBER+5,X'80'    SET CANCEL BIT IN INVOICE NUMBER             
ADJPRIX  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE ADJUSTS TAUC ELEMENT IF US/CAN LINKED INVOICE            
*              CANCEL BITS ARE SET IN INVOICE NUMBERS                           
*              INVOICE RECORD IN AIO                                            
ADJCSUN  NTR1                                                                   
         L     R3,AIO                                                           
         MVI   ELCODE,TAUCELQ      GET US/CANADIAN INVOICE ELEMENT              
         BAS   RE,GETEL                                                         
         BNE   ACSUNX                                                           
         USING TAUCD,R3                                                         
         NI    TAUCINU+5,X'7F'     SET CANCEL BIT IN US INVOICE                 
         NI    TAUCINC+5,X'7F'     AND CANADIAN INVOICE                         
ACSUNX   B     XIT                                                              
         DROP  R3                                                               
*              ROUTINE PROCESSES CHECK RECORDS                                  
         SPACE 1                                                                
CHECKS   NTR1                                                                   
         MVI   GCTAB,X'FF'         INITIALIZE GUARANTEE CONTRACT TABLE          
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY              BUILD INITIAL CHECK RECORD KEY               
         USING TLCKD,R4                                                         
         MVI   TLCKCD,TLCKCDQ      RECORD CODE                                  
         MVC   TLCKAGY,TGAGY       AGENCY                                       
         MVC   TLCKINV,INVNO       INVOICE NUMBER (UN-COMPLEMENTED)             
         BRAS  RE,SETCHK           SET CHECK FILES                              
         GOTO1 HIGH                GET FIRST RECORD                             
         SPACE 1                                                                
CH2      LA    R4,KEY                                                           
         CLC   TLCKKEY(TLCKSORT-TLCKD),KEYSAVE  IF NO LONGER SAME INV.          
         BE    *+12                                                             
         BRAS  RE,SETTAL                        SET TALENT FILES                
         B     XIT                              AND GET OUT                     
         SPACE 1                                                                
         XC    ERRORS,ERRORS       CLEAR ERRORS FOR THIS CAST MEMBER            
         MVI   NUMERR,0                                                         
         SPACE 1                                                                
         MVC   CHKKEY,TLCKKEY      SAVE THIS KEY                                
         MVC   TGCSORT,TLCKSORT    SET GLOBAL VALUES                            
         MVC   TGSSN,TLCKSSN                                                    
         MVC   TGCAT,TLCKCAT                                                    
         SPACE 1                                                                
         GOTO1 GETREC              GET FILE RECORD INTO AIO1                    
         SPACE 1                                                                
         BRAS  RE,ADDGCO           ADD TO GUARANTEE CONTRACT TABLE              
         SPACE 1                                                                
         GOTO1 CATVAL,DMCB,TGCAT   SET CATEGORY INFO                            
         SPACE 1                                                                
         XC    CORPID,CORPID       THERE MAY BE CORP ID AROUND                  
         L     R3,AIO                                                           
         MVI   ELCODE,TATIELQ      LOOK FOR TAX ID ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING TATID,R3                                                         
         MVC   CORPID,TATIID       SAVE IT FOR DUE COMPANY LOOK-UP              
         DROP  R3                                                               
         SPACE 1                                                                
         L     R3,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R3                                                         
         MVC   CYCDATES,TAPDCYCS   SAVE CYCLE DATES                             
         MVC   ERRCYCLE,TAPDCYCS   INIT ERROR CYCLE DATES                       
         MVC   APPLDATE,CYCSTART   SET APPLY DATE                               
         OC    APPLDATE,APPLDATE                                                
         BNZ   *+10                                                             
         MVC   APPLDATE,PAYDATE                                                 
         MVC   APPLCODE,TAPDACDE   APPLY CODE                                   
         MVC   APPLAMNT,TAPDAPPL   APPLY AMOUNT                                 
         MVC   REXPAMT,TAPDREXP    REIMBURSED EXPENSES                          
         MVC   PAYAMT,TAPDPAYC     PAYMENT AMT FROM CORP PAY                    
         CLI   TAPDW4TY,TAW4TYCA   IF CANADIAN                                  
         BE    CH4                                                              
         CLI   TAPDW4TY,TAW4TYCO   IF CORP                                      
         BE    CH4                                                              
         CLI   TAPDW4TY,TAW4TYTR   OR TRUSTEE                                   
         BE    CH4                                                              
         CLI   TAPDW4TY,TAW4TYFO   OR FOREIGNER                                 
         BE    CH4                                                              
         MVC   PAYAMT,TAPDPAYI     ELSE GET AMT FROM INDIV PAY                  
         SPACE 1                                                                
CH4      CLI   ACTNUM,ACTREOP      IF ACTION REOPEN                             
         BNE   CH4A                                                             
         BAS   RE,DCHECK           THEN DELETE CHECK RECORD                     
         BAS   RE,DGUAR                 DELETE GUARANTEE RECORD                 
         B     CH4B                                                             
         SPACE 1                                                                
CH4A     BAS   RE,ACHECK           ELSE ADD REVERSED CHECK RECORD               
         SPACE 1                                                                
CH4B     TM    TAPDSTA2,TAPDSSUB   IF SUBSIDIARY CHECK                          
         BNO   *+12                                                             
         BAS   RE,ADJPRI           ADJ PRIMARY INV NUM TO BE CANCELLED          
         B     CH6                                                              
         SPACE 1                                                                
         TM    PAYSTAT,TAPDPCRD    IF THIS IS A CREDIT PAYMENT                  
         BZ    *+16                                                             
         TM    PAYSTAT,TAPDPBNP    AND NOT BNP                                  
         BO    *+8                                                              
         BAS   RE,DDUECOMP         DELETE AUTO DUE COMPANY RECORD               
         SPACE 1                                                                
         MVC   HALF,TGCSORT+4      SET CAST INPUT SEQ NUMBER IN HALF            
         BAS   RE,DUSAGE           DELETE USAGE HISTORY                         
         SPACE 1                                                                
         TM    TGUSSTA3,SOAPUSE    IF SOAP USE                                  
         BZ    CH5                                                              
         BAS   RE,CECAST           MAY HAVE TO CHANGE ECAST RECORDS             
         B     CH6                 AND DELETE ECAST TRACKING RECORDS            
         SPACE 1                                                                
CH5      BAS   RE,SETFTRK          SET FTRACK TRACKING FOUND STATUS             
         BAS   RE,CCAST            ELSE MAY HAVE TO CHANGE CAST RECORD          
         BAS   RE,DFTRACK          MAY HAVE TO DELETE FIXED CYCLE TRK           
         SPACE 1                                                                
CH6      MVC   KEY,CHKKEY          RESTORE KEY FOR THIS RECORD                  
         OI    DMINBTS,X'08'       SET READ FOR DELETED                         
         BRAS  RE,SETCHK           SET CHECK FILES                              
         GOTO1 HIGH                RE-READ THE RECORD WE JUST WROTE             
         NI    DMINBTS,X'F7'                                                    
         GOTO1 SEQ                 GET NEXT                                     
         B     CH2                                                              
         EJECT                                                                  
*              ROUTINE TO DELETE CHECK RECORD (REOPEN)                          
         SPACE 1                                                                
         USING TAPDD,R3            R3=A(PAYMENT DETAILS EL.)                    
         USING TLDRD,R4            R4=A(ACTIVE CHECK DIRECTORY RECORD)          
DCHECK   NTR1                                                                   
         OI    TLDRSTAT,X'80'      DELETE DIRECTORY RECORD                      
         GOTO1 WRITE           (MUST COME 1ST SINCE SAVPTRS CREAMS KEY)         
         SPACE 1                                                                
         GOTO1 SAVPTRS,DMCB,CHKPTRS  BUILD CHECK PASSIVE POINTER BLOCK          
*                              (MUST COME BEFORE RECORD MARKED DELETED)         
         L     R4,AIO                                                           
         USING TLCKD,R4                                                         
         OI    TLCKSTAT,X'80'      TURN ON DELETED BIT                          
         GOTO1 PUTREC              AND WRITE IT BACK                            
         SPACE 1                                                                
         GOTO1 ADDPTRS,DMCB,(X'40',CHKPTRS)  DELETE CHECK PASSIVE PTRS          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD REVERSED CHECK RECORD (CANCEL)                    
         SPACE 1                                                                
         USING TAPDD,R3            R3=A(PAYMENT DETAILS EL.)                    
         USING TLDRD,R4            R4=A(ACTIVE CHECK DIRECTORY RECORD)          
ACHECK   NTR1                                                                   
         GOTO1 SAVPTRS,DMCB,CHKPTRS  BUILD CHECK PASSIVE POINTER BLOCK          
         SPACE 1                                                                
         OI    TAPDSTAT,TAPDSCNL   SET CANCEL BIT HERE                          
         SPACE 1                                                                
         MVI   ELCODE,TACDELQ      DELETE CHECK DETAILS EL.                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TACWELQ             CHECK WITHHOLDING EL.                 
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TACYELQ             CHECK Y-T-D EL.                       
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAYEELQ             YTD EARNINGS EL. FOR CHECK            
         MVI   BYTE,TAYETCHK                                                    
         GOTO1 DELL,DMCB,(1,BYTE)                                               
         SPACE 1                                                                
         GOTO1 PUTREC              WRITE BACK ORIGINAL CHECK RECORD             
         SPACE 1                                                                
         GOTO1 ADDPTRS,DMCB,CHKPTRS  UPDATE CHECK PASSIVE POINTERS              
         SPACE 1                                                                
         L     R3,AIO                                                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TAPDINV+5,X'80'     MARK INVOICE NUMBER CANCELLED                
         SPACE 1                                                                
         NI    TAPDSTAT,ALL-TAPDSGUP-TAPDSGOF  TURN OFF GUAR. BITS              
         SPACE 1                                                                
         BAS   RE,ADJPRI           SET CANCEL BIT IN PRIMARY INV NUM            
         SPACE 1                                                                
         USING TABYD,R3                                                         
         L     R3,AIO                                                           
         MVI   ELCODE,TABYELQ      GET BILLING YTD ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   ACH10                                                            
         MVC   BYTE,TABYLVL        SAVE LOWEST LEVEL BILLED                     
         ZIC   R1,TABYLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    TABYEL+2(0),TABYEL+2  CLEAR ELEMENT DATA                         
         MVC   TABYLVL,BYTE          BUT RESET LEVEL                            
         SPACE 1                                                                
         USING TAYED,R3                                                         
ACH10    MVI   ELCODE,TAYEELQ      GET YTD EARNINGS EL. FOR BILLS               
         MVI   BYTE,TAYETBIL                                                    
         GOTO1 GETL,DMCB,(1,BYTE)                                               
         BNE   ACH20                                                            
         L     R3,TGELEM                                                        
         ZIC   R1,TAYELEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    TAYEEL+3(0),TAYEEL+3  CLEAR ELEMENT DATA                         
         SPACE 1                                                                
ACH20    GOTO1 REVERSE             REVERSE ALL AMOUNT FIELDS                    
         SPACE 1                                                                
         L     R4,AIO              R4=A(CHECK RECORD)                           
         USING TLCKD,R4                                                         
         MVC   TLCKKEY,CHKKEY      RESTORE ORIGINAL ACTIVE KEY                  
         OI    TLCKINV+5,X'80'     TURN ON CANCELLED BIT                        
         GOTO1 ADDREC              AND ADD IT TO FILE                           
         SPACE 1                                                                
         XC    CHKPTRS,CHKPTRS       CLEAR CHECK POINTER BLOCK                  
         GOTO1 ADDPTRS,DMCB,CHKPTRS  SO THAT WE ADD ALL NEW ONES                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DELETE GUARANTEE RECORD                               
         SPACE 1                                                                
DGUAR    NTR1                                                                   
         CLI   TGUSEQU,UGRT        DON'T BOTHER IF NOT A GUARANTEE              
         BNE   XIT                                                              
         TM    PAYSTAT,TAPDPCRD    OR IF THIS IS A CREDIT PAYMENT               
         BO    XIT                    (PAY DOESN'T PROCESS GRT REC)             
         MVC   AIO,AIO2            USE AIO2, SAVE CHECK REC IN AIO1             
         L     R3,AIO1                                                          
         MVI   ELCODE,TACAELQ      LOOK FOR CAST DET EL IN CHECK REC            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R3                                                         
         SPACE                                                                  
         MVC   TGGUA,TACAGUA       EXTRACT GUARANTEE CODE                       
         XC    TGGUA,HEXFFS        COMPLEMENT IT                                
         SPACE 1                                                                
         BRAS  RE,SETTAL                      SET TALENT FILES                  
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'20',0)  GET GUARANTEE RECORD              
         BNE   DGUX                                                             
         SPACE 1                                                                
         L     R3,AIO                                                           
         MVI   ELCODE,TAGUELQ      LOOK FOR GUARANTEE DETAILS EL.               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAGUD,R3                                                         
         MVC   GUARSTAT,TAGUSTAT                                                
         SPACE 1                                                                
         OC    TAGUINV,TAGUINV     IF GUARANTEE WAS ADDED BY PAY                
         BZ    DGUX2                                                            
         MVC   DUB(6),TGINV                                                     
         XC    DUB(6),HEXFFS                                                    
         CLC   TAGUINV,DUB         VIA THE INVOICE BEING REOPENED               
         BNE   DGUX2                                                            
         OC    TAGUIAY,TAGUIAY                                                  
         BZ    DGUX1                                                            
         CLC   TGAGY,TAGUIAY                                                    
         BNE   DGUX2                                                            
DGUX1    TM    TAGUSTAT,TAGUSINS   ENSURE THAT AN INSTALLMENT PAYMENT           
         BZ    DGUX5               HAS NOT BEEN MADE                            
         B     DGUXE                                                            
         SPACE 1                                                                
DGUX2    TM    PAYSTAT2,TAPDPITC   IF STILL UPDATING GUARANTEES                 
         BO    DGUX                AT PAY TIME                                  
         L     RE,TAGUAMT                                                       
         S     RE,PAYAMT           SUBTRACT PAYMENT AMOUNT                      
         ST    RE,TAGUAMT          FROM GUARANTEE AMOUNT                        
         SPACE 1                                                                
         L     RE,TAGUBAL          SUBTRACT PAYMENT AMOUNT                      
         S     RE,PAYAMT           FROM BALANCE AMOUNT                          
         ST    RE,TAGUBAL                                                       
         SPACE 1                                                                
DGUX3    GOTO1 PUTREC              AND PUT BACK GUARANTEE RECORD                
         B     DGUX                                                             
         SPACE 1                                                                
DGUX5    TM    TAGUSTAT,TAGUSPAY   TEST ADDED BY PAY                            
         BZ    DGUX                                                             
         BAS   RE,CHKTRK              IF TRACKING RECORDS EXISTS                
         BE    DGUX                   DON'T DELETE                              
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLGUCDQ,0  RE-READ GUARANTEE DIRECTORY               
         L     R4,AIO                                                           
         USING TLGUD,R4                                                         
         OI    TLGUSTAT,X'80'      DELETE FILE RECORD                           
         GOTO1 PUTREC                                                           
         SPACE 1                                                                
         OI    KEY+TLDRSTAT-TLDRD,X'80'  DELETE DIRECTORY RECORD                
         GOTO1 WRITE                                                            
         OI    STATUS,GRTDEL       SET GRT DELETED IN STATUS                    
         B     DGUX                                                             
         SPACE 1                                                                
DGUXE    MVI   ERR,TACEEGRT        SET GRT RECORD NOT DELETED ERROR             
         BAS   RE,PUTERR           SAVE ERROR AND DISPLAY ON SCREEN             
         SPACE 1                                                                
DGUX     MVC   AIO,AIO1            RESET AIO                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DETERMINES IF TRACKING RECS EXIST FOR GUAR.              
         SPACE 1                                                                
CHKTRK   NTR1                                                                   
         LA    R3,KEY              BUILD BASIC KEY OF TRACKING RECORD           
         USING TLGTD,R3                                                         
         XC    KEY,KEY                                                          
         MVI   TLGTCD,TLGTCDQ      RECORD CODE                                  
         MVC   TLGTSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         MVC   TLGTGUA,TGGUA       GUARANTEE CODE                               
         XC    TLGTGUA,HEXFFS      (UNCOMPLEMENTED)                             
         GOTO1 HIGH                                                             
         CLC   TLGTKEY(TLGTSTRT-TLGTD),KEYSAVE                                  
         B     XIT                 RETURN CC                                    
         EJECT                                                                  
*              ROUTINE TO DELETE FIXED CYCLE TRACKING RECORDS                   
         SPACE 1                                                                
SETFTRK  NTR1                                                                   
         NI    STATUS,X'FF'-FTRKFND                                             
         CLI   APPLCODE,APPLHLD    IF PAYMENT APPLIED HOLDING FEE               
         BE    SFTRK10             CREDITS                                      
         CLI   APPLCODE,APPLSESS   OR SESSION CREDITS                           
         BNE   XIT                                                              
SFTRK10  BRAS  RE,SETTAL                                                        
                                                                                
         USING TLFTD,R3                                                         
         LA    R3,KEY              READ ALL FTRACK TRACKING RECORDS             
         XC    KEY,KEY             FOR THIS CAST                                
         MVI   TLFTCD,TLFTCDQ                                                   
         MVC   TLFTSSN,TGSSN                                                    
         MVC   TLFTCOM,TGCOM                                                    
         MVC   TLFTCAST,TGCSORT+4                                               
         GOTO1 HIGH                                                             
         B     SFTRK30                                                          
SFTRK20  GOTO1 SEQ                                                              
SFTRK30  CLC   TLFTKEY(TLFTSTRT-TLFTD),KEYSAVE                                  
         BNE   XIT                                                              
         CLC   TLFTINV,INVNO       IF ONE IS FOUND FOR THIS INVOICE             
         BNE   SFTRK20                                                          
         DROP  R3                                                               
                                                                                
         OI    STATUS,FTRKFND      SET OK TO REVERSE FTRACK                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHANGE CAST RECORDS                                   
         SPACE 1                                                                
CCAST    NTR1                                                                   
         XC    FCYCDTES,FCYCDTES                                                
         CLI   NUMERR,0            ALWAYS DO CAST REC IF THERE'S ERRORS         
         BNE   CCA2                                                             
         CLI   TGUSEQU,UDLR        OR IF PAYMENT WAS A DEALER                   
         BE    CCA2                                                             
         CLI   TGUSEQU,UGRT        IF THIS IS A GUARANTEE                       
         BNE   *+12                                                             
         TM    STATUS,GRTDEL       AND GRT RECORD WAS DELETED                   
         BO    CCA2                                                             
         TM    TGCATYPE,EXTRA      OR NOT AN EXTRA (NO APPLIED CREDIT)          
         BO    CCAX                                                             
         TM    TGUSSTA2,APPREUSE   AND A CANDIDATE FOR APPLIED CREDITS          
         BO    CCA2                                                             
         CLI   APPLCODE,APPLHLD    OR WE APPLIED HOLDING FEE CREDITS            
         BE    CCA2                                                             
         CLI   APPLCODE,APPLSESS   OR WE APPLIED SESSION CREDITS                
         BNE   CCAX                                                             
         CLI   TGUSEQU,UMUS        AND NOT MUS USE(A=S FOR FIRST MUSIC)         
         BE    CCAX                                                             
         SPACE 1                                                                
CCA2     BRAS  RE,SETTAL           SET TALENT FILES                             
         SPACE 1                                                                
         USING TLCAD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY             READ CAST RECORD FOR UPDATE                  
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         B     CCA2B                                                            
CCA2A    GOTO1 SEQ                                                              
CCA2B    CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         BNE   CCAX                                                             
         CLC   TLCASEQ,TGCSORT+4                                                
         BNE   CCA2A                                                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         DROP  R3                                                               
         SPACE 1                                                                
         GOTO1 SAVPTRS,DMCB,CSTPTRS  BUILD CAST POINTER BLOCK                   
         SPACE 1                                                                
         CLI   TGUSEQU,UGRT        IF THIS IS A GUARANTEE                       
         BNE   *+12                                                             
         TM    STATUS,GRTDEL       AND GRT RECORD WAS DELETED                   
         BO    CCA8                REMOVE GRT CODE ON CAST REC                  
         SPACE                                                                  
         CLI   TGUSEQU,UDLR        IF THIS IS A DEALER PAYMENT                  
         BNE   CCA2Y                                                            
         BAS   RE,CCASTDLR         PROCESS $0 DLR-GENERATED FTRACK              
         CLI   APPLCODE,APPLHLD    THEN IF APPLIED HOLDING FEE CREDITS          
         BE    CCA2Y                                                            
         CLI   APPLCODE,APPLSESS   OR WE APPLIED SESSION CREDITS                
         BNE   CCA12               GO BACK THAT OUT AS WELL                     
         SPACE                                                                  
CCA2Y    TM    TGCATYPE,EXTRA      IF NOT AN EXTRA (NO APPLIED CREDIT)          
         BO    CCAXE2                                                           
         TM    TGUSSTA2,APPREUSE   AND IS CANDIDATE FOR APPLIED CREDITS         
         BZ    CCA2Z                                                            
         TM    SVTACO3,TACOSSMW    NO FTRACK IF SOCIAL MEDIA WAIVER             
         BO    CCA2Z                                                            
******** OC    PAYAMT,PAYAMT       AND PAY AMT NOT 0(NO APPLIED CREDIT)         
******** BZ    CCAXE2                                                           
         B     CCA3                                                             
CCA2Z    CLI   APPLCODE,APPLHLD    OR WE APPLIED HOLDING FEE CREDITS            
         BE    CCA3                                                             
         CLI   APPLCODE,APPLSESS   OR WE APPLIED SESSION CREDITS                
         BNE   CCAXE2                                                           
         CLI   TGUSEQU,UMUS        AND NOT MUS USE(A=S FOR FIRST MUSIC)         
         BE    CCAXE2                                                           
         SPACE                                                                  
         USING TACRD,R3                                                         
CCA3     XR    R1,R1                                                            
         XC    TGDUB,TGDUB                                                      
         MVI   ELCODE,TACRELQ      FIND APPLIED CREDIT HISTORY EL.              
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CCA4     BAS   RE,NEXTEL           TEST ANY MORE ELEMENTS                       
         BE    CCA5                                                             
         MVI   ERR,TACEENFT        INIT ERR                                     
         TM    TGUSSTA2,APPREUSE   NO - IF CANDIDATE FOR APPLD CREDITS          
         BO    CCAXE               THEN DONE                                    
         B     CCA7                                                             
         SPACE                                                                  
CCA5     TM    TGUSSTA2,APPREUSE   IF CANDIDATE FOR APPLIED CREDITS             
         BZ    CCA6                                                             
         CLC   TACRINV,INVNO       MATCH ON INVOICE NUMBER                      
         BNE   CCA4                                                             
         MVI   ERR,0                                                            
         MVC   ERRCYCLE,TACRSTRT   SAVE ERROR CYCLE                             
         TM    TACRSTAT,TACRSTRK   IF TRACKING THIS CYCLE                       
         BO    CCA5D                                                            
         TM    PAYOPT3,TAPDORET    OR IF RETROACTIVE PAYMENT                    
         BO    CCA5D                                                            
         CLC   PAYDATE,=X'910808'  OR PAID AFTER 8/8/91                         
         BL    *+10                                                             
CCA5D    MVC   FCYCDTES,TACRSTRT   SAVE CYCLE DATES FOR FTRACK REC.             
         SPACE                                                                  
         TM    PAYOPT3,TAPDORET    IF RETRO PAYMENT                             
         BZ    *+12                                                             
         BAS   RE,TACRETRO         HANDLE TACREL DIFFERENTLY                    
         BE    CCA10                                                            
         SPACE                                                                  
         CLI   TGUSEQU,UADC        IF COMBINED SESS/WSP, SKIP TACEEBAL          
         BE    CCA5G               ERR CHECK - TACR ADDED WITH BAL = 0          
         SPACE                                                                  
         L     R1,TACRBAL                                                       
         C     R1,TACRAPPL         IF BALANCE < AMOUNT TO BE APPLIED            
         BNL   *+8                                                              
         MVI   ERR,TACEEBAL        SET ERROR                                    
         SPACE                                                                  
CCA5G    MVI   TACREL,X'FF'        ELSE SET TO DELETE THIS ELEMENT              
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         ZIC   R0,TWASCR           SAVE THE VALUE IN TWASCR                     
         MVI   TWASCR,X'FF'                                                     
         GOTO1 ACTVIN,DMCB,(X'C0',0)  CREATE TAACD EL WITHOUT ADDING IT         
         STC   R0,TWASCR           RESTORE THE VALUE  OF TWASCR                 
         LA    R4,ELEMENT                                                       
         USING TAACD,R4                                                         
         MVC   TAACTSCR,TWASCR     SAVE THE SCRN # IN TAACD ELEMENT             
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
*                                                                               
         B     CCA10                                                            
         SPACE 1                                                                
CCA6     DS    0H                  THIS MUST BE REUSE PAYMENT                   
         CLC   APPLDATE,TACRSTRT   APPLY DATE MUST FALL WITHIN CREDIT           
         BL    CCA4                                                             
         CLC   APPLDATE,TACREND    HISTORY EL. START AND END DATES              
         BH    CCA4                                                             
         OC    TACRINV,TACRINV     IF THERE'S AN INV NUM                        
         BZ    CCA6A                                                            
         CLC   TACRINV,TGDUB       TEST THIS INV LATER THAN SAVED EL            
         BNH   CCA4                                                             
         SPACE 1                                                                
CCA6A    CLC   TACRUSE,=C'GRR'     IF FTRACK WAS CREATED BY A GRR               
         BNE   CCA6B                                                            
         CLI   TACRTYPE,0          TO COVER A SPECIFIC USE                      
         BE    CCA6B                                                            
         CLC   TACRTYPE,TGUSEQU    REJECT IF THIS IS NOT THAT USE               
         BNE   CCA4                                                             
         OI    STATUS,USPECGRR     AND TURN ON STATUS FOR TRACKING              
         SPACE 1                                                                
CCA6B    LR    R1,R3               SAVE A(LAST EL WITH CORRECT DATES)           
         MVC   TGDUB(L'TACRINV),TACRINV  AND ITS INV NUM                        
         B     CCA4                                                             
         SPACE                                                                  
CCA7     MVI   ERR,TACEENFR        INIT ERR                                     
         LTR   R3,R1               SET R4=A(LAST EL WITH CORRECT DATES)         
         BZ    CCAXE                                                            
         MVC   ERRCYCLE,TACRSTRT   SAVE ERROR CYCLE                             
         SPACE                                                                  
         TM    TACRSTAT,TACRSTRK   IF TRACKING THIS CYCLE                       
         BO    CCA7D                                                            
         TM    PAYOPT3,TAPDORET    OR IF RETROACTIVE PAYMENT                    
         BO    CCA7D                                                            
         CLC   PAYDATE,=X'910808'  OR PAID AFTER 8/8/91                         
         BL    *+10                                                             
CCA7D    MVC   FCYCDTES,TACRSTRT   SAVE CYCLE DATES FOR FTRACK REC.             
         SPACE                                                                  
         TM    STATUS,FTRKFND      IF FTRACK TRACKING IS FOUND FOR              
         BZ    CCA10               THIS INVOICE                                 
         MVI   ERR,TACEENEG        INIT ERR                                     
         TM    TACRBAL,X'80'       TEST BALANCE NOT NEGATIVE                    
         BO    CCAXE                                                            
         MVI   ERR,0                                                            
         L     R1,TACRBAL          RESTORE APPLIED AMOUNT TO                    
         S     R1,APPLAMNT         CURRENT BALANCE (APPLD AMT NEGATED)          
         C     R1,TACRAPPL         BE SURE NOT TO EXCEED TOTAL AMOUNT           
         BNH   *+12                                      TO BE APPLIED          
         L     R1,TACRAPPL                                                      
         MVI   ERR,TACEEFUL        SET ERR                                      
         ST    R1,TACRBAL          NOW HAVE NEW BALANCE                         
         B     CCA10                                                            
         SPACE 1                                                                
         USING TLCAD,R3                                                         
CCA8     L     R3,AIO              HANDLE GUARANTEE PAYMENTS                    
         SPACE                                                                  
         OI    TLCASORT,X'02'      SET NOT ON GUAR TO CHANGE ACTIVE KEY         
         SPACE                                                                  
         MVI   ELCODE,TACAELQ      LOOK FOR CAST DETAILS ELEMENT                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R3                                                         
         XC    TACAGUA,TACAGUA     CLEAR GUARANTEE CODE FROM RECORD             
         B     CCAXE2                                                           
         SPACE 1                                                                
CCA10    CLI   ERR,0               IF THERE'S AN ERROR                          
         BE    *+8                                                              
CCAXE    BAS   RE,PUTERR           SAVE IT                                      
CCAXE2   BAS   RE,BLDTACE          BUILD TACEEL                                 
         BNE   CCA12                                                            
         GOTO1 ADDL                                                             
CCA12    GOTO1 PUTREC              WRITE BACK CAST RECORD                       
         SPACE 1                                                                
         GOTO1 ADDPTRS,DMCB,(X'80',CSTPTRS)  UPDATE CAST POINTERS               
         SPACE 1                                                                
CCAX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHANGE CAST RECORDS FOR DLR-GENERATED                 
*              HOLDING FEE FTRACKS                                              
         SPACE 1                                                                
CCASTDLR NTR1                                                                   
         USING TACRD,R3                                                         
         L     R3,AIO                                                           
         MVI   ELCODE,TACRELQ      FIND APPLIED CREDIT HISTORY EL.              
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CCD10    BAS   RE,NEXTEL           TEST ANY MORE ELEMENTS                       
         BNE   NO                                                               
         CLC   TACRINV,INVNO       MATCH ON INVOICE NUMBER                      
         BNE   CCD10                                                            
         CLC   TACRUSE,=C'HLD'                                                  
         BE    CCD20                                                            
         CLC   TACRUSE,=C'SHL'                                                  
         BE    CCD20                                                            
         CLC   TACRUSE,=C'ADH'                                                  
         BNE   CCD10                                                            
CCD20    MVI   TACREL,X'FF'        SET TO DELETE THIS ELEMENT                   
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO CHANGE ECAST RECORDS AND DELETE ETRACK RECS           
*              CHECK RECORD IS IN AIO1                                          
         SPACE 1                                                                
CECAST   NTR1                                                                   
         CLI   APPLCODE,APPLSESS   TEST WE APPLIED CREDITS THIS CHECK           
         BNE   XIT                                                              
         BRAS  RE,SETTAL           SET TALENT FILES                             
         MVC   AIO,AIO2            USE AIO2 FOR ECAST RECORDS                   
         SPACE 1                                                                
         L     R3,AIO1                                                          
         MVI   ELCODE,TASOELQ      GET TASO EL FROM CHECK RECORD                
         BAS   RE,GETEL                                                         
         BE    CECA3                                                            
         DC    H'0'                MUST HAVE AT LEAST 1                         
         SPACE                                                                  
CECA2    LR    R3,R2               SET R3=A(CURRENT TASOEL)                     
         MVI   ELCODE,TASOELQ      SET ELCODE FOR TASO EL                       
         BAS   RE,NEXTEL           GET NEXT                                     
         BNE   CECAX                                                            
CECA3    LR    R2,R3               SAVE R2=A(CURRENT TASOEL)                    
         USING TASOD,R2                                                         
         ZIC   R0,TASONUM          R0=N'SUB-ELEMENTS                            
         LA    R4,TASOSEPI         R4=A(SUB-ELEMENT)                            
         SPACE                                                                  
         USING TASOSEPI,R4                                                      
CECA4    XR    R1,R1                                                            
         ICM   R1,7,TASOAPPL                                                    
         LTR   R1,R1               TEST WE APPLIED CREDITS THIS EPISODE         
         BZ    CECA22                                                           
         TM    TASOAPPL,X'80'      IF APPLIED AMOUNT IS NEGATIVE                
         BZ    *+8                                                              
         ICM   R1,8,=X'FF'         MAKE SURE NEGATIVE IN R1                     
         ST    R1,APPLAMNT         SAVE APPLY AMOUNT FOR EPISODE                
         SPACE                                                                  
         XC    ERRORS,ERRORS       CLEAR ERRORS FOR THIS EPISODE                
         MVI   NUMERR,0                                                         
         NI    STATUS,ALL-DELETR   CLEAR FLAG TO DELETE ETRACK                  
         SPACE                                                                  
         LH    R1,TASOEPI          EPISODE NUMBER                               
         CVD   R1,DUB                                                           
         UNPK  TGEPI,DUB+5(3)      CONVERT IT TO CHARS                          
         OI    TGEPI+4,X'F0'       SAVE IN TGEPI                                
         MVC   SVTGCSRT,TGCSORT    SAVE TGCSORT                                 
         MVC   TGCSORT(1),TGCASORT SET FOR ECAST KEY                            
         XC    TGINV,TGINV         CLEAR TGINV FOR RECVAL                       
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLECCDQ,(X'20',0)  READ ECAST REC FOR UPDATE         
         MVC   TGCSORT,SVTGCSRT    RESTORE TGCSORT                              
         BE    CECA8                                                            
         MVC   TGINV,INVNO         RESET TGINV                                  
         XC    TGINV,HEXFFS        (XC SETS CC)                                 
         B     CECA22              DON'T BOTHER IF NO LONGER ON ECAST           
         SPACE 1                   TRY NEXT EPISODE NUMBER                      
         USING TACRD,R3                                                         
CECA8    MVC   TGINV,INVNO         RESET TGINV                                  
         XC    TGINV,HEXFFS        (XC SETS CC)                                 
         XR    R1,R1                                                            
         XC    TGDUB,TGDUB                                                      
         MVI   ERR,TACEENFR        INIT ERR                                     
         MVI   ELCODE,TACRELQ      FIND APPLIED CREDIT HISTORY EL.              
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   CECAXE                                                           
         SPACE                                                                  
         OI    STATUS,DELETR       SET FLAG TO DELETE ETRACK                    
         MVC   TGFULL,TACRBAL      SAVE OLD BALANCE IN TGFULL                   
         MVI   ERR,TACEENEG        INIT ERR                                     
         SPACE                                                                  
         TM    TACRBAL,X'80'       TEST BALANCE NOT NEGATIVE                    
         BO    CECAXE                                                           
         MVI   ERR,0                                                            
         L     R1,TACRBAL          RESTORE APPLIED AMOUNT TO                    
         A     R1,APPLAMNT         CURRENT BALANCE                              
         C     R1,TACRAPPL         BE SURE NOT TO EXCEED TOTAL AMOUNT           
         BNH   *+12                                      TO BE APPLIED          
         L     R1,TACRAPPL                                                      
         MVI   ERR,TACEEFUL        SET ERR                                      
         ST    R1,TACRBAL          NOW HAVE NEW BALANCE                         
         SPACE 1                                                                
         CLI   ERR,0               IF THERE'S AN ERROR                          
         BE    CECA12                                                           
CECAXE   BAS   RE,PUTERR           SAVE IT                                      
         BAS   RE,BLDTACE          BUILD TACEEL                                 
         BNE   CECA12                                                           
         GOTO1 ADDL                                                             
         SPACE                                                                  
         USING TLECD,R1                                                         
CECA12   L     R1,AIO                                                           
         OC    TGFULL,TGFULL       IF BALANCE WAS 0                             
         BNZ   CECA14                                                           
         OC    TACRBAL,TACRBAL     AND IS NO LONGER 0                           
         BZ    CECA20                                                           
         OI    TLECSTAT,TLECSBAL   TURN ON CREDIT BAL BIT                       
         B     CECA18              SET HAVE CREDIT BAL                          
         SPACE                                                                  
CECA14   OC    TACRBAL,TACRBAL     IF BALANCE WAS NOT 0                         
         BNZ   CECA20              AND IS NOW 0                                 
         NI    TLECSTAT,ALL-TLECSBAL TURN OFF CREDIT BAL BIT                    
         SPACE                                                                  
CECA18   MVC   KEY+TLDRSTAT-TLDRD(1),TLECSTAT  ALSO DO IN ACTV PTR              
         GOTO1 WRITE               WRITE BACK ACTIVE PTR                        
         SPACE                                                                  
CECA20   GOTO1 PUTREC              WRITE BACK CAST RECORD                       
         SPACE 1                                                                
         BAS   RE,DETRACK          DELETE ETRACK RECORDS IF NEEDED              
         SPACE 1                                                                
CECA22   LA    R4,L'TASOSEPI(R4)   BUMP TO NEXT SUB-ELEMENT                     
         BCT   R0,CECA4                                                         
         B     CECA2               LOOK FOR ANOTHER TASOEL                      
         SPACE 1                                                                
CECAX    MVC   AIO,AIO1            RESET AIO                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES TACREL FOR RETRO PAYMENTS                        
*              SETS CC EQUAL IF IT HANDLES IT OK, ELSE SETS CC NOT EQ           
         SPACE                                                                  
         USING TACRD,R3                                                         
TACRETRO NTR1                                                                   
         L     R1,TACRBAL                                                       
         C     R1,TACRAPPL         IF BALANCE NOT BIGGER THAN APPLY AMT         
         BNH   NO                  SET CC NOT EQ TO DELETE MANUAL RETRO         
         SPACE                                                                  
         MVC   TACRBAL,TACRAPPL    RESTORE BALANCE                              
         SPACE                                                                  
         OC    ORIGINV,ORIGINV     IF THERE'S AN ORIGINAL INV#                  
         BZ    *+10                                                             
         MVC   TACRINV,ORIGINV     RESTORE IT TO TACREL                         
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO DELETE FIXED CYCLE TRACKING RECORDS                   
         SPACE 1                                                                
DFTRACK  NTR1                                                                   
         OC    FCYCDTES,FCYCDTES   DON'T BOTHER IF NO FIXED CYCLE DATES         
         BZ    DFTX                                                             
         BRAS  RE,SETTAL           SET TALENT FILES                             
         XC    KEY,KEY                                                          
         LA    R3,KEY              INITIALIZE KEY                               
         USING TLFTD,R3                                                         
         MVI   TLFTCD,TLFTCDQ      RECORD CODE                                  
         MVC   TLFTSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         MVC   TLFTCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLFTCAST,TGCSORT+4  CAST INPUT SEQUENCE NUMBER                   
         MVC   TLFTSTRT(6),FCYCDTES  FIXED CYCLES DATES                         
         XC    TLFTSTRT(6),HEXFFS                (COMPLEMENTED)                 
         SPACE 1                                                                
         MVI   ERR,TACEENTR                    INIT ERR                         
         SPACE 1                                                                
         GOTO1 HIGH                GET FIRST DIRECTORY REC FOR CYCLE            
         B     DFT20                                                            
DFT10    GOTO1 SEQ                                                              
         SPACE 1                                                                
DFT20    CLC   TLFTKEY(TLFTTRK-TLFTD),KEYSAVE  TEST FOUND A GOOD ONE            
         BNE   DFTXE                                                            
         MVI   ERR,TACEESUB        INIT ERR                                     
DFT30    CLC   TLFTINV,INVNO       IF FIRST RECORD ISN'T FOR THIS INV.          
         BE    DFT40                                                            
         TM    STATUS,USPECGRR     AND NOT APPLYING AGAINST                     
         BO    DFT10               USE-SPECIFIC GRR                             
         B     DFTXE               GET OUT (CAN'T DELETE IN MIDDLE)             
         SPACE 1                                                                
DFT40    GOTO1 GETREC              GET THE RECORD                               
         SPACE 1                                                                
         L     R3,AIO              R3=A(RECORD)                                 
         USING TLFTD,R3                                                         
         OI    TLFTSTAT,X'80'      DELETE FILE RECORD                           
         GOTO1 PUTREC                                                           
         SPACE 1                                                                
         OI    KEY+TLDRSTAT-TLDRD,X'80'  DELETE DIRECTORY RECORD                
         GOTO1 WRITE                                                            
         B     DFTX                                                             
         SPACE 1                                                                
DFTXE    BRAS  RE,SETTAL                                                        
         GOTO1 RECVAL,DMCB,TLCACCDQ,(X'20',0)  READ CAST REC FOR UPDATE         
         BNE   DFTX                                                             
         SPACE 1                                                                
         BAS   RE,ADJCAST          ADJUST CAST RECORD FOR NEW ERROR             
         GOTO1 PUTREC              WRITE BACK CAST REC (PTRS UNCHANGED)         
         BAS   RE,SHOWERR          SHOW ERROR ON SCREEN                         
DFTX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DELETE ECAST TRACKING RECORDS                         
         SPACE 1                                                                
DETRACK  NTR1                                                                   
         TM    STATUS,DELETR       TEST NEED TO DELETE ETRACK                   
         BZ    XIT                                                              
         BRAS  RE,SETTAL           SET TALENT FILES                             
         XC    KEY,KEY                                                          
         LA    R3,KEY              INITIALIZE KEY                               
         USING TLETD,R3                                                         
         MVI   TLETCD,TLETCDQ      RECORD CODE                                  
         MVC   TLETSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         MVC   TLETCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLETCAT,TGCAT       CATEGORY                                     
         MVC   TLETEPI,TGEPI       EPISODE NUMBER                               
         XC    TLETEPI,HEXFFS      (COMPLEMENTED)                               
         SPACE 1                                                                
         GOTO1 HIGH                GET FIRST DIRECTORY REC FOR EPISODE          
         SPACE 1                                                                
         MVI   ERR,TACEENTR                    INIT ERR                         
         CLC   TLETKEY(TLETTRK-TLETD),KEYSAVE  TEST FOUND A GOOD ONE            
         BNE   DETXE                                                            
         MVI   ERR,TACEESUB        INIT ERR                                     
         CLC   TLETINV,INVNO       IF FIRST RECORD ISN'T FOR THIS INV.          
         BNE   DETXE               GET OUT (CAN'T DELETE IN MIDDLE)             
         SPACE 1                                                                
         GOTO1 GETREC              GET THE RECORD                               
         SPACE 1                                                                
         L     R3,AIO              R3=A(RECORD)                                 
         OI    TLETSTAT,X'80'      DELETE FILE RECORD                           
         GOTO1 PUTREC                                                           
         SPACE 1                                                                
         OI    KEY+TLDRSTAT-TLDRD,X'80'  DELETE DIRECTORY RECORD                
         GOTO1 WRITE                                                            
         B     DETX                                                             
         SPACE 1                                                                
DETXE    XC    TGINV,TGINV         CLEAR TGINV FOR RECVAL                       
         GOTO1 RECVAL,DMCB,TLECCCDQ,(X'20',0) READ ECAST REC FOR UPDATE         
         BE    DETXE5                                                           
         MVC   TGINV,INVNO         RESET TGINV                                  
         XC    TGINV,HEXFFS        (XC SETS CC)                                 
         B     DETX                                                             
         SPACE 1                                                                
DETXE5   MVC   TGINV,INVNO         RESET TGINV                                  
         XC    TGINV,HEXFFS        (XC SETS CC)                                 
         BAS   RE,ADJCAST          ADJUST CAST RECORD FOR NEW ERROR             
         GOTO1 PUTREC              WRITE BACK CAST REC (PTRS UNCHANGED)         
         BAS   RE,SHOWERR          SHOW ERROR ON SCREEN                         
         SPACE 1                                                                
DETX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DELETE DUE COMPANY RECORDS                            
         SPACE 1                                                                
DDUECOMP NTR1                                                                   
         MVI   DUEFLAG,0                                                        
         L     R1,PAYAMT             IF PAYMENT AMOUNT                          
         A     R1,REXPAMT          + REIMBURSED EXPENSES = 0                    
         LTR   R1,R1                                                            
         BZ    XIT                 DON'T BOTHER, PAY DIDN'T ADD REC             
         SPACE                                                                  
         BRAS  RE,SETTAL           SET TALENT FILES                             
         MVC   AIO,AIO2            USE AIO2, SAVE CHECK REC IN AIO1             
         XC    KEY,KEY                                                          
         LA    R3,KEY              INITIALIZE KEY                               
         USING TLDUD,R3                                                         
         MVI   TLDUCD,TLDUCDQ      RECORD CODE                                  
         MVC   TLDUSSN,TGSSN       SOCIAL SECURITY NUMBER                       
         OC    CORPID,CORPID       IF WE HAVE CORP ID                           
         BZ    *+10                                                             
         MVC   TLDUSSN,CORPID      USE IT                                       
         GOTO1 HIGH                                                             
         B     DDU4                                                             
         SPACE 1                                                                
DDU2     GOTO1 SEQ                 TRY NEXT                                     
         LA    R3,KEY                                                           
         SPACE 1                                                                
DDU4     CLC   TLDUKEY(TLDUDUC-TLDUD),KEYSAVE  TEST STILL SAME SSN              
         BNE   DDUXE                                                            
         MVC   DUEKEY,KEY                                                       
         SPACE 1                                                                
         GOTO1 GETREC              GET THE RECORD                               
         SPACE 1                                                                
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
         SPACE 1                                                                
         L     R3,AIO                                                           
         MVI   ELCODE,TADUELQ      GET DUE COMPANY DETAILS ELEMENT              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADUD,R3                                                         
         TM    TADUSTAT,TADUSAUT   TEST THIS RECORD WAS AN AUTO                 
         BZ    DDU2                                                             
         CLC   TADUCINV,INVNO      INSURE CORRECT INVOICE NUMBER                
         BNE   DDU2                                                             
         SPACE 1                                                                
         L     R3,AIO              FOUND IT                                     
         USING TLDUD,R3                                                         
         OI    TLDUSTAT,X'80'      DELETE FILE RECORD                           
         GOTO1 PUTREC                                                           
         SPACE 1                                                                
         OI    KEY+TLDRSTAT-TLDRD,X'80'  DELETE DIRECTORY RECORD                
         GOTO1 WRITE                                                            
         SPACE 1                                                                
         GOTO1 ADDPTRS,DMCB,(X'40',ASVPTRS)                                     
         MVI   DUEFLAG,DUEFNDQ                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(32),DUEKEY                                                   
         GOTO1 HIGH                                                             
         LA    R3,KEY                                                           
         B     DDU4                                                             
         SPACE                                                                  
DDUXE    CLI   DUEFLAG,DUEFNDQ     DUE COMPANY FOUND?                           
         JE    DDUX                                                             
         MVI   ERR,TACEEDUE        SET DUE COMPANY REC NOT FOUND ERROR          
         BAS   RE,PUTERR           SAVE ERROR AND DISPLAY ON SCREEN             
         SPACE                                                                  
DDUX     MVC   AIO,AIO1            RESET AIO                                    
         B     XIT                                                              
*                                                                               
DUEFLAG  DS    XL1                                                              
DUEFNDQ  EQU   C'Y'                DUE COMPANY FOUND?                           
DUEKEY   DS    XL32                                                             
*                                                                               
         EJECT                                                                  
*              ROUTINE TO DELETE USAGE HISTORY RECORD                           
*              HALF CONTAINS CAST INPUT SEQ NUMBER FOR CAST UH RECS             
         SPACE 1                                                                
DUSAGE   NTR1                                                                   
         BRAS  RE,SETTAL           SET TALENT FILES                             
         XC    KEY,KEY                                                          
         LA    R4,KEY              INITIALIZE KEY                               
         USING TLUHD,R4                                                         
         MVI   TLUHCD,TLUHCDQ      RECORD CODE                                  
         MVC   TLUHCSEQ,HALF       CAST INPUR SEQ NUMBER                        
         MVC   TLUHCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLUHUSE,TGUSCDE     USE CODE                                     
         MVC   TLUHINV,TGINV       INVOICE NUMBER (COMPLEMENTED)                
         SPACE 1                                                                
         GOTO1 HIGH                                                             
         CLC   TLUHKEY,KEYSAVE     IF CAN'T FIND THE RECORD                     
         BNE   XIT                 DON'T BOTHER                                 
         SPACE 1                                                                
         MVC   AIO,AIO2            USE AIO2 (IF CAST UH, CHECK IN AIO1)         
         GOTO1 GETREC              GET THE RECORD                               
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
         L     R4,AIO                                                           
         OI    TLUHSTAT,X'80'      DELETE FILE RECORD                           
         GOTO1 PUTREC                                                           
         SPACE 1                                                                
         OI    KEY+TLDRSTAT-TLDRD,X'80'  DELETE DIRECTORY RECORD                
         GOTO1 WRITE                                                            
         GOTO1 ADDPTRS,DMCB,(X'40',ASVPTRS),AUPPTRS                             
         MVC   AIO,AIO1            RESET AIO                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DELETE PAID STATUS AND INFO ON ADVICE REC             
         SPACE 1                                                                
ADVICE   NTR1                                                                   
         OC    ADVNUM,ADVNUM       DON'T BOTHER IF NO ADVICE PAID               
         BZ    XIT                                                              
         BRAS  RE,SETTAL                      SET TALENT FILES                  
         MVC   TGADV,ADVNUM                   SET TGADV                         
         GOTO1 RECVAL,DMCB,TLDVCDQ,(X'20',0)  GET ADVICE RECORD                 
         BNE   DGUX                           DON'T BOTHER IF NOT FOUND         
         SPACE 1                                                                
         GOTO1 SAVPTRS,DMCB,ASVPTRS  BUILD ADVICE POINTER BLOCK                 
         L     R3,AIO                                                           
         MVI   ELCODE,TADVELQ        LOOK FOR ADVICE DETAILS EL.                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADVD,R3                                                         
         NI    TADVSTAT,X'FF'-TADVSPAY-TADVSCMP                                 
         XC    TADVPINF,TADVPINF             CLEAR PAID INFO                    
         DROP  R3                                                               
         SPACE 1                                                                
         L     R3,AIO                        GET RID OF COMPLETION              
         MVI   ELCODE,TAACELQ                ACTIVITY ELEMENT                   
         BAS   RE,GETEL                                                         
         B     ADV20                                                            
ADV10    BAS   RE,NEXTEL                                                        
ADV20    BNE   ADV30                                                            
         USING TAACD,R3                                                         
         CLI   TAACSCR,X'7A'                                                    
         BNE   ADV10                                                            
         MVI   TAACEL,X'FF'                                                     
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         DROP  R3                                                               
         SPACE 1                                                                
ADV30    GOTO1 PUTREC                        WRITE BACK CHANGED REC             
         GOTO1 ADDPTRS,DMCB,(X'20',ASVPTRS)  UPDATE PASSIVE POINTERS            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PUTS ERROR NUMBER AT ERR                                 
*              INTO NEXT AVAILABLE SPACE IN ERRORS                              
         SPACE                                                                  
PUTERR   NTR1                                                                   
         CLI   NUMERR,15           IF ALREADY HAVE 15 ERRORS                    
         BL    *+6                                                              
         DC    H'0'                NOT ENOUGH ROOM IN ERRORS & TACEERR          
         SPACE                                                                  
         LA    R2,ERRORS                                                        
         ZIC   R1,NUMERR                                                        
         AR    R2,R1                                                            
         MVC   0(1,R2),ERR         SAVE ERROR NUMBER                            
         LA    R1,1(R1)            INCREMENT NUMBER OF ERRORS AND SAVE          
         STC   R1,NUMERR                                                        
         BAS   RE,SHOWERR          SHOW ERROR INFO ON SCREEN                    
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE SHOWS THE ERROR INFORMATION ON SCREEN                    
         SPACE                                                                  
         USING SCREEND,R2                                                       
SHOWERR  NTR1                                                                   
         L     R2,ATHISERR         R2=A(ERROR DISPLAY AREA)                     
         LA    R1,APPLSTH                                                       
         CR    R2,R1               IF PAST LAST DISPLAY AREA                    
         BH    XIT                 DON'T BOTHER                                 
         SPACE                                                                  
         TM    TGUSSTA3,SOAPUSE    IF SOAP USE                                  
         BZ    SHOWE3                                                           
         CLC   SVSCRSSN,TGSSN      DON'T DISPLAY IF SSN                         
         BNE   SHOWE2                                                           
         CLC   SVSCRCAT,TGCAT      AND CAT                                      
         BNE   SHOWE2                                                           
         CLC   SVERR,ERR           AND ERR SAME AS LAST DISPLAYED               
         BE    XIT                                                              
SHOWE2   MVC   SVSCRSSN,TGSSN      SAVE THIS SSN, CAT AND ERR DISPLAYED         
         MVC   SVSCRCAT,TGCAT                                                   
         MVC   SVERR,ERR                                                        
         SPACE                                                                  
SHOWE3   MVC   SCRSSN,TGSSN        DISPLAY SSN, CAT AND ERR ON SCREEN           
         MVC   SCRCAT,TGCAT                                                     
         EDIT  (1,ERR),(2,SCRERR),ALIGN=LEFT                                    
         OC    ERRCYCLE,ERRCYCLE                                                
         BZ    SHOWE5                                                           
         GOTO1 DATCON,DMCB,(X'11',ERRCYCLE),(8,SCRCYC)                          
SHOWE5   LA    R2,SCRNXT                                                        
         ST    R2,ATHISERR                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE ADDS ERROR IN ERR TO TACEEL IN CAST                      
*              OR ECAST RECORD AT AIO                                           
         SPACE                                                                  
         USING TACED,R3                                                         
ADJCAST  NTR1                                                                   
         MVI   ELCODE,TACEELQ      GET CAST ERRORS ELEMENT                      
         MVC   TGDUB(3),TGTODAY1                                                
         MVC   TGDUB+3(3),ERRTIME                                               
         GOTO1 GETL,DMCB,(6,TGDUB)                                              
         BNE   ADJC50                                                           
         L     R3,TGELEM                                                        
         MVC   ELEMENT,0(R3)       SAVE IN ELEMENT                              
         GOTO1 DELL,DMCB,(6,TGDUB) DELETE OLD ONE                               
         SPACE                                                                  
         LA    R3,ELEMENT          BUILD NEW ONE                                
         ZIC   R1,TACELEN                                                       
         LR    R4,R3                                                            
         AR    R4,R1               SET R4=END OF EL TO PUT NEW ERROR            
         LA    R1,1(R1)                                                         
         STC   R1,TACELEN          ADD 1 TO ELEMENT LENGTH                      
         ZIC   R1,TACENUM          AND NUMBER OF SUB ELEMENTS                   
         LA    R1,1(R1)                                                         
         STC   R1,TACENUM                                                       
         MVC   0(1,R4),ERR         PUT NEW ERROR INTO ELEMENT                   
         B     ADJCX                                                            
         SPACE                                                                  
ADJC50   XC    ERRORS,ERRORS       SET UP ERRORS                                
         MVC   ERRORS(1),ERR                                                    
         MVI   NUMERR,1                                                         
         BAS   RE,BLDTACE          BUILD TACE ELEMENT                           
         SPACE                                                                  
ADJCX    GOTO1 ADDL                                                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD TACE ELEMENT FROM ERRORS                        
*              SETS CC EQUAL IF THERE'S AT LEAST 1 ERROR AND ELEMENT            
*              GETS BUILT, ELSE SETS CC NOT EQUAL                               
         SPACE                                                                  
         USING TACED,R3                                                         
BLDTACE  NTR1                                                                   
         ZIC   R1,NUMERR           R1=NUMBER OF ERRORS                          
         LTR   R1,R1                                                            
         BZ    NO                  SET CC NOT EQUAL                             
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         MVI   TACEEL,TACEELQ                                                   
         STC   R1,TACENUM          SET NUMBER OF ERRORS                         
         LA    R2,TACELNQ                                                       
         AR    R2,R1                                                            
         STC   R2,TACELEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TACEERR(0),ERRORS                                                
         SPACE                                                                  
         MVC   TACEDATE,TGTODAY1   DATE                                         
         MVC   TACETIME,ERRTIME    TIME                                         
         MVC   TACECYC,ERRCYCLE    CYCLE                                        
         MVC   TACEINV,TGINV       INVOICE                                      
         XC    TACEINV,HEXFFS      UN-COMPLEMENT IT                             
         MVC   TACEUSEQ,TGUSEQU    USE CODE EQUATE                              
         SPACE                                                                  
         B     YES                 SET CC EQUAL                                 
         EJECT                                                                  
*              ERRORS/EXITS                                                     
         SPACE 1                                                                
CODHOLD  MVC   MYMSGNO,=Y(ERCODREL)  COD INVOICE NOT RELEASED                   
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         B     THEEND                                                           
RECNTFND MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEEND                                                           
NOTPAID  MVI   ERROR,ERNOTPD       INVOICE HASN'T BEEN PAID YET                 
         B     THEEND                                                           
NOTAVAIL MVI   ERROR,ERNOTAV       FUNCTION NOT AVAILABLE FOR THIS INV.         
         B     THEEND                                                           
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA) RECORD / ACTION INVALID FOR P+              
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         B     THEEND                                                           
NOCANCEL MVI   ERROR,ERNOCANC      DON'T NEED TO CANCEL - OK TO REOPEN          
         LA    R2,CONACTH                                 (NOT BILLED)          
         B     THEEND                                                           
WEBERR   MVC   MYMSGNO,=Y(ERUSEWEB)                                             
         MVI   MYMTYP,GTMERR       RECORD MUST BE UPDATED FROM                  
         OI    GENSTAT2,USGETTXT   WEB APPLICATION                              
         B     THEEND                                                           
REOPENED BRAS  RE,OUTMQMSG                                                      
         MVI   MYMSGNO1,31         INVOICE REOPENED                             
         CR    R2,R3               IF THERE ARE CAST ERRORS                     
         BE    *+8                                                              
         MVI   MYMSGNO1,80         CAST ERROR(S) FOUND INVOICE REOPENED         
         B     INFEND                                                           
CANCELED BRAS  RE,OUTMQMSG                                                      
         MVI   MYMSGNO1,32         INVOICE CANCELED                             
         B     INFEND                                                           
PRESSPF  MVI   MYMSGNO1,35         INPUT ACCEPTED - PRESS PF13                  
         B     INFEND                                                           
CMTATTCH MVI   MYMSGNO1,247        COMMENT ATTACHED                             
         MVI   COMATCHD,CMTCHK                                                  
         B     INFEND                                                           
         SPACE 1                                                                
INFEND   OI    GENSTAT2,USGETTXT                                                
RECEND   LA    R2,CONRECH                                                       
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL (R3),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
HEXFFS   DC    6X'FF'                                                           
TYPEI    DC    C'I'                                                             
CODMSG   DC    C'(PUR',X'42',C'AGENCY)'                                         
CODMSG2  DC    C'(PUR',X'42',C'CLIENT)'                                         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE GETS THE ORIGINAL INVOICE NUM FROM THE END OF            
*              THE HISTORY COMMENT (LAST 10 CHARS) IN AIO                       
         SPACE                                                                  
GETORINV NTR1  BASE=*,LABEL=*                                                   
         XC    ORIGINV,ORIGINV                                                  
                                                                                
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANURT4I))  GET THE ELEMENT                    
         JNE   XIT                                                              
                                                                                
         USING TANUD,R3                                                         
         L     R3,TGELEM                                                        
         MVC   ORIGINV,TANUMBER    SAVE IT FOR LATER                            
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
*        ROUTINE ENSURES LARGE SCALE GUARANTEE PAYMENTS ARE ELIGIBLE  *         
*        TO BE REOPENED                                               *         
*        ON ENTRY ... R3=A(PAYMENT DETAILS ELEMENT)                   *         
***********************************************************************         
                                                                                
         USING TAPDD,R3                                                         
CHKGRT   NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTREOP      IF ACTION IS REOPEN                          
         JNE   XIT                                                              
                                                                                
         CLC   TAPDUSE,=C'GRT'     EXIT IF PAY TYPE IS NOT GRT                  
         JNE   XIT                                                              
         TM    TAPDPST1,TAPDPCRD   OR IF THIS IS A CREDIT PAYMENT               
         JO    XIT                                                              
         MVC   SVCYCLE,TAPDCYCS    SAVE APPLYING PAYMENT'S CYCLE                
         DROP  R3                                                               
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         USING TLCKD,R4                                                         
         LA    R4,KEY              READ ALL CHECK RECORDS FOR THIS              
         XC    KEY,KEY             INVOICE                                      
         MVI   TLCKCD,TLCKCDQ                                                   
         MVC   TLCKAGY,TGAGY                                                    
         MVC   TLCKINV,TGINV                                                    
         XC    TLCKINV,CGHEXFFS                                                 
         GOTO1 HIGH                                                             
         J     CGRT20                                                           
CGRT10   GOTO1 SEQ                                                              
CGRT20   CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         JNE   CGRT120                                                          
         GOTO1 GETREC                                                           
         DROP  R4                                                               
                                                                                
         USING TACAD,R3                                                         
         L     R3,AIO              R3=A(CHECK RECORD)                           
         MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACAGUA,TACAGUA     IF CAST IS ON A GUARANTEE                    
         JZ    CGRT10                                                           
         MVC   TGGUA,TACAGUA       SAVE GUARANTEE CODE                          
         MVC   SVKEY,KEY           AND CHECK KEY                                
         DROP  R3                                                               
                                                                                
         MVC   SYSFIL,=CL8'TALFIL' SET TO READ TALENT FILE                      
         MVC   SYSDIR,=CL8'TALDIR'                                              
                                                                                
         USING TLGUD,R3                                                         
         LA    R3,KEY              READ GUARANTEE RECORD                        
         XC    KEY,KEY                                                          
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,SVKEY+TLCKSSN-TLCKD                                      
         MVC   TLGUGUA,TGGUA                                                    
         XC    TLGUGUA,CGHEXFFS                                                 
         GOTO1 HIGH                                                             
         CLC   TLGUKEY,KEYSAVE                                                  
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TAGUD,R3                                                         
         L     R3,AIO                                                           
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         OC    TAGUCOM,TAGUCOM     ONLY CARE ABOUT LARGE OVERSCALE              
         JNZ   CGRT110             GUARANTEES                                   
                                                                                
         CLC   TAGUINV,SCRINV      IF REOPENING GUARANTEE PAYMENT               
******** JNE   CGRT50              THAT CREATED THE GUARANTEE ...               
         JNE   CGRT110             THAT CREATED THE GUARANTEE ...               
         DROP  R3                                                               
                                                                                
         USING TLCAPD,R3                                                        
         LA    R3,KEY              READ ALL CAST ATTACHMENTS FOR                
         XC    TLCAPKEY,TLCAPKEY   THIS GUARANTEE                               
         MVI   TLCAPCD,TLCAGCDQ                                                 
         MVC   TLCAGSSN,SVKEY+TLCKSSN-TLCKD                                     
         MVC   TLCAGGUA,TGGUA                                                   
         GOTO1 HIGH                                                             
         J     CGRT40                                                           
CGRT30   GOTO1 SEQ                                                              
CGRT40   CLC   KEY(TLCAGCOM-TLCAPD),KEYSAVE                                     
         JNE   CGRT50                                                           
         CLC   TLCAGCOM,TGCOM      IF ATTACHED TO ANY ADDITIONAL                
         JE    CGRT30              COMMERCIALS, MUST REMOVE THOSE               
         MVC   TGCOM,TLCAGCOM      ATTACHMENTS FIRST                            
         J     CGERRATC                                                         
         DROP  R3                                                               
                                                                                
CGRT50   MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         USING TLCKPD,R4                                                        
         LA    R4,KEY              READ UNPROCESSED CHECK RECORDS               
         XC    KEY,KEY             FOR THIS PERFORMER                           
         MVI   TLCKPCD,TLCKECDQ                                                 
         MVC   TLCKESSN,SVKEY+TLCKSSN-TLCKD                                     
         MVI   TLCKECUR,C'U'                                                    
         J     CGRT70                                                           
                                                                                
CGRT60   CLI   KEYSAVE+TLCKECUR-TLCKPD,C'C'                                     
         JE    CGRT110             NEXT READ UNPROCESSED CAN$ CHECK             
         MVC   KEY,KEYSAVE         RECORDS FOR THIS PERFOMER                    
         MVI   TLCKECUR,C'C'                                                    
                                                                                
CGRT70   GOTO1 HIGH                                                             
         J     CGRT90                                                           
CGRT80   GOTO1 SEQ                                                              
CGRT90   CLC   KEY(TLCKEEMP-TLCKPCD),KEYSAVE                                    
         JNE   CGRT60                                                           
         CLI   TLCKEDTE,0                                                       
         JNE   CGRT80                                                           
         GOTO1 GETREC                                                           
         DROP  R4                                                               
                                                                                
         USING TLCKD,R3                                                         
         L     R3,AIO              R3=A(CHECK RECORD)                           
         MVC   SVAGY,TLCKAGY       SAVE AGENCY                                  
         MVC   SVINV,TLCKINV       AND INVOICE NUMBER                           
         DROP  R3                                                               
                                                                                
         USING TACAD,R3                                                         
         MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TACAGUA,TGGUA       IGNORE IF PAYMENT IS NOT FOR                 
         JNE   CGRT80              THIS GUARANTEE                               
         DROP  R3                                                               
                                                                                
         USING TAPDD,R3                                                         
         L     R3,AIO              R3=A(CHECK RECORD)                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TAPDUSE,=C'GRT'     SKIP GRT PAYMENTS                            
         JE    CGRT80                                                           
         MVC   SVCLI,TAPDCLI       SAVE CLIENT                                  
                                                                                
         MVC   TGDATE,TAPDCYCS     SAVE PAYMENT'S APPLY DATE                    
         TM    TAPDOPT4,TAPDGRTE   INTO TGDATE                                  
         JZ    CGRT100                                                          
         MVC   TGDATE,TAPDCYCE                                                  
         DROP  R3                                                               
                                                                                
         USING TLCKD,R4                                                         
CGRT100  CLC   TGDATE,SVCYCLE                                                   
         JL    CGRT80                                                           
         CLC   TGDATE,SVCYCLE+3    IF APPLY DATE FITS WITHIN THIS               
         JH    CGRT80              GUARANTEE CYCLE                              
         LA    R4,SVKEY            CANNOT REOPEN THIS GUARANTEE                 
         GOTO1 SSNPACK,DMCB,TLCKSSN,TGPID                                       
         J     CGERRROF                                                         
         DROP  R4                                                               
                                                                                
CGRT110  MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   KEY,SVKEY           RESTORE READ SEQUENCE FOR NEXT               
         GOTO1 HIGH                CHECK ON GRT INVOICE                         
         J     CGRT10                                                           
                                                                                
CGRT120  MVC   SYSFIL,=CL8'TALFIL' RESET TO READ TALENT FILE                    
         MVC   SYSDIR,=CL8'TALDIR'                                              
         J     XIT                                                              
                                                                                
CGERRROF MVC   MYMSGNO,=Y(ERRGROAI)  DISPLAY AGENCY/INVOICE                     
         BRAS  RE,BLDAI              THAT MUST BE REOPENED FIRST                
         J     CGERRXIT                                                         
                                                                                
CGERRATC MVC   MYMSGNO,=Y(ERRGROAC)  DISPLAY ATTACHED CAST THAT                 
         BRAS  RE,BLDAC              IS HOLDING UP THE REOPEN                   
                                                                                
CGERRXIT MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 EXIT,DMCB,0                                                      
                                                                                
CGHEXFFS DC    20X'FF'                                                          
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ENSURES PER CYCLE GUARANTEE PAYMENTS ARE ELIGIBLE    *         
*        TO BE REOPENED                                               *         
*        ON ENTRY ... R3=A(PAYMENT DETAILS ELEMENT)                   *         
***********************************************************************         
                                                                                
         USING TAPDD,R3                                                         
CHKPCY   NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTREOP      IF ACTION IS REOPEN                          
         JNE   XIT                                                              
                                                                                
         TM    TAPDSTA2,TAPDSPRI   EXIT IF PAY TYPE IS NOT PER CYCLE            
         JZ    XIT                                                              
         TM    TAPDPST1,TAPDPCRD   OR IF THIS IS A CREDIT PAYMENT               
         JO    XIT                                                              
         MVC   SVCYCLE,TAPDCYCS    SAVE APPLYING PAYMENT'S CYCLE                
         DROP  R3                                                               
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         USING TLCKD,R4                                                         
         LA    R4,KEY              READ ALL CHECK RECORDS FOR THIS              
         XC    KEY,KEY             INVOICE                                      
         MVI   TLCKCD,TLCKCDQ                                                   
         MVC   TLCKAGY,TGAGY                                                    
         MVC   TLCKINV,TGINV                                                    
         XC    TLCKINV,CPHEXFFS                                                 
         GOTO1 HIGH                                                             
         J     CPCY20                                                           
CPCY10   GOTO1 SEQ                                                              
CPCY20   CLC   KEY(TLCKSORT-TLCKD),KEYSAVE                                      
         JNE   CPCY70                                                           
         GOTO1 GETREC                                                           
         DROP  R4                                                               
                                                                                
         USING TACAD,R3                                                         
         L     R3,AIO              R3=A(CHECK RECORD)                           
         MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TACAGUA,TACAGUA     IF CAST IS ON A GUARANTEE                    
         JZ    CPCY10                                                           
         MVC   TGGUA,TACAGUA       SAVE GUARANTEE CODE                          
         MVC   SVKEY,KEY           AND CHECK KEY                                
         DROP  R3                                                               
                                                                                
         MVC   SYSFIL,=CL8'TALFIL' SET TO READ TALENT FILE                      
         MVC   SYSDIR,=CL8'TALDIR'                                              
                                                                                
         USING TLGUD,R3                                                         
         LA    R3,KEY              READ GUARANTEE RECORD                        
         XC    KEY,KEY                                                          
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,SVKEY+TLCKSSN-TLCKD                                      
         MVC   TLGUGUA,TGGUA                                                    
         XC    TLGUGUA,CPHEXFFS                                                 
         GOTO1 HIGH                                                             
         CLC   TLGUKEY,KEYSAVE                                                  
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         USING TAGUD,R3                                                         
         L     R3,AIO                                                           
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR' SET TO READ CHECK FILE                       
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         OC    TAGUCOM,TAGUCOM     ONLY CARE ABOUT PER CYCLE                    
         JZ    CPCY60              GUARANTEES                                   
         DROP  R3                                                               
                                                                                
         USING TLCKPD,R4                                                        
         LA    R4,KEY              READ UNPROCESSED CHECK RECORDS               
         XC    KEY,KEY             FOR THIS PERFORMER                           
         MVI   TLCKPCD,TLCKECDQ                                                 
         MVC   TLCKESSN,SVKEY+TLCKSSN-TLCKD                                     
         MVI   TLCKECUR,C'U'                                                    
         MVC   TLCKEEMP,=C'TP '                                                 
         GOTO1 HIGH                                                             
         J     CPCY40                                                           
CPCY30   GOTO1 SEQ                                                              
CPCY40   CLC   KEY(TLCKEDTE+1-TLCKPCD),KEYSAVE                                  
         JNE   CPCY60                                                           
         GOTO1 GETREC                                                           
         DROP  R4                                                               
                                                                                
         USING TLCKD,R3                                                         
         L     R3,AIO              R3=A(CHECK RECORD)                           
         MVC   SVAGY,TLCKAGY       SAVE AGENCY                                  
         MVC   SVINV,TLCKINV       AND INVOICE                                  
         DROP  R3                                                               
                                                                                
         USING TACAD,R3                                                         
         MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TACAGUA,TGGUA       IGNORE IF PAYMENT IS NOT FOR                 
         JNE   CPCY30              THIS GUARANTEE                               
         DROP  R3                                                               
                                                                                
         USING TAPDD,R3                                                         
         L     R3,AIO              R3=A(CHECK RECORD)                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TAPDSTA2,TAPDSPRI   SKIP PER CYCLE PAYMENTS                      
         JO    CPCY30                                                           
         MVC   SVCLI,TAPDCLI       SAVE CLIENT                                  
                                                                                
         MVC   TGDATE,TAPDCYCS     SAVE PAYMENT'S APPLY DATE                    
         TM    TAPDOPT4,TAPDGRTE   INTO TGDATE                                  
         JZ    CPCY50                                                           
         MVC   TGDATE,TAPDCYCE                                                  
         DROP  R3                                                               
                                                                                
         USING TLCKD,R4                                                         
CPCY50   CLC   TGDATE,SVCYCLE                                                   
         JL    CPCY30                                                           
         CLC   TGDATE,SVCYCLE+3    IF APPLY DATE FITS WITHIN THIS               
         JH    CPCY30              GUARANTEE CYCLE                              
         LA    R4,SVKEY            CANNOT REOPEN THIS GUARANTEE                 
         GOTO1 SSNPACK,DMCB,TLCKSSN,TGPID                                       
         J     PCYERR                                                           
         DROP  R4                                                               
                                                                                
CPCY60   MVC   KEY,SVKEY           RESTORE READ SEQUENCE FOR NEXT               
         GOTO1 HIGH                CHECK ON PER CYCLE INVOICE                   
         J     CPCY10                                                           
                                                                                
CPCY70   MVC   SYSFIL,=CL8'TALFIL' RESET TO READ TALENT FILE                    
         MVC   SYSDIR,=CL8'TALDIR'                                              
         J     XIT                                                              
                                                                                
PCYERR   MVC   MYMSGNO,=Y(ERRGROAI)  DISPLAY AGENCY/INVOICE                     
         BRAS  RE,BLDAI              THAT MUST BE REOPENED FIRST                
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 EXIT,DMCB,0                                                      
                                                                                
CPHEXFFS DC    20X'FF'                                                          
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PUTS AGENCY/INVOICE INTO BLOCK FOR ERROR DISPLAY     *         
***********************************************************************         
                                                                                
BLDAI    NTR1  BASE=*,LABEL=*                                                   
         MVC   BLOCK(25),SPACES                                                 
         MVC   BLOCK+1(6),SVAGY                                                 
                                                                                
         LA    R2,BLOCK+2                                                       
         LHI   R3,2                                                             
BAI10    CLI   0(R2),C' '                                                       
         JE    BAI20                                                            
         LA    R2,1(R2)                                                         
         AHI   R3,1                                                             
         J     BAI10                                                            
                                                                                
BAI20    MVI   0(R2),C'/'                                                       
         GOTO1 TINVCON,DMCB,SVINV,1(R2),DATCON                                  
         MVI   7(R2),0                                                          
         AHI   R3,7                                                             
         STC   R3,BLOCK                                                         
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PUTS AGENCY/COMMERCIAL ID INTO BLOCK FOR ERR DISPLAY *         
***********************************************************************         
                                                                                
BLDAC    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'24',0)                                   
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVC   BLOCK(25),SPACES                                                 
                                                                                
         USING TLCOD,R3                                                         
         L     R3,AIO                                                           
         MVC   BLOCK+1(6),TLCOAGY                                               
         DROP  R3                                                               
                                                                                
         USING TACOD,R3                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         LA    R2,BLOCK+2                                                       
         LHI   R0,2                                                             
BAC10    CLI   0(R2),C' '                                                       
         JE    BAC20                                                            
         LA    R2,1(R2)                                                         
         AHI   R0,1                                                             
         J     BAC10                                                            
                                                                                
BAC20    MVI   0(R2),C'-'                                                       
         MVC   1(L'TACOCID,R2),TACOCID                                          
         MVI   L'TACOCID+1(R2),0                                                
         AHI   R0,L'TACOCID+1                                                   
         STC   R0,BLOCK                                                         
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TURNS OF CSF STATUS ON COMMERCIAL RECORD             *         
***********************************************************************         
                                                                                
UPDCOMM  NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'34',0)                                   
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         L     R3,AIO                                                           
         USING TACOD,R3                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         NI    TACOSTA2,X'FF'-TACOSJPC    TURN OFF STATUS FOR CSF PAID          
         GOTO1 PUTREC               UPDATE COMMERCIAL RECORD                    
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE OUTPUTS MQ MESSAGE                                   *         
***********************************************************************         
                                                                                
OUTMQMSG NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'VS',SVWID        IF INVOICE WAS PAID VIA A VITA               
         JE    OMQ00               SESSION OR VITA COMPLETION ...               
         CLC   =C'RS',SVWID                                                     
         JE    OMQ00                                                            
         CLC   =C'VC',SVWID                                                     
         JE    OMQ00                                                            
         CLC   =C'RC',SVWID                                                     
         JNE   XIT                                                              
                                                                                
OMQ00    CLI   ACTNUM,ACTREOP      ... AND ACTION IS REOPEN                     
         JNE   OMQ10                                                            
         LA    R2,REOMSG           BUILD REOPEN MQ MESSAGE                      
         MVC   RMTAG,=C'session'                                                
         CLI   SVWID+1,C'S'                                                     
         JE    *+10                                                             
         MVC   RMTAG,=C'complet'                                                
         MVC   RMSID,SVWID                                                      
         MVC   RMINV,APPINV                                                     
         MVC   RMAGY,APPAGY                                                     
         OC    RMAGY,SPACES                                                     
         LHI   R3,RMLNQ                                                         
         J     OMQ20                                                            
                                                                                
OMQ10    CLI   ACTNUM,ACTCAN       ... AND ACTION IS CANCEL                     
         JNE   XIT                                                              
         LA    R2,CNLMSG           BUILD CANCEL MQ MESSAGE                      
         MVC   CMTAG,=C'session'                                                
         CLI   SVWID+1,C'S'                                                     
         JE    *+10                                                             
         MVC   CMTAG,=C'complet'                                                
         MVC   CMSID,SVWID                                                      
         MVC   CMINV,APPINV                                                     
         MVC   CMAGY,APPAGY                                                     
         OC    CMAGY,SPACES                                                     
         LHI   R3,CMLNQ                                                         
                                                                                
OMQ20    GOTO1 NTFYVITA,DMCB,(X'80',(R2)),(R3)                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
REOMSG   DS    CL16                                                             
         DC    C'<?xml version="1.0" encoding="UTF-8"?>'                        
         DC    C'<invoice_action>'                                              
         DC    C'<recall '                                                      
RMTAG    DS    CL7                                                              
         DC    C'_id="'                                                         
RMSID    DS    CL10                                                             
         DC    C'" invoice_number="'                                            
RMINV    DS    CL6                                                              
         DC    C'" agency="'                                                    
RMAGY    DS    CL6                                                              
         DC    C'"/>'                                                           
         DC    C'</invoice_action>'                                             
RMLNQ    EQU   *-REOMSG                                                         
                                                                                
CNLMSG   DS    CL16                                                             
         DC    C'<?xml version="1.0" encoding="UTF-8"?>'                        
         DC    C'<invoice_action>'                                              
         DC    C'<cancellation '                                                
CMTAG    DS    CL7                                                              
         DC    C'_id="'                                                         
CMSID    DS    CL10                                                             
         DC    C'" invoice_number="'                                            
CMINV    DS    CL6                                                              
         DC    C'" agency="'                                                    
CMAGY    DS    CL6                                                              
         DC    C'"/>'                                                           
         DC    C'</invoice_action>'                                             
CMLNQ    EQU   *-CNLMSG                                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS CHECK INFO TO GUARANTEE CONTRACT TABLE          *         
*        ON ENTRY ... AIO=A(CHECK RECORD)                                       
***********************************************************************         
                                                                                
ADDGCO   NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTREOP     IF ACTION IS REOPEN                           
         JNE   AGC10                                                            
         CLI   TGUSEQU,UGRT       ONLY ADD TO GUARANTEE CONTRACT                
         JE    AGC10              TABLE IF USE IS GRT                           
         CLI   TGUSEQU,UPNH       OR PNH                                        
         JNE   XIT                                                              
                                                                                
         USING TAFND,R4                                                         
AGC10    MVI   ELCODE,TAFNELQ     IF CHECK HAS GUARANTEE CONTRACT CODE          
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTGCO))                                     
         JNE   XIT                                                              
         L     R4,TGELEM                                                        
                                                                                
         USING TLCKD,R3                                                         
         L     R3,AIO                                                           
                                                                                
         USING GCD,R2                                                           
         LA    R2,GCTAB                                                         
AGC20    CLI   0(R2),X'FF'        FIND FIRST EMPTY SLOT IN GUARANTEE            
         JE    AGC40              CONTRACT TABLE                                
         CLC   GCSSN,TLCKSSN                                                    
         JNE   AGC30                                                            
         CLC   GCGCO,TAFNNAME                                                   
         JE    XIT                                                              
AGC30    LA    R2,GCLNQ(R2)                                                     
         J     AGC20                                                            
                                                                                
AGC40    MVC   GCSSN,TLCKSSN      ADD SOCIAL SECURITY NUMBER                    
         MVC   GCGCO,TAFNNAME     AND GUARANTEE CONTRACT CODE                   
         MVI   GCLNQ(R2),X'FF'    INTO TABLE                                    
         J     XIT                                                              
         DROP  R2,R3,R4                                                         
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PROCESSES GUARANTEE CONTRACT RECORDS                 *         
***********************************************************************         
                                                                                
PROGCO   NTR1  BASE=*,LABEL=*                                                   
         CLI   GCTAB,X'FF'        IF WE NEED TO UPDATE ANY                      
         JE    XIT                GUARANTEE CONTRACTS ...                       
                                                                                
         BRAS  RE,SETTAL          SET TALENT FILES                              
                                                                                
         LA    R3,KEY                                                           
                                                                                
         MVC   TGDATE,CYCSTART    TGDATE=PAYMENT APPLY DATE                     
         TM    PAYOPT4,TAPDGRTE                                                 
         JZ    *+10                                                             
         MVC   TGDATE,CYCEND                                                    
                                                                                
***********************************************************************         
                                                                                
         USING GCD,R2                                                           
PGC00    LA    R2,GCTAB           FOR EACH GRT CONTRACT IN TABLE ...            
         PACK  DUB,GCGCO+2(4)                                                   
         CVB   R1,DUB                                                           
         LNR   R1,R1                                                            
         STCM  R1,15,TGGCNT                                                     
                                                                                
***********************************************************************         
                                                                                
         USING TLGCD,R3                                                         
         XC    KEY,KEY                                                          
         MVI   TLGCCD,TLGCCDQ     READ GUARANTEE CONTRACT KEY                   
         MVC   TLGCSSN,GCSSN                                                    
         MVC   TLGCGCNT,TGGCNT                                                  
         GOTO1 HIGH                                                             
         CLC   TLGCKEY,KEYSAVE                                                  
         JNE   PGC120                                                           
         MVC   SVGCKEY,KEY                                                      
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC             READ GUARANTEE CONTRACT RECORD                
                                                                                
         USING TAGCD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAGCELQ     FIND CONTRACT YEAR THAT CONTAINS              
         BRAS  RE,GETEL2          PAYMENT APPLY DATE                            
         J     *+8                                                              
PGC10    BRAS  RE,NEXTEL2                                                       
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TGDATE,TAGCSTRT                                                  
         JL    PGC10                                                            
         CLC   TGDATE,TAGCEND                                                   
         JH    PGC10                                                            
         MVC   TGDATE,TAGCSTRT    SAVE CONTRACT YEAR START DATE                 
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
         USING TLOTD,R3                                                         
         XC    KEY,KEY                                                          
         MVI   TLOTCD,TLOTCDQ     READ FOR GUARANTEE CONTRACT YEAR'S            
         MVC   TLOTSSN,GCSSN      LATEST TRACKING RECORD                        
         MVC   TLOTGCNT,TGGCNT                                                  
         MVC   TLOTSTRT,TGDATE                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(TLOTSEQ-TLOTD),KEYSAVE                                       
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVC   LSTGCSEQ,TLOTSEQ   SAVE LATEST SEQUENCE NUMBER                   
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TAGCD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAGCELQ                                                   
         BRAS  RE,GETEL2                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   GCBAL,TAGCBAL      CONTRACT YEAR BALANCE                         
         MVC   GCWRKBAL,TAGCBAL   AND WORKING CONTRACT YEAR BALANCE             
         DROP  R4                                                               
                                                                                
         NI    STATUS,X'FF'-MRECGCTK                                            
         CLC   TLOTINV,INVNO      IF INVOICE NUMBER MATCHES INVOICE             
         JNE   PGC20              BEING REOPENED/CANCELLED                      
         CLC   TLOTAGY,TGAGY      AND AGENCY MATCHES AGENCY                     
         JNE   PGC20              SET INVOICE RESPONSIBLE FOR MOST              
         OI    STATUS,MRECGCTK    RECENT GCONTRK                                
         DROP  R3                                                               
                                                                                
***********************************************************************         
                                                                                
         USING TLOTD,R3                                                         
PGC20    XC    KEY,KEY                                                          
         MVI   TLOTCD,TLOTCDQ     READ ALL TRACKING FOR GUARANTEE               
         MVC   TLOTSSN,GCSSN      CONTRACT YEAR                                 
         MVC   TLOTGCNT,TGGCNT                                                  
         MVC   TLOTSTRT,TGDATE                                                  
         GOTO1 HIGH                                                             
         J     PGC40                                                            
PGC30    GOTO1 SEQ                                                              
PGC40    CLC   KEY(TLOTSEQ-TLOTD),KEYSAVE                                       
         JNE   PGC130                                                           
         TM    KEY+TLDRSTAT-TLDRD,X'80'                                         
         JO    PGC30                                                            
                                                                                
         MVC   SVOTKEY,KEY        SAVE GCONTRK KEY                              
                                                                                
         CLC   TLOTINV,INVNO      IF INVOICE NUMBER MATCHES INVOICE             
         JNE   PGC30              BEING REOPENED/CANCELLED                      
         CLC   TLOTAGY,TGAGY      AND AGENCY MATCHES AGENCY                     
         JNE   PGC30                                                            
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC             GET GCONTRK RECORD                            
                                                                                
***********************************************************************         
                                                                                
         CLI   ACTNUM,ACTREOP     IF ACTION IS REOPEN                           
         JNE   PGC90                                                            
         OC    GCBAL,GCBAL        AND BALANCE FOR THE CONTRACT YEAR             
         JZ    PGC80              IS GREATER THAN ZERO                          
         TM    PAYSTAT,TAPDPCRD   AND THIS IS A CREDIT INVOICE                  
         JZ    PGC60                                                            
         TM    STATUS,MRECGCTK    THAT IS RESPONSIBLE FOR THE                   
         JZ    PGC50              CONTRACT YEAR'S LATEST TRACKING               
         BAS   RE,UPDBAL          DECREMENT BALANCE                             
         BAS   RE,DELTRK          AND DELETE TRACKING                           
         J     PGC120                                                           
                                                                                
***********************************************************************         
                                                                                
*                                 IF ACTION IS REOPEN                           
*                                 AND BALANCE FOR THE CONTRACT YEAR             
*                                 IS GREATER THAN ZERO                          
*                                 AND THIS IS A CREDIT INVOICE                  
*                                 THAT IS NOT RESPONSIBLE FOR THE               
*                                 CONTRACT YEAR'S LATEST TRACKING               
PGC50    BAS   RE,UPDBAL          DECREMENT BALANCE                             
         BAS   RE,ADDTRK          AND ADD TRACKING                              
         J     PGC120                                                           
                                                                                
***********************************************************************         
                                                                                
*                                 IF ACTION IS REOPEN                           
*                                 AND BALANCE FOR THE CONTRACT YEAR             
*                                 IS GREATER THAN ZERO                          
*                                 AND THIS IS NOT A CREDIT INVOICE              
PGC60    TM    STATUS,MRECGCTK    THAT IS RESPONSIBLE FOR THE                   
         JZ    PGC70              CONTRACT YEAR'S LATEST TRACKING               
         BAS   RE,UPDBAL          INCREMENT BALANCE                             
         BAS   RE,DELTRK          AND DELETE TRACKING                           
         J     PGC120                                                           
                                                                                
***********************************************************************         
                                                                                
                                                                                
*                                 IF ACTION IS REOPEN                           
*                                 AND BALANCE FOR THE CONTRACT YEAR             
*                                 IS GREATER THAN ZERO                          
*                                 AND THIS IS NOT A CREDIT INVOICE              
*                                 THAT IS NOT RESPONSIBLE FOR THE               
*                                 CONTRACT YEAR'S LATEST TRACKING               
PGC70    BAS   RE,UPDBAL          INCREMENT BALANCE                             
         BAS   RE,ADDTRK          AND ADD TRACKING                              
         J     PGC120                                                           
                                                                                
***********************************************************************         
                                                                                
                                                                                
*                                 IF ACTION IS REOPEN                           
*                                 AND BALANCE FOR THE CONTRACT YEAR             
*                                 IS ZERO                                       
PGC80    BAS   RE,ADDTRK          ADD TRACKING                                  
         J     PGC120                                                           
                                                                                
***********************************************************************         
                                                                                
*                                 IF ACTION IS CANCEL                           
PGC90    OC    GCBAL,GCBAL        AND BALANCE FOR THE CONTRACT YEAR             
         JZ    PGC110             IS GREATER THAN ZERO                          
         TM    PAYSTAT,TAPDPCRD   AND THIS IS A CREDIT INVOICE                  
         JZ    PGC100                                                           
         BAS   RE,UPDBAL          INCREMENT BALANCE                             
         BAS   RE,ADDTRK          AND ADD TRACKING                              
         J     PGC120                                                           
                                                                                
***********************************************************************         
                                                                                
*                                 IF ACTION IS CANCEL                           
*                                 AND BALANCE FOR THE CONTRACT YEAR             
*                                 IS GREATER THAN ZERO                          
*                                 AND THIS IS NOT A CREDIT INVOICE              
PGC100   BAS   RE,UPDBAL          DECREMENT BALANCE                             
         BAS   RE,ADDTRK          AND ADD TRACKING                              
         J     PGC120                                                           
                                                                                
***********************************************************************         
                                                                                
*                                 IF ACTION IS CANCEL                           
*                                 AND BALANCE FOR THE CONTRACT YEAR             
*                                 IS ZERO                                       
PGC110   BAS   RE,ADDTRK          ADD TRACKING                                  
                                                                                
***********************************************************************         
                                                                                
PGC120   MVC   KEY,SVOTKEY                                                      
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         J     PGC30                                                            
                                                                                
***********************************************************************         
                                                                                
PGC130   CLI   GCLNQ(R2),X'FF'                                                  
         JE    XIT                                                              
         LA    R2,GCLNQ(R2)                                                     
         J     PGC00                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
                                                                                
UPDBAL   NTR1                                                                   
         USING TAGCD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAGCELQ                                                   
         BRAS  RE,GETEL2                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         L     RE,GCWRKBAL        ADD TRACKING AMOUNT TO WORKING                
         A     RE,TAGCAMT         BALANCE                                       
         ST    RE,GCWRKBAL                                                      
         DROP  R4                                                               
                                                                                
         MVC   KEY,SVGCKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLGCKEY),KEYSAVE                                           
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC             READ GUARANTEE CONTRACT RECORD                
                                                                                
         USING TAGCD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAGCELQ     FIND CONTRACT YEAR THAT CONTAINS              
         BRAS  RE,GETEL2          PAYMENT APPLY DATE                            
         J     *+8                                                              
UB10     BRAS  RE,NEXTEL2                                                       
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TGDATE,TAGCSTRT                                                  
         JNE   UB10                                                             
         MVC   TAGCBAL,GCWRKBAL                                                 
         DROP  R4                                                               
                                                                                
         GOTO1 PUTREC                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
                                                                                
DELTRK   NTR1                                                                   
         MVI   RDUPDATE,C'Y'                                                    
         MVC   KEY,SVOTKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLOTKEY),KEYSAVE                                           
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         USING TLOTD,R4                                                         
         L     R4,AIO                                                           
         OI    TLOTSTAT,X'80'                                                   
         GOTO1 PUTREC                                                           
         DROP  R4                                                               
                                                                                
         MVI   KEY+TLDRSTAT-TLDRD,X'80'                                         
         GOTO1 WRITE                                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
                                                                                
ADDTRK   NTR1                                                                   
         MVC   KEY,SVOTKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLOTKEY),KEYSAVE                                           
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         ZICM  RE,LSTGCSEQ,2                                                    
         SHI   RE,1                                                             
         STCM  RE,3,LSTGCSEQ                                                    
                                                                                
         USING TLOTD,R4                                                         
         L     R4,AIO                                                           
         MVC   TLOTSEQ,LSTGCSEQ                                                 
         OC    GCBAL,GCBAL                                                      
         JNZ   AT10                                                             
         OI    TLOTSTA,TLOTSBA0                                                 
         DROP  R4                                                               
                                                                                
         USING TAGCD,R4                                                         
AT10     MVI   ELCODE,TAGCELQ                                                   
         BRAS  RE,GETEL2                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   TAGCBAL,GCWRKBAL                                                 
                                                                                
         XR    RE,RE                                                            
         OC    GCBAL,GCBAL                                                      
         JZ    AT20                                                             
         L     RE,TAGCAMT                                                       
         LCR   RE,RE                                                            
AT20     ST    RE,TAGCAMT                                                       
         DROP  R4                                                               
                                                                                
         USING TACMD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TACMEL,TACMELQ                                                   
         MVI   TACMTYPE,TACMTYPG                                                
                                                                                
         CLI   ACTNUM,ACTREOP                                                   
         JNE   AT30                                                             
         MVI   TACMLEN,23                                                       
         MVC   TACMCOMM(12),=C'REOPENED by '                                    
         MVC   TACMCOMM+12(L'TGCTSTAF),TGCTSTAF                                 
         J     AT40                                                             
                                                                                
AT30     MVI   TACMLEN,24                                                       
         MVC   TACMCOMM(13),=C'CANCELLED by '                                   
         MVC   TACMCOMM+13(L'TGCTSTAF),TGCTSTAF                                 
AT40     GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         GOTO1 ADDREC                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         GETEL2 (R4),DATADISP,ELCODE                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        UTILITIES                                                    *         
***********************************************************************         
                                                                                
SETCHK   NTR1  BASE=*,LABEL=*      SET TO USE CHECK FILES                       
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         J     XIT                                                              
SETTAL   NTR1  BASE=*,LABEL=*      SET TO USE TALENT FILES                      
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PROCESS VRE/VNR CANCEL/REOPEN                                *         
***********************************************************************         
VREVNR   NTR1  BASE=*,LABEL=*                                                   
         MVC   SVINPKEY,KEY                                                     
                                                                                
         USING TLUHD,R3                                                         
         LA    R3,KEY              GET USAGE HISTORY FOR INVOICE                
         XC    TLUHKEY,TLUHKEY                                                  
         MVI   TLUHCD,TLUHCDQ                                                   
         MVC   TLUHCOM,TGCOM                                                    
         MVI   DMINBTS,X'08'       USAGE ALREADY DELETED                        
         GOTO1 HIGH                                                             
         J     VRE10                                                            
VRE05    GOTO1 SEQ                                                              
VRE10    CLC   KEY(TLUHCSEQ-TLUHCD),KEYSAVE                                     
         JNE   VRE180                                                           
         LA    R3,KEY                                                           
         CLC   TLUHINV,TGINV       SAME INVOICE?                                
         JNE   VRE05                                                            
         OC    TLUHCSEQ,TLUHCSEQ   ONLY CAST LEVEL                              
         JZ    VRE05                                                            
         MVC   SVUHCSEQ,TLUHCSEQ   SAVE CAST SEQUENCE #                         
         NI    DMINBTS,X'F7'                                                    
                                                                                
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
                                                                                
         XC    SVUHVER,SVUHVER                                                  
         XC    SVCAVER,SVCAVER                                                  
         XC    NEWVER,NEWVER                                                    
                                                                                
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVRE))                                     
         JE    VRE20                                                            
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVNR))                                     
         JNE   VRE05                                                            
                                                                                
         USING TAFND,R4                                                         
VRE20    L     R4,TGELEM           GET VERSIONS                                 
         ZIC   R1,TAFNLEN                                                       
         SHI   R1,TAFNLNQ+1                                                     
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   SVUHVER(0),TAFNNAME SAVE USAGE VERSIONS                          
                                                                                
         MVC   SVUHKEY,KEY         SAVE USAGE KEY                               
         MVC   SVUHTYPE,TAFNTYPE   SAVE TAFN TYPE                               
                                                                                
         USING TLCAD,R3                                                         
         LA    R3,KEY              REMOVE VERSIONS FOR THE PERFORMER            
         XC    TLCAKEY,TLCAKEY                                                  
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         J     VRE30                                                            
VRE25    GOTO1 SEQ                                                              
VRE30    CLC   KEY(TLCASORT-TLCACD),KEYSAVE                                     
         JNE   VRE170                                                           
         CLC   SVUHCSEQ,TLCASEQ    SAME CAST SEQ #?                             
         JNE   VRE25                                                            
                                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         TM    PAYSTAT,TAPDPCRD    CREDIT INVOICE?                              
         JO    VRE100                                                           
*                                                                               
* PROCESS NON-CREDIT INVOICE                                                    
*                                                                               
         CLI   SVUHTYPE,TAFNTVNR   VNR USE?                                     
         JNE   VRE50                                                            
         MVI   ELCODE,TAFNELQ      YES, UPDATE 1ST NON-RENDERED VERS            
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVN1))                                     
         JNE   VRE50                                                            
                                                                                
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
         LA    R6,SVUHVER          COMPARE USAGE VERSIONS TO CAST               
*                                  1ST NON - RENDERED VERSION                   
VRE40    CLI   0(R6),0                                                          
         JE    VRE50                                                            
         CLC   0(1,R6),TAFNNAME                                                 
         JE    *+12                                                             
         AHI   R6,1                                                             
         J     VRE40                                                            
                                                                                
         MVI   0(R4),X'FF'         DELETE IT                                    
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
                                                                                
VRE50    MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,SVUHTYPE)                                           
         JNE   VRE160                                                           
                                                                                
         USING TAFND,R4                                                         
VRE55    L     R4,TGELEM                                                        
         ZIC   R5,TAFNLEN                                                       
         SHI   R5,TAFNLNQ                                                       
         LA    R4,TAFNNAME                                                      
                                                                                
VRE60    LA    R6,SVUHVER          COMPARE USAGE VERSIONS TO CAST               
VRE65    CLI   0(R6),0                                                          
         JE    VRE70                                                            
         CLC   0(1,R6),0(R4)                                                    
         JE    *+12                                                             
         AHI   R6,1                                                             
         J     VRE65                                                            
                                                                                
         MVI   0(R4),0             REMOVE VERSION                               
VRE70    AHI   R4,1                                                             
         BCT   R5,VRE60                                                         
                                                                                
         L     R4,TGELEM                                                        
         XC    WORK,WORK           REBUILD TAFN ELEMENT IN WORK                 
         MVC   WORK(3),0(R4)                                                    
         MVI   WORK+1,TAFNLNQ                                                   
         ZIC   R6,TAFNLEN                                                       
         SHI   R6,TAFNLNQ                                                       
         LA    R4,TAFNNAME         CAST VERSIONS                                
         LA    R5,WORK+3                                                        
                                                                                
VRE80    CLI   0(R4),0                                                          
         JE    VRE90                                                            
         MVC   0(1,R5),0(R4)                                                    
         AHI   R5,1                                                             
         ZIC   RF,WORK+1           UPDATE LENGTH                                
         AHI   RF,1                                                             
         STC   RF,WORK+1                                                        
                                                                                
VRE90    AHI   R4,1                                                             
         BCT   R6,VRE80                                                         
                                                                                
         L     R4,TGELEM                                                        
         MVI   0(R4),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
                                                                                
         CLI   WORK+1,TAFNLNQ      NO VERSIONS?                                 
         JE    VRE160              NOPE, NO ELEMENT TO ADD                      
                                                                                
         XC    ELEM,ELEM                                                        
         ZIC   RF,WORK+1                                                        
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),WORK                                                     
                                                                                
         MVC   TGBYTE,ELEM+TAFNLNQ SAVE 1ST NON-RENDERED VERSION                
                                                                                
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 ADDELEM                                                          
                                                                                
         CLI   SVUHTYPE,TAFNTVNR   VNR USE?                                     
         JNE   VRE160                                                           
         MVI   ELCODE,TAFNELQ      1ST NON-RENDERED VERSION EXISTS?             
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVN1))                                     
         JE    VRE160              YES - LEAVE IT BE                            
*&&DO                                                                           
         USING TAFND,R4            NO - MUST ADD IT                             
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+1                                                
         MVI   TAFNTYPE,TAFNTVN1                                                
         MVC   TAFNNAME,TGBYTE                                                  
         GOTO1 ADDELEM                                                          
*&&                                                                             
         B     VRE160                                                           
*                                                                               
* PROCESS CREDIT INVOICE                                                        
*                                                                               
VRE100   MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,SVUHTYPE)                                           
                                                                                
         BAS   RE,BLDVER           BUILD UPDATED VERSIONS IN NEWVER             
         J     VRE130                                                           
                                                                                
         USING TAFND,R4                                                         
VRE110   L     R4,TGELEM           SAVE CAST'S VERSIONS                         
         ZIC   R5,TAFNLEN                                                       
         SHI   R5,TAFNLNQ+1                                                     
         EX    R5,*+8                                                           
         J     *+10                                                             
         MVC   SVCAVER(0),TAFNNAME                                              
                                                                                
         BAS   RE,BLDVER           BUILD UPDATED VERSIONS IN NEWVER             
                                                                                
VRE120   L     R4,TGELEM           DELETE OLD TAFN ELEMENT                      
         MVI   0(R4),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
                                                                                
VRE130   LA    R4,ELEM             BUILD NEW TAFN ELEMENT                       
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ                                                  
         MVC   TAFNTYPE,SVUHTYPE                                                
                                                                                
         LA    R4,TAFNNAME                                                      
         LA    R5,NEWVER           WITH UPDATED NEW VERSIONS                    
                                                                                
VRE135   CLI   0(R5),0                                                          
         JE    VRE140                                                           
         MVC   0(1,R4),0(R5)                                                    
         AHI   R4,1                                                             
         AHI   R5,1                                                             
         ZIC   RF,ELEM+1           UPDATE ELEMENT LENGTH                        
         AHI   RF,1                                                             
         STC   RF,ELEM+1                                                        
         J     VRE135                                                           
                                                                                
VRE140   MVI   ELCODE,TAFNELQ                                                   
         GOTO1 ADDELEM                                                          
                                                                                
         CLI   SVUHTYPE,TAFNTVNR   VNR USE?                                     
         JNE   VRE160                                                           
         MVI   ELCODE,TAFNELQ      1ST NON-RENDERED VERSION EXISTS?             
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVN1))                                     
         JE    VRE150                                                           
*&&DO                                                                           
         USING TAFND,R4            NO - MUST ADD IT                             
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+1                                                
         MVI   TAFNTYPE,TAFNTVN1                                                
         MVC   TAFNNAME,NEWVER                                                  
         GOTO1 ADDELEM                                                          
*&&                                                                             
         J     VRE160                                                           
                                                                                
VRE150   L     R4,TGELEM                                                        
         MVC   TAFNNAME(1),NEWVER  UPDATE 1ST VERSION                           
                                                                                
VRE160   GOTO1 PUTREC              UPDATE CAST RECORD                           
                                                                                
VRE170   XC    KEY,KEY                                                          
         MVC   KEY(L'TLUHKEY),SVUHKEY                                           
         MVI   DMINBTS,X'08'       USAGE ALREADY DELETED                        
         GOTO1 HIGH                                                             
         J     VRE05                                                            
                                                                                
VRE180   BAS   RE,DUSAGVNR         DELETE USAGE HISTORY FOR VNR                 
                                                                                
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY             RESTORE GETREC/PUTREC                        
         MVC   KEY(L'TLINPKEY),SVINPKEY                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         NI    DMINBTS,X'F7'                                                    
                                                                                
VREVNRX  J     XIT                                                              
*                                                                               
* BUILD UPDATED NEW VERSIONS IN NEWVER                                          
*                                                                               
BLDVER   NTR1                                                                   
         LA    R4,SVCAVER          CAST'S VERSIONS                              
         LA    R6,SVUHVER          USAGE'S VERSIONS                             
         LA    RF,NEWVER           UPDATED NEW VERSIONS                         
                                                                                
BVER10   CLI   0(R6),0             DONE?                                        
         JNE   BVER30                                                           
                                                                                
BVER20   CLI   0(R4),0             USAGE VERSIONS DONE                          
         JE    BLDVERX             MOVE REST OF CAST VERSIONS IN                
         MVC   0(1,RF),0(R4)                                                    
         AHI   R4,1                                                             
         AHI   RF,1                                                             
         J     BVER20                                                           
                                                                                
BVER30   CLI   0(R4),0                                                          
         JNE   BVER40                                                           
         CLI   0(R6),0             CAST VERSIONS DONE                           
         JE    BLDVERX             MOVE REST OF USAGE VERSIONS IN               
         MVC   0(1,RF),0(R6)                                                    
         AHI   R6,1                                                             
         AHI   RF,1                                                             
         J     BVER30                                                           
                                                                                
BVER40   CLC   0(1,R6),0(R4)       SAME VERSION?                                
         JNE   *+14                                                             
         MVC   0(1,RF),0(R6)       YES - SAVE IT                                
         J     BVER60                                                           
                                                                                
         CLC   0(1,R6),0(R4)                                                    
         JH    *+14                                                             
         MVC   0(1,RF),0(R6)       R6 < R4 SO SAVE R6                           
         J     BVER50                                                           
         MVC   0(1,RF),0(R4)       R4 < R6 SO SAVE R4                           
         J     BVER70                                                           
                                                                                
BVER50   AHI   R6,1                                                             
         AHI   RF,1                                                             
         J     BVER10                                                           
                                                                                
BVER60   AHI   R6,1                                                             
BVER70   AHI   R4,1                                                             
         AHI   RF,1                                                             
         J     BVER10                                                           
                                                                                
BLDVERX  J     XIT                                                              
*                                                                               
* ROUTINE TO DELETE ALL USAGE HISTORY RECORDS FOR VNR                           
*                                                                               
DUSAGVNR NTR1                                                                   
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY              INITIALIZE KEY                               
         USING TLUHD,R4                                                         
         MVI   TLUHCD,TLUHCDQ      RECORD CODE                                  
         MVC   TLUHCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         GOTO1 HIGH                                                             
         J     DUVNR10                                                          
DUVNR05  GOTO1 SEQ                                                              
DUVNR10  CLC   KEY(TLUHCSEQ-TLUHCD),KEYSAVE                                     
         JNE   DUVNRX                                                           
         LA    R4,KEY                                                           
         CLC   TLUHINV,TGINV       SAME INVOICE?                                
         JNE   DUVNR05                                                          
                                                                                
         GOTO1 GETREC              GET THE RECORD                               
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
         L     R4,AIO                                                           
         OI    TLUHSTAT,X'80'      DELETE FILE RECORD                           
         GOTO1 PUTREC                                                           
                                                                                
         OI    KEY+TLDRSTAT-TLDRD,X'80'  DELETE DIRECTORY RECORD                
         GOTO1 WRITE                                                            
         GOTO1 ADDPTRS,DMCB,(X'40',ASVPTRS),AUPPTRS                             
         J     DUVNR05                                                          
                                                                                
DUVNRX   MVC   AIO,AIO1            RESET AIO                                    
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
SVUHCSEQ DS    XL2                 USAGE CAST SEQUENCE #                        
SVUHTYPE DS    XL1                 USAGE TAFN TYPE                              
SVUHKEY  DS    XL32                USAGE KEY                                    
SVINPKEY DS    XL32                INVOICE PASSIVE KEY                          
SVUHVER  DS    XL250               USAGE VERSIONS                               
SVCAVER  DS    XL250               CAST VERIONS                                 
NEWVER   DS    XL250               UPDATED NEW VERSIONS                         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
*              DSECT FOR SCREEN TO DISPLAY ERRORS                               
SCREEND  DSECT                                                                  
         DS    CL8                                                              
SCRSSN   DS    CL9                 SS NUMBER                                    
         DS    CL1                                                              
SCRCAT   DS    CL3                 CATEGORY                                     
         DS    CL1                                                              
SCRERR   DS    CL2                 ERROR NUMBER                                 
         DS    CL1                                                              
SCRCYC   DS    CL17                CYCLE DATES                                  
SCRNXT   EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR44D                                                       
         SPACE 3                                                                
*              SAVED STORAGE AT END OF TWA0                                     
         SPACE 1                                                                
INVNO    DS    XL6                 INVOICE NUMBER (NON-COMPLEMENTED)            
SVINVNO  DS    XL6                 SAVED INVNO                                  
SCRINV   DS    XL6                 INVOICE NUMBER FOR SCREEN RECORDS            
PAYDATE  DS    XL3                 PAYMENT DATE                                 
PAYSTAT  DS    XL1                 PAYMENT STATUS BYTE                          
PAYSTAT2 DS    XL1                 PAYMENT STATUS BYTE 2                        
CODPRT   DS    CL1                 FLAG - PRINTED COD INVOICE                   
CHKKEY   DS    CL(L'KEY)           SAVED LAST ACTIVE CHECK KEY                  
INVKEY   DS    CL(L'KEY)           SAVED ACTIVE INVOICE KEY                     
CYCDATES DS    0XL6                CYCLES DATES FOR THIS PAYMENT                
CYCSTART DS    XL3                                                              
CYCEND   DS    XL3                                                              
APPLDATE DS    XL3                 APPLY DATE                                   
APPLCODE DS    CL1                 APPLY CODE FROM CHECK RECORD                 
APPLAMNT DS    F                   APPLY AMOUNT FROM CHECK RECORD               
CORPID   DS    CL9                 CORP ID FOR DUE COMP RECORDS                 
GUARSTAT DS    XL1                 GUARANTEE STATUS BYTE                        
FCYCDTES DS    XL6                 FIXED CYCLE DATES                            
STATUS   DS    XL1                                                              
PFTOPROC EQU   X'80'               PRESS PF13 TO PROCESS REQUEST                
GRTDEL   EQU   X'40'               GRT RECORD DELETED                           
DELETR   EQU   X'20'               NEED TO DELETE ETRACK RECS                   
USPECGRR EQU   X'10'               USE SPECIFIC GRR                             
MRECGCTK EQU   X'08'               INV RESP FOR LAST GCONTRK                    
FTRKFND  EQU   X'04'               FTRACK TRACKING FOUND?                       
COMATCHD DS    XL1                 COMMENT ATTACHED TO INVOICE STATUS           
NOCMT    EQU   C'N'                NO COMMENT ATTACHED TO INVOICE               
CMTCHK   EQU   C'Y'                COMMENT ATTACHED - CHECK IF OK               
CMTOK    EQU   C'O'                COMMENT ATTACHED - ITS OK                    
ERRORS   DS    XL15                LIST OF CAST ERRORS                          
NUMERR   DS    XL1                 TOTAL NUMBER OF ERRORS                       
ERRCYCLE DS    XL6                 CYCLE DATES                                  
ATHISERR DS    A                   A(HEADER OF NEXT FLD TO SHOW ERROR)          
ERR      DS    XL1                                                              
ERRTIME  DS    XL3                 TIME OF ERROR                                
PAYOPT3  DS    XL1                 PAYMENT OPTION 3                             
ORIGINV  DS    CL6                 ORIGINAL INVOICE FOR RETRO PAYMENTS          
ADVNUM   DS    CL6                 ADVICE NUMBER PAID (X'00'S IF NONE)          
AGYSTAT  DS    XL1                 AGENCY STATUS BYTE                           
CLTSTAT  DS    XL1                 CLIENT STATUS BYTE                           
PAYAMT   DS    F                   PAYMENT AMOUNT FOR CAST                      
REXPAMT  DS    F                   REIMBURSED EXPENSES                          
SVSCRSSN DS    CL9                 LAST ERROR SS NUMBER DISPLAYED               
SVSCRCAT DS    CL3                            CATEGORY                          
SVERR    DS    XL1                            ERROR                             
SVTGCSRT DS    CL(L'TGCSORT)       SAVED TGCSORT                                
CANINV   DS    XL6                 CANADIAN INVOICE NUMBER                      
SVKEY    DS    XL(L'KEY)           SAVED KEY                                    
SVCYCLE  DS    XL6                 SAVED GUARANTEE PAYMENT CYCLE                
SVAGY    DS    CL(L'TGAGY)         SAVED AGENCY                                 
SVCLI    DS    CL(L'TGCLI)         SAVED CLIENT                                 
SVINV    DS    CL(L'TGINV)         SAVED INVOICE                                
SVWID    DS    CL18                SAVED WEB APPLICATION ID                     
UPDCSFST DS    XL1                 UPDATE CSF COMM STATUS                       
PAYOPT4  DS    XL1                 SAVED PAYMENT OPTION 4                       
*                                                                               
SVGCKEY  DS    XL(L'KEY)           SAVED GRT CONTRACT KEY                       
SVOTKEY  DS    XL(L'KEY)           SAVED GRT CONTRACT TRACKING KEY              
GCBAL    DS    F                   GRT CONTRACT YEAR BALANCE                    
GCWRKBAL DS    F                   WORING GRT CONTRACT YEAR BALANCE             
LSTGCSEQ DS    XL(L'TLOTSEQ)       LATEST GCONTRK SEQUENCE NUMBER               
*                                                                               
SITAB    DS    11CL(L'TASIINV)     SUBSIDIARY INVOICE TABLE                     
GCTAB    DS    10CL(GCLNQ)         GUARANTEE CONTRACT TABLE                     
         SPACE 1                                                                
INVPTRS  DS    CL((8*L'TLDRREC)+1) INVOICE POINTER BLOCK                        
CHKPTRS  DS    CL((6*L'TLDRREC)+1) CHECK POINTER BLOCK                          
CSTPTRS  DS    CL((6*L'TLDRREC)+1) CAST POINTER BLOCK                           
         SPACE 1                                                                
ASVPTRS  DS    A                   SAVED PASSIVE POINTER BLOCK                  
AUPPTRS  DS    A                   UPDATED PASSIVE POINTER BLOCK                
         SPACE 1                                                                
SVTACO3  DS    XL1                 SAVED TACOSTA3 FROM INVOICE REC              
         EJECT                                                                  
* DDGENTWA  (MUST FOLLOW LAST SCREEN)                                           
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENEQUS                                                                     
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* TAGENFILE                                                                     
* FAGETTXTD                                                                     
* FAWSSVRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAWSSVRD                                                       
         PRINT ON                                                               
**********************************************************************          
*        DSECT FOR GUARANTEE CONTRACT TRACK TABLE                    *          
**********************************************************************          
                                                                                
GCD      DSECT                                                                  
GCSSN    DS    CL9                                                              
GCGCO    DS    CL6                                                              
GCLNQ    EQU   *-GCD                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'165TAGEN44   08/16/16'                                      
         END                                                                    
