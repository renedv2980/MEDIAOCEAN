*          DATA SET TAREP58    AT LEVEL 076 AS OF 07/01/10                      
*PHASE T70358C                                                                  
*INCLUDE DLFLD                                                                  
         TITLE 'T70358 - COKE EDI'                                              
T70358   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYEND-MYSTART,T70358,R7,R8                                       
         LR    RA,RC                                                            
         USING MYD,RA                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         GOTO1 INITIAL,DMCB,0                                                   
         BAS   RE,MYCLEAR                                                       
         L     R1,TWADCONS                                                      
         USING TWADCOND,R1                                                      
         MVC   DYNALLOC,TDYNALLO                                                
         DROP  R1                                                               
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
*                                                                               
         MVC   AMASTD,TWAMASTC     SAVE ADDRESS OF MASTER                       
         L     R2,TWADCONS                                                      
         USING TWADCOND,R2                                                      
         MVC   ALOGOC,TLOGOC       SAVE A(LOGOC)                                
         L     R2,AMASTD                                                        
         USING MASTD,R2                                                         
         MVC   ALOGO,MCVLOGO       SAVE A(LOGO)                                 
         MVC   AREMOT,MCVREMOT     SAVE A(REMOTE)                               
*                                                                               
         BAS   RE,VREC                                                          
         BAS   RE,OPENTAPE                                                      
         BAS   RE,PREP                                                          
         BAS   RE,CLOSTAPE                                                      
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
*                                  OPTIONAL FIELDS                              
         SPACE 1                                                                
         LA    R2,SPLOPTH          VALIDATE OPTIONS BEFORE OTHERS               
         BAS   RE,VOPTS                                                         
         LA    R2,SPLPERH          PERIOD                                       
         GOTO1 ANY                 (REQUIRED NOW)                               
         ST    R2,APERH                                                         
         BAS   RE,VSFTDAT                                                       
         BE    VREC3                                                            
         GOTO1 VALPERD                                                          
*                                                                               
VREC3    MVI   TIQDTYPE,TIQDBILL   (FILTER ON BILL DATE)                        
         GOTO1 DATCON,DMCB,(1,TIQPSTR),(0,THISYMD)                              
         SPACE 1                                                                
VREC4    LA    R2,SPLAGGH          AGENCY GROUP                                 
         CLI   5(R2),0                                                          
         BE    VREC5                                                            
         GOTO1 ANY                                                              
         MVC   TIFAGG,WORK                                                      
         GOTO1 RECVAL,DMCB,TLAGCDQ,(R2),0                                       
         B     VREC6                                                            
         SPACE 1                                                                
VREC5    LA    R2,SPLAGYH          (NEED GROUP OR AGENCY)                       
         GOTO1 ANY                                                              
         SPACE 1                                                                
VREC6    LA    R2,SPLAGYH          AGENCY                                       
         LA    R3,TIFAGY                                                        
         LA    R4,L'TIFAGY                                                      
         LA    R5,TLAYCDQ                                                       
         BAS   RE,SPECFILT                                                      
         B     XIT                                                              
*****    CLI   5(R2),0                                                          
*****    BE    XIT                                                              
*****    GOTO1 ANY                                                              
*****    MVC   TIFAGY,WORK                                                      
*****    GOTO1 RECVAL,DMCB,TLAYCDQ,(R2),0                                       
*****    B     XIT                                                              
         EJECT                                                                  
*              VALIDATE A FILTER EXPRESSION                                     
         SPACE 3                                                                
*              INPUT               R2=A(HEADER)                                 
*                                  R3=A(SYSIO FILTER AREA)                      
*                                  R4=L'ABOVE                                   
*                                  R5=RECORD TYPE CODE                          
         SPACE 1                                                                
SPECFILT NTR1                                                                   
         CLI   5(R2),0             ANY DATA                                     
         BE    XIT                                                              
         OI    6(R2),X'80'                                                      
         GOTO1 ANY                 PUT INTO WORK                                
         BCTR  R4,0                (L'FILTER - 1)                               
         CLC   WORK(2),=C'-@'      CHECK FOR NEGATIVE LIST                      
         BE    NEGLIST                                                          
         CLC   WORK(2),=C'@-'                                                   
         BE    NEGLIST                                                          
         CLI   WORK,C'-'           CHECK FOR NEGATIVE FILTER                    
         BE    NEGFILT                                                          
         CLI   WORK,C'@'           CHECK FOR POSITIVE LIST                      
         BE    POSLIST                                                          
         SPACE 1                                                                
POSFILT  EX    R4,*+8              POSITIVE FILTER                              
         B     *+10                                                             
         MVC   0(0,R3),WORK                                                     
         LA    R4,WORK                                                          
         B     ALLFILT                                                          
         SPACE 1                                                                
NEGFILT  EX    R4,*+8              NEGATIVE FILTER                              
         B     *+10                                                             
         MVC   0(0,R3),WORK+1                                                   
         NI    0(R3),X'FF'-X'40'                                                
         LA    R4,WORK+1                                                        
         SPACE 1                                                                
ALLFILT  LTR   R5,R5                                                            
         BZ    XIT                                                              
         CH    R5,=H'1'                                                         
         BE    ALLUNVL                                                          
         CH    R5,=H'2'                                                         
         BE    ALLUSEVL                                                         
         GOTO1 RECVAL,DMCB,(R5),(X'80',(R4)),0                                  
         BNE   BADFILT                                                          
         B     XIT                                                              
         SPACE 1                                                                
ALLUNVL  GOTO1 UNIVAL,DMCB,(R4)                                                 
         BNE   BADFILT                                                          
         B     XIT                                                              
         SPACE 1                                                                
ALLUSEVL GOTO1 USEVAL,DMCB,(X'40',(R4))                                         
         BNE   BADFILT                                                          
         B     XIT                                                              
         SPACE 1                                                                
POSLIST  EX    R4,*+8              POSITIVE LIST                                
         B     *+10                                                             
         MVC   0(0,R3),WORK+1                                                   
         NI    0(R3),X'FF'-X'80'                                                
         LA    R5,WORK+1                                                        
         B     VALLIST                                                          
         SPACE 1                                                                
NEGLIST  EX    R4,*+8              NEGATIVE LIST                                
         B     *+10                                                             
         MVC   0(0,R3),WORK+2                                                   
         NI    0(R3),X'FF'-X'80'-X'40'                                          
         LA    R5,WORK+2                                                        
         SPACE 1                                                                
VALLIST  LA    R1,1(R5,R4)         CHECK CODE IS NOT TOO LONG                   
         CLI   0(R1),C' '                                                       
         BH    BADLONG                                                          
         XC    KEY,KEY             CHECK LIST EXISTS                            
         LA    R3,KEY                                                           
         USING TLGLD,R3                                                         
         MVI   TLGLCD,TLGLCDQ                                                   
         MVI   TLGLTYPE,TLGLTYPF                                                
         MVC   TLGLLST,0(R5)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BNE   BADLIST                                                          
         B     XIT                                                              
         DROP  R3                                                               
         SPACE 1                                                                
BADFILT  MVC   CONHEAD(L'FLTERR),FLTERR                                         
         GOTO1 BADXIT                                                           
         SPACE 1                                                                
BADLIST  MVC   CONHEAD(L'LSTERR),LSTERR                                         
         B     BADXIT                                                           
         SPACE 1                                                                
BADLONG  MVC   CONHEAD(L'LNGERR),LNGERR                                         
         B     BADXIT                                                           
         EJECT                                                                  
*=====================================================================          
*              VALIDATE USING SOFT DATES                                        
*              R2 = FIELD HEADER                                                
*=====================================================================          
         USING SOFDATD,R1                                                       
VSFTDAT  NTR1                                                                   
         LA    R1,SDBLOCK                                                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         ST    R2,SOFAINP          A(INPUT)                                     
         LA    R3,OUTDATE                                                       
         ST    R3,SOFAOUT          A(OUTPUT)                                    
         MVC   SOFACOM,ACOMFACS    A(COMFACS)                                   
         MVI   SOFITYPE,SOFITYMD   VALIDATE FOR YEAR, MONTH, DAY                
         MVI   SOFOTYPE,SOFOTSD2   12 BYTE EBCIDIC (YYMMDDYYMMDD)               
         MVI   SOFIINDS,SOFIISFT   VALIDATE ONLY SOFT DATES                     
*                                                                               
         MVC   SOFTODAY,TGTODAY0   TODAY'S DATE                                 
         MVC   SOFCTRY,CTRY        COUNTRY CODE                                 
         MVC   SOFLANG,LANG        LANGUAGE CODE                                
         MVI   SOFSYSN,7           TALENT SYSTEM                                
         GOTO1 SOFTDATE,SOFDATD                                                 
         BZ    *+14                                                             
         MVC   ERROR,SOFERROR                                                   
         B     NO                                                               
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    VSFTDAT5                                                         
         CLI   CONOUTH+5,0         RLP ONLINE, IF GROUP NAME IN OUTPUT          
         BE    VSFTDAT5                                                         
         CLC   =C'FILE',CONDEST    AND FILE IN DESTINATION                      
         BE    YES                 DON'T RESOLVE DATES                          
*                                                                               
VSFTDAT5 OI    SOFIINDS,SOFIIRES   RESOLVE THE DATES TO ACTUAL                  
         GOTO1 SOFTDATE,SOFDATD                                                 
         GOTO1 DATCON,DMCB,(0,OUTDATE),(1,TIQPSTR)                              
         GOTO1 DATCON,DMCB,(0,OUTDATE+6),(1,TIQPEND)                            
*                                                                               
         B     YES                                                              
*                                                                               
         DROP  R1                                                               
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 3                                                                
VOPTS    NTR1                                                                   
         MVI   TRACOPT,C'N'                                                     
         MVI   TAPEOPT,C'Y'                                                     
         MVI   SORTOPT,C'N'                                                     
         MVI   LISTOPT,C'N'                                                     
         MVI   DOWNOPT,C'N'                                                     
         ZAP   TRALIMIT,=P'0'                                                   
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
         SPACE 1                                                                
OPT2     CLC   12(5,R4),=C'TRACE'  TRACE OPTION                                 
         BNE   OPT4                                                             
         MVI   TRACOPT,C'Y'                                                     
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    OPTEND                                                           
         CVD   R1,DUB                                                           
         ZAP   TRALIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     CLC   12(5,R4),=C'LIMIT'  LIMIT OPTION                                 
         BNE   OPT5                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    BADOPT                                                           
         CVD   R1,DUB                                                           
         ZAP   RECLIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT5     CLC   12(6,R4),=C'REPORT' REPORT LIMIT                                 
         BNE   OPT6                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    BADOPT                                                           
         CVD   R1,DUB                                                           
         ZAP   REPLIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   12(4,R4),=C'TAPE'   TAPE OPTION                                  
         BNE   OPT7                                                             
         MVC   TAPEOPT,22(R4)                                                   
         B     OPTEND                                                           
         SPACE 1                                                                
OPT7     CLC   12(4,R4),=C'SORT'   SORT OPTION                                  
         BNE   OPT8                                                             
         MVI   SORTOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8     CLC   12(4,R4),=C'LIST'   LIST OPTION                                  
         BNE   OPT9                                                             
         MVI   LISTOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT9     CLC   12(4,R4),=C'DOWN'   DOWN OPTION                                  
         BNE   OPT10                                                            
         MVI   DOWNOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT10    DS    0H                                                               
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
BADXIT   GOTO1 ERREX2                                                           
         SPACE 1                                                                
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
FLTERR   DC    C'** ERROR ** RECORD NOT FOUND'                                  
LSTERR   DC    C'** ERROR ** MISSING FILTER LIST'                               
LNGERR   DC    C'** ERROR ** CODE TOO LONG'                                     
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
TRALIMIT DC    PL6'0'                                                           
RECLIMIT DC    PL6'9999999'                                                     
REPLIMIT DC    PL6'9999999'                                                     
         SPACE 1                                                                
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
PREP     NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
         MVC   MYSORTER,SORTER                                                  
         DROP  R5                                                               
         MVC   MYTITLE,MYSPACES                                                 
         MVC   MYTITLE(15),=C'COKE EDI'                                         
         ZAP   COUNT,=P'0'                                                      
*                                                                               
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
*        MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
*        MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
*        MVC   TIUSERID,TWAORIG    REQUESTING ID                                
*                                                                               
PREP2    OI    TIQFLAG2,TIQFSUB    SUBSIDIARY CHECKS ONLY                       
         MVI   TIREAD,TLINDCDQ     READ INVOICE RECORDS                         
*        OI    TIFINS2N,TAINSADJ   NO ADJUSTMENTS                               
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              HOOK FROM SYSIO                                                  
*======================================================================         
IOHOOK   NTR1                                                                   
         MVC   AIO,TIAREC          SET IOAREA                                   
         LA    R5,TAPEIO           R5=A(TAPE RECORD)                            
         USING EDIDSCT,R5                                                       
*                                                                               
         CLI   TIMODE,PROCREC      INVOICE RECORD HOOK                          
         BNE   IOHOOKX                                                          
*                                                                               
         L     R4,TIAREC           GET ATTENTION CODE                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   IOHOOKX                                                          
         USING TACOD,R4                                                         
         OC    TACOATT,TACOATT     ATTN CODE MUST BE ZERO                       
         BNZ   IOHOOKX                                                          
*                                                                               
         MVC   SVAGY,TIAGY         SAVE AGENCY CODE                             
         MVC   SVCLI,TICLI         SAVE CLIENT CODE                             
         MVC   SVPRD,TIPRD         SAVE PRODUCT CODE                            
         MVC   SVOFF,TIOFF         SAVE OFFICE CODE                             
         XC    CNTRS(CNTRSX),CNTRS CLEAR COUNTERS                               
*                                                                               
         BAS   RE,SETCAN           SET CANADIAN CONVERSION RATE                 
         BAS   RE,SETINV           SET INVOICE INFORMATION                      
IOHOOKX  B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              SET CANADIAN CONVERSION RATE FOR CAN$ INVOICES                   
*======================================================================         
SETCAN   NTR1                                                                   
         XC    CANRATE,CANRATE                                                  
*        MVI   SUBSID,C'N'                                                      
         L     R4,TIAREC           R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAPDD,R4                                                         
*        TM    TAPDSTA2,TAPDSSUB   SUBSIDIARY INVOICE                           
*        BZ    *+8                                                              
*        MVI   SUBSID,C'Y'                                                      
         TM    TAPDSTAT,TAPDSCAN   CHECK THIS IS CANADIAN                       
         BNO   XIT                                                              
         L     R4,TIAREC                                                        
         MVI   ELCODE,TABDELQ      FIND RATE FROM INVOICE                       
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TABDD,R4                                                         
         OC    TABDCCVT,TABDCCVT                                                
         BZ    XIT                                                              
         ZAP   DUB,TABDCCVT                                                     
         CVB   R1,DUB                                                           
         ST    R1,CANRATE                                                       
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              FIX CANADIAN DOLLARS                                             
*======================================================================         
         USING TAPDD,R4            R4=A(TAPD EL)                                
FIXCAN   NTR1                                                                   
         OC    CANRATE,CANRATE     DON'T BOTHER IF RATE NOT DEFINED             
         BZ    XIT                                                              
         LA    R1,TAPDAMTS                                                      
         LA    R0,TAPDAMTL/L'TAPDAMTS                                           
FIXCAN4  BAS   RE,FIXCAN6          CONVERT THE TAPDS                            
         LA    R1,4(R1)                                                         
         BCT   R0,FIXCAN4                                                       
         B     XIT                                                              
         SPACE 3                                                                
FIXCAN6  NTR1                      ADJUST R1=A(FULLWORD) BY CANRATE             
         L     RF,0(R1)                                                         
         M     RE,CANRATE                                                       
         D     RE,=F'5000'                                                      
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R1)                                                         
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              THIS ROUTINE SETS TAPEIO INFO AT INVOICE LEVEL                   
*======================================================================         
SETINV   NTR1                                                                   
         L     R4,TIAREC           R4=A(INVOICE RECORD)                         
SINV5    BAS   RE,SETAPD           SET TAPD INFO (AMOUNT/SOR)                   
         BAS   RE,SETABD           SET TABD AMOUNTS                             
         OC    CNTINV,CNTINV       DON'T SEND IF INVOICE TOTAL = 0              
         BZ    XIT                                                              
         AP    COUNT,=P'1'                                                      
*                                                                               
         BRAS  RE,SETHDR           HEADER SEGMENT                               
         BRAS  RE,SETBIG           BEGINNING SEGMENT FOR INVOICE                
         BRAS  RE,SETREF23         INVOICE DESCRIPTION                          
         BRAS  RE,SETN1            VENDOR NAME                                  
*                                                                               
         BAS   RE,SETBRND          LOOKUP BRAND BY CLIENT PRODUCT               
*                                                                               
         BRAS  RE,SETIT1           BASELINE ITEM DATA (INVOICE)                 
         BRAS  RE,SETREFS          DETAIL REFS                                  
*                                                                               
         BRAS  RE,SETTDS           TOTAL MONETARY VALUE SUMMARY                 
*        BRAS  RE,SETTXI           TAX INFO SEGMENT (IGNORE)                    
         BRAS  RE,SETCTT           TRANS TOTALS SEGMENT                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              THIS ROUTINE SETS INFO FROM TAPDD                                
*======================================================================         
SETAPD   NTR1                                                                   
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
*                                                                               
         BAS   RE,FIXCAN           ADJUST TO US$ IF NECESSARY                   
         L     R1,TAPDGRS                                                       
         A     R1,TAPDREXP                                                      
         A     R1,TAPDAPPL                                                      
         A     R1,TAPDGUAR                                                      
         ST    R1,CNTTUSGE                                                      
*                                                                               
         L     R1,TAPDPNH          P&H                                          
         ST    R1,CNTPNH                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              THIS ROUTINE SETS AMOUNTS FROM TABDD                             
*======================================================================         
SETABD   NTR1                                                                   
         MVI   ELCODE,TABDELQ      GET BILLING DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TABDD,R4                                                         
         MVC   CNTINV,TABDTOT      INVOICE AMOUNT                               
*                                                                               
         L     R2,TABDTAX                                                       
         A     R2,TABDFICR                                                      
         ST    R2,CNTTAX           PAYROLL TAXES                                
*                                                                               
         L     R2,TABDHND                                                       
         A     R2,TABDHNDC                                                      
         ST    R2,CNTHAND          HANDLING                                     
*                                                                               
         L     R2,TABDGST                                                       
         A     R2,TABDPST                                                       
         ST    R2,CNTGST           GST+PST                                      
*                                                                               
         TM    TABDSTAT,TABDSCNW   TEST T&H IN CAN$                             
         BZ    SETABD7                                                          
         LA    R1,CNTTAX                                                        
         BAS   RE,FIXCAN6                                                       
         LA    R1,CNTHAND                                                       
         BAS   RE,FIXCAN6                                                       
         LA    R1,CNTGST                                                        
         BAS   RE,FIXCAN6                                                       
*                                                                               
SETABD7  B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              SETBRND                                                          
*              OUTPUT: R3 POINTS TO BRAND                                       
*======================================================================         
SETBRND  NTR1                                                                   
*                                                                               
         MVC   SVMATC,MYSPACES                                                  
         MVC   SVCCTR,MYSPACES                                                  
         MVC   SVKEY,TIKEY                                                      
*                                                                               
         MVC   AIO,AIO2            READ PRODUCT RECORD INTO AIO2                
         MVC   TGAGY,TIAGY                                                      
         MVC   TGCLI,TICLI                                                      
         MVC   TGPRD,TIPRD                                                      
*                                                                               
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'A0',0),0                                  
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTMAT))    MATERIAL CODE                    
         BNE   SETBRND5                                                         
         L     R4,TGELEM                                                        
         USING TANUD,R4                                                         
         ZIC   R1,TANULEN                                                       
         SH    R1,=Y(TANULNQ+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVMATC(0),TANUMBER                                               
*                                                                               
SETBRND5 GOTO1 GETL,DMCB,(1,=AL1(TANUTCTR))    MATERIAL CODE                    
         BNE   SETBRNDX                                                         
         L     R4,TGELEM                                                        
         USING TANUD,R4                                                         
         ZIC   R1,TANULEN                                                       
         SH    R1,=Y(TANULNQ+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVCCTR(0),TANUMBER                                               
*                                                                               
SETBRNDX MVC   AIO,TIAREC                                                       
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*              TAPE ROUTINES                                                    
         SPACE 3                                                                
OPENTAPE NTR1                                                                   
         CLI   TAPEOPT,C'Y'                                                     
         BNE   XIT                                                              
         L     R1,DRONE            USING UNUSED CORERES AREA                    
         CLI   0(R1),X'90'         TO SAVE RESULTS FILE NUMBER                  
         BNE   *+8                 BETWEEN REQUESTS                             
         MVI   0(R1),0                                                          
         ZIC   RF,0(R1)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(R1)                                                         
         BCTR  RF,0                                                             
         MVC   WORK(20),=CL20'TALDISK.TA0CODS1'                                 
         GOTO1 DYNALLOC,DMCB,(0,=CL8'EDIOUT'),((RF),WORK)                       
         OPEN  (EDIOUT,OUTPUT)                                                  
         B     XIT                                                              
         SPACE 1                                                                
CLOSTAPE NTR1                                                                   
         BAS   RE,SPLAT                                                         
         LA    R2,MYP                                                           
         USING MYPRINT,R2                                                       
         EDIT  (P6,TAPCOUNT),(7,PTOTALS)                                        
         MVC   PTOTALS+8(12),=C'TAPE RECORDS'                                   
         DROP  R2                                                               
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
         CLI   TAPEOPT,C'Y'                                                     
         BNE   XIT                                                              
         L     R2,=A(EDIOUT)                                                    
         CLOSE EDIOUT                                                           
         B     XIT                                                              
         SPACE 1                                                                
PUTTAPE  NTR1                                                                   
         AP    TAPCOUNT,=P'1'                                                   
         MVC   RECTYPE,=CL16'OUTPUT'                                            
         CLI   TRACOPT,C'Y'                                                     
         BNE   PUTTAPE2                                                         
         CP    TAPCOUNT,TRALIMIT                                                
         BH    PUTTAPE2                                                         
         BAS   RE,TRACEM                                                        
         SPACE 1                                                                
PUTTAPE2 PUT   EDIOUT,TAPEIO                                                    
         B     XIT                                                              
         EJECT                                                                  
*              TRACING ROUTINES                                                 
         SPACE 3                                                                
TRACEINV NTR1                                                                   
         L     R4,TIAMAIN                                                       
         MVC   RECTYPE,=CL16'INVOICE'                                           
         BAS   RE,TRACEREC                                                      
         B     XIT                                                              
         SPACE 1                                                                
TRACECHK NTR1                                                                   
         L     R4,TIAREC                                                        
         MVC   RECTYPE,=CL16'CHECK'                                             
         BAS   RE,TRACEREC                                                      
         B     XIT                                                              
         SPACE 1                                                                
TRACEREC NTR1                                                                   
         MVI   MYP,X'BF'                                                        
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP+60(16),RECTYPE                                               
         BAS   RE,SPLAT                                                         
         LA    R2,40                                                            
         BAS   RE,TRACEL                                                        
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
TRACERC2 BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         ZIC   R2,1(R6)                                                         
         BAS   RE,TRACEL                                                        
         B     TRACERC2                                                         
         SPACE 1                                                                
TRACEM   NTR1                                                                   
         CLI   TRACOPT,C'Y'                                                     
         BNE   XIT                                                              
         MVI   MYP,X'BF'                                                        
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP+60(16),RECTYPE                                               
         BAS   RE,SPLAT                                                         
         LA    R2,TAPEIO                                                        
         LA    R0,4                                                             
         SPACE 1                                                                
TRACE2   MVC   MYP(100),0(R2)                                                   
         OC    MYP(100),MYSPACES                                                
         BAS   RE,SPLAT                                                         
         LA    R2,100(R2)                                                       
         BCT   R0,TRACE2                                                        
         MVC   MYP(83),0(R2)                                                    
         OC    MYP(83),MYSPACES                                                 
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
TRACEL   NTR1                                                                   
*                                  R2=LENGTH, R6=ADDRESS                        
         MVC   MYP,MYSPACES                                                     
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP(0),0(R6)                                                     
         OC    MYP,MYSPACES                                                     
         GOTO1 HEXOUT,DMCB,(R6),MYP3,132,=C'SEP'                                
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP2(0),MYP3                                                     
         MVC   MYP3,MYSPACES                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP3(0),MYP4                                                     
         MVC   MYP4,MYSPACES                                                    
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              SORT UTILITIES                                                   
         SPACE 3                                                                
SORTPUT  NTR1                                                                   
         AP    OUTCOUNT,=P'1'                                                   
         MVC   RECTYPE,=CL16'SORT RECORD'                                       
         CP    OUTCOUNT,TRALIMIT                                                
         BH    *+8                                                              
         BAS   RE,TRACEM                                                        
         CLI   SORTFRST,C'Y'                                                    
         BNE   SORTPUT2                                                         
         GOTO1 MYSORTER,DMCB,SORTCARD,RECCARD                                   
         MVI   SORTFRST,C'N'                                                    
         SPACE 1                                                                
SORTPUT2 GOTO1 MYSORTER,DMCB,=C'PUT',TAPEIO                                     
         B     XIT                                                              
         SPACE 1                                                                
SORTMOVE NTR1                                                                   
         LA    R4,TAPEIO                                                        
         MOVE  (TAPEIO,623),0(R2)                                               
         B     XIT                                                              
         DS    0F                                                               
LASTSKEY DC    XL20'00'                                                         
SORTFRST DC    C'Y'                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(623)'                                 
         EJECT                                                                  
*              ODD ROUTINES                                                     
         SPACE 3                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   P,MYP                                                            
         MVC   P2,MYP2                                                          
         MVC   P3,MYP3                                                          
         MVC   P4,MYP4                                                          
         GOTO1 SPOOL,DMCB,(R5)                                                  
         DROP  R5                                                               
         BAS   RE,MYCLEAR                                                       
         B     XIT                                                              
         SPACE 1                                                                
MYCLEAR  NTR1                                                                   
         MVI   MYP,C' '                                                         
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP2,MYP                                                         
         MVC   MYP3,MYP                                                         
         MVC   MYP4,MYP                                                         
         MVC   MYSPACES,MYP                                                     
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              HEADLINES ETC                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   H1+52(24),MYTITLE                                                
         GOTO1 CENTER,DMCB,H1+52,24                                             
         GOTO1 UNDERLIN,DMCB,(24,H1+52),(X'BF',H2+52)                           
         MVC   H3+52(6),=C'PERIOD'                                              
         L     R1,APERH                                                         
         MVC   H3+59(17),8(R1)                                                  
         MVC   H6,MYH6                                                          
         MVC   H7,MYH7                                                          
         DROP  R5                                                               
         XIT1                                                                   
         SPACE 1                                                                
MYSPECS  SSPEC H1,1,RUN                                                         
         SSPEC H3,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H3,112,PAGE                                                      
         DC    H'0'                                                             
         SPACE 1                                                                
APERH    DS    A                                                                
         SPACE 1                                                                
MYH6     DS    0H                                                               
         DC    CL34' '                                                          
         DC    CL6'AGENCY'                                                      
         DC    CL1' '                                                           
         DC    CL7'INVOICE'                                                     
         DC    CL1' '                                                           
         DC    CL16'ESTIMATE'                                                   
         DC    CL1' '                                                           
         DC    CL12'COMMERCIAL'                                                 
         DC    CL1' '                                                           
         DC    CL12'GROSS AMOUNT'                                               
         DC    CL1' '                                                           
         DC    CL12'     P AND H  '                                             
         DC    CL1' '                                                           
         DC    CL5'PRIOR'                                                       
         DC    CL22' '                                                          
         SPACE 1                                                                
MYH7     DS    0H                                                               
         DC    CL34' '                                                          
         DC    CL6'------'                                                      
         DC    CL1' '                                                           
         DC    CL7'NUMBER '                                                     
         DC    CL1' '                                                           
         DC    CL16'----------------'                                           
         DC    CL1' '                                                           
         DC    CL12'----------'                                                 
         DC    CL1' '                                                           
         DC    CL12'------------'                                               
         DC    CL1' '                                                           
         DC    CL12'     -------  '                                             
         DC    CL1' '                                                           
         DC    CL5'MONTH'                                                       
         DC    CL22' '                                                          
         SPACE 1                                                                
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
*                                  COUNTS                                       
TRACOUNT DC    PL6'0'                                                           
CHKCOUNT DC    PL6'0'                                                           
INVCOUNT DC    PL6'0'                                                           
OUTCOUNT DC    PL6'0'                                                           
RECCOUNT DC    PL6'0'                                                           
         DS    0D                                                               
THSCOUNT DC    PL8'0'                                                           
TAPCOUNT DC    PL6'0'                                                           
REPCOUNT DC    PL6'0'                                                           
TPCH     DC    H'1878'                                                          
         SPACE 1                                                                
ADJMONTB DC    C'010203040506070809101112010203040506070809101112'              
         DC    X'FF'               END OF TABLE MARKER                          
         SPACE 1                                                                
INVAPPLY DS    F                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              OTHER AREAS NOT DIRECTLY ADDRESSABLE                             
*                                                                               
EDIOUT   DCB   DDNAME=EDIOUT,DSORG=PS,MACRF=(PM),                      X        
               RECFM=VB,LRECL=4004,BLKSIZE=32760                                
         SPACE 3                                                                
         EJECT                                                                  
*======================================================================         
*              PUT2TAPE                                                         
*======================================================================         
PUT2TAPE NTR1  BASE=*,LABEL=*                                                   
         PUT   EDIOUT,TAPEIO                                                    
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
*              CLRTAPE                                                          
*======================================================================         
CLRTAPE  NTR1  BASE=*,LABEL=*                                                   
         LA    R5,TAPEIO                                                        
         LA    R1,8                                                             
CLRTAPE1 MVI   0(R5),C' '                                                       
         MVC   1(249,R5),0(R5)                                                  
         LA    R5,250(R5)                                                       
         BCT   R1,CLRTAPE1                                                      
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*======================================================================         
*              SETHDR                                                           
*======================================================================         
SETHDR   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,CLRTAPE                                                       
         LA    R5,TAPEIO                                                        
         USING EDIDSCT,R5                                                       
         MVC   EDTRID,=CL6'810'                                                 
         MVC   EDHDSEQ,=C'00000000000'                                          
         MVC   EDHDDDS,=CL15'DDS'                                               
         MVC   EDHDPRF,=CL15'COKE-BILLING'                                      
         MVC   EDILN,=Y(EDHDLNQ)   LENGTH                                       
*                                                                               
         BRAS  RE,PUT2TAPE                                                      
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
*              SETBIG                                                           
*======================================================================         
SETBIG   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,CLRTAPE                                                       
         LA    R5,TAPEIO                                                        
         USING EDIDSCT,R5                                                       
         MVC   EDTRID,=CL6'810'                                                 
         MVC   EDBISEG,=C'BIG'                                                  
         MVC   EDBISEQ,=C'002'                                                  
         MVC   EDBIRSRV,=C'00000'                                               
         MVC   EDBITRPC,=C'00000'                                               
         GOTO1 DATCON,DMCB,(1,TGTODAY1),(20,EDBIDATE)                           
***      GOTO1 DATCON,DMCB,(1,TIBIDATE),(20,EDBIDATE)                           
         GOTO1 TINVCON,DMCB,TIINV,EDBIINVN,DATCON                               
         MVC   EDBITRTY,=C'DI'     DEBIT                                        
         TM    CNTINV,X'80'        NEGATIVE?                                    
         BZ    *+10                                                             
         MVC   EDBITRTY,=C'CR'     CREDIT                                       
         MVC   EDILN,=Y(EDBILNQ)                                                
*                                                                               
         BRAS  RE,PUT2TAPE                                                      
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
*              SETREF23                                                         
*======================================================================         
SETREF23 NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,CLRTAPE                                                       
         LA    R5,TAPEIO                                                        
         USING EDIDSCT,R5                                                       
         MVC   EDTRID,=CL6'810'                                                 
         MVC   EDHRSEG,=C'REF'                                                  
         MVC   EDHRSEQ,=C'005'                                                  
         MVC   EDHRRSRV,=C'00000'                                               
         MVC   EDHRRIQ,=C'23 '                                                  
         MVC   EDHRRIDT,=CL30'0101'                                             
         MVC   EDILN,=Y(EDHRLNQ)                                                
*                                                                               
         BRAS  RE,PUT2TAPE                                                      
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
*              SETN1                                                            
*======================================================================         
SETN1    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,CLRTAPE                                                       
         LA    R5,TAPEIO                                                        
         USING EDIDSCT,R5                                                       
         MVC   EDTRID,=CL6'810'                                                 
         MVC   EDN1SEG,=C'N1 '                                                  
         MVC   EDN1SEQ,=C'008'                                                  
         MVC   EDN1RSRV,=C'00000'                                               
         MVC   EDN1ENIC,=C'SF '                                                 
         MVC   EDN1IDCQ,=C'ZZ'                                                  
         MVC   EDN1IDCD,=CL80'1003963'                                          
         MVC   EDILN,=Y(EDN1LNQ)                                                
*                                                                               
         BRAS  RE,PUT2TAPE                                                      
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
*              SETIT1                                                           
*======================================================================         
         USING BRNDD,R3                                                         
SETIT1   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,CLRTAPE                                                       
         LA    R5,TAPEIO                                                        
         USING EDIDSCT,R5                                                       
         MVC   EDTRID,=CL6'810'                                                 
         MVC   EDI1SEG,=C'IT1'                                                  
         MVC   EDI1SEQ,=C'035'                                                  
         MVC   EDI1RSRV,=C'00000'                                               
         MVI   EDI1QIDC,C'0'                                                    
         MVI   EDI1QTIN,C'1'                                                    
         MVC   EDI1UNCD,=C'EA'                                                  
         MVI   EDI1UPDC,C'2'                                                    
         EDIT  CNTINV,(16,EDI1UNPR),0,ALIGN=LEFT,ZERO=NOBLANK                   
         MVC   EDI1PSIQ,=C'BC'                                                  
         LTR   R3,R3                                                            
         BZ    *+10                                                             
         MVC   EDI1PSID(10),SVMATC                                              
         MVC   EDILN,=Y(EDI1LNQ)                                                
*                                                                               
         BRAS  RE,PUT2TAPE                                                      
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
*              SETREFS                                                          
*======================================================================         
SETREFS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,CLRTAPE                                                       
         LA    R5,TAPEIO                                                        
         USING EDIDSCT,R5                                                       
         MVC   EDTRID,=CL6'810'                                                 
         MVC   EDDRSEG,=C'REF'                                                  
         MVC   EDDRSEQ,=C'050'                                                  
         MVC   EDDRRSRV,=C'00000'                                               
         MVC   EDILN,=Y(EDDRLNQ)                                                
*                                                                               
         MVC   EDDRRIQ,=C'DP '     COST CENTER                                  
         LTR   R3,R3                                                            
         BZ    *+10                                                             
         MVC   EDDRRIDT(10),SVCCTR                                              
         BRAS  RE,PUT2TAPE                                                      
*                                                                               
         MVC   EDDRRIQ,=C'OQ '     INTERNAL ORDER                               
         MVC   EDDRRIDT,=CL30'500000170'                                        
         CLI   TIMED,C'R'                                                       
         BNE   *+10                                                             
         MVC   EDDRRIDT,=CL30'500000171'                                        
         BRAS  RE,PUT2TAPE                                                      
*                                                                               
         MVC   EDDRRIQ,=C'12 '     G/L ACCOUNT                                  
         MVC   EDDRRIDT,=CL30'0891601000'                                       
         CLI   TIMED,C'R'                                                       
         BNE   *+10                                                             
         MVC   EDDRRIDT,=CL30'0891603000'                                       
*                                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TANPELQ        IF NWK USE, INVOICE HAS AIR DATE           
         BRAS  RE,GETEL                                                         
         BNE   SREFS10                                                          
         USING TANPD,R4                                                         
         CLC   TANPDATE(1),TGTODAY1  IF AIR DATE YEAR NEQ SUBMISSION YR         
         BE    SREFS20               USE ACCRUAL G/L ACCOUNT                    
         MVC   EDDRRIDT,=CL30'0205105014'                                       
         B     SREFS20                                                          
*                                    IF NOT NETWORK USE,                        
SREFS10  CLC   TIBIDATE(1),TGTODAY1  IF INVOICE YEAR NEQ SUBMISSION YR,         
         BE    *+10                  USE ACCRUAL G/L ACOUNT                     
         MVC   EDDRRIDT,=CL30'0205105014'                                       
SREFS20  BRAS  RE,PUT2TAPE                                                      
*                                                                               
         MVC   EDDRRIQ,=C'EX '     ITEM TEXT REF                                
         MVC   EDDRRIDT,=CL30'M010'                                             
         BRAS  RE,PUT2TAPE                                                      
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
*              SETTDS                                                           
*======================================================================         
SETTDS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,CLRTAPE                                                       
         LA    R5,TAPEIO                                                        
         USING EDIDSCT,R5                                                       
         MVC   EDTRID,=CL6'810'                                                 
         MVC   EDSTSEG,=C'TDS'                                                  
         MVC   EDSTSEQ,=C'081'                                                  
         MVC   EDSTRSRV,=C'00000'                                               
         MVI   EDSTAMDC,C'2'                                                    
         EDIT  CNTINV,(16,EDSTAMNT),0,ALIGN=LEFT,ZERO=NOBLANK                   
         MVC   EDILN,=Y(EDSTLNQ)                                                
*                                                                               
         BRAS  RE,PUT2TAPE                                                      
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
*              SETTXI                                                           
*======================================================================         
SETTXI   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,CLRTAPE                                                       
         LA    R5,TAPEIO                                                        
         USING EDIDSCT,R5                                                       
         MVC   EDTRID,=CL6'810'                                                 
         MVC   EDSXSEG,=C'TXI'                                                  
         MVC   EDSXSEQ,=C'082'                                                  
         MVC   EDSXRSRV,=C'00000'                                               
         MVC   EDSXTXTC,=C'SU'                                                  
         MVI   EDSXMADC,C'2'                                                    
         EDIT  CNTTAX,(16,EDSXMAMT),0,ALIGN=LEFT,ZERO=NOBLANK                   
*        MVC   EDSXASID,????                                                    
         MVC   EDILN,=Y(EDSXLNQ)                                                
*                                                                               
         BRAS  RE,PUT2TAPE                                                      
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
*              SETCTT                                                           
*======================================================================         
SETCTT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,CLRTAPE                                                       
         LA    R5,TAPEIO                                                        
         USING EDIDSCT,R5                                                       
         MVC   EDTRID,=CL6'810'                                                 
         MVC   EDSCSEG,=C'CTT'                                                  
         MVC   EDSCSEQ,=C'089'                                                  
         MVC   EDSCRSRV,=C'00000'                                               
         MVI   EDSCLIDC,C'0'                                                    
         MVI   EDSCNMLI,C'1'                                                    
         MVC   EDILN,=Y(EDSCLNQ)                                                
*                                                                               
         BRAS  RE,PUT2TAPE                                                      
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              TASYSEQUS                                                        
*              TASYSDSECT                                                       
         PRINT OFF                                                              
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
         PRINT ON                                                               
BRNDD    DSECT                                                                  
BRNDCLI  DS    CL6                 CLIENT                                       
BRNDPRD  DS    CL6                 PRODUCT                                      
BRNDMAT  DS    CL10                MATERIAL CODE                                
BRNDCC   DS    CL10                COST CENTER                                  
BRNDLNQ  EQU   *-BRNDD                                                          
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 3                                                                
MYD      DSECT                                                                  
MYSTART  DS    0D                                                               
MYREGS   DS    16F                                                              
MYTITLE  DS    CL32                                                             
MYP      DS    CL132                                                            
MYP2     DS    CL132                                                            
MYP3     DS    CL132                                                            
MYP4     DS    CL132                                                            
MYSPACES DS    CL132                                                            
ODDAREA  DS    CL12                                                             
SVKEY    DS    CL(L'KEY)                                                        
SVAGY    DS    CL6                 SAVED AGENCY                                 
SVCLI    DS    CL6                 SAVED CLIENT                                 
SVPRD    DS    CL6                 SAVED PRODUCT                                
SVOFF    DS    CL1                 SAVED OFFICE                                 
SVMATC   DS    CL10                SAVED MATERIAL CODE                          
SVCCTR   DS    CL10                SAVED COST CENTER                            
COUNT    DS    PL4                                                              
*                                                                               
MYSORTER DS    A                                                                
DYNALLOC DS    A                                                                
CANRATE  DS    F                                                                
         SPACE 1                                                                
*                                  OPTIONS                                      
TAPEOPT  DS    CL1                 Y=GENERATE OUTPUT TAPE                       
SORTOPT  DS    CL1                                                              
TRACOPT  DS    CL1                 Y=TRACE                                      
LISTOPT  DS    CL1                 Y=LIST AND COUNT ONLY                        
         DS    CL6                 SPARE                                        
         SPACE 1                                                                
RECTYPE  DS    CL16                                                             
SAVEEL   DS    CL1                                                              
         SPACE 1                                                                
THISYMD  DS    0CL6                                                             
THISYEAR DS    CL2                                                              
THISMON  DS    CL2                                                              
THISDAY  DS    CL2                                                              
THISDET  DS    CL24                                                             
THISOV   DS    CL3                                                              
THISCAT  DS    CL7                                                              
ALLTOT   DS    F                                                                
ALLREXP  DS    F                                                                
ALLPNH   DS    F                                                                
ALLTAX   DS    F                                                                
ALLHAND  DS    F                                                                
TOTGROSS DS    PL8                                                              
*                                                                               
NUMCNTRS EQU   15                  TOTAL NUMBER OF COUNTERS                     
NUMINV   EQU   6                   NUMBER OF INVOICE AMOUNTS                    
CNTRS    DS    0F                                                               
CNTINV   DS    F                   INVOICE TOTAL                                
CNTSINV  DS    0F                  SUB-INVOICE TOTAL                            
NUMSINV  EQU   6                   NUMBER OF SUB-INVOICE AMOUNTS                
CNTTUSGE DS    F                   TALENT USAGE                                 
CNTPNH   DS    F                   P AND H                                      
CNTMUS   DS    F                   MUSICIANS AMOUNT                             
CNTHAND  DS    F                   HANDLING                                     
CNTTAX   DS    F                   PAYROLL TAX                                  
CNTGST   DS    F                   GST                                          
*                                                                               
NUMFEES  EQU   8                   NUMBER OF CHECK AMOUNTS                      
CNTRFEES DS    0F                  AMOUNT TOTALS                                
CNTSING  DS    F                   SINGERS AMOUNT                               
CNTON    DS    F                   ONCAMERA AMOUNT                              
CNTXTRA  DS    F                   EXTRAS AMOUNT                                
CNTOFF   DS    F                   OFF CAMERA (VOICE OVER) AMOUNT               
CNTMODEL DS    F                   HAND MODEL AMOUNT                            
CNTCART  DS    F                   CARTAGE AMOUNT                               
CNTWRD   DS    F                   WARDROBE AMOUNT                              
CNTMISC  DS    F                   MISCELLANEOUS AMOUNT                         
CNTRSX   EQU   *-CNTRS                                                          
AMASTD   DS    A                 A(MASTER)                                      
ALOGOC   DS    A                 A(LOGOC)                                       
ALOGO    DS    A                 A(LOGO)                                        
AREMOT   DS    A                 A(REMOTE)                                      
DLBLOCK  DS    CL(DLCBXLX)                                                      
*                                                                               
SDBLOCK  DS    CL(SOFXTNL)       SOFTDATE BLOCK                                 
OUTDATE  DS    CL12                                                             
*                                                                               
         DS    0D                                                               
TAPEIO   DS    2000C                                                            
*                                                                               
MYEND    DS    0D                                                               
         EJECT                                                                  
*              DSECT TO COVER PRINT LINES                                       
         SPACE 3                                                                
MYPRINT  DSECT                                                                  
         DS    CL34                                                             
PAGY     DS    CL6                                                              
         DS    CL1                                                              
PTOTALS  DS    0CL20                                                            
PINVOICE DS    CL7                                                              
         DS    CL1                                                              
PEST     DS    CL16                                                             
         DS    CL1                                                              
PCID     DS    CL12                                                             
         DS    CL1                                                              
PGROSS   DS    CL12                                                             
         DS    CL1                                                              
PPNH     DS    CL12                                                             
         DS    CL1                                                              
PPRMON   DS    CL5                                                              
         DS    CL22                                                             
         EJECT                                                                  
EDIDSCT  DSECT                     810 INVOICE FLAT FILE (VAR LEN)              
EDILN    DS    CL2                 LENGTH OF VARIABLE LENGTH                    
         DS    CL2                                                              
EDTRID   DS    CL6                 810 CONSTANT                                 
*                                                                               
*                                  < HEADER 'BIG' SEGMENT >                     
EDBISEG  DS    CL3                 BIG                                          
EDBISEQ  DS    CL3                 SEQUENCE (002)                               
EDBIRSRV DS    CL5                 00000                                        
EDBIDATE DS    CL8                 INVOICE DATE (CCYYMMDD)                      
EDBIINVN DS    CL6                 INVOICE NUMBER                               
         DS    CL16                                                             
         DS    CL8                 DATE                                         
         DS    CL22                PURCHASE ORDER NUMBER                        
         DS    CL30                RELEASE NUMBER                               
         DS    CL8                 CHANGE ORDER SEQ NUMBER                      
EDBITRTY DS    CL2                 TRANSACTION TYPE CODE (DI/CR)                
EDBITRPC DS    CL2                 TRANSACTION PURPOSE CODE (00)                
EDBILNQ  EQU   *-EDIDSCT                                                        
         DS    CL2                 ACTION CODE                                  
         DS    CL22                INVOICE NUMBER                               
         EJECT                                                                  
*=====================================================================          
         ORG   EDBISEG                                                          
*                                                                               
*                                  < HEADER 'REF' SEGMENT >                     
EDHRSEG  DS    CL3                 REF                                          
EDHRSEQ  DS    CL3                 SEQUENCE (005)                               
EDHRRSRV DS    CL5                 00000                                        
EDHRRIQ  DS    CL3                 REFERENCE ID QUALIFIER (23 )                 
EDHRRIDT DS    CL30                REFERENCE IDENTIFICATION (0101)              
EDHRLNQ  EQU   *-EDIDSCT                                                        
         DS    CL80                DESCRIPTION                                  
         DS    CL3                 REFERENCE ID QUALIFIER                       
         DS    CL30                REFERENCE IDENTIFICATION                     
         DS    CL3                 REFERENCE ID QUALIFIER                       
         DS    CL30                REFERENCE IDENTIFICATION                     
         DS    CL3                 REFERENCE ID QUALIFIER                       
         DS    CL30                REFERENCE IDENTIFICATION                     
         EJECT                                                                  
*=====================================================================          
         ORG   EDBISEG                                                          
*                                                                               
*                                  < HEADER 'N1' SEGMENT >                      
EDN1SEG  DS    CL3                 N1                                           
EDN1SEQ  DS    CL3                 SEQUENCE (008)                               
EDN1RSRV DS    CL5                 00000                                        
EDN1ENIC DS    CL3                 ENTITY IDENTIFIER CODE (SF)                  
         DS    CL60                NAME                                         
EDN1IDCQ DS    CL2                 IDENTIFICATION CODE QUALIFIER (ZZ)           
EDN1IDCD DS    CL80                IDENTIFICATION CODE (1003963)                
EDN1LNQ  EQU   *-EDIDSCT                                                        
         DS    CL2                 ENTITY RELATIONSHIP CODE                     
         DS    CL3                 ENTITY IDENTIFIER CODE                       
         EJECT                                                                  
*=====================================================================          
         ORG   EDBISEG                                                          
*                                                                               
*                                  < DETAIL 'IT1' SEGMENT >                     
EDI1SEG  DS    CL3                 IT1                                          
EDI1SEQ  DS    CL3                 SEQUENCE (035)                               
EDI1RSRV DS    CL5                 00000                                        
EDI1ASID DS    CL20                ASSIGNED IDENTIFICATION (1)                  
EDI1QIDC DS    CL1                 NUM OF DEC PLACES (0)                        
EDI1QTIN DS    CL10                QTY INVOICED (1)                             
EDI1UNCD DS    CL2                 UNIT/BASIS FOR MEASURE CODE (EA)             
EDI1UPDC DS    CL1                 NUM OF DEC PLACES (2)                        
EDI1UNPR DS    CL17                UNIT PRICE                                   
         DS    CL2                 BASIS OF UNIT PRICE CODE                     
EDI1PSIQ DS    CL2                 PRODUCT/SERVICE ID QUALIFIER (BC)            
EDI1PSID DS    CL48                PRODUCT/SERVICE ID                           
EDI1LNQ  EQU   *-EDIDSCT                                                        
         DS    CL2                 PRODUCT/SERVICE ID QUALIFIER                 
         DS    CL48                PRODUCT/SERVICE ID                           
         DS    CL2                 PRODUCT/SERVICE ID QUALIFIER                 
         DS    CL48                PRODUCT/SERVICE ID                           
         DS    CL2                 PRODUCT/SERVICE ID QUALIFIER                 
         DS    CL48                PRODUCT/SERVICE ID                           
         DS    CL2                 PRODUCT/SERVICE ID QUALIFIER                 
         DS    CL48                PRODUCT/SERVICE ID                           
         DS    CL2                 PRODUCT/SERVICE ID QUALIFIER                 
         DS    CL48                PRODUCT/SERVICE ID                           
         DS    CL2                 PRODUCT/SERVICE ID QUALIFIER                 
         DS    CL48                PRODUCT/SERVICE ID                           
         DS    CL2                 PRODUCT/SERVICE ID QUALIFIER                 
         DS    CL48                PRODUCT/SERVICE ID                           
         DS    CL2                 PRODUCT/SERVICE ID QUALIFIER                 
         DS    CL48                PRODUCT/SERVICE ID                           
         DS    CL2                 PRODUCT/SERVICE ID QUALIFIER                 
         DS    CL48                PRODUCT/SERVICE ID                           
         EJECT                                                                  
*=====================================================================          
         ORG   EDBISEG                                                          
*                                                                               
*                                  < DETAIL 'REF' SEGMENT >                     
EDDRSEG  DS    CL3                 REF                                          
EDDRSEQ  DS    CL3                 SEQUENCE (050)                               
EDDRRSRV DS    CL5                 00000                                        
EDDRRIQ  DS    CL3                 REFERENCE ID QUAL (DP/OQ/12/EX)              
EDDRRIDT DS    CL30                REFERENCE IDENTIFICATION                     
EDDRLNQ  EQU   *-EDIDSCT                                                        
         DS    CL80                DESCRIPTION                                  
         DS    CL3                 REFERENCE ID QUALIFIER                       
         DS    CL30                REFERENCE IDENTIFICATION                     
         DS    CL3                 REFERENCE ID QUALIFIER                       
         DS    CL30                REFERENCE IDENTIFICATION                     
         DS    CL3                 REFERENCE ID QUALIFIER                       
         DS    CL30                REFERENCE IDENTIFICATION                     
         EJECT                                                                  
*=====================================================================          
         ORG   EDBISEG                                                          
*                                                                               
*                                  < SUMMARY 'TDS' SEGMENT >                    
EDSTSEG  DS    CL3                 TDS                                          
EDSTSEQ  DS    CL3                 SEQUENCE (081)                               
EDSTRSRV DS    CL5                 00000                                        
EDSTAMDC DS    CL1                 NUM OF DEC PLACES                            
EDSTAMNT DS    CL15                AMOUNT                                       
EDSTLNQ  EQU   *-EDIDSCT                                                        
         DS    CL1                 NUM OF DEC PLACES                            
         DS    CL15                AMOUNT                                       
         DS    CL1                 NUM OF DEC PLACES                            
         DS    CL15                AMOUNT                                       
         DS    CL1                 NUM OF DEC PLACES                            
         DS    CL15                AMOUNT                                       
         EJECT                                                                  
*=====================================================================          
         ORG   EDBISEG                                                          
*                                                                               
*                                  < SUMMARY 'TXI' SEGMENT >                    
EDSXSEG  DS    CL3                 TXI                                          
EDSXSEQ  DS    CL3                 SEQUENCE (082)                               
EDSXRSRV DS    CL5                 00000                                        
EDSXTXTC DS    CL2                 TAX TYPE CODE (SU)                           
EDSXMADC DS    CL1                 NUM OF DEC PLACES (2)                        
EDSXMAMT DS    CL18                MONETARY AMOUNT                              
         DS    CL1                 NUM OF DEC PLACES                            
         DS    CL10                PERCENT                                      
         DS    CL2                 TAX JURISDICTION CODE QUAL                   
         DS    CL10                TAX JURISDICTION CODE                        
         DS    CL1                 TAX EXEMPT CODE                              
         DS    CL1                 RELATIONSHIP CODE                            
         DS    CL1                 NUM OF DEC PLACES                            
         DS    CL9                 DOLLAR BASIS FOR PERCENT                     
         DS    CL20                TAX ID NUMBER                                
EDSXASID DS    CL20                ASSIGNED IDENTIFICATION                      
EDSXLNQ  EQU   *-EDIDSCT                                                        
         EJECT                                                                  
*=====================================================================          
         ORG   EDBISEG                                                          
*                                                                               
*                                  < SUMMARY 'CTT' SEGMENT >                    
EDSCSEG  DS    CL3                 CTT                                          
EDSCSEQ  DS    CL3                 SEQUENCE (089)                               
EDSCRSRV DS    CL5                 00000                                        
EDSCLIDC DS    CL1                 NUM OF DEC PLACES                            
EDSCNMLI DS    CL6                 NUM OF LINE ITEMS                            
EDSCLNQ  EQU   *-EDIDSCT                                                        
         DS    CL1                 NUM OF DEC PLACES                            
         DS    CL10                HASH TOTAL                                   
         DS    CL1                 NUM OF DEC PLACES                            
         DS    CL10                WEIGHT                                       
         DS    CL2                 UNIT/BASIS FOR MEASURE CODE                  
         DS    CL1                 NUM OF DEC PLACES                            
         DS    CL8                 VOLUME                                       
         DS    CL2                 UNIT/BASIS FOR MEASURE CODE                  
         DS    CL80                DESCRIPTION                                  
         EJECT                                                                  
*=====================================================================          
         ORG   EDBISEG                                                          
*                                                                               
*                                  < HEADER RECORD >                            
EDHDSEQ  DS    CL11                00000000000                                  
         DS    CL24                                                             
EDHDDDS  DS    CL15                DDS                                          
EDHDPRF  DS    CL15                COKE-BILLING                                 
EDHDLNQ  EQU   *-EDIDSCT                                                        
         DS    CL9                 RECEIVED INTERCHANGE CONTROL NUM             
         DS    CL9                 RECEIVED FUNCTIONAL GRP CONTROL NUM          
         DS    CL9                 RECEIVED SET CONTROL NUM                     
         DS    CL12                VERSION ID                                   
         DS    CL2                 EDI STANDARDS RECEIVED                       
         DS    CL1                 EDI STANDARD USED                            
         DS    CL1                 RECEIVED ISA EST/PROD CODE (P/T)             
         DS    CL1                 RELATIONSHIP TEST/PROD CODE (P/T)            
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*TAREPWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDLOGOD                                                                        
*DDMASTD                                                                        
*DDREMOTED                                                                      
*DDDLCB                                                                         
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*DDTWADCONS                                                                     
*FAFACTS                                                                        
*FATIOB                                                                         
*TAREPFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSOFDATD                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPE8D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076TAREP58   07/01/10'                                      
         END                                                                    
