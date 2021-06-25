*          DATA SET TAREP28    AT LEVEL 061 AS OF 10/04/16                      
*PHASE T70328B                                                                  
*INCLUDE DLFLD                                                                  
         TITLE 'T70328 - PUBLICIS INTERFACE'                                    
T70328   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYEND-MYSTART,T70328,R7,R8                                       
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
         CLI   DOWNOPT,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,PREPD                                                         
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
         MVI   SAATOPT,C'N'                                                     
         MVI   TSTOPT,C'N'                                                      
         MVI   WAV2OPT,C'N'                                                     
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
OPT10    CLC   12(7,R4),=C'SAATCHI'  SAATCHI OPTION                             
         BNE   OPT11                                                            
         CLI   RECNUM,PX           NOT VALID OPTION FOR PUBXML                  
         BE    BADOPT                                                           
         MVI   SAATOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT11    CLC   12(4,R4),=C'TEST'   TEST OPTION FOR PUBXML                       
         BNE   OPT12                                                            
         MVI   TSTOPT,C'Y'                                                      
         B     OPTEND                                                           
         SPACE 1                                                                
OPT12    CLC   12(4,R4),=C'WAVE2'  WAVE2 FOR PUBXML                             
         BNE   OPT13                                                            
         CLI   RECNUM,PX           HAS TO BE PUBXML REPORT                      
         BNE   BADOPT                                                           
         MVI   WAV2OPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT13    DS    0H                                                               
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
         MVC   MYTITLE(15),=C'INTERFACE TAPE'                                   
         SPACE 1                                                                
         XC    SEVOICEO(SESSAMTS),SEVOICEO                                      
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         SPACE 1                                                                
PREP2    MVI   TIREAD,TLCKCDQ      SET TO READ CHECKS                           
         OI    TIFINS2N,TAINSADJ   NO ADJUSTMENTS                               
         OI    TIQFLAG2,TIQFSUB    SUBSIDIARY CHECKS ONLY                       
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
         CP    INVCOUNT,=P'0'      NOTHING TO REPORT                            
         BE    PREP5                                                            
*                                                                               
         LA    R5,TAPEIO           COMPLETE LAST RECORD                         
         USING TAPED,R5                                                         
         BAS   RE,AUDIT            ENSURE SUM OF PARTS = WHOLE                  
*                                  PUTTING TOTAL APPLIED FROM CHECKS            
*                                  INTO INTERFACE RECORD                        
         GOTO1 CONVCASH,DMCB,INVAPPLY,INAPLAMT                                  
                                                                                
         XC    INVAPPLY,INVAPPLY                                                
         BAS   RE,SORTPUT                                                       
                                                                                
         CLI   RECNUM,PX                                                        
         BNE   PREP5                                                            
         BRAS  RE,PXMLINVS         PUT TO XML <-- INVOICE                       
*                                                                               
PREP5    BAS   RE,DOREST           SORT, PRINT AND WRITE TAPE                   
         B     XIT                                                              
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   XIT                                                              
*                                                                               
IOHOOK2  BAS   RE,EXCLINV          EXCLUDE INVOICE FROM INTERFACE?              
         BE    XIT                                                              
         BAS   RE,FILLSORT         FILL A SORT RECORD                           
         B     XIT                                                              
         EJECT                                                                  
*              FILL IN SORT RECORD                                              
         SPACE 3                                                                
FILLSORT NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
*                                                                               
         L     RE,TIAREC                                                        
         USING TLCKD,RE                                                         
         CLI   0(RE),TLCKCDQ          CHECK RECORD?                             
         BNE   FSORTA                                                           
         CLC   TIINV,TLCKINV          IF TIINV DOES NOT MATCH INVOICE           
         BE    FSORTA                 IN NEW RECORD,                            
         MVC   TIINV,TLCKINV          UPDATE TIINV WITH NEW INVOICE             
         DROP  RE                                                               
*                                                                               
FSORTA   CLC   THISAGY,TIAGY                                                    
         BNE   FSORTB                                                           
         CLC   THISINV,TIINV                                                    
         BE    FILLCHEK                                                         
FSORTB   CLI   THISINV,0                                                        
         BE    FS100                                                            
         BAS   RE,AUDIT            ENSURE SUM OF PARTS = WHOLE                  
*                                  PUTTING TOTAL APPLIED FROM CHECKS            
*                                  INTO INTERFACE RECORD                        
***      GOTO1 CONVCASH,DMCB,INVAPPLY,INAPLAMT                                  
         XC    INVAPPLY,INVAPPLY                                                
         SPACE 1                                                                
         BAS   RE,SORTPUT                                                       
                                                                                
         CLI   RECNUM,PX                                                        
         BNE   FS100                                                            
         BRAS  RE,PXMLINVS         PUT TO XML <-- INVOICE                       
                                                                                
         XC    SEVOICEO(SESSAMTS),SEVOICEO                                      
*                                                                               
         SPACE 1                                                                
FS100    AP    INVCOUNT,=P'1'                                                   
         CP    INVCOUNT,TRALIMIT                                                
         BH    *+8                                                              
         BAS   RE,TRACEINV                                                      
*                                                                               
FS130    MVC   THISAGY,TIAGY                                                    
         MVC   THISINV,TIINV                                                    
         BAS   RE,CLEARTAP                                                      
         L     R4,TIAMAIN                                                       
         USING TLIND,R4                                                         
         MVC   INAGY,TLINAGY                                                    
         MVC   WORK(6),TLININV                                                  
         DROP  R4                                                               
         XC    WORK(6),=6X'FF'                                                  
         GOTO1 TINVCON,DMCB,WORK,ININV,DATCON                                   
*                                                                               
         OC    SVINDATE,SVINDATE                                                
         BZ    FS160                                                            
         GOTO1 DATCON,DMCB,(1,SVINDATE),(20,ININVDT)                            
*                                                                               
FS160    L     R6,TIAMAIN                                                       
         BAS   RE,SETCAN           SET CANADIAN CONVERSION RATE                 
         BAS   RE,FIXCAN           ADJUST TO US$ IF NECESSARY                   
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
FS200    BAS   RE,NEXTEL                                                        
         BNE   FSEND                                                            
         CLI   0(R6),TACOELQ                                                    
         BE    FSCO                                                             
         CLI   0(R6),TABDELQ                                                    
         BE    FSBD                                                             
         CLI   0(R6),TANUELQ                                                    
         BE    FSNU                                                             
         CLI   0(R6),TAFNELQ                                                    
         BE    FSFN                                                             
         CLI   0(R6),TAPDELQ                                                    
         BE    FSPD                                                             
         B     FS200                                                            
         SPACE 1                                                                
         USING TACOD,R6                                                         
FSCO     MVC   INCID,TACOCID                                                    
         MVI   GENMTRS,C'N'                                                     
         CLC   INAGY,=C'0162'      GENERAL MOTORS, AGY=0162 / 0163              
         BE    *+10                                                             
         CLC   INAGY,=C'0163'                                                   
         BNE   FS200                                                            
*        CLC   TACOCLG,=CL6'GM'      AND CHECK CLIENT GROUP TOO                 
*        BNE   FS200                                                            
         MVI   GENMTRS,C'Y'                                                     
         B     FS200                                                            
         SPACE 1                                                                
         USING TABDD,R6                                                         
FSBD     L     R0,TABDTAX                                                       
         A     R0,TABDHND                                                       
         A     R0,TABDFICR                                                      
         A     R0,TABDGST                                                       
         A     R0,TABDPST                                                       
         A     R0,TABDACOM                                                      
         A     R0,TABDSIGN                                                      
         ST    R0,ALLTAX                                                        
         S     R0,TABDHND                                                       
         ST    R0,ALLTAXX          FOR PUBXML                                   
         L     R0,TABDHND                                                       
         A     R0,TABDHNDC                                                      
         ST    R0,ALLHANDX                                                      
                                                                                
         TM    TABDSTAT,TABDSCNW   TEST T&H IN CAN$                             
         BZ    FSBD10                                                           
         LA    R1,ALLTAX                                                        
         BAS   RE,FIXCAN6                                                       
         LA    R1,ALLTAXX                                                       
         BAS   RE,FIXCAN6                                                       
         L     R0,ALLTAX                                                        
         LA    R1,TABDHNDC                                                      
         BAS   RE,FIXCAN6                                                       
         LA    R1,ALLHANDX                                                      
         BAS   RE,FIXCAN6                                                       
FSBD10   GOTO1 CONVCASH,DMCB,ALLTAX,INTAX                                       
         MVC   ALLHAND,TABDHNDC                                                 
         GOTO1 CONVCASH,DMCB,ALLHAND,INHAND                                     
                                                                                
         MVC   ALLTOT,TABDTOT                                                   
         GOTO1 CONVCASH,DMCB,ALLTOT,INTOT                                       
         MVC   ALLCSF,TABDCSF                                                   
         GOTO1 CONVCASH,DMCB,ALLCSF,INCSF                                       
         B     FS200                                                            
         SPACE 1                                                                
         USING TANUD,R6                                                         
FSNU     ZIC   R1,TANULEN                                                       
         MVC   WORK,MYSPACES                                                    
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),TANUMBER                                                 
         CLI   TANUTYPE,TANUTAUT                                                
         BE    FSAUTH                                                           
         CLI   TANUTYPE,TANUTEST                                                
         BE    FSEST                                                            
         B     FS200                                                            
         SPACE 1                                                                
FSAUTH   MVC   INPO,WORK                                                        
         B     FS200                                                            
         SPACE 1                                                                
FSEST    MVC   INEST,WORK         ESTIMATE NUMBER                               
         CLI   SAATOPT,C'Y'       IF OPTION SAATCHI,                            
         BNE   FS200                                                            
         MVC   MYWORK(3),INEST    BUS. UNIT NOW FIRST 3 CHAR OF CLI             
         B     FS200                                                            
         SPACE 1                                                                
         USING TAFND,R6                                                         
FSFN     CLI   TAFNTYPE,TAFNTTTL   COMMERCIAL TITLE                             
         BNE   FS200                                                            
         ZIC   R1,TAFNLEN                                                       
         MVC   WORK,MYSPACES                                                    
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   INTITLE(0),TAFNNAME                                              
         B     FS200                                                            
         SPACE 1                                                                
         USING TAPDD,R6                                                         
FSPD     MVC   INBRAND,TAPDPRD                                                  
         OC    INBRAND,MYSPACES                                                 
         MVC   INUSECD,TAPDUSE                                                  
         MVC   INCLI,TAPDCLI                                                    
         OC    INCLI,MYSPACES                                                   
         MVC   INPRD,TAPDPRD                                                    
         OC    INPRD,MYSPACES                                                   
         MVC   INESTPER,=C'00'                                                  
         CLI   TAPDESPD,0                                                       
         BE    FSPD1                                                            
         CLI   TAPDESPD,C' '                                                    
         BE    FSPD1                                                            
         EDIT  (1,TAPDESPD),(2,INESTPER),FILL=0                                 
         SPACE 1                                                                
FSPD1    GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         TM    TGUSSTAT,SESSION    IF NOT SESSION                               
         BO    FSPD10                                                           
         CLC   TIEMP(2),=C'PG'     OR NOT EMPLOYER P&G                          
         BNE   FSPD10                                                           
         BAS   RE,CHKADJ           IF ADJUSTMENT CODE NECESSARY                 
         BNE   *+8                                                              
         MVI   INPRMON,C'A'        SET ADJUSTMENT                               
         SPACE 1                                                                
FSPD10   MVC   SVCYCSTR,TAPDCYCS                                                
         MVC   SVCYCEND,TAPDCYCE                                                
         GOTO1 CONVDATE,DMCB,TAPDCYCS,INCYCS                                    
         GOTO1 CONVDATE,DMCB,TAPDCYCE,INCYCE                                    
                                                                                
         GOTO1 =A(FNDMTRL),DMCB,TAPDUSE                                         
                                                                                
         MVC   INUSE,TGUSNAME                                                   
         LA    R2,THISDET                                                       
         MVC   THISDET,MYSPACES                                                 
         BAS   RE,EDITDET                                                       
         MVC   INUSEDET,THISDET                                                 
*                                                                               
         L     R1,TAPDPNH          P&H AND I&R                                  
         A     R1,TAPDINR                                                       
         ST    R1,ALLPNH                                                        
                                                                                
         GOTO1 CONVCASH,DMCB,ALLPNH,INPNH                                       
         MVC   INAPLCOD,TAPDACDE                                                
         OI    INAPLCOD,X'F0'                                                   
         MVC   ALLREXP,TAPDREXP                                                 
         B     FS200                                                            
         SPACE 1                                                                
FSEND    MVI   INREUSE,C'P'                                                     
         TM    TGUSSTAT,SESSION                                                 
         BO    *+8                                                              
         MVI   INREUSE,C'R'                                                     
         LA    R2,INENTRY                                                       
         DROP  R5                                                               
         USING INENTRY,R2                                                       
         LA    R0,15               INITIALIZE FOR 15 ENTRIES                    
         SPACE 1                                                                
FSEND2   MVC   INCAT,MYSPACES                                                   
         MVC   INCOUNT,EZEROS                                                   
         MVC   INOVPCT,EZEROS                                                   
         MVC   INPEOAMT,EZEROS                                                  
         LA    R2,L'INENTRY(R2)                                                 
         BCT   R0,FSEND2                                                        
         B     FILLCHEK                                                         
         DROP  R2                                                               
         SPACE 1                                                                
THISAGY  DC    CL6'      '                                                      
THISINV  DC    XL6'00'                                                          
EZEROS   DC    15C'0'                                                           
         EJECT                                                                  
*              EXCLUDE INVOICE FROM INTERFACE?                                  
         SPACE 3                                                                
EXCLINV  NTR1                                                                   
         L     R6,TIAMAIN                                                       
         MVI   ELCODE,TAINELQ      CHECK INVOICE FORCED NOT INTERFACE           
         BAS   RE,GETEL                                                         
         BNE   EXCLINV5                                                         
         USING TAIND,R6                                                         
         MVC   SVINDATE,TAINBDTE   SAVE INVOICE DATE                            
         TM    TAINSTA3,TAINSNI                                                 
         BO    YES                                                              
*                                                                               
EXCLINV5 L     R6,TIAMAIN          OR DURING PAY                                
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   EXCLINV7                                                         
         USING TAPDD,R6                                                         
         TM    TAPDOPT4,TAPDONOI                                                
         BO    YES                                                              
*                                                                               
EXCLINV7 CLI   SAATOPT,C'Y'       IF OPTION SAATCHI,                            
         BNE   NO                                                               
         L     R6,TIAMAIN         CHECK CLIENT SET TO NOT INTERFACE             
         MVI   ELCODE,TANUELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
EXCLINV8 BAS   RE,NEXTEL                                                        
         BNE   NO                                                               
         USING TANUD,R6                                                         
         CLI   TANUTYPE,TANUTEST  NEED CLIENT FROM ESTIMATE NUMBER              
         BNE   EXCLINV8                                                         
*                                                                               
         MVC   MYWORK,MYSPACES                                                  
         MVC   MYWORK(3),TANUMBER+3                                             
*                                                                               
         LA    RF,BUTABLE          ONLY USE THESE BUSINESS UNITS                
EXCLIN8L CLI   0(RF),X'FF'         IN THE TABLE                                 
         BE    YES                 EXCLUDE ALL OTHERS                           
         CLC   0(3,RF),TANUMBER                                                 
         BE    EXCLINV9                                                         
         LA    RF,3(RF)                                                         
         B     EXCLIN8L                                                         
*                                                                               
EXCLINV9 BRAS  RE,CLINOINT         CHECK IF CLIENT IS NOT INTERFACING           
         BE    YES                                                              
*                                                                               
         B     NO                                                               
*                                                                               
BUTABLE  DS    0H                                                               
         DC    C'402'                                                           
         DC    C'410'                                                           
         DC    C'412'                                                           
         DC    C'413'                                                           
         DC    C'414'                                                           
         DC    C'415'                                                           
         DC    C'416'                                                           
         DC    C'420'                                                           
         DC    C'421'                                                           
         DC    C'422'                                                           
         DC    C'424'                                                           
         DC    C'425'                                                           
         DC    C'428'                                                           
         DC    C'429'                                                           
         DC    C'431'                                                           
         DC    C'432'                                                           
         DC    C'441'                                                           
         DC    C'442'                                                           
         DC    C'443'                                                           
         DC    C'444'                                                           
         DC    C'450'                                                           
         DC    C'456'                                                           
         DC    C'457'                                                           
         DC    C'464'                                                           
         DC    C'065'                                                           
         DC    C'088'                                                           
         DC    X'FF'                                                            
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE CHECKS ADJUSTMENT CODE NEEDED                            
*              YES - IF PERIOD MONTH IS LESS THAN CURRENT MONTH                 
*                                                                               
         USING TAPED,R5                                                         
CHKADJ   NTR1                                                                   
         CLC   INESTPER,=C'00'     IF NO PERIOD MONTH                           
         BE    YES                 ALWAYS ADJUST                                
*                                                                               
         LA    R1,ADJMONTB         R1=A(2 YEAR MONTH TABLE)                     
CHKADJ5  CLC   INESTPER,0(R1)      FIND PERIOD IN TABLE                         
         BE    *+12                                                             
         BAS   RE,BUMPR1                                                        
         B     CHKADJ5                                                          
*                                                                               
         LA    R2,1                R2=DIFFERENCE IN MONTHS                      
         LA    R1,2(R1)                                                         
CHKADJ8  CLC   THISMON,0(R1)       LOOP FOR DIFFERENCE TO CURRENT MONTH         
         BE    CHKADJ10                                                         
         LA    R2,1(R2)            BUMP MONTH COUNTER                           
         BAS   RE,BUMPR1                                                        
         B     CHKADJ8                                                          
*                                                                               
CHKADJ10 CH    R2,=H'6'            IF DIFFERENCE 5 MONTHS OR LESS               
         BL    YES                 SET ADJUSTMENT CODE NECESSARY                
         B     NO                                                               
         SPACE 2                                                                
BUMPR1   DS    0H                                                               
         LA    R1,2(R1)                                                         
         CLI   0(R1),X'FF'         TEST END OF TABLE                            
         BNER  RE                                                               
         DC    H'0'                                                             
         DROP  R5                                                               
         EJECT                                                                  
*              SET CANADIAN CONVERSION RATE FOR CAN$ INVOICES                   
         SPACE 3                                                                
SETCAN   NTR1                                                                   
         XC    CANRATE,CANRATE                                                  
         LR    R5,R6                                                            
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAPDD,R6                                                         
         TM    TAPDSTAT,TAPDSCAN   CHECK THIS IS CANADIAN                       
         BNO   XIT                                                              
         LR    R6,R5                                                            
         MVI   ELCODE,TABDELQ      FIND RATE FROM INVOICE                       
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TABDD,R6                                                         
         OC    TABDCCVT,TABDCCVT                                                
         BZ    XIT                                                              
         ZAP   DUB,TABDCCVT                                                     
         CVB   R1,DUB                                                           
         ST    R1,CANRATE                                                       
         B     XIT                                                              
         EJECT                                                                  
*              FIX CANADIAN DOLLARS                                             
         SPACE 3                                                                
FIXCAN   NTR1                                                                   
         OC    CANRATE,CANRATE     DON'T BOTHER IF RATE NOT DEFINED             
         BZ    XIT                                                              
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAPDD,R6                                                         
         LA    R1,TAPDAMTS                                                      
         LA    R0,TAPDAMTL/L'TAPDAMTS                                           
FIXCAN4  BAS   RE,FIXCAN6          CONVERT THE TAPDS                            
         LA    R1,4(R1)                                                         
         BCT   R0,FIXCAN4                                                       
         B     XIT                                                              
         SPACE 3                                                                
FIXCAN6  NTR1                      ADJUST FULLWORD BY CANRATE                   
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
*              ROUTINE ENSURES SUM OF PARTS EQUALS WHOLE                        
         SPACE 1                                                                
AUDIT    NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         LA    R2,INENTRY                                                       
         USING INENTRY,R2                                                       
         SPACE 1                                                                
         ZAP   TGDUB,=P'0'         CLEAR DETAIL TOTAL                           
         SPACE 1                                                                
         LA    R0,15               ADD UP PEOPLE AMOUNTS                        
AUD5     GOTO1 NEWPACK,DMCB,INPEOAMT                                            
         AP    TGDUB,DUB                                                        
         LA    R2,L'INENTRY(R2)                                                 
         BCT   R0,AUD5                                                          
         DROP  R2                                                               
         SPACE 1                                                                
         L     R1,ALLTOT           INVOICE TOTAL                                
         S     R1,ALLREXP          - REIMB. EXPENSES                            
         S     R1,ALLPNH           - P&H/I&R                                    
         S     R1,ALLTAX           - TAX/GST                                    
         S     R1,ALLHAND          - HANDLING                                   
         S     R1,ALLCSF           - CSF                                        
         S     R1,INVAPPLY         - APPLIED CREDS (ADDED BACK TO CHKS)         
                                                                                
         CVB   R0,TGDUB            COMPARE AGAINST PAYMENTS                     
         ST    R0,ALLPAY                                                        
         MVC   ALLAPPLY,INVAPPLY                                                
                                                                                
         SR    R1,R0                - DETAIL TOTAL                              
         BZ    AUDX                                                             
         OC    CANRATE,CANRATE     IF THIS ISN'T CAN$ INVOICE                   
         BNZ   *+6                                                              
         DC    H'0'                DIE IF SUM OF PARTS NEQ WHOLE                
                                                                                
         LR    RF,R1                                                            
                                                                                
         A     R1,ALLTAX           ELSE ADD DIFF. BACK TO TAX AMOUNT            
         ST    R1,ALLTAX                                                        
         A     RF,ALLHANDX         FOR PUBXML, ADD BACK TO HANDLING             
         ST    RF,ALLHANDX                                                      
         GOTO1 CONVCASH,DMCB,ALLTAX,INTAX                                       
                                                                                
AUDX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINES FOR INDIVIDUAL RECORDS (CHECKS)                         
         SPACE 3                                                                
FILLCHEK DS    0H                                                               
         CP    INVCOUNT,TRALIMIT                                                
         BH    *+8                                                              
         BAS   RE,TRACECHK                                                      
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         L     R4,TIAREC                                                        
         USING TLCKD,R4                                                         
         GOTO1 CATVAL,DMCB,TLCKCAT                                              
         MVC   THISCAM,=C'   '                                                  
                                                                                
         CLC   TLCKSSN,=C'000000055'                                            
         BE    FILLC5              THIS IS ACTRA CHECK - NOT A PERSON!          
         CLC   TLCKSSN,=C'000000066'                                            
         BE    FILLC5              THIS IS ACTRA CHECK - NOT A PERSON!          
         CLC   TLCKSSN,=C'000003755'                                            
         BE    FILLC5              THIS IS ACTRA CHECK - NOT A PERSON!          
         CLC   TLCKSSN,=C'000003876'                                            
         BE    FILLC5              THIS IS ACTRA CHECK - NOT A PERSON!          
         CLC   TLCKSSN,=C'000007106'                                            
         BE    FILLC5              THIS IS ACTRA CHECK - NOT A PERSON!          
         CLC   TLCKSSN,=C'000008213'                                            
         BE    FILLC5              THIS IS ACTRA CHECK - NOT A PERSON!          
         CLC   TLCKSSN,=C'000003855'                                            
         BE    FILLC5              THIS IS ACTRA CHECK - NOT A PERSON!          
         CLC   TLCKSSN,=C'000006816'                                            
         BE    FILLC5              THIS IS ACTRA CHECK - NOT A PERSON!          
         CLC   TLCKSSN,=C'237385560'                                            
         BE    FILLC6              THIS IS AFM CHECK - NOT A PERSON!            
         CLC   TLCKSSN,=C'999999999'                                            
         BE    FILLC6              THIS IS AFM CHECK - NOT A PERSON!            
         CLC   TLCKSSN,=C'210390905'                                            
         BE    FILLC6              LOCAL 661-708 H&W CHECK                      
         CLC   TLCKSSN,=C'131801294'                                            
         BE    FILLC6              THIS IS AFM CHECK - NOT A PERSON!            
         B     FILLC10                                                          
FILLC5   CLC   TLCKSORT,=6X'FF'                                                 
         BE    XIT                 IGNORE IF GENERATED BY PAY                   
                                                                                
FILLC6   LR    R6,R4                                                            
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAPDD,R6                                                         
         L     R1,ALLPNH                                                        
         A     R1,TAPDPAYC         ELSE ADD PAYMENT TO P&H TOTAL                
         ST    R1,ALLPNH           (TO KEEP IN BALANCE)                         
         GOTO1 CONVCASH,DMCB,ALLPNH,INPNH                                       
         B     XIT                 AND GET OUT                                  
         SPACE 1                                                                
FILLC10  MVC   THISOV,EZEROS                                                    
         L     R6,TIAREC                                                        
         BAS   RE,FIXCAN           ADJUST TO US$ IF NECESSARY                   
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TACAD,R6                                                         
*                                                                               
         MVC   THISCAM,TACAONOF                                                 
         MVC   THISUN,TACAUN       SAVE UNION CODE                              
         CLI   SAATOPT,C'Y'        IF OPTION SAATCHI,                           
         BNE   FILLC20                                                          
         BRAS  RE,GETSUB           GET SUB CATEGORY                             
         B     LCAT6                                                            
*                                  LOOK UP CATEGORY TO THISCAT                  
FILLC20  MVC   THISCAT,MYSPACES                                                 
         MVC   THISCAT(3),TACAONOF      DEFAULT TO 'ON' OR 'OFF'                
         LA    R1,CATLIST                                                       
         SPACE 1                                                                
LCAT2    CLI   0(R1),X'FF'                                                      
         BE    LCAT6                                                            
         CLC   TLCKCAT(2),0(R1)                                                 
         BE    LCAT4                                                            
         LA    R1,10(R1)                                                        
         B     LCAT2                                                            
         DROP  R4                                                               
         SPACE 1                                                                
LCAT4    MVC   THISCAT,3(R1)                                                    
         SPACE 1                                                                
LCAT6    LR    R6,R4                                                            
         MVI   ELCODE,TAOPELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   FC2                                                              
         USING TAOPD,R6                                                         
         MVI   BYTE,0              FLAG FOR NOT % SCALE                         
         LA    RF,THISOV                                                        
         TM    TAOPPCT,X'80'       IS THIS A PERCENT SCALE?                     
         BNO   LCAT8                                                            
         NI    TAOPPCT,X'FF'-X'80' YES                                          
         MVI   0(RF),C'%'                                                       
         LA    RF,1(RF)                                                         
         MVI   BYTE,1              SET FLAG FOR % SCALE                         
LCAT8    MVC   DUB,TAOPPCT                                                      
         L     R1,DUB                                                           
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CLI   BYTE,0                                                           
         BNE   LCAT10                                                           
         EDIT  (R1),(3,(RF)),FILL=0                                             
         B     FC2                                                              
LCAT10   EDIT  (R1),(2,(RF)),FILL=0                                             
         SPACE 1                                                                
FC2      LR    R6,R4                                                            
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAPDD,R6                                                         
         LA    R2,INENTRY                                                       
         DROP  R5                                                               
         USING INENTRY,R2                                                       
         LA    R0,15                                                            
         SPACE 1                                                                
FC4      CLC   INCAT,MYSPACES                                                   
         BE    FC8                                                              
         CLC   INCAT,THISCAT                                                    
         BNE   FC6                                                              
         CLC   INOVPCT,THISOV                                                   
         BE    FC8                                                              
         SPACE 1                                                                
FC6      LA    R2,L'INENTRY(R2)                                                 
         BCT   R0,FC4                                                           
         B     XIT                                                              
         SPACE 1                                                                
FC8      MVC   INCAT,THISCAT                                                    
         OC    INCAT,MYSPACES                                                   
         MVC   INOVPCT,THISOV                                                   
         PACK  DUB,INCOUNT                                                      
         AP    DUB,=P'1'                                                        
         EDIT  (P8,DUB),(2,INCOUNT),FILL=0                                      
*                                                                               
         GOTO1 NEWPACK,DMCB,INPEOAMT                                            
         CVB   R1,DUB                                                           
                                                                                
         MVI   BYTE,C'N'             USE APPLIED AMOUNTS                        
         L     RF,TAPDPAYI                                                      
         A     RF,TAPDPAYC                                                      
                                                                                
         IF (LTR,RF,RF,Z)            ZERO DOLLAR?                               
           L     RE,TAPDGRS                                                     
           A     RE,TAPDAPPL                                                    
           A     RE,TAPDGUAR                                                    
           IF    (LTR,RE,RE,Z)                                                  
             MVI   BYTE,C'Y'         SKIP APPLIED AMOUNTS                       
           ENDIF                                                                
         ENDIF                                                                  
                                                                                
         IF (TM,TAPDOPT3,TAPDORET,Z)                                            
           IF (CLI,BYTE,EQ,C'N')     USE APPLIED AMOUNTS?                       
             S   RF,TAPDAPPL         ADD BACK APPLIED (10/23/91)                
             S   RF,TAPDGUAR         AND GUARANTEES   (01/22/92)                
           ENDIF                                                                
         ENDIF                                                                  
         ST    RF,WORK                                                          
                                                                                
         AR    R1,RF                                                            
         ST    R1,DUB                                                           
                                                                                
         L     RF,WORK                                                          
         GOTO1 =A(SESSBUCK),DMCB,(RF)                                           
                                                                                
FC9      GOTO1 CONVCASH,DMCB,DUB,INPEOAMT                                       
         IF (TM,TAPDOPT3,TAPDORET,Z)                                            
           IF (CLI,BYTE,EQ,C'N')     USE APPLIED AMOUNTS?                       
             L   R1,TAPDAPPL         ADD TO APPLIED FOR INVOICE                 
             A   R1,TAPDGUAR                                                    
             A   R1,INVAPPLY                                                    
             ST  R1,INVAPPLY                                                    
           ENDIF                                                                
         ENDIF                                                                  
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 1                                                                
CATLIST  DS    0H                                                               
         DC    CL3'G3?',CL7'SINGERS'                                            
         DC    CL3'G6?',CL7'SINGERS'                                            
         DC    CL3'G9?',CL7'SINGERS'                                            
         DC    CL3'ANN',CL7'ANN    '                                            
         DC    CL3'D  ',CL7'DUO    '                                            
         DC    CL3'S ?',CL7'SOLO   '                                            
         DC    CL3'SM?',CL7'SOLO   '                                            
         DC    CL3'SS?',CL7'SOLO   '                                            
         DC    X'FF'                                                            
         EJECT                                                                  
*              NOW HANDLE THE OUTPUT                                            
         SPACE 3                                                                
DOREST   NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         ZAP   TOTGROSS,=P'0'                                                   
         DROP  R5                                                               
         CP    OUTCOUNT,=P'0'                                                   
         BE    DOREST3                                                          
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         SPACE 1                                                                
DOREST2  GOTO1 MYSORTER,DMCB,=C'GET'                                            
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BNZ   DOREST4                                                          
         BAS   RE,SPLAT                                                         
DOREST3  LA    R2,MYP                                                           
         USING MYPRINT,R2                                                       
         MVC   PTOTALS(13),=C'*** TOTAL ***'                                    
         EDIT  (P8,TOTGROSS),(12,PGROSS),2,MINUS=YES                            
         BAS   RE,SPLAT                                                         
         CLI   SAATOPT,C'Y'    IF OPTION SAATCHI,                               
         BNE   XIT                                                              
         CLI   BDCATFLG,C'Y'   ANY BAD CATEGORY CODES?                          
         BNE   XIT                                                              
         MVC   MYP(33),=C'*** ERROR - BAD CATEGORY CODE ***'                    
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
*                                                                               
DOREST4  MOVE  (TAPEIO,646),0(R2)                                               
         CLI   RECNUM,PX                                                        
         BE    DOREST9                                                          
         BAS   RE,PUTTAPE          WRITE OUT THIS TAPE RECORD                   
                                                                                
DOREST9  BAS   RE,DOREPORT               AND PRINT REPORT                       
         B     DOREST2                                                          
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
DOREPORT NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         LA    R2,MYP                                                           
         USING MYPRINT,R2                                                       
         MVC   PAGY+1(4),INAGY                                                  
         MVC   PINVOICE(6),ININV                                                
         MVC   PEST,INEST                                                       
         MVC   PCID(10),INCID                                                   
         GOTO1 NEWPACK,DMCB,INTOT                                               
         AP    TOTGROSS,DUB                                                     
         CVB   R1,DUB                                                           
         EDIT  (R1),(12,PGROSS),2,MINUS=YES                                     
         GOTO1 NEWPACK,DMCB,INPNH                                               
         CVB   R1,DUB                                                           
         EDIT  (R1),(12,PPNH),2,MINUS=YES                                       
         MVC   PPRMON+2(1),INPRMON                                              
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
*              ROUTINE TO GET FROM DATASET AND PRINT SECOND REPORT              
*                                                                               
PREPD    NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         BAS   RE,NEWPRTQ           SET NEW PRINT QUEUE REPORT                  
         GOTO1 REQTWA,DMCB,(3,ATWA),,VPRINT,(C'B',ABOX)                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R5)                                                  
*                                                                               
         L     R2,AMASTD            DO NOT PRINT LOGOS                          
         USING MASTD,R2                                                         
         NI    MCPRTIND,X'FF'-MCPRTINL                                          
         DROP  R2                                                               
*                                                                               
         L     R2,=A(TADOWN)                                                    
         OPEN  ((2),INPUT)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,DLBLOCK                                                       
         USING DLCBD,R3                                                         
         BAS   RE,INITDWN                                                       
*                                                                               
PREPD2   GET   (R2),TAPEIO         GET REC FROM TEMP DATASET                    
         LA    R4,TAPEIO                                                        
         LA    R0,12               RECORD IS 483 BYTES LONG (40X12+3)           
*                                                                               
PREPD3   MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
*                                                                               
         MVI   DLCBLEN,40                                                       
         MVC   DLCBFLD(40),0(R4)    PASS DATA                                   
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
         LA    R4,40(R4)           BUMP TO NEXT TAPE FIELD                      
         BCT   R0,PREPD3                                                        
*                                                                               
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVI   DLCBLEN,3                                                        
         MVC   DLCBFLD(3),0(R4)   PASS DATA                                     
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     PREPD2                                                           
*                                                                               
NOMORE   CLOSE ((2))               CLOSE THE DATASET                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   DOWNOPT,C'Y'                                                     
         BE    NOMORE2                                                          
         L     R1,ALOGO            NO-OP SO DON'T END AGAIN                     
         MVC   0(2,R1),=X'07FE'                                                 
         B     PREPDX                                                           
NOMORE2  MVI   DLCBACT,DLCBEOR                                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
PREPDX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
*                                                                               
NEWPRTQ  NTR1                                                                   
         XC    SPECS,SPECS         CLEAR SPECS                                  
         XC    HEADHOOK,HEADHOOK   AND HEADLINE HOOK                            
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R5)                                                  
*                                                                               
         L     R2,ALOGOC                                                        
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 ALOGO,DMCB,(R2)     END REPORT LOGOS                             
*                                                                               
         TM    WHEN,X'20'          SOON?                                        
         BO    NEWP20                                                           
         GOTO1 VPRINT,DMCB,=C'CLOSE'                                            
*                                                                               
NEWP20   L     R2,ABOX                                                          
         USING BOXD,R2                                                          
         LA    RE,132                                                           
         ST    RE,BOXWIDTH         SET LENGTH OF PRINT LINE                     
*                                                                               
         L     RF,AMASTD                                                        
         USING MASTD,RF                                                         
         L     R2,AREMOT                                                        
         USING REMOTED,R2                                                       
*                                                                               
         TM    WHEN,X'20'          SOON?                                        
         BZ    NEWP30                                                           
         XC    MCREMPQK,MCREMPQK                                                
         B     NEWP40                                                           
*                                                                               
NEWP30   MVC   REMOTABF,MCVPQBUF                                                
         MVC   REMOTADM,MCVDMGR                                                 
         MVC   REMOTAOP,MCVPQOPN                                                
         MVC   REMOTDST,MCDESTID                                                
         XC    MCALTREF,MCALTREF                                                
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTCLS,C'Q'                                                    
         MVC   REMOTJID,=C'TDM'                                                 
NEWP40   MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(4),=C'DMBB'                                             
         MVC   REMOTFRM(4),=C'DATA'                                             
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
INITDWN  NTR1                                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
*                                                                               
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,SPLATDWN         A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     PREPDX                                                           
         EJECT                                                                  
*              USER SUPPLIED PRINT ROUTINE - DOWNLOAD                           
         SPACE 1                                                                
SPLATDWN NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R5)                                                  
         MVI   LINE,1              PREVENT PAGE BREAK                           
         B     PREPDX                                                           
         DROP  R5                                                               
         SPACE 2                                                                
*                                                                               
*              UTILITIES                                                        
         SPACE 3                                                                
CLEARREC NTR1                                                                   
         MVC   TAPEIO+20(80),MYSPACES                                           
         LA    R2,TAPEIO+100                                                    
         LA    R0,6                                                             
         B     CLEARTP2                                                         
         SPACE 1                                                                
CLEARTAP NTR1                                                                   
         LA    R2,TAPEIO                                                        
         LA    R0,5                                                             
         SPACE 1                                                                
CLEARTP2 MVC   0(100,R2),MYSPACES                                               
         LA    R2,100(R2)                                                       
         BCT   R0,CLEARTP2                                                      
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         MVC   INTAX,EZEROS                                                     
         MVC   INHAND,EZEROS                                                    
         MVC   INAPLAMT,EZEROS                                                  
         MVC   INTOT,EZEROS                                                     
         MVC   INPNH,EZEROS                                                     
         MVI   INAPLCOD,C' '                                                    
         B     XIT                                                              
         SPACE 1                                                                
CONVDATE NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVC   0(6,R3),=C'000000'                                               
         OC    0(3,R2),0(R2)                                                    
         BZ    XIT                                                              
         GOTO1 DATCON,DMCB,(1,0(R2)),(X'20',0(R3))                              
         B     XIT                                                              
         SPACE 1                                                                
CONVCASH NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         L     R4,0(R2)                                                         
         EDIT  (R4),(15,0(R3)),FILL=0                                           
         LTR   R4,R4                                                            
         BNM   XIT                                                              
         MVI   0(R3),C'-'          NEGATIVE                                     
         B     XIT                                                              
*                                                                               
NEWPACK  NTR1                                                                   
         L     R4,0(R1)                                                         
         MVC   WORK(L'INPEOAMT),0(R4)                                           
*                                                                               
         NI    WORK+L'INPEOAMT-1,X'CF'                                          
         CLI   WORK,C'-'           NEGATIVE?                                    
         BNE   NWPACK5                                                          
         MVI   WORK,C'0'                                                        
         OI    WORK+L'INPEOAMT-1,X'D0'                                          
NWPACK5  PACK  DUB,WORK(L'INPEOAMT)                                             
         B     XIT                                                              
*                                                                               
CONVMAJ  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         LA    R4,MAJTAB                                                        
         SPACE 1                                                                
CONVMAJ2 MVC   0(1,R3),1(R4)                                                    
         CLC   0(1,R2),0(R4)                                                    
         BE    XIT                                                              
         CLI   0(R2),X'FF'                                                      
         BE    XIT                                                              
         LA    R4,2(R4)                                                         
         B     CONVMAJ2                                                         
         SPACE 1                                                                
MAJTAB   DS    0H                                                               
         DC    X'00',C'0'          NONE                                         
         DC    X'80',C'1'          NY                                           
         DC    X'20',C'2'          CHI                                          
         DC    X'A0',C'3'          NY + CHI                                     
         DC    X'40',C'4'          LA                                           
         DC    X'C0',C'5'          NY + LA                                      
         DC    X'60',C'6'          CHI + LA                                     
         DC    X'E0',C'7'          ALL                                          
         DC    X'FF',C'0'          BOO BOO                                      
         EJECT                                                                  
*              EDIT DETAILS OF PAYMENT                                          
         SPACE 3                                                                
EDITDET  NTR1                                                                   
*                                  R6=A(PAYMENT DETAIL ELEMENT)                 
*                                  R2=A(OUTPUT AREA)                            
         USING TAPDD,R6                                                         
         OC    TAPDSTUS,TAPDSTUS   *** FUDGE                                    
         BNZ   INPDU10                                                          
         OC    TAPDUNIT(3),TAPDUNIT                                             
         BNZ   INPDU20                                                          
         B     XIT                                                              
         SPACE 1                                                                
INPDU10  CLC   TAPDUSES,=H'1'                                                   
         BNE   INPDU12                                                          
         MVC   0(3,R2),=C'USE'                                                  
         EDIT  (2,TAPDSTUS),(4,4(R2)),ALIGN=LEFT                                
         B     XIT                                                              
         SPACE 1                                                                
INPDU12  MVC   0(4,R2),=C'USES'                                                 
         LA    R2,5(R2)                                                         
         LH    R3,TAPDSTUS                                                      
         EDIT  (R3),(4,0(R2)),ALIGN=LEFT                                        
         AR    R2,R0                                                            
         MVI   0(R2),C'-'                                                       
         AH    R3,TAPDUSES                                                      
         BCTR  R3,0                                                             
         EDIT  (R3),(4,1(R2)),ALIGN=LEFT                                        
         B     XIT                                                              
         SPACE 1                                                                
INPDU20  GOTO1 MAJVAL,DMCB,(X'80',TAPDMAJ)                                      
         MVC   0(L'TGMACHAR,R2),TGMACHAR                                        
         LA    R2,L'TGMACHAR-1(R2)                                              
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         OC    TAPDUNIT,TAPDUNIT                                                
         BZ    XIT                                                              
         EDIT  (2,TAPDUNIT),(4,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                   
         AR    R2,R0                                                            
         MVC   1(5,R2),=C'UNITS'                                                
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINES TO ENSURE SUB RECORDS AROUND                            
         SPACE 3                                                                
         SPACE 1                                                                
NEEDCO   NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
         XC    NEEDKEY,NEEDKEY     ENSURE COMMERCIAL TITLE                      
         LA    R4,NEEDKEY                                                       
         USING TLCOPD,R4                                                        
         MVI   TLCOPCD,TLCOICDQ                                                 
         MVC   TLCOIAGY,INAGY                                                   
         MVC   TLCOICID,INTITLE                                                 
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         EJECT                                                                  
*              GENERAL BUFFER HANDLER                                           
         SPACE 3                                                                
NEEDREC  NTR1                                                                   
         L     R2,ABUFFER                                                       
         LTR   R2,R2                                                            
         BNZ   NREC2                                                            
         L     R0,LBUFFER                                                       
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ABUFFER                                                       
         L     R2,ABUFFER                                                       
         MVC   0(4,R2),=F'100'     SET UP FOR 100 RECORDS                       
         MVC   4(4,R2),=F'4000'    4000 BYTES EACH                              
         XC    8(4,R2),8(R2)                                                    
         LA    RF,100                                                           
         M     RE,=F'4000'                                                      
         LA    RE,12(R2)                                                        
*                                  CLEAR BUFFER FIRST TIME                      
         XCEF                                                                   
         B     NREC2                                                            
         SPACE 1                                                                
ABUFFER  DC    A(0)                                                             
LBUFFER  DC    F'400016'           (100*4000 + 16)                              
         SPACE 1                                                                
NREC2    DS    0H                  NOW R2 HAS A(BUFFER)                         
*                                  BYTES  1-4 N'ENTRIES                         
*                                  BYTES  5-8 L'ENTRY                           
*                                  BYTES 9-12 NUMBER OF LAST ENTRY              
         LA    R4,12(R2)           BYTES 13+  THE BUFFER!                       
         L     R0,0(R2)                                                         
         SPACE 1                                                                
NREC6    CLC   NEEDKEY,0(R4)       IS MY RECORD IN THE BUFFER?                  
         BE    NREC10                                                           
         A     R4,4(R2)                                                         
         BCT   R0,NREC6                                                         
         SPACE 1                                                                
         MVI   NEEDHIT,C'N'                                                     
         MVC   KEY,NEEDKEY         NO, NOW NEED THE RECORD                      
         GOTO1 HIGH                                                             
         CLC   NEEDKEY(32),KEY                                                  
         BNE   NREC8                                                            
         MVI   NEEDHIT,C'Y'                                                     
         SPACE 1                                                                
NREC8    L     R1,8(R2)            NO - PICK UP N'LAST ENTRY                    
         LA    R1,1(R1)                 ROUND ROBIN                             
         C     R1,0(R2)            HAVE WE GOT TO THE END OF BUFFER?            
         BNH   *+8                                                              
         LA    R1,1                YES, SO GO BACK TO THE BEGINNING             
         ST    R1,8(R2)                                                         
         BCTR  R1,0                                                             
         M     R0,4(R2)            DISPLACE INTO THE BUFFER                     
         LA    R4,12(R1,R2)                                                     
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         L     R2,4(R2)                                                         
         MOVE  ((R4),(R2)),(R3)    MOVE INTO OUR AREA                           
         OC    TIKEY,TIKEY         IS SYSIO READING RECORDS                     
         BZ    NREC10                                                           
         TM    TISTAT,TISTRDCK     UNLESS READING CHECK FILE                    
         BO    NREC10                                                           
         MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                REREAD TO ESTABLISH SEQUENCE                 
         SPACE 1                                                                
NREC10   ST    R4,NEEDAREC         PASS BACK A RECORD                           
         BAS   RE,GETNAME                                                       
         B     ITSFINE                                                          
         SPACE 1                                                                
NEEDAREC DS    A                                                                
NEEDKEY  DC    XL32'00'                                                         
NEEDNAME DS    CL36                                                             
NEEDSHRT DS    CL16                                                             
NEEDHIT  DS    CL1                                                              
NEEDTYPE DS    CL1                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
GETNAME  NTR1                                                                   
*                                  R4=A(RECORD) TO NEEDNAME                     
         MVC   SAVEEL,ELCODE       SAVE PRESENT ELCODE                          
         MVC   NEEDNAME,MYSPACES                                                
         CLI   0(R4),TLW4CDQ                                                    
         BE    GETW4NM                                                          
         MVI   ELCODE,TANAELQ                                                   
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         USING TANAD,R6                                                         
         ZIC   R1,TANALEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     ITSFINE                                                          
         MVC   NEEDNAME(0),TANANAME                                             
         SPACE 1                                                                
GETW4NM  MVI   ELCODE,TAW4ELQ                                                   
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         USING TAW4D,R6                                                         
         MVC   NEEDNAME(32),TAW4CRPN                                            
         MVC   NEEDTYPE,TAW4TYPE                                                
         B     ITSFINE                                                          
         SPACE 1                                                                
GETSHORT NTR1                                                                   
*                                  R4=A(RECORD) TO NEEDSHRT                     
         MVC   SAVEEL,ELCODE       SAVE PRESENT ELCODE                          
         MVI   ELCODE,TASNELQ      SHORT NAME                                   
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   NEEDSHRT,MYSPACES                                                
         MVC   ELCODE,SAVEEL                                                    
         BNE   XIT                                                              
         USING TASND,R6                                                         
         ZIC   R1,TASNLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   NEEDSHRT(0),TASNAME                                              
         SPACE 1                                                                
ITSFINE  SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NOGOOD   LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
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
                                                                                
         MVC   WORK(20),=CL20'TALTAPE.TA0PXDS1'                                 
         LA    R2,=CL8'PXMTAPE'                                                 
         CLI   RECNUM,PX                                                        
         BNE   OPNTAPE3                                                         
         CLI   WAV2OPT,C'Y'                                                     
         BNE   OPNTAPE5                                                         
         MVC   WORK(20),=CL20'TALTAPE.TA0PYDS1'                                 
         LA    R2,=CL8'PYMTAPE'                                                 
         B     OPNTAPE5                                                         
                                                                                
OPNTAPE3 MVC   WORK(20),=CL20'TALTAPE.TA0DMDS1'                                 
         LA    R2,=CL8'DMBTAPE'                                                 
         CLI   SAATOPT,C'Y'                                                     
         BNE   OPNTAPE5                                                         
         MVC   WORK(20),=CL20'TALTAPE.TA0STDS1'                                 
         LA    R2,=CL8'SATTAPE'                                                 
                                                                                
OPNTAPE5 GOTO1 DYNALLOC,DMCB,(0,(R2)),((RF),WORK)                               
         L     R2,=A(PXMTAPE)                                                   
         CLI   RECNUM,PX                                                        
         BNE   OPNTAPE6                                                         
         CLI   WAV2OPT,C'Y'                                                     
         BNE   OPNTAPE7                                                         
         L     R2,=A(PYMTAPE)                                                   
         B     OPNTAPE7                                                         
                                                                                
OPNTAPE6 L     R2,=A(DMBTAPE)                                                   
         CLI   SAATOPT,C'Y'                                                     
         BNE   *+8                                                              
         L     R2,=A(SATTAPE)                                                   
                                                                                
OPNTAPE7 CLI   DOWNOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,TADOWN         IF DOWNLOADING, USE TADOWN                     
         OPEN  ((2),OUTPUT)                                                     
                                                                                
         CLI   RECNUM,PX                                                        
         BNE   XIT                                                              
         BRAS  RE,PXMLHEAD       XML HEADER                                     
         B     XIT                                                              
*----------------------------------------------------------------------         
CLOSTAPE NTR1                                                                   
         BAS   RE,SPLAT                                                         
         LA    R2,MYP                                                           
         USING MYPRINT,R2                                                       
         EDIT  (P6,TAPCOUNT),(7,PTOTALS)                                        
         MVC   PTOTALS+8(12),=C'TAPE RECORDS'                                   
         DROP  R2                                                               
         BAS   RE,SPLAT                                                         
                                                                                
         CLI   RECNUM,PX                                                        
         BNE   CLSTAPE3                                                         
         BRAS  RE,PXMLTRLR                                                      
                                                                                
CLSTAPE3 CLI   TAPEOPT,C'Y'                                                     
         BNE   XIT                                                              
         L     R2,=A(PXMTAPE)                                                   
         CLI   RECNUM,PX                                                        
         BNE   CLSTAPE4                                                         
         CLI   WAV2OPT,C'Y'                                                     
         BNE   CLSTAPE5                                                         
         L     R2,=A(PYMTAPE)                                                   
         B     CLSTAPE5                                                         
                                                                                
CLSTAPE4 L     R2,=A(DMBTAPE)                                                   
         CLI   SAATOPT,C'Y'                                                     
         BNE   *+8                                                              
         L     R2,=A(SATTAPE)                                                   
                                                                                
CLSTAPE5 CLI   DOWNOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,TADOWN         IF DOWNLOADING, USE TADOWN                     
         CLOSE ((2))                                                            
         B     XIT                                                              
*----------------------------------------------------------------------         
PUTTAPE  NTR1                                                                   
         CLI   RECNUM,PX           PUBXML SHOULD ONLY COUNT INVOICES            
         BE    *+10                                                             
         AP    TAPCOUNT,=P'1'                                                   
                                                                                
         MVC   RECTYPE,=CL16'OUTPUT'                                            
         CLI   TRACOPT,C'Y'                                                     
         BNE   PUTTAPE2                                                         
         CP    TAPCOUNT,TRALIMIT                                                
         BH    PUTTAPE2                                                         
         BAS   RE,TRACEM                                                        
         SPACE 1                                                                
PUTTAPE2 CLI   TAPEOPT,C'Y'                                                     
         BNE   XIT                                                              
         L     R0,AIO2           XML CODE USES AIO2                             
         L     R1,=A(PXMTAPE)                                                   
         CLI   RECNUM,PX                                                        
         BNE   PUTTAPE3                                                         
         CLI   WAV2OPT,C'Y'                                                     
         BNE   PUTTAPE4                                                         
         L     R1,=A(PYMTAPE)                                                   
         B     PUTTAPE4                                                         
                                                                                
PUTTAPE3 LA    R0,TAPEIO         ALL OTHERS USE TAPEIO                          
         L     R1,=A(DMBTAPE)                                                   
         CLI   SAATOPT,C'Y'                                                     
         BNE   *+8                                                              
         L     R1,=A(SATTAPE)                                                   
                                                                                
PUTTAPE4 CLI   DOWNOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R1,TADOWN         IF DOWNLOADING, USE TADOWN                     
         PUT   (1),(0)                                                          
                                                                                
         CLI   RECNUM,PX                                                        
         BNE   XIT                                                              
         L     R4,AIO2           RESET R4 FOR XML TAGIT                         
         BRAS  RE,CLRXMLTG                                                      
         XIT1  REGS=(R4)                                                        
         EJECT                                                                  
*              TRACING ROUTINES                                                 
         SPACE 3                                                                
TRACEINV NTR1                                                                   
         L     R6,TIAMAIN                                                       
         MVC   RECTYPE,=CL16'INVOICE'                                           
         BAS   RE,TRACEREC                                                      
         B     XIT                                                              
         SPACE 1                                                                
TRACECHK NTR1                                                                   
         L     R6,TIAREC                                                        
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
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*              SORT UTILITIES                                                   
         SPACE 3                                                                
SORTPUT  NTR1                                                                   
         CLI   RECNUM,PX           IF PUBXML                                    
         BNE   SORTPUT1                                                         
         OC    ALLTOT,ALLTOT         DON'T PUT $0 INVOICES                      
         BZ    XIT                                                              
                                                                                
SORTPUT1 AP    OUTCOUNT,=P'1'                                                   
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
         MOVE  (TAPEIO,646),0(R2)                                               
         B     XIT                                                              
         DS    0F                                                               
LASTSKEY DC    XL20'00'                                                         
SORTFRST DC    C'Y'                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,25,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(646)'                                 
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
         SPACE 3                                                                
         ENTRY PYMTAPE                                                          
         SPACE 1                                                                
PYMTAPE  DCB   DDNAME=PYMTAPE,DSORG=PS,MACRF=(PM),                     X        
               RECFM=FU,LRECL=168,BUFNO=2,BLKSIZE=5040                          
*                                                                               
         ENTRY PXMTAPE                                                          
         SPACE 1                                                                
PXMTAPE  DCB   DDNAME=PXMTAPE,DSORG=PS,MACRF=(PM),                     X        
               RECFM=FU,LRECL=168,BUFNO=2,BLKSIZE=5040                          
*                                                                               
         ENTRY DMBTAPE                                                          
         SPACE 1                                                                
DMBTAPE  DCB   DDNAME=DMBTAPE,DSORG=PS,MACRF=(PM),                     X        
               RECFM=FU,LRECL=646,BUFNO=2,BLKSIZE=6460                          
*                                                                               
         ENTRY SATTAPE                                                          
         SPACE 1                                                                
SATTAPE  DCB   DDNAME=SATTAPE,DSORG=PS,MACRF=(PM),                     X        
               RECFM=FU,LRECL=646,BUFNO=2,BLKSIZE=6460                          
*                                                                               
TADOWN   DCB   DDNAME=TADOWN,DSORG=PS,MACRF=(GM,PM),                   X        
               RECFM=FB,LRECL=646,BLKSIZE=6460,EODAD=NOMORE                     
         SPACE 3                                                                
         EJECT                                                                  
*----------------------------------------------------------------------         
*              ROUTINE ADDS AMOUNT TO A SESSION BUCKET BASED ON CAST            
*                 ASSUMES TGCAT IS SET                                          
*                 P1 = AMOUNT TO ACCUMULATE                                     
*----------------------------------------------------------------------         
*        SPACE 1                                                                
         USING TAPED,R5                                                         
SESSBUCK NTR1  BASE=*,LABEL=*                                                   
         CLI   WAV2OPT,C'Y'        WAVE 2 ONLY                                  
         JNE   XIT                                                              
         CLI   INREUSE,C'R'        SESSION ONLY                                 
         JE    XIT                                                              
                                                                                
         L     R2,0(R1)            AMOUNT TO ACCUMULATE                         
                                                                                
         LA    R3,SEVOICEO         VOICE OVERS                                  
         CLC   TGCAT,=CL3'VO '                                                  
         BE    SBUCK900                                                         
         CLC   TGCAT,=CL3'CV '                                                  
         BE    SBUCK900                                                         
         CLC   TGCAT,=CL3'MV '                                                  
         BE    SBUCK900                                                         
         CLC   TGCAT,=CL3'ANN'                                                  
         BE    SBUCK900                                                         
         CLC   TGCAT,=CL3'NAR'                                                  
         BNE   SBUCK100                                                         
         CLC   THISCAM,=C'OFF'                                                  
         BE    SBUCK900                                                         
                                                                                
SBUCK100 LA    R3,SEHANDMD         HAND MODELS                                  
         CLC   TGCAT,=CL3'HM '                                                  
         BE    SBUCK900                                                         
         CLC   TGCAT,=CL3'HMB'                                                  
         BE    SBUCK900                                                         
                                                                                
         LA    R3,SEEXTRAS         EXTRAS                                       
         CLC   TGCAT,=CL3'PE '                                                  
         BE    SBUCK900                                                         
         TM    TGCATYPE,EXTRA                                                   
         BO    SBUCK900                                                         
                                                                                
         LA    R3,SESINGER         SINGERS                                      
         CLC   TGCAT,=CL3'SV '                                                  
         BE    SBUCK900                                                         
         TM    TGCASTAT,SINGER                                                  
         BO    SBUCK900                                                         
                                                                                
         LA    R3,SEMUSICN         MUSICIAN                                     
         GOTO1 UNITEST,DMCB,TGCAUNIS,AFM,0,0,0                                  
         BO    SBUCK900                                                         
                                                                                
         LA    R3,SEPRTMDL         PRINT MODEL                                  
         CLC   TGCAT,=CL3'PM '                                                  
         BE    SBUCK900                                                         
                                                                                
         LA    R3,SECREW           CREW CAST                                    
         BAS   RE,ISCREW                                                        
         BE    SBUCK900                                                         
                                                                                
         LA    R3,SEONCPRN         ON-CAMERA                                    
         CLC   THISCAM,=C'ON '                                                  
         BE    SBUCK900                                                         
                                                                                
         LA    R3,SEOTHER          ANYTHING NOT ABOVE GOES HERE                 
*                                                                               
SBUCK900 L     RE,0(R3)            ACCUMLATE TO CORRESPONDING BUCKET            
         AR    RE,R2                                                            
         ST    RE,0(R3)                                                         
                                                                                
         J     XIT                                                              
         EJECT                                                                  
ISCREW   NTR1                                                                   
         LA    RF,CREWCAST                                                      
                                                                                
ISCREW10 CLI   0(RF),X'FF'                                                      
         JE    ISCREWN                                                          
         CLC   TGCAT,0(RF)         CHECK IF CAST CATEGORY IS CREW               
         JE    XIT                                                              
         AHI   RF,3                                                             
         J     ISCREW10                                                         
                                                                                
ISCREWN  LTR   RB,RB               RETURN CC=NO                                 
         J     XIT                                                              
*                                                                               
CREWCAST DS    0C                                                               
         DC    CL3'AD '                                                         
         DC    CL3'APR'                                                         
         DC    CL3'BBE'                                                         
         DC    CL3'CAR'                                                         
         DC    CL3'COA'                                                         
         DC    CL3'COS'                                                         
         DC    CL3'CPT'                                                         
         DC    CL3'DEC'                                                         
         DC    CL3'DI2'                                                         
         DC    CL3'DIR'                                                         
         DC    CL3'DO '                                                         
         DC    CL3'EL '                                                         
         DC    CL3'GAF'                                                         
         DC    CL3'GEN'                                                         
         DC    CL3'GO '                                                         
         DC    CL3'GR '                                                         
         DC    CL3'GRB'                                                         
         DC    CL3'GRD'                                                         
         DC    CL3'GRK'                                                         
         DC    CL3'HA '                                                         
         DC    CL3'KHS'                                                         
         DC    CL3'KMU'                                                         
         DC    CL3'LMP'                                                         
         DC    CL3'MUA'                                                         
         DC    CL3'OTC'                                                         
         DC    CL3'PH '                                                         
         DC    CL3'PR '                                                         
         DC    CL3'PRA'                                                         
         DC    CL3'PRM'                                                         
         DC    CL3'PTR'                                                         
         DC    CL3'SCS'                                                         
         DC    CL3'SDR'                                                         
         DC    CL3'SE '                                                         
         DC    CL3'SPE'                                                         
         DC    CL3'W  '                                                         
         DC    CL3'W2 '                                                         
         DC    CL3'W2B'                                                         
         DC    CL3'W2H'                                                         
         DC    CL3'W2S'                                                         
         DC    CL3'W3 '                                                         
         DC    CL3'W3B'                                                         
         DC    CL3'W3H'                                                         
         DC    CL3'W3S'                                                         
         DC    CL3'WB '                                                         
         DC    CL3'WH '                                                         
         DC    CL3'WS '                                                         
         DC    X'FF'                                                            
*                                                                               
*                                                                               
*              ROUTINE TO GET CLIENT IF OPTION SAATCHI                          
*              CLIENT MAY BE SET FOR NO INTERFACE                               
*        SPACE 1                                                                
         USING TAPED,R5                                                         
CLINOINT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING TLCLD,R6                                                         
         MVI   TLCLCD,TLCLCDQ                                                   
         MVC   TLCLAGY,TIAGY      AGENCY                                        
         MVC   TLCLCLI,MYWORK     CLIENT                                        
         GOTO1 HIGH                                                             
         CLC   KEY(TLCLSPR3-TLCLKEY),KEYSAVE                                    
         BNE   CNOINTN             DIDN'T FIND CLIENT                           
*                                                                               
         MVC   BYTE,ELCODE         SAVE ELCODE                                  
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,TABRELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   CNOINTN                                                          
         USING TABRD,R6                                                         
         TM    TABRSTAT,TABRSNIN   SET TO NOT INTERFACE?                        
         BZ    CNOINTN                                                          
*                                                                               
CNOINTY  SR    R1,R1                                                            
         B     *+8                                                              
CNOINTN  LA    R1,1                                                             
         LTR   R1,R1                                                            
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
         MVC   ELCODE,BYTE         RESTORE ELCODE                               
         B     XIT                                                              
         XIT1                                                                   
         LTORG                                                                  
         DROP  R5,R6                                                            
         EJECT                                                                  
*                                                                               
*              ROUTINE TO GET BUSINESS UNIT IF OPTION SAATCHI                   
*              BUSINESS UNIT SHOULD COME BEFORE CLI/PRD/JOB IN ESTIMATE         
*              R5 --> TAPEIO                                                    
*        SPACE 1                                                                
         USING TAPED,R5                                                         
GETBUSU  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,16               LENGTH OF ESTIMATE = 16                      
         LA    R2,WORK+15          CHECK END OF ESTIMATE FOR SPACES             
GBUSU10  CLI   0(R2),X'40'         SPACE?                                       
         BH    GBUSU20                                                          
         BCTR  R1,0                SUBTRACT 1 FROM LENGTH FOR SPACE             
         AHI   R2,-1               BUMP BACKWARDS                               
         B     GBUSU10                                                          
*                                                                               
GBUSU20  CHI   R1,12               IF LENGTH OF ESTIMATE NUMBER > 12,           
         BH    GBUSUX              BUSINESS UNIT MUST ALREADY BE THERE          
         STC   R1,SVLEN            SAVE LENGTH OF ESTIMATE                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING TLJBD,R6                                                         
         MVI   TLJBCD,TLJBCDQ                                                   
         MVC   TLJBAGY,INAGY      AGENCY                                        
*                                                                               
         CLC   TLJBAGY,=C'2071  '  IF AGENCY IS 2071                            
         BNE   *+10                                                             
         MVC   TLJBAGY,=C'4937  '  LOOK FOR JOB UNDER 4937                      
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(TLJBDTE-TLJBD),KEYSAVE                                       
         BNE   GBUSUX                                                           
         MVC   FULL(3),TLJBDTE     LATEST DATE OF JOB RECS FOR THIS AGY         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVI   TLJBCD,TLJBCDQ                                                   
         MVC   TLJBAGY,INAGY       AGENCY                                       
*                                                                               
         CLC   TLJBAGY,=C'2071  '  IF AGENCY IS 2071                            
         BNE   *+10                                                             
         MVC   TLJBAGY,=C'4937  '  LOOK FOR JOB UNDER 4937                      
*                                                                               
         MVC   TLJBDTE,FULL                                                     
         MVC   TLJBCLI(3),WORK     ASSUME 1ST 3 CHARS ARE CLIENT                
         OC    TLJBCLI,MYSPACES                                                 
         MVC   TLJBPRD(3),WORK+3   ASSUME 2ND 3 CHARS ARE PRODUCT               
         OC    TLJBPRD,MYSPACES                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(TLJBSEQ-TLJBD),KEYSAVE  IF JOB RECORD NOT FOUND,             
         BNE   GBUSUX              ASSUME 1ST 3 CHARS ARE BUS UNIT              
*                                                                               
         MVC   BYTE,ELCODE         SAVE ELCODE                                  
         XC    MYWORK,MYWORK                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,TANUELQ      GET BUSINESS UNIT ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   GBUSUX                                                           
         USING TANUD,R6                                                         
         CLI   TANUTYPE,TANUBUSI                                                
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   MYWORK(3),TANUMBER  BUSINESS UNIT                                
         ZIC   R1,SVLEN            SAVED LENGTH OF ESTIMATE                     
         BCTR  R1,0                LENGTH OF ESTIMATE NUMBER IN WORK            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYWORK+3(0),WORK    CLI/PRD/JOB                                  
         MVC   INEST,MYWORK                                                     
         OC    INEST,MYSPACES                                                   
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
         MVC   ELCODE,BYTE         RESTORE ELCODE                               
GBUSUX   XIT1                                                                   
         LTORG                                                                  
         DROP  R5,R6                                                            
         EJECT                                                                  
*                                                                               
*              ROUTINE TO GET SUB CATEGORY IF OPTION SAATCHI                    
*              R4 --> TIAREC                                                    
*        SPACE 1                                                                
         USING TLCKD,R4                                                         
GETSUB   NTR1  BASE=*,LABEL=*                                                   
         MVC   THISCAT,MYSPACES                                                 
         CLC   THISUN,=C'AFM'       IF UNION AFM,                               
         BNE   *+14                                                             
         MVC   THISCAT(2),=CL2'HM'  USE SUBCAT HM FOR ALL CATEGORIES            
         B     GSUBX                                                            
*                                                                               
         LA    R3,SUBTAB                                                        
         USING SUBTABD,R3                                                       
GSUB10   CLI   0(R3),X'FF'                                                      
         BNE   GSUB15                                                           
         MVC   THISCAT(3),TLCKCAT                                               
         MVC   THISCAT+3(4),=C'????'  CATEGORY NOT IN TABLE                     
         MVI   BDCATFLG,C'Y'          SET BAD CATEGORY FLAG                     
         B     GSUBX                                                            
GSUB15   CLC   SUBCAT,TLCKCAT                                                   
         BE    GSUB20                                                           
         LA    R3,SUBLNQ(R3)                                                    
         B     GSUB10                                                           
*                                                                               
GSUB20   MVC   THISCAT(2),SUBSUB   SUB CATEGORY                                 
*                                                                               
GSUBX    XIT1                                                                   
         DROP  R3,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
SUBTAB   DS    0CL5                                                             
* SAG/AFTRA                                                                     
         DC    CL3'P  ',CL2'GO'                                                 
         DC    CL3'ACR',CL2'GO'                                                 
         DC    CL3'ANN',CL2'VO'                                                 
         DC    CL3'G3 ',CL2'SI'                                                 
         DC    CL3'G6 ',CL2'SI'                                                 
         DC    CL3'G9 ',CL2'SI'                                                 
         DC    CL3'C3 ',CL2'SI'                                                 
         DC    CL3'C6 ',CL2'SI'                                                 
         DC    CL3'C9 ',CL2'SI'                                                 
         DC    CL3'G3M',CL2'SI'                                                 
         DC    CL3'G6M',CL2'SI'                                                 
         DC    CL3'G9M',CL2'SI'                                                 
         DC    CL3'S  ',CL2'SI'                                                 
         DC    CL3'SM ',CL2'SI'                                                 
         DC    CL3'SS1',CL2'SI'                                                 
         DC    CL3'SS2',CL2'SI'                                                 
         DC    CL3'SS3',CL2'SI'                                                 
         DC    CL3'SS4',CL2'SI'                                                 
         DC    CL3'D  ',CL2'SI'                                                 
         DC    CL3'CV ',CL2'VO'                                                 
         DC    CL3'EX ',CL2'GX'                                                 
         DC    CL3'EXB',CL2'GX'                                                 
         DC    CL3'HM ',CL2'GE'                                                 
         DC    CL3'HMB',CL2'GE'                                                 
         DC    CL3'SD ',CL2'GO'                                                 
         DC    CL3'GD3',CL2'GO'                                                 
         DC    CL3'GD6',CL2'GO'                                                 
         DC    CL3'GD9',CL2'GO'                                                 
         DC    CL3'ST ',CL2'GO'                                                 
         DC    CL3'SA ',CL2'GO'                                                 
         DC    CL3'PI ',CL2'GO'                                                 
         DC    CL3'PIL',CL2'GO'                                                 
         DC    CL3'PUP',CL2'GN'                                                 
         DC    CL3'ZZZ',CL2'GN'                                                 
         DC    CL3'SE ',CL2'VO'                                                 
         DC    CL3'EXD',CL2'GX'                                                 
         DC    CL3'GEN',CL2'GN'                                                 
* ACTRA/UDA                                                                     
         DC    CL3'PP ',CL2'GO'                                                 
         DC    CL3'SOC',CL2'GO'                                                 
         DC    CL3'VO ',CL2'VO'                                                 
         DC    CL3'SS ',CL2'SI'                                                 
         DC    CL3'SSM',CL2'SI'                                                 
         DC    CL3'SS1',CL2'SI'                                                 
         DC    CL3'SS2',CL2'SI'                                                 
         DC    CL3'SS3',CL2'SI'                                                 
         DC    CL3'SS4',CL2'SI'                                                 
         DC    CL3'GS ',CL2'SI'                                                 
         DC    CL3'GS3',CL2'SI'                                                 
         DC    CL3'GS6',CL2'SI'                                                 
         DC    CL3'GS9',CL2'SI'                                                 
         DC    CL3'GSM',CL2'SI'                                                 
         DC    CL3'GSS',CL2'SI'                                                 
         DC    CL3'GD ',CL2'GO'                                                 
         DC    CL3'SD ',CL2'GO'                                                 
         DC    CL3'DEM',CL2'GO'                                                 
         DC    CL3'E  ',CL2'GX'                                                 
         DC    CL3'GE ',CL2'GX'                                                 
         DC    CL3'ST ',CL2'GO'                                                 
         DC    CL3'US ',CL2'GX'                                                 
         DC    CL3'SI ',CL2'GX'                                                 
         DC    CL3'SB ',CL2'GX'                                                 
         DC    CL3'SA ',CL2'GO'                                                 
         DC    CL3'CAR',CL2'GO'                                                 
         DC    CL3'PT ',CL2'GN'                                                 
         DC    CL3'PE ',CL2'GX'                                                 
         DC    CL3'GEN',CL2'GN'                                                 
         DC    CL3'MV ',CL2'VO'                                                 
         DC    CL3'SV ',CL2'VO'                                                 
*PRINT                                                                          
         DC    CL3'PM ',CL2'AN'                                                 
         DC    CL3'PH ',CL2'AN'                                                 
         DC    X'FF'                                                            
         EJECT                                                                  
*======================================================================         
PXMLHEAD NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO2                                                          
         BRAS  RE,CLRXMLTG                                                      
                                                                                
         GOTO1 =A(TAGIT),DMCB,(L'TXMLHD,TXMLHD)       <?xml                     
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLINVS,TXMLINVS)   <Invoices                 
         BRAS  RE,PUTTAPE                                                       
         J     XIT                                                              
         LTORG                                                                  
TXMLHD   DC    C'<?xml version="1.0" encoding="UTF-8"?>'                        
TXMLINVS DC    C'<Invoices xmlns="urn:publicis.com:0077:PUBLICIS_I_PUR_X        
               EDI1.0">'                                                        
         EJECT                                                                  
*======================================================================         
PXMLINVS NTR1  BASE=*,LABEL=*                                                   
         OC    ALLTOT,ALLTOT       IF INVOICE TOTAL = 0, SKIP                   
         JZ    XIT                                                              
                                                                                
         L     R4,AIO2                                                          
         BRAS  RE,CLRXMLTG                                                      
                                                                                
         LA    R5,TAPEIO                                                        
         USING TAPED,R5                                                         
                                                                                
         GOTO1 =A(TAGIT),DMCB,(L'TXMLINS,TXMLINS)     <Invoice>                 
         BRAS  RE,PUTTAPE                                                       
                                                                                
         L     R1,ALLPAY                                                        
         CLI   WAV2OPT,C'Y'        WAVE2 HAS REIMB ITEM                         
         BE    *+8                                                              
         A     R1,ALLREXP                                                       
         A     R1,ALLAPPLY                                                      
         ST    R1,ALLPAY                                                        
                                                                                
         BRAS  RE,PXMLIHD          INVOICE HEADER                               
         BRAS  RE,PXMLIIT          INVOICE ITEM                                 
                                                                                
         GOTO1 =A(TAGIT),DMCB,(L'TXMLINE,TXMLINE)     </Invoice>                
         BRAS  RE,PUTTAPE                                                       
         AP    TAPCOUNT,=P'1'                                                   
                                                                                
         J     XIT                                                              
         LTORG                                                                  
TXMLINS  DC    C'    <Invoice xmlns="">'                                        
TXMLINE  DC    C'    </Invoice>'                                                
         EJECT                                                                  
*======================================================================         
PXMLIHD  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 =A(TAGIT),DMCB,(L'TXMLIHDS,TXMLIHDS)   <Header>                  
         BRAS  RE,PUTTAPE                                                       
                                                                                
         LA    R3,TXMLVND                                                       
         CLI   TSTOPT,C'Y'                                                      
         BNE   PXMLIH10                                                         
         LA    R3,TXMLVNDT                                                      
PXMLIH10 GOTO1 =A(TAGIT),DMCB,(L'TXMLVND,(R3))        VendorNumber              
                                                                                
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLCMPS,TXMLCMPS)     CompanyCode             
                                                                                
         CLI   TSTOPT,C'Y'                                                      
         BE    PXMLIH30                                                         
PXMLIH20 GOTO1 =A(TAGIT),DMCB,(3,INEST+1)                                       
         B     PXMLIH35                                                         
PXMLIH30 GOTO1 =A(TAGIT),DMCB,(3,=C'116')                                       
*XMLIH30 GOTO1 =A(TAGIT),DMCB,(3,=C'111')                                       
                                                                                
PXMLIH35 GOTO1 =A(TAGIT),DMCB,(L'TXMLCMPE,TXMLCMPE)                             
         BRAS  RE,PUTTAPE                                                       
                                                                                
         GOTO1 =A(TAGIT),DMCB,(L'TXMLDDTS,TXMLDDTS)   <DocumentDate>            
         MVC   BLOCK(2),ININVDT+6          INV DATE IN DDMMYYYY                 
         MVC   BLOCK+2(2),ININVDT+4                                             
         MVC   BLOCK+4(4),ININVDT                                               
         GOTO1 =A(TAGIT),DMCB,(8,BLOCK)                                         
         GOTO1 =A(TAGIT),DMCB,(L'TXMLDDTE,TXMLDDTE)   </DocumentDate>           
         BRAS  RE,PUTTAPE                                                       
                                                                                
         GOTO1 =A(TAGIT),DMCB,(L'TXMLJOB,TXMLJOB)     JobNumber                 
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLPON,TXMLPON)     PONumber                  
         BRAS  RE,PUTTAPE                                                       
                                                                                
         GOTO1 =A(TAGIT),DMCB,(L'TXMLRDNS,TXMLRDNS)   <ReferenceDoc             
         IF (CLC,ININV(2),EQ,=C'F6')                                            
           IF (CLI,ININV+2,LT,C'5')                   F60??? - F64???           
             MVI   ININV,C'Z'                         Repl F with Z             
           ENDIF                                                                
         ENDIF                                                                  
         GOTO1 =A(TAGIT),DMCB,(6,ININV)                                         
         GOTO1 =A(TAGIT),DMCB,(L'TXMLRDNE,TXMLRDNE)   </ReferenceDoc            
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLGRAS,TXMLGRAS)   <GrossAmount>             
         GOTO1 =A(TAGAMT),DMCB,ALLTOT                                           
         GOTO1 =A(TAGIT),DMCB,(12,BLOCK)                                        
         GOTO1 =A(TAGIT),DMCB,(L'TXMLGRAE,TXMLGRAE)   </GrossAmount>            
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLNTAS,TXMLNTAS)   <NetAmount>               
         GOTO1 =A(TAGAMT),DMCB,ALLTOT                                           
         GOTO1 =A(TAGIT),DMCB,(12,BLOCK)                                        
         GOTO1 =A(TAGIT),DMCB,(L'TXMLNTAE,TXMLNTAE)   </NetAmount>              
         BRAS  RE,PUTTAPE                                                       
                                                                                
         GOTO1 =A(TAGIT),DMCB,(L'TXMLCURR,TXMLCURR)   Currency                  
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLHDRT,TXMLHDRT)   HeaderText                
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLIHDE,TXMLIHDE)   </Header>                 
         BRAS  RE,PUTTAPE                                                       
         J     XIT                                                              
         LTORG                                                                  
TXMLIHDS DC    C'        <Header>'                                              
TXMLIHDE DC    C'        </Header>'                                             
TXMLVND  DC    C'            <VendorNumber>0200033034</VendorNumber>'           
TXMLVNDT DC    C'            <VendorNumber>200033034</VendorNumber> '           
*XMLVNDT DC    C'            <VendorNumber>200018869</VendorNumber> '           
TXMLCMPS DC    C'            <CompanyCode>'                                     
TXMLCMPE DC    C'</CompanyCode>'                                                
TXMLDDTS DC    C'            <DocumentDate>'                                    
TXMLDDTE DC    C'</DocumentDate>'                                               
TXMLJOB  DC    C'            <JobNumber/>'                                      
TXMLPON  DC    C'            <PONumber/>'                                       
TXMLRDNS DC    C'            <ReferenceDocumentNumber>'                         
TXMLRDNE DC    C'</ReferenceDocumentNumber>'                                    
TXMLGRAS DC    C'            <GrossAmount>'                                     
TXMLGRAE DC    C'</GrossAmount>'                                                
TXMLNTAS DC    C'            <NetAmount>'                                       
TXMLNTAE DC    C'</NetAmount>'                                                  
TXMLCURR DC    C'            <CurrencyISO>USD</CurrencyISO>'                    
TXMLHDRT DC    C'            <HeaderText/>'                                     
         EJECT                                                                  
*======================================================================         
PXMLIIT  NTR1  BASE=*,LABEL=*                                                   
         CLI   WAV2OPT,C'Y'  WAVE 2                                             
         BNE   PXMLIIT6                                                         
         CLI   INREUSE,C'R'  SESSION ONLY                                       
         JE    PXMLIIT6                                                         
                                                                                
*    02 ON 000 (Wave 2 Sessions)                                                
         LA    R3,SEVOMTL                                                       
         LA    R6,SEVOICEO                                                      
         LHI   R0,(SESSAMTS/4)                                                  
                                                                                
PXMLIIT2 MVC   SVMATERL,0(R3)                                                   
         GOTO1 =A(ITEM),DMCB,(L'TXMLITT1,TXMLITT1),(R6)                         
PXMLITT3 AHI   R6,4                                                             
         AHI   R3,6                                                             
         BCT   R0,PXMLIIT2                                                      
                                                                                
         J     PXMLIIT8                                                         
                                                                                
*    02 ON 000 (Resuse or Wave 1 Sessions)                                      
PXMLIIT6 GOTO1 =A(ITEM),DMCB,(L'TXMLITT1,TXMLITT1),ALLPAY                       
                                                                                
*    Pension + Welfare                                                          
PXMLIIT8 MVC   SVMATERL,=C'P01837'                                              
         CLI   GENMTRS,C'Y'                                                     
         JNE   *+10                                                             
         MVC   SVMATERL,=C'P01590'                                              
         GOTO1 =A(ITEM),DMCB,(L'TXMLITT3,TXMLITT3),ALLPNH                       
*    Payroll Tax                                                                
         MVC   SVMATERL,=C'P01838'                                              
         CLI   GENMTRS,C'Y'                                                     
         JNE   *+10                                                             
         MVC   SVMATERL,=C'P01590'                                              
         GOTO1 =A(ITEM),DMCB,(L'TXMLITT4,TXMLITT4),ALLTAXX                      
*    Handling                                                                   
         MVC   SVMATERL,=C'P01104'                                              
         CLI   GENMTRS,C'Y'                                                     
         JNE   *+10                                                             
         MVC   SVMATERL,=C'P01590'                                              
         GOTO1 =A(ITEM),DMCB,(L'TXMLITT5,TXMLITT5),ALLHANDX                     
*    Reimbursement                                                              
         CLI   WAV2OPT,C'Y'                                                     
         JNE   XIT                                                              
         MVC   SVMATERL,=C'P01591'                                              
         GOTO1 =A(ITEM),DMCB,(L'TXMLITT2,TXMLITT2),ALLREXP                      
         J     XIT                                                              
*                                                                               
SEVOMTL  DC    C'P01647'           VOICE OVER                                   
         DC    C'P01597'           HAND MODEL                                   
         DC    C'P01595'           EXTRAS                                       
         DC    C'P02257'           SINGERS                                      
         DC    C'P02256'           MUSICIANS                                    
         DC    C'P00921'           PRINT MODEL                                  
         DC    C'P03404'           CREW                                         
         DC    C'P01593'           ON-CAMERA PRINCIPAL                          
         DC    C'P01588'           OTHER                                        
         EJECT                                                                  
*---------------------------------------------------------------------          
ITEM     NTR1                                                                   
                                                                                
         L     R3,DMCB                                L'ITEM,A(ITEM)            
         L     R6,DMCB+4                              Amount                    
         L     RF,0(R6)                                                         
         LTR   RF,RF                                                            
         JZ    XIT                                                              
                                                                                
         GOTO1 =A(TAGIT),DMCB,(L'TXMLIITS,TXMLIITS)   <Item>                    
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(R3)                                              
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLNATS,TXMLNATS)   <NetAmount>               
         GOTO1 =A(TAGAMT),DMCB,(R6)                                             
         GOTO1 =A(TAGIT),DMCB,(12,BLOCK)                                        
         GOTO1 =A(TAGIT),DMCB,(L'TXMLNATE,TXMLNATE)   </NetAmount>              
         BRAS  RE,PUTTAPE                                                       
                                                                                
         GOTO1 =A(TAGIT),DMCB,(L'TXMLTAT,TXMLTAT)     TaxAmount                 
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLQTY,TXMLQTY)     Quantity                  
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLMTLS,TXMLMTLS)   Material                  
                                                                                
         CLC   =C'1376',INAGY                         1376 WANTS SAME           
         JNE   ITEM2A05                                MATERIAL CODE            
         MVC   SVMATERL,=C'P03719'                     FOR ALL 4 ITEMS          
         CLI   INREUSE,C'R'                                                     
         JNE   ITEM2A05                                REUSE                    
         MVC   SVMATERL,=C'P03722'                     FOR ALL 4 ITEMS          
                                                                                
ITEM2A05 GOTO1 =A(TAGIT),DMCB,(L'SVMATERL,SVMATERL)                             
         GOTO1 =A(TAGIT),DMCB,(L'TXMLMTLE,TXMLMTLE)                             
         BRAS  RE,PUTTAPE                                                       
                                                                                
         GOTO1 =A(TAGIT),DMCB,(L'TXMLJOBS,TXMLJOBS)   <JobNumber>               
                                                                                
         CLI   TSTOPT,C'Y'                                                      
         JE    ITEM2A10                                                         
         GOTO1 =A(TAGIT),DMCB,(L'INEST,INEST)                                   
         J     ITEM2A15                                                         
ITEM2A10 GOTO1 =A(TAGIT),DMCB,(15,=C'B1162-000007-00')    TEST                  
*TEM2A10 GOTO1 =A(TAGIT),DMCB,(15,=C'B1111-001267-00')    TEST                  
ITEM2A15 GOTO1 =A(TAGIT),DMCB,(L'TXMLJOBE,TXMLJOBE)   </JobNumber>              
                                                                                
         BRAS  RE,PUTTAPE                                                       
                                                                                
         GOTO1 =A(TAGIT),DMCB,(L'TXMLPON2,TXMLPON2)   PONumber                  
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLPOI,TXMLPOI)     POItem                    
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLERF,TXMLERF)     EmployeeReference         
         BRAS  RE,PUTTAPE                                                       
                                                                                
         GOTO1 =A(TAGIT),DMCB,(L'TXMLTCTS,TXMLTCTS)   CID                       
         GOTO1 =A(TAGIT),DMCB,(L'INCID,INCID)                                   
         GOTO1 =A(TAGIT),DMCB,(L'TXMLTCTE,TXMLTCTE)                             
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLTC2S,TXMLTC2S)   Title                     
         GOTO1 =A(TAGIT),DMCB,(L'INTITLE,INTITLE)                               
         GOTO1 =A(TAGIT),DMCB,(L'TXMLTC2E,TXMLTC2E)                             
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLTCES,TXMLTCES)   Cycle End                 
         GOTO1 DATCON,DMCB,(1,SVCYCEND),(20,OUTDATE)                            
         MVC   BLOCK(2),OUTDATE+6                     DDMMYYYY                  
         MVC   BLOCK+2(2),OUTDATE+4                                             
         MVC   BLOCK+4(4),OUTDATE                                               
         GOTO1 =A(TAGIT),DMCB,(8,BLOCK)                                         
         GOTO1 =A(TAGIT),DMCB,(L'TXMLTCEE,TXMLTCEE)                             
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLTCSS,TXMLTCSS)   Cycle Start               
         GOTO1 DATCON,DMCB,(1,SVCYCSTR),(20,OUTDATE)                            
         MVC   BLOCK(2),OUTDATE+6                     DDMMYYYY                  
         MVC   BLOCK+2(2),OUTDATE+4                                             
         MVC   BLOCK+4(4),OUTDATE                                               
         GOTO1 =A(TAGIT),DMCB,(8,BLOCK)                                         
         GOTO1 =A(TAGIT),DMCB,(L'TXMLTCSE,TXMLTCSE)                             
         BRAS  RE,PUTTAPE                                                       
                                                                                
         GOTO1 =A(TAGIT),DMCB,(L'TXMLCVRC,TXMLCVRC)   CarVoucherRemit           
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLCVD,TXMLCVD)     CarVoucherDescr           
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLSTN,TXMLSTN)     SSRTrackingNumber         
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLSTD,TXMLSTD)     SSRTransactionDat         
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLSSH,TXMLSSH)     SSRShipper                
         BRAS  RE,PUTTAPE                                                       
         GOTO1 =A(TAGIT),DMCB,(L'TXMLSRC,TXMLSRC)     SSRRecipient              
         BRAS  RE,PUTTAPE                                                       
                                                                                
         GOTO1 =A(TAGIT),DMCB,(L'TXMLIITE,TXMLIITE)   </Item>                   
         BRAS  RE,PUTTAPE                                                       
         J     XIT                                                              
         EJECT                                                                  
                                                                                
         LTORG                                                                  
TXMLIITS DC    C'        <Item>'                                                
TXMLIITE DC    C'        </Item>'                                               
TXMLITT1 DC    C'            <Text>02 ON 000</Text>'                            
TXMLITT2 DC    C'            <Text>REIMBURSEMENT</Text>'                        
TXMLITT3 DC    C'            <Text>PENSION and WELFARE</Text>'                  
TXMLITT4 DC    C'            <Text>PAYROLL TAX</Text>'                          
TXMLITT5 DC    C'            <Text>HANDLING</Text>'                             
TXMLNATS DC    C'            <NetAmount>'                                       
TXMLNATE DC    C'</NetAmount>'                                                  
TXMLTATS DC    C'            <TaxAmount>'                                       
TXMLTATE DC    C'</TaxAmount>'                                                  
TXMLTAT  DC    C'            <TaxAmount>0</TaxAmount>'                          
TXMLQTY  DC    C'            <Quantity>1</Quantity>'                            
TXMLMTLS DC    C'            <Material>'                                        
TXMLMTLE DC    C'</Material>'                                                   
TXMLJOBS DC    C'            <JobNumber>'                                       
TXMLJOBE DC    C'</JobNumber>'                                                  
TXMLPON2 DC    C'            <PONumber/>'                                       
TXMLPOI  DC    C'            <POItem/>'                                         
TXMLERF  DC    C'            <EmployeeReference/>'                              
TXMLTCTS DC    C'            <TalentCommercialTitle1>'                          
TXMLTCTE DC    C'</TalentCommercialTitle1>'                                     
TXMLTC2S DC    C'            <TalentCommercialTitle2>'                          
TXMLTC2E DC    C'</TalentCommercialTitle2>'                                     
TXMLTCES DC    C'            <TalentCycleEndDate>'                              
TXMLTCEE DC    C'</TalentCycleEndDate>'                                         
TXMLTCSS DC    C'            <TalentCycleStartDate>'                            
TXMLTCSE DC    C'</TalentCycleStartDate>'                                       
                                                                                
TXMLCVRC DC    C'            <CarVoucherRemitComment/>'                         
TXMLCVD  DC    C'            <CarVoucherDescription/>'                          
TXMLSTN  DC    C'            <SSRTrackingNumber/>'                              
TXMLSTD  DC    C'            <SSRTransactionDate/>'                             
TXMLSSH  DC    C'            <SSRShipper/>'                                     
TXMLSRC  DC    C'            <SSRRecipient/>'                                   
         EJECT                                                                  
*======================================================================         
PXMLTRLR NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO2                                                          
         BRAS  RE,CLRXMLTG                                                      
                                                                                
         GOTO1 =A(TAGIT),DMCB,(L'TXMLINVE,TXMLINVE)   </Invoices>               
         BRAS  RE,PUTTAPE                                                       
         J     XIT                                                              
         LTORG                                                                  
TXMLINVE DC    C'</Invoices>'                                                   
         EJECT                                                                  
*======================================================================         
CLRXMLTG NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,PX                                                        
         JNE   XIT                                                              
         L     R4,AIO2                                                          
         MVI   0(R4),C' '                                                       
         MVC   1(167,R4),0(R4)                                                  
         J     XIT                                                              
*======================================================================         
*              THIS ROUTINE PUTS THE XML TAGS WHERE R4 IS                       
*                   R4 IS ALSO UPDATED                                          
*              P1 = (TAG LENGTH, TAG)                                           
*======================================================================         
TAGIT    NTR1  BASE=*,LABEL=*                                                   
         L     R1,DMCB             A(XML TAG)                                   
         SR    R2,R2                                                            
         IC    R2,DMCB                                                          
         AHI   R2,-1                                                            
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R1)                                                    
         BAS   RE,SPCLCHR                                                       
                                                                                
         AR    R4,R2               TO LAST CHAR                                 
                                                                                
TAGIT3   CLI   0(R4),C' '          TRIMS OFF TRAILING SPACES                    
         BH    TAGIT9                                                           
         AHI   R4,-1                                                            
         B     TAGIT3                                                           
                                                                                
TAGIT9   AHI   R4,1                                                             
                                                                                
         XIT1  REGS=(R4)                                                        
         SPACE 3                                                                
SPCLCHR  NTR1                                                                   
         AR    R4,R2                                                            
         AHI   R2,1                                                             
SCHR010  CLI   0(R4),C'&&'         REPLACE & WITH +                             
         BNE   *+8                                                              
         MVI   0(R4),C'+'                                                       
         AHI   R4,-1                                                            
         BCT   R2,SCHR010                                                       
         J     XIT                                                              
         EJECT                                                                  
*======================================================================         
TAGAMT   NTR1  BASE=*,LABEL=*                                                   
         MVC   BLOCK(12),MYSPACES                                               
         LA    R3,BLOCK                                                         
         L     R2,DMCB                                                          
         OC    0(4,R2),0(R2)       ZERO?                                        
         BNE   TAGAMT10                                                         
         MVC   BLOCK(4),=C'0.00'                                                
         J     XIT                                                              
TAGAMT10 CLC   0(4,R2),=X'00000064'        IF LESS THAN 1.00                    
         JL    TAGAMT20                                                         
         CLC   0(4,R2),=X'FFFFFF9D'        NEGATIVE TOO                         
         JL    TAGAMT50                                                         
TAGAMT20 MVI   BLOCK,C'0'                  PUT LEADING ZERO                     
         AHI   R3,1                                                             
TAGAMT50 EDIT  (B4,0(R2)),(12,(R3)),2,ALIGN=LEFT,MINUS=YES                      
         J     XIT                                                              
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
FNDMTRL  NTR1  BASE=*,LABEL=*                                                   
         MVC   SVMATERL,=C'P01588'    DEFAULT MATERIAL CODE                     
         CLI   GENMTRS,C'Y'                                                     
         BNE   FMTRL050                                                         
         MVC   SVMATERL,=C'P01590'    GENERAL MOTORS DEF MATERIAL CODE          
         J     XIT                                                              
                                                                                
FMTRL050 L     R2,0(R1)            USE CODE                                     
         LA    R4,MATRLMAP                                                      
FMTRL100 CLI   0(R4),X'FF'                                                      
         JE    XIT                                                              
         CLC   0(3,R2),0(R4)                                                    
         JE    FMTRL900                                                         
         AHI   R4,9                                                             
         J     FMTRL100                                                         
                                                                                
FMTRL900 MVC   SVMATERL,3(R4)                                                   
         J     XIT                                                              
                                                                                
MATRLMAP DC    C'_PM',C'P01588'                                                 
         DC    C'ACB',C'P03691'                                                 
         DC    C'ADC',C'P03719'                                                 
         DC    C'ADD',C'P03694'                                                 
         DC    C'ADH',C'P03699'                                                 
         DC    C'ADO',C'P03719'                                                 
         DC    C'ADT',C'P03719'                                                 
         DC    C'ADW',C'P03721'                                                 
         DC    C'ALF',C'P03719'                                                 
         DC    C'ARN',C'P03719'                                                 
         DC    C'ARR',C'P03719'                                                 
         DC    C'ARS',C'P03719'                                                 
         DC    C'AUD',C'P03690'                                                 
         DC    C'BSC',C'P03719'                                                 
         DC    C'BSM',C'P03719'                                                 
         DC    C'BSR',C'P03719'                                                 
         DC    C'BSS',C'P03719'                                                 
         DC    C'BSU',C'P03719'                                                 
         DC    C'CAU',C'P03690'                                                 
         DC    C'CBL',C'P03691'                                                 
         DC    C'CDM',C'P03694'                                                 
         DC    C'CLA',C'P03692'                                                 
         DC    C'CMR',C'P03707'                                                 
         DC    C'CMS',C'P03719'                                                 
         DC    C'CNL',C'P03719'                                                 
         DC    C'CNM',C'P01588'                                                 
         DC    C'CPN',C'P03713'                                                 
         DC    C'DEM',C'P03694'                                                 
         DC    C'DIO',C'P00099'                                                 
         DC    C'DLR',C'P03693'                                                 
         DC    C'DOR',C'P03707'                                                 
         DC    C'DWN',C'P03719'                                                 
         DC    C'EDS',C'P01078'                                                 
         DC    C'EQY',C'P01078'                                                 
         DC    C'EVE',C'P01078'                                                 
         DC    C'FGM',C'P03696'                                                 
         DC    C'FGR',C'P03697'                                                 
         DC    C'FGS',C'P03719'                                                 
         DC    C'FMU',C'P03695'                                                 
         DC    C'GRR',C'P03698'                                                 
         DC    C'GRT',C'P03698'                                                 
         DC    C'HLD',C'P03699'                                                 
         DC    C'IFB',C'P03711'                                                 
         DC    C'IFS',C'P03711'                                                 
         DC    C'IHM',C'P01588'                                                 
         DC    C'IMS',C'P03719'                                                 
         DC    C'INA',C'P03700'                                                 
         DC    C'INF',C'P03711'                                                 
         DC    C'INR',C'P03720'                                                 
         DC    C'INS',C'P03719'                                                 
         DC    C'INU',C'P01588'                                                 
         DC    C'IRN',C'P03701'                                                 
         DC    C'ISS',C'P03719'                                                 
         DC    C'ISU',C'P01588'                                                 
         DC    C'ITN',C'P03702'                                                 
         DC    C'IVR',C'P00099'                                                 
         DC    C'LCB',C'P03704'                                                 
         DC    C'LFT',C'P03719'                                                 
         DC    C'LNA',C'P03703'                                                 
         DC    C'LNC',C'P03703'                                                 
         DC    C'LNF',C'P03703'                                                 
         DC    C'LNN',C'P03703'                                                 
         DC    C'LOC',C'P03711'                                                 
         DC    C'MRR',C'P03719'                                                 
         DC    C'MUS',C'P03707'                                                 
         DC    C'MVI',C'P03705'                                                 
         DC    C'MVM',C'P00922'                                                 
         DC    C'MVN',C'P03706'                                                 
         DC    C'NBM',C'P03710'                                                 
         DC    C'NBS',C'P03719'                                                 
         DC    C'NET',C'P03711'                                                 
         DC    C'NIM',C'P00922'                                                 
         DC    C'NMR',C'P03709'                                                 
         DC    C'NMU',C'P01588'                                                 
         DC    C'OTC',C'P03711'                                                 
         DC    C'OTH',C'P03711'                                                 
         DC    C'OTM',C'P03711'                                                 
         DC    C'PAX',C'P03712'                                                 
         DC    C'PBS',C'P03714'                                                 
         DC    C'PEM',C'P03713'                                                 
         DC    C'PEN',C'P03713'                                                 
         DC    C'PNH',C'P01837'                                                 
         DC    C'PNP',C'P03711'                                                 
         DC    C'PPF',C'P03719'                                                 
         DC    C'PRG',C'P03711'                                                 
         DC    C'PRL',C'P03711'                                                 
         DC    C'PRM',C'P03719'                                                 
         DC    C'PRR',C'P03711'                                                 
         DC    C'PRS',C'P00921'                                                 
         DC    C'PRT',C'P01588'                                                 
         DC    C'PUB',C'P03719'                                                 
         DC    C'RAD',C'P03711'                                                 
         DC    C'REN',C'P03699'                                                 
         DC    C'RET',C'P03711'                                                 
         DC    C'RGM',C'P03707'                                                 
         DC    C'RLO',C'P03711'                                                 
         DC    C'RNT',C'P03708'                                                 
         DC    C'RRN',C'P03708'                                                 
         DC    C'RRR',C'P03719'                                                 
         DC    C'RRS',C'P03719'                                                 
         DC    C'SCB',C'P03691'                                                 
         DC    C'SCN',C'P03719'                                                 
         DC    C'SCS',C'P03719'                                                 
         DC    C'SFR',C'P03697'                                                 
         DC    C'SFS',C'P03719'                                                 
         DC    C'SHL',C'P03699'                                                 
         DC    C'SIN',C'P03720'                                                 
         DC    C'SIR',C'P03701'                                                 
         DC    C'SIU',C'P01588'                                                 
         DC    C'SLF',C'P03719'                                                 
         DC    C'SMI',C'P03715'                                                 
         DC    C'SMN',C'P03716'                                                 
         DC    C'SMU',C'P03707'                                                 
         DC    C'SNA',C'P03694'                                                 
         DC    C'SNM',C'P03718'                                                 
         DC    C'SNT',C'P03717'                                                 
         DC    C'SNU',C'P01588'                                                 
         DC    C'SNW',C'P03711'                                                 
         DC    C'SOT',C'P03711'                                                 
         DC    C'SPN',C'P03713'                                                 
         DC    C'SRE',C'P03719'                                                 
         DC    C'SRS',C'P03719'                                                 
         DC    C'SSS',C'P03719'                                                 
         DC    C'STR',C'P01588'                                                 
         DC    C'SWS',C'P03721'                                                 
         DC    C'TAG',C'P03719'                                                 
         DC    C'ULM',C'P03711'                                                 
         DC    C'VAR',C'P03711'                                                 
         DC    C'VNR',C'P01078'                                                 
         DC    C'VNW',C'P03711'                                                 
         DC    C'VRE',C'P01078'                                                 
         DC    C'WSC',C'P03721'                                                 
         DC    C'WSM',C'P03721'                                                 
         DC    C'WSP',C'P03721'                                                 
         DC    X'FFFFFF'                                                        
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              TASYSEQUS                                                        
*              TASYSDSECT                                                       
         PRINT OFF                                                              
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
         PRINT ON                                                               
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
MYSORTER DS    A                                                                
DYNALLOC DS    A                                                                
CANRATE  DS    F                                                                
         SPACE 1                                                                
*                                  OPTIONS                                      
TAPEOPT  DS    CL1                 Y=GENERATE OUTPUT TAPE                       
SORTOPT  DS    CL1                                                              
TRACOPT  DS    CL1                 Y=TRACE                                      
LISTOPT  DS    CL1                 Y=LIST AND COUNT ONLY                        
SAATOPT  DS    CL1                 Y=SAATCHI SUB CATEGORIES                     
TSTOPT   DS    CL1                 TEST FILE FOR PUBXML                         
WAV2OPT  DS    CL1                 WAVE2 OPTION FOR PUBXML                      
GENMTRS  DS    CL1                 Y=GENERAL MOTORS                             
         DS    CL3                 SPARE                                        
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
THISCAM  DS    CL3                                                              
THISUN   DS    CL3                                                              
                                                                                
ALLTOT   DS    F                                                                
ALLREXP  DS    F                                                                
ALLPNH   DS    F                                                                
ALLTAX   DS    F                                                                
ALLHAND  DS    F                                                                
ALLTAXX  DS    F                                                                
ALLHANDX DS    F                                                                
ALLCSF   DS    F                                                                
ALLPAY   DS    F                                                                
ALLAPPLY DS    F                                                                
TOTGROSS DS    PL8                                                              
*                                WAVE 2 SESSION AMOUNTS                         
SEVOICEO DS    F                 VOICE OVER                                     
SEHANDMD DS    F                 HAND MODEL                                     
SEEXTRAS DS    F                 EXTRAS                                         
SESINGER DS    F                 SINGERS                                        
SEMUSICN DS    F                 MUSICIANS                                      
SEPRTMDL DS    F                 PRINT MODEL                                    
SECREW   DS    F                 CREW                                           
SEONCPRN DS    F                 ON-CAMERA PRINCIPAL                            
SEOTHER  DS    F                 OTHER                                          
SESSAMTS EQU   *-SEVOICEO                                                       
*                                                                               
AMASTD   DS    A                 A(MASTER)                                      
ALOGOC   DS    A                 A(LOGOC)                                       
ALOGO    DS    A                 A(LOGO)                                        
AREMOT   DS    A                 A(REMOTE)                                      
DLBLOCK  DS    CL(DLCBXLX)                                                      
*                                                                               
SDBLOCK  DS    CL(SOFXTNL)       SOFTDATE BLOCK                                 
OUTDATE  DS    CL12                                                             
*                                                                               
BDCATFLG DS    CL1                 Y = BAD CATEGORY FLAG                        
*                                                                               
MYWORK   DS    CL16                                                             
SVLEN    DS    XL2               SAVED ESTIMATE LENGTH                          
SVINDATE DS    XL3                                                              
SVCYCSTR DS    XL3                                                              
SVCYCEND DS    XL3                                                              
SVMATERL DS    CL6               MATERIAL CODE                                  
*                                                                               
         DS    0D                                                               
TAPEIO   DS    646C                                                             
         SPACE 1                                                                
MYEND    DS    0D                                                               
         EJECT                                                                  
               DSECT TO COVER SUB CATEGORY TABLE                                
         SPACE 3                                                                
SUBTABD  DSECT                                                                  
SUBCAT   DS    CL3                                                              
SUBSUB   DS    CL2                                                              
SUBLNQ   EQU   *-SUBTABD                                                        
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
*              DSECT TO COVER TAPE RECORDS                                      
         SPACE 3                                                                
TAPED    DSECT                                                                  
TAPEREC  DS    0C                  RECORD LENGTH OF 645                         
INREC    DS    0C                                                               
INAGY    DS    CL4                                                              
INBRAND  DS    CL4                                                              
INREUSE  DS    CL1                                                              
INEST    DS    CL16                                                             
INESTPER DS    CL2                                                              
INPRMON  DS    CL1                                                              
INTITLE  DS    CL30                                                             
INCID    DS    CL10                                                             
INCYCS   DS    CL6                                                              
INCYCE   DS    CL6                                                              
INUSE    DS    CL15                                                             
INUSEDET DS    CL15                                                             
ININV    DS    CL6                                                              
INTAX    DS    CL15                                                             
INHAND   DS    CL15                                                             
INAPLCOD DS    CL1                                                              
INAPLAMT DS    CL15                                                             
INTOT    DS    CL15                                                             
INPNH    DS    CL15                                                             
INPO     DS    CL10                                                             
*                                                                               
INENTRY  DS    0CL27               UP TO 15 PEOPLE ENTRIES                      
INCAT    DS    CL7                    CATEGORY                                  
INCOUNT  DS    CL2                    COUNT                                     
INOVPCT  DS    CL3                    OVERSCALE PERCENT                         
INPEOAMT DS    CL15                   AMOUNT FOR ABOVE                          
*                                                                               
         DS    (14*L'INENTRY)C     THE OTHER 14 PEOPLE ENTRY                    
*                                                                               
INCLI    DS    CL6                 CLIENT                                       
INPRD    DS    CL6                 CLIENT                                       
INUSECD  DS    CL3                 USE CODE                                     
ININVDT  DS    CL8                 INVOICE DATE                                 
*                                                                               
INCSF    DS    CL15                COMMERCIAL SERVICE FEE                       
*                                                                               
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
**PAN#1  DC    CL21'061TAREP28   10/04/16'                                      
         END                                                                    
