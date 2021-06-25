*          DATA SET ACLDXPRNT  AT LEVEL 184 AS OF 12/22/97                      
*PHASE ACLDPRNT,*                                                               
*INCLUDE ACRECTYP                                                               
*INCLUDE DATCON                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'ACLDPRNT EXTENAL'                                               
         SPACE 2                                                                
***********************************************************************         
* PRINT RECORDS FROM A FILE                                           *         
***********************************************************************         
         SPACE 1                                                                
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
ACLDPRNT CSECT                                                                  
         NMOD1 WORKX-WORKD,ACLDPRNT                                             
         USING WORKD,RC                                                         
         EJECT                                                                  
***********************************************************************         
* CONTROL FLOW LOGIC                                                  *         
***********************************************************************         
         SPACE 1                                                                
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST(PLISTL),0(R1)                                              
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXRET   EQU   *                                                                
         B     DMXIT                                                            
*                                                                               
DMXINIT  ZAP   LINE,MAXLINE                                                     
         B     DMXIT                                                            
*                                                                               
DMXEOF   EQU   *                                                                
         B     DMXIT                                                            
*                                                                               
DMXIT    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* PROCESS A RECORD                                                    *         
***********************************************************************         
                                                                                
DMXREC   L     R2,AREC             R2=A(INPUT RECORD)                           
         USING ACTRECD,R2                                                       
         GOTO1 VRECTYP,DMCB,(C'D',ACTRECD)                                      
         MVC   RECTYPE,0(R1)       RECORD TYPE                                  
         MVC   COMPANY,1(R1)       COMPANY CODE                                 
         MVC   COMPDSP,2(R1)       DISPLACEMENT TO COMPANY IN KEY               
                                                                                
         LA    R4,RECTAB           POINT TO RECORD TABLE                        
         USING RECTABD,R4                                                       
DMXREC02 CLI   RECTREC,RECTEOTQ    END OF TABLE?                                
         BE    DMXREC04                                                         
         CLC   RECTREC,RECTYPE                                                  
         BE    DMXREC04                                                         
         LA    R4,RECTABL(R4)      MOVE TO NEXT TABLE ENTRY                     
         B     DMXREC02                                                         
DMXREC04 GOTO1 HEXPRT,DMCB,RECTNAM,KEYLEN,ACTKEY                                
         GOTO1 VPRINTER                                                         
                                                                                
         LA    R3,ACTRFST          R3=A(FIRST ELEMENT ON RECORD)                
         USING ELD,R3                                                           
         SR    R0,R0                                                            
DMXREC06 CLI   ELCOD,0             END OF RECORD?                               
         BE    DMXREC12            EXIT FOR NEXT RECORD                         
         LA    R4,ELETAB           POINT TO START OF ELEMENT TABLE              
         USING ELETABD,R4                                                       
DMXREC08 CLI   ELETEL,ELETEOTQ     END OF TABLE?                                
         BE    DMXREC10                                                         
         CLC   ELCOD,ELETEL        MATCH ELEMENT CODE TO TABLE                  
         BE    DMXREC10                                                         
         LA    R4,ELETABL(R4)      MOVE TO NEXT TABLE ENTRY                     
         B     DMXREC08                                                         
DMXREC10 IC    R0,ELLEN            R0=ELEMENT LENGTH                            
         GOTO1 HEXPRT,DMCB,ELETNAM,(R0),ELD                                     
         AR    R3,R0               MOVE TO NEXT ELEMENT                         
         B     DMXREC06                                                         
                                                                                
DMXREC12 MVI   P,C'*'              FILL PLINE WITH *'S                          
         MVC   P+1(L'P-1),P        FILL WITH *'S                                
         GOTO1 VPRINTER            THE STAR LINE                                
         B     DMXKEEP                                                          
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT CHARACTER AND HEX FORMAT                           *         
*                                                                     *         
* P1     -     A(8 CHARACTER NAME)                                    *         
* P2     -     WIDTH OF THE DATA                                      *         
* P3     -     A(DATA)                                                *         
***********************************************************************         
                                                                                
HEXPRT   NTR1  ,                   PRINTING ROUTINE                             
         LM    R2,R4,0(R1)                                                      
         MVC   P+1(L'RECTNAM),0(R2)                                             
*                                                                               
HEXPRT02 LR    R2,R3                                                            
         CH    R2,=Y(MAXWIDTH)                                                  
         BNH   *+8                                                              
         LH    R2,=Y(MAXWIDTH)     R2=WIDTH OF PRINTING THIS LINE               
         BCTR  R2,0                                                             
         EX    R2,*+4                                                           
         MVC   P+20(0),0(R4)       MOVE DATA TO PRINT LINE                      
         EX    R2,*+4                                                           
         TR    P+20(0),OUTTAB      TRANSLATE TO PRINTABLE                       
         ZAP   DUB,LINE                                                         
*        AP    LINE,=P'3'                                                       
*        CP    LINE,MAXLINE                                                     
         AP    DUB,=P'3'                                                        
         CP    DUB,MAXLINE                                                      
         BNH   *+10                                                             
         ZAP   LINE,MAXLINE                                                     
         GOTO1 VPRINTER                                                         
         LA    R2,1(R2)                                                         
         GOTO1 VHEXOUT,DMCB,(R4),HEXWORK,(R2),SEP                               
         BCTR  R2,0                                                             
         EX    R2,*+4                                                           
         MVC   P+20(0),HEXWORK     MOVE ZONES HALF TO PLINE                     
         GOTO1 VPRINTER                                                         
         LA    RF,HEXWORK+1(R2)                                                 
         EX    R2,*+4                                                           
         MVC   P+20(0),0(RF)       MOVE NUMERIC HALF TO PLINE                   
         GOTO1 VPRINTER                                                         
         LA    R2,1(R2)                                                         
         AR    R4,R2               POINT TO NEXT INPUT CHUNK                    
         SR    R3,R2               DECREMENT DATA WIDTH                         
         BP    HEXPRT02                                                         
HEXPRTX  B     DMXIT                                                            
         EJECT                                                                  
         LTORG                                                                  
                                                                                
VRECTYP  DC    V(ACRECTYP)                                                      
VHELLO   DC    V(HELLO)                                                         
VHEXIN   DC    V(HEXIN)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VDATCON  DC    V(DATCON)                                                        
ACCMST   DC    C'ACCMST '                                                       
SEP      DC    C'SEP'              HEXOUT PARAMETER                             
MAXWIDTH EQU   60                  PRINT WIDTH                                  
KEYLEN   EQU   ACTRFST-ACTKEY                                                   
         EJECT                                                                  
RECTAB   DS    0X                  RECORD TYPE LOOK UP TABLE                    
         DC    AL1(ACRTUNKN),CL8'ACRTUNKN'                                      
         DC    AL1(ACRTCPY),CL8'ACRTCPY '                                       
         DC    AL1(ACRTUNT),CL8'ACRTUNT '                                       
         DC    AL1(ACRTLDG),CL8'ACRTLDG '                                       
         DC    AL1(ACRTACTH),CL8'ACRTACTH'                                      
         DC    AL1(ACRTACTL),CL8'ACRTACTL'                                      
         DC    AL1(ACRTOFA),CL8'ACRTOFA '                                       
         DC    AL1(ACRTCHDP),CL8'ACRTCHDP'                                      
         DC    AL1(ACRTCHDH),CL8'ACRTCHDH'                                      
         DC    AL1(ACRTCAC),CL8'ACRTCAC '                                       
         DC    AL1(ACRTTRN),CL8'ACRTTRN '                                       
         DC    AL1(ACRTTRNA),CL8'ACRTTRNA'                                      
         DC    AL1(ACRTOTHR),CL8'ACRTOTHR'                                      
         DC    AL1(ACRTOFF),CL8'ACRTOFF'                                        
         DC    AL1(ACRTMIN),CL8'ACRTMIN'                                        
         DC    AL1(ACRTPMD),CL8'ACRTPMD'                                        
         DC    AL1(ACRTWCO),CL8'ACRTWCO'                                        
         DC    AL1(ACRTBAT),CL8'ACRTBAT'                                        
         DC    AL1(ACRTSCM),CL8'ACRTSCM'                                        
         DC    AL1(ACRTORD),CL8'ACRTORD'                                        
         DC    AL1(ACRTBUD),CL8'ACRTBUD'                                        
         DC    AL1(ACRTLST),CL8'ACRTLST'                                        
         DC    AL1(ACRTFEEC),CL8'ACRTFEEC'                                      
         DC    AL1(ACRTFEEA),CL8'ACRTFEEA'                                      
         DC    AL1(ACRTFEEP),CL8'ACRTFEEP'                                      
         DC    AL1(ACRTPCR),CL8'ACRTPCR'                                        
         DC    AL1(ACRTPRL),CL8'ACRTPRL'                                        
         DC    AL1(ACRTSBL),CL8'ACRTSBL'                                        
         DC    AL1(ACRTOGRG),CL8'ACRTOGRG'                                      
         DC    AL1(ACRTOGRO),CL8'ACRTOGRO'                                      
         DC    AL1(ACRTMGR),CL8'ACRTMGR'                                        
         DC    AL1(ACRTWGR),CL8'ACRTWGR'                                        
         DC    AL1(ACRTUFS),CL8'ACRTUFS'                                        
         DC    AL1(ACRTPOP),CL8'ACRTPOP'                                        
         DC    AL1(ACRTSCH),CL8'ACRTSCH'                                        
         DC    AL1(ACRTCAT),CL8'ACRTCAT'                                        
         DC    AL1(ACRTPAN),CL8'ACRTPAN'                                        
         DC    AL1(ACRTEVE),CL8'ACRTEVE'                                        
         DC    AL1(ACRTTXT),CL8'ACRTTXT'                                        
         DC    AL1(ACRTSUT),CL8'ACRTSUT'                                        
         DC    AL1(ACRTRES),CL8'ACRTRES'                                        
         DC    AL1(ACRTINT),CL8'ACRTINT'                                        
         DC    AL1(ACRTIDJ),CL8'ACRTIDJ'                                        
         DC    AL1(ACRTRAT),CL8'ACRTRAT'                                        
         DC    AL1(ACRTDAT),CL8'ACRTDAT'                                        
         DC    AL1(ACRTTAX),CL8'ACRTTAX'                                        
         DC    AL1(ACRTNBT),CL8'ACRTNBT'                                        
         DC    AL1(ACRTNBP),CL8'ACRTNBP'                                        
         DC    AL1(ACRTBSC),CL8'ACRTBSC'                                        
         DC    AL1(ACRTMPD),CL8'ACRTMPD'                                        
         DC    AL1(ACRTMPR),CL8'ACRTMPR'                                        
         DC    AL1(ACRTPAR),CL8'ACRTPAR'                                        
         DC    AL1(ACRTGRB),CL8'ACRTGRB'                                        
         DC    AL1(ACRTOAP),CL8'ACRTOAP'                                        
         DC    AL1(ACRTANC),CL8'ACRTANC'                                        
         DC    AL1(ACRTAJN),CL8'ACRTAJN'                                        
         DC    AL1(ACRTSRM),CL8'ACRTSRM'                                        
         DC    AL1(ACRTPBA),CL8'ACRTPBA'                                        
         DC    AL1(ACRTPBP),CL8'ACRTPBP'                                        
         DC    AL1(ACRTPTA),CL8'ACRTPTA'                                        
         DC    AL1(ACRTMRH),CL8'ACRTMRH'                                        
         DC    AL1(ACRTSES),CL8'ACRTSES'                                        
         DC    AL1(ACRTCKA),CL8'ACRTCKA'                                        
         DC    AL1(ACRTSRC),CL8'ACRTSRC'                                        
         DC    AL1(ACRTADV),CL8'ACRTADV'                                        
         DC    AL1(ACRTACP),CL8'ACRTACP'                                        
         DC    AL1(ACRTAGR),CL8'ACRTAGR'                                        
         DC    AL1(ACRTAGP),CL8'ACRTAGP'                                        
         DC    AL1(ACRTRAP),CL8'ACRTRAP'                                        
         DC    AL1(ACRTJCB),CL8'ACRTJCB'                                        
         DC    AL1(ACRTSTU),CL8'ACRTSTU'                                        
         DC    AL1(ACRTPRC),CL8'ACRTPRC'                                        
         DC    AL1(ACRTSPO),CL8'ACRTSPO'                                        
         DC    AL1(ACRTAPO),CL8'ACRTAPO'                                        
         DC    AL1(ACRTKWD),CL8'ACRTKWD'                                        
         DC    AL1(ACRTCASP),CL8'ACRTCASP'                                      
         DC    AL1(ACRTPID),CL8'ACRTPID'                                        
         DC    AL1(ACRTCCP),CL8'ACRTCCP'                                        
         DC    AL1(ACRTAPG),CL8'ACRTAPG'                                        
         DC    AL1(ACRTHDRA),CL8'ACRTHDRA'                                      
         DC    AL1(ACRTHDRB),CL8'ACRTHDRB'                                      
         DC    AL1(ACRTTRLA),CL8'ACRTTRLA'                                      
         DC    AL1(ACRTTRLB),CL8'ACRTTRLB'                                      
         DC    AL1(ACRTPER),CL8'ACRTPER'                                        
         DC    AL1(ACRTCAH),CL8'ACRTCAH'                                        
         DC    AL1(ACRTCMT),CL8'ACRTCMT'                                        
         DC    AL1(ACRTPAY),CL8'ACRTPAY'                                        
         DC    AL1(ACRTPHI),CL8'ACRTPHI'                                        
         DC    AL1(ACRTCPR),CL8'ACRTCPR'                                        
         DC    AL1(ACRTCAP),CL8'ACRTCAP'                                        
         DC    AL1(ACRTCAS),CL8'ACRTCAS'                                        
         DC    AL1(ACRTSTD),CL8'ACRTSTD'                                        
         DC    AL1(ACRTPBC),CL8'ACRTPBC'                                        
         DC    AL1(ACRTTIM),CL8'ACRTTIM'                                        
         DC    AL1(ACRTTTH),CL8'ACRTTTH'                                        
         DC    AL1(ACRTTSL),CL8'ACRTTSL'                                        
         DC    AL1(ACRTGIN),CL8'ACRTGIN'                                        
         DC    AL1(ACRTBDP),CL8'ACRTBDP'                                        
         DC    AL1(ACRTTSW),CL8'ACRTTSW'                                        
         DC    AL1(ACRTEDT),CL8'ACRTEDT'                                        
         DC    AL1(ACRTORES),CL8'ACRTORES'                                      
         DC    AL1(ACRTSSAV),CL8'ACRTSSAV'                                      
         DC    AL1(ACRTTPOX),CL8'ACRTTPOX'                                      
         DC    AL1(ACRTCONT),CL8'ACRTCONT'                                      
         DC    AL1(ACRTCONP),CL8'ACRTCONP'                                      
         DC    AL1(ACRTINV),CL8'ACRTINV'                                        
         DC    AL1(ACRTMOS),CL8'ACRTMOS'                                        
         DC    AL1(ACRTPCRT),CL8'ACRTPCRT'                                      
         DC    AL1(ACRTRSF),CL8'ACRTRSF'                                        
*&&UK                                                                           
         DC    AL1(ACRTOCO),CL8'ACRTOCO'                                        
         DC    AL1(ACRTTCP),CL8'ACRTTCP'                                        
         DC    AL1(ACRTGBC),CL8'ACRTGBC'                                        
*&&                                                                             
RECTABX  DC    AL1(RECTEOTQ),CL8'UNKNOWN '                                      
RECTABN  EQU   (*-RECTAB)/RECTABL   NUMBER OF RECORDS                           
         SPACE 1                                                                
ELETAB   DS    0X                  ELEMENT LOOK UP TABLE                        
         DC    AL1(FHDELQ),CL8'FHDEL'                                           
         DC    AL1(HDRELQ),CL8'HDREL'                                           
         DC    AL1(BTHELQ),CL8'BTHEL'                                           
         DC    AL1(BTIELQ),CL8'BTIEL'                                           
         DC    AL1(GINELQ),CL8'GINEL'                                           
         DC    AL1(BROELQ),CL8'BROEL'                                           
         DC    AL1(CPYELQ),CL8'CPYEL'                                           
         DC    AL1(PMDELQ),CL8'PMDEL'                                           
         DC    AL1(WCOELQ),CL8'WCOEL'                                           
         DC    AL1(RTYELQ),CL8'RTYEL'                                           
         DC    AL1(LDGELQ),CL8'LDGEL'                                           
         DC    AL1(GLPELQ),CL8'GLPEL'                                           
         DC    AL1(ACLELQ),CL8'ACLEL'                                           
         DC    AL1(SHOELQ),CL8'SHOEL'                                           
         DC    AL1(MSLELQ),CL8'MSLEL'                                           
         DC    AL1(MDIELQ),CL8'MDIEL'                                           
         DC    AL1(MDTELQ),CL8'MDTEL'                                           
         DC    AL1(BCNELQ),CL8'BCNEL'                                           
         DC    AL1(BIVELQ),CL8'BIVEL'                                           
         DC    AL1(BAMELQ),CL8'BAMEL'                                           
         DC    AL1(LITELQ),CL8'LITEL'                                           
         DC    AL1(LIDELQ),CL8'LIDEL'                                           
         DC    AL1(NAMELQ),CL8'NAMEL'                                           
         DC    AL1(NUMELQ),CL8'NUMEL'                                           
         DC    AL1(ADRELQ),CL8'ADREL'                                           
         DC    AL1(OTHELQ),CL8'OTHEL'                                           
         DC    AL1(PPRELQ),CL8'PPREL'                                           
         DC    AL1(FFNELQ),CL8'FFNEL'                                           
         DC    AL1(JOBELQ),CL8'JOBEL'                                           
         DC    AL1(ABIELQ),CL8'ABIEL'                                           
         DC    AL1(XBPELQ),CL8'XBPEL'                                           
         DC    AL1(PMRELQ),CL8'PMREL'                                           
         DC    AL1(FTNELQ),CL8'FTNEL'                                           
         DC    AL1(RBRELQ),CL8'RBREL'                                           
         DC    AL1(BACELQ),CL8'BACEL'                                           
         DC    AL1(SPAELQ),CL8'SPAEL'                                           
         DC    AL1(ICPELQ),CL8'ICPEL'                                           
         DC    AL1(MBTELQ),CL8'MBTEL'                                           
         DC    AL1(MTPELQ),CL8'MTPEL'                                           
         DC    AL1(RSTELQ),CL8'RSTEL'                                           
         DC    AL1(ASTELQ),CL8'ASTEL'                                           
         DC    AL1(ABLELQ),CL8'ABLEL'                                           
         DC    AL1(APOELQ),CL8'APOEL'                                           
         DC    AL1(BUDELQ),CL8'BUDEL'                                           
         DC    AL1(ESTELQ),CL8'ESTEL'                                           
         DC    AL1(RATETAXQ),CL8'RATEL'                                         
         DC    AL1(RATEVATQ),CL8'RATEL'                                         
         DC    AL1(RATEDSCQ),CL8'RATEL'                                         
         DC    AL1(CEXELQ),CL8'CEXEL'                                           
         DC    AL1(PBAELQ),CL8'PBAEL'                                           
         DC    AL1(WPBELQ),CL8'WPBEL'                                           
         DC    AL1(XPRELQ),CL8'XPREL'                                           
         DC    AL1(SANELQ),CL8'SANEL'                                           
         DC    AL1(SCMELQ),CL8'SCMEL'                                           
         DC    AL1(OMEELQ),CL8'OMEEL'                                           
         DC    AL1(PRTELQ),CL8'PRTEL'                                           
         DC    AL1(DEXELQ),CL8'DEXEL'                                           
         DC    AL1(PRUELQ),CL8'PRUEL'                                           
         DC    AL1(CACELQ),CL8'CACEL'                                           
         DC    AL1(TRNELQ),CL8'TRNEL'                                           
         DC    AL1(BUKELQ),CL8'BUKEL'                                           
         DC    AL1(XPYELQ),CL8'XPYEL'                                           
         DC    AL1(MXPELQ),CL8'MXPEL'                                           
         DC    AL1(VBIELQ),CL8'VBIEL'                                           
         DC    AL1(RBIELQ),CL8'RBIEL'                                           
         DC    AL1(XBIELQ),CL8'XBIEL'                                           
         DC    AL1(BNDELQ),CL8'BNDEL'                                           
         DC    AL1(SPDELQ),CL8'SPDEL'                                           
         DC    AL1(TAUELQ),CL8'TAUEL'                                           
         DC    AL1(PXDELQ),CL8'PXDEL'                                           
         DC    AL1(CPJELQ),CL8'CPJEL'                                           
         DC    AL1(SCIELQ),CL8'SCIEL'                                           
         DC    AL1(PCIELQ),CL8'PCIEL'                                           
         DC    AL1(MSAELQ),CL8'MSAEL'                                           
         DC    AL1(TCIELQ),CL8'TCIEL'                                           
         DC    AL1(OCNELQ),CL8'OCNEL'                                           
         DC    AL1(PBKELQ),CL8'PBKEL'                                           
         DC    AL1(XNMELQ),CL8'XNMEL'                                           
         DC    AL1(EMPELQ),CL8'EMPEL'                                           
         DC    AL1(APTELQ),CL8'APTEL'                                           
         DC    AL1(MFCELQ),CL8'MFCEL'                                           
         DC    AL1(TPRELQ),CL8'TPREL'                                           
         DC    AL1(GPNELQ),CL8'GPNEL'                                           
         DC    AL1(GXBELQ),CL8'GXBEL'                                           
         DC    AL1(CUBELQ),CL8'CUBEL'                                           
         DC    AL1(BSAELQ),CL8'BSAEL'                                           
         DC    AL1(CALELQ),CL8'CALEL'                                           
         DC    AL1(SUTELQ),CL8'SUTEL'                                           
         DC    AL1(TRSELQ),CL8'TRSEL'                                           
         DC    AL1(DUEELQ),CL8'DUEEL'                                           
         DC    AL1(DSCELQ),CL8'DSCEL'                                           
         DC    AL1(HOMELQ),CL8'HOMEL'                                           
         DC    AL1(MPYELQ),CL8'MPYEL'                                           
         DC    AL1(ANOELQ),CL8'ANOEL'                                           
         DC    AL1(ONCELQ),CL8'ONCEL'                                           
         DC    AL1(ORDELQ),CL8'ORDEL'                                           
         DC    AL1(OAMELQ),CL8'OAMEL'                                           
         DC    AL1(FARELQ),CL8'FAREL'                                           
         DC    AL1(ADSELQ),CL8'ADSEL'                                           
         DC    AL1(ADIELQ),CL8'ADIEL'                                           
         DC    AL1(RRNELQ),CL8'RRNEL'                                           
         DC    AL1(ADEELQ),CL8'ADEEL'                                           
         DC    AL1(RTEELQ),CL8'RTEEL'                                           
         DC    AL1(MXCELQ),CL8'MXCEL'                                           
         DC    AL1(SNMELQ),CL8'SNMEL'                                           
         DC    AL1(TXBELQ),CL8'TXBEL'                                           
         DC    AL1(MRHELQ),CL8'MRHEL'                                           
         DC    AL1(SIDELQ),CL8'SIDEL'                                           
         DC    AL1(MRXELQ),CL8'MRXEL'                                           
         DC    AL1(TRXELQ),CL8'TRXEL'                                           
         DC    AL1(NOTELQ),CL8'NOTEL'                                           
         DC    AL1(PTAELQ),CL8'PTAEL'                                           
         DC    AL1(BLHELQ),CL8'BLHEL'                                           
         DC    AL1(PGHELQ),CL8'PGHEL'                                           
         DC    AL1(AFCELQ),CL8'AFCEL'                                           
         DC    AL1(SORELQ),CL8'SOREL'                                           
         DC    AL1(UNPELQ),CL8'UNPEL'                                           
         DC    AL1(BLFELQ),CL8'BLFEL'                                           
         DC    AL1(PBIELQ),CL8'PBIEL'                                           
         DC    AL1(BOFELQ),CL8'BOFEL'                                           
         DC    AL1(NDXELQ),CL8'NDXEL'                                           
         DC    AL1(PHRELQ),CL8'PHREL'                                           
         DC    AL1(DOAELQ),CL8'DOAEL'                                           
         DC    AL1(METELQ),CL8'METEL'                                           
         DC    AL1(LOCELQ),CL8'LOCEL'                                           
         DC    AL1(PAYELQ),CL8'PAYEL'                                           
         DC    AL1(PATELQ),CL8'PATEL'                                           
         DC    AL1(PDEELQ),CL8'PDEEL'                                           
         DC    AL1(TMRELQ),CL8'TMREL'                                           
         DC    AL1(TMPELQ),CL8'TMPEL'                                           
         DC    AL1(SHRELQ),CL8'SHREL'                                           
         DC    AL1(CEDELQ),CL8'CEDEL'                                           
         DC    AL1(TIMELQ),CL8'TIMEL'                                           
         DC    AL1(PTHELQ),CL8'PTHEL'                                           
         DC    AL1(MCHELQ),CL8'MCHEL'                                           
         DC    AL1(TSCELQ),CL8'TSCEL'                                           
         DC    AL1(VPDELQ),CL8'VPDEL'                                           
         DC    AL1(TRPELQ),CL8'TRPEL'                                           
         DC    AL1(CNTELQ),CL8'CNTEL'                                           
         DC    AL1(CTGELQ),CL8'CTGEL'                                           
         DC    AL1(CPOELQ),CL8'CPOEL'                                           
         DC    AL1(CXFELQ),CL8'CXFEL'                                           
         DC    AL1(OMJELQ),CL8'OMJEL'                                           
         DC    AL1(TSLELQ),CL8'TSLEL'                                           
         DC    AL1(TSDELQ),CL8'TSDEL'                                           
         DC    AL1(PGRELQ),CL8'PGREL'                                           
         DC    AL1(PACELQ),CL8'PACEL'                                           
         DC    AL1(UFSELQ),CL8'UFSEL'                                           
         DC    AL1(COIELQ),CL8'COIEL'                                           
         DC    AL1(OPDELQ),CL8'OPDEL'                                           
         DC    AL1(SCHELQ),CL8'SCHEL'                                           
         DC    AL1(SCSELQ),CL8'SCSEL'                                           
         DC    AL1(CADELQ),CL8'CADEL'                                           
         DC    AL1(CWKELQ),CL8'CWKEL'                                           
         DC    AL1(EUPELQ),CL8'EUPEL'                                           
         DC    AL1(ENAELQ),CL8'ENAEL'                                           
         DC    AL1(EAPELQ),CL8'EAPEL'                                           
         DC    AL1(EDAELQ),CL8'EDAEL'                                           
         DC    AL1(PPAELQ),CL8'PPAEL'                                           
         DC    AL1(FLDELQ),CL8'FLDEL'                                           
         DC    AL1(THDELQ),CL8'THDEL'                                           
         DC    AL1(TFDELQ),CL8'TFDEL'                                           
         DC    AL1(EPRELQ),CL8'EPREL'                                           
         DC    AL1(BESELQ),CL8'BESEL'                                           
         DC    AL1(MNAELQ),CL8'MNAEL'                                           
         DC    AL1(BDAELQ),CL8'BDAEL'                                           
         DC    AL1(JNAELQ),CL8'JNAEL'                                           
         DC    AL1(BNCELQ),CL8'BNCEL'                                           
*        DC    AL1(INCELQ),CL8'INCEL'                                           
         DC    AL1(BCYELQ),CL8'BCYEL'                                           
         DC    AL1(STUELQ),CL8'STUEL'                                           
         DC    AL1(LNKELQ),CL8'LNKEL'                                           
         DC    AL1(PRCELQ),CL8'PRCEL'                                           
         DC    AL1(EPTELQ),CL8'EPTEL'                                           
         DC    AL1(XTHELQ),CL8'XTHEL'                                           
         DC    AL1(XTCELQ),CL8'XTCEL'                                           
         DC    AL1(XTMELQ),CL8'XTMEL'                                           
         DC    AL1(APEELQ),CL8'APEEL'                                           
         DC    AL1(RHDELQ),CL8'RHDEL'                                           
         DC    AL1(RRWELQ),CL8'RRWEL'                                           
         DC    AL1(RCLELQ),CL8'RCLEL'                                           
         DC    AL1(RPFELQ),CL8'RPFEL'                                           
         DC    AL1(RFLELQ),CL8'RFLEL'                                           
         DC    AL1(IPRELQ),CL8'IPREL'                                           
         DC    AL1(IESELQ),CL8'IESEL'                                           
         DC    AL1(MNOELQ),CL8'MNOEL'                                           
         DC    AL1(JCBELQ),CL8'JCBEL'                                           
         DC    AL1(BSDELQ),CL8'BSDEL'                                           
         DC    AL1(CRDELQ),CL8'CRDEL'                                           
         DC    AL1(RQCELQ),CL8'RQCEL'                                           
         DC    AL1(OFIELQ),CL8'OFIEL'                                           
         DC    AL1(OFLELQ),CL8'OFLEL'                                           
         DC    AL1(OFAELQ),CL8'OFAEL'                                           
         DC    AL1(TIDELQ),CL8'TIDEL'                                           
         DC    AL1(MPGELQ),CL8'MPGEL'                                           
         DC    AL1(PIDELQ),CL8'PIDEL'                                           
         DC    AL1(RALELQ),CL8'RALEL'                                           
         DC    AL1(XXPELQ),CL8'XXPEL'                                           
         DC    AL1(FFTELQ),CL8'FFTEL'                                           
         DC    AL1(DATELQ),CL8'DATEL'                                           
         DC    AL1(AFPELQ),CL8'AFPEL'                                           
         DC    AL1(TAXIELQ),CL8'TAXEL'                                          
         DC    AL1(TAXOELQ),CL8'TAXEL'                                          
         DC    AL1(BHDELQ),CL8'BHDEL'                                           
         DC    AL1(SFSELQ),CL8'SFSEL'                                           
         DC    AL1(ASKELQ),CL8'ASKEL'                                           
         DC    AL1(BSCELQ),CL8'BSCEL'                                           
         DC    AL1(ITCELQ),CL8'ITCEL'                                           
         DC    AL1(GDAELQ),CL8'GDAEL'                                           
         DC    AL1(GLRELQ),CL8'GLREL'                                           
         DC    AL1(BICELQ),CL8'BICEL'                                           
         DC    AL1(BIAELQ),CL8'BIAEL'                                           
         DC    AL1(BIOELQ),CL8'BIOEL'                                           
         DC    AL1(LGLELQ),CL8'LGLEL'                                           
         DC    AL1(LOKELQ),CL8'LOKEL'                                           
         DC    AL1(WFMELQ),CL8'WFMEL'                                           
         DC    AL1(HLDELQ),CL8'HLDEL'                                           
         DC    AL1(ASIELQ),CL8'ASIEL'                                           
         DC    AL1(FFRELQ),CL8'FFREL'                                           
         DC    AL1(ALAELQ),CL8'ALAEL'                                           
         DC    AL1(AOLELQ),CL8'AOLEL'                                           
         DC    AL1(RVNELQ),CL8'RVNEL'                                           
         DC    AL1(MBIELQ),CL8'MBIEL'                                           
         DC    AL1(RSFELQ),CL8'RSFEL'                                           
         DC    AL1(SPYELQ),CL8'SPYEL'                                           
         DC    AL1(FWTELQ),CL8'FWTEL'                                           
         DC    AL1(PTRELQ),CL8'PTREL'                                           
         DC    AL1(DTSELQ),CL8'DTSEL'                                           
         DC    AL1(AGRELQ),CL8'AGREL'                                           
*&&UK*&& DC    AL1(OCAELQ),CL8'OCAEL'                                           
ELETABX  DC    AL1(ELETEOTQ),CL8'????????'                                      
ELETABN  EQU   (*-ELETAB)/ELETABL  NUMBER OF ELEMENTS                           
         SPACE 1                                                                
OUTTAB   DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  00-0F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  10-1F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  20-2F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  30-3F                    
         DC    XL16'404B4B4B4B4B4B4B4B4B4A4B4C4D4E4F'  40-4F                    
         DC    XL16'504B4B4B4B4B4B4B4B4B5A5B5C5D5E5F'  50-5F                    
         DC    XL16'60614B4B4B4B4B4B4B4B6A6B6C6D6E6F'  60-6F                    
         DC    XL16'4B4B4B4B4B4B4B4B4B797A7B7C7D7E7F'  70-7F                    
         DC    XL16'4B8182838485868788894B4B4B4B4B4B'  80-8F                    
         DC    XL16'4B9192939495969798994B4B4B4B4B4B'  90-9F                    
         DC    XL16'4BA1A2A3A4A5A6A7A8A94B4B4B4B4B4B'  A0-AF                    
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C94B4B4B4B4B4B'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D94B4B4B4B4B4B'  D0-DF                    
         DC    XL16'E04BE2E3E4E5E6E7E8E94B4B4B4B4B4B'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'  F0-FF                    
                                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0A                                                               
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
VCARDS   DS    A                                                                
VPEELDT  DS    A                                                                
VISREC   DS    A                                                                
PLISTL   EQU   *-PLIST                                                          
RECTYPE  DS    XL1                                                              
COMPANY  DS    XL1                                                              
COMPDSP  DS    XL1                                                              
HEXWORK  DS    CL(MAXWIDTH*2)      DUMMY PRINT LINE                             
WORKX    EQU   *                                                                
         SPACE 1                                                                
RECTABD  DSECT                     ** RECORD TYPE TABLE **                      
RECTREC  DS    XL(L'RECTYPE)       RECORD TYPE                                  
RECTEOTQ EQU   X'FF'               END OF TABLE INDICATOR                       
RECTNAM  DS    CL8                 RECORD NAME                                  
RECTABL  EQU   *-RECTABD                                                        
         SPACE 1                                                                
ELETABD  DSECT                     ** ELEMENT TYPE TABLE **                     
ELETEL   DS    XL(L'RECTYPE)       RECORD TYPE                                  
ELETEOTQ EQU   X'FF'               END OF TABLE INDICATOR                       
ELETNAM  DS    CL8                 RECORD NAME                                  
ELETABL  EQU   *-ELETABD                                                        
         SPACE 1                                                                
ELD      DSECT                     ** DSECT FOR ELEMENT **                      
ELCOD    DS    XL1                                                              
ELLEN    DS    XL1                                                              
ELLN1Q   EQU   *-ELD                                                            
         EJECT                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACOPTEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACOPTEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'184ACLDXPRNT 12/22/97'                                      
         END                                                                    
