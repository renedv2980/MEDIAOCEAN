*          DATA SET ACCLB11    AT LEVEL 031 AS OF 08/16/00                      
*PHASE T62111A                                                                  
CLB11    TITLE '- BILL PROGRAM - MATCH 1 LIST - C VERSION'                      
CLB11    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB11**,R8,RR=RE                                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK                                                      
         USING MWORKD,RC                                                        
         LH    R7,=Y(BSDICT-TWAD)                                               
         LA    R7,TWAD(R7)                                                      
         USING BSDICT,R7                                                        
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         USING PRORATAD,LSPRATA                                                 
         USING TRNRECD,IOKEY                                                    
M        USING PTAELD,MPTAELD                                                   
         ST    RE,BORELO                                                        
         SRL   RF,24                                                            
         SLL   RF,2                                                             
*                                                                               
         B     *+4(RF)                                                          
         B     LSTFRST             FIRST FOR THIS LIST                          
         B     SCRFRST             FIRST FOR THIS SCREEN                        
         B     EXIT                LAST FOR THIS SCREEN                         
         B     EXITY               FIRST FOR VALIDATE THIS LINE                 
         B     VALCLM              VALIDATE COLUMN                              
         B     EXITY               LAST FOR VALIDATE THIS LINE                  
         B     DISCLM              DISPLAY COLUMN                               
         B     GETFRST             GET FIRST RECORD FOR LIST                    
         B     GETNEXT             GET NEXT RECORD                              
         B     SETHEAD             SET UP MY OWN HEADING                        
         B     VALSEL              VALIDATE LIST SELECTION                      
         B     EXITY               DISPLAY SUB-TOTAL                            
         B     EXIT                DISPLAY SCREEN TOTAL                         
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* FIRST FOR SCREEN                                                    *         
***********************************************************************         
         SPACE 1                                                                
SCRFRST  MVC   BASJOBC,BCJOBCOD                                                 
         MVC   BASJOBN,BCJOBNAM                                                 
         GOTO1 SETBILL                                                          
         MVC   MLSOPS,LSOPS        EVALUATE FILTERS/OPTIONS                     
         XC    LSOPS(LSOPL),LSOPS                                               
*                                                                               
         L     RE,=A(VALOPT)                                                    
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         MVC   AOVEROUT,ALSVALS                                                 
*                                                                               
         XC    BOWORK2,BOWORK2                                                  
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         OC    BOWORK2(L'GOCBDMTC),GOCBDMTC-GOBBLOCK(RF)                        
         BNZ   *+10                                                             
         MVC   BOWORK2(DEFCLML),DEFCLM                                          
         GOTO1 AVALOPT,BOPARM,OPTTAB,BOWORK2,0                                  
         BNE   SCRFRSTN                                                         
*                                                                               
SFRST30  CLC   MLSOPS,LSOPS        SET CC=HIGH IF FILTERS HAVE CHANGED          
         BNE   EXITH                                                            
         B     EXITY                                                            
*                                                                               
SCRFRSTN MVC   LSOPS(LSOPL),MLSOPS RESTORE PREVIOUS FILTERS                     
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXITL                                                            
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* SET UP HEADINGS                                                     *         
***********************************************************************         
         SPACE 1                                                                
SETHEAD  LM    R2,R4,0(R1)         R4=A(2ND HEADLINE)                           
         USING CLMTABD,R2          R2=A(COLUMN TABLE ENTRY)                     
*                                                                               
         XR    R3,R3               POINT R3 TO END OF HEADLINE                  
         IC    R3,CLMHWDTH                                                      
         AR    R3,R4                                                            
         BCTR  R3,0                                                             
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
*                                                                               
         CLI   0(R4),C' '          POINT R4 TO START OF HEADLINE                
         BH    *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
*                                                                               
         LR    RF,R3                                                            
         SR    RF,R4                                                            
         CH    RF,=Y(L'CSCPYCUR+2)                                              
         BNL   *+10                                                             
         XR    RF,RF                                                            
         B     *+8                                                              
         SRL   RF,1                                                             
         BCTR  RF,0                                                             
         AR    RF,R4                                                            
         MVI   0(RF),C'('                                                       
         MVC   1(L'CSCPYCUR,RF),CSCPYCUR                                        
         MVI   L'CSCPYCUR+1(RF),C')'                                            
*                                                                               
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE COLUMN                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALCLM   SLL   R1,2                                                             
         B     *+4(R1)                                                          
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* DISPLAY COLUMN                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISCLM   L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     DISWC               WORKCODE                                     
         B     DISSUP              SUPPLIER/CONTRA                              
         B     DISREF              TRANSACTION REFERENCE                        
         B     DISBIL              BILL NUMBER                                  
         B     DISCUR              CURRENCY                                     
         B     DISNET              NET                                          
         B     DISCOM              COMMISSION                                   
         B     DISEXC              EXCHANGE RATE                                
         B     DISNETA             NET (AGENCY CURRENCY)                        
         B     DISCOMA             COMMISSION (AGENCY CURRENCY)                 
         SPACE 1                                                                
***********************************************************************         
* DISPLAY WORKCODE                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISWC    BAS   RE,SETMAT                                                        
         MVC   FVIFLD(L'TRNKWORK),TRNKWORK                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY SUPPLIER/CONTRA                                             *         
***********************************************************************         
         SPACE 1                                                                
DISSUP   MVC   FVIFLD(L'TRNKULC),TRNKULC                                        
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY REFERENCE                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISREF   MVC   FVIFLD(L'TRNKREF),TRNKREF                                        
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY BILL NUMBER                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISBIL   MVC   FVIFLD(L'TLMBILL),TLMBILL                                        
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY CURRENCY CODE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISCUR   MVC   FVIFLD(L'TLMCUR),TLMCUR                                          
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY NET AVAILABLE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISNET   CLC   CSCPYCUR,TLMCUR                                                  
         BE    DISNETA                                                          
         CURED (P6,M.PTANETF),(14,FVIFLD),TLMCTAB,MINUS=YES                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY COMMISSION                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISCOM   CLC   CSCPYCUR,TLMCUR                                                  
         BE    DISCOMA                                                          
         CURED (P6,M.PTARFCOM),(14,FVIFLD),TLMCTAB,MINUS=YES                    
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY EXCHANGE RATE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISEXC   CLC   CSCPYCUR,TLMCUR                                                  
         BE    EXIT                                                             
         ZAP   BODUB1,BCPZERO                                                   
         MVO   BODUB1,TLMXEXC                                                   
         CURED (P8,BODUB1),(11,FVIFLD),5,DMCB=BODMCB                            
         LA    RF,FVIFLD+10                                                     
         LA    R0,4                                                             
DISEXC02 CLI   0(RF),C'0'                                                       
         BNE   EXIT                                                             
         MVI   0(RF),C' '                                                       
         BCTR  RF,0                                                             
         BCT   R0,DISEXC02                                                      
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY NET (AGENCY CURRENCY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DISNETA  CURED (P6,M.PTANET),(14,FVIFLD),CSCURCPY,MINUS=YES                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY COMMISSION (AGENCY CURRENCY)                                *         
***********************************************************************         
         SPACE 1                                                                
DISCOMA  CURED (P6,M.PTARCOM),(14,FVIFLD),CSCURCPY,MINUS=YES                    
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
LSTFRST  MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,BCJOBCOD                                                 
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* GET FIRST/NEXT RECORD FOR LIST                                      *         
***********************************************************************         
         SPACE 1                                                                
GETFRST  LA    R1,IOHIGH+IOACCDIR                                               
         B     GNEXT04                                                          
GETNEXT  GOTO1 GETITEM,1           GET NEXT BILL# FOR RECORD                    
         BE    EXITY                                                            
GNEXT02  LA    R1,IOSEQ+IOACCDIR                                                
GNEXT04  GOTO1 AIO                                                              
         BNE   EXITN                                                            
         CLC   TRNKEY(TRNKWORK-TRNKEY),IOKEYSAV                                 
         BNE   EXITN                                                            
         OC    TRNKDATE,TRNKDATE   ENSURE HAVE TRANSACTION RECORD               
         BZ    GNEXT02                                                          
         CLC   TRNKDATE,BCSPACES                                                
         BE    GNEXT02                                                          
         TM    TRNKSTAT,TRNSREVS   EXCLUDE REVERSALS                            
         BNZ   GNEXT02                                                          
         CLI   TRNKSTYP,99         INCLUDE ADVANCES                             
         BE    GNEXT06                                                          
         TM    TRNKSTAT,TRNSDRFT   EXCLUDE OTHER DRAFTS                         
         BO    GNEXT02                                                          
         CLC   TRNKWORK,ORDWC      INCLUDE ORDERS                               
         BNE   GNEXT02                                                          
GNEXT06  GOTO1 ASETTRN,BOPARM,(C'D',TRNRECD)                                    
         BNE   GETNEXT                                                          
*                                                                               
         MVC   IODAOVER,TRNKDA                                                  
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         USING TRNELD,TRNRFST                                                   
         CP    TRNAMNT,BCPZERO                                                  
         BNE   GNEXT02                                                          
*                                                                               
         GOTO1 ASETTRN,BOPARM,(C'M',TRNRECD)                                    
         BNE   GNEXT02                                                          
         MVI   TLMINDS,TLMIADV     TEST ADVANCE/ORDER                           
         CLI   TRNTYPE,99                                                       
         BE    *+8                                                              
         MVI   TLMINDS,TLMIORD                                                  
         GOTO1 GETITEM,0                                                        
         BNE   GNEXT02                                                          
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         MVC   TLMINCAC,GOINCAC-GOBBLOCK(RF)                                    
*                                                                               
GETNEXTX B     EXITY                                                            
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO NEXT UPDATED ITEM ON RECORD                              *         
*                                                                     *         
* NTRY: R1 = ZERO TO GET FIRST ITEM ELSE GET NEXT ITEM                *         
* EXIT: CC = NOT EQUAL IF NO MORE ITEMS ON RECORD                     *         
***********************************************************************         
         SPACE 1                                                                
GETITEM  NTR1  ,                                                                
         XR    R0,R0                                                            
         LTR   R1,R1               TEST GET NEXT ITEM                           
         BZ    GITEM02                                                          
         MVC   TLREC(TLMT1LNQ),STLREC                                           
         L     R3,ALSTPTA                                                       
         B     GITEM08                                                          
*                                                                               
GITEM02  MVC   STLREC,TLREC                                                     
         L     R3,ALSTPTA                                                       
         L     R3,AIO1                                                          
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         TM    TLMINDS,TLMIORD     TEST AN ORDER                                
         BZ    GITEM04             YES - MAY BE PARTLY MATCHED                  
         GOTO1 AMCHORD,BOPARM,(C'I',(R3)),AIO4                                  
         L     R3,AIO4                                                          
         USING PTAELD,R3                                                        
*                                                                               
GITEM04  CLI   PTAEL,0             TEST EOR                                     
         BE    GETITEMN                                                         
         CLI   PTAEL,PTAELQ        TEST REGULAR ALLOCATION PTAEL                
         BNE   GITEM08                                                          
         CLI   PTATYPE,PTATRAL                                                  
         BNE   GITEM08                                                          
         TM    PTASTAT1,PTASPEND+PTASREVS+PTASREVD+PTASREVU                     
         BZ    GITEM10             TEST NOT PENDING OR REVERSE(D)               
GITEM08  IC    R0,PTALN                                                         
         AR    R3,R0                                                            
         B     GITEM04                                                          
*                                                                               
GITEM10  MVC   TLMBILL,PTARBLNO    SAVE BILL#                                   
         MVC   TLMDATC,PTARBLDT    SAVE BILL DATE                               
         GOTO1 VDATCON,BOPARM,(2,TLMDATC),(1,TLMDATP)                           
         MVC   TLMCUR,PTACUR       SAVE CURRENCY CODE                           
         MVC   TLMCTAB,CSCURCPY    SET CURRENCY INFO                            
         CLC   CSCPYCUR,TLMCUR                                                  
         BE    GETITEMY                                                         
         GOTO1 GETBILL             GET EXCHANGE RATE OF BILL                    
         BNE   GETITEMN                                                         
         GOTO1 GETCUR,BOPARM,TLMCUR,TLMCTAB                                     
         BE    GETITEMY                                                         
         GOTO1 AIO,IOREAD+IOACCDIR                                              
*                                                                               
GETITEMY ST    R3,ALSTPTA                                                       
         B     EXITY                                                            
*                                                                               
GETITEMN XC    ALSTPTA,ALSTPTA                                                  
         B     EXITN                                                            
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECTION                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   MVC   BCLSTMAT,TLREC                                                   
         SLL   R1,2                                                             
         B     *(R1)                                                            
         B     VALINV                                                           
*                                                                               
VALINV   TM    TLMINDS,TLMIMAT     * INVOICE (MATCH 2) *                        
         BO    EXITY                                                            
         BZ    EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* TEST INVOICE STILL MATCHABLE AND SAVE A(PTA ELEMENT)                *         
***********************************************************************         
         SPACE 1                                                                
SETMAT   NTR1  ,                                                                
         XC    MPTAELD,MPTAELD                                                  
         ZAP   M.PTANET,BCPZERO                                                 
         ZAP   M.PTANETF,BCPZERO                                                
         ZAP   M.PTARCOM,BCPZERO                                                
         ZAP   M.PTARFCOM,BCPZERO                                               
         L     R3,AIO1                                                          
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         TM    TLMINDS,TLMIORD     TEST AN ORDER                                
         BZ    SMAT02              YES - MAY BE PARTLY MATCHED                  
         GOTO1 AMCHORD,BOPARM,(C'I',(R3)),AIO4                                  
         L     R3,AIO4                                                          
         USING PTAELD,R3                                                        
SMAT02   XR    RF,RF                                                            
SMAT04   CLI   PTAEL,0             MATCH ON BILL PTA ELEMENT                    
         BE    SETMATN                                                          
         CLI   PTAEL,PTAELQ                                                     
         BNE   SMAT08                                                           
         CLI   PTATYPE,PTATRAL                                                  
         BNE   SMAT08                                                           
         TM    PTASTAT1,PTASPEND                                                
         BO    SMAT08                                                           
         CLC   PTARBLNO,TLMBILL                                                 
         BNE   SMAT08                                                           
         CLC   PTARBLDT,TLMDATC                                                 
         BE    SMAT10                                                           
SMAT08   IC    RF,PTALN                                                         
         BXH   R3,RF,SMAT04                                                     
*                                                                               
SMAT10   MVC   MPTAELD,PTAELD      COPY PTA ELEMENT                             
         CP    PTANET,BCPZERO      ENSURE AMOUNT ON BILL                        
         BNE   SETMATY                                                          
         CP    PTARCOM,BCPZERO                                                  
         BNE   SETMATY                                                          
*                                                                               
SETMATN  NI    TLMINDS,FF-TLMIMAT                                               
         B     EXITN                                                            
*                                                                               
SETMATY  OI    TLMINDS,TLMIMAT                                                  
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CONVERT CURRENCY CODE INTO TABLE ENTRY                   *         
*                                                                     *         
* NTRY: P1=A(CURRENCY CODE)                                           *         
*       P2=A(CURRENCY TABLE ENTRY)                                    *         
* EXIT: CC=NOT EQUAL IF VBLDCUR CALLED                                *         
***********************************************************************         
         SPACE 1                                                                
GETCUR   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
*                                                                               
         LA    R4,CODTAB                                                        
         USING CODTABD,R4                                                       
         LA    R0,CODTABL                                                       
         LA    R1,CODTABX-1                                                     
GCUR02   CLI   CODTABD,EOT                                                      
         BE    GCUR04                                                           
         CLC   CODTCOD,0(R2)                                                    
         BNE   *+14                                                             
         MVC   0(L'CODTTAB,R3),CODTTAB                                          
         B     EXITY                                                            
         BXLE  R4,R0,GCUR02                                                     
         SR    R4,R0               USE LAST ENTRY IF TABLE FULL                 
*                                                                               
GCUR04   MVC   CODTCOD,0(R2)                                                    
         GOTO1 VBLDCUR,BOPARM,CODTCOD,(X'80',CODTTAB),ACOM                      
         MVC   0(L'CODTTAB,R3),CODTTAB                                          
         MVI   CODTABD+CODTABL,EOT                                              
         B     EXITN                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET UP BILL TABLE                                        *         
***********************************************************************         
         SPACE 1                                                                
SETBILL  NTR1  ,                                                                
         CLI   BILLTAB,0          TEST BILL TABLE SET UP                        
         BNE   EXIT                                                             
         LA    R3,BILLTAB                                                       
         USING BILLTABD,R3                                                      
         XC    TRNKEY,TRNKEY                                                    
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,BCJOBCOD                                                 
         MVC   TRNKWORK,BILLWC                                                  
*                                                                               
         LA    R0,BILLTABN                                                      
         LA    R1,IOHIGH+IOACCDIR                                               
         B     *+8                                                              
SBILL02  LA    R1,IOSEQ+IOACCDIR                                                
         GOTO1 AIO                                                              
         BNE   SBILL04                                                          
         CLC   TRNKEY(TRNKCULC-TRNKEY),IOKEYSAV                                 
         BNE   SBILL04                                                          
         OC    TRNKDATE,TRNKDATE                                                
         BZ    SBILL02                                                          
         CLC   TRNKDATE,BCSPACES                                                
         BE    SBILL02                                                          
         MVC   BILLDATE,TRNKDATE                                                
         MVC   BILLREF#,TRNKREF                                                 
         MVC   BILLDA,TRNKDA                                                    
         XC    BILLX,BILLX                                                      
         LA    R3,BILLTABL(R3)                                                  
         BCT   R0,SBILL02                                                       
         DC    H'0'                                                             
SBILL04  MVI   BILLTABD,EOT                                                     
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO LOOK UP EXCHANGE RATE TABLE  - BILL                      *         
*                                                                     *         
* NTRY: TLMBILL = BILL#                                               *         
*       TLMDATP = BILL DATE                                           *         
* EXIT: TLMX     = EXCHANGE RATE                                      *         
***********************************************************************         
         SPACE 1                                                                
GETBILL  NTR1  ,                                                                
*                                                                               
         LA    R3,BILLTAB                                                       
GBILL02  CLI   BILLTABD,EOT                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BILLDATE,TLMDATP                                                 
         BNE   *+14                                                             
         CLC   BILLREF#,TLMBILL                                                 
         BE    GBILL04                                                          
         LA    R3,BILLTABL(R3)                                                  
         B     GBILL02                                                          
*                                                                               
GBILL04  OC    BILLX,BILLX                                                      
         BNZ   GBILL08                                                          
         MVC   IODAOVER,BILLDA                                                  
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO3                                                          
         LA    R4,TRNRFST-TRNRECD(R4)                                           
         USING AFCELD,R4                                                        
         XR    RF,RF                                                            
GBILL06  CLI   AFCEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   AFCEL,AFCELQ                                                     
         BE    *+12                                                             
         IC    RF,AFCLN                                                         
         BXH   R4,RF,GBILL06                                                    
         MVC   BILLX,AFCX                                                       
*                                                                               
GBILL08  MVC   TLMX,BILLX                                                       
         B     EXITY                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
FF       EQU   X'FF'                                                            
ACCMST   DC    C'ACCMST '                                                       
BILLWC   DC    C'99'                                                            
ORDWC    DC    C'**'                                                            
*                                                                               
DEFCLM   DS    0XL1                                                             
         DC    AL1(MT1#BIL)                                                     
         DC    AL1(MT1#CUR)                                                     
         DC    AL1(MT1#NET)                                                     
         DC    AL1(MT1#COM)                                                     
         DC    AL1(MT1#EXC)                                                     
         DC    AL1(MT1#NETA)                                                    
         DC    AL1(MT1#COMA)                                                    
         DC    AL1(EOT)                                                         
DEFCLML  EQU   *-DEFCLM                                                         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OPTION TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
CLB11    CSECT                                                                  
OPTTAB   DS    0X                                                               
*                                  DISPLAY=COLUMN CODES                         
         DC    AL2(UC8DSP-TWAD,UC3DSP-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,L'LSDISLST,L'LSDISLST)                           
         DC    AL1((*-OPTTAB)/OPTTABL)                                          
         DC    AL2(OPTDISQ,LSDIS-LSVALSD)                                       
         DC    CL4'+'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  GENERAL TRANSACTION OPTIONS                  
         DC    AL2(TRNOPTQ)                                                     
*                                                                               
OPTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
*********************************************************************           
* VALIDATE OPTION INPUT                                             *           
*********************************************************************           
         SPACE 1                                                                
         DROP  R8,RB                                                            
         DS    0D                                                               
VALOPT   NMOD1 0,**VALO**                                                       
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         LR    R4,R2               R4=A(OPTTAB ENTRY)                           
         USING OPTTABD,R4                                                       
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *(RF)                                                            
VALROUTS DS    0XL4                                                             
         SPACE 1                                                                
VALX     XIT1  ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKC                                                     
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBF1D                                                       
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBCOLS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBCOLSC                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
*                                                                               
CODTABD  DSECT                     ** CURRENCY CODE/TABLE TABLE **              
CODTCOD  DS    CL3                 CODE                                         
CODTTAB  DS    XL(CURTABL)         TABLE ENTRY                                  
CODTABL  EQU   *-CODTABD                                                        
*                                                                               
BILLTABD DSECT                     ** BILL TABLE **                             
BILLDATE DS    PL3                 DATE                                         
BILLREF# DS    CL6                 REFERENCE NUMBER                             
BILLDA   DS    XL4                 DISK ADDRESS                                 
BILLX    DS    CL(L'TLMX)          EXCHANGE RATE                                
BILLTABL EQU   *-BILLTABD                                                       
         SPACE 1                                                                
***********************************************************************         
* LOCAL W/S                                                           *         
***********************************************************************         
         SPACE 1                                                                
MWORKD   DSECT                                                                  
DMCB     DS    6A                                                               
*                                                                               
ALSTPTA  DS    A                   A(LAST PTA ELEMENT ON ITEM)                  
STLREC   DS    XL(TLMT1LNQ)        SAVED LAST TIME TSAR RECORD                  
*                                                                               
MLSOPS   DS    XL(LSOPL)           SAVED FILTERS                                
*                                                                               
MPTAELD  DS    XL(PTARLN2Q)                                                     
*                                                                               
CODTAB   DS    10XL(CODTABL)                                                    
CODTABN  EQU   (*-CODTAB)/CODTABL                                               
CODTABX  DS    XL1                                                              
*                                                                               
BILLTAB  DS    150XL(BILLTABL)                                                  
BILLTABN EQU   (*-BILLTAB)/BILLTABL                                             
BILLTABX DS    XL1                                                              
         ORG   MWORKD+OVERWRKL                                                  
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031ACCLB11   08/16/00'                                      
         END                                                                    
