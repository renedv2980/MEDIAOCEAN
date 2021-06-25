*          DATA SET ACACCGEN   AT LEVEL 072 AS OF 02/02/11                      
*PHASE T00A82A,*                                                                
*INCLUDE WRIDATE                                                                
*INCLUDE WRIPEVAL                                                               
*INCLUDE ADDAY                                                                  
         TITLE 'T00A82 - ACCPAK WRITER GENERAL ROUTINES'                        
         PRINT NOGEN                                                            
T00A82   CSECT                                                                  
         REQUS                                                                  
         USING *,RF                                                             
GEN      NTR1                                                                   
         DROP  RF                                                               
         LR    RB,RF                                                            
         USING T00A82,RB,R7,R6,R8                                               
         B     *+12                                                             
         DC    CL8'**GEN***'                                                    
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         LA    R6,2048(R7)                                                      
         LA    R6,2048(R6)                                                      
         LA    R8,2048(R6)                                                      
         LA    R8,2048(R8)                                                      
         USING GEND,RC                                                          
*        L     R8,ASPOOLD                                                       
*        USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING T614FFD,RA                                                       
         SPACE 1                                                                
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     VWRIUSER                                                         
         B     VVALLEDG                                                         
         B     VVALACC                                                          
         B     VVALFILT                                                         
         B     VVALPERD                                                         
         B     VVALOPTS                                                         
         B     VVALTITS                                                         
         DC    24X'00'                                                          
         SPACE 1                                                                
         B     VVALHEAD                                                         
         B     VVALMID                                                          
         B     VVALROWS                                                         
         B     VVALCOLS                                                         
         DC    24X'00'                                                          
         SPACE 1                                                                
         B     VINTDRIV                                                         
         B     VINTDRON                                                         
         B     VWRPDRON                                                         
         B     VINTHEAD                                                         
         DC    8X'00'                                                           
         SPACE 1                                                                
         B     VGENHEAD                                                         
         B     VGETNAME                                                         
         B     VGETHEIR                                                         
         B     VGETCODE                                                         
         B     VGETLABL                                                         
         B     VVNAMOUT                                                         
         B     VVGETL                                                           
         SPACE 1                                                                
         B     VNUMERIC                                                         
         B     VPACK                                                            
         DC    24X'00'                                                          
         SPACE 1                                                                
         B     VCURSERR                                                         
         B     VERRXIT                                                          
         DC    64X'00'                                                          
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              USER ID RECORD                                                   
         SPACE 3                                                                
VWRIUSER L     R1,SYSPARMS                                                      
         MVC   AGENCY,0(R1)                                                     
         MVC   AGYSIGN,MYSPACES                                                 
         XC    AGYALPHA,AGYALPHA                                                
         MVI   AGYNUM,X'FF'        ASSUME NOT NUMERIC                           
*                                  AGENCY NAME & ADDRESS FROM ID REC.           
         MVC   FILENAME,=CL8'CTFILE'                                            
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),10(RA)                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         LA    R4,CTIDATA                                                       
         SR    R3,R3                                                            
         SPACE 1                                                                
VUSER1   CLI   0(R4),0                                                          
         BE    VUSER3                                                           
         CLI   0(R4),X'30'                                                      
         BE    VUSER2A                                                          
         CLI   0(R4),X'02'                                                      
         BE    VUSER2B                                                          
         CLI   0(R4),X'06'                                                      
         BE    VUSER2C                                                          
         CLI   0(R4),X'21'                                                      
         BE    VUSER2D                                                          
         SPACE 1                                                                
VUSER1A  IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     VUSER1                                                           
         SPACE 1                                                                
         USING CTDSTD,R4                                                        
VUSER2A  DS    0H                                                               
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   USERNAME,CTDSTNAM                                                
         MVC   USERADDR,CTDSTADD                                                
         DROP  R5                                                               
         B     VUSER1A                                                          
         SPACE 1                                                                
         USING CTDSCD,R4                                                        
VUSER2B  MVC   AGYSIGN,CTDSC                                                    
         MVC   WORK(3),AGYSIGN+2                                                
         NC    WORK(3),=X'F0F0F0'                                               
         CLC   WORK(3),=X'F0F0F0'                                               
         BNE   VUSER1A                                                          
         MVC   AGYNUM,AGYSIGN+2                                                 
         B     VUSER1A                                                          
         SPACE 1                                                                
         USING CTAGYD,R4                                                        
VUSER2C  MVC   AGYALPHA,CTAGYID                                                 
         B     VUSER1A                                                          
         SPACE 1                                                                
         USING CTSYSD,R4                                                        
VUSER2D  CLI   CTSYSNUM,6          INSURE THIS ONE'S FOR ACC                    
         BNE   VUSER1A                                                          
         OC    TWAACCS(2),TWAACCS  IS ACCESS INFO THERE                         
         BNZ   *+10                YES                                          
         MVC   TWAACCS(4),CTSYSLMT NO - MOVE IT IN NOW                          
         CLC   =C'TAL',CTSYSLMT+3  OLD FORMAT                                   
         BNE   *+8                                                              
         MVI   TWAACCS+3,C' '      INSURE LAST BYTE IS BLANK (OFFICE)           
         B     VUSER1A                                                          
         SPACE 1                                                                
VUSER3   XC    FILENAME,FILENAME                                                
         CLC   AGENCY,WRICOMP                                                   
         BE    VUSER4                                                           
         MVC   WRICOMP,AGENCY      FIRST FOR COMPANY                            
         MVC   KEY,MYSPACES                                                     
         MVC   KEY(1),WRICOMP      READ COMPANY RECORD FOR NAME                 
         MVI   RDUPDATE,C'N'       AVOID CONTENTION ON COMPANY REC              
         GOTO1 READ                                                             
         GOTO1 NAMOUT,DMCB,AIO,COMPNAME                                         
         SPACE 1                                                                
VUSER4   GOTO1 GETFACT,DMCB,0                                                   
         L     R4,DMCB                                                          
         MVC   FACTWRK(80),0(R4)                                                
         SPACE 1                                                                
VUSERX   B     XIT                                                              
         EJECT                                                                  
*              VALIDATE LEDGER                                                  
         SPACE 3                                                                
VVALLEDG GOTO1 ANY                                                              
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK)                                      
         MVI   FIELDERR,1                                                       
         LA    R3,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    BADLEDG                                                          
         LA    R4,LEDGLIST                                                      
         XC    LEDGLIST,LEDGLIST                                                
         SPACE 1                                                                
VVALL2   MVC   0(2,R4),12(R3)                                                   
         CLI   12(R3),C'T'                                                      
         BNE   VVALL3                                                           
         GOTO1 HEXIN,DMCB,13(R3),1(R4),2                                        
         OC    DMCB+12(4),DMCB+12                                               
         BZ    BADLEDG                                                          
         SPACE 1                                                                
VVALL3   BAS   RE,VVALL4                                                        
         BNE   BADLEDG                                                          
         AI    FIELDERR,1                                                       
         LA    R3,32(R3)                                                        
         LA    R4,2(R4)                                                         
         BCT   R0,VVALL2                                                        
         B     XIT                                                              
         SPACE 1                                                                
VVALL4   NTR1                                                                   
         MVC   KEY,MYSPACES                                                     
         LA    R1,KEY                                                           
         USING ACKEYD,R1                                                        
         MVC   ACKEYACC(1),WRICOMP                                              
         MVC   ACKEYACC+1(2),0(R4)                                              
         MVC   QCOMPANY(3),ACKEYACC                                             
         DROP  R1                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         B     XIT                                                              
         SPACE 1                                                                
BADLEDG  MVC   CONHEAD(L'LEDGERR),LEDGERR                                       
         B     MYCURSOR                                                         
         EJECT                                                                  
*              VALIDATE ACCOUNT                                                 
         SPACE 3                                                                
VVALACC  XC    QACCOUNT,QACCOUNT   THIS IS AN OPTIONAL FIELD                    
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVC   KEY,MYSPACES                                                     
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(3),QCOMPANY                                             
         GOTO1 ANY                                                              
         MVC   QACCOUNT,WORK                                                    
         MVC   ACKEYACC+3(12),QACCOUNT                                          
         GOTO1 READ                                                             
         CLI   OPTION,C'Y'         OPTION TO GET NAME RETURNED                  
         BNE   XIT                                                              
         BAS   RE,BUMP             IN THE NEXT FIELD                            
         GOTO1 NAMOUT,DMCB,AIO,8(R2)                                            
         OI    6(R2),X'80'         TRANSMIT THIS                                
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE FILTERS                                                 
         SPACE 3                                                                
VVALFILT XC    QFILTERS,QFILTERS   PRECLEAR FILTERS                             
         XC    QFILTCA,QFILTCA                                                  
         XC    QFILTWRK,QFILTWRK                                                
         XC    QFILTNOF,QFILTNOF                                                
         XC    QFILTBT,QFILTBT                                                  
         MVI   QFILTMED,0                                                       
         MVI   QFILTOFF,0                                                       
         MVI   QFILTMG,0                                                        
         MVI   QFILTOG,0                                                        
         MVI   QFILTWG,0                                                        
         MVI   QFILTLOK,0                                                       
         MVI   QFILTCLS,0                                                       
         MVI   QFILTRTY,0                                                       
         MVI   QFILTREV,0                                                       
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(20,(R2)),(6,BLOCK)                                 
         MVI   FIELDERR,1                                                       
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADFILT                                                          
         SPACE 1                                                                
FILT2    LA    RE,QFILTCA+1                                                     
         LA    RF,2                                                             
         CLC   12(2,R4),=C'CU'     CONTRA UNIT/LEDGE                            
         BE    FILTSET                                                          
         LA    RF,14                                                            
         CLC   12(2,R4),=C'CA'     CONTRA U/L/ACCOUNT                           
         BE    FILTSET                                                          
         LA    RE,QFILTATT                                                      
         CLC   12(2,R4),=C'AT'     ATTRIBUTE                                    
         BE    TRNSFILT                                                         
         LA    RE,QFILTWL          WORK LIST                                    
         LA    RF,5                                                             
         CLC   12(2,R4),=C'WL'                                                  
         BE    LISTSET                                                          
         LA    RE,QFILTCL          CONTRA ACCOUNT LIST                          
         CLC   12(3,R4),=C'CL '                                                 
         BE    LISTSET                                                          
         CLC   12(3,R4),=C'CLI'                                                 
         BE    LISTSET                                                          
         LA    RE,QFILTAL          ACCOUNT LIST                                 
         CLC   12(2,R4),=C'AL'                                                  
         BE    LISTSET                                                          
         LA    RE,QFILTTUN         TALENT UNIT                                  
         LA    RF,4                                                             
         CLC   12(5,R4),=C'TUNIT'                                               
         BE    FILTSET                                                          
         LA    RE,QFILTAGY         TALENT AGENCY                                
         LA    RF,3                                                             
         CLC   12(3,R4),=C'AGY'                                                 
         BE    FILTSET                                                          
         CLC   12(6,R4),=C'AGENCY'                                              
         BE    FILTSET                                                          
         LA    RE,QFILTWRK         WORK CODE                                    
         LA    RF,2                                                             
         CLC   12(2,R4),=C'WC'                                                  
         BE    FILTSET                                                          
         LA    RE,QFILTNOF         OFFICE                                       
         CLC   12(2,R4),=C'OF'                                                  
         BE    FILTSET                                                          
         LA    RE,QFILTBT          BATCH TYPE                                   
         CLC   12(2,R4),=C'BT'                                                  
         BE    TRNSFILT                                                         
         LA    RE,QFILTMED         MEDIA                                        
         LA    RF,1                                                             
         CLC   12(2,R4),=C'ME'                                                  
         BE    FILTSET                                                          
         LA    RE,QFILTMG          MEDIA GROUP                                  
         CLC   12(2,R4),=C'MG'                                                  
         BE    FILTSET                                                          
         LA    RE,QFILTOG          OFFICE GROUP                                 
         CLC   12(2,R4),=C'OG'                                                  
         BE    FILTSET                                                          
         LA    RE,QFILTWG                                                       
         CLC   12(2,R4),=C'WG'     WORK GROUP                                   
         BE    FILTSET                                                          
         LA    RE,QFILTLOK                                                      
         CLC   12(4,R4),=C'LOCK'   LOCKED                                       
         BE    FILTSET                                                          
         LA    RE,QFILTCLS                                                      
         CLC   12(4,R4),=C'CLOSED' CLOSED                                       
         BE    FILTSET                                                          
         LA    RE,QFILTRTY                                                      
         CLC   12(2,R4),=C'RT'     RT TYPE                                      
         BE    TRNSFILT                                                         
         LA    RE,QFILTTTY                                                      
         CLC   12(2,R4),=C'TT'     TIME TYPE                                    
         BE    FILTSET                                                          
         LA    RE,QFILTREV                                                      
         CLC   12(3,R4),=C'REV'    REVERSALS                                    
         BE    FILTSET                                                          
         LA    RE,QFILTERS                                                      
         CLC   12(2,R4),=C'F1'     FILTERS 1-5                                  
         BE    FILTSET                                                          
         LA    RE,QFILTERS+1                                                    
         CLC   12(2,R4),=C'F2'                                                  
         BE    FILTSET                                                          
         LA    RE,QFILTERS+2                                                    
         CLC   12(2,R4),=C'F3'                                                  
         BE    FILTSET                                                          
         LA    RE,QFILTERS+3                                                    
         CLC   12(2,R4),=C'F4'                                                  
         BE    FILTSET                                                          
         LA    RE,QFILTF5                                                       
         CLC   12(2,R4),=C'F5'                                                  
         BE    FILTSET                                                          
         CLC   12(5,R4),=C'DATE '  DATE FILTER OPTION                           
         BNE   BADFILT                                                          
         MVI   QFILTDAT,C'Y'                                                    
         B     FILTEND                                                          
         SPACE 1                                                                
BADFILT  MVC   CONHEAD(L'FLTERR),FLTERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADFILT2 MVC   CONHEAD(L'FLT2ERR),FLT2ERR                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
TRNSFILT MVI   FRCETRNS,C'Y'       FILTER FORCES TRANSACTION READING            
         B     FILTSET                                                          
         SPACE 1                                                                
LISTSET  LR    R1,RE                                                            
         BAS   RE,CHEKLIST         ALSO CHECK LIST EXISTS                       
         LR    RE,R1                                                            
         SPACE 1                                                                
FILTSET  CLC   12(2,R4),=C'BT'     SPECIAL FOR BATCH TYPE                       
         BNE   FILTSET2                                                         
         LA    R1,22(R4)                                                        
         CLI   0(R1),C'-'                                                       
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
         CLI   1(R1),C' '          FORCE 2-BYTE FIELDS                          
         BH    FILTSET2                                                         
         MVC   1(1,R1),0(R1)                                                    
         MVI   0(R1),C'0'                                                       
         SPACE 1                                                                
FILTSET2 LA    R1,22(RF,R4)        R1=A(NEXT BYTE) S/B SPACE                    
         BCTR  RF,0                MOVE IN FILTER VALUE                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),22(R4)                                                   
         CLI   0(RE),C'-'          CHECK FOR MINUS FILTER                       
         BNE   FILTSET4                                                         
         LA    R1,1(R1)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),23(R4)                                                   
         NI    0(RE),X'BF'         40 BIT OFF IN FIRST BYTE                     
         SPACE 1                                                                
FILTSET4 CLI   0(R1),C' '          MAKE SURE SECOND HALF NOT TOO LONG           
         BH    BADFILT2                                                         
         SPACE 1                                                                
FILTEND  LA    R4,42(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,FILT2                                                         
         B     XIT                                                              
         SPACE 1                                                                
CHEKLIST NTR1                                                                   
         SPACE 1                                                                
         LA    R1,22(R4)           CHECK THAT LIST EXISTS                       
         CLI   0(R1),C'-'                                                       
         BNE   *+8                                                              
         LA    R1,1(R1)            GET PAST NEGATIVE LIST                       
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING ACLKEY,R4                                                        
         MVI   ACLKCODE,ACLKCEQU                                                
         MVC   ACLKCOMP,QCOMPANY                                                
         MVC   ACLKLIST,0(R1)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BE    XIT                                                              
         SPACE 1                                                                
BADLIST  MVC   CONHEAD(L'LSTERR),LSTERR                                         
         B     MYCURSOR                                                         
         EJECT                                                                  
*              VALIDATE PERIOD FIELDS                                           
         SPACE 3                                                                
*                                  TRANSACTIONS FIRST                           
*                                                                               
VVALPERD XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING PERVALD,R3                                                       
         MVC   PVALESTA(12),=C'600101273112'                                    
         XC    APERFLD,APERFLD                                                  
         MVI   QPERTYPE,0                                                       
         CLI   5(R2),0                                                          
         BE    VVALPER2                                                         
         XC    WORK,WORK                                                        
         MVI   FRCETRNS,C'Y'       FORCE TRANSACTION READING                    
         ST    R2,APERFLD                                                       
         MVI   QPERTYPE,C'T'                                                    
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         GOTO1 PERVAL,DMCB,((R0),8(R2)),(0,WORK)                                
         CLI   4(R1),0                                                          
         BE    VVALPER2                                                         
         CLI   4(R1),4                                                          
         BE    VVALPER2                                                         
         B     VVALPERR            INVALID DATE                                 
*                                                                               
VVALPER2 GOTO1 DATCON,DMCB,(0,PVALESTA),(1,QTRASTR)                             
         GOTO1 DATCON,DMCB,(0,PVALEEND),(1,QTRAEND)                             
         MVC   PARAS(12),PVALESTA                                               
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         SPACE 1                                                                
*                                  NOW DO MONTH OF SERVICE                      
         XC    WORK,WORK                                                        
         MVC   PVALESTA(12),=C'600101273112'                                    
         CLI   5(R2),0                                                          
         BE    VVALPER4                                                         
         XC    WORK,WORK                                                        
         ST    R2,APERFLD                                                       
         MVI   QPERTYPE,C'M'                                                    
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         GOTO1 PERVAL,DMCB,((R0),8(R2)),(0,WORK)                                
         TM    PVALASSM,PVALASD+PVALAED                                         
         BNO   VVALPERR                                                         
         CLI   4(R1),0                                                          
         BE    VVALPER4                                                         
         CLI   4(R1),4                                                          
         BE    VVALPER4                                                         
*                                                                               
VVALPERR MVC   CONHEAD(L'INVDTE),INVDTE                                         
         B     MYCURSOR                                                         
*                                                                               
VVALPER4 GOTO1 DATCON,DMCB,(0,PVALESTA),(1,DUB)                                 
         MVC   QMOSSTR,DUB                                                      
         GOTO1 DATCON,DMCB,(0,PVALEEND),(1,DUB)                                 
         MVC   QMOSEND,DUB                                                      
         MVC   WORK(12),PVALESTA    MOS DATES                                   
         MVC   WORK+12(12),PARAS     PERIOD DATES                               
         DROP  R3                                                               
*                                                                               
         LA    R3,WORK             USE MOS FOR SOFT DATE GENERATION             
         CLI   5(R2),0                                                          
         BNE   *+8                                                              
         LA    R3,WORK+12          BUT TRANS IF MOS NOT ENTERED                 
         GOTO1 DATCON,DMCB,(0,(R3)),(1,DUB)                                     
         GOTO1 DATCON,DMCB,(0,6(R3)),(1,DUB+2)                                  
         MVC   PPERIOD,DUB         GET PACKED PERIOD SET UP                     
         GOTO1 ,DMCB,(FISCOPT,(R3)),ADATES,ACOMFACS                             
         L     RF,=V(WRIDATE)                                                   
         LA    R2,RELOW                                                         
         S     R2,RELOW                                                         
         AR    RF,R2                                                            
         BASR  RE,RF                                                            
         SPACE 1                                                                
*                                  SET BEFSTART TO MONTH BEFORE START           
         MVC   WORK+4(2),=C'01'                                                 
         SR    R1,R1                                                            
         BCTR  R1,0                                                             
         ST    R1,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,WORK)                                  
         MVC   BEFSTART,WORK                                                    
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 3                                                                
VVALOPTS DS    0H                                                               
         MVI   BOXOPT,C'Y'                                                      
         MVI   LEFTOPT,C'N'                                                     
         MVI   SPACOPT,1                                                        
         MVI   DOWNOPT,0                                                        
         XC    MYRNKMAX,MYRNKMAX                                                
         MVI   DRINDS,0                                                         
         MVI   DRINDS2,0                                                        
         BAS   RE,GETFISC                                                       
         MVI   THOUOPT,C'N'                                                     
         MVI   WIDEOPT,C'N'                                                     
         MVI   TRACEOPT,C'N'                                                    
         MVI   BRACKOPT,C'N'                                                    
         MVI   GRANDOPT,C'N'                                                    
         MVI   NARROPT,C'N'                                                     
         MVI   TRANSOPT,C'N'                                                    
         MVI   ALTNOPT,C'N'                                                     
         MVI   SEPOPT,0                                                         
         MVI   OUTOPTS,0                                                        
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVI   FIELDERR,1                                                       
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
         SPACE 1                                                                
OPT2     CLC   12(3,R4),=C'BOX'    BOX OPTION                                   
         BNE   OPT4                                                             
         MVC   BOXOPT,22(R4)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     CLC   12(4,R4),=C'LEFT'   LEFT OPTION                                  
         BNE   OPT6                                                             
         MVI   LEFTOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   12(2,R4),=C'S   '   SPACING OPTION                               
         BNE   OPT8                                                             
         MVC   SPACOPT,11(R4)                                                   
         CLI   SPACOPT,1                                                        
         BL    BADOPT                                                           
         CLI   SPACOPT,3                                                        
         BH    BADOPT                                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8     CLC   12(5,R4),=C'DOWN '  DOWNLOADING OPTION                           
         BNE   OPT8B                                                            
         OI    DOWNOPT,GLDLACTV                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8B    CLC   12(3,R4),=C'MAX'    MAX OPTION FOR RANKING                       
         BE    OPT8D                                                            
         CLC   12(3,R4),=C'TOP'    NOW CALLED TOP                               
         BNE   OPT9                                                             
         SPACE 1                                                                
OPT8D    MVC   MYRNKMAX,8(R4)                                                   
         OC    MYRNKMAX,MYRNKMAX                                                
         BZ    BADOPT                                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT9     CLC   12(6,R4),=C'NOHEAD' DON'T DOWNLOAD HEADINGS                      
         BNE   OPT10                                                            
         OI    DOWNOPT,GLDLNOHD                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT10    CLC   12(2,R4),=C'FISCAL' FISCAL=1-12 OPTION                           
         BNE   OPT12                                                            
         L     R1,8(R4)                                                         
         LTR   R1,R1                                                            
         BZ    BADOPT                                                           
         CH    R1,=H'12'                                                        
         BH    BADOPT                                                           
         LA    R1,FISCLIST-1(R1)                                                
         MVC   FISCOPT,0(R1)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT12    CLC   12(2,R4),=C'THOU'   THOUSAND OPTION                              
         BNE   OPT14                                                            
         MVI   THOUOPT,C'Y'                                                     
         CLI   22(R4),C'1'         (1 DEC PLACES)                               
         BNE   *+8                                                              
         MVI   THOUOPT,C'1'                                                     
         CLI   22(R4),C'2'         (2 DEC PLACES)                               
         BNE   *+8                                                              
         MVI   THOUOPT,C'2'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT14    CLC   12(2,R4),=C'WIDE'   WIDE PRINTING (165)                          
         BNE   OPT16                                                            
         MVI   WIDEOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT16    CLC   12(5,R4),=C'TRACE'  TRACE OPTION                                 
         BNE   OPT18                                                            
         MVI   TRACEOPT,C'Y'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT18    CLC   12(2,R4),=C'BRACKET' OPTION                                      
         BNE   OPT18B                                                           
         MVI   BRACKOPT,C'Y'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT18B   CLC   12(2,R4),=C'COMMAS ' OPTION                                      
         BNE   OPT18D                                                           
         OI    OUTOPTS,DRCOMMAO                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT18D   CLC   12(5,R4),=C'ALIGN'   ALIGNMENT OPTIONS                           
         BNE   OPT18H                                                           
         CLI   22(R4),C'R'                                                      
         BE    OPT18F                                                           
         CLI   22(R4),C'L'                                                      
         BNE   BADOPT                                                           
         OI    OUTOPTS,DRALGNLO    LEFT                                         
         B     OPTEND                                                           
         SPACE 1                                                                
OPT18F   OI    OUTOPTS,DRALGNRO    RIGHT                                        
         B     OPTEND                                                           
         SPACE 1                                                                
OPT18H   CLC   12(7,R4),=C'NOBLANK' ZERO=NOBLANK OPTION                         
         BNE   OPT20                                                            
         OI    OUTOPTS,DRZEROO                                                  
         B     OPTEND                                                           
         SPACE 1                                                                
OPT20    CLC   12(2,R4),=C'GRAND'   GRAND TOTAL OPTION                          
         BNE   OPT22                                                            
         MVI   GRANDOPT,C'Y'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT22    CLC   12(6,R4),=C'NARROW'  OPTION                                      
         BNE   OPT24                                                            
         MVI   NARROPT,C'Y'                                                     
         MVI   LEFTOPT,C'Y'         FORCE LEFT OPTION                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT24    CLC   12(2,R4),=C'TRANS'   OPTION TO READ TRANSACTIONS                 
         BNE   OPT26                                                            
         MVI   TRANSOPT,C'Y'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT26    CLC   12(6,R4),=C'ALLDET'  OPTION TO GET ALL DETAILS                   
         BNE   OPT28                                                            
         OI    DRINDS,X'02'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT28    CLC   12(6,R4),=C'ALLTOT'  OPTION TO GET ALL TOTALS                    
         BNE   OPT30                                                            
         OI    DRINDS,X'04'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT30    CLC   12(4,R4),=C'TON '    TRACE ON KEY                                
         BNE   OPT32                                                            
         MVC   ACIOTRON,22(R4)                                                  
         B     OPTEND                                                           
         SPACE 1                                                                
OPT32    CLC   12(5,R4),=C'TOFF '   TRACE OFF KEY                               
         BNE   OPT34                                                            
         MVC   ACIOTROF,22(R4)                                                  
         B     OPTEND                                                           
         SPACE 1                                                                
OPT34    CLC   12(5,R4),=C'TMAX '   MAX TO TRACE                                
         BNE   OPT36                                                            
         MVC   ACIOTRMX,8(R4)                                                   
         B     OPTEND                                                           
         SPACE 1                                                                
OPT36    CLC   12(3,R4),=C'ALT '    ALTERNATE NAME OPTION                       
         BNE   OPT46                                                            
         MVI   ALTNOPT,C'Y'                                                     
         CLI   22(R4),C' '         BLANK=Y                                      
         BE    OPTEND                                                           
         CLI   22(R4),C'Y'         Y=NAME THEN ALT                              
         BE    OPTEND                                                           
         CLI   22(R4),C'O'         O=ALT ONLY                                   
         BNE   BADOPT                                                           
         MVI   ALTNOPT,C'O'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT46    CLC   12(5,R4),=C'SOLID'   PRINT WHOLE PRINT LINES                     
         BNE   OPT48                                                            
         OI    DRINDS2,GLPWHOLE                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT48    CLC   12(4,R4),=C'XBOX'    EXTRA BOX AFTER ROWS                        
         BNE   OPT50                                                            
         OI    DRINDS2,GLEXTBOX                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT50    CLC   12(6,R4),=C'PBREAK'  PAGE BREAK FOR DETAIL TOTALS                
         BNE   OPT52                                                            
         OI    DRINDS2,GLPBREAK                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT52    CLC   12(4,R4),=C'CUT '    ONLY ONE LINE OF OUTPUT                     
         BNE   OPT52B                                                           
         OI    DOWNOPT,GLDLCUT                                                  
         B     OPTEND                                                           
         SPACE 1                                                                
OPT52B   CLC   12(5,R4),=C'STRIP'  OPTION TO STRIP                              
         BNE   OPT52D                                                           
         OI    DOWNOPT,GLDLSTRP                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT52D   CLC   12(7,R4),=C'NOTRUNC'    NO TRUNCATION                            
         BNE   OPT52F                                                           
         OI    DOWNOPT,GLDLNOTR                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT52F   CLC   12(7,R4),=C'ALLALPH'    ALL ALPHA                                
         BNE   OPT54                                                            
         OI    DOWNOPT,GLDLALPH                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT54    CLC   12(8,R4),=C'DOWNTAPE'    DOWN IN 'TAPE' FORMAT                   
         BNE   OPT56                                                            
         OI    DOWNOPT,GLDLACTV+GLDLNOHD+GLDLSTRP+GLDLNOTR+GLDLALPH             
         B     OPTEND                                                           
         SPACE 1                                                                
OPT56    CLC   12(7,R4),=C'INCLUDE' INCLUDE OPTION FOR MAX/MIN                  
         BNE   OPT60                                                            
         OI    DRINDS2,GLINCLUD                                                 
         B     OPTEND                                                           
         SPACE 1                                                                
OPT60    CLC   12(4,R4),=C'SEP '    SEPARATE LINE FOR EACH TRANS                
         BNE   OPT62                                                            
         MVI   SEPOPT,C'Y'                                                      
         B     OPTEND                                                           
         SPACE 1                                                                
OPT62    CLC   12(4,R4),=C'PER '    PERIOD TYPE                                 
         BNE   OPT64                                                            
         MVI   QFILTDTY,QFILTDBI    PER=BIL                                     
         CLC   22(3,R4),=C'BIL'                                                 
         BE    OPTEND                                                           
         B     BADOPT                                                           
         SPACE 1                                                                
OPT64    DS    0H                                                               
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              FISCAL SUPPORT                                                   
         SPACE 3                                                                
GETFISC  NTR1                                                                   
         MVC   KEY,MYSPACES                                                     
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(1),WRICOMP                                              
         GOTO1 READ                                                             
         GOTO1 GETL,DMCB,(X'10',AIO),0                                          
         CLI   WRIELERR,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,WRIELAD                                                       
         USING ACCOMPD,R4                                                       
         MVC   FISCOPT,ACMPSTM                                                  
         B     XIT                                                              
         SPACE 1                                                                
FISCLIST DC    C'123456789ABC'                                                  
         EJECT                                                                  
*              VALIDATE TITLE AND SUBTITLE                                      
         SPACE 3                                                                
VVALTITS MVC   TITLE,MYSPACES                                                   
         MVC   TITLE(24),=C'ACCOUNTING REPORT WRITER'                           
         CLI   5(R2),0                                                          
         BE    VVALTIT2                                                         
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
         SPACE 1                                                                
VVALTIT2 CLI   NARROPT,C'Y'                                                     
         BE    VVALTIT3                                                         
         GOTO1 CENTER,DMCB,TITLE,32                                             
         SPACE 1                                                                
VVALTIT3 MVC   SUBTITLE,MYSPACES                                                
         BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BE    VVALTIT4                                                         
         GOTO1 ANY                                                              
         MVC   SUBTITLE,WORK                                                    
         CLI   NARROPT,C'Y'                                                     
         BE    VVALTIT4                                                         
         GOTO1 CENTER,DMCB,SUBTITLE,32                                          
         SPACE 1                                                                
VVALTIT4 MVC   PERTITLE,MYSPACES                                                
         BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BE    VVALTIT6                                                         
         GOTO1 ANY                                                              
         MVC   PERTITLE,WORK                                                    
         SPACE 1                                                                
VVALTIT6 MVC   RTITLE,MYSPACES                                                  
         BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 ANY                                                              
         MVC   RTITLE,WORK                                                      
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE HEADERS                                                 
         SPACE 1                                                                
VVALHEAD XC    TOTWIDTH,TOTWIDTH   NOT CHECKING REPORT WIDTH YET                
         ZIC   R3,MAX                                                           
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,5                                                             
         STC   R3,MAX                                                           
         BAS   RE,DELINS           CHECK FOR DELETES/INSERT                     
         LA    R4,4                (START ON HEAD 4)                            
         MVI   ANYROWSW,C'N'                                                    
         SPACE 1                                                                
VVH2     MVI   MYPOSO,C'H'                                                      
         STC   R4,MYPOSO+1                                                      
         MVI   MYPOSO+2,2          (COLUMN 2)                                   
         BAS   RE,VALROW                                                        
         BAS   RE,BUMP                                                          
         LA    R4,1(R4)                                                         
         CLI   5(R2),0                                                          
         BE    VVH4                                                             
         BCT   R3,VVH2                                                          
         SPACE 1                                                                
VVH4     LA    R4,2(R4)                                                         
         CH    R4,=H'8'                                                         
*        BH    *+8                                                              
         BNH   *+8                                                              
         LA    R4,8                                                             
         STC   R4,MYFIRSTH                                                      
         B     XIT                                                              
         SPACE 3                                                                
*              VALIDATE MID                                                     
         SPACE 1                                                                
VVALMID  MVI   MYPOSO,C'M'                                                      
         MVI   MYPOSO+1,1                                                       
         MVI   MYPOSO+2,1                                                       
         BAS   RE,VALROW                                                        
         B     XIT                                                              
         SPACE 3                                                                
*              VALIDATE ROWS                                                    
         SPACE 1                                                                
VVALROWS MVI   TOTWIDTH+1,1        START CHECKING REPORT WIDTH NOW              
         ST    R2,ALASTCOL         (REALLY A(FIRST ROW!))                       
         ZIC   R3,MAX                                                           
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,6                                                             
         STC   R3,MAX                                                           
         BAS   RE,DELINS           CHECK FOR DELETES/INSERT                     
         SPACE 1                                                                
VVR2     XC    MYPOSO,MYPOSO                                                    
         BAS   RE,VALROW                                                        
         ZIC   R0,TOTWIDTH+1                                                    
         BCTR  R0,0                                                             
         CLI   ROW1WIDE,0                                                       
         BNE   *+8                                                              
         STC   R0,ROW1WIDE                                                      
         BAS   RE,BUMP                                                          
         BCT   R3,VVR2                                                          
         STC   R0,ROWWIDTH                                                      
         L     R2,ALASTCOL                                                      
         CLI   ANYROWSW,C'N'                                                    
         BE    BADNEED1                                                         
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'        IF WE ARE OFFLINE                            
         BNE   XIT                                                              
         CLI   SEPOPT,C'Y'         AND UNIQUE FEATURE SPECIFIED                 
         BNE   XIT                                                              
         XC    BLOCK(12),BLOCK     GENERATE A UNIQUE ENTRY                      
         MVC   BLOCK+12(30),MYSPACES                                            
         MVI   BLOCK,6                                                          
         MVC   BLOCK+12(6),=C'UNIQUE'                                           
         LA    R4,BLOCK                                                         
         GOTO1 VROWDRON                                                         
         NI    DRFLAGO,X'7F'       (ENSURE IT DOESN'T PRINT!)                   
         GOTO1 GROWDRON                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROWS - FIRST VALIDATE FOR KEYWORD                                
         SPACE 3                                                                
VALROW   NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVI   ANYROWSW,C'Y'                                                    
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK),0                                    
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    BADROW                                                           
         MVI   FIELDERR,1                                                       
         LA    R4,BLOCK                                                         
         SPACE 1                                                                
         GOTO1 VROWDRON            VALIDATE A ROW ENTRY                         
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
         CLI   DRATTRIB,C'C'       COLUMN ONLY ENTRIES NOT ALLOWED              
         BE    BADROW2                                                          
         CLC   12(4,R4),=C'RANK'                                                
         BNE   VROW2                                                            
         CH    R0,=H'1'            OF IT'S ON ITS OWN                           
         BE    VROW21                 MAKE NON PRINT                            
         LA    R4,32(R4)           RANK NEEDS A COMPUTE EXPRESSION              
         BCT   R0,*+8                                                           
         B     BADROW                                                           
         AI    FIELDERR,1                                                       
         MVI   DRCMPMAX,C'P'                                                    
         CLI   OFFLINE,C'Y'                                                     
         BNE   VROW1                                                            
         GOTO1 GROWDRON                                                         
         GOTO1 GCMPDRON                                                         
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
         B     XIT                                                              
         SPACE 1                                                                
VROW1    GOTO1 VCMPDRON            VALIDATE A COMPUTE EXPRESSION                
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
         OC    TOTWIDTH,TOTWIDTH   IF WE ARE IN THE ROWS                        
         BZ    VROW2                                                            
         CH    R3,=H'1'            CHECK THIS IS NOT THE LAST ROW               
         BE    BADLRANK                                                         
         LR    R3,R2                                                            
         BAS   RE,BUMP                                                          
         CLI   5(R2),0             AND THERE IS INPUT IN NEXT                   
         LR    R2,R3                                                            
         BE    BADLRANK            NOT GOOD TO RANK ON LAST ROW                 
         SPACE 1                                                                
VROW2    CLI   MYPOSO,C'H'         SPECIAL FOR HEADS                            
         BNE   VROW4                                                            
         OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
         OI    DRFOPTS,DRFSKIP     WITH SKIP OPTION                             
         MVI   DRFSPACE,0                                                       
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW4    CLI   MYPOSO,C'M'         SPECIAL FOR MID                              
         BNE   VROWNXT                                                          
         OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
         MVI   DRLSPACE,1          WITH ONE SPACE                               
         B     VROWNXT                                                          
         EJECT                                                                  
*              CHECK FOR SUBSIDIARY ROW EXPRESSIONS                             
         SPACE 3                                                                
VROW12   CLC   12(2,R4),=C'* '     TOTAL EXPRESSION                             
         BNE   VROW14                                                           
         OI    DRTOTAL,X'80'                                                    
*******  MVI   DRTSPACE,1          SPACE AFTER TOTALS                           
         LR    RE,R2                                                            
         SPACE 1                                                                
VROW12B  ZIC   RF,0(RE)                                                         
         AR    RE,RF               HAVE A LOOK AT THE NEXT LEVEL                
         CLI   0(RE),0                                                          
         BE    VROWNXT                                                          
         TM    1(RE),X'20'                                                      
         BO    VROW12B             FIND AN UNPROTECTED FIELD                    
         ZIC   RF,5(RE)                                                         
         LTR   RF,RF               WITH SOME DATA                               
         BZ    VROW12B                                                          
         LA    RE,8(RE)                                                         
         SPACE 1                                                                
VROW12D  CLI   0(RE),C'*'          IF A TOTAL IS NOT SPECIFIED                  
         BE    VROWNXT                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,VROW12D                                                       
*******  OI    DRLAST,X'80'        GENERATE A SPACE BEFORE TOTALS               
*******  MVI   DRLSPACE,1                                                       
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW14   CLC   12(5,R4),=C'SKIP '  SKIP TO CHANNEL 1 AFTER BREAK                
         BNE   VROW16                                                           
         OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
         OI    DRFOPTS,DRFSKIP     WITH SKIP OPTION                             
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW16   CLC   12(6,R4),=C'SPACE ' SPACE OPTION                                 
         BNE   VROW18                                                           
         OI    DRLAST,X'80'        GENERATE LAST STATEMENT                      
         MVI   DRLSPACE,1          WITH AT LEAST ONE SPACE                      
         CLI   1(R4),0             CHECK SECOND PARAMETER                       
         BE    VROWNXT                                                          
         MVC   DRLSPACE,11(R4)                                                  
         CLI   DRLSPACE,0          S/B 1-3 LINES                                
         BE    BADROW                                                           
         CLI   DRLSPACE,3                                                       
         BH    BADROW                                                           
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW18   CLC   12(2,R4),=C'U '                                                  
         BNE   VROW20                                                           
         BAS   RE,VUSRDRON                                                      
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW20   CLC   12(3,R4),=C'NP '    OPTION NOT TO PRINT                          
         BNE   VROW22                                                           
         SPACE 1                                                                
VROW21   NI    DRFLAGO,X'7F'                                                    
         NI    DRHEAD1,X'7F'                                                    
         NI    DRHEAD2,X'7F'                                                    
         NI    DRHEAD3,X'7F'                                                    
         NI    DRHEAD4,X'7F'                                                    
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW22   CLC   12(4,R4),=C'DET '   TOTAL DETAIL EXPRESSION                      
         BE    VROW23                                                           
         CLC   12(2,R4),=C'D '     DET=N OR D=N FORMAT                          
         BNE   VROW24                                                           
         SPACE 1                                                                
VROW23   OI    DRTOTAL,X'80'                                                    
         MVI   DRTSPACE,1          SPACE AFTER TOTALS                           
         MVC   DRTDET(1),11(R4)    PICK UP NUMBER OF DETAILS                    
         CLI   DRTDET,0            MUST BE SOMETHING NUMERIC                    
         BE    BADROW                                                           
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW24   TM    2(R4),X'80'         NUMERIC=OUTPUT WIDTH OVERRIDE                
         BNO   VROW26                                                           
         L     R1,4(R4)            OUTPUT WIDTH OVERRIDE                        
         STC   R1,DRLENO           (NEW OUTPUT LENGTH)                          
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW26   B     BADROW                                                           
         SPACE 1                                                                
VROWNXT  LA    R4,32(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,VROW12                                                        
         EJECT                                                                  
*              FINAL ADJUSTMENTS                                                
         SPACE 3                                                                
         OC    TOTWIDTH,TOTWIDTH   IF WE ARE CHECKING WIDTH                     
         BZ    VROWGEN                                                          
         TM    DRFLAGO,X'80'                                                    
         BNO   VROWGEN                                                          
         LH    R1,TOTWIDTH         ADJUST CURRENT WIDTH                         
         ZIC   RF,DRLENO                                                        
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         STH   R1,TOTWIDTH                                                      
         SPACE 1                                                                
VROWGEN  CLI   OFFLINE,C'Y'        NOW GENERATE ELEMENTS                        
         BNE   XIT                                                              
         MVC   DRPOSO,MYPOSO                                                    
         SPACE 1                                                                
         TM    DRTOTAL,X'80'       WAS TOTAL REQUESTED?                         
         BNO   VROWADJ2                                                         
         CLI   DRRTNO,X'41'        IF OUT ROUTINE SPECIFIED                     
         BL    VROWADJ2                                                         
         CLI   DRLENO,14              AND LENGTH IS AT LEAST 14                 
         BL    VROWADJ2                                                         
         MVC   DRTRTN,DRRTNO       USE THIS FOR TOTAL AS WELL                   
         MVC   DRTARGS,DRARGSO     AND PASS THROUGH THE ARGUMENTS               
         MVI   DRTNARGS,16                                                      
         MVI   DRTLITLN,0                                                       
         SPACE 1                                                                
VROWADJ2 GOTO1 GROWDRON                                                         
         B     XIT                                                              
         SPACE 1                                                                
BADROW   MVC   CONHEAD(L'ROWERR),ROWERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADROW2  MVC   CONHEAD(L'COLONLY),COLONLY                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADLRANK MVC   CONHEAD(L'LRANKERR),LRANKERR                                     
         B     MYEND                                                            
         EJECT                                                                  
*              VALIDATE COLUMNS                                                 
         SPACE 3                                                                
VVALCOLS ZIC   R0,MAX                                                           
         LTR   R0,R0                                                            
         BZ    XIT                                                              
         CLI   5(R2),0                                                          
         BE    BADNEED1            NEED AT LEAST 1 COLUMN                       
         BAS   RE,DELINS           CHECK FOR DELETES/INSERT                     
         MVI   MYLABEL,C'A'                                                     
         ST    R2,ALASTCOL                                                      
         BAS   RE,SETMAX           SET LAST COLUMN FOR COMPUTE                  
*                                                                               
         XC    EDITLIST,EDITLIST   CLEAR, IN CASE RUNNING ONLINE                
*                                                                               
         LA    R3,EDITLIST                                                      
         MVI   MYCOLNUM,1                                                       
         SPACE 1                                                                
VVALCOL2 XC    MYPOSO,MYPOSO                                                    
         MVC   0(1,R3),MYLABEL     SAVE LABEL IN EDIT LIST                      
         BAS   RE,VALCOL                                                        
         BAS   RE,BUMP                                                          
         CLI   CLEXTEND,2          UNLESS THIS IS A CONTINUATION                
         BE    *+10                                                             
         MVC   MYLABEL,8(R2)       SAVE THE LABEL                               
         BAS   RE,BUMP                                                          
         LA    R3,4(R3)                                                         
         AI    MYCOLNUM,1                                                       
         BCT   R0,VVALCOL2                                                      
         CLI   TRANSOPT,C'Y'       OPTION TO READ TRANSACTIONS                  
         BNE   *+8                                                              
         MVI   FRCETRNS,C'Y'                                                    
         CLI   FRCETRNS,C'Y'       TELL ACCIO ABOUT TRANSACTIONS                
         BNE   *+8                                                              
         MVI   FCRDTRNS,C'Y'                                                    
         SPACE 1                                                                
         TM    DOWNOPT,X'80'       DON'T CHECK IF DOWN LOADING                  
         BO    XIT                                                              
         CLC   TOTWIDTH,=H'80'     CHECK NOT TOO BIG NOW                        
         BNH   XIT                                                              
         CLI   NARROPT,C'Y'        ONLY 80 ALLOWED WITH NARROW OPT              
         BE    VVALCBIG                                                         
         CLC   TOTWIDTH,=H'132'    CHECK NOT TOO BIG NOW                        
         BNH   XIT                                                              
         CLI   WIDEOPT,C'Y'                                                     
         BNE   VVALCBIG                                                         
         CLC   TOTWIDTH,=H'165'                                                 
         BNH   XIT                                                              
         SPACE 1                                                                
VVALCBIG MVC   CONHEAD(36),=C'REPORT HAS NNN CHARACTERS - TOO WIDE'             
         LA    R3,CONHEAD+11                                                    
         EDIT  (2,TOTWIDTH),(3,(R3))                                            
         L     R2,ALASTCOL                                                      
         B     MYEND                                                            
         EJECT                                                                  
*              VALIDATE FIRST COLUMN EXPRESSION                                 
         SPACE 3                                                                
VALCOL   NTR1                                                                   
         MVI   FIELDERR,1                                                       
         CLI   CLEXTEND,2          HANDLING EXTENSION HERE                      
         BNE   VCOLB                                                            
         CLI   5(R2),0                                                          
         BE    VCOLNXT2                                                         
         LA    R4,BLOCK+42+42      (DON'T DISTURB FIRST 2 ENTRIES               
         B     VCOL0               FOR EXTEND - COMPUTES MAY BE THERE)          
         SPACE 1                                                                
VCOLB    CLI   5(R2),0                                                          
         BE    XIT                                                              
         ST    R2,ALASTCOL                                                      
         XC    BLOCK(252),BLOCK                                                 
         XC    DRARGSI,DRARGSI                                                  
         LA    R4,BLOCK+42                                                      
         MVI   CLEXTEND,0                                                       
         ZIC   R1,5(R2)                                                         
         LA    R1,8-1(R1,R2)       (R1=A(LAST CHARACTER))                       
         CLI   0(R1),C','          IF THIS IS A COMMA                           
         BNE   VCOL0                                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,5(R2)            REDUCE APPARENT LENGTH                       
         MVI   CLEXTEND,1          AND NOTE THAT THERE IS AN EXTENSION          
         SPACE 1                                                                
VCOL0    GOTO1 SCANNER,DMCB,(20,(R2)),(5,(R4)),0                                
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    BADCOL                                                           
         CLI   CLEXTEND,1          EXTENSION?                                   
         BNE   VCOL1               NO                                           
         SR    R1,R1               YES,ADD 1 BACK                               
         IC    R1,5(R2)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,5(R2)                                                         
         SPACE 1                                                                
VCOL1    CLI   CLEXTEND,2                                                       
         BE    VCOL12                                                           
         GOTO1 VCOLDRON            VALIDATE A COLUMN ENTRY                      
         CLI   DRERROR,0                                                        
         BNE   VCOL4                                                            
         CLI   DRATTRIB,C'R'       ROW ONLY ENTRIES NOT ALLOWED                 
         BE    BADCOL2                                                          
         CLC   12(2,R4),=C'B '     EXTRA FOR BUDGET                             
         BE    VCOL2                                                            
         CLC   12(3,R4),=C'B0 '                                                 
         BE    VCOL2                                                            
         CLI   1(R4),0             ENTRY=XXXX IS BAD                            
         BE    VCOLNXT                                                          
         B     BADCOL                                                           
         SPACE 1                                                                
VCOL2    BAS   RE,COLBUDG                                                       
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL4    XC    BLOCK(42),BLOCK     MAY BE A COMPUTE EXPRESSION                  
         MVI   BLOCK,7                                                          
         MVC   BLOCK+12(30),MYSPACES                                            
         MVC   BLOCK+12(7),=C'COMPUTE'                                          
         LA    R4,BLOCK                                                         
         GOTO1 VCOLDRON            VALIDATE THE COMPUTE COLUMN                  
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
         LA    R4,BLOCK+42                                                      
         CLI   OFFLINE,C'Y'                                                     
         BE    VCOL6                                                            
         GOTO1 VCMPDRON            VALIDATE A COMPUTE EXPRESSION                
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
         SPACE 1                                                                
VCOL6    BAS   RE,COMPEDIT         AUTO EDIT FOR COMPUTES                       
         B     VCOLNXT                                                          
         SPACE 1                                                                
SETMAX   NTR1                                                                   
         MVI   BYTE,C'A'           FIND LAST INPUT COLUMN                       
         SPACE 1                                                                
SETMAX2  CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVC   DRCMPMAX,BYTE                                                    
         BAS   RE,BUMP                                                          
         MVC   BYTE,8(R2)                                                       
         BAS   RE,BUMP                                                          
         BCT   R0,SETMAX2                                                       
         B     XIT                                                              
         EJECT                                                                  
*              CHECK FOR SUBSIDIARY COLUMN EXPRESSIONS                          
         SPACE 3                                                                
VCOL12   TM    2(R4),X'80'         NUMERIC=OUTPUT WIDTH OVERRIDE                
         BNO   VCOL14                                                           
         CLI   7(R4),61            61,62 ARE PERIOD EXPRESSIONS                 
         BE    VCOL14                                                           
         CLI   7(R4),62                                                         
         BE    VCOL14                                                           
         L     R1,4(R4)            OUTPUT WIDTH OVERRIDE                        
         STC   R1,DRLENO           (NEW OUTPUT LENGTH)                          
         B     VCOLNXT                                                          
         SPACE 1                                                                
*                                  CHECK FOR PERIOD EXPRESSION                  
VCOL14   GOTO1 ,DMCB,12(R4),(RC)                                                
         L     RF,=V(WRIPEVAL)                                                  
         LA    RE,RELOW                                                         
         S     RE,RELOW                                                         
         AR    RF,RE                                                            
         BASR  RE,RF                                                            
         CLI   DUB,0                                                            
         BE    VCOL18                                                           
         LA    R1,DUB+1            WRIPEVAL CAN PASS BACK PERIOD                
         CLI   DUB,253             253 MEANS BINARY M/Y FOLLOWS                 
         BE    VCOL16                                                           
         CLI   DUB,250                                                          
         BH    VCOL15                                                           
         ZIC   R1,DUB              VALID PERIOD NUMBER                          
         BCTR  R1,0                CHECK TO SEE IF THIS EXPRESSION              
         SLL   R1,2                IS OK FOR ACTUAL REQUEST PERIOD              
         A     R1,ADATES                                                        
         OC    0(4,R1),0(R1)                                                    
         BNZ   VCOL16                                                           
         MVC   CONHEAD(L'PEXERR),PEXERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
VCOL15   MVI   QNEEDCD,C'T'        (TELL ACCIO WE NEED COMPRESSED)              
         MVI   FRCETRNS,C'Y'       AND WE'LL HAVE TO GET AT DETAILS             
         SPACE 1                                                                
VCOL16   MVC   DRARGSI+12(4),0(R1) PERIOD S/E BECOMES INPUT ARGS                
         MVI   DRNARGSI,16         SO WE NOW NEED ALL 16                        
         BAS   RE,PERPOP           MAY POP IN A PERIOD HEADING                  
         CLI   DUB,254             TEST COMPRESSED DATE RANGE                   
         BE    *+8                                                              
         CLI   DUB,255             DITTO                                        
         BNE   *+8                                                              
         NI    DRARGSI+14,X'FF'-X'80' TURN OFF BIT SO SYSDRIVE CAN TELL         
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL18   LA    R1,DRHEAD1          CHECK FOR HEADING OVERRIDES                  
         CLC   12(2,R4),=C'H '                                                  
         BE    VCOL20                                                           
         CLC   12(3,R4),=C'H1 '                                                 
         BE    VCOL20                                                           
         LA    R1,DRHEAD2                                                       
         CLC   12(3,R4),=C'H2 '                                                 
         BE    VCOL20                                                           
         LA    R1,DRHEAD3                                                       
         CLC   12(3,R4),=C'H3 '                                                 
         BE    VCOL20                                                           
         LA    R1,DRHEAD4                                                       
         CLC   12(3,R4),=C'H4 '                                                 
         BNE   VCOL24                                                           
         SPACE 1                                                                
VCOL20   XC    0(64,R1),0(R1)      TURN OFF ROUTINE & ARGS                      
         CLI   1(R4),0                                                          
         BE    VCOLNXT             HN= CAUSES REMOVAL                           
         OI    0(R1),X'80'         OTHERWISE TURN IT BACK ON                    
         MVC   27(1,R1),1(R4)      PASS LITERAL LENGTH TO DRONE                 
         CLC   1(1,R4),DRLENO                                                   
         BNH   VCOL22              CHECK LITERAL NOT WIDER THAN COLUMN          
         MVC   CONHEAD(L'HOVERR),HOVERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
VCOL22   MVC   28(24,R1),22(R4)    PASS DRONE THE LITERAL                       
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL24   CLC   12(3,R4),=C'NP '    OPTION NOT TO PRINT                          
         BNE   VCOL28                                                           
         MVI   MYPOSO,C'N'                                                      
         NI    DRHEAD1,X'7F'                                                    
         NI    DRHEAD2,X'7F'                                                    
         NI    DRHEAD3,X'7F'                                                    
         NI    DRHEAD4,X'7F'                                                    
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL28   CLC   12(5,R4),=C'THOU '  THOUSAND OPTION                              
         BNE   VCOL30                                                           
         MVI   DRDIVO,5                                                         
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL30   CLC   12(8,R4),=C'BRACKET '   (MINUS NUMBERS)                          
         BNE   VCOL32                                                           
         OI    DROPTSO,DRBKMINO                                                 
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL32   CLC   12(2,R4),=C'U '     USER RECORD                                  
         BNE   VCOL33                                                           
         BAS   RE,VUSRDRON                                                      
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL33   CLC   12(4,R4),=C'DEC '   DECIMAL POINTS                               
         BNE   VCOL34                                                           
         L     R1,8(R4)                                                         
         STC   R1,DRDECO                                                        
         CLC   22(2,R4),=C'0 '                                                  
         BE    VCOLNXT                                                          
         LTR   R1,R1                                                            
         BZ    BADCOL                                                           
         CH    R1,=H'5'                                                         
         BH    BADCOL                                                           
         B     VCOLNXT                                                          
         EJECT                                                                  
*              SUPPORT COLUMN FILTERS                                           
         SPACE 3                                                                
VCOL34   LA    RF,1                SET FILTER TYPE VALUE IN RF                  
         CLC   12(3,R4),=C'F1 '                                                 
         BE    VCOL36              F1 TO F4 ARE FILTERS 1-4                     
         LA    RF,2                                                             
         CLC   12(3,R4),=C'F2 '                                                 
         BE    VCOL36                                                           
         LA    RF,3                                                             
         CLC   12(3,R4),=C'F3 '                                                 
         BE    VCOL36                                                           
         LA    RF,4                                                             
         CLC   12(3,R4),=C'F4 '                                                 
         BE    VCOL36                                                           
         LA    RF,5                                                             
         CLC   12(3,R4),=C'CL '                                                 
         BE    VCOL36              5=CONTRA LEDGER                              
         LA    RF,6                                                             
         CLC   12(3,R4),=C'ME '                                                 
         BE    VCOL36              6=MEDIA                                      
         LA    RF,7                                                             
         CLC   12(3,R4),=C'TT '                                                 
         BE    VCOL35              7=TIME TYPE                                  
         LA    RF,8                                                             
         CLC   12(3,R4),=C'MG '                                                 
         BE    VCOL36              8=MEDIA GROUP                                
         LA    RF,9                                                             
         CLC   12(3,R4),=C'OG '                                                 
         BE    VCOL36              9=OFFICE  GROUP                              
         LA    RF,10                                                            
         CLC   12(3,R4),=C'WG '                                                 
         BE    VCOL36              10=WORK GROUP                                
         LA    RF,11                                                            
         CLC   12(3,R4),=C'RT '                                                 
         BE    VCOL35              11=RTYPE                                     
         SPACE 1                                                                
*                                  FILTERS OVER 20 ARE LONGER                   
         LA    RF,20                                                            
         CLC   12(3,R4),=C'AC '                                                 
         BE    VCOL36              20=ACCOUNT CODE                              
         LA    RF,21                                                            
         CLC   12(3,R4),=C'CA '                                                 
         BE    VCOL36              21=CONTRA ACCOUNT                            
         LA    RF,22                                                            
         CLC   12(3,R4),=C'WC '                                                 
         BE    VCOL36              22=WORK CODE                                 
         LA    RF,23                                                            
         CLC   12(3,R4),=C'AT '                                                 
         BE    VCOL35              23=ATTRIBUTE                                 
         LA    RF,24                                                            
         CLC   12(3,R4),=C'BT '                                                 
         BE    VCOL35              24=BATCH TYPE                                
         LA    RF,25                                                            
         CLC   12(3,R4),=C'OF '                                                 
         BE    VCOL35              25=OFFICE CODE                               
         B     VCOL44              THAT'S IT FOR NOW                            
         SPACE 1                                                                
VCOL35   MVI   FRCETRNS,C'Y'       FILTER REQUIRES DETAIL LEVEL                 
         CLC   12(3,R4),=C'BT '                                                 
         BNE   VCOL36              ADJUST BT= EXPRESSIONS                       
         LA    RE,22(R4)                                                        
         CLI   0(RE),C'-'                                                       
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         CLI   1(RE),C' '                                                       
         BH    VCOL36                                                           
         MVC   1(1,RE),0(RE)                                                    
         MVI   0(RE),C'0'                                                       
         SPACE 1                                                                
VCOL36   LA    RE,DRARGSI+6        UP TO 3 SLOTS SUPPORTED                      
         CLI   0(RE),0                                                          
         BE    VCOL38                                                           
         LA    RE,DRARGSI+8                                                     
         CLI   0(RE),0                                                          
         BE    VCOL38                                                           
         LA    RE,DRARGSI+10                                                    
         CLI   0(RE),0                                                          
         BE    VCOL38                                                           
         B     BADCOL                                                           
         SPACE 1                                                                
VCOL38   STC   RF,0(RE)            SAVE FILTER TYPE NUMBER                      
         MVC   1(1,RE),22(R4)      AND VALUE                                    
         MVI   DRNARGSI,16         ENSURE FULL ARGUMENTS                        
         CLI   22(R4),X'41'                                                     
         BL    BADCOL              MUST BE SOME VALUE                           
         CLI   0(RE),20            SPECIAL STUFF FOR LONGER FILTERS             
         BL    VCOL40                                                           
         MVC   1(1,RE),MYCOLNUM    SAVE COLUMN NUMBER                           
         ZIC   RF,0(RE)                                                         
         SH    RF,=H'20'                                                        
         SLL   RF,8                INDEX INTO FILTER SCHEME (*256)              
         ZIC   R1,MYCOLNUM         COLUMN INDEX                                 
         BCTR  R1,0                                                             
         SLL   R1,4                16 BYTES FOR EACH COLUMN                     
         AR    R1,RF                                                            
         A     R1,AACCFILT                                                      
         MVC   0(16,R1),22(R4)     AND SAVE FILTERED DATA                       
         CLI   22(R4),C'-'                                                      
         BNE   VCOLNXT                                                          
         MVC   0(16,R1),23(R4)     (NEGATIVE ACCOUNT)                           
         NI    0(R1),X'BF'                                                      
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL40   LA    R1,23(R4)                                                        
         CLI   22(R4),C'-'         CAN BE NEGATIVE FILTER                       
         BNE   VCOL42                                                           
         LA    R1,1(R1)                                                         
         CLI   23(R4),X'41'                                                     
         BL    BADCOL                                                           
         MVC   1(1,RE),23(R4)                                                   
         NI    1(RE),X'BF'         TURN OFF X'40' BIT                           
         SPACE 1                                                                
VCOL42   CLI   0(R1),C' '          MAKE SURE EXPRESSION NOT TOO LONG            
         BH    BADFILT2                                                         
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL44   CLC   12(4,R4),=C'CUME'   CUME OUPTUT OPTION                           
         BNE   VCOL45                                                           
         OI    DROPTSO+1,DRCUME                                                 
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL45   CLC   12(8,R4),=C'DOWNCAST'    DOWNCAST COMPUTE OPTION                 
         BNE   VCOL46                                                           
         OI    DROPTSO+1,DRDWNCMP                                               
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL46   CLC   12(4,R4),=C'RANK'   (COLUMN) RANK                                
         BNE   VCOL48                                                           
         MVI   COLRANK,1                                                        
         CLI   16(R4),C'-'                                                      
         BE    VCOLNXT                                                          
         CLI   22(R4),C'-'                                                      
         BE    VCOLNXT                                                          
         MVI   COLRANK,2                                                        
         B     VCOLNXT                                                          
         SPACE 1                                                                
*COLRANK  DC    X'00'                                                           
         SPACE 1                                                                
VCOL48   BAS   RE,VCOLMXMN         MAY BE MAX OR MIN EXPRESSION                 
         BE    VCOLNXT                                                          
         SPACE 1                                                                
VCOLBAD  B     BADCOL                                                           
         SPACE 1                                                                
VCOLNXT  LA    R4,42(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,VCOL12                                                        
         SPACE 1                                                                
         CLI   CLEXTEND,1          IF THERE IS AN EXTENSION PENDING             
         BNE   VCOLNXT2                                                         
         MVI   CLEXTEND,2             NOT TIME TO WRAP UP YET                   
         B     XIT                                                              
         SPACE 1                                                                
VCOLNXT2 MVI   CLEXTEND,0                                                       
         SPACE 1                                                                
         BAS   RE,ARGADJ           MAY ADJUST ARGUMENTS FOR SPECIALS            
         BAS   RE,OPTADJ           ADJUSTMENTS FROM OPTIONS                     
         SPACE 1                                                                
         CLI   MYPOSO,C'N'         IF THERE IS ANY PRINTING                     
         BE    VCOLGEN                                                          
         LH    R1,TOTWIDTH         ADJUST CURRENT WIDTH                         
         ZIC   RF,DRLENO                                                        
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         STH   R1,TOTWIDTH                                                      
         SPACE 1                                                                
VCOLGEN  CLI   OFFLINE,C'Y'        NOW GENERATE ELEMENTS                        
         BNE   XIT                                                              
         MVC   DRLABELI,MYLABEL                                                 
         MVC   1(1,R3),DRDECO      SAVE EDIT CHARACTERISTICS                    
         MVC   2(1,R3),DRDIVO                                                   
         MVC   DRPOSO,MYPOSO                                                    
         GOTO1 GCOLDRON                                                         
         CLC   BLOCK+12(7),=C'COMPUTE'                                          
         BNE   VCOLGEN2                                                         
         LA    R4,BLOCK+42                                                      
         GOTO1 GCMPDRON                                                         
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
         SPACE 1                                                                
VCOLGEN2 BAS   RE,ANYMOCOL                                                      
         B     XIT                                                              
         SPACE 1                                                                
BADCOL   MVC   CONHEAD(L'COLERR),COLERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADUSER  MVC   CONHEAD(L'USRERR),USRERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADCOL2  MVC   CONHEAD(L'ROWONLY),ROWONLY                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADNEED1 MVC   CONHEAD(L'NEED1ERR),NEED1ERR                                     
         B     MYEND                                                            
         EJECT                                                                  
*              ANY MORE COLUMNS TO GENERATE                                     
         SPACE 3                                                                
ANYMOCOL NTR1                                                                   
         CLI   COLRANK,0           COLUMN RANKING                               
         BE    XIT                                                              
         CLI   OFFLINE,C'Y'                                                     
         BNE   XIT                                                              
         XC    BLOCK(12),BLOCK     FOR DATE TOTAL/DETAIL EXPRESSIONS            
         MVI   BLOCK,7                                                          
         MVC   BLOCK+12(30),MYSPACES                                            
         MVC   BLOCK+12(7),=C'COLRANK'                                          
         LA    R4,BLOCK                                                         
         GOTO1 VCOLDRON                                                         
         MVC   DRARGSI(1),COLRANK       PASS ARGUMENT                           
         MVI   DRNARGSI,1                                                       
         MVI   COLRANK,0                                                        
*****    OI    DROPTSO+1,DRNOLBOX  NO BOX TO LEFT OF RANK                       
         GOTO1 GCOLDRON                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FIGURE OUT EDITS FOR COMPUTES                         
         SPACE 3                                                                
COMPEDIT NTR1                                                                   
         CLI   BRACKOPT,C'Y'       IF USER ASKED FOR BRACKETS                   
         BNE   *+12                                                             
         MVI   DRFLOATO,0          DO NOT FLOAT IN MINUS AS WELL!               
         OI    DROPTSO,DRBKMINO    AND ENSURE BRACKET OPTION                    
*                                  R4=A(SCANNER TABLE ENTRY)                    
         ZIC   R1,0(R4)            PICK UP EXPRESSION LENGTH                    
         LA    R1,10(R1,R4)                                                     
         CLI   0(R1),C'%'          IS LAST OPERATOR PERCENT?                    
         BE    COMPPCT                                                          
         CLI   0(R1),C'I'          OR INDEX?                                    
         BE    COMPINX                                                          
         BCTR  R1,0                                                             
         CLI   0(R1),C'V'          OR VERTICAL PERCENT                          
         BE    COMPPCT                                                          
         LA    RE,8(R2)            RE=A(START OF INPUT STRING)                  
         ZIC   R0,5(R2)            R0=LENGTH OF INPUT STRING                    
         SPACE 1                                                                
COMPED1  LA    R1,EDITLIST         ELSE LOOK FOR FIRST OPERAND                  
         SPACE 1                                                                
COMPED2  CLI   0(R1),0                                                          
         BNE   COMPED2B                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,COMPED1                                                       
         B     XIT                 NO VARIABLES IN INPUT STRING                 
         SPACE 1                                                                
COMPED2B CLC   0(1,R1),0(RE)                                                    
         BE    COMPED3                                                          
         LA    R1,4(R1)                                                         
         B     COMPED2                                                          
         SPACE 1                                                                
COMPED3  MVC   DRDECO,1(R1)        PICK UP ITS EDIT CHARACTERISTIC              
         MVC   DRDIVO,2(R1)                                                     
         B     XIT                                                              
         SPACE 1                                                                
COMPPCT  MVI   DRDECO,2            PERCENTS HAVE 2 DEC                          
         CLI   BRACKOPT,C'Y'       AND, UNLESS BRACKETS SPECIFIED,              
         BE    *+8                                                              
         MVI   DRTRAILO,C'%'            END WITH PERCENT SIGN                   
         B     XIT                                                              
         SPACE 1                                                                
COMPINX  MVI   DRDECO,0            INDEXES HAVE 0 DEC                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO RESOLVE BUDGET PARAMETERS                             
         SPACE 3                                                                
*              INPUT               R4=A(SCANNER BLOCK)                          
*                                  BUDGET CODE/NAME AT 22(R4)                   
         SPACE 1                                                                
COLBUDG  NTR1                                                                   
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         USING ACBTKEY,R5                                                       
         MVI   ACBTKTYP,ACBTKTEQ                                                
         MVC   ACBTKCMP,WRICOMP                                                 
         MVC   ACBTKNO1,10(R4)     MAY BE A NUMBER                              
         MVC   DRARGSI(2),10(R4)   PASS BUDGET NUMBER AS ARGUMENT               
         MVI   DRNARGSI,2                                                       
         OC    8(4,R4),8(R4)                                                    
         BNZ   COLB2                                                            
         MVC   ACBTKCOD,22(R4)     OTHERWISE USE A CODE                         
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE     NEED TO MATCH THROUGH CODE                   
         BNE   NOBUD                                                            
         MVC   DRARGSI(2),ACBTKNO2 PASS BUDGET NUMBER AS ARGUMENT               
         B     COLB4                                                            
         SPACE 1                                                                
COLB2    GOTO1 HIGH                READING FOR NUMERIC BUDGET                   
         CLC   KEY(5),KEYSAVE                                                   
         BE    COLB4                                                            
         SPACE 1                                                                
NOBUD    MVC   CONHEAD(L'NOBUDERR),NOBUDERR                                     
         B     MYCURSOR                                                         
         SPACE 1                                                                
COLB4    CLI   QBUDGET,0           IS THIS THE FIRST BUDGET REQUESTED           
         BNE   COLB6                                                            
         MVC   QBUDGET,DRARGSI     THEN TELL ACCIO TO FILTER THIS               
         B     COLB10                                                           
         SPACE 1                                                                
COLB6    CLC   QBUDGET,DRARGSI     IS THIS THE SAME AS BEFORE?                  
         BE    COLB10                                                           
         MVC   QBUDGET,=X'FFFF'       NO - TELL ACCIO ALL                       
         SPACE 1                                                                
COLB10   GOTO1 GETL,DMCB,(X'1B',AIO),0                                          
         CLI   WRIELERR,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,WRIELAD                                                       
         USING ACBCD,R3                                                         
         CLI   ACBCCOL1,X'41'      IS THERE A HEADING                           
         BL    XIT                                                              
         OI    DRHEAD1,X'80'       PASS THROUGH HEADING DETAILS                 
         MVI   DRH1LITL,10         (WIDTH OF 10)                                
         MVC   DRH1LIT(10),ACBCCOL1                                             
         SPACE 1                                                                
         CLI   ACBCCOL2,X'41'      MAY BE A SECOND HEADING                      
         BL    XIT                                                              
         OI    DRHEAD2,X'80'       PASS THROUGH HEADING DETAILS                 
         MVI   DRH2LITL,10         (WIDTH OF 10)                                
         MVC   DRH2LIT(10),ACBCCOL2                                             
         B     XIT                                                              
         DROP  R3                                                               
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE TO ADJUST PERIOD                                         
         SPACE 3                                                                
PERADJ   NTR1                                                                   
         CLC   WORK(6),=C'000000' IF START NOT SPECIFIED                        
         BNE   PERADJ2                                                          
         MVC   WORK(2),WORK+6      COPY YEAR FROM END                           
         MVC   WORK+2(2),=C'01'    USE JANUARY                                  
         MVC   WORK+4(2),=C'01'    MAKE DAY 01                                  
         B     XIT                                                              
         SPACE 1                                                                
PERADJ2  CLC   WORK+6(6),=C'000000' IF END NOT SPECIFIED                        
         BNE   XIT                                                              
         MVC   WORK+6(2),WORK      COPY YEAR FROM START                         
         MVC   WORK+6+2(2),=C'12'  USE DECEMBER                                 
         MVC   WORK+6+4(2),=C'31'  MAKE DAY 31                                  
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO ADJUST ARGUMENTS                                      
         SPACE 3                                                                
ARGADJ   NTR1                                                                   
         CLC   DRRTNI(6),=C'BUCKET'                                             
         BNE   XIT                                                              
         CLI   DRARGSI,C'A'                                                     
         BNE   ARGADJ2                                                          
         XC    DRARGSI+12(2),DRARGSI+12                                         
         CLI   DRARGSI+14,0                                                     
         BNE   *+10                                                             
         MVC   DRARGSI+14(2),QMOSEND                                            
         B     XIT                                                              
         SPACE 1                                                                
ARGADJ2  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PUT IN PERIOD HEADING SUPPORT                         
         SPACE 3                                                                
PERPOP   NTR1                                                                   
         CLI   MYPOSO,C'N'         NOT NEEDED IF NOT PRINTING                   
         BE    XIT                                                              
         CLI   DRARGSI+14,0        WAS A PERIOD SPECIFIED?                      
         BE    XIT                                                              
         LA    R1,DRHEAD1          YES SO FIND AN EMPTY HEADING                 
         TM    0(R1),X'80'                                                      
         BNO   PERPOP2                                                          
         LA    R1,DRHEAD2                                                       
         TM    0(R1),X'80'                                                      
         BNO   PERPOP2                                                          
         LA    R1,DRHEAD3                                                       
         TM    0(R1),X'80'                                                      
         BNO   PERPOP2                                                          
         LA    R1,DRHEAD4                                                       
         TM    0(R1),X'80'                                                      
         BNO   PERPOP2                                                          
         B     XIT                 NO ROOM!                                     
         SPACE 1                                                                
PERPOP2  OI    0(R1),X'80'         FOUND A SPACE - SO TURN ON                   
         MVC   1(8,R1),=CL8'PERPOP'                                             
         MVC   9(4,R1),DRARGSI+12  PASS THE PERIOD S/E                          
         MVC   13(1,R1),DUB        (WRIPEVAL PASSED THIS BACK)                  
         MVC   14(11,R1),22(R4)    (AGE NEEDS EXPRESSION TOO)                   
         MVI   25(R1),16           ALL ARGUMENTS                                
         B     XIT                                                              
         SPACE 3                                                                
*              ADJUST DRONE BLOCK FOR OPTIONS                                   
         SPACE 1                                                                
OPTADJ   NTR1                                                                   
         CLI   THOUOPT,C'N'        OPTION TO SHOW ALL IN THOUSANDS              
         BE    OPTADJ2                                                          
         MVI   DRDIVO,5                                                         
         MVI   DRDECO,0                                                         
         CLI   THOUOPT,C'Y'                                                     
         BE    OPTADJ2                                                          
         MVI   DRDIVO,4            OPTION TO SHOW 1 DEC PLACE                   
         MVI   DRDECO,1                                                         
         CLI   THOUOPT,C'1'                                                     
         BE    OPTADJ2                                                          
         MVI   DRDIVO,3            ELSE ITS 2 DEC PLACES                        
         MVI   DRDECO,2                                                         
         SPACE 1                                                                
OPTADJ2  CLI   BRACKOPT,C'Y'       OPTION TO SHOW MINUS BRACKETED               
         BNE   *+8                                                              
         OI    DROPTSO,DRBKMINO                                                 
         OC    DROPTSO,OUTOPTS     PASS THROUGH OTHER OPTIONS                   
         B     XIT                                                              
         EJECT                                                                  
*              GENERAL HEADLINE HOOK ROUTINES                                   
         SPACE 3                                                                
VGENHEAD L     R2,AH1              DEAL WITH MAIN TITLE                         
         LA    R2,48(R2)                                                        
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,16(R2)                                                        
         CLI   NARROPT,C'Y'                                                     
         BNE   VGENHB                                                           
         SH    R2,=H'47'                                                        
         L     R1,AH4                                                           
         MVC   BLOCK(24),59(R1)    SAVE H4 RIGHT FOR NARROW                     
         MVC   59(24,R1),MYSPACES                                               
         SPACE 1                                                                
VGENHB   MVC   0(32,R2),TITLE      (TITLES ARE ALREADY CENTERED)                
         A     R2,PWIDTH                                                        
         GOTO1 UNDERLIN,DMCB,(32,TITLE),(X'BF',(R2))                            
         A     R2,PWIDTH                                                        
         MVC   0(32,R2),SUBTITLE   AND THE SUBTITLE                             
         SPACE 1                                                                
         L     R2,AH4              LINE UP THE LEFT SIDE                        
         LA    R2,1(R2)                                                         
         LA    R3,15                                                            
         L     R1,AGLOBAL                                                       
         USING GLOBALD,R1                                                       
         ZIC   R4,GLFHEADL                                                      
         DROP  R1                                                               
         SH    R4,=H'6'                                                         
         BAS   RE,GETLONG                                                       
         LA    R3,16(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BAS   RE,SHUFFLE                                                       
         LA    R3,15                                                            
         BAS   RE,GETLONG                                                       
         LA    R3,16(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BAS   RE,SHUFFLE                                                       
         SPACE 1                                                                
         L     R2,AH4              PERIOD TO HEAD5 RIGHT                        
         LA    R2,96(R2)                                                        
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,32(R2)                                                        
         CLI   NARROPT,C'Y'                                                     
         BNE   *+14                                                             
         SH    R2,=H'37'                                                        
         MVC   0(24,R2),BLOCK      (REPLACE SAVED H4 RIGHT)                     
         A     R2,PWIDTH                                                        
         MVC   0(6,R2),=C'PERIOD'                                               
         LA    RF,7(R2)                                                         
         L     RE,APERFLD          PICK UP PERIOD FROM SCREEN                   
         LTR   RE,RE                                                            
         BZ    VGENH2                                                           
         ZIC   R1,5(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(RE)                                                    
         SPACE 1                                                                
VGENH2   CLI   PERTITLE,X'41'      USER CAN OVERWRITE PERIOD                    
         BL    *+10                                                             
         MVC   0(32,R2),PERTITLE                                                
         CLC   PERTITLE(5),=C'CREAM'     OR CREAM IT!                           
         BNE   *+10                                                             
         MVC   0(32,R2),MYSPACES                                                
         CLC   PERTITLE(5),=C'BLANK'     UK LIKED BLANK!                        
         BNE   *+10                                                             
         MVC   0(32,R2),MYSPACES                                                
         A     R2,PWIDTH                                                        
         MVC   0(32,R2),RTITLE     RIGHT TITLE ON HEAD6                         
         B     XIT                                                              
         EJECT                                                                  
*              SUBSIDIARY ROUTINES FOR GENHEAD                                  
         SPACE 3                                                                
GETLONG  NTR1                                                                   
*              INPUTS              R2=A(FIELD ON FIRST LINE)                    
*                                  R3=MAX WIDTH                                 
*                                  R4=NUMBER OF LINES                           
*              OUTPUT              FULL=WIDEST FOUND                            
         SPACE 1                                                                
GETLONG2 ST    R3,FULL                                                          
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         LA    R1,0(R2,R3)                                                      
         BCTR  R1,0                R1=END OF PRESENT FIELD                      
         LR    R0,R4                                                            
         SPACE 1                                                                
GETLONG4 CLI   0(R1),C' '          SEE IF ANYTHING SIGNIFICANT                  
         BH    XIT                                                              
         A     R1,PWIDTH           ON EACH OF THE LINES                         
         BCT   R0,GETLONG4                                                      
         BCTR  R3,0                                                             
         B     GETLONG2                                                         
         SPACE 3                                                                
SHUFFLE  NTR1                                                                   
*              INPUTS              R2=A(START DATA ON FIRST LINE)               
*                                  R3=A(FROM DATA)                              
*                                  R4=NUMBER OF LINES                           
         SPACE 1                                                                
SHUFFLE2 MVC   WORK,0(R3)                                                       
         MVC   0(60,R3),MYSPACES                                                
         MVC   0(60,R2),WORK                                                    
         A     R2,PWIDTH                                                        
         A     R3,PWIDTH                                                        
         BCT   R4,SHUFFLE2                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MAINTAIN ACCOUNT NAME BUFFER                          
         SPACE 3                                                                
*              PARAMETERS          1 A(ACCOUNT CODE CL15)                       
*                                  2 A(36 BYTE OUTPUT AREA)                     
         SPACE 1                                                                
VGETNAME LM    R2,R3,0(R1)         R2=INPUT R3=OUTPUT                           
         L     R4,ANAMPOOL         R4=POOL HEADER                               
*                                  CONTAINS PRESENT/MAX/WIDTH                   
         LA    R5,12(R4)           R5=POOL ENTRY                                
         L     R0,4(R4)            R0=MAX N'ENTRIES                             
         SPACE 1                                                                
VGN2     CLI   0(R5),0             IS ENTRY EMPTY                               
         BE    VGNGET              YES - GO AND GET ACCOUNT                     
         CLC   0(15,R5),0(R2)      CAN WE FIND ACCOUNT                          
         BE    VGNEND              YES - PASS BACK NAME                         
         A     R5,8(R4)                                                         
         BCT   R0,VGN2                                                          
         SPACE 1                                                                
VGNGET   L     R1,0(R4)            PICK UP POOL POINTER                         
         LA    R1,1(R1)            +1                                           
         C     R1,4(R4)            CHECK AGAINST POOL MAX                       
         BNH   *+8                                                              
         LA    R1,1                RESET TO START OF POOL                       
         ST    R1,0(R4)            AND SAVE THIS                                
         BCTR  R1,0                                                             
         MH    R1,10(R4)           DISPLACE TO THIS ENTRY                       
         LA    R5,12(R4,R1)                                                     
         MVC   0(15,R5),0(R2)      SAVE ACCOUNT NUMBER                          
         MVC   15(36,R5),MYSPACES  PRECLEAR ACCOUNT NAME                        
         XC    KEY,KEY                                                          
         MVC   KEY(15),0(R2)                                                    
         CLI   0(R2),X'01'         NEW OFFICES PASSED AS X'01'                  
         BNE   VGNGET2                                                          
         MVC   KEY,MYSPACES                                                     
         MVC   KEY(2),0(R2)                                                     
         MVC   KEY+40(2),2(R2)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(42),KEYSAVE                                                  
         BNE   VGNREST                                                          
         B     VGNGET4                                                          
         SPACE 1                                                                
VGNGET2  GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE                                                  
         BNE   VGNREST                                                          
         CLI   0(R2),X'09'         CHECK FOR MEDIA RECORDS                      
         BE    VGNMED                                                           
         CLI   0(R2),X'0A'         AND FOR WORK CODE RECORDS                    
         BE    VGNWORK                                                          
         SPACE 1                                                                
VGNGET4  GOTO1 NAMOUT,DMCB,AIO,15(R5)                                           
         B     VGNREST                                                          
         SPACE 1                                                                
VGNMED   GOTO1 GETL,DMCB,(X'11',AIO),0     MEDIA ELEMENTS                       
         CLI   WRIELERR,0                                                       
         BNE   VGNREST                                                          
         L     R1,WRIELAD                                                       
         USING ACMDEL,R1                                                        
         MVC   15(12,R5),ACMDDESC+3                                             
*&&UK*&& MVC   WORK(12),15(R5)                                                  
*&&UK*&& BAS   RE,GETALTN                                                       
*&&UK*&& MVC   15(12,R5),WORK                                                   
         B     VGNREST                                                          
*&&UK                                                                           
GETALTN  NTR1                                                                   
         CLI   ALTNOPT,C'Y'        ALTERNATE NAME OPTION (UK)                   
         BNE   XIT                                                              
         GOTO1 GETL,DMCB,(X'56',AIO),0                                          
         CLI   WRIELERR,0                                                       
         BNE   XIT                 NO ALT NAME                                  
         L     R4,WRIELAD                                                       
         SPACE 1                                                                
         USING ACXNMD,R4                                                        
         ZIC   R5,ACXNMLEN                                                      
         SH    R5,=H'5'                                                         
         BM    XIT                                                              
         MVC   WORK,MYSPACES                                                    
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ACXNMSN                                                  
         B     XIT                                                              
*&&                                                                             
         DROP  R1                                                               
         SPACE 1                                                                
VGNWORK  GOTO1 GETL,DMCB,(X'12',AIO),0     WORK CODE ELEMENT                    
         CLI   WRIELERR,0                                                       
         BNE   VGNREST                                                          
         L     R1,WRIELAD                                                       
         USING ACANALD,R1                                                       
         MVC   15(15,R5),ACANDESC                                               
*&&UK*&& MVC   WORK(15),15(R5)                                                  
*&&UK*&& BAS   RE,GETALTN                                                       
*&&UK*&& MVC   15(15,R5),WORK                                                   
         DROP  R1                                                               
         SPACE 1                                                                
VGNREST  CLI   ACMODE,REQLAST      IS ACCIO READING RECORDS                     
         BE    VGNEND                                                           
         MVC   KEY,ACIOKEY                                                      
         GOTO1 HIGH                RESTORE SEQUENCE FOR ACCIO                   
         SPACE 1                                                                
VGNEND   MVC   0(36,R3),15(R5)     RETURN NAME TO USER                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MAINTAIN HEIRARCHY BUFFER                             
         SPACE 3                                                                
*              PARAMETERS          1 A(ACCOUNT CODE CL15)                       
*                                  2 A(66 BYTE OUTPUT AREA)                     
*                                  COVERED BY ACHEIRD                           
         SPACE 1                                                                
VGETHEIR LM    R2,R3,0(R1)         R2=INPUT R3=OUTPUT                           
         L     R4,ALDGPOOL         R4=POOL HEADER                               
*                                  CONTAINS PRESENT/MAX/WIDTH                   
         LA    R5,12(R4)           R5=POOL ENTRY                                
         L     R0,4(R4)            R0=MAX N'ENTRIES                             
         SPACE 1                                                                
VGH2     CLI   0(R5),0             IS ENTRY EMPTY                               
         BE    VGHGET              YES - GO AND GET LEDGER                      
         CLC   0(3,R5),0(R2)       IS LEDGER HERE ALREADY?                      
         BE    VGHEND              YES - PASS BACK HEIRARCHY                    
         A     R5,8(R4)                                                         
         BCT   R0,VGH2                                                          
         SPACE 1                                                                
VGHGET   L     R1,0(R4)            PICK UP POOL POINTER                         
         LA    R1,1(R1)            +1                                           
         C     R1,4(R4)            CHECK AGAINST POOL MAX                       
         BNH   *+8                                                              
         LA    R1,1                RESET TO START OF POOL                       
         ST    R1,0(R4)            AND SAVE THIS                                
         BCTR  R1,0                                                             
         MH    R1,10(R4)           DISPLACE TO THIS ENTRY                       
         LA    R5,12(R4,R1)                                                     
         MVC   0(3,R5),0(R2)       SAVE UNIT AND LEDGER                         
         XC    3(64,R5),3(R5)      PRECLEAR HEIRARCHY ELEMENT                   
         MVI   3(R5),12                                                         
         MVC   4(15,R5),=CL15'ACCOUNT'                                          
         MVC   KEY,MYSPACES                                                     
         MVC   KEY(3),0(R2)                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE                                                  
         BNE   VGHREST                                                          
         GOTO1 GETL,DMCB,(X'16',AIO),0                                          
         CLI   WRIELERR,0                                                       
         BNE   VGHREST                                                          
         L     R1,WRIELAD                                                       
         MVC   3(64,R5),2(R1)                                                   
         SPACE 1                                                                
VGHREST  CLI   ACMODE,REQLAST      IS ACCIO READING RECORDS                     
         BE    VGHEND                                                           
         MVC   KEY,ACIOKEY                                                      
         GOTO1 HIGH                RESTORE SEQUENCE FOR ACCIO                   
         SPACE 1                                                                
VGHEND   MVC   0(2,R3),=X'1642'    RETURN ELEMENT TO USER                       
         MVC   2(64,R3),3(R5)                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DEDUCE ACCOUNT CODE AT SELECTED LEVEL                 
         SPACE 3                                                                
*              PARAMETERS       R5 1 1 LEVEL NUMBER                             
*                               R2   2 A(ACCOUNT CODE CL15)                     
*                               R3 2 A(CODE AT SELECTED LEVEL CL15)             
*                               R4 3 A(PIECE OF CODE AT LEVEL CL12)             
         SPACE 1                                                                
VGETCODE LM    R2,R4,0(R1)                                                      
         ZIC   R5,0(R1)                                                         
         MVC   0(15,R3),0(R2)                                                   
         MVC   0(12,R4),3(R2)                                                   
         LTR   R5,R5               LEVEL ZERO GETS WHOLE CODE                   
         BZ    XIT                                                              
         MVC   0(15,R3),MYSPACES                                                
         MVC   0(12,R4),MYSPACES                                                
         GOTO1 GETHEIR,DMCB,(R2),ELEMENT                                        
         BAS   RE,SETLEV                                                        
         LR    RF,R5                                                            
         BCTR  RF,0                                                             
         SLL   RF,4                (LEVEL-1)*16                                 
         LA    RF,ELEMENT+2(RF)                                                 
         ZIC   R1,0(RF)            SHOULD BE LENGTH AT LEVEL N                  
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         CH    R1,=H'12'                                                        
         BH    XIT                                                              
         LA    R1,2(R1)            GOING TO PASS CUL AND ACCOUNT                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)                                                    
         SPACE 1                                                                
*                                  NOW RETURN JUST THE LOW LEVEL PIECE          
         LA    R3,3(R3)            ADDRESS OF ACCOUNT CODE ONLY                 
         MVC   0(12,R4),0(R3)      PRESET TO RETURN JUST THE                    
         CH    R5,=H'1'            ACCOUNT IF LEVEL IS 1                        
         BE    XIT                                                              
         MVC   0(12,R4),MYSPACES                                                
         SH    RF,=H'16'           BACK UP TO PREVIOUS HEIRARCHY                
         ZIC   R0,0(RF)            PICK UP THIS LEVEL                           
         SR    R1,R0               REDUCE LENGTH                                
         SH    R1,=H'3'            BY PREVIOUS LEVEL AND C/U/L                  
         AR    R3,R0               AND ADD INTO FROM ADDRESS                    
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R4),0(R3)                                                    
         EJECT                                                                  
*              ROUTINE TO DEDUCE LABEL AT SELECTED LEVEL                        
         SPACE 3                                                                
*              PARAMETERS          1 1 LEVEL NUMBER                             
*                                    2 A(CUL CL3)                               
*                                  2 A(LABEL CL15)                              
         SPACE 1                                                                
VGETLABL LM    R2,R3,0(R1)                                                      
         ZIC   R5,0(R1)                                                         
         GOTO1 GETHEIR,DMCB,(R2),ELEMENT                                        
         BAS   RE,SETLEV                                                        
         LR    RF,R5                                                            
         BCTR  RF,0                                                             
         SLL   RF,4                (LEVEL-1)*16                                 
         LA    RF,ELEMENT+3(RF)    POSITION TO LABEL AT LEVEL 1-4               
         MVC   0(15,R3),0(RF)                                                   
         B     XIT                                                              
         SPACE 1                                                                
SETLEV   LTR   R5,R5               IF LEVEL (R5) IS NOT YET SET                 
         BNZR  RE                                                               
         LA    R5,4                                                             
         LA    R1,ELEMENT                                                       
         USING ACHEIRD,R1                                                       
         LA    R1,ACHRLEVD                                                      
         SPACE 1                                                                
SETLEV2  CLI   0(R1),1             FIND LOWEST LEVEL                            
         BL    SETLEV4                                                          
         CLI   0(R1),12                                                         
         BH    SETLEV4                                                          
         BR    RE                                                               
         SPACE 1                                                                
SETLEV4  SH    R1,=H'16'                                                        
         BCT   R5,SETLEV2                                                       
         DC    H'0'                                                             
         EJECT                                                                  
*              DRONE UTILITIES                                                  
         SPACE 3                                                                
VINTDRON DS    0H                  INITIALIZATION                               
         MVI   DRWHO,DRACCWHO                                                   
         MVI   DRACTION,DRINIT                                                  
         MVC   DRDICT,=CL8'ACCOUNT'                                             
         MVC   DRALTDIC,=CL8'DRIVER'                                            
         MVC   DRCOMFAC,ACOMFACS                                                
         MVC   DRMAXWID,=H'999'    FORCE BIG - I CHECK WIDTH                    
         MVI   FCRDTRNS,C'N'       SET UP NOT TO READ DETAILS                   
         MVI   FCRDHIST,C'N'       OR BUCKETS                                   
         MVI   FRCETRNS,C'N'                                                    
         GOTO1 DRONE,DMCB,DRGEN                                                 
         CLI   GRANDOPT,C'Y'       IF GRAND TOTALS REQUESTED                    
         BNE   XIT                                                              
         MVC   DROLDBUF,DRCURBUF                                                
         L     R1,DRCURBUF                                                      
         MVC   0(2,R1),=X'4802'    DEAL WITH THAT NOW                           
         LA    R1,2(R1)                                                         
         MVC   0(3,R1),=X'871000'                                               
         MVC   3(13,R1),=C'REPORT TOTALS'                                       
         LA    R1,16(R1)                                                        
         ST    R1,DRCURBUF                                                      
         B     XIT                                                              
         SPACE 1                                                                
VROWDRON NTR1                      VALIDATE A ROW                               
         MVI   DRACTION,DRROW                                                   
         B     ALLVAL                                                           
         SPACE 1                                                                
GROWDRON NTR1                      GENERATE A ROW                               
         MVI   DRACTION,DRGENROW                                                
         B     ALLDRONE                                                         
         SPACE 1                                                                
VCOLDRON NTR1                      VALIDATE A COLUMN                            
         MVI   DRACTION,DRCOL                                                   
         B     ALLVAL                                                           
         SPACE 1                                                                
GCOLDRON NTR1                      GENERATE A COLUMN                            
         MVI   DRACTION,DRGENCOL                                                
         B     ALLDRONE                                                         
         SPACE 1                                                                
VCMPDRON NTR1                      VALIDATE A COMP                              
         MVI   DRACTION,DRCMP                                                   
         B     ALLVAL                                                           
         SPACE 1                                                                
GCMPDRON NTR1                      GENERATE A COMP                              
         MVI   DRACTION,DRGENCMP                                                
         B     ALLVAL              (LOOKS LIKE A VALIDATION)                    
         SPACE 1                                                                
VWRPDRON DS    0H                  WRAP UP                                      
         MVI   DRACTION,DRWRAPUP                                                
         GOTO1 DRONE,DMCB,DRGEN                                                 
         BAS   RE,TRACDRON         (OPTIONAL TRACE)                             
         B     XIT                                                              
         SPACE 1                                                                
VUSRDRON NTR1                      VALIDATE USER RECORD                         
         MVI   DRACTION,DRUSER                                                  
         MVC   DRUSRKEY(2),AGYALPHA         KEY IS AGENCY                       
         MVC   DRUSRKEY+2(8),22(R4)                AND USER CODE                
         GOTO1 DRONE,DMCB,DRGEN                                                 
         CLI   DRERROR,0                                                        
         BNE   BADUSER                                                          
         TM    DROPTSO,DRBKMINO    IF BRACKET=M SPECIFIED                       
         BNO   XIT                                                              
         NI    DROPTSO,X'DF'       DON'T NEED MINUS=YES                         
         CLI   DRFLOATO,C'-'                                                    
         BNE   XIT                                                              
         MVI   DRFLOATO,0                OR FLOAT=-                             
         B     XIT                                                              
         EJECT                                                                  
*              MORE DRONE UTILITIES                                             
         SPACE 3                                                                
ALLVAL   XC    WORK,WORK           GENERATE A PSEUDO TWA HEADER                 
         MVC   WORK+5(1),0(R4)     (PASS THROUGH THE LENGTH)                    
         MVC   WORK+8(30),12(R4)                                                
         LA    R1,WORK                                                          
         ST    R1,DRACCFLD                                                      
         OI    DRFLAGS,DREXPDIC    TELL DRONE TO EXPLODE DICT.                  
         GOTO1 DRONE,DMCB,DRGEN                                                 
         BAS   RE,SETREAD                                                       
         B     XIT                                                              
         SPACE 1                                                                
ALLDRONE GOTO1 DRONE,DMCB,DRGEN                                                 
         B     XIT                 USER NEEDS TO TEST DRERROR                   
         SPACE 1                                                                
BADDRONE MVC   CONHEAD,DRERRMSG    ERROR - SO SHOW WHAT DRONE PASSED            
         B     MYCURSOR                                                         
         SPACE 1                                                                
TRACDRON NTR1                                                                   
         B     XIT                 REMOVED THIS                                 
         EJECT                                                                  
*              ROUTINE TO SET READ SWITCHES FROM DICTIONARY                     
         SPACE 3                                                                
SETREAD  NTR1                                                                   
         CLC   DRRTNI(6),=C'BUCKET'                                             
         BNE   *+8                                                              
         MVI   FCRDHIST,C'Y'                                                    
******   CLC   DRRTNI(6),=C'BUDGET'                                             
******   BNE   *+8                                                              
******   MVI   FCRDBUDG,C'Y'                                                    
         CLC   DRRTNI(5),=C'TRANS'                                              
         BNE   *+8                                                              
         MVI   FCRDTRNS,C'Y'                                                    
         CLI   DRATTRIB+1,C'T'     SECOND ATTRIBUTE OF T                        
         BNE   *+8                                                              
         MVI   FRCETRNS,C'Y'       FORCES US TO READ TRANSACTIONS               
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZE TO RUN DRIVER                                         
         SPACE 1                                                                
*                                  LOADS PHASES                                 
*                                  SETS GLOBAL ADDRESSES                        
         SPACE 1                                                                
VINTDRIV CLI   OFFLINE,C'Y'                                                     
         BNE   XIT                                                              
         GOTO1 CALLOV,DMCB,X'9A000000',0,0  LOAD T6??9A(GLOBAL STORAGE)         
         L     R4,DMCB                     FOR DRIVER                           
         ST    R4,AGLOBAL                                                       
         SPACE 1                                                                
*                                  THIS ALSO CONTAINS BUFFERS                   
         LA    R2,16(R4)                                                        
         L     R1,0(R2)                                                         
         LA    R2,4+8(R1,R2)                                                    
         ST    R2,ANAMPOOL                                                      
         L     R1,4(R2)            MAX NUMBER OF NAMES                          
         M     R0,8(R2)            WIDTH OF EACH ENTRY                          
         LA    R2,12+8(R1,R2)                                                   
         ST    R2,ALDGPOOL                                                      
         L     R1,4(R2)            MAX NUMBER OF LEDGERS                        
         M     R0,8(R2)            WIDTH OF EACH ENTRY                          
         LA    R2,12+8(R1,R2)                                                   
         L     R1,0(R2)                                                         
         ST    R1,ACIOLOGB         OFFICE GROUP BUFFER                          
         LA    R2,4(R2)                                                         
         ST    R2,ACIOAOGB                                                      
         LA    R2,4(R1,R2)                                                      
         ST    R1,ACIOLMGB         MEDIA GROUP BUFFER                           
         LA    R2,4(R2)                                                         
         ST    R2,ACIOAMGB                                                      
         LA    R2,4(R1,R2)                                                      
         ST    R1,ACIOLWGB         WORK GROUP BUFFER                            
         LA    R2,4(R2)                                                         
         ST    R2,ACIOAWGB                                                      
         LA    R2,4(R1,R2)                                                      
         ST    R2,A20K             A(20 K USER AREA)                            
         SPACE 1                                                                
         USING GLOBALD,R4                                                       
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A3A'   LOAD T00A3A (DRIVER)                 
         L     R2,DMCB                                                          
         ST    R2,DRIVER                                                        
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A81'   LOAD T00A81 (ACCPAK DRIVER)          
         L     R2,DMCB                                                          
         ST    R2,GLASYSDR                                                      
         ST    RC,GLAWORKD                                                      
         MVC   GLAPROG,ADPGPROG                                                 
         MVI   GLTWORKD,GLTSPOOL                                                
         MVC   GLFHEADL,MYFIRSTH                                                
         MVC   GLSPACE,SPACOPT      PASS THRU SPACING OPT                       
         MVC   GLBOXOPT,BOXOPT                BOX OPTION                        
         MVC   GLLFTOPT,LEFTOPT           AND LEFT OPTION                       
         MVC   GLDOWNLD,DOWNOPT     DOWNLOAD OPTION                             
         MVC   GLINDS,DRINDS        PASS THROUGH DRIVER INDICATORS              
         MVC   GLINDS2,DRINDS2                                                  
         MVC   GLRNKMAX,MYRNKMAX         AND RANK MAX OPTION                    
         MVI   GLNORBOX,X'40'      TURN OFF BOW BOXES FOR TOTALS                
         SPACE 1                                                                
DRI2     CLI   TRACEOPT,C'Y'        OPTION TO TRACE                             
         BNE   DRI4                                                             
         MVI   GLTRACE,C'Y'                                                     
         SPACE 1                                                                
DRI4     DS    0H                                                               
         EJECT                                                                  
*              INITIALIZATION OF PRINT RELATED FIELDS                           
         SPACE 3                                                                
VINTHEAD L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         CLI   WIDEOPT,C'Y'                                                     
         BE    DRIWIDE                                                          
         MVC   BOXWIDTH,=F'132'                                                 
         MVI   BOXFONT,0                                                        
         LA    R1,H1               PRINT ADDRESSES FOR STANDARD                 
         ST    R1,AH1                                                           
         LA    R1,H4                                                            
         ST    R1,AH4                                                           
         LA    R1,P                                                             
         ST    R1,AP1                                                           
         MVC   PWIDTH,=F'132'                                                   
         LA    R1,REGSPECS                                                      
         ST    R1,SPECS                                                         
         CLI   NARROPT,C'Y'                                                     
         BNE   XIT                                                              
         LA    R1,NARSPECS                                                      
         ST    R1,SPECS                                                         
         B     XIT                                                              
         SPACE 1                                                                
DRIWIDE  MVC   BOXWIDTH,=F'165'                                                 
         MVI   BOXFONT,1                                                        
         L     R4,BOXAWIDE                                                      
         USING WIDED,R4                                                         
         LA    R1,XHEAD1                                                        
         ST    R1,AH1                                                           
         LA    R1,XHEAD4                                                        
         ST    R1,AH4                                                           
         LA    R1,XP                                                            
         ST    R1,AP1                                                           
         MVC   PWIDTH,=F'198'                                                   
         LA    R1,WIDSPECS                                                      
         ST    R1,SPECS                                                         
         B     XIT                                                              
         DROP  R4                                                               
         DROP  R5                                                               
         EJECT                                                                  
       ++INCLUDE DDVALMNMX                                                      
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 3                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
GETL     NTR1                                                                   
VVGETL   LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,WRIELIST,(C'G',=C'ACCOUNT '),                     X        
               ((R4),(R2)),((R5),(R3))                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET NAME OUT OF A RECORD                              
         SPACE 3                                                                
*              P1   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 1-3  A(OUTPUT AREA)                                    
         SPACE 1                                                                
NAMOUT   NTR1                                                                   
VVNAMOUT LM    R2,R3,0(R1)                                                      
         MVC   0(36,R3),MYSPACES                                                
*&&UK*&& CLI   ALTNOPT,C'O'        ALT NAME ONLY OPTION                         
*&&UK*&& BE    VVNAMALT                                                         
         GOTO1 GETL,DMCB,(X'20',(R2)),0                                         
         CLI   WRIELERR,0                                                       
         BNE   XIT                 NO NAME ELEMENT                              
         L     R4,WRIELAD                                                       
         SPACE 1                                                                
         USING ACNAMED,R4                                                       
         ZIC   R5,ACNMLEN                                                       
         LTR   R5,R5                                                            
         BZ    XIT                                                              
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),ACNMNAME                                                 
*&&UK                                                                           
         CLI   ALTNOPT,C'Y'        ALTERNATE NAME OPTION (UK)                   
         BNE   XIT                                                              
VVNAMALT GOTO1 GETL,DMCB,(X'56',(R2)),0                                         
         CLI   WRIELERR,0                                                       
         BNE   XIT                 NO ALT NAME                                  
         L     R4,WRIELAD                                                       
         SPACE 1                                                                
         USING ACXNMD,R4                                                        
         ZIC   R5,ACXNMLEN                                                      
         SH    R5,=H'5'                                                         
         BM    XIT                                                              
         MVC   0(36,R3),MYSPACES                                                
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),ACXNMSN                                                  
*&&                                                                             
         B     XIT                                                              
         EJECT                                                                  
*              OTHER DATA HANDLING ROUTINES                                     
         SPACE 3                                                                
VNUMERIC TM    4(R2),X'08'                                                      
         BO    VPACK                                                            
         MVI   ERROR,3                                                          
         B     VEXIT                                                            
         SPACE 1                                                                
VPACK    SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         SR    R1,R1                                                            
         ZAP   DUB,=P'0'                                                        
         LTR   R3,R3                                                            
         BZ    XITR1                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         SPACE 1                                                                
XITR1    XIT1  REGS=(R1)                                                        
         EJECT                                                                  
*              INSERT DELETE UNPROTECTED FIELDS                                 
         SPACE 3                                                                
*              INPUT               R2=A(FIRST UNPROTECTED FIELD)                
*                                  MAX=NUMBER OF INPUT FIELDS                   
         SPACE 1                                                                
DELINS   NTR1                                                                   
         CLI   OFFLINE,C'Y'                                                     
         BE    XIT                                                              
         CLI   PFAID,3             WAS PF3 OR PF4 HIT                           
         BE    DI2                                                              
         CLI   PFAID,4                                                          
         BE    DI2                                                              
         CLI   PFAID,15            PF15 PF16 EQUIVALENT                         
         BE    DI2                                                              
         CLI   PFAID,16                                                         
         BNE   XIT                                                              
         SPACE 1                                                                
DI2      L     R4,ATIOB                                                         
         USING TIOBD,R4                                                         
         LH    R4,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         AR    R4,RA               INTO TWA                                     
         ZIC   R0,MAX                                                           
         SPACE 1                                                                
DI4      CR    R2,R4                                                            
         BE    DI6                                                              
         BAS   RE,BUMPTOUN                                                      
         BCT   R0,DI4                                                           
         B     XIT                 (NOT IN THIS PART OF THE SCREEN)             
         SPACE 1                                                                
DI6      CLI   PFAID,3                                                          
         BE    DEL2                                                             
         CLI   PFAID,15                                                         
         BE    DEL2                                                             
         XC    BLOCK(80),BLOCK                                                  
         SPACE 1                                                                
INS2     MVC   BLOCK+80(80),8(R2)  SAVE THIS FIELD                              
         ZIC   R1,0(R2)            GET L'FIELD-1 INTO R1                        
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BNO   *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),BLOCK       MOVE IN PREVIOUS (OR CLEAR)                  
         OI    6(R2),X'80'                                                      
         MVC   BLOCK(80),BLOCK+80                                               
         BAS   RE,BUMPTOUN                                                      
         BCT   R0,INS2                                                          
         B     INSFOUND                                                         
         SPACE 1                                                                
INSFOUND MVC   CONHEAD(L'INSMESS),INSMESS                                       
         L     R4,ATIOB                                                         
         LH    R2,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         AR    R2,RA               INTO TWA                                     
         B     VERRX2                                                           
         SPACE 1                                                                
DEL2     LR    R3,R2                                                            
         BAS   RE,BUMPTOUN                                                      
         CLI   5(R2),0                                                          
         BE    DEL4                                                             
         ZIC   R1,0(R3)            GET L'FIELD-1 INTO R1                        
         SH    R1,=H'9'                                                         
         TM    1(R3),X'02'                                                      
         BNO   *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE NEXT INTO THIS                          
         OI    6(R3),X'80'                                                      
         BCT   R0,DEL2                                                          
         SPACE 1                                                                
DEL4     EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR LAST ONE                               
         OI    6(R3),X'80'                                                      
         LR    R2,R3                                                            
         MVC   CONHEAD(L'DELMESS),DELMESS                                       
         B     VERRX2                                                           
         DROP  R4                                                               
         EJECT                                                                  
*              POSITION CURSOR TO CORRECT FIELD IN ERRORS                       
         SPACE 3                                                                
*              INPUTS              R2=A(SCREEN HEADER)                          
*                                  FIELDERR=NUMBER OF FIELD IN ERROR            
         SPACE 1                                                                
VCURSERR CLI   FIELDERR,0          APPLICATION MUST SET FIELD NUMBER            
         BE    VERRXIT                                                          
         CLI   OFFLINE,C'Y'                                                     
         BE    VERRXIT                                                          
         L     R4,ATIOB                                                         
         USING TIOBD,R4                                                         
         OI    6(R2),X'80'         TRANSMIT ERROR FIELD HEADER                  
         OI    TIOBINDS,TIOBSETC   INSTRUCT CURSOR SETTING                      
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD       DISPLACEMENT FROM START OF TWA               
         LA    RE,8(R2)                                                         
         SR    R1,R1               COMPUTE FIELD DISPLACEMENT INTO R1           
         ZIC   R0,5(R2)            R0 HAS FIELD LENGTH                          
         ZIC   RF,FIELDERR                                                      
         BCT   RF,CURSERR2         CHECK IF ERROR IS IN FIELD 1                 
         B     CURSERR4                                                         
         SPACE 1                                                                
CURSERR2 CLI   0(RE),C','          SCAN FOR THE COMMAS                          
         BNE   CURSERR4                                                         
         BCT   RF,CURSERR4                                                      
         LA    R1,1(R1)            FOUND ENOUGH - SPACE PAST LAST               
         B     CURSERR6                                                         
         SPACE 1                                                                
CURSERR4 LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,CURSERR2                                                      
         SR    R1,R1               ERROR - DIDN'T FIND ENOUGH COMMAS            
         SPACE 1                                                                
CURSERR6 STC   R1,TIOBCURI         SET CURSOR DISPLACEMENT WITHIN FIELD         
         B     VERRXIT                                                          
         EJECT                                                                  
*              COMMON EXIT ROUTINES                                             
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            GET TO NEXT SCREEN FIELD                     
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            GET TO NEXT UNPROTECTED FIELD                
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
FINV     MVI   ERROR,INVALID                                                    
         B     VERRXIT                                                          
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'                                                      
         B     VERRXIT                                                          
         SPACE 1                                                                
MYCURSOR MVI   ERROR,X'FE'                                                      
         GOTO1 CURSERR                                                          
         SPACE 1                                                                
VEXIT    DS    0H                                                               
VERRXIT  OI    6(R2),X'40'         POSITION CURSOR                              
         OI    CONHEADH+6,X'80'    ALWAYS TRANSMIT HEADER                       
         CLI   ERROR,X'FE'                                                      
         BE    VERRX2                                                           
         GOTO1 ERREX               SYSTEM MESSAGE                               
         SPACE 1                                                                
VERRX2   GOTO1 ERREX2              MY OWN ERROR MESSAGE                         
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS TABLES ETC                                             
         SPACE 1                                                                
*                                  ERROR MESSAGES                               
         SPACE 1                                                                
LEDGERR  DC    C'** ERROR ** THIS UNIT/LEDGER NOT FOUND'                        
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
FLTERR   DC    C'** ERROR ** INVALID FILTER'                                    
FLT2ERR  DC    C'** ERROR ** FILTER EXPRESSION TOO LONG'                        
LSTERR   DC    C'FILTER LIST IS NOT ON FILE'                                    
ROWERR   DC    C'** ERROR ** INVALID ROW EXPRESSION'                            
ROWONLY  DC    C'** ERROR ** NOT ALLOWED AS A COLUMN'                           
COLERR   DC    C'** ERROR ** INVALID COLUMN EXPRESSION'                         
USRERR   DC    C'** ERROR ** CANT FIND USER RECORD'                             
COLONLY  DC    C'** ERROR ** NOT ALLOWED AS A ROW'                              
INVDTE   DC    C'** ERROR ** INVALID DATE EXPRESSION'                           
ENTERR   DC    C'ENTRY IS NOT AVAILABLE FOR SELECTED LEDGER'                    
WIDERR   DC    C'WIDTH OVERRIDE HAS MADE REPORT TOO WIDE'                       
HOVERR   DC    C'HEADING OVERRIDE IS WIDER THAN COLUMN'                         
PEXERR   DC    C'PERIOD EXPRESSION INCONSISTENT WITH REQUEST'                   
NOBUDERR DC    C'** ERROR ** BUDGET NOT FOUND'                                  
NEED1ERR DC    C'MUST BE AT LEAST 1 ROW AND 1 COLUMN'                           
LRANKERR DC    C'NOT VALID TO RANK ON LAST ROW'                                 
INSMESS  DC    C'NEW FIELD INSERTED ON SCREEN'                                  
DELMESS  DC    C'FIELD DELETED ON SCREEN'                                       
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
CORETAB  DS    0C                  TABLE OF CORE-RESIDENT MODULES               
         DC    X'30'               GENCON                                       
         DC    X'39'               DRONE                                        
         DC    X'80'               ACCIO                                        
         DC    X'82'               ACCGEN                                       
CORES    EQU   (*-CORETAB)                                                      
         SPACE 1                                                                
RELOW    DC    A(*)                                                             
         SPACE 1                                                                
REGSPECS DS    0C                                                               
         SSPEC H1,2,RUN            SPECS FOR REGULAR PRINTING                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
         DC    X'00'                                                            
         SPACE 1                                                                
NARSPECS DS    0C                                                               
         SSPEC H1,60,RUN           SPECS FOR NARROW PRINTING                    
         SSPEC H2,60,REQUESTOR                                                  
         SSPEC H4,60,REPORT                                                     
         SSPEC H4,73,PAGE                                                       
         DC    X'00'                                                            
         SPACE 1                                                                
WIDSPECS DS    0C                                                               
         WSPEC H1,2,RUN            SPECS FOR WIDE PRINTING                      
         WSPEC H2,2,REQUESTOR                                                   
         WSPEC H1,129,AGYNAME                                                   
         WSPEC H2,129,AGYADD                                                    
         WSPEC H4,129,REPORT                                                    
         WSPEC H4,142,PAGE                                                      
         DC    X'00'                                                            
         SPACE 1                                                                
*EDITLIST DC    XL64'00'            ONLY USED OFFLINE                           
         SPACE 1                                                                
MYSPACES DC    CL132' '                                                         
         EJECT                                                                  
*              LTORG                                                            
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACWRIWORKD                                                     
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACWRIFFD                                                                       
*DDGENTWA                                                                       
*ACGENBOTH                                                                      
*CTGENFILE                                                                      
*FAFACTS                                                                        
*FATIOB                                                                         
*DDCOMFACS                                                                      
*DRGLOBAL                                                                       
*DDBIGBOX                                                                       
*DDPERVALD                                                                      
*DDWIDED                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACWRIFFD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDWIDED                                                        
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072ACACCGEN  02/02/11'                                      
         END                                                                    
