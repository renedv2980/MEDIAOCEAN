*          DATA SET SPREPSA02  AT LEVEL 114 AS OF 06/17/20                      
*PHASE SPSA02B                                                                  
         TITLE 'SPSA02 - READ STABUCK RECORDS FOR SAP INTERFACE'                
*====================================================================           
* READ AN ACC RECOVERY FILE LOOKING FOR SR POSTINGS FOR SPOT BILLING            
* FOR EACH BILL GENERATED, READ THE STATION BUCKET RECORDS ON THE               
* SPTFILE TO FIND THE DOLLARS FOR EACH STATION BILLED                           
*                                                                               
* FOR TESTING, SET QOPT5=Y AND PASTE KEY OF TRANS REC INTO MYKEY                
* --OR-- SET QOPT5=D AND PUT D/A OF TRANS REC IN COL 50 OF REQUEST              
*====================================================================           
***********************************************************************         
*JSHA 113 SPEC-39522 - AR EXTRACT DID NOT PULL CORRECT CALL LETTERS   *         
*JSHA 114 SPEC-41753 - REMOVE PROVINCE AFTER NETWORK TO MATCH AP FILE *         
***********************************************************************         
                                                                                
SPSA02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPSA02                                                         
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPSA02+4096,RC                                                   
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX00                                                             
*                                                                               
         CLI   MODE,REQLAST                                                     
         JE    ENDIN                                                            
*                                                                               
EQXIT    CR    RC,RC                                                            
         J     *+6                                                              
NEQXIT   LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
FX00     DS    0H                                                               
         CLI   QOPT5,C' '          TEST THIS IS A TEST RUN                      
         JNE   FX02                YES - NO RECOVERY FILE                       
         MVI   RCVOPEN,C'Y'                                                     
         OPEN  (RECVIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FX02     MVI   SAPOPEN,C'Y'                                                     
         OPEN  (SAPOUT,(OUTPUT))                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
*==============================================================                 
* GET THE ACCESS RECORD TO GET THE COMPANY CODE                                 
*==============================================================                 
                                                                                
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING CT5REC,R4                                                        
*                                                                               
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGENCY                                                  
         CLC   QOPT1(2),=C'  '     TEST OVERRIDE AGENCY                         
         JE    *+10                                                             
         MVC   CT5KALPH,QOPT1                                                   
         DROP  R4                                                               
*                                                                               
         MVC   AREC,ADBUY                                                       
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',WORK,AREC                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AREC                                                          
         LA    R4,CT5DATA-CT5REC(R4)                                            
         USING CTSYSD,R4                                                        
         SR    R0,R0                                                            
         B     FX02B                                                            
*                                                                               
FX02A    IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
*                                                                               
FX02B    CLI   0(R4),0                                                          
         JE    *+2                                                              
*                                                                               
         CLI   CTSYSEL,CTSYSELQ    X'21' ELEM?                                  
         BNE   FX02A                                                            
         CLI   CTSYSNUM,X'06'      ACC?                                         
         BNE   FX02A                                                            
         MVC   SVCMPNY,CTSYSAGB    SAVE COMPANY CODE                            
         MVC   SVACCSE,CTSYSSE     SAVE ACC SE NUMBER                           
         DROP  R4                                                               
                                                                                
         EJECT                                                                  
*==============================================================                 
* READ NETWORK DEFINITION PASSIVES TO DECODE THE BUYKEY NETWORK                 
*==============================================================                 
                                                                                
FX04     CLI   COUNTRY,C'C'        TEST CANADA                                  
         BNE   FX08                                                             
*                                                                               
         LA    R0,4                                                             
         LARL  R1,NETLIST                                                       
         XC    0(256,R1),0(R1)                                                  
         LA    R1,256(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D91'                                                  
         MVC   KEY+2(2),QAGY                                                    
         GOTO1 HIGH                                                             
         B     FX06B                                                            
*                                                                               
FX06A    GOTO1 SEQ                                                              
*                                                                               
FX06B    CLC   KEY(4),KEYSAVE                                                   
         BNE   FX08                                                             
*                                                                               
         LA    R4,KEY                                                           
         USING NWKPASS,R4                                                       
*                                                                               
         LLC   RE,NWKPSEQ                                                       
         SLL   RE,2                X 4 BYTES/NETWORK                            
         LARL  R0,NETLIST                                                       
         AR    RE,R0                                                            
         MVC   0(4,RE),NWKPNET     SAVE CALL LETTERS                            
         B     FX06A                                                            
         EJECT                                                                  
QADD     EQU   3                                                                
QACCMST  EQU   X'6A'                                                            
*                                                                               
FX08     CLI   QOPT5,C' '          TEST DO ONE ACCFILE TRANSACTION              
         JE    FX10                NO                                           
         BRAS  RE,GETMYREC                                                      
         J     FX12                                                             
*                                                                               
FX10     LA    R0,RCVLEN                                                        
         GET   RECVIN,(0)                                                       
*                                                                               
FX12     CLI   DM$RFILTY,QACCMST                                                
         JNE   FX10                                                             
         CLI   DM$RRECTY,QADD                                                   
         JNE   FX10                                                             
*                                                                               
         LA    R8,RKEY             BUMP PAST RECOVERY HEADER                    
         USING AC_TRNRECD,R8                                                    
*                                                                               
         CLC   SVCUL,0(R8)         RIGHT COMP/UNIT/LDGR                         
         BNE   FX10                                                             
*                                                                               
         LA    RE,RCVLEN                                                        
         AH    RE,0(RE)            GET RECLEN                                   
         XC    0(2,RE),0(RE)       CLEAR 2 BYTES AFTER RECORD                   
*                                                                               
         LA    R6,AC_TRNRFST        IF NOT A TRNEL, SKIP REC                    
         CLI   0(R6),AC_TRNELQ                                                  
         JNE   FX10                                                             
*                                                                               
         USING AC_TRNELD,R6                                                     
         TM    AC_TRNSTAT,AC_TRNSDR  TEST DEBIT                                 
         JZ    FX10                  NO - IGNORE                                
         ST    R6,SVTRNEL                                                       
*                                                                               
         CLI   AC_TRNTYPE,9        TEST MEDIA BILLING ELEM                      
         JNE   FX10                                                             
         DROP  R6                                                               
*                                                                               
         LA    R6,AC_TRNRFST                                                    
         MVI   ELCDLO,AC_MBIELQ                                                 
         MVI   ELCDHI,AC_MBIELQ                                                 
         BAS   RE,NEXTEL2          IN CASE ELEM IS FIRST                        
         BNE   FX20                                                             
*                                                                               
         USING AC_MBIELD,R6                                                     
         CLI   AC_MBISYS,C'S'      TEST SPOT                                    
         BNE   FX10                                                             
*                                                                               
         MVC   SVMED,AC_MBIMED                                                  
         MVC   SVQCLT,AC_MBICLI                                                 
         MVC   SVQPRD,AC_MBIPRD                                                 
         PACK  DUB,AC_MBIEST                                                    
         CVB   R0,DUB                                                           
         STC   R0,SVEST                                                         
         MVC   SVPWOS,AC_MBIMOS                                                 
         B     FX40                                                             
         DROP  R6                                                               
*                                                                               
FX20     LA    R6,AC_TRNRFST                                                    
         MVI   ELCDLO,AC_MDPELQ                                                 
         MVI   ELCDHI,AC_MDPELQ                                                 
         BAS   RE,NEXTEL2                                                       
         BNE   FX30                                                             
*                                                                               
         USING AC_MDPELD,R6                                                     
         CLI   AC_MDPSYS,C'S'      TEST SPOT                                    
         BNE   FX10                                                             
*                                                                               
         MVC   SVMED,AC_MDPMED                                                  
         MVC   SVQCLT,AC_MDPCLI                                                 
         MVC   SVQPRD,AC_MDPPRD                                                 
         PACK  DUB,AC_MDPEST                                                    
         CVB   R0,DUB                                                           
         STC   R0,SVEST                                                         
         MVC   SVPWOS,AC_MDPMOS                                                 
         B     FX40                                                             
                                                                                
*                                                                               
FX30     LA    R6,AC_TRNRFST                                                    
         MVI   ELCDLO,AC_MDTELQ                                                 
         MVI   ELCDHI,AC_MDTELQ                                                 
         BAS   RE,NEXTEL2                                                       
         JNE   *+2                                                              
*                                                                               
         USING AC_MDTELD,R6                                                     
         CLI   AC_MDTSYS,C'S'      TEST SPOT                                    
         BNE   FX10                                                             
*                                                                               
         MVC   SVMED,AC_MDTMED                                                  
         MVC   SVQCLT,AC_MDTCLI                                                 
         MVC   SVQPRD,AC_MDTPRD                                                 
         PACK  DUB,AC_MDTEST                                                    
         CVB   R0,DUB                                                           
         STC   R0,SVEST                                                         
         MVC   SVPWOS,AC_MDTMOS                                                 
*                                                                               
FX40     CLI   COUNTRY,C'C'        TEST CANADA                                  
         JE    FX44                YES -ACCEPT ALL MEDIA                        
*                                                                               
         LA    R0,L'MDTAB          TEST MEDIA CODE                              
         LA    R1,MDTAB                                                         
*                                                                               
FX42     CLC   SVMED,0(R1)         EXCLUDE MEDIA N                              
         JE    FX10                                                             
         LA    R1,1(R1)                                                         
         JCT   R0,FX42                                                          
         J     FX44                                                             
*                                                                               
MDTAB    DC    C'NCSDOV'           EXCLUDE MEDIA EXCEPT FOR CANADA              
*                                                                               
FX44     MVI   SVPWOS+2,1          CONVERT PWOS TO 2-BYTE BINARY                
         UNPK  WORK(7),SVPWOS(4)   E.G., FBF4F0F8F0F1YX                         
         GOTO1 DATCON,DMCB,WORK,(3,SVMOS)                                       
         L     R6,SVTRNEL                                                       
         USING AC_TRNELD,R6                                                     
*                                                                               
         MVC   SVINV,AC_TRNREF     SAVE BILLMON/INV NUM                         
*                                                                               
         PACK  DUB,AC_TRNREF+2(4)  PACK THE INVOICE NUMBER                      
         CVB   R0,DUB                                                           
         STH   R0,SVINVB                                                        
*                                                                               
         LA    R6,AC_TRNRFST                                                    
         MVI   ELCDLO,AC_GDAELQ                                                 
         MVI   ELCDHI,AC_GDAELQ                                                 
*                                                                               
FX46     BAS   RE,NEXTEL                                                        
         JNE   *+2                                                              
         USING AC_GDAELD,R6                                                     
         CLI   AC_GDATYPE,2         TEST MEDIA BILLING DATE                     
         BNE   FX46                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(1,AC_GDADATE),(2,SVBILLDT)                          
         DROP  R6                                                               
         EJECT                                                                  
*================================================================               
* GET THE CLIENT RECORD TO TRANSLATE THE PRODUCT CODE                           
*================================================================               
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         NI    KEY+1,X'F0'           DROP MEDIA                                 
*                                                                               
         MVI   BYTE,X'01'                                                       
         CLI   SVMED,C'T'                                                       
         BE    FX52                                                             
         MVI   BYTE,X'02'                                                       
         CLI   SVMED,C'R'                                                       
         BE    FX52                                                             
         MVI   BYTE,X'03'                                                       
         CLI   SVMED,C'N'                                                       
         BE    FX52                                                             
         MVI   BYTE,X'04'                                                       
         CLI   SVMED,C'X'                                                       
         BE    FX52                                                             
         DC    H'0'                                                             
*                                                                               
FX52     OC    KEY+1(1),BYTE                                                    
         GOTO1 CLPACK,DMCB,SVQCLT,KEY+2                                         
*                                                                               
         L     R8,ADCLT                                                         
         USING CLTHDRD,R8                                                       
*                                                                               
         CLC   KEY(13),0(R8)       CLTHDR ALREADY READ?                         
         BE    FX60                YES                                          
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
*                                                                               
         GOTO1 GETCLT                                                           
*                                                                               
FX60     LA    RE,CLIST            POINT TO PRDLIST                             
         DROP  R8                                                               
*                                                                               
FX62     CLC   SVQPRD,0(RE)                                                     
         BE    FX64                                                             
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   FX62                                                             
         DC    H'0'                                                             
*                                                                               
FX64     MVC   SVPRDCD,3(RE)        SAVE BINARY PRD                             
                                                                                
*===============================================================                
* NOW READ BILLING HEADER RECORD                                                
* KEY HAS KEY OF CLIENT RECORD IN IT                                            
*===============================================================                
                                                                                
         MVC   KEY+4(3),SVQPRD                                                  
         MVC   KEY+7(1),SVEST                                                   
         MVC   KEY+8(2),SVMOS                                                   
* NO DDS DATE!                                                                  
         GOTO1 DATCON,DMCB,(2,SVBILLDT),(X'20',WORK)   GET YYMMDD               
         PACK  DUB,WORK(2)                                                      
         LLC   R0,DUB+7            GET LAST DIGIT OF YEAR (E.G., 7F)            
         N     R0,=X'000000F0'     DROP ZONE BITS                               
*                                                                               
         PACK  DUB,WORK+2(2)                                                    
         CVB   R1,DUB              GET MONTH (X'01'-X'0C')                      
         OR    R1,R0                                                            
         STC   R1,KEY+10                                                        
*                                                                               
         MVC   KEY+11(2),SVINVB                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         MVC   SVBILKEY,KEY                                                     
*                                                                               
         L     R8,ADBUY                                                         
         ST    R8,AREC                                                          
         USING BILLRECD,R8                                                      
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         MVC   SVREVINV,BLREVINO   SAVE INVOICE THIS BILL REVERSED              
         MVC   SVREVDAT,BLREVDAT                                                
*                                                                               
         BAS   RE,SETTYPE          SET BILL TYPE                                
*                                                                               
         CLI   NOVENDOR,C'Y'                                                    
         JNE   FX70                                                             
                                                                                
*==================================================================             
* CREATE MYSAMB FROM MANUAL OR COMM ONLY BILL BECAUSE                           
* THERE ARE NO VENDOR DETAILS                                                   
*==================================================================             
                                                                                
*                                                                               
FX66     BAS   RE,BLDSAMB          BUILD COMMON SAP FIELDS                      
*                                                                               
         LA    R7,MYSAMBREC                                                     
         USING SAMBRECD,R7                                                      
*                                                                               
         MVC   SAMBVNDR(7),=C'NETWORK'                                          
         MVC   SAMBVJN,SAMBVNDR                                                 
*                                                                               
         ZAP   SAMBDOLS,BNETP      NET DOLLARS                                  
         ZAP   SAMBGRS,BGRSP       GROSS DOLLARS                                
* MH 05FEB18 - NO DOLALRS FOR ADJ OR COMM ONLY BILLS                            
**NOP    TM    BILSTAT,X'82'       TEST ADJ OR COMM ONLY BILL                   
**NOP    JZ    *+10                                                             
**NOP    ZAP   SAMBDOLS,BACTP                                                   
*                                                                               
         LLC   R0,SVMOS            BILLABLE YR/SVC                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAMBYS,DUB                                                       
*                                                                               
         IC    R0,SVMOS+1          BILLABLE MOS                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAMBMS,DUB                                                       
*                                                                               
         BAS   RE,PUTSAMB                                                       
*                                                                               
         CLI   QOPT5,C' '          TEST RUN?                                    
         JNE   EXIT                YES - DONE                                   
         J     FX10                NO - READ NEXT RECOVERY                      
         DROP  R7                                                               
         EJECT                                                                  
*=============================================================                  
* SET BILLING TYPE FROM BILL HEADER REC                                         
*=============================================================                  
                                                                                
SETTYPE  NTR1                                                                   
         MVI   BILLMAN,C'N'                                                     
         MVI   BILLADJ,C'N'                                                     
         MVI   BILLCOMM,C'N'                                                    
         MVI   BILLCOS2,C'N'                                                    
         MVI   BILLAOR,C'N'                                                     
         MVI   BILLNET,C'N'                                                     
         MVI   NOVENDOR,C'N'                                                    
*                                                                               
         TM    BILSTAT,X'80'       TEST ADJ BILL                                
         JZ    *+12                                                             
         MVI   BILLADJ,C'Y'                                                     
         MVI   NOVENDOR,C'Y'                                                    
*                                                                               
         TM    BILSTAT,X'40'       TEST MANUAL BILL                             
         JZ    *+12                                                             
         MVI   BILLMAN,C'Y'                                                     
         MVI   NOVENDOR,C'Y'                                                    
*                                                                               
         TM    BILSTAT,X'02'       TEST COMM ONLY                               
         JZ    *+12                                                             
         MVI   BILLCOMM,C'Y'                                                    
         MVI   NOVENDOR,C'Y'                                                    
*                                                                               
         TM    BILSTAT,X'34'       TEST AOR                                     
         JZ    *+8                                                              
         MVI   BILLAOR,C'Y'                                                     
*                                                                               
         TM    BILSTAT,X'08'       TEST NET BILL                                
         JZ    *+8                                                              
         MVI   BILLNET,C'Y'                                                     
*                                                                               
         TM    BILSTAT2,BSTC2Q     TEST COST2 BILL                              
         JZ    *+8                                                              
         MVI   BILLCOS2,C'Y'                                                    
         J     EXIT                                                             
         DROP  R8                                                               
                                                                                
*===============================================================                
* NOW READ STATION BILLING RECORDS                                              
*===============================================================                
                                                                                
FX70     MVI   ANYSTABS,C'N'                                                    
*                                                                               
         LA    R8,KEY                                                           
         USING STABUCKD,R8                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   STABKCOD,=X'0E01'                                                
         L     RE,ADCLT                                                         
         MVC   STABKAM(3),1(RE)     MOVE A-M/CLT FROM CLTHDR                    
         MVC   STABKPRD,SVPRDCD                                                 
         MVC   STABKEST,SVEST                                                   
         DROP  R8                                                               
*                                                                               
         GOTO1 HIGH                                                             
         B     FX74                                                             
*                                                                               
FX72     GOTO1 SEQ                                                              
*                                                                               
FX74     CLC   KEY(7),KEYSAVE      SAME 0E01/AM/CLT/PRD/EST                     
         JE    FX75                                                             
         CLI   ANYSTABS,C'Y'       DID WE FIND ANY STABUCK RECS                 
         JE    FX90                YES                                          
                                                                                
* REREAD BILL HEADER AND TREAT AS NO VENDOR                                     
                                                                                
         MVC   KEY,SVBILKEY        RESTORE BILLING HEADER KEY                   
         L     R8,ADBUY                                                         
         ST    R8,AREC                                                          
         USING BILLRECD,R8                                                      
         GOTO1 GET                                                              
         MVI   NOVENDOR,C'Y'                                                    
         J     FX66                AND TREAT AS NO VENDOR RECORD                
*                                                                               
FX75     L     R8,ADBUY                                                         
         ST    R8,AREC                                                          
         USING STABUCKD,R8                                                      
*                                                                               
         GOTO1 GET                                                              
                                                                                
*=================================================================              
* PER JCLE, NEVER USE COS2 STABUCK DATA -                                       
* ALWAYS REPORT DOLLARS FROM MEDIA DOLLARS STABUCK 25JUL18 MHER                 
*=================================================================              
                                                                                
         LA    R6,STABELEM                                                      
         USING STABELEM,R6                                                      
*                                                                               
**NOP**  CLI   BILLCOS2,C'Y'       TEST COST2 BILL IN PROCESS                   
**NOP**  JE    FX76A               NO                                           
         CLI   STABKCUR,0          SO PROCESS ONLY NON-COST2 BILLS              
         JE    FX76X                                                            
         J     FX72                ELSE SKIP                                    
*                                                                               
FX76A    DS    0H                                                               
**NOP**  CLI   STABKCUR,0          DOING COST2 STABUCK REC                      
**NOP**  JE    FX72                SKIP IF NOT COST2                            
*                                                                               
FX76X    LA    R6,STABELEM                                                      
*                                                                               
         MVI   ELCDLO,X'0E'        GET NORMAL ELS                               
         MVI   ELCDHI,X'1E'        AND UNBILLING ELS                            
         BAS   RE,NEXTEL2                                                       
         B     *+8                                                              
*                                                                               
FX78     BAS   RE,NEXTEL                                                        
         JNE   FX72                                                             
*                                                                               
         CLC   STABPER,SVMOS       MATCH Y/M OF SERVICE                         
         BNE   FX78                                                             
*                                                                               
         CLC   STABBDT,SVBILLDT    RIGHT BILL DATE                              
         BNE   FX78                NO - SKIP                                    
*                                                                               
         MVC   HALF,STABINV                                                     
         NI    HALF,X'3F'          DROP X'80'+X'40'                             
         CLC   HALF,SVINVB         RIGHT INVOICE                                
         BNE   FX78                                                             
                                                                                
*=============================================================                  
* BUILD OUTPUT RECORD                                                           
*=============================================================                  
                                                                                
FX80     MVI   ANYSTABS,C'Y'       SET FLAG FOR FOUND STABUCK REC               
*                                                                               
         BAS   RE,BLDSAMB          BUILD COMMON FIELDS                          
*                                                                               
         LA    R7,MYSAMBREC                                                     
         USING SAMBRECD,R7                                                      
*                                                                               
         LA    R1,STAWORK          UNPACK MKT/STATION                           
         USING STAPACKD,R1                                                      
*                                                                               
         XC    STAWORK,STAWORK                                                  
         MVC   STAPACOM,ACOMFACS                                                
         MVI   STAPACT,C'U'                                                     
         MVC   STAPCTRY,COUNTRY                                                 
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPMED,SVMED                                                    
         MVC   STAPMKST,STABKMKT   PACKED MKT-STA                               
         GOTO1 VSTAPACK,STAWORK                                                 
*                                                                               
         CLC   STAPQSTA(4),=C'YCKD'                                             
         JNE   FX82                                                             
         MVC   SAMBVNDR(7),=C'NETWORK'                                          
         MVC   SAMBVJN,SAMBVNDR                                                 
         MVC   SAMBTYPE,=C'MAN'                                                 
         J     FX86                                                             
*                                                                               
FX82     MVC   SAMBVJN(5),STAPQSTA VENDOR FOR JOIN                              
         CLI   SVMED,C'T'          SUB-MEDIA FOR ALL BUT TV                     
         BNE   *+8                                                              
         MVI   SAMBVJN+4,C' '                                                   
*                                                                               
         CLI   SAMBVJN+4,C'/'      NEED TO FIX NETWORK                          
         BNE   *+8                                                              
         MVI   SAMBVJN+4,C'N'                                                   
*                                                                               
         MVC   SAMBVNDR(8),STAPQSTA   MOVE 8 CHAR STATION TOO                   
*                                                                               
* REMOVE VIA SPEC-39522                                                         
*                                                                               
*&&DO                                                                           
         CLI   SVMED,C'T'                                                       
         BNE   *+8                                                              
         MVI   SAMBVNDR+4,C' '                                                  
*&&                                                                             
*                                                                               
         CLI   SVMED,C'N'          FOR NETWORK, CLEAR PROVINCE                  
         BNE   *+10                                                             
         MVC   SAMBVNDR+4(4),SPACES                                             
*                                                                               
         MVI   NETFLAG,C'N'                                                     
         CLI   STAPQSTA+4,C'N'        TEST NETWORK                              
         BNE   FX86                                                             
         MVI   NETFLAG,C'Y'                                                     
         LLC   R0,STABKSTA+2          GET NETWORK NUMBER                        
         SLL   R0,2                   X 4                                       
         LARL  RE,NETLIST                                                       
         AR    RE,R0                                                            
         MVC   SAMBVNDR+4(4),0(RE)    MOVE NETWORK                              
         DROP  R1                                                               
*                                                                               
FX86     L     R0,STABNET                                                       
         CLI   0(R6),X'1E'         TEST UNBILLING ELEM                          
         BNE   *+6                                                              
         LCR   R0,R0                                                            
         CVD   R0,DUB                                                           
         ZAP   SAMBDOLS,DUB                                                     
*                                                                               
         L     R0,STABGRS                                                       
         CLI   0(R6),X'1E'         TEST UNBILLING ELEM                          
         BNE   *+6                                                              
         LCR   R0,R0                                                            
         CVD   R0,DUB                                                           
         ZAP   SAMBGRS,DUB                                                      
*                                                                               
         BAS   RE,PUTSAMB          PUT RECORD                                   
         J     FX78                AND READ FOR MORE                            
         DROP  R7                                                               
*                                                                               
FX90     CLI   QOPT5,C' '          IS IT A TEST RUN?                            
         JE    FX10                NO - NEXT RECOVERY REC                       
         J     EXIT                YES                                          
         EJECT                                                                  
*============================================================                   
* END OF RECOVERY FILE -  CLOSE FILES AND                                       
*============================================================                   
                                                                                
ENDIN    CLI   RCVOPEN,C'Y'        TEST RECOVERY OPEN                           
         JNE   ENDIN2                                                           
*                                                                               
         MVI   RCVOPEN,C'N'                                                     
         CLOSE RECVIN                                                           
         LTR   RF,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ENDIN2   CLI   SAPOPEN,C'Y'                                                     
         JNE   ENDINX                                                           
*                                                                               
         MVI   SAPOPEN,C'N'                                                     
         CLOSE SAPOUT                                                           
         LTR   RF,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ENDINX   MVI   MODE,REQLAST                                                     
         J     EXIT                                                             
*                                                                               
RCVOPEN  DC    C'N'                                                             
SAPOPEN  DC    C'N'                                                             
         EJECT                                                                  
*==================================================================             
* ROUTINE TO BUILD COMMON SAP DATA FIELDS                                       
*==================================================================             
                                                                                
BLDSAMB  NTR1                                                                   
*                                                                               
         MVC   MYSAMBREC,SPACES                                                 
         LA    R7,MYSAMBREC                                                     
         USING SAMBRECD,R7                                                      
*                                                                               
         MVC   SAMBKAGY,AGENCY                                                  
         MVI   SAMBKSYS,SAPKSYS_SPOT                                            
         MVC   SAMBKMED,SVMED                                                   
         MVC   SAMBKCLT,SVQCLT                                                  
         MVC   SAMBKOVRD,SAMBKMED  MEDIA OVRD FOR LOOKUP=ACTUAL MEDIA           
*                                                                               
         L     RE,ADCLT                                                         
         MVC   SAMBKOFF,CACCOFC-CLTHDR(RE)                                      
         CLI   SAMBKOFF,C' '                                                    
         BH    BLDSAMB2                                                         
         MVC   SAMBKOFF,COFFICE-CLTHDR(RE)                                      
         MVI   SAMBKOFF,C' '                                                    
*                                                                               
BLDSAMB2 LLC   R0,SVMOS            YR/SVC                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAMBYS,DUB                                                       
*                                                                               
         IC    R0,SVMOS+1          MOS                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAMBMS,DUB                                                       
*                                                                               
         MVC   SAMBKPRD,SVQPRD                                                  
         LLC   R0,SVEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAMBKEST,DUB                                                     
*                                                                               
         MVC   SAMBKINV,SVINV                                                   
*                                                                               
         CLI   BILLMAN,C'Y'                                                     
         JNE   *+14                                                             
         MVC   SAMBTYPE,=C'MAN'                                                 
         J     BLDSAMBX                                                         
*                                                                               
         CLI   BILLADJ,C'Y'                                                     
         JNE   *+14                                                             
         MVC   SAMBTYPE,=C'ADJ'                                                 
         J     BLDSAMBX                                                         
*                                                                               
         CLI   BILLCOMM,C'Y'                                                    
         JNE   *+14                                                             
         MVC   SAMBTYPE,=C'COM'                                                 
         J     BLDSAMBX                                                         
*                                                                               
         CLI   BILLCOS2,C'Y'                                                    
         JNE   *+14                                                             
         MVC   SAMBTYPE,=C'CO2'                                                 
         J     BLDSAMBX                                                         
*                                                                               
         CLI   BILLAOR,C'Y'                                                     
         JNE   *+14                                                             
         MVC   SAMBTYPE,=C'AOR'                                                 
         J     BLDSAMBX                                                         
*                                                                               
         CLI   BILLNET,C'Y'                                                     
         JNE   *+10                                                             
         MVC   SAMBTYPE,=C'NET'                                                 
*                                                                               
BLDSAMBX J     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
*===========================================================                    
* COMMON ROUTINE TO PUT/PRINT SAP RECORDE                                       
*===========================================================                    
*                                                                               
PUTSAMB  NTR1                                                                   
         PUT   SAPOUT,MYSAMBREC                                                 
         AP    OUTCNT,=P'1'                                                     
*                                                                               
         LA    R7,MYSAMBREC                                                     
         USING SAMBRECD,R7                                                      
*                                                                               
         MVC   PAGY,SAMBKAGY                                                    
         MVC   PMED,SAMBKMED                                                    
         MVC   POFFC,SAMBKOFF                                                   
         MVC   PCLT,SAMBKCLT                                                    
         MVC   PPRD,SAMBKPRD                                                    
         MVC   PEST,SAMBKEST                                                    
         MVC   PSTA,SAMBVNDR                                                    
*                                                                               
         CLI   NETFLAG,C'Y'        TEST A NETWORK STATION                       
         BNE   PRTSAMB2                                                         
         ICM   R0,15,PSTA+4        GET NETWORK LETTERS                          
         MVI   PSTA+4,C'/'                                                      
         STCM  R0,15,PSTA+5                                                     
*                                                                               
PRTSAMB2 MVC   PBILTYPE,SAMBTYPE                                                
*                                                                               
         MVC   PINV,SAMBKINV                                                    
*                                                                               
         MVC   PYRSVC,SAMBYS                                                    
         MVC   PMONSVC,SAMBMS                                                   
         EDIT  (P6,SAMBGRS),(12,PGRS),2,MINUS=YES                               
         EDIT  (P6,SAMBDOLS),(12,PNET),2,MINUS=YES                              
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         DROP  R7                                                               
*                                                                               
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         LLC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================              
* IF QOPT5=Y, READ AN ACCFILE RECORD INSTEAD OF RECOVERY TAPE                   
* IF QOPT5=D, READ ACCMST USING DISK ADDR IN COL 50 OF REQUEST                  
*=================================================================              
                                                                                
GETMYREC NTR1                                                                   
*                                                                               
         L     RE,UTL                                                           
         MVC   SVSPTSE,4(RE)       SAVE PRINT SE NUMBER                         
         MVC   4(1,RE),SVACCSE     SET ACC SE NUMBER                            
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=CL8'ACC',ACCFLIST,RCVREC                
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
*                                                                               
         CLI   QOPT5,C'D'          TEST HAVE DISK ADDRESS OF TRANS REC          
         JNE   GETMY2                                                           
* DA IS IN COL 50                                                               
         GOTO1 HEXIN,DMCB,QBOOK1,BIGKEY+50,8,0                                  
         OC    12(4,R1),12(R1)                                                  
         JE    *+2                                                              
         J     GETMY4                                                           
*                                                                               
GETMY2   MVC   BIGKEY(42),MYKEY                                                 
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCDIR',MYKEY,BIGKEY                      
         CLC   MYKEY(42),BIGKEY                                                 
         JNE   *+2                                                              
                                                                                
* READ TO PAST RECOVERY HEADER                                                  
                                                                                
GETMY4   GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST',BIGKEY+50,RKEY,      X        
               DMWORK                                                           
*                                                                               
         MVI   DM$RFILTY,QACCMST                                                
         MVI   DM$RRECTY,QADD                                                   
*                                                                               
         LH    RE,RKEY+42          ADD RECORD LEN                               
         AHI   RE,28               +4/RECLEN +24/RECVHDR                        
         STH   RE,RCVLEN                                                        
*                                                                               
         L     RE,UTL                                                           
         MVC   4(1,RE),SVSPTSE                                                  
         J     EXIT                                                             
*                                                                               
ACCFLIST DC    C'N',CL7'ACCDIR'                                                 
         DC    C'N',CL7'ACCMST'                                                 
         DC    CL8'X'                                                           
*                                                                               
*                                                                               
MYKEY    DC    X'DEE2D9C4C2F1E6E2E4F0F0F0404040C4C2404040'                      
         DC    X'E2D7D6E340D9C1C4C9D64040B60815F6F8F1F5F5'                      
         DC    X'F800'                                                          
MYKEYL   EQU   *-MYKEY                                                          
*                                                                               
BIGKEY   DS    CL64                                                             
*                                                                               
COUNTS   DS    0D                                                               
OUTCNT   DC    PL4'0',CL20'SAP RECS OUT'                                        
COUNTX   EQU   *                                                                
*                                                                               
SVTRNEL  DS    A                                                                
*                                                                               
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
SVBILLDT DS    XL2                                                              
SVSYS    DS    CL1                                                              
SVMED    DS    XL1                                                              
SVMOS    DS    XL3                                                              
SVPWOS   DS    XL3                                                              
SVINVB   DS    XL2                                                              
SVINV    DS    CL6                                                              
SVQCLT   DS    CL3                                                              
SVQPRD   DS    CL3                                                              
NETFLAG  DS    CL1                                                              
BILLMAN  DS    CL1                                                              
BILLADJ  DS    CL1                                                              
BILLCOMM DS    CL1                                                              
BILLCOS2 DS    CL1                                                              
BILLAOR  DS    CL1                                                              
BILLNET  DS    CL1                                                              
NOVENDOR DS    CL1                                                              
ANYSTABS DS    CL1                                                              
SVACCSE  DS    XL1                                                              
SVSPTSE  DS    XL1                                                              
SVBILKEY DS    XL18                                                             
SVREVINV DS    XL2                                                              
SVREVDAT DS    XL2                                                              
*                                                                               
SVCUL    DS    0XL3                                                             
SVCMPNY  DS    X                                                                
SVUL     DC    C'SR'                                                            
                                                                                
         DS    0D                                                               
STAWORK  DS    XL64                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'*SAMBREC*'                                                   
MYSAMBREC DS   CL100                                                            
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,                        X        
               MACRF=GM,EODAD=ENDIN                                             
*                                                                               
SAPOUT   DCB   DDNAME=SAPOUT,DSORG=PS,RECFM=FB,LRECL=100,BLKSIZE=12800,X        
               MACRF=PM                                                         
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'NETLIST'                                                     
NETLIST  DS    256F                                                             
*                                                                               
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RCVLEN   DC    F'0'                                                             
RCVREC   DS    0C                                                               
*PREFIX=DM$                                                                     
       ++INCLUDE DMRCVRHDR                                                      
*PREFIX=                                                                        
RKEY     DS    XL13                                                             
         DS    6000C                                                            
         EJECT                                                                  
       ++INCLUDE ACSAPREC                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
*                                                                               
* DSECT FOR PRINT LINE                                                          
         ORG   P                                                                
PAGY     DS    CL2                                                              
         DS    CL1                                                              
PMED     DS    CL1                                                              
         DS    CL1                                                              
POFFC    DS    CL2                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PSTA     DS    CL9                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PYRSVC   DS    CL2                                                              
         DS    CL1                                                              
PMONSVC  DS    CL2                                                              
         DS    CL1                                                              
PBILTYPE DS    CL3                                                              
         DS    CL2                                                              
PINV     DS    CL6                                                              
         DS    CL2                                                              
PGRS     DS    CL13                                                             
         DS    CL2                                                              
PNET     DS    CL13                                                             
*                                                                               
STABUCKD DSECT                                                                  
       ++INCLUDE SPGENSTAB                                                      
       ++INCLUDE SPGENNDEF                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
*PREFIX=AC_                                                                     
       ++INCLUDE ACGENFILE                                                      
*PREFIX   _                                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'114SPREPSA02 06/17/20'                                      
         END                                                                    
