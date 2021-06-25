*          DATA SET PPREPSA02  AT LEVEL 076 AS OF 07/19/18                      
*PHASE PPSA02B                                                                  
*INCLUDE HEXIN                                                                  
                                                                                
*====================================================================           
* READ AN ACC RECOVERY FILE LOOKING FOR SR POSTINGS FOR PRINT BILLING           
* FOR EACH BILL GENERATED, READ THE INSERTIONS ON THE                           
* PRTFILE TO FIND THE DOLLARS FOR EACH PUB                                      
*                                                                               
* FOR TESTING, SET QOPT5=Y AND PASTE KEY OF TRANS REC INTO MYKEY                
* --OR-- SET QOPT5=D AND PUT D/A OF TRANS REC IN COL 50 OF REQUEST              
*====================================================================           
*====================================================================           
         TITLE 'PPSA02 - READ BILLING DATA FOR SAP INTERFACE'                   
PPSA02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPSA02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
*                                                                               
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
*                                                                               
         CLI   SVCMPNY,0           HAVE WE DONE PROCESSING                      
         JNE   EQXIT               YES - KEEP EXITING                           
*                                                                               
         CLI   MODE,REQFRST        SOMETIMES GET REQFRST                        
         JE    FX00                                                             
         CLI   MODE,PROCLST        SOMETIMES GET PROCLST                        
         BE    FX00                CANNOT SORT OUT WHY SO JUST DO ONE           
*                                                                               
EQXIT    CR    RC,RC                                                            
         J     *+6                                                              
NEQXIT   LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
FX00     DS    0H                                                               
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   QOPT5,C' '          THIS A TEST RUN?                             
         JNE   FX02                                                             
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
         MVC   CT5KALPH,QAGENCY                                                 
*                                                                               
         CLC   QOPT1(2),=C'  '     TEST OVERRIDE ACC AGENCY                     
         JE    *+10                                                             
         MVC   CT5KALPH,QOPT1                                                   
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',WORK,PBUYREC              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,PBUYREC                                                       
         LA    R4,CT5DATA-CT5REC(R4)                                            
         USING CTSYSD,R4                                                        
         SR    R0,R0                                                            
         B     FX06                                                             
*                                                                               
FX04     IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
*                                                                               
FX06     CLI   0(R4),0                                                          
         JE    *+2                                                              
*                                                                               
         CLI   CTSYSEL,CTSYSELQ    X'21' ELEM?                                  
         BNE   FX04                                                             
         CLI   CTSYSNUM,X'06'      ACC?                                         
         BNE   FX04                                                             
         MVC   SVCMPNY,CTSYSAGB    SAVE COMPANY CODE                            
         MVC   SVACCSE,CTSYSSE     AND ACC SE NUMBER                            
         DROP  R4                                                               
*                                                                               
         CLI   QOPT5,C' '          THIS A TEST RUN                              
         JE    FX10                                                             
         BRAS  RE,GETMYREC                                                      
         J     FX12                                                             
         EJECT                                                                  
QADD     EQU   3                                                                
QACCMST  EQU   X'6A'                                                            
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
         LA    R6,AC_TRNRFST        FIND TRNEL - PROCESS DEBITS ONLY            
         USING AC_TRNELD,R6                                                     
*                                                                               
         TM    AC_TRNSTAT,AC_TRNSDR      TEST DEBIT                             
         JZ    FX10                      NO - IGNORE                            
*                                                                               
         CLI   AC_TRNTYPE,9           TEST MEDIA BILLING ELEM                   
         JNE   FX10                                                             
*                                                                               
         LA    R6,AC_TRNRFST                                                    
         MVI   ELCODE,AC_MBIELQ                                                 
         BAS   RE,NEXTEL2                                                       
         JNE   *+2                                                              
         DROP  R6                                                               
*                                                                               
         USING AC_MBIELD,R6                                                     
         CLI   AC_MBISYS,C'P'      TEST PRINT                                   
         BNE   FX10                                                             
*                                                                               
         MVC   SVMED,AC_MBIMED                                                  
         MVC   SVQCLT,AC_MBICLI                                                 
         MVC   SVQPRD,AC_MBIPRD                                                 
         PACK  DUB,AC_MBIEST                                                    
         CVB   R0,DUB                                                           
         STH   R0,SVEST                                                         
         MVC   SVPWOS,AC_MBIMOS                                                 
         B     FX40                                                             
         DROP  R6                                                               
*                                                                               
FX20     LA    R6,AC_TRNRFST                                                    
         MVI   ELCODE,AC_MDPELQ                                                 
         BAS   RE,NEXTEL                                                        
         BNE   FX30                                                             
*                                                                               
         USING AC_MDPELD,R6                                                     
         CLI   AC_MDPSYS,C'P'      TEST PRINT                                   
         BNE   FX10                                                             
*                                                                               
         MVC   SVMED,AC_MDPMED                                                  
         MVC   SVQCLT,AC_MDPCLI                                                 
         MVC   SVQPRD,AC_MDPPRD                                                 
         PACK  DUB,AC_MDPEST                                                    
         CVB   R0,DUB                                                           
         STH   R0,SVEST                                                         
         MVC   SVPWOS,AC_MDPMOS                                                 
         B     FX40                                                             
                                                                                
*                                                                               
FX30     LA    R6,AC_TRNRFST                                                    
         MVI   ELCODE,AC_MDTELQ                                                 
         BAS   RE,NEXTEL                                                        
         JNE   *+2                                                              
*                                                                               
         USING AC_MDTELD,R6                                                     
         CLI   AC_MDTSYS,C'P'      TEST PRINT                                   
         BNE   FX10                                                             
*                                                                               
         MVC   SVMED,AC_MDTMED                                                  
         MVC   SVQCLT,AC_MDTCLI                                                 
         MVC   SVQPRD,AC_MDTPRD                                                 
         PACK  DUB,AC_MDTEST                                                    
         CVB   R0,DUB                                                           
         STH   R0,SVEST                                                         
         MVC   SVPWOS,AC_MDTMOS                                                 
*                                                                               
FX40     MVI   SVPWOS+2,1          CONVERT PWOS TO 2-BYTE BINARY                
         UNPK  WORK(7),SVPWOS(4)   E.G., FBF4F0F8F0F1YX                         
         GOTO1 DATCON,DMCB,WORK,(3,SVMOS)                                       
*                                                                               
         LA    R6,AC_TRNRFST                                                    
         USING AC_TRNELD,R6                                                     
*                                                                               
         MVC   SVINV,AC_TRNREF        SAVE INV NUM                              
*                                                                               
         PACK  DUB,AC_TRNREF+2(4)     PACK THE INVOICE NUMBER                   
         CVB   R0,DUB                                                           
         STH   R0,SVINVB                                                        
         DROP  R6                                                               
*                                                                               
         LA    R6,AC_TRNRFST                                                    
         MVI   ELCODE,AC_GDAELQ                                                 
*                                                                               
FX42     BAS   RE,NEXTEL                                                        
         JNE   *+2                                                              
         USING AC_GDAELD,R6                                                     
         CLI   AC_GDATYPE,2         TEST MEDIA BILLING DATE                     
         BNE   FX42                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(1,AC_GDADATE),(3,SVBILLDT)                          
         DROP  R6                                                               
         EJECT                                                                  
*==============================================================                 
* READ CLIENT RECORD                                                            
*==============================================================                 
                                                                                
         XC    KEY,KEY                                                          
         LA    R8,KEY                                                           
         USING PCLTKEY,R8                                                       
*                                                                               
         MVC   PCLTKAGY,QAGENCY                                                 
         MVC   PCLTKMED,SVMED                                                   
         MVI   PCLTKRCD,X'02'                                                   
         MVC   PCLTKCLT,SVQCLT                                                  
         DROP  R8                                                               
*                                                                               
         CLC   PCLTKEY,KEY                                                      
         BE    FX50                                                             
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         JNE   *+2                                                              
*                                                                               
         GOTO1 GETCLI                                                           
                                                                                
* READ BILLING HEADER REC                                                       
                                                                                
FX50     XC    KEY,KEY                                                          
         LA    R8,KEY                                                           
         USING PBILLKEY,R8                                                      
*                                                                               
         MVC   PBILKAGY,QAGENCY                                                 
         MVC   PBILKMED,SVMED                                                   
         MVI   PBILKRCD,X'08'                                                   
         MVC   PBILKCLT,SVQCLT                                                  
         MVC   PBILKPRD,SVQPRD                                                  
         MVC   PBILKEST,SVEST                                                   
         MVC   PBILKMOS,SVMOS                                                   
         MVC   PBILKBMN,SVBILLDT                                                
         MVC   PBILKBNO,SVINVB                                                  
         DROP  R8                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         JNE   *+2                                                              
         MVC   SVBILKEY,KEY                                                     
*                                                                               
         GOTO1 GETBILL                                                          
*                                                                               
         BAS   RE,SETTYPE                                                       
*                                                                               
FX52     CLI   NOVENDOR,C'Y'                                                    
         JNE   FX60                                                             
                                                                                
*==================================================================             
* CREATE SAMBREC FROM MANUAL OR COMM ONLY BILL BECAUSE                          
* THERE ARE NO VENDOR DETAILS                                                   
*==================================================================             
                                                                                
         BAS   RE,BLDSAMB                                                       
*                                                                               
         LA    R7,MYSAMBREC                                                     
         USING SAMBRECD,R7                                                      
                                                                                
         ZAP   SAMBDOLS,PBILLNET                                                
         ZAP   SAMBGRS,PBILLGRS                                                 
*                                                                               
FX54     CLI   PBILSEP,C'C'        TEST THIS IS SEP CASH DISC BILL              
         JE    FX56                                                             
         CLI   PBILSEP,C'A'        OR SEP ADJ BIL                               
         JE    FX56                                                             
         TM    PBILCMSW,X'02'      TEST COMM ONLY                               
         JO    FX56                                                             
*                                                                               
         CP    SAMBDOLS,=P'0'                                                   
         JE    FX59                DO NOT REPORT 0 BILL AMOUNTS                 
*                                                                               
FX56     LLC   R0,PBILKMOS         BILLABLE YR/SVC                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAMBYS,DUB                                                       
*                                                                               
         IC    R0,PBILKMOS+1       BILLABLE MOS                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAMBMS,DUB                                                       
*                                                                               
         MVC   SAMBVNDR(7),=C'NETWORK'                                          
         MVC   SAMBVJN,SAMBVNDR                                                 
*                                                                               
         BAS   RE,PUTSAMB                                                       
*                                                                               
FX59     CLI   QOPT5,C' '          TEST RUN?                                    
         JNE   FX90                YES - GO CHECK FOR MANUAL BILLS              
         J     FX10                NO - READ NEXT RECOVERY                      
         EJECT                                                                  
*===============================================================                
* NOW READ INSERTIONS                                                           
*===============================================================                
                                                                                
FX60     MVI   ANYBUYS,C'N'                                                     
         XC    KEY,KEY                                                          
         LA    R8,KEY                                                           
         USING PBUYKEY,R8                                                       
*                                                                               
         MVC   PBUYKAGY,QAGENCY                                                 
         MVC   PBUYKMED,SVMED                                                   
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,SVQCLT                                                  
         MVC   PBUYKPRD,SVQPRD                                                  
         DROP  R8                                                               
*                                                                               
FX62     MVI   DMINBTS,X'08'       READ DELETED RECORDS !                       
         GOTO1 HIGH                                                             
         B     FX64X                                                            
*                                                                               
FX64     GOTO1 SEQ                                                              
*                                                                               
FX64X    LA    R8,KEY                                                           
         USING PBUYKEY,R8                                                       
*                                                                               
FX66     CLC   KEY(10),KEYSAVE     SAME AG/MED/TY/CL/PRD                        
         JE    FX68                                                             
         CLI   ANYBUYS,C'Y'        FIND ANY BUYS?                               
         JE    FX90                YES                                          
         MVI   NOVENDOR,C'Y'       SET TO PROCESS AS NOVENDOR                   
         J     FX52                                                             
*                                                                               
FX68     CLC   PBUYKEST,SVEST      ACTUAL TO BILLED ESTIMATE                    
         BNE   FX64                                                             
         DROP  R8                                                               
*                                                                               
FX70     GOTO1 GETBUY                                                           
*                                                                               
         LA    R8,PBUYREC                                                       
*                                                                               
         CLC   PBDBDATE(2),SVMOS   MATCH Y/M OF SERVICE                         
         BNE   FX64                                                             
*                                                                               
         MVI   ELCODE,X'26'                                                     
         LA    R6,PBDELEM                                                       
         J     *+8                                                              
*                                                                               
FX72     L     R6,SV26EL                                                        
         BAS   RE,NEXTEL                                                        
*                                                                               
FX74     JNE   FX64                                                             
         ST    R6,SV26EL                                                        
*                                                                               
         USING PBILELEM,R6                                                      
*                                                                               
FX76     CLC   PBPRD,SVQPRD                                                     
         BNE   FX72                                                             
*                                                                               
         CLC   PBINVNO,SVINVB                                                   
         BNE   FX72                                                             
*                                                                               
         CLC   PBLDATE,SVBILLDT    RIGHT BILL DATE                              
         BNE   FX72                NO - SKIP                                    
                                                                                
*==============================================================                 
* OUTPUT RECORD TO SAP FILE                                                     
*==============================================================                 
                                                                                
         MVI   ANYBUYS,C'Y'                                                     
         BAS   RE,BLDSAMB          BUILD COMMON FIELDS                          
*                                                                               
         LA    R7,MYSAMBREC                                                     
         USING SAMBRECD,R7                                                      
                                                                                
* WHILE R6 STILL POINTS TO PBILELEM!                                            
                                                                                
FX78     ICM   R0,15,PBGROSS                                                    
         CVD   R0,DUB                                                           
         ZAP   SAMBGRS,DUB                                                      
*                                                                               
         ICM   RE,15,PBAGYCOM                                                   
         SR    R0,RE               GIVES NET                                    
         CVD   R0,DUB                                                           
         ZAP   SAMBDOLS,DUB                                                     
*                                                                               
         CLI   BILLCOS2,C'Y'       SET COS2 GROSS IF NEEDED                     
         JNE   *+8                                                              
         BAS   RE,GETCOS2                                                       
*                                                                               
         LLC   R0,PBDBDATE         BILLABLE YR/SVC                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAMBYS,DUB                                                       
*                                                                               
         IC    R0,PBDBDATE+1       BILLABLE MOS                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAMBMS,DUB                                                       
*                                                                               
         BAS   RE,PUTSAMB                                                       
*                                                                               
         CLI   NOVENDOR,C'Y'       TEST NOT READING INSERTIONS                  
         JNE   FX72                READING - CONTINUE                           
         DROP  R7                                                               
         EJECT                                                                  
*===================================================================            
* READ BILL HEADERS FOR CLT/PRD/EST/MOS TO SEE IF MANUAL BILL                   
* WAS REVERSED BY THIS BILL                                                     
*===================================================================            
                                                                                
FX90     XC    KEY,KEY                                                          
         MVC   KEY(14),SVBILKEY    A-M/CLT/PRD/EST/MONSVC                       
         GOTO1 HIGH                                                             
         J     FX94                                                             
*                                                                               
FX92     GOTO1 SEQ                                                              
*                                                                               
FX94     CLC   KEY(14),KEYSAVE     A-M/CLT/PRD/EST/MONSVC                       
         JNE   FX98                                                             
*                                                                               
         GOTO1 GETBILL                                                          
*                                                                               
         CLI   PBILLTYP,C'M'       TEST MANUAL BILL                             
         JNE   FX92                                                             
         CLC   PBILMCAN+1(5),SVINV+1  MATCH 5 CHARS OF INVOICE NUM              
         JNE   FX92                                                             
*                                                                               
         BAS   RE,SETTYPE          SET BILL TYPE                                
*                                                                               
         MVI   NOVENDOR,C'Y'       TO GET NO VENDOR FORMATTING                  
                                                                                
         BAS   RE,BLDSAMB          BUILD COMMON FIELDS                          
*                                                                               
         LA    R7,MYSAMBREC                                                     
         USING SAMBRECD,R7                                                      
*                                                                               
         LLC   R0,PBILKMOS         BILLABLE YR/SVC                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAMBYS,DUB                                                       
*                                                                               
         IC    R0,PBILKMOS+1       BILLABLE MOS                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAMBMS,DUB                                                       
*                                                                               
         ZAP   SAMBDOLS,PBILLNET                                                
         MP    SAMBDOLS,=P'-1'                                                  
*                                                                               
         ZAP   SAMBGRS,PBILLGRS                                                 
         MP    SAMBGRS,=P'-1'                                                   
*                                                                               
         BAS   RE,PUTSAMB                                                       
         J     FX92                CHECK FOR MORE                               
*                                                                               
HEXTAB   DC    C'0123456789ABC'                                                 
*                                                                               
FX98     CLI   QOPT5,C' '          IS IT A TEST RUN?                            
         JE    FX10                NO - NEXT RECOVERY REC                       
         EJECT                                                                  
*============================================================                   
* END OF RECOVERY FILE -  CLOSE FILES AND                                       
*============================================================                   
                                                                                
ENDIN    CLI   RCVOPEN,C'Y'                                                     
         JNE   ENDIN2                                                           
*                                                                               
         MVI   RCVOPEN,C'N'                                                     
         CLOSE RECVIN                                                           
         LTR   RF,RF                                                            
         JNE   *+2                                                              
*                                                                               
ENDIN2   CLI   SAPOPEN,C'Y'                                                     
         JNE   ENDINX                                                           
*                                                                               
         MVI   SAPOPEN,C'N'                                                     
         CLOSE SAPOUT                                                           
         LTR   RF,RF                                                            
         JNE   *+2                                                              
*                                                                               
ENDINX   MVI   MODE,REQLAST                                                     
         J     EXIT                                                             
*                                                                               
RCVOPEN  DC    C'N'                                                             
SAPOPEN  DC    C'N'                                                             
         EJECT                                                                  
*================================================================               
* FOR COS2 BILL, FIND GROSS COST FROM OPEN RATE ELEMENT                         
*================================================================               
                                                                                
GETCOS2  NTR1                                                                   
         MVI   ELCODE,X'30'                                                     
         LA    R6,PBDELEM                                                       
*                                                                               
GETCOS2A BAS   RE,NEXTEL                                                        
         JNE   GETCOS2X                                                         
*                                                                               
         USING PORELEMD,R6                                                      
*                                                                               
         CLI   PORCOSTY,C'U'       IGNORE UNIT COST ELEM                        
         JE    GETCOS2A                                                         
*                                                                               
         CLI   PORC$TYP,PORC$GRQ    MUST BE COS2 GROSS $                        
         JNE   GETCOS2A                                                         
*                                                                               
         ZAP   SAMBGRS,PORCOS                                                   
*                                                                               
GETCOS2X J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*==============================================================                 
* SET BILL TYPE                                                                 
*==============================================================                 
                                                                                
SETTYPE  NTR1                                                                   
         MVI   BILLMAN,C'N'                                                     
         MVI   BILLADJ,C'N'                                                     
         MVI   BILLCOMM,C'N'                                                    
         MVI   BILLCOS2,C'N'                                                    
         MVI   BILLAOR,C'N'                                                     
         MVI   BILLNET,C'N'                                                     
         MVI   BILLCD,C'N'                                                      
         MVI   NOVENDOR,C'N'                                                    
*                                                                               
         LA    R8,PBILLREC                                                      
         USING PBILLREC,R8                                                      
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,PBILLLEN                                                    
         AR    RE,R8               POINT TO EOR                                 
         XC    0(2,RE),0(RE)       CLEAR 2 BYTES FOLLOWING REC                  
*                                                                               
         TM    PBILCMSW,X'80'      TEST ADJUSTMENT BILL                         
         JO    *+12                                                             
         CLI   PBILSEP,C'A'        TEST SEP ADJUSTMENT BILL                     
         JNE   SETTYP2                                                          
         MVI   BILLADJ,C'Y'                                                     
         MVI   NOVENDOR,C'Y'                                                    
*                                                                               
SETTYP2  CLI   PBILLTYP,C'M'       TEST MANUAL BILL                             
         JNE   *+12                NO                                           
         MVI   BILLMAN,C'Y'                                                     
         MVI   NOVENDOR,C'Y'                                                    
*                                                                               
         TM    PBILCMSW,X'02'      TEST COMM ONLY                               
         JO    *+12                                                             
         CLI   PBILSEP,C'A'        TEST SEP ADJ BILL (COMMISSION?)              
         JNE   SETTYP4                                                          
         MVI   BILLCOMM,C'Y'                                                    
         MVI   NOVENDOR,C'Y'                                                    
*                                                                               
SETTYP4  CLI   PBILSEP,C'C'        TEST SEP CASH DISCOUNT BILL                  
         JNE   *+12                                                             
         MVI   BILLCD,C'Y'                                                      
         MVI   NOVENDOR,C'Y'                                                    
*                                                                               
         TM    PBILCMSW,X'30'      TEST AOR BILL                                
         JZ    *+8                                                              
         MVI   BILLAOR,C'Y'                                                     
*                                                                               
         TM    PBILCMSW,X'08'      TEST NET BILL                                
         JZ    *+8                                                              
         MVI   BILLNET,C'Y'                                                     
*                                                                               
         CLI   PBILSEP,C'C'        TEST THIS IS SEP CASH DISC BILL              
         JE    *+12                                                             
         CLI   PBILSEP,C'A'        OR SEP ADJ BIL                               
         JNE   *+8                                                              
         MVI   NOVENDOR,C'Y'                                                    
*                                                                               
         LA    RE,PBILLEL          POINT TO 08 EL                               
*                                                                               
SETTYP10 LLC   R0,1(RE)                                                         
         AR    RE,R0               POINT TO NEXT ELEM (IF ANY)                  
         CLI   0(RE),0             TEST EOR                                     
         JE    SETTYPX                                                          
*                                                                               
         CLI   0(RE),X'09'                                                      
         JNE   SETTYP10                                                         
         TM    PBILLIND-PBILOTH(RE),X'82'    TEST COST 2 BILL                   
         JZ    *+8                                                              
         MVI   BILLCOS2,C'Y'                                                    
*                                                                               
SETTYPX  J     EXIT                                                             
         EJECT                                                                  
*==================================================================             
* ROUTINE TO BUILD COMMON SAP DATA FIELDS                                       
*==================================================================             
                                                                                
BLDSAMB  NTR1                                                                   
         MVC   MYSAMBREC,SPACES                                                 
         LA    R7,MYSAMBREC                                                     
         USING SAMBRECD,R7                                                      
*                                                                               
         MVC   SAMBKAGY,QAGENCY                                                 
         MVI   SAMBKSYS,SAPKSYS_PRNT                                            
         MVC   SAMBKMED,SVMED                                                   
         MVC   SAMBKCLT,SVQCLT                                                  
         MVC   SAMBKOVRD,SAMBKMED MEDIA CODE OVRD=ACTUAL MEDIA                  
*                                                                               
         MVC   SAMBKOFF,PCLTOFF                                                 
         MVI   SAMBKOFF+1,C' '                                                  
*                                                                               
         MVC   SAMBKPRD,SVQPRD                                                  
*                                                                               
         LH    R0,SVEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAMBKEST,DUB                                                     
*                                                                               
         MVC   SAMBKINV,SVINV                                                   
*                                                                               
         CLI   NOVENDOR,C'Y'                                                    
         JNE   BLDSAMB2                                                         
         MVC   SAMBVJN(7),=C'NETWORK'                                           
         J     BLDSAMB6                                                         
*                                                                               
         USING PBUYREC,R8                                                       
BLDSAMB2 LA    R0,6                                                             
         CLI   PBUYKEDT,0          DO WE HAVE AN EDITION?                       
         JH    BLDSAMB4                                                         
         LA    R0,5                                                             
         CLI   PBUYKZON,0          DO WE HAVE A ZONE?                           
         JH    BLDSAMB4                                                         
         LA    R0,4                                                             
*                                                                               
BLDSAMB4 GOTO1 HEXOUT,DMCB,PBUYKPUB,SAMBVJN,(R0),=C'TOG'                        
         DROP  R8                                                               
*                                                                               
BLDSAMB6 MVC   SAMBVNDR,SAMBVJN                                                 
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
         CLI   BILLCD,C'Y'                                                      
         JNE   *+14                                                             
         MVC   SAMBTYPE,=C'CD '                                                 
         J     BLDSAMBX                                                         
*                                                                               
         CLI   BILLNET,C'Y'                                                     
         JNE   *+10                                                             
         MVC   SAMBTYPE,=C'NET'                                                 
*                                                                               
BLDSAMBX J     EXIT                                                             
         DROP  R7                                                               
                                                                                
*===========================================================                    
* COMMON ROUTINE TO PUT/PRINT SAP RECORDE                                       
*===========================================================                    
                                                                                
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
*                                                                               
         MVC   PPUB(7),=C'NETWORK'                                              
         CLI   NOVENDOR,C'Y'                                                    
         JE    PRTSAMB2                                                         
*                                                                               
         LA    R0,PBUYKPUB-PBUYKEY(R8)                                          
         GOTO1 PUBEDIT,DMCB,(R0),PPUB                                           
*                                                                               
PRTSAMB2 OC    PPUB,SPACES                                                      
         MVC   PINV,SAMBKINV                                                    
*                                                                               
         MVC   PBILTYPE,SAMBTYPE                                                
*                                                                               
         MVC   PYRSVC,SAMBYS                                                    
         MVC   PMONSVC,SAMBMS                                                   
         EDIT  (P6,SAMBGRS),(12,PGRS),2,MINUS=YES                               
         EDIT  (P6,SAMBDOLS),(12,PNET),2,MINUS=YES                              
*                                                                               
         GOTO1 REPORT                                                           
         J     EXIT                                                             
         DROP  R7                                                               
                                                                                
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
NEXTEL2  CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         CLI   0(R6),0                                                          
         BNE   NEXTEL                                                           
         LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================              
* IF QOPT5=Y, READ AN ACCFILE RECORD INSTEAD OF RECOVERY TAPE                   
* IF QOPT5=D, READ ACCMST USING DISK ADDR IN COL 50 OF REQUEST                  
*=================================================================              
                                                                                
GETMYREC NTR1                                                                   
*                                                                               
         L     RE,UTL                                                           
         MVC   SVPRTSE,4(RE)       SAVE PRINT SE NUMBER                         
         MVC   4(1,RE),SVACCSE     SET ACC SE NUMBER                            
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=CL8'ACC',ACCFLIST,RCVREC                
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         CLI   QOPT5,C'D'          TEST READ BY DISK ADDRESS                    
         JNE   GETMY2                                                           
* DA IS IN COL 50                                                               
         GOTO1 =V(HEXIN),DMCB,QBPDATE,BIGKEY+50,8,0                             
         OC    12(4,R1),12(R1)                                                  
         JE    *+2                                                              
         J     GETMY4                                                           
*                                                                               
GETMY2   GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCDIR',MYKEY,BIGKEY                      
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
         MVC   4(1,RE),SVPRTSE                                                  
         J     EXIT                                                             
*                                                                               
ACCFLIST DC    C'N',CL7'ACCDIR'                                                 
         DC    C'N',CL7'ACCMST'                                                 
         DC    CL8'X'                                                           
*                                                                               
*                                                                               
MYKEY    DC    X'DBE2D9C3D5E8D3D7F1F0F0F0F0F0F0F0F1404040'                      
         DC    X'D7D9C9D5E340D5C5E6E24040B61001F1F0F0F0F4'                      
         DC    X'F000'                                                          
MYKEYL   EQU   *-MYKEY                                                          
*                                                                               
BIGKEY   DS    CL64                                                             
*                                                                               
COUNTS   DS    0D                                                               
OUTCNT   DC    PL4'0',CL20'SAP RECS OUT'                                        
COUNTX   EQU   *                                                                
*                                                                               
         DS    0D                                                               
SVBILKEY DS    XL32                                                             
SV26EL   DS    A                                                                
*                                                                               
SVINVB   DS    H                                                                
SVEST    DS    H                                                                
ELCODE   DS    X                                                                
SVBILLDT DS    XL3                                                              
SVSYS    DS    CL1                                                              
SVMED    DS    XL1                                                              
SVMOS    DS    XL3                                                              
SVPWOS   DS    PL3                                                              
SVINV    DS    CL6                                                              
SVQCLT   DS    CL3                                                              
SVQPRD   DS    CL3                                                              
BILLMAN  DS    CL1                                                              
BILLADJ  DS    CL1                                                              
BILLCOMM DS    CL1                                                              
BILLCOS2 DS    CL1                                                              
BILLAOR  DS    CL1                                                              
BILLNET  DS    CL1                                                              
BILLCD   DS    CL1                                                              
NOVENDOR DS    CL1                                                              
SVACCSE  DS    XL1                                                              
SVPRTSE  DS    XL1                                                              
ANYBUYS  DS    C                                                                
*                                                                               
SVCUL    DS    0XL3                                                             
SVCMPNY  DS    X                                                                
SVUL     DC    C'SR'                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'*SAMBREC'                                                    
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
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
PORELEMD DSECT                                                                  
       ++INCLUDE PORELEM                                                        
       ++INCLUDE PPREPWORK                                                      
                                                                                
* DSECT FOR PRINT LINE                                                          
                                                                                
PPWORKD  DSECT                                                                  
         ORG   P                                                                
PAGY     DS    CL2                                                              
         DS    CL1                                                              
PMED     DS    CL1                                                              
         DS    CL2                                                              
POFFC    DS    CL2                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PPUB     DS    CL14                                                             
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
         DS    CL3                                                              
PNET     DS    CL13                                                             
*                                                                               
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE CTGENFILE                                                      
*PREFIX=AC_                                                                     
       ++INCLUDE ACGENFILE                                                      
*PREFIX=                                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076PPREPSA02 07/19/18'                                      
         END                                                                    
