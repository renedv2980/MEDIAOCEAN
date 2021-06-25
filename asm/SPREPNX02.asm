*          DATA SET SPREPNX02  AT LEVEL 024 AS OF 10/25/17                      
*PHASE SPNX02B                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'SPNX02 - SORT ACC RECOVERY BILLIING TRANSACTIONS'               
*======================================================================         
* READ AN ACC RECOVERY FILE LOOKING FOR SR POSTINGS FOR NETWORK BILLING         
* THEN BUILD SORT RECORDS TO PASS TO SPNA02.                                    
*                                                                               
* IF QOPT5=Y THEN READ THE ACC TRANSACTION IN MYKEY INSTEAD OF RECVIN           
*                                                                               
* IF QOPT5=D THEN READ THE ACC TRANS WHOSE DISK ADDRESS IS IN COL 50            
*            OF REQUEST                                                         
*======================================================================         
                                                                                
SPNX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPNX02                                                         
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPNX02+4096,RC                                                   
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX00                                                             
*                                                                               
EQXIT    CR    RC,RC                                                            
         J     *+6                                                              
NEQXIT   LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
FX00     DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                                                               
         CLI   QOPT5,C' '          TEST RUN?                                    
         JNE   FX02                YES - SKIP RECOVERY OPEN                     
         MVI   RCVOPEN,C'Y'                                                     
         OPEN  (RECVIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
FX02     MVI   POSTOPEN,C'Y'                                                    
         OPEN  (POSTOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         J     FX04                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,32,A),FORMAT=BI'                             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=32'                                    
                                                                                
*==============================================================                 
* GET THE ACCESS RECORD TO GET THE COMPANY CODE                                 
*==============================================================                 
                                                                                
FX04     XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING CT5REC,R4                                                        
*                                                                               
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGENCY                                                  
*                                                                               
         CLC   QOPT1(2),=C'  '     TEST OVERRIDE FOR ACC AGENCY                 
         JE    *+10                NO                                           
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
         B     FX08                                                             
*                                                                               
FX06     IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
*                                                                               
FX08     CLI   0(R4),0                                                          
         JE    *+2                                                              
*                                                                               
         CLI   CTSYSEL,CTSYSELQ    X'21' ELEM?                                  
         BNE   FX06                                                             
         CLI   CTSYSNUM,X'06'      ACC?                                         
         BNE   FX06                                                             
         MVC   SVCMPNY,CTSYSAGB    SAVE COMPANY CODE                            
         MVC   SVACCSE,CTSYSSE     SAVE ACC SE NUMBER                           
         DROP  R4                                                               
         EJECT                                                                  
QADD     EQU   3                                                                
QACCMST  EQU   X'6A'                                                            
*                                                                               
         CLI   QOPT5,C' '          TEST DO ONE ACCMST TRANSACTION               
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
         MVI   SRSYS,C'N'          SET 'SPOT' SYSTEM                            
         MVC   SRAGY,QAGY          AND SET REQUESTING AGY                       
*                                                                               
         LA    R6,AC_TRNRFST                                                    
         MVI   ELCDLO,AC_MBIELQ                                                 
         MVI   ELCDHI,AC_MBIELQ                                                 
         BAS   RE,NEXTEL2          IN CASE ELEM IS FIRST                        
         BNE   FX20                                                             
*                                                                               
* NETWORK IS A SPOT SYSTEM - MEDIA CODE IS USED FOR                             
         USING AC_MBIELD,R6                                                     
         CLI   AC_MBISYS,C'S'      TEST SPOT SYSTEM (NET IS PART OF S)          
         BNE   FX10                                                             
*                                                                               
         MVC   SRMED,AC_MBIMED                                                  
         MVC   SRCLT,AC_MBICLI                                                  
         MVC   SRPRD,AC_MBIPRD                                                  
         MVC   SREST,AC_MBIEST                                                  
         PACK  DUB,AC_MBIEST                                                    
         CVB   R0,DUB                                                           
         STC   R0,SVEST                                                         
         MVC   SRBILMOS,AC_MBIMOS                                               
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
         CLI   AC_MDPSYS,C'N'      TEST NETWORK                                 
         BNE   FX10                                                             
*                                                                               
         MVC   SRMED,AC_MDPMED                                                  
         MVC   SRCLT,AC_MDPCLI                                                  
         MVC   SRPRD,AC_MDPPRD                                                  
         MVC   SREST,AC_MDPEST                                                  
         PACK  DUB,AC_MDPEST                                                    
         CVB   R0,DUB                                                           
         STC   R0,SVEST                                                         
         MVC   SRBILMOS,AC_MDPMOS                                               
         B     FX40                                                             
                                                                                
*                                                                               
FX30     LA    R6,AC_TRNRFST                                                    
         MVI   ELCDLO,AC_MDTELQ                                                 
         MVI   ELCDHI,AC_MDTELQ                                                 
         BAS   RE,NEXTEL2                                                       
         JNE   *+2                                                              
*                                                                               
         USING AC_MDTELD,R6                                                     
         CLI   AC_MDTSYS,C'N'      TEST NETWORK                                 
         BNE   FX10                                                             
*                                                                               
         MVC   SRMED,AC_MDTMED                                                  
         MVC   SRCLT,AC_MDTCLI                                                  
         MVC   SRPRD,AC_MDTPRD                                                  
         MVC   SREST,AC_MDTEST                                                  
         PACK  DUB,AC_MDTEST                                                    
         CVB   R0,DUB                                                           
         STC   R0,SVEST                                                         
         MVC   SRBILMOS,AC_MDTMOS                                               
*                                                                               
FX40     LA    R0,L'MDTAB          TEST MEDIA CODE                              
         LA    R1,MDTAB                                                         
*                                                                               
FX42     CLC   SRMED,0(R1)                                                      
         JE    FX44                                                             
         LA    R1,1(R1)                                                         
         JCT   R0,FX42                                                          
         J     FX10                                                             
*                                                                               
MDTAB    DC    C'NCSDOV'                                                        
*                                                                               
FX44     MVI   SRBILMOS+2,1        CONVERT PWOS TO 3-BYTE BINARY                
         UNPK  WORK(7),SRBILMOS(4) E.G., FBF4F0F8F0F1YX                         
         GOTO1 DATCON,DMCB,WORK,(3,SRBILMOS)                                    
*                                                                               
         L     R6,SVTRNEL                                                       
         USING AC_TRNELD,R6                                                     
*                                                                               
         MVC   SRBILINV,AC_TRNREF  SAVE BILLMON/INV NUM                         
*                                                                               
         LA    R6,AC_TRNRFST                                                    
         MVI   ELCDLO,AC_GDAELQ                                                 
         MVI   ELCDHI,AC_GDAELQ                                                 
*                                                                               
FX46     BAS   RE,NEXTEL                                                        
         JNE   *+2                                                              
         USING AC_GDAELD,R6                                                     
         CLI   AC_GDATYPE,2         TEST MEDIA BILLING DATE                     
         JNE   FX46                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(1,AC_GDADATE),(2,SRBILDAT)                          
         DROP  R6                                                               
         EJECT                                                                  
*================================================================               
* NOW PUT RECORD TO SORT                                                        
*================================================================               
                                                                                
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRREC                                    
         AP    SORTCNT,=P'1'                                                    
         CLI   QOPT5,C' '          IS THIS A TEST RUN                           
         JE    FX10                                                             
         EJECT                                                                  
*============================================================                   
* END OF RECOVERY FILE -  SORT AND PRINT RECORDS                                
*============================================================                   
                                                                                
ENDIN    CP    SORTCNT,=P'0'                                                    
         JNE   ENDIN2                                                           
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         J     ENDINX                                                           
*                                                                               
ENDIN2   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   RE,15,4(R1)         GET OUTPUT REC ADDRESS                       
         JZ    ENDINX                                                           
*                                                                               
         MVC   SRREC,0(RE)         MOVE TO WHERE I CAN SEE IT                   
*                                                                               
         PUT   POSTOUT,SRREC                                                    
*                                                                               
         BAS   RE,PRTSRT                                                        
         B     ENDIN2                                                           
*                                                                               
ENDINX   CLI   RCVOPEN,C'Y'                                                     
         JNE   ENDINX2                                                          
*                                                                               
         CLOSE RECVIN                                                           
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
ENDINX2  CLI   POSTOPEN,C'Y'                                                    
         JNE   ENDINXX                                                          
*                                                                               
         CLOSE POSTOUT                                                          
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
ENDINXX  MVI   MODE,REQLAST                                                     
         J     EXIT                                                             
         EJECT                                                                  
PRTSRT   NTR1                                                                   
*                                                                               
         MVC   PAGY,QAGY                                                        
         MVC   PMED,SRMED                                                       
         MVC   PCLT,SRCLT                                                       
         MVC   PPRD,SRPRD                                                       
         MVC   PEST,SREST                                                       
         GOTO1 DATCON,DMCB,(2,SRBILDAT),(8,PBILDATE)                            
*                                                                               
         LLC   R0,SRBILMOS                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PYRSVC,DUB                                                       
*                                                                               
         LLC   R0,SRBILMOS+1                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PMONSVC(2),DUB                                                   
*                                                                               
         MVC   PBILINV(2),SRBILINV INVOICE NUMBER                               
         MVI   PBILINV+2,C'-'                                                   
         MVC   PBILINV+3(4),SRBILINV+2                                          
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         LLC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JZ    *+2                                                              
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================              
* IF QOPT5=Y, READ AN ACCFILE RECORD INSTEAD OF RECOVERY TAPE                   
*         =D, READ ACCMST DISK ADDRESS IN COL 50                                
*=================================================================              
                                                                                
GETMYREC NTR1                                                                   
*                                                                               
         L     RE,UTL                                                           
         MVC   SVSPTSE,4(RE)       SAVE PRINT SE NUMBER                         
         MVC   4(1,RE),SVACCSE     SET ACC SE NUMBER                            
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=CL8'ACC',ACCFLIST,RCVREC                
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
*                                                                               
         CLI   QOPT5,C'D'                                                       
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
MYKEY    DC    X'DBE2D9E3F1F1F1E2C1D5C4D4C1D540F0F7404040'                      
         DC    X'E2D7D6E340E3E54040404040B70320F0F3F0F0F4'                      
         DC    X'F100'                                                          
MYKEYL   EQU   *-MYKEY                                                          
*                                                                               
BIGKEY   DS    CL64                                                             
*                                                                               
COUNTS   DS    0D                                                               
OUTCNT   DC    PL4'0',CL20'RECORDS OUT'                                         
COUNTX   EQU   *                                                                
SORTCNT  DC    PL4'0'                                                           
*                                                                               
SVTRNEL  DS    A                                                                
*                                                                               
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
SVACCSE  DS    X                                                                
SVSPTSE  DS    X                                                                
SVBILKEY DS    XL18                                                             
RCVOPEN  DC    C'N'                                                             
POSTOPEN DC    C'N'                                                             
*                                                                               
SVCUL    DS    0XL3                                                             
SVCMPNY  DS    X                                                                
SVUL     DC    C'SR'                                                            
*                                                                               
         DS    0D                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,                        X        
               MACRF=GM,EODAD=ENDIN                                             
*                                                                               
         DS    0D                                                               
POSTOUT  DCB   DDNAME=POSTOUT,DSORG=PS,RECFM=FB,                       X        
               LRECL=32,BLKSIZE=16000,MACRF=PM                                  
         LTORG                                                                  
         DS    0D                                                               
         DC    CL8'**SRREC*'                                                    
SRREC    DS    0XL32                                                            
SRSYS    DS    CL1                                                              
SRAGY    DS    CL2                                                              
SRMED    DS    CL1                                                              
SRCLT    DS    CL3                                                              
SRPRD    DS    CL3                                                              
SREST    DS    CL3                                                              
*                                                                               
SRBILDAT DS    XL2                 2-BYTE PACKED                                
SRBILINV DS    CL6                                                              
SRBILMOS DS    XL2                 Y/M PWOS                                     
         DS    XL9                 SPARE                                        
SRRECL   EQU   *-SRREC                                                          
*                                                                               
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RCVLEN   DC    F'0'                                                             
RCVREC   DS    0C                                                               
*PREFIX=DM$                                                                     
       ++INCLUDE DMRCVRHDR                                                      
*PREFIX=                                                                        
RKEY     DS    0C                  KEY OF RECORD                                
         DS    6100C                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
*                                                                               
* DSECT FOR PRINT LINE                                                          
         ORG   P                                                                
PSYS     DS    CL1                                                              
         DS    CL2                                                              
PAGY     DS    CL2                                                              
         DS    CL1                                                              
PMED     DS    CL1                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PBILDATE DS    CL8                                                              
         DS    CL1                                                              
PYRSVC   DS    CL2                                                              
         DS    CL1                                                              
PMONSVC  DS    CL2                                                              
         DS    CL1                                                              
PBILINV  DS    CL7                                                              
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
*PREFIX=AC_                                                                     
       ++INCLUDE ACGENFILE                                                      
*PREFIX   _                                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024SPREPNX02 10/25/17'                                      
         END                                                                    
