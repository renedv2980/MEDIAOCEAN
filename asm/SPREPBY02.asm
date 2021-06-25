*          DATA SET SPREPBY02  AT LEVEL 034 AS OF 07/02/19                      
*PHASE SPBY02T                                                                  
*INCLUDE HEXOUT                                                                 
         SPACE 1                                                                
         TITLE 'SPBY02 - SPOT BUY COPY TO MED+08'                               
SPBY02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPBY02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPBY02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    PBUY                                                             
         CLI   MODE,STAFRST                                                     
         BE    SFST                                                             
         CLI   MODE,STALAST                                                     
         BE    SLAS                                                             
         CLI   MODE,CLTFRST                                                     
         BE    CFST                                                             
         CLI   MODE,ESTFRST                                                     
         BE    EFST                                                             
         CLI   MODE,ESTLAST                                                     
         BE    ELST                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   BX02                                                             
*                                                                               
RFST     DS    0H                                                               
*                                                                               
         CLI   COUNTRY,C'C'        CANADIAN AGENCY?                             
         BNE   RFST02                                                           
         MVC   P+2(40),=C'SBY IS NOT COMPATIBLE WITH CANADIAN BUYS'             
         GOTO1 REPORT                                                           
         MVC   P+2(27),=C'REQUEST HAS BEEN TERMINATED'                          
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         DC    H'0'                SHOULD NOT RETURN                            
*                                                                               
RFST02   MVI   FORCEHED,C'Y'                                                    
         XC    ADDCOUNT,ADDCOUNT                                                
         XC    UPDCOUNT,UPDCOUNT                                                
         CLC   QPRD,=C'ALL'                                                     
         BNE   *+10                                                             
         MVC   QPRD,=C'POL'        FORCE ALL TO POL                             
*                                                                               
         XC    WORK,WORK                                                        
         XC    BINMKSTA,BINMKSTA                                                
*                                                                               
         CLC   QMKT,=C'    '                                                    
         BE    REQF10                                                           
         CLC   =C'ALL',QMKT                                                     
         BE    REQF10                                                           
         XC    DUB,DUB                                                          
         PACK  DUB,QMKT                                                         
         CVB   RF,DUB                                                           
         STCM  RF,3,BINMKT                                                      
*                                                                               
REQF10   DS    0H                                                               
         CLC   =C'ALL',QSTA                                                     
         BE    REQF60                                                           
         MVC   WORK(4),=C'0000'                                                 
         MVC   WORK+10(5),QSTA           STATION CALL LETTERS                   
         GOTO1 MSPACK,DMCB,WORK,WORK+10,WORK+15                                 
         MVC   BINSTA,WORK+17                                                   
*                                                                               
REQF60   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
BX02     CLI   MODE,REQLAST                                                     
         BE    RLST                                                             
*                                                                               
EXIT     XIT1                                                                   
RELO     DC    A(0)                                                             
         EJECT                                                                  
*                                                                               
*===============================================================*               
* CLTFRST - GET SOME PROFILE INFO                                               
*===============================================================*               
*                                                                               
CFST     DS    0H                                                               
         XC    B0PROF,B0PROF       B0 PROFILE                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0B0'                                                 
         MVC   WORK+4(2),AGY                                                    
         MVC   WORK+6(1),MED                                                    
         MVC   WORK+7(3),CLIENT                                                 
         L     RF,ADCLT                                                         
         LA    RF,COFFICE-CLTHDR(RF)                                            
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),0(RF)                                                 
         GOTO1 GETPROF,DMCB,(X'40',WORK),B0PROF,DATAMGR                         
         B     EXIT                                                             
*                                                                               
*                                                                               
*===============================================================*               
* ESTFRST - BUILD STATION TABLE FOR THIS ESTIMATE                               
*===============================================================*               
*                                                                               
EFST     DS    0H                                                               
*                                                                               
         L     R0,=A(STATAB)                                                    
         LHI   R1,STANLINQ                                                      
         XCEFL (R0),(R1)                                                        
*                                                                               
         BRAS  RE,BLDSTTAB                                                      
*                                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
*===============================================================*               
* ESTLAST                                                                       
*===============================================================*               
*                                                                               
ELST     DS    0H                                                               
*                                                                               
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         MVI   DMOUTBTS,X'FD'      DON'T PRINT ERROR IF GET DELETED             
*                                                                               
         BRAS  RE,PRCSTTAB                                                      
*                                                                               
         B     EXIT                                                             
*                                                                               
*===============================================================*               
* STAFRST - BUILD TABLE OF EXISTING 'NEW' RECORDS FOR THIS MKT  *               
*===============================================================*               
*                                                                               
SFST     DS    0H                                                               
*                                                                               
         MVIY  BUYTABX9,X'00'      SET TABLE TO NULLS                           
         LAY   RF,BUYTABX9+1                                                    
         LAY   RE,BUYTABX9                                                      
         LA    R1,TABX9LNQ-1       TABLE LENGTH MINUS ONE                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         SR    R3,R3               MAX TABLE ENTRIES CONTROLLER                 
         MVC   WKSVKEY,KEY                                                      
         XC    KEY,KEY                                                          
*                                                                               
         MVC   KEY(1),BAGYMD       FIRST 9 BYTES IF BUY RECORD KEY              
         OI    KEY,X'08'           X'08' IS ON IN AGY/MED                       
         MVC   KEY+1(2),BCLT                                                    
         MVC   KEY+3(1),BPRD                                                    
         MVC   KEY+4(5),BMKTSTA                                                 
         MVC   KEY+9(1),BEST                                                    
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS TOO                
         MVI   DMOUTBTS,X'FD'      DON'T PRINT ERROR IF GET DELETED             
*                                                                               
         GOTO1 HIGH                                                             
         B     SFST30                                                           
*                                                                               
SFSTSEQ  OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS TOO                
         MVI   DMOUTBTS,X'FD'      DON'T PRINT ERROR IF GET DELETED             
         GOTO1 SEQ                                                              
*                                                                               
SFST30   CLC   KEY(10),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST                 
         BNE   SFSTX                                                            
*                                                                               
         LA    R6,WKIO                                                          
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         CLI   KEY+10,0            IF KEY+10 NOT = 0 OR FF                      
         BE    SFST40              THEN SKIP IT IT'S SPILL                      
         CLI   KEY+10,X'FF'                                                     
         BNE   SFSTSEQ                                                          
*                                                                               
SFST40   CHI   R3,512                                                           
         BNH   *+6                                                              
         DC    H'0'                MAX TABLE ENTRY IS 512!!!                    
         LA    R3,1(R3)            ONE MORE ENTRY IN TABLE                      
*                                                                               
         LLC   R5,KEY+11           1-BYTE LINE NUM IN DIR                       
         CLI   KEY+3,X'FF'         POL                                          
         BE    *+10                                                             
         LLC   R5,KEY+12           LINE # FOR PRD (NON-POL)                     
*                                                                               
         TM    KEY+13,BUYRLN2      TEST 2-BYTE LINENUMS                         
         BZ    *+10                                                             
         ICM   R5,3,KEY+11         LINE # FOR PRD (NON-POL)                     
*                                                                               
         LR    R0,R5               SAVE LINE NUMBER                             
         BCTR  R5,0                                                             
         MHI   R5,ENTRYLNQ         X 8                                          
         LAY   R4,BUYTABX9         POINT TO BEGINNING OF TABLE                  
         AR    R4,R5                                                            
*                                                                               
         MVC   1(4,R4),KEY+14      MOVE DISK ADDRESS INTO TABLE                 
         STCM  R0,3,5(R4)          MOVE LINE# INTO TABLE                        
*                                                                               
         L     R6,AREC                                                          
         TM    15(R6),X'80'        TEST DELETED                                 
         BNO   *+8                                                              
         OI    0(R4),X'80'         DELETED RECORD                               
*                                                                               
         B     SFSTSEQ                                                          
*                                                                               
SFSTX    XC    KEY,KEY                                                          
         MVC   KEY(L'WKSVKEY),WKSVKEY                                           
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0            RESTORE ORIGINAL RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(12),KEYSAVE     GOT TO HAVE ORIGINAL REC BACK!               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SFSTXX   B     EXIT                                                             
*                                                                               
*===============================================================*               
* STALAST - SEARCH TABLE FOR ENTRIES THAT ARE NOT USED          *               
*===============================================================*               
*                                                                               
SLAS     DS    0H                                                               
*                                                                               
         MVC   WKSVKEY,KEY                                                      
         SR    R3,R3                                                            
         LAY   R4,BUYTABX9                                                      
*                                                                               
SLAS10   CHI   R3,512                                                           
         BNL   SLASX                                                            
         OC    1(4,R4),1(R4)       CONTAIN DISK ADDRESS?                        
         JZ    *+12                NO - SKIP                                    
         TM    0(R4),X'01'                                                      
         BNO   SLAS30              HAS NOT BEEN USED!                           
         LA    R3,1(R3)                                                         
         LA    R4,ENTRYLNQ(R4)                                                  
         B     SLAS10                                                           
*                                                                               
SLAS30   DS    0H                                                               
*                                                                               
         TM    0(R4),X'80'         RECORD IS ALREADY DELETED?                   
         BO    SLAS40                                                           
         MVC   KEY+14(4),1(R4)     MOVE IN DISK ADDRESS                         
         GOTO1 GET                                                              
         L     R6,AREC                                                          
         USING BUYRECD,R6                                                       
         OI    BUYRCNTL,BUYRDEL    DELETING RECORD ON FILE                      
*                                                                               
         GOTO1 PUT                                                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),BUYKEY                                                   
         CLI   KEY+3,X'FF'         POL                                          
         BNE   SLAS32                                                           
         MVC   KEY+11(1),BUYKEY+10 MOVE LINE# INTO KEY                          
         B     SLAS33                                                           
*                                                                               
SLAS32   MVC   KEY+12(1),BUYKEY+10 MOVE LINE# INTO KEY                          
*                                                                               
SLAS33   TM    BUYRCNTL,BUYRLN2    TEST 2-BYTE LINENUMS                         
         JZ    SLAS34                                                           
         ICM   R0,3,BUYKEY+10                                                   
         STCM  R0,3,KEY+11         SET LINE# INTO KEY                           
*                                                                               
SLAS34   GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(12),KEYSAVE     NOTE COMPARES ONLY 12 BYTES!                 
         BE    *+6                                                              
         DC    H'0'                RECORD IS DIFFERENT!!                        
*                                                                               
         OI    KEY+13,X'80'        DELETING KEY ON DIRECTORY                    
         GOTO1 WRITE                                                            
*                                                                               
         DROP  R6                                                               
*                                                                               
SLAS40   LA    R3,1(R3)                                                         
         LA    R4,ENTRYLNQ(R4)                                                  
         B     SLAS10                                                           
*                                                                               
SLASX    XC    KEY,KEY                                                          
         MVC   KEY(L'WKSVKEY),WKSVKEY                                           
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0            RESTORE ORIGINAL RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     GOT TO HAVE ORIGINAL REC BACK!               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SLASXX   B     EXIT                                                             
*                                                                               
*===============================================================*               
* PROCBUY - ADD                                                                 
*===============================================================*               
*                                                                               
PBUY     DS    0H                                                               
*                                                                               
         CLI   KEY+10,0            IF KEY+10 NOT = 0 OR FF                      
         BE    PBUY02              THEN SKIP IT IT'S SPILL                      
         CLI   KEY+10,X'FF'                                                     
         BNE   PBUYX                                                            
*                                                                               
PBUY02   DS    0H                                                               
*                                                                               
         BRAS  RE,MARKTAB                                                       
*                                                                               
         MVC   WKSVKEY,KEY         SAVE OFF ORIGINAL KEY                        
*                                                                               
         OI    KEY,X'08'           X'08' IS ON (AGY/MED) IN KEY                 
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         OI    BUYKAM,X'08'        X'08' IS ON (AGY/MED) IN RECORD              
         DROP  R6                                                               
*                                                                               
         LLC   R5,KEY+11                                                        
         CLI   KEY+3,X'FF'                                                      
         JE    *+10                                                             
         LLC   R5,KEY+12                                                        
*                                                                               
         TM    KEY+13,BUYRLN2      TEST 2-BYTE LINENUMS                         
         JZ    *+8                                                              
         ICM   R5,3,KEY+11                                                      
*                                                                               
         BCTR  R5,0                                                             
         MHI   R5,ENTRYLNQ                                                      
         LAY   R4,BUYTABX9         POINT TO BEGINNING OF TABLE                  
         AR    R4,R5                                                            
         OC    1(4,R4),1(R4)                                                    
         BZ    PBUY30              NO DADDESS, ADD X9 REC TO FILE               
*                                                                               
         TM    0(R4),X'80'                                                      
         BO    PBUY60              RECORD HAS BEEN DELETED                      
*                                                                               
* RECORD EXIST AND IS NOT DELETED, UPDATE RECORD                                
*                                                                               
         MVC   KEY+14(4),1(R4)     MOVE DISK ADDRESS IN TABLE TO KEY            
         LA    R6,WKIO                                                          
         ST    R6,AREC                                                          
*                                                                               
         MVI   DMOUTBTS,X'FD'      DON'T PRINT ERROR IF GET DELETED             
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         BAS   RE,DATETIME         DATE TIME ELEM                               
         GOTO1 PUTBUY                                                           
*                                                                               
         ICM   R3,15,UPDCOUNT                                                   
         LA    R3,1(R3)                                                         
         STCM  R3,15,UPDCOUNT                                                   
*                                                                               
         LA    R3,P                                                             
         USING PLINED,R3                                                        
         MVC   PSTATUS,=C'UPDATE'                                               
         DROP  R3                                                               
*                                                                               
         BAS   RE,PRTBUY                                                        
*                                                                               
         OI    0(R4),X'01'         ENTRY IN TABLE HAS BEEN USED                 
*                                                                               
         BAS   RE,CLRHIST          CLEAR BUY HISTORY REC                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'WKSVKEY),WKSVKEY                                           
*                                                                               
         MVI   DMOUTBTS,X'FD'      DON'T PRINT ERROR IF GET DELETED             
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0            RESTORE ORIGINAL RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     GOT TO HAVE ORIGINAL REC BACK!               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     PBUYX                                                            
*                                                                               
PBUY30   DS    0X                  ADD RECORD                                   
*                                                                               
         BAS   RE,DATETIME         DATE TIME ELEM                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         NI    BDSTAT3,X'FF'-BDST3_DSKADD                                       
         DROP  R6                                                               
         MVC   AREC,ADBUY                                                       
         GOTO1 ADD                                                              
*                                                                               
         ICM   R3,15,ADDCOUNT                                                   
         LA    R3,1(R3)                                                         
         STCM  R3,15,ADDCOUNT                                                   
*                                                                               
         LA    R3,P                                                             
         USING PLINED,R3                                                        
         MVC   PSTATUS,=C'ADD   '                                               
         DROP  R3                                                               
*                                                                               
         BAS   RE,PRTBUY                                                        
*                                                                               
         BAS   RE,CLRHIST          CLEAR BUY HISTORY REC                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'WKSVKEY),WKSVKEY                                           
*                                                                               
         MVI   DMOUTBTS,X'FD'      DON'T PRINT ERROR IF GET DELETED             
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0            RESTORE ORIGINAL RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     GOT TO HAVE ORIGINAL REC BACK!               
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PBUYX                                                            
*                                                                               
PBUY60   DS    0X                  UNDELETE RECORD                              
*                                                                               
         MVC   KEY+14(4),1(R4)     MOVE DISK ADDRESS IN TABLE TO KEY            
         LA    R6,WKIO                                                          
         ST    R6,AREC                                                          
*                                                                               
         MVI   DMOUTBTS,X'FD'      DON'T PRINT ERROR IF GET DELETED             
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         L     R6,AREC                                                          
         USING BUYRECD,R6                                                       
         NI    BUYRCNTL,X'FF'-BUYRDEL  UNDELETING RECORD ON FILE                
*                                                                               
         BAS   RE,DATETIME         DATE TIME ELEM                               
         GOTO1 PUTBUY              RECORD ON FILE IS UNDELETED                  
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED KEY                        
         MVI   DMOUTBTS,X'FD'      DON'T PRINT ERROR IF GET DELETED             
*                                                                               
         GOTO1 HIGH                                                             
         TM    DMCB+8,X'02'        REC HAS BEEN DELETED                         
*        BO    *+6    REMOVED, TO PREVENT DEATHS, IN CASE OF                    
*        DC    H'0'   DUPLICATE BY REQUESTS                                     
*                                                                               
         CLC   KEY(13),KEYSAVE     RECORD HAS TO BE SAME!                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    KEY+13,X'7F'        KEY ON DIRECTORY IS UNDELETED                
*                                                                               
         GOTO1 WRITE                                                            
*                                                                               
         ICM   R3,15,UPDCOUNT                                                   
         LA    R3,1(R3)                                                         
         STCM  R3,15,UPDCOUNT                                                   
*                                                                               
         LA    R3,P                                                             
         USING PLINED,R3                                                        
         MVC   PSTATUS,=C'UNDEL '                                               
         DROP  R3                                                               
*                                                                               
         BAS   RE,PRTBUY                                                        
*                                                                               
         OI    0(R4),X'01'         ENTRY IN TABLE HAS BEEN USED                 
*                                                                               
         BAS   RE,CLRHIST          CLEAR BUY HISTORY REC                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'WKSVKEY),WKSVKEY                                           
*                                                                               
         MVI   DMOUTBTS,X'FD'      DON'T PRINT ERROR IF GET DELETED             
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0            RESTORE ORIGINAL RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     GOT TO HAVE ORIGINAL REC BACK!               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PBUYX    B     EXIT                                                             
*===============================================================*               
*        ADD DATE/TIME ELEM TO COPIED BUY                                       
*===============================================================*               
*                                                                               
DATETIME NTR1                                                                   
*                                                                               
         CLI   B0PROF+7,C'0'       IS THERE A REASON CODE SCHEME                
         BNH   DTX                 THEN DON'T ADD ELEM                          
*                                                                               
         L     R6,ADBUY                                                         
         CLI   0(R6),X'10'         TEST BUYREC                                  
         BNH   DTX                                                              
         LA    R6,24(R6)                                                        
         SR    R0,R0                                                            
DT10     IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'91'                                                      
         BE    DT20                                                             
         CLI   0(R6),0                                                          
         BNE   DT10                                                             
         B     DT30                                                             
*                                                                               
DT20     GOTO1 RECUP,DMCB,ADBUY,(R6)   DELETE EXISTING ELEMENT                  
*                                                                               
DT30     XC    ELEM,ELEM           MAKE NEW ONE                                 
         LA    R5,ELEM                                                          
         USING BCELEM,R5                                                        
         MVI   BCELCOD,BCELCODQ    X'91'                                        
         MVI   BCELLEN,BCLENQ                                                   
         GOTO1 DATCON,DMCB,(5,0),(2,BCDATE)                                     
         THMS  DDSTIME=YES                                                      
         ST    R1,WORK                                                          
         ST    R0,WORK+4                                                        
         AP    WORK(4),WORK+4(4)   ADD DDS TIME TO GET MILITARY                 
         L     R1,WORK                                                          
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STCM  R1,3,BCTIME                                                      
*                                                                               
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
         SR    R0,R0                                                            
DT40     IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   DT40                                                             
         GOTO1 RECUP,DMCB,ADBUY,(R5),(R6)                                       
*                                                                               
DTX      XIT1                                                                   
         DROP  R5                                                               
*                                                                               
*===============================================================*               
*        CLEAR HISTORY RECORD                                                   
*===============================================================*               
*                                                                               
CLRHIST  NTR1                                                                   
*                                                                               
         CLI   B0PROF+7,C'0'       IS THERE A REASON CODE SCHEME                
         BNH   CHX                 THEN DON'T BOTHER                            
*                                                                               
         L     R6,ADBUY                                                         
         XC    XKEY,XKEY                                                        
         LA    R5,XKEY                                                          
         USING HISTRECD,R5                                                      
         MVI   HISTTYP,HISTTYQ     X'0D'                                        
         MVI   HISTSTYP,HISTSTYQ   X'0B'                                        
         MVC   HISTBUYK,0(R6)      SET BUY KEY                                  
         MVC   HISTBUYK+11(1),BUYRLIN    MOVE BUYLINE                           
         TM    15(R6),BUYRLN2            TEST 2-BYTE LINENUMS                   
         JZ    *+10                                                             
         MVC   HISTBUYK+11(2),BUYRLIN    MOVE BUYLINE                           
         MVI   HISTBUYK+10,0          AND MAKE LIKE DIR KEY                     
         NI    HISTBUYK,X'FF'-X'08'   MAKE SURE NOT COPY                        
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
*                                                                               
         CLI   QOPT5,C'Y'                                                       
         BNE   CH10                                                             
         MVC   P(3),=C'KEY'                                                     
         GOTO1 =V(HEXOUT),DMCB,XKEY,P+12,32                                     
         GOTO1 REPORT                                                           
         MVC   P(7),=C'KEYSAVE'                                                 
         GOTO1 =V(HEXOUT),DMCB,XKEYSAVE,P+12,32                                 
         GOTO1 REPORT                                                           
*                                                                               
CH10     CLC   XKEY(L'HISTKEY),XKEYSAVE                                         
         BNE   CHX                                                              
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFILE',XKEY+36,WKIO,       X        
               (0,XDMWORK)                                                      
         MVI   WKIO+42,0                                                        
         MVC   WKIO+32(2),=X'002A'  EMPTY LEN= 42                               
         CLI   RCWRITE,C'N'                                                     
         BE    CHX                                                              
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFILE',XKEY+36,WKIO,       X        
               (0,XDMWORK)                                                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHX      B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*===============================================================*               
* REQLAST -                                                     *               
*===============================================================*               
*                                                                               
RLST     DS    0H                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(16),=C'RECORDS ADDED:  '                                       
         EDIT  (B4,ADDCOUNT),(10,P+16),ZERO=NOBLANK                             
         GOTO1 REPORT                                                           
         MVC   P(16),=C'RECORDS UPDATE: '                                       
         EDIT  (B4,UPDCOUNT),(10,P+16),ZERO=NOBLANK                             
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*===============================================================*               
*        PRINT BUY RECORDS                                                      
*===============================================================*               
*                                                                               
PRTBUY   NTR1  BASE=*,LABEL=*                                                   
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
PRTB2    LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   PAGY(2),BUYALPHA                                                 
         MVC   PMED,QMED                                                        
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(1),WORK+4                                                 
         CLI   WORK+4,C' '                                                      
         BH    *+8                                                              
         MVI   PSTA+5,C'T'                                                      
*                                                                               
         EDIT  (B1,BUYKEST),(3,PEST),FILL=0                                     
         MVI   PEST+3,C'-'                                                      
*                                                                               
         LLC   R0,BUYKBUY          NEW LINE NUMBER                              
         TM    BUYRCNTL,BUYRLN2    TEST 2-BYTE LINENUMS                         
         JZ    *+8                                                              
         ICM   R0,3,BUYKBUY                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
         LLC   R0,KEY+11           GET OLD LINE #                               
         CLI   KEY+3,X'FF'         POL                                          
         BE    *+10                                                             
         LLC   R0,KEY+12           LINE # FOR PRD (NON-POL)                     
*                                                                               
         TM    KEY+13,BUYRLN2      TEST 2-BYTE LINENUM                          
         JZ    *+8                                                              
         ICM   R0,3,KEY+11                                                      
*                                                                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLINOLD,DUB                                                      
*                                                                               
         CLI   QOPT5,C'Y'                                                       
         BNE   PRTB10                                                           
         L     R2,AREC                                                          
         GOTO1 =V(HEXOUT),DMCB,(R2),PKEY,18                                     
*                                                                               
PRTB10   GOTO1 REPORT                                                           
         J     EXIT                                                             
         DROP  R6,R5                                                            
***********************************************************************         
         EJECT                                                                  
*                                                                               
PRTCOUNT DC    PL4'0'                                                           
MAXCHARS DC    H'60'               MAX INPUT CHARS PER LINE                     
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
         DS    0D                                                               
*                                                                               
ELCODE   DS    X                                                                
*                                                                               
* TABLE OF RECORD COUNT BUCKETS                                                 
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL24                                                            
INRECS   DC    F'0',CL20'RECORDS IN'                                            
OUTRECS  DC    F'0',CL20'RECORDS OUT'                                           
*                                                                               
         SPACE 2                                                                
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
* WORKING STORAGE AREA                                                          
*                                                                               
WKSVKEY  DS    CL13                WORKING STORAGE SAVE KEY                     
ADDCOUNT DS    CL4                 ADD RECORDS COUNTER                          
UPDCOUNT DS    CL4                 UPDATE RECORDS COUNTER                       
B0PROF   DS    CL16                B0 PROFILE                                   
*                                                                               
XKEY     DS    CL48                                                             
XKEYSAVE DS    CL48                                                             
ELEM     DS    CL100                                                            
XDMWORK  DS    24F                                                              
WKIO     DS    CL6000              WORKING STORAGE IO AREA                      
*                                                                               
         DC    CL8'BUYTABX9'                                                    
         DS    0D                                                               
BUYTABX9 DS    512CL(ENTRYLNQ)     TABLE OF BUY REC STATUS & DADDRESS           
ENTRYLNQ EQU   8                   LENGTH OF ONE ENTRY IN TABLE                 
TABX9LNQ EQU   *-BUYTABX9                                                       
*                                                                               
*                                                                               
         DC    CL6'STATAB'                                                      
         DS    0D                                                               
STATAB   DS    (STANLINQ)CL(STTABLNQ)     TABLE OF STATIONS                     
STATABX  DS    X                                                                
STANLINQ EQU   6000                LENGTH OF ONE ENTRY IN TABLE                 
STTABLNQ EQU   8                   LENGTH OF ONE ENTRY IN TABLE                 
*                                                                               
* MKT/STA      5                                                                
* STATUS BYTE  1                                                                
*                                                                               
* DELETED     X'80'                                                             
* PROCESSED   X'40'                                                             
*                                                                               
STATABLQ EQU   *-STATAB                                                         
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* READS ALL BUYS FOR A GIVEN ESTIMATE, AND BUILDS LIST OF STATIONS              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
BLDSTTAB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    R3,R3               MAX TABLE ENTRIES CONTROLLER                 
         L     R1,=A(STATAB)       POINT TO BEGINNING OF TABLE                  
         MVI   0(R1),X'FF'                                                      
         L     R1,ADEST                                                         
         MVC   BLDSVKEY,0(R1)      SAVE OFF ORIGINAL KEY                        
         XC    KEY,KEY                                                          
         MVC   KEY(1),BAGYMD       FIRST 9 BYTES IF BUY RECORD KEY              
         OI    KEY,X'08'           X'08' IS ON IN AGY/MED                       
         MVC   KEY+1(2),BCLT                                                    
         MVC   KEY+3(1),BPRD                                                    
         MVC   KEY+4(5),BINMKSTA                                                
*                                                                               
BLDSTHI  GOTO1 HIGH                                                             
         B     BLDST30                                                          
*                                                                               
BLDSTSEQ GOTO1 SEQ                                                              
*                                                                               
BLDST30  DS    0H                                                               
         CLC   KEY(4),KEYSAVE      SAME A/M,CLT,PRD?                            
         BNE   BLDSTX                                                           
*                                                                               
         CLC   BINMKT,=X'0000'                                                  
         BE    BLDST32                                                          
         CLC   BINMKT,KEY+4                                                     
         BE    *+14                                                             
         MVC   KEY+6(3),=X'FFFFFF'   FORCE TO READ TO NEXT MKT                  
         B     BLDSTHI                                                          
*                                                                               
BLDST32  DS    0H                                                               
         CLC   BINSTA,=X'000000'                                                
         BE    BLDST34                                                          
         CLC   BINSTA,KEY+6                                                     
         BE    *+14                                                             
         MVC   KEY+9(4),=4X'FF'   FORCE TO READ TO NEXT STA                     
         B     BLDSTHI                                                          
*                                                                               
BLDST34  DS    0H                                                               
         CLC   KEY+9(1),BEST       SAME EST?                                    
         BE    *+14                                                             
         MVC   KEY+10(3),=X'FFFFFF'   FORCE TO READ TO NEXT EST                 
         B     BLDSTHI                                                          
*                                                                               
         CLI   KEY+10,0            IF KEY+10 NOT = 0 OR FF                      
         BE    BLDST36             THEN SKIP IT IT'S SPILL                      
         CLI   KEY+10,X'FF'                                                     
         BNE   BLDSTSEQ                                                         
*                                                                               
BLDST36  CHI   R3,STANLINQ         OVER MAX NUMBER OF LINES?                    
         BNH   *+6                                                              
         DC    H'0'           MAX TABLE SIZE EXCEEDED!!!                        
*                                                                               
         L     R4,=A(STATAB)                                                    
BLDST40  CLI   0(R4),X'FF'                                                      
         BNE   BLDST50                                                          
         MVC   0(5,R4),KEY+4       INSERT MKT/STATION                           
         MVI   STTABLNQ(R4),X'FF'  MARK THE END                                 
         AHI   R3,1                UPDATE COUNTER                               
         B     BLDST55                                                          
*                                                                               
BLDST50  DS    0H                                                               
         CLC   0(5,R4),KEY+4       MK/STA ALREADY IN THE TABLE?                 
         BE    BLDST55             YES - READ NEXT BUY REC                      
         AHI   R4,STTABLNQ         NEXT LINE IN THE TABLE                       
         B     BLDST40                                                          
*                                                                               
BLDST55  DS    0H                                                               
         MVC   KEY+9(4),=4X'FF'       FORCE TO READ NEXT MK/STA                 
         B     BLDSTHI                                                          
*                                                                               
BLDSTX   XC    KEY,KEY                                                          
         MVC   KEY(L'BLDSVKEY),BLDSVKEY                                         
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0            RESTORE ORIGINAL RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     GOT TO HAVE ORIGINAL REC BACK!               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXIT                                                             
BLDSVKEY DS    XL13                                                             
         LTORG                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* FINDS MK/STA FOR THIS BUY IN THE TABLE, AND MARKS THE ENTRY PROCESSED         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
MARKTAB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,ADBUY                                                         
         LA    R2,(BUYKMSTA-BUYKEY)(R2)     POINT TO MK/STA                     
         L     R3,=A(STATAB)                                                    
*                                                                               
MARKT20  DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    MARKTX                                                           
         CLC   0(5,R3),0(R2)       SAME MKT/STA?                                
         BE    MARKT40                                                          
         AHI   R3,STTABLNQ         NEXT LINE                                    
         B     MARKT20                                                          
*                                                                               
MARKT40  DS    0H                                                               
         L     R1,ADBUY                                                         
         TM    (BUYRCNTL-BUYKEY)(R1),X'80'    IF RECORD IS DELETED              
         BO    *+8                 DO NOT MARK RECORD APROCESSED                
         OI    5(R3),X'40'         MARK RECORD PROCESSED                        
*                                                                               
MARKTX   J     EXIT                                                             
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* READS ALL ORIGINAL BUYS FOR A GIVEN ESTIMATE, CHECKS IF PROCESSED,            
* DELETES THOSE THAT AREN'T                                                     
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PRCSTTAB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    R3,R3               MAX TABLE ENTRIES CONTROLLER                 
         L     R1,ADEST                                                         
         MVC   PRCSVKEY,0(R1)      SAVE OFF ORIGINAL KEY                        
         XC    KEY,KEY                                                          
         MVC   KEY(1),BAGYMD       FIRST 9 BYTES IF BUY RECORD KEY              
         OI    KEY,X'08'           X'08' IS ON IN AGY/MED                       
         MVC   KEY+1(2),BCLT                                                    
         MVC   KEY+3(1),BPRD                                                    
*                                                                               
PRCSTHI  GOTO1 HIGH                                                             
         B     PRCST30                                                          
*                                                                               
PRCSTSEQ DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
PRCST30  DS    0H                                                               
         CLC   KEY(4),KEYSAVE      SAME A/M,CLT,PRD?                            
         BNE   PRCSTX                                                           
         CLC   KEY+9(1),BEST       SAME EST?                                    
         BE    PRCST40                                                          
         MVC   KEY+10(3),=X'FFFFFF'   FORCE TO READ TO NEXT EST                 
         B     PRCSTHI                                                          
*                                                                               
PRCST40  DS    0H                                                               
         L     R4,=A(STATAB)       POINT TO BEGINNING OF TABLE                  
*                                                                               
PRCST50  DS    0H                                                               
         CLI   0(R4),X'FF'          END OF TABLE?                               
         BE    PRCSTSEQ                                                         
*                                                                               
         CLC   0(5,R4),KEY+4       SAME MKT/STA                                 
         BE    PRCST60                                                          
         AHI   R4,STTABLNQ         NEXT LINE                                    
         B     PRCST50                                                          
*                                                                               
PRCST60  DS    0H                                                               
*                                                                               
         TM    5(R4),X'40'                                                      
         BO    PRCSTSEQ                                                         
*                                                                               
         OI    KEY+L'BUYKEY,X'80'                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   PRCST65                                                          
         GOTO1 WRITE                                                            
*                                                                               
PRCST65  DS    0H                                                               
         L     R6,=A(WKIO)                                                      
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         OI    (BUYRCNTL-BUYKEY)(R6),X'80'                                      
*                                                                               
         LA    R3,P                                                             
         USING PLINED,R3                                                        
         MVC   PSTATUS,=C'DELETE'                                               
         DROP  R3                                                               
         MVC   SVADBUY,ADBUY                                                    
         MVC   ADBUY,AREC                                                       
         BRAS  RE,PRTBUY                                                        
         MVC   ADBUY,SVADBUY                                                    
*                                                                               
         L     R1,=A(UPDCOUNT)                                                  
         ICM   R3,15,0(R1)                                                      
         LA    R3,1(R3)                                                         
         STCM  R3,15,0(R1)                                                      
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   PRCST66                                                          
         GOTO1 PUT                                                              
*                                                                               
PRCST66  DS    0H                  TAKE CARE OF THE ACTIVE KEY                  
         CLI   KEY+10,X'FF'        BRD PASSIVE KEY?                             
         BNE   PRCST70                                                          
         MVC   PRCSVK2,KEY                                                      
         MVI   KEY+3,X'FF'                                                      
         XC    KEY+10(L'KEY-10),KEY+10                                          
         MVC   KEY+11(1),PRCSVK2+12                                             
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   PRCST68                                                          
         OI    KEY+L'BUYKEY,X'80'                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   PRCST68                                                          
         GOTO1 WRITE                                                            
PRCST68  DS    0H                                                               
         MVC   KEY,PRCSVK2                                                      
         GOTO1 HIGH                                                             
*                                                                               
PRCST70  DS    0H                                                               
         B     PRCSTSEQ             READ NEXT RECORD                            
*                                                                               
PRCSTX   XC    KEY,KEY                                                          
         MVC   KEY(L'PRCSVKEY),PRCSVKEY                                         
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0            RESTORE ORIGINAL RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     GOT TO HAVE ORIGINAL REC BACK!               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     EXIT                                                             
PRCSVKEY DS    XL13                                                             
PRCSVK2  DS    XL13                                                             
SVADBUY  DS    XL4                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENHIST                                                      
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PMED     DS    CL1                                                              
         DS    CL3                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL3                                                              
PLINOLD  DS    CL3                                                              
         DS    CL4                                                              
PSTATUS  DS    CL6                                                              
         DS    CL4                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
*                                                                               
BINMKSTA DS    0XL5                                                             
BINMKT   DS    XL2                                                              
BINSTA   DS    XL3                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034SPREPBY02 07/02/19'                                      
         END                                                                    
