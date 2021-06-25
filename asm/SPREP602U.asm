*          DATA SET SPREP602U  AT LEVEL 110 AS OF 03/19/03                      
*PHASE SPR602A,*                                                                
*INCLUDE COVAIL                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'SPREPR602 (SPR602) - RTS SPOT BUY AUDIT EXTRACT'                
         PRINT NOGEN                                                            
*                                                                               
*                                                                               
**********************************************************************          
*                                                                    *          
*  SPREPR602 (SPR602) --- RTS SPOT BUY EXTRACT FOR AUDIT             *          
*                          EXTRACT OF SPOT INFORMATION               *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* REQUEST CARD:                                                      *          
*                                                                    *          
*    QOPT2:   A  =  AUDIT REPORT GENERATION                          *          
*             F  =  GENERATE SPOT SIDE FILE OF BUYS                  *          
*    QOPT3:   Y  =  DISPLAY PREVIOUSLY UNTRANSFERRED REP BUYS        *          
*                   AS MISMATCHED                                    *          
*    QOPT4:   Y  =  DISPLAY RAW DATA PRINTOUTS                       *          
*    QOPT5:   Y  =  DISPLAY CANCELLED REP BUYLINES                   *          
*                                                                    *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*                                                                    *          
*  MAR17/00 (BU ) --- >INITIAL DEVELOPMENT                           *          
*                                                                    *          
*  MAR03/03 (BU ) --- >SKIP DISPLAY OF 'NO SPOTS ON REP BUYLINE'     *          
*                                                                    *          
*                      ***  END TOMBSTONE  ***                       *          
**********************************************************************          
*                                                                               
SPR602   CSECT                                                                  
         NMOD1 0,SPR602,R9,RR=R2                                                
*                                                                               
         L     RC,0(R1)                                                         
         USING SPWORKD,RC,RA                                                    
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
*                                                                               
         ST    R2,RELO                                                          
         STM   R2,RC,SAVEREGS                                                   
         STM   R8,RA,SPR2R8                                                     
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         BAS   RE,MAINLINE                                                      
         GOTO1 AENDREQ                                                          
         DC    H'0'                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        MAIN LINE                                                              
*                                                                               
MAINLINE NTR1                                                                   
*                                                                               
**       MVC   P+1(17),=C'ENTERING MAINLINE'                                    
**       GOTO1 REPORT                                                           
         GOTO1 INIT,DMCB                                                        
         BNZ   MAINERR                                                          
**       MVC   P+1(17),=C'INIT SUCCESSFUL  '                                    
**       GOTO1 REPORT                                                           
         CLI   QOPT2,C'A'          REQUEST FOR AUDIT?                           
         BNE   RMYN0020            NO  - CREATE FILE                            
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         BAS   RE,AUDITRUN         YES - DON'T CREATE FILE                      
*                                     DO COMPARISON OF FILES                    
         CLOSE REPSINA                                                          
         CLOSE SPOTSIN                                                          
*                                                                               
         B     RMYN0460            FINISH JOB                                   
         EJECT                                                                  
**********************************************************************          
*    FILE CREATION PATH                                              *          
*        QOPT2 = F:  EXTRACT DATA FROM SPOT FILE FOR SPECIFIED REP   *          
*                                                                    *          
*                                                                    *          
*                                                                               
**********************************************************************          
RMYN0020 EQU   *                                                                
         XC    SORTREC,SORTREC                                                  
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
*                                                                               
*   NOTE:  SPOT USES A STRANGE KEY SETUP.  BUYS BEGIN WITH A NYBBLE             
*        WHICH INDICATES THE REP, FOLLOWED BY A NYBBLE OF '1' FOR TV,           
*        '2' FOR RADIO.                                                         
*                                                                               
         LA    RF,REPTAB           SET A(REP TABLE)                             
RMYN0040 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BNE   *+6                 NO  - PROCEED                                
         DC    H'0'                YES - REP NOT RECOGNIZED:  DIE               
         CLC   QAGY,0(RF)          REP IN REQUEST IN TABLE?                     
         BE    RMYN0060            YES                                          
         LA    RF,LREPTAB(RF)      NO  - BUMP TO NEXT SLOT                      
         B     RMYN0040            GO BACK FOR NEXT                             
RMYN0060 EQU   *                                                                
         MVC   LIVEREP,2(RF)       SAVE AGENCY REP CODE                         
         MVC   MEDCODE,3(RF)       SAVE MEDIA CODE FOR REP                      
         MVC   KEY(1),LIVEREP      SCAN KEY TYPE: REP FROM TABLE                
         GOTO1 HIGH                GET FIRST RECORD                             
         B     RMYN0100                                                         
RMYN0080 EQU   *                                                                
         GOTO1 SEQ                                                              
RMYN0100 EQU   *                                                                
         MVI   PAIDBUY,0           CLEAR BUYLINE PAID FLAG                      
         CLC   KEY(1),LIVEREP      ALL KEYS PROCESSED?                          
         BNE   RMYN0400            YES - FINISHED                               
*                                                                               
         L     RF,TOTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,TOTCTR                                                        
         CLI   KEY+10,X'FF'        PASSIVE KEY?                                 
         BE    RMYN0080            YES - SKIP IT                                
         GOTO1 GET                                                              
         L     R2,AREC                                                          
         LA    R2,24(R2)           SET A(DESCRIPTION ELT OF BUY)                
         USING BDELEM,R2                                                        
         CLC   BDEND,STRTDATE      BUY ENDS PRIOR TO CUTOFF DATE?               
         BL    RMYN0080            YES - GO BACK FOR NEXT RECORD                
         DROP  R2                                                               
*                                                                               
*   TEST RECORD DISPLAY                                                         
****     GOTO1 REPORT                                                           
******   L     R4,AREC             A(RECORD)                                    
*****    ZICM  RF,13(R4),2         SET L(RECORD)                                
****     GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D',RR=Y           
*   TEST RECORD DISPLAY END                                                     
*                                                                               
         L     R2,AREC                                                          
         GOTO1 HELLO,DMCB,(C'G',SPTFILE),(X'70',(R2)),(0,0),(0,0)               
         CLI   DM4,0                                                            
         BNE   RMYN0080            NO ID ELEMENT: SKIP RECORD                   
         L     R2,DM4              SET A(70 ELEMENT FOUND)                      
         GOTO1 HEXIN,DMCB,3(R2),WORK+48,8,0                                     
*                                  SAVE REP CONTRACT NUMBER                     
         MVC   WORK+52(2),=C'XX'   SET DEFAULT REP                              
         CLI   1(R2),X'0F'         SHORT ELEMENT?                               
         BE    RMYN0120            YES - NO REP CODE                            
         MVC   WORK+52(2),15(R2)   NO  - SAVE REP CODE                          
RMYN0120 EQU   *                                                                
         MVC   WORK+54(1),=X'FF'   SET DEFAULT VALUE OF LINE#                   
         CLC   12(3,R2),=C'   '    ANY LINE NUMBER ENTERED?                     
         BNH   RMYN0140            NO                                           
         PACK  DUB(8),12(3,R2)     YES - PACK IT DOWN                           
         CVB   RF,DUB              CONVERT VALUE TO BINARY                      
         STC   RF,WORK+54                                                       
*                                                                               
*   TEST DUMP DISPLAY                                                           
*        MVC   P+1(06),=C'LINE#='                                               
**       MVC   P+7(21),0(R2)                                                    
***      MVC   P+30(1),WORK+54                                                  
****     GOTO1 REPORT                                                           
*****    L     RF,DISPCTR                                                       
****     LA    RF,1(RF)                                                         
***      ST    RF,DISPCTR                                                       
**       CLC   DISPCTR,=F'20'                                                   
*        BNH   RMYN0140                                                         
*        DC    H'0'                                                             
*   TEST DUMP DISPLAY END                                                       
*                                                                               
RMYN0140 EQU   *                                                                
         CLC   =C'XX',WORK+52      DEFAULT ID IN 70?                            
         BNE   RMYN0150            NO  - USE WHAT IS THERE                      
         L     R2,AREC                                                          
         GOTO1 HELLO,DMCB,(C'G',SPTFILE),(X'98',(R2)),(0,0),(0,0)               
         CLI   DM4,0                                                            
         BNE   RMYN0150            NO 98 ELEMENT                                
         L     R2,DM4              SET A(98 ELEMENT FOUND)                      
         MVC   WORK+52(2),11(R2)   INSERT REP POWER CODE                        
RMYN0150 EQU   *                                                                
         MVI   REALPROD,0          CLEAR REALPROD                               
         XC    ELT0BCTR,ELT0BCTR   CLEAR COUNTER                                
         CLI   KEY+3,X'FF'         POOL PRODUCT KEY?                            
         BNE   RMYN0260            NO  - CAN USE KEY'S PRODUCT                  
*                                  POOL PRODUCTS HAVE REAL PRODUCT              
*                                     CODE IN SPOT BUY ELEMENT.                 
*                                  ALL THESE BUYS COME FROM REP, WHERE          
*                                     THERE IS ONLY A SINGLE PRODUCT            
*                                     IN A BUY.                                 
         L     R2,AREC                                                          
         LA    R2,24(R2)           SET A(01 ELEMENT)                            
RMYN0160 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    RMYN0180            YES                                          
         CLI   0(R2),X'0B'         SPOT ELEMENT?                                
         BNE   RMYN0240            NO                                           
         L     RF,ELT0BCTR         YES - ADD 1 TO SPOTS CTR                     
         LA    RF,1(RF)                                                         
         ST    RF,ELT0BCTR         SAVE COUNTER                                 
         B     RMYN0200                                                         
****     GOTO1 HELLO,DMCB,(C'G',SPTFILE),(X'0B',(R2)),(0,0),(0,0)               
***      CLI   DM4,0                                                            
***      BE    RMYN0200                                                         
RMYN0180 EQU   *                                                                
         OC    ELT0BCTR,ELT0BCTR   ANY SPOTS IN BUY?                            
         BNZ   RMYN0260            YES - PROCESS                                
**       MVC   P+1(18),=C'NO SPOTS FOR BUY: '                                   
**       L     R2,AREC                                                          
**       MVC   P+20(24),0(R2)                                                   
**       GOTO1 REPORT                                                           
**       L     R4,AREC             A(RECORD)                                    
**       ZICM  RF,13(R4),2         SET L(RECORD)                                
**       GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D',RR=Y           
         L     RF,NOSPSPTS         NO  -                                        
         LA    RF,1(RF)                                                         
         ST    RF,NOSPSPTS                                                      
         B     RMYN0080            SKIP THE RECORD                              
RMYN0200 EQU   *                                                                
         CLI   REALPROD,0          PRODUCT CODE SAVED YET?                      
         BNE   RMYN0220            YES                                          
         MVC   REALPROD,10(R2)     SAVE REAL PRODUCT CODE                       
RMYN0220 EQU   *                                                                
         CLI   PAIDBUY,0           BUY ALREADY MARKED PAID?                     
         BNE   RMYN0240            YES                                          
         OC    4(2,R2),4(R2)       ANY PAY DATE IN ELT?                         
         BZ    RMYN0240            NO                                           
         MVI   PAIDBUY,1           YES - SET FLAG                               
         L     RF,PAIDBUYS         NO  -                                        
         LA    RF,1(RF)                                                         
         ST    RF,PAIDBUYS                                                      
**       MVC   P+1(10),=C'PAID BUY: '                                           
**       GOTO1 REPORT                                                           
RMYN0240 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         B     RMYN0160            GO BACK FOR NEXT                             
RMYN0260 EQU   *                                                                
*                                                                               
*****    L     R4,AREC             A(RECORD)                                    
*****    ZICM  RF,13(R4),2         SET L(RECORD)                                
*****    GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D',RR=Y           
*                                                                               
*   TEST FIRST DISPLAY                                                          
*        MVC   P+01(05),=C'KEY :'                                               
*        MVC   P+10(13),KEY                                                     
*        L     RF,DM4                                                           
*        MVC   P+30(21),0(RF)                                                   
*        GOTO1 REPORT                                                           
*        C     RF,=F'20'           TEST CUTOFF                                  
*        BL    RMYN0080                                                         
*        B     RMYN0400                                                         
*   TEST FIRST DISPLAY                                                          
         L     RF,PROCCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,PROCCTR                                                       
         L     RF,BUYCTR           PROCESSING COUNTERS                          
         LA    RF,1(RF)                                                         
         ST    RF,BUYCTR                                                        
*                                                                               
*   TEST END                                                                    
***      C     RF,=F'500'          DISPLAY FIRST N BUYS                         
***      BNL   RMYN0400            END WHEN N EXCEEDED!!                        
*   TEST END OF JOB                                                             
*                                                                               
         CLC   PROCCTR,=F'5000'    N CONTRACTS PROCESSED?                       
         BNE   RMYN0280            NO                                           
*                                                                               
         XC    PROCCTR,PROCCTR                                                  
         MVC   P+19(05),=C'BUYS:'                                               
         EDIT  BUYCTR,(8,P+27)                                                  
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,P+37,4,=C'TOG'                                  
         GOTO1 REPORT                                                           
*                                                                               
RMYN0280 EQU   *                                                                
*   TEMPORARY BRANCH TO COUNT BUYS                                              
***      B     RMYN0080            GO BACK FOR NEXT BUY                         
RMYN0300 EQU   *                                                                
RMYN0320 EQU   *                                                                
         XC    SORTREC,SORTREC                                                  
         GOTO1 CLUNPK,DMCB,KEY+1,SCLIENT                                        
*        MVC   SCLIENT,RBUYSPCL    INSERT CLIENT                                
         MVC   SAVKEY13,KEY        SAVE KEY FOR RESTART AND FOR COMMON          
*                                     SOURCE AREA FOR SETUP                     
         CLC   LASTCLT,KEY+1       SAME CLIENT?                                 
         BE    RMYN0340            YES - CLIENT RECORD ALREADY READ             
*                                                                               
*   TEST DISPLAY:                                                               
*        MVC   P+1(24),=C'READING NEW CLIENT: NEW='                             
*        MVC   P+26(2),SAVKEY13+1                                               
*        MVC   P+30(04),=C'OLD='                                                
*        MVC   P+35(2),LASTCLT                                                  
*        GOTO1 REPORT                                                           
*   TEST DISPLAY END                                                            
*                                                                               
         MVC   LASTCLT,KEY+1       SAVE LAST CLIENT                             
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY+1(3),SAVKEY13   INSERT AGY/MEDIA+CLT CODE                    
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(13),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NO  - MUST BE ON FILE                        
         MVC   AREC,AIOAREA2       SET A(IOAREA2) FOR READ                      
         GOTO1 GET                 READ CLIENT RECORD                           
         MVC   KEY(13),SAVKEY13    RESTART SPOT BUY KEYS                        
         GOTO1 HIGH                                                             
         MVC   AREC,AIOAREA1       RESET A(IOAREA)                              
RMYN0340 EQU   *                                                                
         L     R2,AIOAREA2         SET A(IOAREA2) FOR CLIENT REC                
         USING CLTHDR,R2           SET USING                                    
         LA    RF,CLIST            SET A(PRODUCT LIST)                          
         DROP  R2                                                               
         MVC   CHEKPROD,SAVKEY13+3 LOAD KEY PRODUCT                             
         CLI   SAVKEY13+3,X'FF'    POOL BUY?                                    
         BNE   RMYN0360            NO  -                                        
         MVC   CHEKPROD,REALPROD   YES - USE SPOT PRODUCT                       
RMYN0360 EQU   *                                                                
         OC    0(4,RF),0(RF)       ANY ENTRY?                                   
         BNZ   *+6                 YES                                          
         DC    H'0'                NO  - NO PRODUCT FOUND                       
         CLC   3(1,RF),CHEKPROD    PRODUCT FOUND IN LIST?                       
         BE    RMYN0380            YES                                          
         LA    RF,4(RF)            NO  - BUMP TO NEXT ENTRY                     
         B     RMYN0360            GO BACK FOR NEXT                             
RMYN0380 EQU   *                                                                
         MVC   SPROD,0(RF)         INSERT PRODUCT FROM CLIST                    
         MVC   SESTIMAT,SAVKEY13+9 INSERT ESTIMATE                              
***      MVC   SBUYLN#,SAVKEY13+12 INSERT SPOTPAK BUYLINE #                     
         MVC   SBUYLN#,SAVKEY13+11 INSERT SPOTPAK BUYLINE #                     
*                                  MAYBE SOMEONE CALL TELL ME WHAT              
*                                     CONTENTS OF 'BUY DETAILS' IS              
         GOTO1 MSUNPK,DMCB,SAVKEY13+4,WORK,WORK+4                               
         MVC   SSTATION,WORK+4     INSERT STATION                               
         MVC   SCON#,WORK+48       INSERT REP CONTRACT NUMBER                   
         MVC   SREPBYL#,WORK+54    INSERT REP LINE NUMBER OR DEFAULT            
         MVC   SREPCOD,WORK+52     INSERT REP CODE                              
         MVC   SBUYKEY,SAVKEY13    INSERT ORIGINAL KEY                          
         MVC   SPAIDBUY,PAIDBUY    INSERT PAID BUYS FLAG                        
         MVC   SSPOTSIN,ELT0BCTR   INSERT NUMBER OF BUYS                        
         MVI   PAIDBUY,0           CLEAR BUYLINE PAID FLAG                      
*                                                                               
*   TEST DROP DEAD                                                              
***      DC    H'0'                                                             
*   TEST DROP DEAD END                                                          
*                                                                               
         GOTO1 ASORT,DMCB,=C'PUT',SORTREC                                       
*                                                                               
**       CLI   SPAIDBUY,0                                                       
**       BE    RMYN0390                                                         
**       MVC   P+5(07),=C'SORTGEN' **TEST**                                     
**       MVC   P+15(48),SORTREC    **TEST**                                     
**       GOTO1 REPORT              **TEST**                                     
RMYN0390 EQU   *                                                                
         L     RF,SORTCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,SORTCTR          ADD TO SORT OUT COUNT                        
*                                                                               
*   TEST                                                                        
*        C     RF,=F'1000'         END ON N RECORDS                             
*        BE    RMYN0400                                                         
*   TEST END                                                                    
         B     RMYN0080            GO BACK FOR NEXT BUY RECORD                  
*                                                                               
                                                                                
RMYN0400 EQU   *                                                                
*                                                                               
*   TEST SORTBACK                                                               
*        MVC   P+1(08),=C'SORTBACK'                                             
*        GOTO1 REPORT                                                           
*   TEST SORTBACK                                                               
*                                                                               
         GOTO1 GETSORT                                                          
         CLI   STYP,X'FF'          END OF FILE REACHED?                         
         BNE   RMYN0420            NO                                           
         MVI   SORTREC,X'FF'       FILL RECORD WITH FOXES                       
         MVC   SORTREC+1(47),SORTREC                                            
         MVC   REC(48),SORTREC                                                  
         GOTO1 PUTRECS             YES                                          
         L     RF,SORTBACK                                                      
         LA    RF,1(RF)                                                         
         ST    RF,SORTBACK                                                      
         B     RMYN0440                                                         
RMYN0420 EQU   *                                                                
*                                                                               
*   TEST SORTBACK                                                               
*        MVC   P+1(08),=C'SORTREAD'                                             
*        GOTO1 REPORT                                                           
*   TEST SORTBACK                                                               
*                                                                               
         XC    REC,REC                                                          
         MVC   REC(48),SORTREC                                                  
         GOTO1 PUTRECS             YES                                          
         L     RF,SORTBACK                                                      
         LA    RF,1(RF)                                                         
         ST    RF,SORTBACK                                                      
         B     RMYN0400            GO BACK FOR NEXT                             
         EJECT                                                                  
***>>>                                                                          
RMYN0440 EQU   *                                                                
         GOTO1 DISPTOTS,DMCB,(RC)                                               
*                                  DISPLAY TOTALS FOR RUN                       
         MVC   P+1(15),=C'CLOSING SPOTOUT'                                      
         GOTO1 REPORT                                                           
*                                                                               
         CLOSE SPOTOUT             CLOSE OUTPUT FILES                           
         MVC   P+1(15),=C'CLOSED  SPOTOUT'                                      
         GOTO1 REPORT                                                           
RMYN0460 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
DISPTOTS NTR1                                                                   
         MVC   P+1(17),=C'SPOT BUY RECORDS:'                                    
         EDIT  BUYCTR,(8,P+24)                                                  
         GOTO1 REPORT                                                           
         MVC   P+1(17),=C'NO-SPOT BUYS    :'                                    
         EDIT  NOSPOTS,(8,P+24)                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(17),=C'PAID BUYS       :'                                    
         EDIT  PAIDBUYS,(8,P+24)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(17),=C'SORT RECS OUT   :'                                    
         EDIT  SORTCTR,(8,P+24)                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(17),=C'SORT RECS BACK  :'                                    
         EDIT  SORTBACK,(8,P+24)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(17),=C'TOTL BUY RECORDS:'                                    
         EDIT  TOTCTR,(8,P+24)                                                  
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
MAINERR  EQU   *                                                                
         MVC   P+1(17),=C'MAINERR EXIT     '                                    
         GOTO1 REPORT                                                           
         B     TESTBAD                                                          
*        HHOOK --- HEADHOOK ROUTINE                                             
*                                                                               
HHOOK    NTR1                                                                   
*                                                                               
         USING HHOOK,RF                                                         
         LA    R1,SAVEREGS-HHOOK                                                
         AR    R1,RF                                                            
         LM    R2,RC,0(R1)                                                      
         DROP  RF                                                               
*                                                                               
         MVC   H2+94(17),=C'DATA CUTOFF DATE:'                                  
         GOTO1 DATCON,DMCB,(3,STRTDATE),(5,H2+113)                              
*                                                                               
HHOOK100 EQU   *                                                                
         B     TESTGOOD                                                         
         SPACE 2                                                                
SAVEREGS DS    11A                                                              
         EJECT                                                                  
*                                                                               
*        COMMON CODE                                                            
*                                                                               
TESTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
TESTBAD  EQU   *                                                                
         LA    R0,1                                                             
TESTEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        INIT --- SET INITIAL VALUES FOR THIS RUN                               
*                                                                               
INIT     NTR1                                                                   
*                                                                               
         LA    RF,HHOOK                                                         
         ST    RF,HEADHOOK                                                      
         L     RF,=V(HELLO)                                                     
         A     RF,RELO                                                          
         ST    RF,HELLO                                                         
         L     RF,=V(SORTER)                                                    
         A     RF,RELO                                                          
         ST    RF,ASORT                                                         
*                                                                               
         MVI   RCSUBPRG,0                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(3,WORK+6)                                  
         ZIC   RF,WORK+7           EXTRACT MONTH                                
         CLI   WORK+7,3            MARCH OR EARLIER?                            
         BH    INIT0010            NO  - JUST ADJUST MONTH                      
         ZIC   RE,WORK+6           YES - ADJUST YEAR                            
         BCTR  RE,0                BACK UP ONE YEAR                             
         STC   RE,WORK+6           REPLACE YEAR                                 
         LA    RF,12(RF)           SET MONTH UP FOR ADJUSTMENT                  
INIT0010 EQU   *                                                                
         SH    RF,=H'3'            BACK UP THREE MONTHS                         
         STC   RF,WORK+7                                                        
         MVI   WORK+8,1            SET DAY TO 1                                 
         GOTO1 DATCON,DMCB,(3,WORK+6),(3,STRTDATE)                              
**       MVC   P+1(20),=C'AUDIT START DATE IS:'                                 
**       GOTO1 DATCON,DMCB,(3,STRTDATE),(5,P+25)                                
**       GOTO1 REPORT                                                           
*                                                                               
         GOTO1 =V(COVAIL),DMCB,C'GET',F'50000',F'50000',RR=RELO                 
         OC    DM2(4),DM2                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R2,DM2                                                           
         MVC   0(8,R2),=C'*IOAREA*'                                             
         AH    R2,=H'8'                                                         
         ST    R2,AREC                                                          
         ST    R2,AIOAREA1                                                      
         AH    R2,=H'6100'                                                      
         ST    R2,AIOAREA2                                                      
         AH    R2,=H'6100'                                                      
         LA    R2,IOAREA3          STATION READ IO AREA                         
         ST    R2,AIOAREA3                                                      
*                                                                               
*                                                                               
         L     RE,UTL                                                           
         MVC   SPOTSYS,4(RE)       SET SPOT SYSTEM ENTRY #                      
*                                                                               
*                                                                               
         CLI   QOPT2,C'A'          AUDIT  RUN?                                  
         BE    INIT0020            YES                                          
         OPEN  (SPOTOUT,(OUTPUT))  NO  - FILE CREATION RUN                      
         LTR   RF,RF               TEST OPEN RETURN CODE                        
         BZ    INIT0040            GOOD OPEN                                    
         DC    H'0'                BAD  OPEN                                    
INIT0020 EQU   *                                                                
         OPEN  (SPOTSIN,(INPUT))                                                
         LTR   RF,RF                   TEST OPEN RETURN CODE                    
         BZ    *+6                                                              
         DC    H'0'                                                             
**       MVC   P+01(14),=C'SPOTSIN OPENED'                                      
**       GOTO1 REPORT              **TEST**                                     
         OPEN  (REPSINA,(INPUT))                                                
         LTR   RF,RF                   TEST OPEN RETURN CODE                    
         BZ    *+6                                                              
         DC    H'0'                                                             
**       MVC   P+01(14),=C'REPSINA OPENED'                                      
**       GOTO1 REPORT              **TEST**                                     
INIT0040 EQU   *                                                                
*                                                                               
****     GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',=C'NCTFILE X'                     
*                                                                               
         XC    SORTREC,SORTREC                                                  
*                                                                               
         CLI   QOPT2,C'A'          AUDIT  RUN?                                  
         BE    INIT0060            YES                                          
*                                  NO  - OPEN SORT FILE                         
         GOTO1 ASORT,DMCB,SORTCARD,RECCARD,0                                    
INIT0060 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO FOR RETURN                     
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*    SORT RETURN AND END-OF-FILE TESTING                                        
*                                                                               
GETSORT  NTR1                                                                   
         CLI   STYP,X'FF'          EOF REACHED?                                 
         BE    GSOR0080            YES                                          
         MVI   STYP,X'FF'                                                       
         GOTO1 ASORT,DMCB,=C'GET'                                               
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    GSOR0080            TEST RETURN ZERO=EOF                         
         MVC   SORTREC,0(R6)                                                    
*        MVC   P+5(07),=C'GETSORT' **TEST**                                     
*        MVC   P+15(48),SORTREC    **TEST**                                     
*        GOTO1 REPORT              **TEST**                                     
GSOR0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  PUTRECS:  GENERATE OUTFILE ENTRIES                            *              
******************************************************************              
*                                                                               
PUTRECS  NTR1                                                                   
***      LH    RF,REC-4            ADD LENGTH OF 'LENGTH WORD'                  
*                                     TO RECORD CONTROL                         
***      LA    RF,4(RF)                                                         
***      STH   RF,REC-4            PUT IT BACK                                  
         LA    R0,REC                                                           
         PUT   SPOTOUT,(R0)        PUT RECORD TO OUTPUT                         
PUTR0040 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   AUDITRUN:  RETURN, COMPARE, AND PRINT RESULTS FOR SORTFILES                 
*                                                                               
AUDITRUN NTR1                                                                   
**       MVC   P+1(17),=C'ENTERING AUDITRUN'                                    
**       GOTO1 REPORT                                                           
         BAS   RE,SETMEDIA         SET THE MEDIA CODE                           
UDAR0020 EQU   *                                                                
         BAS   RE,GETREP           GET REP RECORD                               
         BAS   RE,GETSPOT          GET SPOT RECORD                              
UDAR0040 EQU   *                                                                
         CLC   REC(4),=X'FFFFFFFF' REP END OF FILE REACHED?                     
         BNE   UDAR0060            NO                                           
         CLC   REC2(4),=X'FFFFFFFF' SPOT END OF FILE REACED?                    
         BE    UDAR0260            YES - BOTH FINISHED: END JOB                 
UDAR0060 EQU   *                                                                
*                                                                               
*   TEST END OF JOB                                                             
***      CLC   REPBUYCT,=F'1000'   TEST END                                     
***      BE    UDAR0260                                                         
*   TEST END OF JOB                                                             
*                                                                               
         CLI   QOPT3,C'Y'          SHOW NON-XFER'D REP BUYS?                    
         BE    UDAR0080            YES - PROCESS ALL BUYS                       
*                                  NO  - CHECK REP BUY FOR SPOT BUY #           
         CLI   REC+SBUYLN#-SORTREC,0                                            
*                                  REP RECORD HAS SPOT BUY#?                    
         BZ    UDAR0160            NO  - SKIP THIS REP BUY                      
UDAR0080 EQU   *                                                                
         CLC   REC(19),REC2        REP VS SPOT                                  
         BL    UDAR0100            REP/NO SPOT                                  
         BH    UDAR0180            SPOT/NO REP                                  
*                                                                               
*   KEYS MATCH:  TREAT AS SUCH                                                  
*                                                                               
**       MVC   P+1(17),=C'REP == SPOT: REP='                                    
**       MVC   P+18(32),REC                                                     
**       MVC   P+52(5),=C'SPOT='                                                
**       MVC   P+57(32),REC2                                                    
**       GOTO1 REPORT                                                           
         CLI   RSCANCEL,C'C'       REP BUY CANCELLED?                           
         BNE   UDAR0020            NO  -                                        
         CLC   REC2+SSPOTSIN-SORTREC(4),SPACES                                  
*                                  YES - DOES SPOT BUY HAVE SPOTS?              
         BNH   UDAR0020            NO                                           
         MVC   P+1(17),=C'REP BUY XCL: REP='                                    
         MVC   P+18(32),REC                                                     
         MVC   P+52(30),=C'ERROR: SPOT BUY CONTAINS SPOTS'                      
         L     RF,UNCLRXCL                                                      
         LA    RF,1(RF)                                                         
         ST    RF,UNCLRXCL                                                      
         B     UDAR0020            GO BACK FOR BOTH                             
UDAR0100 EQU   *                                                                
*                                                                               
*  REROUTE TO SKIP REP SIDE OF REPORT IF 'SPOTONLY'                             
*                                                                               
         CLC   =C'SPOTONLY',QUESTOR                                             
         BNE   UDAR0115            NO  - DON'T SKIP REP SIDE                    
         XC    P,P                 CLEAR PRINT LINE                             
         B     UDAR0160            GET NEXT ENTRY                               
*                                                                               
UDAR0115 EQU   *                                                                
*                                                                               
         MVC   P+1(17),=C'REP/NO SPOT: REP='                                    
         MVC   P+18(32),REC                                                     
         CLI   RSBUYLN#,0                                                       
*                                  REP RECORD HAS SPOT BUY#?                    
         BNZ   UDAR0120            YES - THIS IS A MISMATCH                     
         MVC   P+52(27),=C'REP BUY NOT TRANSFERRED YET'                         
         L     RF,NOPRVXFR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,NOPRVXFR                                                      
         B     UDAR0140                                                         
UDAR0120 EQU   *                                                                
         CLI   RSRSPTWK,0          ANY SPOTS WITH BUYLINE?                      
         BZ    UDAR0125            NO                                           
         CLI   RSCANCEL,C'C'       CANCELLED REP BUYLINE?                       
         BNE   UDAR0124            NO                                           
         MVC   P+52(30),=C'REP BUY CANCELLED: NO SPOT BUY'                      
         L     RF,REPXCLD                                                       
         LA    RF,1(RF)                                                         
         ST    RF,REPXCLD                                                       
         CLI   QOPT5,C'Y'          DISPLAY CANCELLED REP BUYLINES?              
         BE    UDAR0140            YES                                          
UDAR0123 EQU   *                                                                
         XC    P,P                 NO  - CLEAR PRINT LINE                       
         B     UDAR0160            GET NEXT ENTRY                               
UDAR0124 EQU   *                                                                
         BAS   RE,CHKSTA           CHECK FOR STATION EXISTENCE                  
         BZ    UDAR0130            STATION FOUND ON FILE                        
         MVC   P+52(35),=C'STATION SWITCHED? NOT FOUND ON SPOT'                 
         L     RF,NOSTAT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,NOSTAT                                                        
         B     UDAR0140                                                         
UDAR0125 EQU   *                                                                
         MVC   P+52(23),=C'NO SPOTS ON REP BUYLINE'                             
         L     RF,NOSPOTS                                                       
         LA    RF,1(RF)                                                         
         ST    RF,NOSPOTS                                                       
*                                                                               
*   NEXT BRANCH SKIPS DISPLAY OF A MAJOR INFORMATIONAL-ONLY ITEM                
*                                                                               
         B     UDAR0123            SKIP DISPLAY OF THIS BUYLINE                 
*                                                                               
         B     UDAR0140                                                         
UDAR0130 EQU   *                                                                
         MVC   P+52(5),=C'SPOT='                                                
         MVC   P+57(32),REC2                                                    
UDAR0140 EQU   *                                                                
         MVC   PRINTSAV,P          SAVE MAIN PRINT LINE                         
         CLI   QOPT4,C'Y'          PRINT RAW DATA ALSO?                         
         BNE   UDAR0150            NO  - ONLY FORMATTED                         
         GOTO1 REPORT              YES - PRINT RAW DATA LINE                    
UDAR0150 EQU   *                                                                
         BAS   RE,REPNOSPT         FORMAT REP/NO SPOT LINE                      
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
UDAR0160 EQU   *                                                                
         BAS   RE,GETREP           READ REP SIDE                                
         B     UDAR0040            GO BACK AND COMPARE                          
UDAR0180 EQU   *                                                                
         MVC   P+1(18),=C'SPOT/NO REP: SPOT='                                   
         MVC   P+19(32),REC2                                                    
         MVC   P+59(4),=C'REP='                                                 
         MVC   P+63(32),REC                                                     
         CLC   REC(13),REC2        SAME EST/STATION?                            
         BE    UDAR0190            YES                                          
         BL    UDAR0220            REP EST/STATION < SPOT EST/STATION           
         CLC   RECREP(13),REC2     SAME AS LAST EST/STATION (REP)?              
         BNE   UDAR0220            NO                                           
         CLC   RECREP+14(4),REC2+14    SAME CONTRACT AS LAST?                   
         BNE   UDAR0220            NO  -                                        
         B     UDAR0195                                                         
UDAR0190 EQU   *                                                                
         CLC   REC+13(1),REC2+13   SAME SPOT BUY #?                             
         BH    UDAR0195            NO  - SPOT < REP: NO REP BUY                 
         CLC   REC+14(4),REC2+14   YES - SAME CONTRACT?                         
         BNE   UDAR0220            NO  -                                        
UDAR0195 EQU   *                                                                
         MVC   P+6(07),=C'DUPE:  '                                              
         L     RF,DUPECTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,DUPECTR                                                       
         CLI   REC+34,1            PAID FLAG SET?                               
         BNE   UDAR0200            NO                                           
         MVC   P+11(2),=C'PD'                                                   
         L     RF,DUPEPAID                                                      
         LA    RF,1(RF)                                                         
         ST    RF,DUPEPAID                                                      
UDAR0200 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,REC+21),(0,P+52)                                  
         MVC   PRINTSAV,P          SAVE MAIN PRINT LINE                         
         CLI   QOPT4,C'Y'          PRINT RAW DATA ALSO?                         
         BNE   UDAR0210            NO  - ONLY FORMATTED                         
         GOTO1 REPORT                                                           
UDAR0210 EQU   *                                                                
         BAS   RE,SPTNOREP         FORMAT SPOT/NO REP LINE                      
         CLC   =C'SPOTONLY',QUESTOR                                             
         BNE   UDAR0212            NO  -                                        
         MVI   LINE,1              YES - FORCE SINGLE PAGE PRINTOUT             
         CLC   =C'PAID',P+88       PAID LINE?                                   
         BE    UDAR0220            YES - DON'T PRINT THIS LINE                  
UDAR0212 EQU   *                                                                
         GOTO1 REPORT                                                           
*                                                                               
*   PRINTSAV TEST                                                               
**       MVC   P+1(09),=C'PRINTSAV:'                                            
**       MVC   P+10(100),PRINTSAV                                               
**       GOTO1 REPORT                                                           
*   PRINTSAV TEST END                                                           
*                                                                               
         CLC   =C'SPOTONLY',QUESTOR                                             
         BE    UDAR0215            YES - DON'T DOUBLE-SPACE                     
         GOTO1 REPORT                                                           
UDAR0215 EQU   *                                                                
         BAS   RE,DROPBUY          DROP SPOT BUY RECORD                         
         B     UDAR0240                                                         
UDAR0220 EQU   *                                                                
         XC    P,P                 CLEAR PRINTLINE                              
***>>>   GOTO1 REPORT                                                           
UDAR0240 EQU   *                                                                
         BAS   RE,GETSPOT          READ SPOT SIDE                               
         B     UDAR0040            GO BACK AND COMPARE                          
UDAR0260 EQU   *                                                                
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVI   RCSUBPRG,1          CHANGE REPORT HEADING                        
         MVC   P+1(26),=C'AUDIT COMPARISON COMPLETED'                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(37),=C'CORRESPONDING SPOT BUY MISSING      :'                
         EDIT  CORRBUY,(8,P+48),FLOAT=-                                         
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(37),=C'CORRESPONDING SPOT BUY MISSING      :'                
         GOTO1 REPORT                                                           
         MVC   P+1(37),=C'   NO SPOTS ON REP BUYLINE          :'                
         EDIT  NOSPOTS,(8,P+48)                                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(37),=C'REP BUY  CANCELLED:  NO SPOT BUY    :'                
         EDIT  REPXCLD,(8,P+48)                                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(37),=C'SPOT BUY CONTAINS NO SPOTS          :'                
         EDIT  NOSPSPTS,(8,P+48)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(37),=C'  (NO DETAILS WITHIN REPORT)        :'                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(37),=C'STATION SWITCHED? NOT FOUND ON SPOT :'                
         EDIT  NOSTAT,(8,P+48)                                                  
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(37),=C'REP BUY NOT TRANSFERRED YET         :'                
         EDIT  NOPRVXFR,(8,P+48)                                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(37),=C'DUPLICATE SPOT BUYS FOUND           :'                
         EDIT  DUPECTR,(8,P+48)                                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(37),=C'DUPLICATE SPOT BUYS WITH PAID SPOTS :'                
         EDIT  DUPEPAID,(8,P+48)                                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(37),=C'CANCELLED BUYS NOT CLEARED ON SPOT  :'                
         EDIT  UNCLRXCL,(8,P+48)                                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(37),=C'LOST SPOT BUYS                      :'                
         EDIT  LOSTCTR,(8,P+48)                                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   REPNOSPT:  AN UNMATCHED REP BUY RECORD HAS TO BE DISPLAYED.  THE            
*        RECORD IS IN REC/RSORTREC.  THE DESCRIPTIVE INFORMATION IS             
*        SAVED WITHIN PRINTSAV.                                                 
REPNOSPT NTR1                                                                   
         MVC   P,SPACES            CLEAR THE EXISTING PRINTLINE                 
         MVC   P+4(3),RSCLIENT     INSERT CLIENT CODE                           
         MVC   P+09(3),RSPROD      INSERT PRODUCT CODE                          
         EDIT  (1,RSESTIMT),(3,P+16)                                            
         MVC   P+22(4),RSRSTATN    INSERT FIRST 4 CHARS OF STATION              
         CLI   RSRSTATN+4,C' '     ANY MEDIA IN STATION?                        
         BNH   REPN0020            NO  -                                        
         MVI   P+26,C'-'           YES - INSERT SEPARATOR                       
         MVC   P+27(1),RSRSTATN+4  INSERT MEDIA                                 
REPN0020 EQU   *                                                                
         GOTO1 HEXOUT,DMCB,RSCON#,P+30,4,=C'TOG'                                
         EDIT  RSREPBL#,(3,P+41)                                                
         EDIT  RSBUYLN#,(3,P+47)                                                
         MVC   P+54(2),RSREPCOD    INSERT REP CODE                              
         GOTO1 DATCON,DMCB,(3,RSXFRDTE),(5,P+58)                                
         GOTO1 DATCON,DMCB,(2,RSFLTSTR),(5,P2+53)                               
         GOTO1 DATCON,DMCB,(2,RSFLTEND),(5,P2+63)                               
         MVC   P+69(1),RSCANCEL    INSERT CANCEL FLAG                           
         EDIT  RSRSPTWK,(3,P+74),ZERO=NOBLANK                                   
         MVC   P+88(40),PRINTSAV+52                                             
         CLC   P+88(5),=C'SPOT='   SPOT KEY DISPLAYED?                          
         BNE   REPN0040            NO  - LEAVE ERROR MESSAGE                    
         MVC   P+88(40),SPACES     YES - CLEAR OUT MESSAGE                      
         MVC   P+88(30),=C'CORRESPONDING SPOT BUY MISSING'                      
         L     RF,CORRBUY                                                       
         LA    RF,1(RF)                                                         
         ST    RF,CORRBUY                                                       
REPN0040 EQU   *                                                                
         BAS   RE,CLRDUPE          SUPPRESS DUPLICATED PRINTLINE DATA           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SPTNOREP:  AN UNMATCHED SPOT BUY RECORD HAS TO BE DISPLAYED.  THE           
*        RECORD IS IN REC2/SORTREC.  THE DESCRIPTIVE INFORMATION IS             
*        SAVED WITHIN PRINTSAV.  THIS MAY ALSO BE A DUPE.                       
SPTNOREP NTR1                                                                   
         LA    R4,REC2                                                          
         USING SORTREC,R4                                                       
         MVC   P,SPACES            CLEAR THE EXISTING PRINTLINE                 
         MVC   P+4(3),SCLIENT      INSERT CLIENT CODE                           
         MVC   P+09(3),SPROD       INSERT PRODUCT CODE                          
         EDIT  (1,SESTIMAT),(3,P+16)                                            
         MVC   P+22(4),SSTATION    INSERT FIRST 4 CHARS OF STATION              
         CLI   SSTATION+4,C' '     ANY MEDIA IN STATION?                        
         BNH   SPTN0020            NO  -                                        
         MVI   P+26,C'-'           YES - INSERT SEPARATOR                       
         MVC   P+27(1),SSTATION+4  INSERT MEDIA                                 
SPTN0020 EQU   *                                                                
         GOTO1 HEXOUT,DMCB,SCON#,P+30,4,=C'TOG'                                 
         EDIT  SREPBYL#,(3,P+41)                                                
         EDIT  SBUYLN#,(3,P+47)                                                 
         MVC   P+54(2),SREPCOD     INSERT REP CODE                              
         EDIT  SSPOTSIN,(3,P+74),ZERO=NOBLANK                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         MVC   P+88(40),PRINTSAV+52                                             
         CLC   PRINTSAV+6(4),=C'DUPE'                                           
*                                  DUPLICATE SPOT BUY ENCOUNTERED?              
         BNE   SPTN0040            NO  - LEAVE ERROR MESSAGE                    
         CLC   P+41(3),=C'255'     DEFAULT REP BUYLINE NUMBER?                  
         BNE   SPTN0030            NO                                           
         MVC   P+41(3),SPACES      YES - CLEAR PRINT LINE VALUE                 
SPTN0030 EQU   *                                                                
         MVC   P+88(40),SPACES     YES - CLEAR OUT MESSAGE                      
         CLI   SPAIDBUY-SORTREC+REC2,0         PAID?                            
         BE    SPTN0036                        NO                               
         MVC   P+88(32),=C'PAID BUY DUPE: CANNOT DELETE    '                    
         B     SPTN0038                                                         
SPTN0036 EQU   *                                                                
         MVC   P+88(32),=C'SPOT BUY DUPE: VERIFY AND DELETE'                    
SPTN0038 EQU   *                                                                
         L     RF,DUPEBUY                                                       
         LA    RF,1(RF)                                                         
         ST    RF,DUPEBUY                                                       
SPTN0040 EQU   *                                                                
         BAS   RE,CLRDUPE          SUPPRESS DUPLICATED PRINTLINE DATA           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CLRDUPE:  SUPPRESS DATA DUPLICATED FROM PREVIOUS PRINTLINE                  
*                                                                               
CLRDUPE  NTR1                                                                   
*                                                                               
*   LEAVE DUPLICATED DATA IF 'SPOTONLY' RUN                                     
*                                                                               
         CLC   =C'SPOTONLY',QUESTOR                                             
         BE    CDUP0120                                                         
*                                                                               
         ZIC   RF,LINE             CHECK ROOM ON PAGE                           
         LA    RF,2(RF)            TWO LINES LEFT?                              
         STC   RF,BYTE                                                          
         CLC   BYTE,MAXLINES       STILL ROOM ON PAGE?                          
         BNH   CDUP0010            YES                                          
         MVC   LASTPRNT,SPACES     NO  - CLEAR LAST PRINTLINE                   
CDUP0010 EQU   *                                                                
         MVC   PREPPRNT,P          SAVE PRE-CLEARED PRINT LINE                  
         LA    R2,PRINTABL         SET A(PRINT LINE DESCRIPTION)                
CDUP0020 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    CDUP0100            YES - FINISHED                               
         LA    R3,P                                                             
         LA    R4,LASTPRNT                                                      
         ZIC   RF,0(R2)            GET DISPLACEMENT TO FIELD                    
         ZIC   RE,1(R2)            GET LENGTH OF FIELD                          
         AR    R3,RF               SET DISPLACEMENT                             
         AR    R4,RF                                                            
         EX    RE,CDUP0040         COMPARE BY LENGTH                            
         BNE   CDUP0100            NOT SAME:  NO MORE CLEARING                  
         EX    RE,CDUP0060         CLEAR BY LENGTH                              
         LA    R2,LPRINTAB(R2)     BUMP TO NEXT ENTRY                           
         B     CDUP0020            GO BACK FOR NEXT FIELD                       
CDUP0040 EQU   *                                                                
         CLC   0(0,R3),0(R4)                                                    
CDUP0060 EQU   *                                                                
         MVC   0(0,R3),SPACES      CLEAR THE PRINT FIELD                        
CDUP0100 EQU   *                                                                
         MVC   LASTPRNT,PREPPRNT   SAVE CURRENT PRINTLINE                       
CDUP0120 EQU   *                                                                
         XIT1                                                                   
*                                                                               
*    PRINTABL:  DISPLACEMENT AND LENGTH OF FIELDS ON PRINTLINE                  
PRINTABL DC    AL1(04),AL1(3)      CLIENT CODE                                  
LPRINTAB EQU   *-PRINTABL                                                       
         DC    AL1(09),AL1(3)      PRODUCT CODE                                 
         DC    AL1(16),AL1(3)      ESTIMATE NUMBER                              
         DC    AL1(22),AL1(6)      STATION CALLS                                
         DC    AL1(30),AL1(8)      CONTRACT NUMBER                              
         DC    AL1(54),AL1(2)      REP CODE                                     
         DC    AL1(58),AL1(8)      TRANSFER DATE                                
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
*   SETMEDIA:  LOCATE THE REP IN REPTAB, AND SET MEDIA CODE FOR                 
*        DURATION OF THE RUN                                                    
SETMEDIA NTR1                                                                   
         LA    RF,REPTAB           SET A(REP TABLE)                             
SMED0040 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BNE   *+6                 NO  - PROCEED                                
         DC    H'0'                YES - REP NOT RECOGNIZED:  DIE               
         CLC   QAGY,0(RF)          REP IN REQUEST IN TABLE?                     
         BE    SMED0060            YES                                          
         LA    RF,LREPTAB(RF)      NO  - BUMP TO NEXT SLOT                      
         B     SMED0040            GO BACK FOR NEXT                             
SMED0060 EQU   *                                                                
         MVC   MEDCODE,3(RF)       SAVE MEDIA CODE FOR REP                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CHKSTA:  BUILD SPOT STATION KEY, CHECK FOR STATION EXISTENCE.               
*        STATION MIGHT HAVE BEEN SWITCHED ON REP SIDE AND SPOT                  
*        SIDE HAS NOT BEEN DONE YET.  OR VICE VERSA.                            
*                                                                               
CHKSTA   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STAKEY,R4                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED(1),MEDCODE                                               
         MVC   STAKCALL(5),REC+SSTATION-SORTREC                                 
*                                  INSERT STATION CALL LETTERS                  
         MVC   STAKAGY(2),QAGY                                                  
         CLC   STAKEY(9),LASTSTA   STATION PREVIOUSLY SEEN?                     
         BNE   CSTA0020            NO                                           
         CLI   LASTFIND,C'N'       YES - FOUND?                                 
         BE    CSTA0050            NO                                           
         SR    R0,R0               YES -                                        
         B     CSTA0100                                                         
CSTA0020 EQU   *                                                                
         MVI   LASTFIND,C'N'       SET STATION NOT FOUND                        
         MVC   LASTSTA,STAKEY      SAVE 1ST 9 CHARS OF KEY                      
         GOTO1 STAGET                                                           
         CLC   STAKEY(9),KEYSAVE                                                
         BNE   CSTA0050                                                         
         MVI   LASTFIND,C'Y'       SET STATION FOUND                            
         SR    R0,R0               FOUND - SET CC = ZERO                        
         B     CSTA0100                                                         
CSTA0050 EQU   *                                                                
         LTR   RB,RB                                                            
CSTA0100 EQU   *                                                                
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
STAGET   NTR1                                                                   
         MVC   COMMAND,DMREAD                                                   
         MVC   FILE,STATION                                                     
         MVC   KEYSAVE,KEY                                                      
         MVI   DMOUTBTS,X'EF'                                                   
         LA    R2,KEY                                                           
         L     R3,UTL                                                           
         IC    R4,DMINBTS                                                       
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),FILE,(R2),IOAREA3,DMWORK,0           
         MVC   4(1,R3),SPOTSYS                                                  
DMCHECK  EQU   *                                                                
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         MVI   DMOUTBTS,X'FF'                                                   
         BNZ   DMERRS                                                           
         B     CSTAGOOD                                                         
DMERRS   EQU   *                                                                
         CLC   =C'GETREC',COMMAND  TRYING TO READ RECORD?                       
         BE    CSTABAD             YES - WILL BE SKIPPED                        
         DC    H'0'                                                             
CSTAGOOD EQU   *                                                                
         MVC   KEYSAVE,IOAREA3                                                  
         SR    R0,R0                                                            
         B     CSTAEXIT                                                         
CSTABAD  EQU   *                                                                
         LA    R0,1                                                             
CSTAEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DROPBUY:  USE KEY FROM SPOT SORT REC TO ACCESS KEY, TO                      
*        VERIFY ITS EXISTENCE                                                   
*                                                                               
DROPBUY  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(13),REC2+SBUYKEY-SORTREC                                     
*        MVC   P+1(10),=C'KEY FIND ='                                           
*        MVC   P+11(24),KEY                                                     
*        GOTO1 REPORT                                                           
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(13),KEYSAVE     KEY FOUND?                                   
         BE    DBUY0020            YES                                          
         MVC   P+1(13),=C'LOST SPOT BUY'                                        
         MVC   P+16(14),=C'KEY NOT FOUND='                                      
         MVC   P+33(24),KEY                                                     
         GOTO1 REPORT                                                           
         L     RF,LOSTCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,LOSTCTR                                                       
         B     DBUY0100            EXIT                                         
DBUY0020 EQU   *                                                                
*                                                                               
*   DISPLAY KEY WHICH SHOULD BE DROPPED                                         
*                                                                               
***      MVC   P+1(16),=C'KEY S/B DROPPED:'                                     
***      MVC   P+20(24),KEY                                                     
***      MVC   P+50(4),=C'REP='                                                 
***      MVC   P+56(32),REC                                                     
***      GOTO1 REPORT                                                           
         L     RF,TESTCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,TESTCTR                                                       
DBUY0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
GETREP   NTR1                                                                   
*                                                                               
         MVC   RECREP,REC          SAVE PREVIOUS REP RECORD                     
*                                                                               
         GET   REPSINA             GET REP BUY RECORD                           
*                                                                               
         LR    RE,R1                                                            
         LA    RF,REC              SET A(REP BUY FIELD)                         
         LA    R1,32                                                            
         MOVE  ((RF),(R1)),(RE)                                                 
         L     RF,REPBUYCT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,REPBUYCT                                                      
***      MVC   P+1(08),=C'REP REC='                                             
***      MVC   P+9(40),REC                                                      
***      GOTO1 REPORT                                                           
         XIT1                                                                   
         SPACE 5                                                                
BUYSDEAD DC    H'0'                EOD SHOULDN'T HAPPEN                         
*                                                                               
GETSPOT  NTR1                                                                   
*                                                                               
         GET   SPOTSIN             GET SPOT BUY RECORD                          
*                                                                               
         LR    RE,R1                                                            
         LA    RF,REC2             SET A(SPOT BUY FIELD)                        
         LA    R1,48                                                            
         MOVE  ((RF),(R1)),(RE)                                                 
         L     RF,SPTBUYCT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,SPTBUYCT                                                      
***      MVC   P+1(08),=C'SPT REC='                                             
***      MVC   P+9(48),REC2                                                     
***      GOTO1 REPORT                                                           
         XIT1                                                                   
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        THE SORT RECORD                                                        
*                                                                               
         DS    0D                                                               
ASORTREC DS    A                                                                
         DS    0D                                                               
         DC    CL8'*SORTREC'                                                    
SORTREC  DS    0CL48                                                            
STYP     DS    CL1      0          JIC:  SET TO ZERO                            
SCLIENT  DS    CL3     +1          SPOTPAK CLIENT CODE                          
SPROD    DS    CL3     +4          SPOTPAK PRODUCT CODE                         
SESTIMAT DS    CL1     +7          SPOTPAK ESTIMATE NUMBER                      
SSTATION DS    CL5     +8          STATION CALL LETTERS                         
SBUYLN#  DS    CL1     +13         SPOTPAK BUYLINE NUMBER                       
SCON#    DS    CL4     +14         REP CONTRACT NUMBER                          
SREPBYL# DS    CL1     +18         REP BUYLINE NUMBER                           
SREPCOD  DS    CL2     +19         REP CODE                                     
SBUYKEY  DS    CL13    +21         ORIGINAL BUY KEY                             
SPAIDBUY DS    CL1     +34         PAID SPOTS FLAG .NE. 0                       
SSPOTSIN DS    CL4     +35         SPOTS EXIST IN ORDER                         
         DS    CL9     +39                                                      
SRECLEN  EQU   *-SORTREC                                                        
*                                                                               
         DS    0H                                                               
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,19,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=48'                                    
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL48                AREA FOR REP BUY RECORD                      
         ORG   REC                                                              
         SPACE 2                                                                
RSORTREC DS    0CL48                                                            
RSTYP    DS    CL1                 JIC:  SET TO ZERO                            
RSCLIENT DS    CL3                 SPOTPAK CLIENT CODE                          
RSPROD   DS    CL3                 SPOTPAK PRODUCT CODE                         
RSESTIMT DS    CL1                 SPOTPAK ESTIMATE NUMBER                      
RSRSTATN DS    CL5                 STATION CALL LETTERS                         
RSBUYLN# DS    CL1                 SPOTPAK BUYLINE NUMBER                       
RSCON#   DS    CL4                 REP CONTRACT NUMBER                          
RSREPBL# DS    CL1                 REP BUYLINE NUMBER                           
RSREPCOD DS    CL2                 REP CODE                                     
RSXFRDTE DS    CL3                 TRANSFER DATE                                
RSCANCEL DS    CL1                 CANCEL FLAG                                  
RSRSPTWK DS    CL1                 SPOTS PER WEEK                               
RSFLTSTR DS    CL2                 FLIGHT START DATE COMPRESSED                 
RSFLTEND DS    CL2                 FLIGHT END   DATE COMPRESSED                 
         DS    CL18                FILLER                                       
*                                                                               
         ORG                                                                    
         DS    F                   LENGTH OF SPOT BUY RECORD                    
REC2     DS    CL48                AREA FOR RECORD                              
         SPACE 2                                                                
RECREP   DS    CL48                SAVE FOR REP RECORD                          
         SPACE 2                                                                
*                                                                               
*        DCB'S                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'***DCB**'                                                    
SPOTOUT  DCB   DDNAME=SPOTOUT,DSORG=PS,RECFM=FB,LRECL=48,              X        
               BLKSIZE=7200,MACRF=PM,BUFNO=2                                    
*                                                                               
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=FB,MACRF=PM,              X        
               LRECL=40,BLKSIZE=6160,BUFNO=2                                    
*                                                                               
SPOTSIN  DCB   DDNAME=SPOTSIN,DSORG=PS,RECFM=FB,LRECL=48,              X        
               BLKSIZE=7200,MACRF=(GL,PM),BUFNO=2,EODAD=BUYSDEAD                
*                                                                               
         SPACE 3                                                                
REPSINA  DCB   DDNAME=REPSINA,DSORG=PS,RECFM=FB,MACRF=(GL,PM),         X        
               LRECL=40,BLKSIZE=6160,BUFNO=2,EODAD=BUYSDEAD                     
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* LOCAL EQUATES                                                                 
*                                                                               
*        LOCAL CONSTANTS                                                        
*                                                                               
USED     DC    CL4'USED'    STATUS TEXT                                         
EOFKEY   DC    40X'FF'                                                          
USEDFLAG DC    X'00'                                                            
HEADIN   DC    X'00'                                                            
GOTDATA  DC    X'00'                                                            
*                                                                               
*   REPTAB:  REP POWER CODE DETERMINES SPOT AGENCY CODE AS WELL AS              
*        MEDIA TYPE FOR RECORD LOOKUPS                                          
*        CHAR 0 - 1  =  REP POWER CODE                                          
*        CHAR 2 - 3  =  AGENCY POWER CODE                                       
*        CHAR 5      =  MEDIA CODE                                              
*                                                                               
REPTAB   DC    C'BL',X'11',C'T'    BLAIR                                        
LREPTAB  EQU   *-REPTAB                                                         
         DC    C'PV',X'61',C'T'    PETRY                                        
         DC    C'FN',X'81',C'T'    FOX                                          
         DC    C'K3',X'62',C'R'    KATZ RADIO                                   
         DC    C'IR',X'B2',C'R'    INTEREP                                      
         DC    C'V8',X'41',C'T'    PMC SPOT FILE                                
         DC    X'0000'             DELIMITER                                    
LIVEREP  DS    X                   REP IN PROGRESS                              
MEDCODE  DS    CL1                                                              
STRTDATE DS    XL3                 CUTOFF DATE FOR EXTRACT                      
DISPCTR  DS    F                                                                
LASTSTA  DS    CL9                 LAST STATION KEY SOUGHT                      
LASTFIND DS    CL1                 LAST STATION FOUND?                          
*                                  Y  =  YES                                    
*                                  N  =  N                                      
*                                                                               
*                                                                               
*        LOCAL VARIABLES                                                        
*                                                                               
SAVKEY13 DS    CL13                KEY FOR RESTART                              
LASTCLT  DS    CL2                 LAST CLIENT CODE                             
REALPROD DS    CL1                                                              
PAIDBUY  DS    CL1                                                              
CHEKPROD DS    CL1                                                              
PRDCODE  DS    CL2                                                              
PRDP     DS    CL3                 PIGGY PRODUCT CODE                           
BPRDP    DS    CL1                 PIGGY PRODUCT CODE FROM CLIENT REC           
BPRDPOL  DS    CL1                 POOL PRODUCT CODE FROM CLIENT REC            
SAVAPROF DS    CL20                                                             
SAVCPROF DS    CL15                                                             
ELCODE   DS    X                                                                
SPOTSYS  DS    X            SPOT SYS NUMBER (TO AND FROM UTL)                   
STARTKEY DS    CL13                KEY SAVE FOR RESTART                         
IOAREA3  DS    CL256                                                            
PRINTSAV DS    CL132                                                            
LASTPRNT DS    CL132                                                            
PREPPRNT DS    CL132                                                            
*                                                                               
*        SAVED REGS FOR NMOD ROUTINES                                           
*                                                                               
SPR2R8   DS    F                              SAVED REG 8                       
SPR2R9   DS    F                              SAVED REG 9                       
SPR2RA   DS    F                              SAVED REG A                       
SORTCTR  DS    F                                                                
SORTBACK DS    F                                                                
NOSPOTS  DS    F                                                                
NOSPSPTS DS    F                                                                
PROCCTR  DS    F                                                                
BUYCTR   DS    F                                                                
TOTCTR   DS    F                                                                
PAIDBUYS DS    F                                                                
REPBUYCT DS    F                                                                
SPTBUYCT DS    F                                                                
ELT0BCTR DS    F                                                                
DROPCTR  DS    F                                                                
TESTCTR  DS    F                                                                
DUPECTR  DS    F                                                                
DUPEPAID DS    F                                                                
DUPEBUY  DS    F                                                                
CORRBUY  DS    F                                                                
NOSTAT   DS    F                                                                
NOPRVXFR DS    F                                                                
UNCLRXCL DS    F                                                                
LOSTCTR  DS    F                                                                
REPXCLD  DS    F                                                                
*                                                                               
* LOCAL ADDRESSES                                                               
*                                                                               
RELO     DS    A                                                                
AIOAREA1 DS    A                          A(IOAREA1)                            
AIOAREA2 DS    A                          A(IOAREA2)                            
AIOAREA3 DS    A                          A(IOAREA3)                            
HELLO    DS    A                          LINKED HELLO                          
*                                                                               
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
         DS    0D                                                               
         EJECT                                                                  
SBUYRECD DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
MKTHDRD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
STAHDRD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
         ORG   DPTTAB                                                           
         DS    0F                                                               
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DDMASTD                                                        
         EJECT                                                                  
       ++INCLUDE DDREMOTED                                                      
         EJECT                                                                  
       ++INCLUDE SPMEDBLOCK                                                     
         EJECT                                                                  
       ++INCLUDE SPREPPTBUF                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'110SPREP602U 03/19/03'                                      
         END                                                                    
