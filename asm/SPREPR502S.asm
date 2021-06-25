*          DATA SET SPREPR502S AT LEVEL 241 AS OF 05/01/02                      
*PHASE SPR502A,*                                                                
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
         TITLE 'SPREPR502 (SPR502) - REP -> SPOT BUY X-FER XTRACT'              
         PRINT NOGEN                                                            
*                                                                               
*                                                                               
**********************************************************************          
*                                                                    *          
*  SPREPR502 (SPR502) --- REPPAK TO SPOTPAK OVERNIGHT BUY TRANSFER   *          
*                          EXTRACT OF SPOT INFORMATION               *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* REQUEST CARD:                                                      *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*                                                                    *          
*  AUT04/99 (BU ) --- >INITIAL DEVELOPMENT                           *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                      ***  END TOMBSTONE  ***                       *          
**********************************************************************          
*                                                                               
SPR502   CSECT                                                                  
         NMOD1 0,SPR502,R9,RR=R2                                                
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
         MVC   P+1(17),=C'ENTERING MAINLINE'                                    
         GOTO1 REPORT                                                           
         GOTO1 INIT,DMCB                                                        
         BNZ   MAINERR                                                          
         MVC   P+1(17),=C'INIT SUCCESSFUL  '                                    
         GOTO1 REPORT                                                           
         CLI   QOPT2,C'T'          REQUEST FOR UPDATE/TEST?                     
         BE    RMYN0005            YES                                          
         CLI   QOPT2,C'U'          REQUEST FOR UPDATE?                          
         BNE   RMYN0010            NO  - CREATE FILE                            
RMYN0005 EQU   *                                                                
         BAS   RE,UPDATRUN         YES - DON'T CREATE FILE                      
*                                     DO COMPARISON OF FILES                    
         CLOSE REPSINA                                                          
         CLOSE SPOTSIN                                                          
*                                                                               
         B     RMYN0840            FINISH JOB                                   
RMYN0010 EQU   *                                                                
         XC    SORTREC,SORTREC                                                  
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
*                                                                               
*   NOTE:  SPOT USES A STRANGE KEY SETUP.  BUYS BEGIN WITH A NYBBLE             
*        WHICH INDICATES THE REP, FOLLOWED BY A NYBBLE OF '1' FOR TV,           
*        '2' FOR RADIO.  IT WILL BE NECESSARY TO DYNAMICALLY FIGURE             
*        THIS OUT.  FOR NOW, KRG IS ALWAYS '62', INTEREP IS 'B2'                
*                                                                               
         MVI   KEY,X'62'           SCAN KEY TYPE: KATZ                          
**       MVI   KEY,X'B2'           SCAN KEY TYPE: INTEREP                       
         GOTO1 HIGH                GET FIRST RECORD                             
         B     RMYN0040                                                         
RMYN0020 EQU   *                                                                
         GOTO1 SEQ                                                              
RMYN0040 EQU   *                                                                
         CLI   KEY,X'62'           ALL KEYS PROCESSED? KATZ                     
**       CLI   KEY,X'B2'           ALL KEYS PROCESSED? INTEREP                  
         BNE   RMYN0200            YES - FINISHED                               
*                                                                               
         L     RF,TOTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,TOTCTR                                                        
         CLI   KEY+10,X'FF'        PASSIVE KEY?                                 
         BE    RMYN0020            YES - SKIP IT                                
         GOTO1 GET                                                              
         L     R2,AREC                                                          
         LA    R2,24(R2)           SET A(DESCRIPTION ELT OF BUY)                
         USING BDELEM,R2                                                        
         CLC   BDEND,=X'630901'    BUY ENDS PRIOR TO SEP1/99?                   
         BL    RMYN0020            YES - GO BACK FOR NEXT RECORD                
         DROP  R2                                                               
         L     R2,AREC                                                          
         GOTO1 HELLO,DMCB,(C'G',SPTFILE),(X'70',(R2)),(0,0),(0,0)               
         CLI   DM4,0                                                            
         BNE   RMYN0020            NO ID ELEMENT: SKIP RECORD                   
         L     R2,DM4              SET A(70 ELEMENT FOUND)                      
         GOTO1 HEXIN,DMCB,3(R2),WORK+48,8,0                                     
*                                  SAVE REP CONTRACT NUMBER                     
         MVC   WORK+52(2),15(R2)   SAVE REP CODE                                
         MVI   REALPROD,0          CLEAR REALPROD                               
         MVI   PAIDBUY,0           CLEAR BUYLINE PAID FLAG                      
         XC    ELT0BCTR,ELT0BCTR   CLEAR COUNTER                                
         CLI   KEY+3,X'FF'         POOL PRODUCT KEY?                            
         BNE   RMYN0060            NO  - CAN USE KEY'S PRODUCT                  
*                                  POOL PRODUCTS HAVE REAL PRODUCT              
*                                     CODE IN SPOT BUY ELEMENT.                 
*                                  ALL THESE BUYS COME FROM REP, WHERE          
*                                     THERE IS ONLY A SINGLE PRODUCT            
*                                     IN A BUY.                                 
         L     R2,AREC                                                          
         LA    R2,24(R2)           SET A(01 ELEMENT)                            
RMYN0045 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    RMYN0046            YES                                          
         CLI   0(R2),X'0B'         SPOT ELEMENT?                                
         BNE   RMYN0055            NO                                           
         L     RF,ELT0BCTR         YES - ADD 1 TO SPOTS CTR                     
         LA    RF,1(RF)                                                         
         ST    RF,ELT0BCTR         SAVE COUNTER                                 
         B     RMYN0047                                                         
****     GOTO1 HELLO,DMCB,(C'G',SPTFILE),(X'0B',(R2)),(0,0),(0,0)               
***      CLI   DM4,0                                                            
***      BE    RMYN0047                                                         
RMYN0046 EQU   *                                                                
         OC    ELT0BCTR,ELT0BCTR   ANY SPOTS IN BUY?                            
         BNZ   RMYN0060            YES - PROCESS                                
**       MVC   P+1(18),=C'NO SPOTS FOR BUY: '                                   
**       L     R2,AREC                                                          
**       MVC   P+20(24),0(R2)                                                   
**       GOTO1 REPORT                                                           
**       L     R4,AREC             A(RECORD)                                    
**       ZICM  RF,13(R4),2         SET L(RECORD)                                
**       GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D',RR=Y           
         L     RF,NOSPOTS          NO  -                                        
         LA    RF,1(RF)                                                         
         ST    RF,NOSPOTS                                                       
         B     RMYN0020            SKIP THE RECORD                              
RMYN0047 EQU   *                                                                
         CLI   REALPROD,0          PRODUCT CODE SAVED YET?                      
         BNE   RMYN0050            YES                                          
         MVC   REALPROD,10(R2)     SAVE REAL PRODUCT CODE                       
RMYN0050 EQU   *                                                                
         CLI   PAIDBUY,0           BUY ALREADY MARKED PAID?                     
         BNE   RMYN0055            YES                                          
         OC    4(2,R2),4(R2)       ANY PAY DATE IN ELT?                         
         BZ    RMYN0055            NO                                           
         MVI   PAIDBUY,1           YES - SET FLAG                               
         L     RF,PAIDBUYS         NO  -                                        
         LA    RF,1(RF)                                                         
         ST    RF,PAIDBUYS                                                      
***      MVC   P+1(10),=C'PAID BUY: '                                           
***      GOTO1 REPORT                                                           
RMYN0055 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         B     RMYN0045            GO BACK FOR NEXT                             
RMYN0060 EQU   *                                                                
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
*        BL    RMYN0020                                                         
*        B     RMYN0200                                                         
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
***      BNL   RMYN0200            END WHEN N EXCEEDED!!                        
*   TEST END OF JOB                                                             
*                                                                               
         CLC   PROCCTR,=F'5000'    N CONTRACTS PROCESSED?                       
         BNE   RMYN0080            NO                                           
*                                                                               
         XC    PROCCTR,PROCCTR                                                  
         MVC   P+19(05),=C'BUYS:'                                               
         EDIT  BUYCTR,(8,P+27)                                                  
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,P+37,4,=C'TOG'                                  
         GOTO1 REPORT                                                           
*                                                                               
RMYN0080 EQU   *                                                                
*   TEMPORARY BRANCH TO COUNT BUYS                                              
***      B     RMYN0020            GO BACK FOR NEXT BUY                         
RMYN0100 EQU   *                                                                
RMYN0120 EQU   *                                                                
         XC    SORTREC,SORTREC                                                  
         GOTO1 CLUNPK,DMCB,KEY+1,SCLIENT                                        
*        MVC   SCLIENT,RBUYSPCL    INSERT CLIENT                                
         MVC   SAVKEY13,KEY        SAVE KEY FOR RESTART AND FOR COMMON          
*                                     SOURCE AREA FOR SETUP                     
         CLC   LASTCLT,KEY+1       SAME CLIENT?                                 
         BE    RMYN0140            YES - CLIENT RECORD ALREADY READ             
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
RMYN0140 EQU   *                                                                
         L     R2,AIOAREA2         SET A(IOAREA2) FOR CLIENT REC                
         USING CLTHDR,R2           SET USING                                    
         LA    RF,CLIST            SET A(PRODUCT LIST)                          
         DROP  R2                                                               
         MVC   CHEKPROD,SAVKEY13+3 LOAD KEY PRODUCT                             
         CLI   SAVKEY13+3,X'FF'    POOL BUY?                                    
         BNE   RMYN0160            NO  -                                        
         MVC   CHEKPROD,REALPROD   YES - USE SPOT PRODUCT                       
RMYN0160 EQU   *                                                                
         OC    0(4,RF),0(RF)       ANY ENTRY?                                   
         BNZ   *+6                 YES                                          
         DC    H'0'                NO  - NO PRODUCT FOUND                       
         CLC   3(1,RF),CHEKPROD    PRODUCT FOUND IN LIST?                       
         BE    RMYN0180            YES                                          
         LA    RF,4(RF)            NO  - BUMP TO NEXT ENTRY                     
         B     RMYN0160            GO BACK FOR NEXT                             
RMYN0180 EQU   *                                                                
         MVC   SPROD,0(RF)         INSERT PRODUCT FROM CLIST                    
         MVC   SESTIMAT,SAVKEY13+9 INSERT ESTIMATE                              
***      MVC   SBUYLN#,SAVKEY13+12 INSERT SPOTPAK BUYLINE #                     
         MVC   SBUYLN#,SAVKEY13+11 INSERT SPOTPAK BUYLINE #                     
*                                  MAYBE SOMEONE CALL TELL ME WHAT              
*                                     CONTENTS OF 'BUY DETAILS' IS              
         GOTO1 MSUNPK,DMCB,SAVKEY13+4,WORK,WORK+4                               
         MVC   SSTATION,WORK+4     INSERT STATION                               
         MVC   SCON#,WORK+48       INSERT REP CONTRACT NUMBER                   
         MVC   SREPBYL#,=X'FF'     INSERT REP LINE NUMBER DEFAULT               
         MVC   SREPCOD,WORK+52     INSERT REP CODE                              
         MVC   SBUYKEY,SAVKEY13    INSERT ORIGINAL KEY                          
         MVC   SPAIDBUY,PAIDBUY    INSERT PAID BUYS FLAG                        
*                                                                               
*   TEST DROP DEAD                                                              
***      DC    H'0'                                                             
*   TEST DROP DEAD END                                                          
*                                                                               
         GOTO1 ASORT,DMCB,=C'PUT',SORTREC                                       
*                                                                               
***      MVC   P+5(07),=C'SORTGEN' **TEST**                                     
***      MVC   P+15(48),SORTREC    **TEST**                                     
***      GOTO1 REPORT              **TEST**                                     
         L     RF,SORTCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,SORTCTR          ADD TO SORT OUT COUNT                        
*                                                                               
*   TEST                                                                        
*        C     RF,=F'1000'         END ON N RECORDS                             
*        BE    RMYN0200                                                         
*   TEST END                                                                    
         B     RMYN0020            GO BACK FOR NEXT BUY RECORD                  
*                                                                               
                                                                                
RMYN0200 EQU   *                                                                
*                                                                               
*   TEST SORTBACK                                                               
*        MVC   P+1(08),=C'SORTBACK'                                             
*        GOTO1 REPORT                                                           
*   TEST SORTBACK                                                               
*                                                                               
         GOTO1 GETSORT                                                          
         CLI   STYP,X'FF'          END OF FILE REACHED?                         
         BNE   RMYN0300            NO                                           
         MVI   SORTREC,X'FF'       FILL RECORD WITH FOXES                       
         MVC   SORTREC+1(47),SORTREC                                            
         MVC   REC(48),SORTREC                                                  
         GOTO1 PUTRECS             YES                                          
         L     RF,SORTBACK                                                      
         LA    RF,1(RF)                                                         
         ST    RF,SORTBACK                                                      
         B     RMYN0800                                                         
RMYN0300 EQU   *                                                                
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
         B     RMYN0200            GO BACK FOR NEXT                             
         EJECT                                                                  
***>>>                                                                          
RMYN0800 EQU   *                                                                
         GOTO1 DISPTOTS,DMCB,(RC)                                               
*                                  DISPLAY TOTALS FOR RUN                       
         MVC   P+1(15),=C'CLOSING SPOTOUT'                                      
         GOTO1 REPORT                                                           
*                                                                               
         CLOSE SPOTOUT             CLOSE OUTPUT FILES                           
         MVC   P+1(15),=C'CLOSED  SPOTOUT'                                      
         GOTO1 REPORT                                                           
RMYN0840 EQU   *                                                                
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
         MVI   RCSUBPRG,1                                                       
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
*                                                                               
*                                                                               
         L     RE,UTL                                                           
         MVC   SPOTSYS,4(RE)       SET SPOT SYSTEM ENTRY #                      
*                                                                               
*                                                                               
         CLI   QOPT2,C'U'          UPDATE RUN?                                  
         BE    INIT0020            YES                                          
         CLI   QOPT2,C'T'          UPDATE RUN/SOFT?                             
         BE    INIT0020            YES                                          
         OPEN  (SPOTOUT,(OUTPUT))                                               
         LTR   RF,RF                   TEST OPEN RETURN CODE                    
         BZ    INIT0040            GOOD OPEN                                    
         DC    H'0'                BAD  OPEN                                    
INIT0020 EQU   *                                                                
         OPEN  (SPOTSIN,(INPUT))                                                
         LTR   RF,RF                   TEST OPEN RETURN CODE                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   P+01(14),=C'SPOTSIN OPENED'                                      
         GOTO1 REPORT              **TEST**                                     
         OPEN  (REPSINA,(INPUT))                                                
         LTR   RF,RF                   TEST OPEN RETURN CODE                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   P+01(14),=C'REPSINA OPENED'                                      
         GOTO1 REPORT              **TEST**                                     
INIT0040 EQU   *                                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',=C'NCTFILE X'                     
*                                                                               
         XC    SORTREC,SORTREC                                                  
*                                                                               
         CLI   QOPT2,C'U'          UPDATE RUN?                                  
         BE    INIT0060            YES                                          
         CLI   QOPT2,C'T'          UPDATE RUN/SOFT?                             
         BE    INIT0060            YES                                          
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
*   UPDATRUN:  RETURN, COMPARE, AND PRINT RESULTS FOR SORTFILES                 
*                                                                               
UPDATRUN NTR1                                                                   
**       MVC   P+1(17),=C'ENTERING UPDATRUN'                                    
**       GOTO1 REPORT                                                           
UDAR0010 EQU   *                                                                
         BAS   RE,GETREP           GET REP RECORD                               
         BAS   RE,GETSPOT          GET SPOT RECORD                              
UDAR0020 EQU   *                                                                
         CLC   REC(4),=X'FFFFFFFF' REP END OF FILE REACHED?                     
         BNE   UDAR0040            NO                                           
         CLC   REC2(4),=X'FFFFFFFF' SPOT END OF FILE REACED?                    
         BE    UDAR0800            YES - BOTH FINISHED: END JOB                 
UDAR0040 EQU   *                                                                
*                                                                               
*   TEST END OF JOB                                                             
**       CLC   REPBUYCT,=F'1000'   TEST END                                     
**       BE    UDAR0800                                                         
*   TEST END OF JOB                                                             
*                                                                               
         CLC   REC(18),REC2        REP VS SPOT                                  
         BL    UDAR0060            REP/NO SPOT                                  
         BH    UDAR0080            SPOT/NO REP                                  
**       MVC   P+1(17),=C'REP == SPOT: REP='                                    
**       MVC   P+18(32),REC                                                     
**       MVC   P+52(5),=C'SPOT='                                                
**       MVC   P+57(32),REC2                                                    
**       GOTO1 REPORT                                                           
         B     UDAR0010            EQUAL:  LEAVE AS IS                          
UDAR0060 EQU   *                                                                
**       MVC   P+1(17),=C'REP/NO SPOT: REP='                                    
**       MVC   P+18(32),REC                                                     
**       MVC   P+52(5),=C'SPOT='                                                
**       MVC   P+57(32),REC2                                                    
**       GOTO1 REPORT                                                           
         BAS   RE,GETREP           READ REP SIDE                                
         B     UDAR0020            GO BACK AND COMPARE                          
UDAR0080 EQU   *                                                                
         MVC   P+1(18),=C'SPOT/NO REP: SPOT='                                   
         MVC   P+19(32),REC2                                                    
         MVC   P+59(4),=C'REP='                                                 
         MVC   P+63(32),REC                                                     
         CLC   REC(13),REC2        CHECK WITHIN CONTRACT                        
         BNE   UDAR0100            NOT SAME CONTRACT                            
         CLC   REC+14(4),REC2+14   CHECK CONTRACT                               
         BNE   UDAR0100                                                         
         MVC   P+6(07),=C'DUPE:  '                                              
         L     RF,DUPECTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,DUPECTR                                                       
         CLI   REC+34,1            PAID FLAG SET?                               
         BNE   UDAR0090            NO                                           
         MVC   P+10(2),=C'PD'                                                   
         L     RF,DUPEPAID                                                      
         LA    RF,1(RF)                                                         
         ST    RF,DUPEPAID                                                      
UDAR0090 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,REC+21),(0,P+52)                                  
         GOTO1 REPORT                                                           
         BAS   RE,DROPBUY          DROP SPOT BUY RECORD                         
         B     UDAR0120                                                         
UDAR0100 EQU   *                                                                
         XC    P,P                 CLEAR PRINTLINE                              
***>>>   GOTO1 REPORT                                                           
UDAR0120 EQU   *                                                                
         BAS   RE,GETSPOT          READ SPOT SIDE                               
         B     UDAR0020            GO BACK AND COMPARE                          
UDAR0800 EQU   *                                                                
         MVC   P+1(19),=C'COMPARISON FINISHED'                                  
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         CLI   QOPT2,C'U'          HARD RUN?                                    
         BNE   UDAR0820            NO  - SOFT                                   
         MVC   P+1(17),=C'SPT BUYS DROPPED:'                                    
         EDIT  DROPCTR,(8,P+24)                                                 
         GOTO1 REPORT                                                           
         B     UDAR0830                                                         
UDAR0820 EQU   *                                                                
         MVC   P+1(17),=C'SPT TEST DROPPED:'                                    
         EDIT  TESTCTR,(8,P+24)                                                 
         GOTO1 REPORT                                                           
UDAR0830 EQU   *                                                                
         MVC   P+1(17),=C'DUPES FOUND     :'                                    
         EDIT  DUPECTR,(8,P+24)                                                 
         GOTO1 REPORT                                                           
         MVC   P+1(17),=C'PAID DUPES      :'                                    
         EDIT  DUPEPAID,(8,P+24)                                                
         GOTO1 REPORT                                                           
         MVC   P+1(17),=C'LOST SPOT BUYS  :'                                    
         EDIT  LOSTCTR,(8,P+24)                                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DROPBUY:  USE KEY FROM SPOT SORT REC TO ACCESS KEY, MARK                    
*        AS DELETED, BEFORE PROCEEDING TO NEXT SPOT KEY                         
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
         MVC   P+1(14),=C'KEY NOT FOUND='                                       
         MVC   P+15(24),KEY                                                     
         GOTO1 REPORT                                                           
         L     RF,LOSTCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,LOSTCTR                                                       
         B     DBUY0100            EXIT                                         
DBUY0020 EQU   *                                                                
*                                                                               
*   TEST DISPLAY KEY FOUND                                                      
*                                                                               
         OI    KEY+L'BUYKEY,X'80'  SET KEY DELETE BIT                           
*                                                                               
         CLI   QOPT2,C'T'          REQUEST FOR UPDATE/TEST?                     
         BE    DBUY0060            YES                                          
         GOTO1 WRITE               NO  - MARK BUY FOR DELETION                  
         MVC   P+1(12),=C'KEY DROPPED='                                         
         MVC   P+13(24),KEY                                                     
         GOTO1 REPORT                                                           
         L     RF,DROPCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,DROPCTR                                                       
         B     DBUY0100                                                         
DBUY0060 EQU   *                                                                
         MVC   P+1(18),=C'KEY TO BE DROPPED:'                                   
         MVC   P+20(24),KEY                                                     
         MVC   P+50(4),=C'REP='                                                 
         MVC   P+56(32),REC                                                     
         GOTO1 REPORT                                                           
         L     RF,TESTCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,TESTCTR                                                       
DBUY0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
GETREP   NTR1                                                                   
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
         DS    CL13                                                             
SRECLEN  EQU   *-SORTREC                                                        
*                                                                               
         DS    0H                                                               
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,19,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=48'                                    
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL48                AREA FOR REP BUY RECORD                      
         SPACE 2                                                                
*                                                                               
         DS    F                   LENGTH OF SPOT BUY RECORD                    
REC2     DS    CL48                AREA FOR RECORD                              
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
*                                                                               
*        SAVED REGS FOR NMOD ROUTINES                                           
*                                                                               
SPR2R8   DS    F                              SAVED REG 8                       
SPR2R9   DS    F                              SAVED REG 9                       
SPR2RA   DS    F                              SAVED REG A                       
SORTCTR  DS    F                                                                
SORTBACK DS    F                                                                
NOSPOTS  DS    F                                                                
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
LOSTCTR  DS    F                                                                
*                                                                               
* LOCAL ADDRESSES                                                               
*                                                                               
RELO     DS    A                                                                
AIOAREA1 DS    A                          A(IOAREA1)                            
AIOAREA2 DS    A                          A(IOAREA2)                            
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
**PAN#1  DC    CL21'241SPREPR502S05/01/02'                                      
         END                                                                    
