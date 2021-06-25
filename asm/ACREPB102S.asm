*          DATA SET ACREPB102S AT LEVEL 012 AS OF 05/01/02                      
*PHASE ACB102A,*                                                                
*INCLUDE BUDACC                                                                 
*INCLUDE SQUASHER                                                               
         TITLE 'ACB102 - NEW STYLE BUDGET LIST'                                 
         PRINT NOGEN                                                            
ACB102   CSECT                                                                  
         NMOD1 0,*ACB102*,RA,RR=R5                                              
         L     RC,0(R1)                                                         
         USING ACWORKD,RC                                                       
         LA    R9,SPACEND                                                       
         USING ACB1D,R9                                                         
         ST    R5,RELO                                                          
         MVI   REREADSW,C'N'                                                    
*-------------------------------------------------------------------*           
*            REPORT OPTIONS                                         *           
*                                                                   *           
*  OPTION  1  - PERIOD                 M,Q,H,Y                      *           
*  OPTION  2  - LAST MONTH FOR ACTUAL  1-9,A,B,C                    *           
*  OPTION  3  - ACCOUNT LEVEL TOTALS   1-3,S                        *           
*  OPTION  4  - CONTRA LEVEL           0-3                          *           
*  OPTION  5  - SHOW INACTIVE ACCOUNTS Y                            *           
*-------------------------------------------------------------------*           
*-------------------------------------------------------------------*           
*            PROFILES(PROGPROF)                                     *           
*  PROFILE 1  - NEW PAGE AT LEVEL BREAK  Y,N,A                      *           
*-------------------------------------------------------------------*           
*-------------------------------------------------------------------*           
*        RUN FIRST                                                              
*-------------------------------------------------------------------*           
RUNF01   CLI   MODE,RUNFRST                                                     
         BNE   REQF01                                                           
RUNF02   LA    RE,RELOTAB                                                       
         LA    R1,ATYPES                                                        
RUNF03   L     RF,0(RE)                                                         
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RUNF03                                                           
         B     EXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        REQUEST FIRST                                                          
*-----------------------------------------------------------------*             
REQF01   CLI   MODE,REQFRST                                                     
         BNE   LDGF01                                                           
         ZAP   CONCNT,=P'0'                                                     
         XC    LASTKEY,LASTKEY                                                  
         MVI   WANT,C'N'                                                        
         MVC   BUDNUM,QSRTAREA       BUDGET CODE                                
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
                                                                                
         MVC   WORK(2),RCDATE+6    JAN/1 OF CURRENT YEAR IS DEFAULT             
         MVC   WORK+2(4),=C'0101'                                               
         CLC   QSTART,SPACES                                                    
         BE    *+10                                                             
         MVC   WORK(4),QSTART      CAN OVERRIDE BY REQUEST                      
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+10)                                 
         MVC   PSTART,WORK+10                                                   
*MN                                                                             
         MVC   PSTART+2(1),=X'01'                                               
*MN                                                                             
                                                                                
         MVC   WORK+2(2),=C'12'    DEFAULT END IS DECEMBER                      
         CLC   QEND,SPACES                                                      
         BE    *+10                                                             
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+10)                                 
         MVC   PEND,WORK+10                                                     
*MN                                                                             
         MVC   PEND+2(1),=X'01'                                                 
*MN                                                                             
         MVC   ACTEND,PEND           SAVE END FOR ACTUAL                        
         CLI   QOPT2,C' '                                                       
         BE    REQF05                                                           
         MVC   ACTEND+1(1),QOPT2        MONTH NUMBER                            
         NI    ACTEND+1,X'0F'                                                   
         CLI   QOPT2,C'A'                                                       
         BNE   *+8                                                              
         MVI   ACTEND+1,X'10'                                                   
         CLI   QOPT2,C'B'                                                       
         BNE   *+8                                                              
         MVI   ACTEND+1,X'11'                                                   
         CLI   QOPT2,C'C'                                                       
         BNE   *+8                                                              
         MVI   ACTEND+1,X'12'                                                   
         CLC   ACTEND,PSTART         IF ACTUAL HIGHER THAN START OK             
         BNL   *+10                                                             
         MVC   ACTEND(1),PEND          ELSE THEY MEAN NEXT YEAR                 
         CLC   ACTEND,PEND                                                      
         BNH   *+10                                                             
         MVC   ACTEND,PEND                                                      
                                                                                
REQF05   LA    RE,OPT1TAB                                                       
         MVI   NUMTHS,1            NUMBER OF MONTHS IN PERIOD                   
REQF06   CLI   0(RE),X'FF'                                                      
         BE    REQF09              END OF TABLE(ASSUME 1)                       
         CLC   QOPT1,0(RE)                                                      
         BE    REQF08              FOUND THE OPTION'S VALUE.                    
         LA    RE,2(RE)                                                         
         B     REQF06                                                           
                                                                                
REQF08   MVC   NUMTHS,1(RE)       SUBSTITUTE FOR OPTION VALUE.                  
                                                                                
*        BUILD STRING OF 2 BYTE - PERIOD START/END DATES                        
                                                                                
REQF09   ZIC   R6,NUMTHS           MONTHS                                       
*MN      MH    R6,=H'31'           X DAYS IN A MONTH                            
         XC    PERTAB,PERTAB                                                    
         SR    R3,R3                                                            
         LA    R5,PERTAB                                                        
         MVC   0(2,R5),PSTART                                                   
                                                                                
REQF10   MVC   WORK(2),0(R5)                     THIS START YYMM                
         MVI   WORK+2,1                          DAY 1                          
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+3)                                  
*MN      GOTO1 ADDAY,DMCB,WORK+3,WORK+9,(R6)     ADD DAYS IN PERIOD             
         GOTO1 ADDAY,DMCB,(C'M',WORK+3),WORK+9,(R6)                             
         GOTO1 DATCON,DMCB,(0,WORK+9),(1,FULL)   START OF NEXT PERIOD           
         MVC   WORK(4),WORK+9                    DAY 1                          
         MVC   WORK+4(2),=C'01'                                                 
*MN      GOTO1 ADDAY,DMCB,WORK,WORK+6,F'-1'      LESS ONE DAY                   
         GOTO1 ADDAY,DMCB,(C'D',WORK),WORK+6,F'-1'     LESS ONE DAY             
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,2(R5))  END OF THIS PERIOD             
         LA    R5,4(R5)                                                         
         AH    R3,=H'1'            COUNT NUMBER OF ENTRIES                      
         MVI   0(R5),X'FF'                       END OF TABLE                   
         CLC   FULL(2),PEND                                                     
         BH    REQF15                                                           
         MVC   0(2,R5),FULL                      START OF NEXT                  
         B     REQF10                                                           
                                                                                
*        GET THE BUDGET TYPE RECORD                                             
                                                                                
REQF15   MH    R3,=H'2'                                                         
         STC   R3,NUMBUCKS                                                      
         XC    BUDKEY,BUDKEY                                                    
         LA    R2,BUDKEY                                                        
         USING ACKEYD,R2                                                        
         MVI   ACBTKTYP,ACBTKTEQ                                                
         MVC   ACBTKCMP,QCOMPANY                                                
         MVC   ACBTKNO1+1(1),BUDNUM     BUDGET CODE                             
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',BUDKEY,BUDIO                     
         CLC   BUDIO(5),BUDKEY                                                  
         BE    *+6                                                              
         DC    H'0'                CAN'T READ BUDGET RECORD                     
         LA    R2,BUDIO                                                         
         MVC   TYPHCODE,ACBTKCOD   BUDGET CODE                                  
         LA    R6,ACRECORD                                                      
         USING ACBCD,R6                                                         
REQF16   CLI   ACBCEL,ACBCELEQ                                                  
         BNE   REQF17                                                           
         MVC   TYPHED1,ACBCCOL1                                                 
         MVC   TYPHED2,ACBCCOL2                                                 
         B     REQF19                                                           
         USING ACBVD,R6                                                         
REQF17   CLI   ACBVEL,ACBVELEQ                                                  
         BNE   REQF19                                                           
         CLC   ACBVACUL,QUNIT                                                   
         BE    REQF22                                                           
REQF19   ZIC   R0,ACBVLEN                                                       
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   REQF16                                                           
         B     EXIT                NOT SET-UP FOR THIS U/L                      
                                                                                
REQF22   XC    ACCCNTL,ACCCNTL                                                  
         LA    R5,ACCCNTL          SET UP ACCOUNT CONTROL                       
         USING CTLD,R5                                                          
         MVC   CTLUNT(2),QUNIT     UNIT LEDGER                                  
         MVC   CTLBLV,ACBVACLV     ACCOUNT LEVEL FOR BUDGETTING                 
         CLI   CTLBLV,0                                                         
         BNE   *+8                                                              
         MVI   CTLBLV,1                                                         
                                                                                
         MVI   CNTRTAB,X'FF'        END OF CONTRA TABLE                         
         ZIC   R3,ACBVLEN                                                       
         SH    R3,=H'5'           CALCULATE NO. OF U/L CODE IN ELEMENT          
         BZ    REQF30                                                           
         SR    R2,R2                                                            
         D     R2,=F'3'           R3 HAS NUMBER OF CONTRA U/L CODES             
         LTR   R3,R3                                                            
         BZ    REQF30                                                           
         LA    R4,ACBVCAUN        R4 TO FIRST CONTRA U/L                        
         LA    R5,CNTRTAB                                                       
         USING CTLD,R5                                                          
REQF23   XC    CTLUNT(CTLLNQ),CTLUNT                                            
         MVC   CTLUNT(3),0(R4)     UNIT/LEDGER AND LEVEL TO TABLE               
         CLI   CTLUNT,C'+'                                                      
         BNE   *+16                                                             
         MVC   CTLBLN,CTLBLV       FOR +3(1)  LEVEL IS LENGTH CODE              
         MVC   CTLRLN,CTLBLV                                                    
         LA    R5,CTLLNQ(R5)                                                    
         MVI   0(R5),X'FF'                                                      
         LA    R4,3(R4)            NEXT CONTRA UNIT LEDGER ENTRY                
         BCT   R3,REQF23                                                        
         LA    R5,CNTRTAB                                                       
                                                                                
REQF24   LA    R2,BUDKEY          NOW GET LEDGER RECORD FOR U/L                 
         USING ACKEYD,R2                                                        
         MVC   BUDKEY,SPACES                                                    
         CLI   CTLUNT,0                                                         
         BE    REQF28                                                           
         CLI   CTLUNT,C'+'                                                      
         BE    REQF28                                                           
         MVC   ACKEYACC(1),QCOMPANY                                             
         MVC   ACKEYACC+1(2),CTLUNT      UNIT/LEDGER TO KEY                     
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',BUDKEY,BUDIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                CAN'T READ CONTRA UNIT/LEDGER                
         LA    R2,BUDIO                                                         
         LA    R6,ACRECORD                                                      
         USING ACHEIRD,R6                                                       
REQF26   CLI   ACHREL,X'16'                                                     
         BE    REQF27                                                           
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   REQF26                                                           
         DC    H'0'                 NO HEIRARCHY ELEMENT                        
                                                                                
REQF27   ZIC   R1,CTLBLV           CONTRA-LEVEL BUDGETTING                      
         LTR   R1,R1                                                            
         BZ    REQF27A             BUDGETTING BY UNIT LEDGER                    
         BCTR  R1,0                                                             
         MH    R1,=H'16'                                                        
         LA    R2,ACHRLEVA(R1)                                                  
         MVC   CTLBLN,0(R2)        CONTRA BUDGETTING LENGTH                     
REQF27A  MVC   CTLRLV,CTLBLV       REPORTING LEVEL SAME AS BUDGET               
         MVC   CTLRLN,CTLBLN                                                    
         CLI   QOPT4,C' '          OPTION TO REPORT AT HIGHER LEVEL             
         BE    REQF28                                                           
         CLI   QOPT4,C'0'                                                       
         BNE   *+16                                                             
         MVI   CTLRLV,0            OPTION TO REPORT BY U/L                      
         MVI   CTLRLN,0                                                         
         B     REQF28                                                           
                                                                                
         MVC   BYTE,QOPT4                                                       
         NI    BYTE,X'0F'                                                       
         CLC   BYTE,CTLRLV                                                      
         BNL   REQF28                                                           
         ZIC   R1,BYTE             CONTRA-LEVEL REPORTING                       
         BCTR  R1,0                                                             
         MH    R1,=H'16'                                                        
         LA    R2,ACHRLEVA(R1)                                                  
         CLI   0(R2),0             INVALID LEVEL FOR THIS LEDGER                
         BE    REQF28                                                           
         MVC   CTLRLV,BYTE                                                      
         MVC   CTLRLN,0(R2)        CONTRA BUDGETTING LENGTH                     
REQF28   LA    R5,CTLLNQ(R5)                                                    
         CLI   0(R5),X'FF'                                                      
         BNE   REQF24              NEXT CONTRA UNIT LEDGER ENTRY                
                                                                                
REQF30   DS    0H                                                               
         MVC   BUDHEAD,SPACES      SET UP PERIOD FOR HEADLINE                   
         MVC   BUDHEAD(6),=C'BUDGET'                                            
         MVC   ACTHEAD,SPACES                                                   
         MVC   ACTHEAD(6),=C'ACTUAL'                                            
         GOTO1 DATCON,DMCB,(1,PSTART),(9,BUDHEAD+7)                             
         MVC   ACTHEAD+7(6),BUDHEAD+7                                           
         CLC   PEND,PSTART                                                      
         BE    REQF33              ONE MONTH                                    
         MVC   BUDHEAD+14(2),=C'TO'                                             
         GOTO1 DATCON,DMCB,(1,PEND),(9,BUDHEAD+17)                              
REQF33   CLC   ACTEND,PSTART                                                    
         BE    REQF35              ONE MONTH                                    
         MVC   ACTHEAD+14(2),=C'TO'                                             
         GOTO1 DATCON,DMCB,(1,ACTEND),(9,ACTHEAD+17)                            
REQF35   DS    0H                                                               
         MVI   WANT,C'Y'                                                        
         B     EXIT                                                             
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        FIRST FOR LEDGER                                                       
*--------------------------------------------------------------------*          
LDGF01   CLI   WANT,C'N'                                                        
         BE    EXIT                IGNORE ALL MODES FOR THIS REQUEST            
         CLI   MODE,LEDGFRST                                                    
         BNE   ACF01                                                            
         L     RE,ATYPTAB          SET UP TO CLEAR THE BUDGET TABLES.           
         L     RF,=A(TABLNQ)                                                    
         XCEF                                                                   
         L     RF,=A(MAXITEMS)                                                  
         GOTO1 ,SBBINPAR,0,A(BINTAB1),0,51,(0,15),(RF)                          
                                                                                
         LA    R5,ACCCNTL                                                       
         USING CTLD,R5                                                          
         ZIC   R1,CTLBLV           ACCOUNT BUDGETTING LEVEL                     
         BCTR  R1,0                                                             
         MH    R1,=H'16'                                                        
         L     R6,ADLDGHIR                                                      
         USING ACHEIRD,R6                                                       
         LA    R2,ACHRLEVA(R1)                                                  
         MVC   CTLBLN,0(R2)        ACCOUNT LENGTH FOR THIS LEVEL                
         CLI   CTLBLN,0                                                         
         BNE   *+8                                                              
         MVI   CTLBLN,12           BAD CODE - USE FULL ACCOUNT                  
         MVC   CTLRLV,CTLBLV                                                    
         MVC   CTLRLN,CTLBLN       DEFAULT FOR REPORTING IS BUDGET              
         CLI   QOPT3,C' '          OPTION TO REPORT AT HIGHER LEVEL             
         BE    LDGF10                                                           
         CLI   QOPT3,C'S'          SUPPRESS LEVEL TOTALS                        
         BE    LDGF10                                                           
         MVC   BYTE,QOPT3                                                       
         NI    BYTE,X'0F'                                                       
         CLC   BYTE,CTLRLV                                                      
         BNL   LDGF10                                                           
         ZIC   R1,BYTE             ACCOUNT REPORTING LEVEL                      
         BCTR  R1,0                                                             
         MH    R1,=H'16'                                                        
         LA    R2,ACHRLEVA(R1)                                                  
         CLI   0(R2),0             INVALID LEVEL FOR THIS LEDGER                
         BE    LDGF10                                                           
         MVC   CTLRLV,BYTE                                                      
         MVC   CTLRLN,0(R2)                                                     
LDGF10   LA    R1,1                                                             
         LA    R2,ACHRLEVA         GET NUMBER OF LEVELS                         
         CLI   0(R2),12            IN ACCOUNT STRUCTURE                         
         BE    LDGF13                                                           
         LA    R1,1(R1)                                                         
         LA    R2,16(R2)                                                        
         B     *-16                                                             
LDGF13   STC   R1,BYTE                                                          
                                                                                
         USING DRYVTABD,R8                                                      
         MVC   DRYVTAB,DRYVCON                                                  
         LA    R8,DRYVTAB          R8 = A(MAIN CONTROL TABLE).                  
         LA    R0,5                                                             
         SR    R1,R1                                                            
         LA    R5,ACCCNTL                                                       
LDGF31   MVI   DRYVBUF,0           SET ALL BUFFALO LEVELS TO ZERO               
         CLC   DRYVLVL,CTLBLV      FIND ACCOUNT BUDGETTING LEVEL                
         BH    LDGF32                                                           
         AH    R1,=H'1'                                                         
         STC   R1,DRYVBUF                                                       
         CLI   DRYVBUF,1           IF LEVEL WE ARE BUDGETTING AT                
         BNE   LDGF32                                                           
         CLC   DRYVLVL,BYTE        IS LOWEST LEVEL OF ACCOUNT                   
         BNE   LDGF32                                                           
         MVI   DRYVLAST,ACCLAST    THEN LOWEST MODE WE SEE IS ACCLAST           
LDGF32   LA    R8,DRYVTBLQ(R8)                                                  
         BCT   R0,LDGF31                                                        
                                                                                
         STC   R1,HIGHLVL          SET HIGHEST BUFFALO LEVEL                    
         CLI   QOPT3,C'S'          SUPPRESS LEVEL TOTALS                        
         BNE   *+8                                                              
         MVI   HIGHLVL,1           THIS WILL SUPPRESS LEVELS                    
         CLC   HIGHLVL,2                                                        
         BH    *+8                                                              
         MVI   HIGHLVL,1           SUPPRESS DUPLICTE LEVELS                     
         ZAP   TOTBUDG,=P'0'                                                    
         ZAP   TOTACT,=P'0'                                                     
         BAS   RE,SETBUFF          INITIALIZE BUFFALO TABLE                     
         B     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        PROCESS ALL ACCOUNT LEVELS.                                            
*-------------------------------------------------------------------*           
         USING DRYVTABD,R8                                                      
ACF01    LA    R8,DRYVTAB                                                       
         LA    R0,5                                                             
ACF03    CLC   MODE,DRYVPROC       IS THIS A FIRST MODE                         
         BE    ACF06                                                            
         LA    R8,DRYVTBLQ(R8)                                                  
         BCT   R0,ACF03                                                         
         B     SBAC01                                                           
                                                                                
ACF06    ZICM  R2,DRYVAREC,3       GET A(RECORD FOR THIS LEVEL)                 
         L     R2,ACWORKD(R2)                                                   
         MVC   ALSTREC,LASTIO      A(LAST RECORD)                               
         CLI   DRYVBUF,1           BUFFALO LVL ONE IS BUDGETING LVL             
         BNE   EXIT                                                             
         MVI   SVMODE,0                                                         
         CP    LVLCNT,=P'0'        ANY DATA IN TABLE FROM PREVIOUS BUD          
         BE    ACF12               NOTHING IN TABLE                             
                                                                                
ACF10    MVC   SVMODE,MODE         SAVE THE CURRENT                             
         MVC   MODE,DRYVLAST       FORCE MODE TO LAST                           
         B     ACL10               CLEAR OUT TABLE FROM PREVIOUS                
*                                  I WILL RETURN TO ACF01                       
                                                                                
ACF12    CLI   QOPT5,C'Y'          OPTION TO SHOW INACTIVE ACCOUNTS             
         BNE   *+10                                                             
         AP    LVLCNT,=P'1'        ASSUME 1 LEVEL                               
         USING ACKEYD,R2                                                        
         CLC   LASTKEY,ACKEYACC    COMPARE LAST KEY SAVED                       
         BE    EXIT                AND GET OUT IF EQUAL                         
         MVC   LASTKEY,ACKEYACC    ELSE SAVE IT FOR NEXT TIME                   
                                                                                
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         MVC   PFOURTH,SPACES                                                   
         MVC   P+1(14),ACKEYACC+1  GET ACCT CODE.                               
         LA    R3,P+16             FIND END OF ACCOUNT                          
         CLI   0(R3),X'40'                                                      
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         LA    R3,2(R3)            SET R3 TO START OF NAME                      
                                                                                
         LA    R6,ACRECORD                                                      
         USING ACNAMED,R6                                                       
                                                                                
ACF19    CLI   ACNMEL,0                                                         
         BE    ACF25                                                            
         CLI   ACNMEL,X'20'                                                     
         BE    ACF11                                                            
         ZIC   R0,ACNMLEN                                                       
         AR    R6,R0                                                            
         B     ACF19                                                            
                                                                                
ACF11    ZIC   RF,ACNMLEN                                                       
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ACNMNAME                                                 
         AH    RF,=H'1'                                                         
         LA    R2,P+27                                                          
         SR    R2,R3                                                            
         GOTO1 CHOPPER,DMCB,((RF),WORK),((R2),(R3)),(C'P',3)                    
                                                                                
ACF25    DS    0H                                                               
         L     RE,ABUDBLOK          CLEAR BUDACC CONTROL BLOCK.                 
         LA    RF,L'BUDBLOK                                                     
         XCEF                                                                   
                                                                                
         USING BUDACCD,R7                                                       
         L     R7,ABUDBLOK                                                      
         MVC   BUADMGR,DATAMGR                                                  
         MVC   BUDTCON,DATCON                                                   
         MVC   BUATYTBL,ATYPTAB      START OF TYPE TABLE                        
         L     RF,=A(TYPLNQ)         LENGTH OF TYPE TABLE                       
         ST    RF,BUTYTBLN                                                      
         MVC   BUTYTEND,ATYPTAB      END OF TYPE TABLE                          
         MVC   BUAMTTBL,AAMTTAB      START OF AMOUNT TABLE                      
         L     RF,=A(AMTLNQ)         LENGTH OF AMOUNT                           
         ST    RF,BUAMTBLN                                                      
         MVC   BUAMTEND,AAMTTAB      END OF AMOUNT TABLE                        
         MVC   BUBUDNO+1(1),BUDNUM   SET BUDGET NUMBER FILTER.                  
         MVC   BUSTDATE,PSTART       PASS ANY DATE RESTRICTIONS.                
         MVC   BUTBLPER,NUMTHS       SET LENGTH OF BUDGETTING PERIODS.          
         MVC   BUNDDATE,PEND                                                    
         MVI   BUTYPE,TABLER         TELL BUDACC TO BUILD TABLES.               
         MVC   BUSERKEY,LASTKEY      GET KEY OF LAST RECORD PASSED              
         MVI   REREADSW,C'Y'                                                    
         L     RF,ABUDBLOK                                                      
         GOTO1 BUDACC,DMCB,(RF)                                                 
         BE    *+6                                                              
         DC    H'0'                  BAD ERROR IN BUDACC.                       
         CLI   BUMODE,FINISHED                                                  
         BE    *+6                   COMPLETE TABLES WERE BUILT.                
         DC    H'0'                  NOT ENOUGH ROOM FOR TABLES.                
                                                                                
         CLC   BUAMTTBL,BUAMTEND                                                
         BE    *+8                   NO BUDGET DATA FOR THIS LEVEL.             
         BAS   RE,POSTBUD            ADD BUDGET DATA TO BUFFALO                 
         BAS   RE,REREAD                                                        
         B     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        FIRST FOR SUB-ACCOUNT                                                  
*-------------------------------------------------------------------*           
SBAC01   CLI   MODE,PROCSBAC                                                    
         BNE   ACL01                                                            
         CLI   BUCKTYPE,C' '         ONLY REGULAR BUCKETS                       
         BNE   EXIT                                                             
         MVI   ACTIVITY,C'N'                                                    
         L     R2,ADSUBAC            R2=A(SUB-ACCT HEADER RECORD).              
         ST    R2,ALSTREC            A(LAST RECORD)                             
         USING ACKEYD,R2                                                        
         MVC   UL,ACKEYACC+1                                                    
         MVC   CONUL,ACKEYCON+1                                                 
         MVC   BUFFCON,SPACES        DEFAULT IS ALL CONTRAS                     
                                                                                
         LA    R5,CNTRTAB                                                       
         CLI   0(R5),X'FF'           ALL CONTRAS                                
         BE    SBAC07                                                           
         CLI   0(R5),0               ALL CONTRAS                                
         BE    SBAC07                                                           
SBAC03   CLC   CTLUNT(2),CONUL       LOOK FOR MATCH LEDGER                      
         BE    SBAC05                                                           
         CLI   CTLUNT,C'+'                                                      
         BE    SBAC05                                                           
         LA    R5,CTLLNQ(R5)         IN CONTRA TABLE                            
         CLI   0(R5),X'FF'                                                      
         BNE   SBAC03                                                           
         B     EXIT                  NOT IN TABLE - SKIP ACTUAL                 
                                                                                
SBAC05   LA    R3,ACKEYCON                                                      
         CLI   CTLUNT,C'+'           USING DISP/LENGTH +3(1)                    
         BNE   SBAC06                                                           
         ZIC   R1,CTLUNT+1                                                      
         AR    R3,R1                 R3 TO CONTRA ACCOUNT FIELD                 
SBAC06   ZIC   R1,CTLRLN             LENGTH OF CONTRA                           
         LH    RF,=H'2'              FOR C/U/L                                  
         CLI   CTLUNT,C'+'                                                      
         BNE   *+8                                                              
         LH    RF,=H'-1'             USING LENGTH/DISPLACEMENT FIX              
         AR    R1,RF                 LENGTH FOR EX.                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BUFFCON(0),0(R3)                                                 
                                                                                
SBAC07   LA    R1,BUFFAMT                                                       
         ZIC   R0,NUMBUCKS                                                      
         ZAP   0(8,R1),=P'0'         CLEAR BUFFALO ACCUMS                       
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
                                                                                
         LA    R6,ACRECORD                                                      
         USING TRHISTD,R6                                                       
SBAC09   CLI   TRHSEL,0              FIND HISTORY ELEMENTS.                     
         BE    SBAC50                END OF RECORD                              
         CLI   TRHSEL,X'45'                                                     
         BE    SBAC13                                                           
SBAC11   ZIC   R0,TRHSLEN                                                       
         AR    R6,R0                                                            
         B     SBAC09                                                           
                                                                                
SBAC13   CLC   TRHSYEAR(2),ACTEND                                               
         BH    SBAC11                PAST END DATE FOR ACTUAL                   
         LA    R3,PERTAB                                                        
         LA    R4,BUFFAMT                                                       
SBAC15   CLC   TRHSYEAR(2),0(R3)                                                
         BL    SBAC16                HIST LESS THAN PERIOD START                
         CLC   TRHSYEAR(2),2(R3)                                                
         BNH   SBAC20                OK TO ADD TO BUCKETS                       
                                                                                
SBAC16   LA    R4,16(R4)             NEXT SET OF ACCUMS                         
         LA    R3,4(R3)              NEXT START/END COMBO                       
         CLI   0(R3),X'FF'                                                      
         BNE   SBAC15                                                           
         B     SBAC11                END OF PERIOD TABLE - SKIP ITEM            
                                                                                
SBAC20   MVI   ACTIVITY,C'Y'                                                    
         CLC   UL,=C'28'             FOR SOME FUNNY U/LS ADD CREDITS            
         BE    SBAC30                AND SUBTRACT DEBITS                        
         CLC   UL,=C'29'                                                        
         BE    SBAC30                                                           
         CLC   UL,=C'SI'                                                        
         BE    SBAC30                                                           
         CLC   UL,=C'13'                                                        
         BE    SBAC30                                                           
         CLC   UL,=C'1C'                                                        
         BNE   SBAC25                                                           
         CLC   CONUL,=C'11'                                                     
         BE    SBAC25                                                           
         CLC   CONUL,=C'12'                                                     
         BNE   SBAC30                                                           
SBAC25   AP    8(8,R4),TRHSDR        ACCUMULATE EXPENDITURE FOR                 
         SP    8(8,R4),TRHSCR        THIS PERIOD                                
         B     SBAC11                                                           
SBAC30   AP    8(8,R4),TRHSCR                                                   
         SP    8(8,R4),TRHSDR                                                   
         B     SBAC11                GET NEXT HISTORY ELEMENT.                  
                                                                                
SBAC50   CLI   ACTIVITY,C'Y'         ANY ACTIVITY                               
         BNE   SBAC60                                                           
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFFREC                               
         AP    LVLCNT,=P'1'                                                     
         LA    R7,WORK                                                          
         USING BUAMTBLD,R7                                                      
         MVC   BUACON,BUFFCON                                                   
         XC    BUFFCON,BUFFCON                                                  
         MVI   BUFFCON,X'FF'         ADD TOTAL RECORD                           
         BASR  RE,RF                                                            
         BAS   RE,NAMESBAC                                                      
SBAC60   BAS   RE,REREAD                                                        
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        LAST FOR ALL ACCOUNT LEVELS.                                           
*------------------------------------------------------------------*            
         USING DRYVTABD,R8                                                      
ACL01    LA    R8,DRYVTAB                                                       
         LA    R0,5                                                             
ACL03    CLI   DRYVBUF,0                                                        
         BE    *+14                                                             
         CLC   MODE,DRYVLAST         IS THIS A LAST MODE                        
         BE    ACL06                                                            
         LA    R8,DRYVTBLQ(R8)                                                  
         BCT   R0,ACL03                                                         
         B     REQL01                                                           
                                                                                
ACL06    MVC   ALSTREC,LASTIO        SAVE ADDRESS OF LAST KEY                   
         MVC   THISLVL,DRYVBUF                                                  
         MVI   SVMODE,0                                                         
         CLI   DRYVBUF,1                                                        
         BE    ACL10                 ALREADY HAVE NAME FOR LOW LEVEL            
         CP    LVLCNT,=P'0'          ANY DATA IN TABLE FROM PREV BUDGET         
         BE    ACL07                 NOTHING IN TABLE                           
         LA    R8,DRYVTAB                                                       
         CLI   DRYVBUF,1             FIND LEVEL 1 ENTRY                         
         BE    *+12                                                             
         LA    R8,DRYVTBLQ(R8)                                                  
         B     *-12                                                             
         MVC   SVMODE,MODE           SAVE THE CURRENT                           
         MVC   MODE,DRYVLAST         FORCE MODE TO LAST                         
         B     ACL10                 CLEAR OUT TABLE FROM PREVIOUS              
                                                                                
ACL07    CLI   MODE,LEDGLAST                                                    
         BE    ACL10                                                            
         ZICM  R2,DRYVAREC,3         GET A(RECORD FOR THIS LEVEL)               
         L     R2,ACWORKD(R2)                                                   
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         MVC   PFOURTH,SPACES                                                   
         MVC   P+1(14),ACKEYACC+1 GET ACCT CODE.                                
         LA    R3,P+16               FIND END OF ACCOUNT                        
         CLI   0(R3),X'40'                                                      
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         LA    R3,2(R3)              SET R3 TO START OF NAME                    
                                                                                
         LA    R6,ACRECORD                                                      
         USING ACNAMED,R6                                                       
                                                                                
ACL08    CLI   ACNMEL,0                                                         
         BE    ACL10                                                            
         CLI   ACNMEL,X'20'                                                     
         BE    ACL09                                                            
         ZIC   R0,ACNMLEN                                                       
         AR    R6,R0                                                            
         B     ACL08                                                            
                                                                                
ACL09    ZIC   RF,ACNMLEN                                                       
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ACNMNAME                                                 
         AH    RF,=H'1'                                                         
         LA    R2,P+27                                                          
         SR    R2,R3                                                            
         GOTO1 CHOPPER,DMCB,((RF),WORK),((R2),(R3)),(C'P',3)                    
                                                                                
ACL10    MVC   THISLVL,DRYVBUF                                                  
         CLI   MODE,LEDGLAST                                                    
         BNE   ACL20                                                            
         CLI   HIGHLVL,1                                                        
         BE    EXIT             SKIP LEDGER TOTALS IF ONLY ONE LEVEL            
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         MVC   PFOURTH,SPACES                                                   
         MVC   P+1(13),=C'LEDGER TOTALS'                                        
         B     ACL25                                                            
                                                                                
ACL20    LA    R5,ACCCNTL                                                       
         USING CTLD,R5                                                          
         CLC   DRYVLVL,CTLRLV        CURRENT LVL VS. OPTIONAL REPORT            
         BH    ACL27                 IF IT'S SKIP PRINTING IS LEVEL             
                                                                                
ACL25    BAS   RE,PRTBUF             PROCESS THIS LEVEL                         
ACL27    CLI   THISLVL,1                                                        
         BNE   ACL40                 NOT LOWEST LEVEL                           
         CLI   HIGHLVL,1                                                        
         BE    ACL40                 NO LEDGER TOTS IF ONE OTHER LEVEL          
         ZIC   R2,THISLVL                                                       
         LA    R1,DMCB+8             BUILD LEVEL LIST FOR BUFFALO               
         LA    RF,LVLCNT             RF TO NEXT LEVEL                           
                                                                                
ACL30    XC    0(4,R1),0(R1)         ,1,2,3,4,(X'80',5)                         
         STC   R2,3(R1)                                                         
         AP    0(4,RF),=P'1'         ADD TO NEXT LEVEL                          
         LA    RF,L'LVLCNT(RF)                                                  
         AH    R2,=H'1'                                                         
         CLC   3(1,R1),HIGHLVL                                                  
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     ACL30                                                            
                                                                                
         OI    0(R1),X'80'           END OF LIST                                
         GOTO1 BUFFALO,DMCB,=C'ADD',ABUFF                                       
                                                                                
ACL40    ZIC   RF,THISLVL                                                       
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ABUFF,(X'80',(RF))                        
         ZIC   R3,THISLVL                                                       
         BCTR  R3,0                                                             
         MH    R3,=H'4'                                                         
         LA    R3,LVLCNT(R3)                                                    
         ZAP   0(4,R3),=P'0'         CLEAR LEVEL COUNT                          
         CLI   SVMODE,0              IS THERE A SAVE MODE                       
         BE    EXIT                  IF NOT OK TO GET OUT                       
         MVC   MODE,SVMODE           RESTORE A FIRST MODE                       
         MVI   SVMODE,0                                                         
         B     ACF01                 AND DO THE FIRST MODE                      
         EJECT                                                                  
*------------------------------------------------------------------*            
*        LAST FOR REQUEST                                                       
*------------------------------------------------------------------*            
REQL01   B    EXIT                                                              
         EJECT                                                                  
*------------------------------------------------------------------*            
*        POST BUDGET AMOUNTS TO BUFFALO                                         
*------------------------------------------------------------------*            
POSTBUD  NTR1                                                                   
         USING BUAMTBLD,R7                                                      
         L     R7,AAMTTAB            A(AMOUNT TABLE)                            
POSTBD01 MVC   BUFFCON,BUACON                                                   
         LA    R5,CNTRTAB                                                       
         CLI   0(R5),X'FF'           ALL CONTRAS                                
         BE    POSTBD07                                                         
         CLI   0(R5),0               ALL CONTRAS                                
         BE    POSTBD07                                                         
POSTBD02 CLC   CTLUNT(2),BUACON+1    LOOK FOR MATCH LEDGER                      
         BE    POSTBD04                                                         
         CLI   CTLUNT,C'+'                                                      
         BE    POSTBD04                                                         
         LA    R5,CTLLNQ(R5)         IN CONTRA TABLE                            
         CLI   0(R5),X'FF'                                                      
         BNE   POSTBD02                                                         
         B     EXIT                  NOT IN TABLE - SKIP ACTUAL                 
                                                                                
POSTBD04 MVC   BUFFCON,SPACES                                                   
         ZIC   R1,CTLRLN             LENGTH OF CONTRA                           
         AH    R1,=H'2'              + C/U/L                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BUFFCON(0),BUACON                                                
POSTBD07 BAS   RE,NAMESBAC           GET SUB-ACCOUNT NAME                       
         LA    R1,BUFFAMT                                                       
         ZIC   R0,NUMBUCKS                                                      
         ZAP   0(8,R1),=P'0'         CLEAR BUFFALO ACCUMS                       
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
                                                                                
         ZIC   R0,BUANOMTH           NUMBER OF MONTH ENTRIES                    
         LA    R8,BUAMNTHS           BUDGET AMOUNTS                             
         USING BUAMNTHD,R8                                                      
         LA    R3,PERTAB             PERIOD TABLE                               
         LA    R4,BUFFAMT            BUFFALO AMOUNTS                            
                                                                                
POSTBD09 CLC   BUAEYYMM,2(R3)        BUDGET END VS. PERIOD END                  
         BNH   POSTBD11                                                         
         LA    R3,4(R3)                                                         
         LA    R4,16(R4)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   POSTBD09              TRY NEXT PERIOD                            
         DC    H'0'                  INCONSISTENT BUDGET PERIODS                
                                                                                
POSTBD11 OC    BUAMBUD,BUAMBUD                                                  
         BZ    *+10                                                             
         AP    0(8,R4),BUAMBUD       ADD BUDGET AMOUNT                          
         LA    R8,BUAMLN2Q(R8)       NEXT AMOUNT ENTRY                          
         BCT   R0,POSTBD09                                                      
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFFREC                               
         AP    LVLCNT,=P'1'                                                     
         XC    BUFFCON,BUFFCON                                                  
         MVI   BUFFCON,X'FF'                                                    
         BASR  RE,RF                 ADD THE TOTAL LINE                         
                                                                                
         ZIC   R2,BUANOMTH         R2 = NO. OF PERIODS IN AMOUNT ENTRY.         
         LA    R3,BUAMLN2Q         R3 = L'EACH PERIODS DATA.                    
         MR    R2,R2               R3 = L'ALL PERIODS FOR ENTRY.                
         LA    R3,BUAMLN1Q(R3)     R3 = L'ALL PERIODS + L'AMOUNT DETAIL         
         AR    R7,R3               R8 = A(NEXT AMOUNT ENTRY).                   
         OC    BUANUM,BUANUM                                                    
         BNZ   POSTBD01              PROCESS NEXT AMOUNT ENTRY                  
         B     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        ROUTINE TO RETRIEVE AND PROCESS BUFFALO RECORDS                        
*-------------------------------------------------------------------*           
         USING DRYVTABD,R8                                                      
PRTBUF   NTR1                                                                   
         CLI   PROGPROF,C'A'         NEW PAGE PER ACCOUNT                       
         BE    PRTBUF01                                                         
         CLI   PROGPROF,C'Y'         NEW PAGE AT LEVEL BREAK                    
         BNE   PRTBUF03                                                         
         CLC   THISLVL,LASTLVL       IF THIS LEVEL IS HIGHER                    
         BNL   PRTBUF03              DON'T FORCE NEW PAGE                       
PRTBUF01 MVI   FORCEHED,C'Y'         NEW PAGE AT NEXT LOW LEVEL                 
PRTBUF03 MVC   LASTLVL,THISLVL                                                  
         CLI   THISLVL,1                                                        
         BE    PRTBUF10                                                         
         ZIC   R3,THISLVL            IF NOT LEVEL ONE                           
         BCTR  R3,0                  DON'T PRINT                                
         MH    R3,=H'4'                                                         
         LA    R3,LVLCNT(R3)                                                    
         CP    0(4,R3),=P'1'         IF ONLY ONE ENTRY                          
         BL    EXIT                                                             
PRTBUF10 XC    BUFFREC,BUFFREC                                                  
         ZAP   CONCNT,=P'0'          COUNT CONTRA ACCOUNTS                      
         MVC   LVLDESC,=CL15'LEDGER'                                            
         CLI   DRYVLVL,0                                                        
         BE    PRTBUF12                                                         
         ZIC   R1,DRYVLVL                                                       
         BCTR  R1,0                                                             
         MH    R1,=H'16'                                                        
         L     R6,ADLDGHIR                                                      
         USING ACHEIRD,R6                                                       
         LA    R2,ACHRDESA(R1)                                                  
         MVC   LVLDESC,0(R2)         LEVEL DESCRIPTION                          
PRTBUF12 ZIC   RF,THISLVL                                                       
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,BUFFREC,(RF)                         
         CLI   DMCB+8,0                                                         
         BE    PRTBUF13                                                         
         CLI   QOPT5,C'Y'            OPTION TO SHOW INACTIVE ACCOUNTS           
         BNE   EXIT                                                             
         CLI   THISLVL,1                                                        
         BNE   EXIT                                                             
         MVI   SPACING,2                                                        
         BAS   RE,REPORTER           IF YES FORCE CODE/NAME TO PRINT            
         B     EXIT                                                             
                                                                                
PRTBUF13 LA    R7,AMTWRK             BUILD DUMMY BUAMTBLD LINE                  
         USING BUAMTBLD,R7                                                      
         LA    R8,BUAMNTHS                                                      
         USING BUAMNTHD,R8                                                      
                                                                                
         MVI   BUANUM,0                                                         
         MVC   BUANUM+1(1),BUDNUM    BUDGET NUMBER                              
         MVC   BUACON,BUFFCON        CONTRA ACCOUNT                             
         SR    R0,R0                                                            
         LA    R3,PERTAB             PERIOD TABLE                               
         LA    R4,BUFFAMT                                                       
PRTBUF15 CP    0(8,R4),=P'0'                                                    
         BNE   PRTBUF16                                                         
         CP    8(8,R4),=P'0'                                                    
         BE    PRTBUF20              NO DATA FOR PERIOD                         
                                                                                
PRTBUF16 MVC   BUASYYMM(4),0(R3)     START/END TO WORK ENTRY                    
         ZAP   BUAMBUD,0(8,R4)                                                  
         ZAP   BUAMUSER,8(8,R4)                                                 
         LA    R8,BUAMLN2Q(R8)       NEXT MONTH ENTRY                           
         AH    R0,=H'1'              COUNT NUMBER OF PERIODS                    
                                                                                
PRTBUF20 LA    R4,16(R4)             NEXT BUFFAMT PAIR                          
         LA    R3,4(R3)              NEXT PERIOD PAIR                           
         CLI   0(R3),X'FF'                                                      
         BNE   PRTBUF15              END OF TABLE                               
                                                                                
         STC   R0,BUANOMTH           SAVE NUMBER OF MONTHS                      
         LTR   R0,R0                                                            
         BZ    *+14                  NO DATA TO PRONT                           
         BAS   RE,AMOUNTER           FORMAT AND PRINT                           
         AP    CONCNT,=P'1'                                                     
         ZIC   RF,THISLVL                                                       
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,BUFFREC,(RF)                          
         CLI   DMCB+8,0                                                         
         BNE   EXIT                                                             
         CLI   BUFFCON,X'FF'                                                    
         BNE   PRTBUF13              NOT TOTAL RECORD                           
         CP    CONCNT,=P'1'                                                     
         BE    EXIT                  ONLY ONE CONTRA- NO TOTAL REQUIRED         
         B     PRTBUF13                                                         
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
         USING BUAMTBLD,R7                                                      
AMOUNTER NTR1                                                                   
         CLC   BUACON,SPACES                                                    
         BE    AMT020                ALL CONTRAS, NO NAME SHOWN.                
         MVC   WCONID,SPACES                                                    
         MVC   WCONCODE,BUACON+1                                                
         MVC   SBACKEY,BUACON                                                   
         CLI   SBACKEY,X'FF'                                                    
         BNE   AMT010                                                           
         MVC   WORK,SPACES                                                      
         MVI   WORK,C'*'                                                        
         MVC   WORK+2(15),LVLDESC    SE-UP FOR LEVEL TOTALS                     
         MVC   WORK+18(6),=C'TOTALS'                                            
         MVI   WORK+25,C'*'                                                     
         GOTO1 SQUASHER,DMCB,WORK,26                                            
         MVC   P+28(26),WORK                                                    
         B     AMT020                                                           
AMT010   BAS   RE,GETSBAC            RETURNS A(SBAC ENTRY) IN R2                
         MVC   WCONNAME,15(R2)                                                  
         GOTO1 SQUASHER,DMCB,WCONID,51                                          
         L     RF,DMCB+4                                                        
         GOTO1 CHOPPER,DMCB,((RF),WCONID),(26,P+28),(C'P',3)                    
AMT020   ZAP   TOTBUDG,=P'0'                                                    
         ZAP   TOTACT,=P'0'                                                     
         ZIC   R0,BUANOMTH                                                      
         LA    R8,BUAMNTHS                                                      
                                                                                
         LA    R3,P                                                             
         LA    R1,4                  PRINT 4 LINES AT TIME                      
         USING BUAMNTHD,R8                                                      
AMT040   OC    BUAMBUD,BUAMBUD                                                  
         BZ    *+10                  NULL BUDGETTING PERIOD.                    
         AP    TOTBUDG,BUAMBUD                                                  
         AP    TOTACT,BUAMUSER                                                  
         BAS   RE,FORMAT                                                        
         LA    R3,132(R3)            R3 TO NEXT PRINT LINE                      
         BCT   R1,AMT046                                                        
         BAS   RE,REPORTER           PRINT THE BLOCK                            
         LA    R3,P                  SET FOR NEXT LOOP                          
         LA    R1,4                                                             
AMT046   LA    R8,BUAMLN2Q(R8)                                                  
         BCT   R0,AMT040                                                        
         CLC   P,SPACES                                                         
         BE    *+8                                                              
         BAS   RE,REPORTER           PRINT BLOCK, IF LESS THAN 4 LINES          
         CLI   BUANOMTH,1            IF ONE LINE FOR SUBACCT, NO TOTAL          
         BE    AMTEXIT               LINE.                                      
                                                                                
         LA    R8,TOTALS                                                        
         MVI   TOTTOT,X'FF'          MARK AS TOTAL LINE.                        
         LA    R3,P                                                             
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
AMTEXIT  BAS   RE,REPORTER                                                      
         B     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
FORMAT   NTR1                                                                   
         MVC   WBUDGET,BUAMBUD                                                  
         MVC   WACTUAL,BUAMUSER                                                 
         OC    WBUDGET,WBUDGET                                                  
         BZ    *+10                  NULL BUDGETTING PERIOD.                    
         SRP   WBUDGET,X'3E',5       ROUND BUDGET TO POUNDS/DOLLARS.            
         SRP   WACTUAL,X'3E',5       SAME FOR ACTUAL.                           
         CLI   BUASYYMM,X'FF'                                                   
         BNE   FORM010               NOT THE TOTAL LINE.                        
         MVC   54(5,R3),=C'TOTAL'                                               
         B     FORM020                                                          
                                                                                
FORM010  MVC   WDATE(2),BUASYYMM                                                
         MVI   WDATE+2,1                                                        
         GOTO1 DATCON,DMCB,(1,WDATE),(9,56(R3))                                 
         CLI   NUMTHS,1              1 MONTH ONLY NEED BE PRINTED.              
         BE    FORM015                                                          
         MVC   WDATE(2),BUAEYYMM                                                
         GOTO1 (RF),(R1),,(9,64(R3))                                            
         GOTO1 SQUASHER,DMCB,(0,56(R3)),(C'-',14)                               
FORM015  MVC   74(4,R3),=C'NONE'                                                
         OC    WBUDGET,WBUDGET                                                  
         BZ    FORM022               NULL BUDGETTING PERIOD.                    
FORM020  EDIT  WBUDGET,(10,69(R3)),ZERO=NOBLANK,MINUS=YES                       
FORM022  EDIT  WACTUAL,(10,80(R3)),ZERO=NOBLANK,MINUS=YES                       
         OC    WBUDGET,WBUDGET                                                  
         BZ    EXIT                  NULL BUDGETTING PERIOD.                    
         ZAP   BIGPAK,WACTUAL                                                   
         SP    BIGPAK,WBUDGET        BALANCE IN BIGPAK.                         
         EDIT  (P7,BIGPAK+8),(10,91(R3)),FLOAT=-,ZERO=NOBLANK                   
         CP    WBUDGET,=P'0'         ZERO MAKE VARIANCE UNINTERESTING.          
         BE    EXIT                                                             
         CP    WACTUAL,=P'0'                                                    
         BE    EXIT                                                             
         ZAP   BIGPAK,WACTUAL        GET PCT OF ACTUAL/BUDGET                   
         MP    BIGPAK,=P'10000'      VARIANCE=-BALANCE/BUDGET (PCNT).           
         DP    BIGPAK,WBUDGET                                                   
         EDIT  (P7,BIGPAK),(8,102(R3)),2,FLOAT=-                                
         B     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
REPORTER NTR1                                                                   
         MVC   HEAD4+83(23),BUDHEAD                                             
         MVC   HEAD5+83(23),ACTHEAD                                             
         MVC   HEAD6+83(11),=C'BUDGET TYPE'                                     
         MVC   HEAD6+95(10),TYPHCODE                                            
         MVC   MID1+1(109),HEDLYN1                                              
         MVC   MID2+1(109),HEDLYN2                                              
         LA    RE,TYPHED1+L'TYPHED1-1   SET UP TO RIGHT JUSTIFY LONGEST         
         LA    RF,TYPHED2+L'TYPHED2-1   HEADING OVER COLUMN AND CENTER          
         LA    R1,L'TYPHED1-1           THE SHORTEST ABOVE OR BELOW IT.         
REP010   CLI   0(RE),C' '            FIND SIGNIFICANT L'-1 OF LONGEST           
         BNE   REP020                HEADER IN R1.                              
         CLI   0(RF),C' '                                                       
         BNE   REP024                                                           
         BCTR  RE,0                                                             
         BCTR  RF,0                                                             
         BCT   R1,REP010                                                        
                                                                                
REP020   LA    R2,MID1+68            R2 = A(DEST OF LONGEST HEADER).            
         LA    R3,TYPHED1            R3 = A(LONGEST HEADER)                     
         LA    R7,MID2+68            R7 = A(DEST OF SHORTEST HEADER).           
         LA    R8,TYPHED2            R8 = A(SHORTEST HEADER).                   
         B     REP030                                                           
                                                                                
REP024   LA    R2,MID2+68            AS AT REP020, BUT FOR OPPOSITE             
         LA    R3,TYPHED2            RELATIONSHIP,IE TYPHED2 IS LONGEST         
         LA    R7,MID1+68                                                       
         LA    R8,TYPHED1                                                       
                                                                                
REP030   LA    R0,L'TYPHED1-1        RIGHT JUSTIFY LONGEST HEADER IN            
         SR    R0,R1                 APPROPRIATE HEAD LINE.                     
         AR    R2,R0                 R2 = A(RIGHT JUSTIFIED COL START).         
         EX    R1,*+8                R1 = L'-1 OF LONGEST HEADER.               
         B     *+10                                                             
         MVC   0(0,R2),0(R3)         MOVES IN LONGEST HEADER.                   
         AR    R7,R0                 MINIMUM DISP FOR SHORTES IS THIS           
         CLI   0(RF),C' '            FIND SIG L'-1 OF SHORTEST HEADER           
         BNE   *+6                   WITH RE POINTING TO END OF THE             
         LR    RE,RF                 APPROPRIATE HEADER R1=L'-1 SHORTY          
                                                                                
REP032   CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,REP032                                                        
                                                                                
         LA    R3,L'TYPHED1-1      CENTER SHORTEST HEADER ABOVE OR              
         SR    R3,R1               BELOW LONGEST ONE.                           
         SR    R3,R0               R3 = DIFFERENCE OF THE TWO DISPLCMNT         
         LA    R3,1(R3)            HALF THE DIFFERENC ROUNDED UP                
         SRL   R3,1                ADDED TO MINIMUM DISP IN R7 SHOULD           
         AR    R7,R3               GIVE CENTERED POSITION FOR SHORTEST.         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R8)         MOVES IN THE SHORTEST.                     
REP040   DC    0H'0'                                                            
REP041   GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
NAMESBAC NTR1                                                                   
         USING BUAMTBLD,R7                                                      
         CLC   BUACON,SPACES                                                    
         BE    NBEXT                 BUDGET FOR ALL CONTRAS, NO NAME.           
         MVC   SBACREC,SPACES                                                   
         CLC   LASTKEY+1(2),=C'SR'   THESE LEDGERS                              
         BE    NSB100                HAVE PHANTOM CONTRA.                       
         CLC   LASTKEY+1(2),=C'SF'                                              
         BE    NSB120                                                           
         CLC   LASTKEY+1(2),=C'2P'                                              
         BE    NSB120                                                           
         CLC   LASTKEY+1(2),=C'29'                                              
         BE    NSB120                                                           
                                                                                
NSD020   L     R2,ALSTREC            RE = A(IOAREA).                            
         MVI   REREADSW,C'Y'         SAVE MONACC KEY FOR RE-READ                
         LA    R2,BUDKEY                                                        
         MVC   0(42,R2),SPACES       READ FOR ACCT REC FOR CONTRA.              
         MVC   0(L'ACKEYCON,R2),BUACON                                          
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',BUDKEY,BUDIO,0                   
         TM    DMCB+8,X'10'                                                     
         BO    NBEXT                 NOT FOUND IS OK.                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,BUDIO                                                         
         USING ACKEYD,R2                                                        
                                                                                
         MVC   SBACKEY,ACKEYACC                                                 
         LA    R6,ACRECORD           LOOK FOR NAME ELEMENT.                     
         USING ACNAMED,R6                                                       
NSB060   CLI   ACNMEL,0                                                         
         BE    NSB180                                                           
         CLI   ACNMEL,X'20'                                                     
         BE    NSB080                                                           
         ZIC   R0,ACNMLEN                                                       
         AR    R6,R0                                                            
         B     NSB060                                                           
                                                                                
NSB080   ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SBACNAME(0),ACNMNAME                                             
         B     NSB180                                                           
                                                                                
NSB100   MVC   SBACKEY,BUACON                                                   
         B     NSB180                                                           
                                                                                
NSB120   L     R6,ADSUBAC            GET NAME FROM SUB-ACCT HEADER.             
         AH    R6,DATADISP                                                      
         MVC   SBACKEY,BUACON                                                   
                                                                                
         USING TRSUBHD,R6                                                       
NSB140   CLI   TRSBEL,0                                                         
         BE    NSB100                                                           
         CLI   TRSBEL,X'43'                                                     
         BE    NSB160                                                           
         ZIC   R0,TRSBLEN                                                       
         AR    R6,R0                                                            
         B     NSB140                                                           
                                                                                
NSB160   ZIC   R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         BM    NSB180                                                           
         EXMVC R1,SBACNAME,TRSBNAME                                             
                                                                                
NSB180   GOTO1 BINSRCH,SBBINPAR,(1,SBACREC)                                     
         OC    SBBINPAR+1(3),SBBINPAR+1                                         
         BNZ   *+6                                                              
         DC    H'0'                  TABLE IS FULL.                             
NBEXT    B     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
GETSBAC  NTR1                                                                   
         GOTO1 BINSRCH,SBBINPAR,(2,SBACREC)                                     
         CLI   SBBINPAR,1                                                       
         BNE   *+6                   GOT THE ENTRY SUCCESSFULLY.                
         DC    H'0'                  ENTRY NOT FOUND.                           
         ZICM  R2,SBBINPAR+1,3       R2 = A(DESIRED ENTRY).                     
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        SET BUFFALOC                                                           
*-------------------------------------------------------------------*           
SETBUFF  NTR1                                                                   
         USING BUFFALOD,R4                                                      
         L     R4,ABUFF                                                         
         XC    BUFFDOPT(BUFFFLIP-BUFFDOPT),BUFFDOPT                             
         MVI   BUFFFLIP,C'A'                                                    
         XC    BUFFCB(BUFFDADS-BUFFCB),BUFFCB                                   
         ZIC   R3,HIGHLVL                                                       
         ST    R3,BUFFROWS                                                      
         ZIC   R2,NUMBUCKS                                                      
         ST    R2,BUFFCOLS                                                      
         MR    R2,R2                 COLS X ROWS                                
         MH    R3,=H'8'              X 8                                        
         ST    R3,BUFFLDTA           DATA LENGTH                                
         AH    R3,=H'15'             KEY AND COMMENT                            
         ST    R3,BUFFLALL           RECORD LENGTH                              
         LR    R5,R3                                                            
         SR    R2,R2                                                            
         L     R3,=A(BUFFENDX)       END OF BUFFER                              
         LA    R1,BUFFAREA           LESS START OF BUFFER                       
         SR    R3,R1                 GIVES LENGTH                               
         DR    R2,R5                 DIVIDE BUFFER LEN BY RECORD LEN            
         ST    R3,BUFFCRMX           MAX IN CORE                                
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         LA    R0,5                                                             
         LA    R3,LVLCNT             CLEAR LEVEL COUNTS                         
         ZAP   0(4,R3),=P'0'                                                    
         LA    R3,4(R3)                                                         
         BCT   R0,*-10                                                          
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
REREAD   NTR1                                                                   
         CLI   REREADSW,C'N'                                                    
         BE    RREX                  NO REREAD NECESSARY.                       
         L     R2,ALSTREC                                                       
         MVC   BUDKEY,0(R2)                                                     
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',BUDKEY,BUDIO,0                   
         CLI   DMCB+8,0                                                         
         BE    RREX                                                             
         DC    H'0'                                                             
RREX     B     EXIT                                                             
                                                                                
OKEXIT   CR    RB,RB                                                            
         B     EXIT                                                             
                                                                                
ERREXIT  LTR   RB,RB                                                            
         B     EXIT                                                             
                                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        DATA CONSTANTS                                                         
*-------------------------------------------------------------------*           
RELOTAB  DS    0A                                                               
         DC    A(BUFFALOC)                                                      
         DC    A(BUDBLOK)                                                       
         DC    A(TYPTAB)                                                        
         DC    A(AMTTAB)                                                        
         DC    V(BUDACC)                                                        
         DC    V(SQUASHER)                                                      
         DC    X'FF'                                                            
                                                                                
HEDLYN1  DC    CL27'ACCOUNT',CL29'CONTRA-ACCT',CL26'BUDGETTING'                 
         DC    CL10'ACTUAL',C'VARIANCE  PERCENT'                                
HEDLYN2  DC    CL27'-------',CL29'-----------',CL26'  PERIOD  '                 
         DC    CL10'------',C'--------  ACT/BUD'                                
                                                                                
                                                                                
                                                                                
OPT1TAB  DC    C' ',X'01'            NO INPUT = MONTHLY.                        
         DC    C'M',X'01'            MONTHLY.                                   
         DC    C'Q',X'03'            QUARTERLY.                                 
         DC    C'H',X'06'            HALF-YEARLY.                               
         DC    C'Y',X'0C'            YEARLY.                                    
         DC    AL1(255)              TABLE END.                                 
                                                                                
DRYVCON  DC    AL1(4,0,PROCACC,ACCLAST),AL3(ADACC-ACWORKD)                      
         DC    AL1(3,0,PROCLEVC,LEVCLAST),AL3(ADHEIRC-ACWORKD)                  
         DC    AL1(2,0,PROCLEVB,LEVBLAST),AL3(ADHEIRB-ACWORKD)                  
         DC    AL1(1,0,PROCLEVA,LEVALAST),AL3(ADHEIRA-ACWORKD)                  
         DC    AL1(0,0,LEDGFRST,LEDGLAST),AL3(0)                                
         DC    AL1(255)                                                         
DRYVX    EQU   *-DRYVCON                                                        
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        LITERAL POOL                                                           
*-------------------------------------------------------------------*           
         LTORG                                                                  
         EJECT                                                                  
                                                                                
                                                                                
         BUFF  LINES=500,                                              X        
               ROWS=5,                                                 X        
               COLUMNS=24,                                             X        
               FLAVOR=PACKED,                                          X        
               KEYLIST=(15,A)                                                   
BUFFENDX EQU   *                                                                
                                                                                
                                                                                
         DS    0F                                                               
BUDBLOK  DS    CL(BUDACLNQ+1000)                                                
                                                                                
TYPTAB   DS    0D                                                               
         DS    CL(1*BUTYTBLQ)        TYPE TABLE                                 
         DS    CL1                                                              
TYPLNQ   EQU   *-TYPTAB                                                         
                                                                                
AMTTAB   DS    0D                    AMOUNT TABLE                               
         DS    (800*((12*BUAMLN2Q)+BUAMLN1Q))C                                  
         DS    CL1                                                              
AMTLNQ   EQU   *-AMTTAB                                                         
TABLNQ   EQU   *-TYPTAB                                                         
                                                                                
MAXITEMS EQU   10000                                                            
BINTAB1  DS    0D                                                               
         DS    (MAXITEMS*51)C                                                   
         EJECT                                                                  
*------------------------------------------------------------------*            
*        ACB102 WORKING STORAGE                                                 
*------------------------------------------------------------------*            
ACB1D    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
ABUFF    DS    A                                                                
ABUDBLOK DS    A                                                                
ATYPTAB  DS    A                     A(TYPE TABLE)                              
AAMTTAB  DS    A                     A(AMOUNT TABLE)                            
BUDACC   DS    V                                                                
SQUASHER DS    V                                                                
                                                                                
BUFFREC  DS    0CL207                                                           
BUFFCON  DS    CL15                  CONTRA ACCT                                
BUFFAMT  DS    24PL8                 12 PERIODS(BUDGET / ACTUAL)                
                                                                                
                                                                                
SBACREC  DS    0CL51                                                            
SBACKEY  DS    CL15                                                             
SBACNAME DS    CL36                                                             
                                                                                
SBBINPAR DS    6F                                                               
LASTKEY  DS    CL32                                                             
PSTART   DS    CL2                                                              
         DS    CL1                                                              
PEND     DS    CL2                                                              
         DS    CL1                                                              
ACTEND   DS    CL2                                                              
WDATE    DS    CL3                                                              
WBUDGET  DS    PL8                                                              
WACTUAL  DS    PL8                                                              
BIGPAK   DS    PL15                                                             
                                                                                
LVLCNT   DS    5PL4                  COUNT NUMBER IN EACH LEVEL                 
                                                                                
WCONID   DS    0CL51                                                            
WCONCODE DS    CL14                                                             
         DS    CL1                                                              
WCONNAME DS    CL36                                                             
                                                                                
                                                                                
AMTWRK   DS    CL((12*BUAMLN2Q)+BUAMLN1Q+1)                                     
                                                                                
TOTALS   DS    0CL20                                                            
TOTTOT   DS    CL4                                                              
TOTBUDG  DS    PL8                                                              
TOTACT   DS    PL8                   PERIOD TOTALS                              
                                                                                
DRYVTAB  DS    CL(DRYVX)                                                        
                                                                                
REREADSW DS    CL1                                                              
TYPHCODE DS    CL10                                                             
TYPHED1  DS    CL10                                                             
TYPHED2  DS    CL10                                                             
BUDHEAD  DS    CL23                                                             
ACTHEAD  DS    CL23                                                             
UL       DS    CL2                                                              
CONUL    DS    CL2                                                              
CONCNT   DS    PL4                                                              
                                                                                
ALSTREC  DS    A                     A(LAST MONACC KEY)                         
BUDNUM   DS    CL1                   BUDGET NUMBER                              
NUMTHS   DS    CL1                   NUMBER OF MONTHS IN EACH PERIOD            
NUMBUCKS DS    CL1                   TOTAL NUMBER OF BUCKETS                    
PERTAB   DS    CL49                  UP TO 12 SETS (START/END) PERIOD           
ACTIVITY DS    CL1                                                              
SVMODE   DS    CL1                                                              
THISLVL  DS    CL1                   CURRENT BUFFALO LEVEL                      
HIGHLVL  DS    CL1                   HIGHEST BUFFALO LEVEL                      
LASTLVL  DS    CL1                   LAST LEVEL                                 
LVLDESC  DS    CL15                  LEVEL DESCRIPTION                          
WANT     DS    CL1                   N=INVALID REQUEST                          
ACCCNTL  DS    CL(CTLLNQ)            ENTRY TO CONTROL ACCOUNTS                  
CNTRTAB  DS    20CL(CTLLNQ)          LIST OF CONTRA-ACCOUNT ENTRIES             
BUDKEY   DS    CL42                                                             
BUDIO    DS    1008C                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        LITERAL POOL                                                           
*-------------------------------------------------------------------*           
DRYVTABD DSECT                                                                  
DRYVLVL  DS    AL1                   LEVEL NUMBER, 1=A,2=B,ETC.                 
DRYVBUF  DS    AL1                   LEVEL FOR BUFFALO                          
DRYVPROC DS    AL1                   PROC MODE FOR LEVEL.                       
DRYVLAST DS    AL1                   LAST MODE FOR LEVEL.                       
DRYVAREC DS    AL3                   A(ACCOUNT RECORD FOR LEVEL).               
DRYVTBLQ EQU   *-DRYVTABD                                                       
                                                                                
*        DSECT TO COVER CONTROL ACCOUNT / CONTRA LEVEL                          
                                                                                
CTLD     DSECT                                                                  
CTLUNT   DS    CL1                   - UNIT                                     
CTLLED   DS    CL1                   - LEDGER                                   
CTLBLV   DS    CL1                   - BUDGETTING LEVEL                         
CTLBLN   DS    CL1                   - LENGTH OF BUDGETTING LEVEL               
CTLRLV   DS    CL1                   - OPTIONAL REPORTING LEVEL                 
CTLRLN   DS    CL1                   - LENGTH OF REPORTING LEVEL                
CTLLNQ   EQU   *-CTLD                                                           
         EJECT                                                                  
       ++INCLUDE ACBUDACCD                                                      
         PRINT OFF                                                              
                                                                                
*                                  INCLUDED HERE ARE - ACREPWORKD               
*                                                      ACGENBOTH                
*                                                      ACGENMODES               
*                                                      ACBUDACCD                
*                                                      ACMASTD                  
*                                                      DDBUFFALOD               
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACREPB102S05/01/02'                                      
         END                                                                    
