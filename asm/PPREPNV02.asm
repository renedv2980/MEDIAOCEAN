*          DATA SET PPREPNV02  AT LEVEL 031 AS OF 07/09/14                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 044155.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE PPNV02A                                                                  
*INCLUDE PUBFLOAT                                                               
*INCLUDE PERVERT                                                                
*INCLUDE XSORT                                                                  
*INCLUDE PRNTOFC                                                                
*                                                                               
*        CHANGES                                                                
*                                                                               
*  BPLA  05/11  NEW NV PROFILE (PROGPROF+12) TO SKIP LINES                      
*               BEFORE PRINTING ADDRESS ON THE LETTER                           
*               -ADJUSTING FOR OTHER ENVELOPE WINDOW LOCATIONS                  
*                                                                               
*  SMYE  02/10  CHECK FOR ADBUYER INVOICE ELEMENT IN BUY RECORD                 
*               DON'T SHOW ON LETTER IF BUY IS ATTACHED IN THE                  
*               NEW ADBUYER INVOICE SYSTEM. TREAT THESE BUYS                    
*               AS IF MATCHED UNDER THE OLDER MATCH PROGRAM.                    
*                                                                               
*  SMYE 10/6/07 FIX BUG AT TSTBXC WHERE PROFILE WAS NOT BEING TESTED            
*               BEFORE CHECKING FOR INVOICES IN INVOICE RECORDS                 
*                                                                               
*  SMYE  04/07  IGNORE DELETED INVOICE DETAIL ELEMENTS IN                       
*               ADBUYER INVOICE RECORDS                                         
*                                                                               
*  SMYE  09/06  CHECK FOR INVOICE IN ADBUYER INVOICE RECORDS                    
*               DON'T SHOW ON LETTER IF ONE FOUND COVERING                      
*               THE INSERTION'S DATE (PROFILE CONTROLLED)                       
*                                                                               
*  SMYE 05/04/06 INCREASE PRDTAB TO 1500 PRODUCTS IN NVWORKDF                   
*                                                                               
*  SMYE  11/05  PRINT 2-CHR OFFICE                                              
*                                                                               
*  SMYE 07/27/04 INCREASE PRDTAB TO 1000 PRODUCTS IN NVWORKDF                   
*                                                                               
*  SMYE  06/04  CHANGES INCLUDING ISSUE NAME BY YKAP FROM 04/03                 
*                                                                               
*  BPLA  11/01  CHECK FOR INVOICE IN INV MATCH RECORD                           
*               DON'T SHOW ON LETTER IF ONE FOUND COVERING                      
*               THE INSERTION'S DATE                                            
*                                                                               
*  SMYE  5/00   CHANGES FOR LARGER PPNEWFILE                                    
*                                                                               
*  BPLA  4/99   FIX BUG IN PRNTPUB - ONE PUB PER PAGE PROBLEM                   
*                                                                               
*  BPLA  7/97   CHANGES TO READ CONTROL FILE FAX RECORDS                        
*                                                                               
*                                                                               
*  SMYE  5/97   MOVED OFFOUT CALLS TO CLTFRST - THEY SOMEHOW CAUSED             
*               PPREQREP TO DIE WHEN CALLED FROM PRINTIT                        
*                                                                               
*  BPLA  5/97   NO-OP DISPLAY OF "M" FOR MATCHED INSERTIONS                     
*               (FIND *BPLA IN COL 1)                                           
*                                                                               
*  BPLA  5/97   IF PROGPROF+9 IS "Y" PUT ON LETTER                              
*               IF DATED PAY ELEMENT FOUND- EVEN IF MATCHED                     
*               OTHERWISE NO MATCHED INSERTIONS ON LETTERS                      
*                                                                               
*  SMYE  3/97   ADDED 2-CHARACTER OFFICE PRINTING (OFFOUT) IN PRINTIT           
*                                                                               
*  SMYE  6/96   MODIFIED BLDMLST ROUTINE TO HANDLE 21ST CENTURY YEARS           
*                                                                               
*  SMYE  12/12/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                     
*                                                                               
*    BPLA 9/95      NO NV LETTER FOR MATCHED INSERTIONS                         
*                ALSO AT TSTBX CHECK FOR QOPT7 = 'R' (REPORT ONLY)              
*                REMOVED - SO 'L' WILL PRINT BEFORE INSERTION                   
*                EVEN IF LETTERS ARE NOT BEING PRODUCED TO INDICATE             
*                THAT THIS INSERTION WOULD HAVE BEEN ON A LETTER                
*                BUG FIXED - ONLY DISPLAY "L*" OR "I*" IF LINES                 
*                            OR INCHES ARE PRESENT IN TOTALS                    
*                                                                               
*    BPLA 5/95      CHECK FOR MAXIMUM LETTERS AND EXPAND LETRTAB                
*                   TO 990,000 (4500 LETTERS FROM 4000)                         
*                                                                               
*    BPLA 6/94      CHANGE PPNVLETR TO PPVNVTRN                                 
*                   AS FER REQUEST FROM ANN MARIE                               
*                   IF REQUEST FOR ONE PUB ADD PUB TO EDICT DATA                
*                                                                               
*    BPLA 12/93     ADDITIONAL CHANGES FOR FAXING                               
*                   USE FORCEHED = 'P' INSTEAD OF 'Y'                           
*                                                                               
*    BPLA 10/26/93  BEGIN CHANGES FOR FAXING                                    
*                                                                               
*    BPLA 8/9/93  PRINT PBYOBFD BEFORE COMMENTS                                 
*                                                                               
*    BPLA 7/18/91 ALLOW FOR OFFILE REQUESTS ($N)                                
*                                                                               
*    BPLA 7/29/91 FIX LETTER HEAD ROUTINE  - SETTING SPACING TO                 
*                 4 WHEN PRINT CLIENT CODE CAUSED REPORT TO PRINT IT            
*                 TWICE                                                         
*                                                                               
         TITLE 'PPNV02  NV REPORT'                                              
PPNV02   CSECT                                                                  
         NMOD1 0,PPNV02,RR=R9                                                   
*                                                                               
*   QOPT1      P=PAID ITEMS ONLY                                                
*   QOPT1      U=UNPAID ITEMS ONLY                                              
*   QOPT1 **** WILL ALWAYS BE SET TO U AT FBUYREQ                               
*   QOPT2      Y=ONE PUB PER PAGE                                               
*   QOPT3      C=CASH DISC PUBS ONLY                                            
*              N=NON-CASH DISC PUBS ONLY                                        
*   QOPT4      AD FILE INFO  A,B,1,2                                            
*   QOPT5      Y=FLAG BILLED/TRAFFICKED ITEMS                                   
*   QOPT6      Y=SHOW PAYING ADDR                                               
*   QOPT7      L=SUPPRESS REPORT AND PRINT ONLY NV LETTERS                      
*   QOPT7      R=SUPPRESS LETTERS AND PRINT ONLY REPORT                         
*   QOPT7      F= FAX LETTERS                                                   
*   QOPT7      S= SUPPRESS REPORT AND FAX LETTERS                               
*                                                                               
*   QSORT      05= MAG/SUP/TRADE NAME ORDER                                     
*              07= NEWS/OUTDOOR IN MKT ORDER                                    
*              08= REP ORDER                                                    
*              09= PAYADDR ORDER                                                
*                                                                               
*   QPAY(6)    AS OF DATE (NOT ON REQ SCREEN)                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         PRINT NOGEN                                                            
*                                                                               
         LA    R8,1(RB)                                                         
         LA    R8,4095(R8)                                                      
         USING PPNV02,RB,R8                                                     
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,=V(NVWORK)                                                    
         A     R5,RELO                                                          
         USING NVWRKD,R5                                                        
*                                                                               
         CLI   MODE,FBUYREQ                                                     
         BNE   CKM1                                                             
         MVI   RCWHATPR,0      MUST RESET SINCE IT MAY HAVE LEFT                
*                              AT X'02'                                         
         BAS   R9,INITIAL                                                       
*                              SET FAXOPT FROM QOPT7                            
         MVI   FAXOPT,C'N'                                                      
         CLI   QOPT7,C'R'      REPORT ONLY - NO LETTERS                         
         BE    FREQ5                                                            
         CLI   QOPT7,C'L'      LETTERS ONLY - NOT FAXING                        
         BE    FREQ5                                                            
         CLI   QOPT7,C' '                                                       
         BE    FREQ5                                                            
         CLI   QOPT7,C'F'      REPORT AND FAXING LETTERS                        
         BNE   FREQ2                                                            
         MVI   QOPT7,C' '      ALTER QOPT7 TO BLANK                             
         B     FREQ4                                                            
*                                                                               
FREQ2    CLI   QOPT7,C'S'      LETTERS ONLY - FAXING LETTERS                    
         BNE   FREQ5                                                            
         MVI   QOPT7,C'L'      ALTER QOPT7 TO 'L' (NO REPORT)                   
FREQ4    MVI   FAXOPT,C'Y'                                                      
         MVI   RCWHATPR,X'01'  SET TO X'01' FOR REPORT AND NON-FAXED            
*                              LETTERS                                          
FREQ5    DS    0H                                                               
         MVC   SVFAXOPT,FAXOPT      SAVE "REAL' FAXOPT                          
*                                                                               
         GOTO1 VBLDMLST              BUILD MTH LIST                             
         MVI   QOPT1,C'U'                                                       
         L     RF,=A(LETRTAB)        REBUILD LETTER TABLE                       
         A     RF,RELO                                                          
         ST    RF,ALETRTAB                                                      
         ST    RF,ATABADDR                                                      
         XC    LETCNT,LETCNT                                                    
         XC    SVMEDCLI,SVMEDCLI                                                
         MVI   ONECLI,C'N'                                                      
         B     EXT                                                              
*                                                                               
CKM1     CLI   MODE,LBUYREQ                                                     
         BNE   CKM2                                                             
         GOTO1 VREQEND             GO DO REQUEST TOTALS                         
         GOTO1 VLETRPRT            PRINT LETTERS AT END OF REPORT               
         XC    ATABADDR,ATABADDR                                                
         XC    LETCNT,LETCNT                                                    
         B     EXT                                                              
*                                                                               
CKM2     CLI   MODE,FBUYCLI                                                     
         BNE   CKM3                                                             
         GOTO1 VCLIFRST                                                         
         B     EXT                                                              
*                                                                               
CKM3     CLI   MODE,FBUYPUB                                                     
         BNE   CKM4                                                             
         GOTO1 VPUBFRST                                                         
         B     EXT                                                              
*                                                                               
CKM4     CLI   MODE,PROCBUY                                                     
         BNE   CKM6                                                             
         B     PROCESS                                                          
*                                                                               
CKM6     CLI   MODE,LBUYPUB                                                     
         BNE   CKM8                                                             
         GOTO1 VPUBEND                                                          
         B     EXT                                                              
*                                                                               
CKM8     CLI   MODE,LBUYPRO                                                     
         BNE   CKM9                                                             
         GOTO1 VPRDEND                                                          
         B     EXT                                                              
*                                                                               
CKM9     CLI   MODE,LBUYCLI                                                     
         BNE   CKM11                                                            
         GOTO1 VCLTEND                                                          
         B     EXT                                                              
*                                                                               
CKM11    CLC   QSORT,=C'08'                                                     
         BE    CKM11A                                                           
         CLC   QSORT,=C'09'                                                     
         BNE   CKM12                                                            
CKM11A   CLI   MODE,FBUYREP                                                     
         BNE   CKM11C                                                           
         MVI   REPACT,0                                                         
         B     EXT                                                              
*                                                                               
CKM11C   CLI   MODE,LBUYREP                                                     
         BNE   CKM12                                                            
         CLI   REPACT,C'Y'                                                      
         BNE   EXT                                                              
         GOTO1 VREPEND                                                          
         B     EXT                                                              
*                                                                               
CKM12    CLI   MODE,FBUYPRO                                                     
         BNE   CKM14                                                            
         CLI   PRDSW,1             SEE IF DOING PRDS SEPARATELY                 
         BNE   EXT                                                              
         MVI   FORCEHED,C'P'     NEW PAGE AND FORCE PAGE                        
         B     EXT                                                              
*                                                                               
CKM14    B     EXT                                                              
         EJECT                                                                  
PROCESS  EQU   *                                                                
         OC    KEY+21(3),KEY+21       IGNORE PASSIVE POINTERS                   
         BNZ   EXT                                                              
*                                                                               
         CLI   PROGPROF+11,C'Y'    SUPPRESS IF MATCH OR ADBUYER INVOICE         
         BNE   PROC10              HAS BEEN ENTERED WHOSE                       
*                                  PERIOD COVERS THIS BUY ?                     
         BAS   RE,CKINV                                                         
         CLI   P+3,C'I'            INVOICE FOUND?                               
         BNE   PROC02                                                           
         MVI   P+3,C' '                                                         
         B     EXT                 SKIP THIS BUY                                
*                                                                               
PROC02   DS    0H                  NEW ADBUYER INVOICE CHECKING                 
*                                                                               
         BAS   RE,CKANV                                                         
         CLI   P+3,C'I'            ADBUYER INVOICE FOUND?                       
         BNE   PROC10                                                           
         MVI   P+3,C' '                                                         
         B     EXT                 SKIP THIS BUY                                
*                                                                               
PROC10   CLI   PUBSW,0             SEE IF DOING THIS PUB                        
         BNE   EXT                 NO                                           
PROC12   CLC   LASTYM,PBUYKDAT     CHECK FOR CHANGE OF MONTH                    
         BE    *+10                                                             
         GOTO1 VMTHEND                                                          
         MVC   LASTYM,PBUYKDAT                                                  
*                                                                               
*                                                                               
CKQOPT1  CLI   ASOFDTE,0                                                        
         BE    CKQOPT1X         NOT USING AS OF DATE                            
         XC    PGROSS(16),PGROSS                                                
         LA    R3,PBDELEM                                                       
CKQOPT1A CLI   0(R3),X'25'                                                      
         BNE   NEXTEL                                                           
         USING PPDUMD03,R3                                                      
         OC    PPDDATE,PPDDATE                                                  
         BZ    NEXTEL                                                           
         CLC   PPDDATE(3),ASOFDTE                                               
         BH    NEXTEL                                                           
         ST    R8,FULL        SAVE 2ND BASE REG                                 
**                                                                              
         LM    R7,R9,PGROSS                                                     
         A     R7,PPGROSS                                                       
         A     R8,PPAGYCOM                                                      
         A     R9,PPCSHDSC                                                      
         STM   R7,R9,PGROSS                                                     
         LM    R7,R9,PPGROSS                                                    
         SR    R7,R8                                                            
         SR    R7,R9                                                            
         L     R6,PAID                                                          
         AR    R6,R7                                                            
         ST    R6,PAID                                                          
*                                                                               
         L     R8,FULL         RESTORE 2ND BASE REG                             
*                                                                               
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BNE   CKQOPT1A                                                         
         B     CKQOPT1X         END OF RECORD                                   
         DROP  R3                                                               
*                                                                               
CKQOPT1X CLI   PBUYCNTL,X'80'        SEE IF DELETED                             
         BNE   CKPD                                                             
         OC    PGROSS(12),PGROSS                                                
         BZ    NEXTBUY              NOT PAID SO IGNORE                          
         XC    GROSS(20),GROSS                                                  
CKPD     CLI   QOPT1,C'P'                                                       
         BNE   CKUNPD                                                           
         OC    PGROSS(12),PGROSS                                                
         BZ    NEXTBUY                                                          
         B     BUYOK                                                            
*                                                                               
CKUNPD   CLI   QOPT1,C'U'                                                       
         BNE   BUYOK        DOING ALL ITEMS                                     
         CLC   GROSS(12),PGROSS                                                 
         BE    NEXTBUY                                                          
*                                                                               
BUYOK    DS    0H                                                               
         CLC   SAVEPUB,PBUYKPUB                                                 
         BE    BUYOK5                                                           
         GOTO1 VPRNTPUB                                                         
BUYOK5   CLC   SAVEPRD,PBUYKPRD                                                 
         BE    *+8                                                              
         BAS   RE,PROFRST                                                       
*                                                                               
BUYOK10  MVC   MTHACT(5),=5C'Y'    SET ACTIVITY SWITCHES                        
*                                                                               
         MVI   REPACT,C'Y'                                                      
*                                                                               
         LA    R2,BUYOUTA                                                       
         USING PPBYOUTD,R2                                                      
         GOTO1 PPBYOUT,DMCB,BUYOUTA                                             
         MVC   P+5(8),PBYOINS                                                   
         CLI   PBYOINS2,C' '                                                    
         BE    PPBY00                                                           
         MVI   PSECOND+4,C'+'                                                   
         MVC   PSECOND+5(8),PBYOINS2                                            
         B     PPBYX                                                            
PPBY00   DS    0H                                                               
*                                                                               
         CLC   PBYOISNM,SPACES                                                  
         BE    PPBYX                                                            
         MVC   PSECOND+5(11),PBYOISNM                                           
         B     PPBYX                                                            
PPBYX    DS    0H                                                               
         CLI   PBUYCNTL,X'80'           SEE IF DELETED                          
         BNE   PRTN1A                                                           
         MVI   P+13,C'D'                                                        
         MVC   P+53(15),=C'CLEARED+DELETED'                                     
         B     PRTN2                                                            
*****                                                                           
PRTN1A   ST    R8,FULL          SAVE 2ND BASE REG                               
*                                                                               
         LM    R6,R9,GROSS                                                      
         STM   R6,R9,BUYGO                                                      
         L     R8,FULL          RESTORE 2ND BASE REG                            
*                                                                               
         SR    R6,R7                                                            
         ST    R6,BUYGLAC                                                       
         CLI   PROGPROF+2,C'0'                                                  
         BE    PRTN2                                                            
         CLI   PROGPROF+2,C'G'                                                  
         BE    PRTN1                                                            
         CLI   PROGPROF+2,C'C'                                                  
         BNE   BUYOK20                                                          
         L     R3,BUYCD                                                         
         B     BUYOK50                                                          
BUYOK20  CLI   PROGPROF+2,C'1'                                                  
         BNE   BUYOK30                                                          
         L     R3,BUYGO                                                         
         S     R3,BUYCD                                                         
         B     BUYOK50                                                          
BUYOK30  CLI   PROGPROF+2,C'2'                                                  
         BNE   BUYOK40                                                          
         L     R3,BUYNP                                                         
         B     BUYOK50                                                          
BUYOK40  L     R3,BUYGLAC        =NET                                           
BUYOK50  EDIT  (R3),(14,P+53),2,COMMAS=YES,MINUS=YES                            
         B     PRTN2                                                            
*****                                                                           
PRTN1    MVC   P+53(14),PBYOGRS                                                 
PRTN2    DS    0H                                                               
         CLI   QMEDIA,C'N'                                                      
         BNE   PRTMAG                                                           
         MVC   P+24(8),PBYOUR                                                   
         CLI   PBYOSPC,C' '                                                     
         BE    PRTN5                                                            
         MVC   P+33(10),PBYOSPC                                                 
         MVC   PSECOND+33(11),PBYOPRM                                           
         B     PRTN10                                                           
*                                                                               
PRTN5    MVC   P+33(11),PBYOPRM                                                 
*                                                                               
PRTN10   MVC   P+45(7),PBYOUNTS                                                 
         LA    R3,5                                                             
         LA    R4,P+45                                                          
PRTN20   CLI   P+51,C' '                                                        
         BNE   PRTEST                                                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),PBYOUNTS                                                 
         MVI   0(R4),C' '                                                       
         LA    R4,1(R4)                                                         
         BCT   R3,PRTN20                                                        
         B     PRTEST                                                           
*                                                                               
PRTMAG   DS    0H                                                               
         MVC   P+25(20),PBYOSPC                                                 
         MVC   PSECOND+25(20),PBYOSPC2                                          
*                                                                               
PRTEST   BAS   RE,TSTBLTR                                                       
         SR    R0,R0                                                            
         IC    R0,PBUYKEST                                                      
         SLL   R0,8                                                             
         IC    R0,PBUYKEST+1                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+20(3),DUB+6(2)                                                 
*        GOTO1 DTCNV,DMCB,(1,PBDPDATE),(4,P+14)                                 
         GOTO1 DATCON,DMCB,(3,PBDPDATE),(7,P+14)                                
         ST    R8,FULL         SAVE 2ND BASE REG                                
*                                                                               
         LM    R6,R9,GROSS                                                      
         STM   R6,R9,BUYGO                                                      
*                                                                               
         L     R8,FULL        RESTORE 2ND BASE REG                              
*                                                                               
         SR    R6,R7                                                            
         ST    R6,BUYGLAC                                                       
*****                                                                           
         CLI   PROGPROF+3,C'0'                                                  
         BE    PRTE3                                                            
         CLI   PROGPROF+3,C'G'                                                  
         BNE   PRTE2A                                                           
         L     R3,BUYGO                                                         
         B     PRTE2                                                            
PRTE2A   CLI   PROGPROF+3,C'C'                                                  
         BNE   PRTE2B                                                           
         L     R3,BUYCD                                                         
         B     PRTE2                                                            
PRTE2B   CLI   PROGPROF+3,C'1'                                                  
         BNE   PRTE2C                                                           
         L     R3,BUYGO                                                         
         S     R3,BUYCD                                                         
         B     PRTE2                                                            
PRTE2C   CLI   PROGPROF+3,C'2'                                                  
         BNE   PRTE2D                                                           
         L     R3,BUYNP                                                         
         S     R4,BUYCD                                                         
         B     PRTE2                                                            
PRTE2D   L     R3,BUYGLAC         =NET                                          
*****                                                                           
PRTE2    EDIT  (R3),(14,P+68),2,COMMAS=YES,MINUS=YES                            
PRTE3    CLC   PBUYKPRD,=C'ZZZ'                                                 
         BNE   PRTE20                                                           
         CLI   PBYOZZZ,C' '                                                     
         BE    PRTE20                                                           
         CLC   PSECOND+25(20),SPACES        SEE IF PSECOND IS USED              
         BNE   PRTE10              YES                                          
         MVC   PSECOND+25(50),PBYOZZZ                                           
PRTE5    GOTO1 VPRINTIT                                                         
         B     PRTEX                                                            
*                                                                               
PRTE10   GOTO1 VPRINTIT                                                         
         MVC   P+25(50),PBYOZZZ                                                 
PRTE15   GOTO1 VPRINTIT                                                         
         B     PRTEX                                                            
*                                                                               
PRTE20   GOTO1 VPRINTIT                                                         
PRTEX    OC    PGROSS(12),PGROSS                                                
         BZ    LTSTIO                   ADD TO MTH TOTALS                       
         CLI   QOPT1,C'P'                                                       
         BNE   PRTPPD                                                           
         CLC   GROSS(12),PGROSS                                                 
         BE    LTSTIO              TOTALLY PAID SO ADD TO MTH ACCUMS            
         MVC   P+35(17),=C'UNCLEARED PORTION'                                   
*                                                                               
*              SUBTRACT PGROSS FROM GROSS AND PUT IN PGROSS SO THAT             
*              PGROSS,PAGYCOM,PCSHDSC,PAID WILL REFLECT UNPAID AMTS             
*                                                                               
         L     R3,GROSS                                                         
         L     R4,PGROSS                                                        
         SR    R3,R4                                                            
         ST    R3,PGROSS                                                        
         L     R3,AGYCOM                                                        
         L     R4,PAGYCOM                                                       
         SR    R3,R4                                                            
         ST    R3,PAGYCOM                                                       
         L     R3,CSHDSC                                                        
         L     R4,PCSHDSC                                                       
         SR    R3,R4                                                            
         ST    R3,PCSHDSC                                                       
         L     R3,PYABLE                                                        
         L     R4,PAID                                                          
         SR    R3,R4                                                            
         ST    R3,PAID                                                          
         B     PRTPPD1                                                          
PRTPPD   CLI   QOPT1,C'U'                                                       
         BNE   LTSTIO       IF DOING ALL ITEMS - NO ADJUSTMENT                  
         MVC   P+35(18),=C'PREVIOUSLY CLEARED'                                  
PRTPPD1  DS    0H                                                               
*****                                                                           
         CLI   PROGPROF+2,C'0'                                                  
         BE    PPDM                                                             
         CLI   PROGPROF+2,C'N'                                                  
         BNE   PPD1                                                             
         L     R3,PGROSS                 =NET                                   
         S     R3,PAGYCOM                                                       
         B     PPDI                                                             
*                                                                               
PPD1     CLI   PROGPROF+2,C'C'                                                  
         BNE   PPD2                                                             
         L     R3,PCSHDSC                                                       
         B     PPDI                                                             
PPD2     CLI   PROGPROF+2,C'1'                                                  
         BNE   PPD3                                                             
         L     R3,PGROSS                                                        
         S     R3,PCSHDSC                                                       
         B     PPDI                                                             
PPD3     CLI   PROGPROF+2,C'2'                                                  
         BNE   PPD4                                                             
         L     R3,PAID                                                          
         B     PPDI                                                             
PPD4     L     R3,PGROSS           PROGPROF+2=G                                 
*****                                                                           
PPDI     EDIT  (R3),(14,P+53),2,COMMAS=YES,MINUS=YES,FLOAT=$                    
         MVI   0(R1),C'('                                                       
         MVC   P+53(14),WORK+3                                                  
         MVI   P+66,C')'                                                        
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   P+66(2),=C'-)'                                                   
PPDM     DS    0H                                                               
*****                                                                           
         CLI   PROGPROF+3,C'0'                                                  
         BE    PPDT                                                             
         CLI   PROGPROF+3,C'G'                                                  
         BNE   PPD6                                                             
         L     R3,PGROSS                                                        
         B     PPDS                                                             
PPD6     CLI   PROGPROF+3,C'C'                                                  
         BNE   PPD7                                                             
         L     R3,PCSHDSC                                                       
         B     PPDS                                                             
PPD7     CLI   PROGPROF+3,C'1'                                                  
         BNE   PPD8                                                             
         L     R3,PGROSS                                                        
         S     R3,PCSHDSC                                                       
         B     PPDS                                                             
PPD8     CLI   PROGPROF+3,C'2'                                                  
         BNE   PPD9                                                             
         L     R3,PAID                  PAID= NET-CD                            
         B     PPDS                                                             
PPD9     L     R3,PGROSS          PROGPROF+2 MUST = N                           
         S     R3,PAGYCOM                                                       
*****                                                                           
PPDS     EDIT  (R3),(14,P+68),2,COMMAS=YES,MINUS=YES,FLOAT=$                    
         MVI   0(R1),C'('                                                       
         MVC   P+68(14),WORK+3                                                  
         MVI   P+81,C')'                                                        
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   P+81(2),=C'-)'                                                   
PPDT     GOTO1 VPRINTIT                                                         
         L     R3,BUYGO                                                         
         S     R3,PGROSS                                                        
         ST    R3,BUYGO                                                         
         L     R3,BUYGLAC                                                       
         L     R4,PGROSS                                                        
         S     R4,PAGYCOM               R4 NOW NET PAID                         
         SR    R3,R4                                                            
         ST    R3,BUYGLAC                                                       
         L     R3,BUYCD                                                         
         S     R3,PCSHDSC                                                       
         ST    R3,BUYCD                                                         
         L     R3,BUYNP                                                         
         S     R3,PAID                                                          
         ST    R3,BUYNP                                                         
*                                                                               
LTSTIO   DS    0H                                                               
         XC    SV70ELM,SV70ELM                                                  
         CLI   PROGPROF+1,C'N'      NO I/O'S                                    
         BE    RLMTH                                                            
         MVI   SV70ELM,X'01'        SO I'LL KNOW I WAS DOING I/O'S              
         LA    R6,PBUYREC+33                                                    
         MVI   ELCOD,X'70'                                                      
LTST5    BAS   RE,LNEXTEL                                                       
         BNE   LTST15                                                           
         OC    2(3,R6),2(R6)                                                    
         BZ    LTST5                                                            
         CLC   SV70ELM+2(3),2(R6)                                               
         BNL   LTST5                                                            
         MVC   SV70ELM(11),0(R6)                                                
         B     LTST5                                                            
*                                                                               
*                                                                               
*              SAVE FIRST 11 BYTES OF 70 ELEM                                   
SV70ELM  DS    CL11                                                             
*                                                                               
LNEXTEL  DS    0H       GET  NEXT ELEMENT                                       
         CLI   0(R6),0                                                          
         BE    LNEXTELX                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCOD                                                    
         BER   RE                                                               
         B     LNEXTEL                                                          
LNEXTELX LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
LTST15   CLI   SV70ELM,X'01'          MEANS I DIDN'T FIND ANY                   
         BE    RLMTH                                                            
         CLI   PROGPROF+1,C'L'        ONLY ON LETTERS                           
         BE    RLMTH                                                            
*        GOTO1 DTCNV,DMCB,(1,SV70ELM+2),(0,P+35)                                
         GOTO1 DATCON,DMCB,(3,SV70ELM+2),(0,P+35)                               
         MVC   P+25(9),=C'LAST I/O='                                            
         MVC   P+34(1),PBUYREC+2                                                
         MVI   P+35,C'-'                                                        
         MVI   P+41,C'-'                                                        
         MVC   HALF,SV70ELM+5                                                   
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+42(4),DUB                                                      
*                                                                               
         CLI   SV70ELM+10,C'N'                                                  
         BNE   LTST20                                                           
         MVC   P+47(3),=C'NEW'                                                  
         B     LTST30                                                           
*                                                                               
LTST20   CLI   SV70ELM+10,C'C'                                                  
         BNE   LTST25                                                           
         MVC   P+47(3),=C'CHA'                                                  
         B     LTST30                                                           
*                                                                               
LTST25   CLI   SV70ELM+10,C'D'                                                  
         BNE   LTST30                                                           
         MVC   P+47(3),=C'CAN'                                                  
*                                                                               
*LTST30   GOTO1 DTCNV,DMCB,(1,SV70ELM+2),(4,P+51)                               
LTST30   GOTO1 DATCON,DMCB,(3,SV70ELM+2),(7,P+51)                               
         GOTO1 VPRINTIT                                                         
*                                                                               
RLMTH    DS    0H          PRINT BEST FOOD DAY/WEEK OF MESSAGE                  
         CLC   PBYOBFD(L'PBYOBFD),SPACES                                        
         BE    RL0                                                              
         MVC   P+25(L'PBYOBFD),PBYOBFD                                          
         GOTO1 VPRINTIT                                                         
*                                                                               
RL0      DS    0H                      PRINT COMMENTS                           
         CLI   PROGPROF+6,C'Y'         PROFILE BYTE TO SUPRESS PRINTING         
         BE    RL2X                    OF COMMENTS                              
*                                                                               
         LA    R3,PBYOCOMS                                                      
         LA    R4,5                                                             
RL1      CLI   0(R3),C' '                                                       
         BE    RL2X                                                             
         MVC   P+25(47),0(R3)                                                   
         GOTO1 VPRINTIT                                                         
         LA    R3,47(R3)                                                        
         BCT   R4,RL1                                                           
*                                                                               
         DROP  R2                                                               
*                                                                               
RL2X     DS    0H                                                               
         LA    R2,PPFILED+4095                                                  
         LA    R2,1(R2)                                                         
         USING PPFILED+4096,R2                                                  
         OC    PBDJOB,PBDJOB                                                    
         BZ    RLM2                                                             
         CLI   QOPT4,C' '                                                       
         BE    RLM2                                                             
         CLI   QOPT4,C'A'         AD NO ONLY                                    
         BE    RLM1A          NO NEED TO READ JOB REC                           
*                                                                               
*                                                                               
RLM1     DS    0H                                                               
         TM    QOPT4,X'30'                                                      
         BZ    RLM1A                                                            
         MVC   P+25(6),=C'COPY ='                                               
         MVC   P+25+7(17),PJOBCPY                                               
         B     RLM1B                                                            
*                                                                               
RLM1A    DS    0H                                                               
         MVC   P+25(8),=C'AD NO. ='                                             
         MVC   P+25+9(6),PBDJOB                                                 
*                                                                               
RLM1B    GOTO1 VPRINTIT                                                         
         TM    QOPT4,X'02'                                                      
         BZ    RLM2                                                             
         MVC   P+25(25),PJOBCAP1                                                
         MVC   PSECOND+25(25),PJOBCAP2                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
         GOTO1 VPRINTIT                                                         
*                                                                               
RLM2     CLI   PBDSPACE,C'*'      SEE IF REAL INSERTION                         
         BE    RLMTH1              NO - BYPASS INS TOTALS                       
         L     R3,MTHINS                                                        
         AH    R3,=H'1'                                                         
         ST    R3,MTHINS                                                        
RLMTH1   CLI   QMEDIA,C'N'         IF MAG                                       
         BNE   RLMTH2              BYPASS LINES                                 
         L     R3,MTHLINES                                                      
RLM2A    CLI   PROGPROF,C'I'                                                    
         BNE   RLM2C                                                            
         CLI   PBDUIND,X'89'                                                    
         BE    RLMTH1B                                                          
         CLI   PBDUIND,C'I'                                                     
         BE    RLM2B                                                            
         L     R6,UNITS                                                         
         CVD   R6,DUB                                                           
         MP    DUB,=P'1000'                                                     
         DP    DUB,=P'14'                                                       
         SRP   DUB(6),63,5                                                      
         ZAP   DUB,DUB(6)                                                       
         CVB   R6,DUB                                                           
         ST    R6,UNITS                                                         
         B     RLMTH1B                                                          
RLM2B    L     R6,UNITS                                                         
         CVD   R6,DUB                                                           
         MP    DUB,=P'100'                                                      
         CVB   R6,DUB                                                           
         ST    R6,UNITS                                                         
         B     RLMTH1B                                                          
RLM2C    CLI   PBDUIND,C'I'        CONVERT INCHES TO LINES FOR ACCUMS           
         BE    RLMTH1A                                                          
         CLI   PBDUIND,X'89'       CONVERT INCHES TO LINES FOR ACCUMS           
         BNE   RLMTH1B             LOWER CASE I INCHES TO 2 DECIMALS            
RLMTH1A  SR    R6,R6                                                            
         L     R7,UNITS                                                         
         M     R6,=F'14'                                                        
         ST    R7,UNITS                                                         
         CLI   PBDUIND,X'89'       SEE IF LOWER CASE I                          
         BNE   RLMTH1B             NO                                           
         CVD   R7,DUB                                                           
         AP    DUB,=P'50'          MUST ROUND TO NEAREST LINE                   
         DP    DUB,=P'100'                                                      
         ZAP   DUB,DUB(6)                                                       
         CVB   R7,DUB                                                           
         ST    R7,UNITS                                                         
*                                                                               
RLMTH1B  EQU   *                                                                
         A     R3,UNITS                                                         
         ST    R3,MTHLINES                                                      
RLMTH2   L     R3,MTHGO                                                         
         A     R3,BUYGO                                                         
         ST    R3,MTHGO                                                         
         L     R3,MTHGLAC                                                       
         A     R3,BUYGLAC                                                       
         ST    R3,MTHGLAC                                                       
         L     R3,MTHCD                                                         
         A     R3,BUYCD                                                         
         ST    R3,MTHCD                                                         
         L     R3,MTHNP                                                         
         A     R3,BUYNP                                                         
         ST    R3,MTHNP                                                         
         MVC   SAVEYMD,BLBLDT                                                   
         CLI   QBPDATE,C'B'                                                     
         BE    LETTER                                                           
         MVC   SAVEYMD,PBUYKDAT                                                 
         B     LETTER                                                           
         EJECT                                                                  
*                                                                               
LETTER   DS    0H                                                               
         CLI   QOPT7,C'R'          SEE IF ONLY DOING REPORT                     
         BE    EXT                 NO POSTING TO LETRTAB                        
         CLI   LETSW,0                                                          
         BE    EXT                                                              
         CLI   NOLETSW,C'Y'    WILL BE 'Y' IF NO NVTEXT RECORD                  
         BE    EXT             FOUND AT CLTFRST                                 
*                                                                               
         LA    R4,1(RC)                                                         
         LA    R4,4095(R4)                                                      
         USING PPFILED+4096,R4                                                  
*                                                                               
*                                                                               
LET1P    L     R3,ATABADDR               GET SAVED ADDRESS INTO TABLE           
*                                                                               
         USING TABDSC,R3                                                        
         XC    0(TBLLEN,R3),0(R3)        CLEAR SPACE                            
         CLI   PREPNAME,C' '             IF NO REPNAME USE PUBNAME              
         BH    LET10                     AND ADDRESS                            
*                                                                               
         CLI   FAXOPT,C'Y'                                                      
         BNE   LET1P5                                                           
*                                                                               
         LA    R6,PUBREC+33                                                     
         MVI   ELCOD,X'11'                                                      
         BAS   RE,LNEXTEL                                                       
         BNE   LET1P2                                                           
         USING PUBSADEL,R6                                                      
         MVC   TABFAX,PUBSFAXN                                                  
*                                                                               
         DROP  R6                                                               
*                                                                               
LET1P2   DS    0H                                                               
*                                                                               
LET1P5   MVC   TABNAME(20),PUBNAME                                              
         MVC   TABAD1,PUBLINE1                                                  
         MVC   TABAD2,PUBLINE2                                                  
         MVC   TABATTN(14),=C'CREDIT MANAGER'                                   
         B     LET20                                                            
*                                                                               
LET10    DS    0H                                                               
         CLI   FAXOPT,C'Y'          SEE IF FAXING LETTERS                       
         BNE   *+10                                                             
         MVC   TABFAX,PREPFAX                                                   
*                                                                               
         MVC   TABNAME,PREPNAME                                                 
         MVC   TABAD1,PREPLIN1                                                  
         MVC   TABAD2,PREPLIN2                                                  
         CLC   PREPATTN(20),=20C' '                                             
         BH    LET15                                                            
         MVC   TABATTN(14),=C'CREDIT MANAGER'                                   
         B     LET20                                                            
LET15    MVC   TABATTN,PREPATTN                                                 
*                                                                               
LET20    DS    0H                                                               
         MVC   TABCLI,PBUYKCLT                                                  
         MVC   TABMED,PBUYKMED                                                  
         MVC   TABPRD,PBUYKPRD                                                  
         MVC   TABPUB,PBUYKPUB                                                  
         MVC   TABEST,PBUYKEST                                                  
         MVC   TABINS,PBUYKDAT                                                  
         CLI   PBDFREQ,C'M'            SEE IF MONTHLY                           
         BNE   *+8                                                              
         MVI   TABINS+2,0               ZERO INSERTION DAY                      
         MVC   TABSUBL,PBUYKLIN         SUB LINE NUMBER                         
         OC    TABNAME,SPACES                                                   
         OC    TABAD1,SPACES                                                    
         OC    TABAD2,SPACES                                                    
         OC    TABATTN,SPACES                                                   
         CLI   PROGPROF+1,C'R'           SEE IF LAST I/O ONLY ON REPORT         
         BE    *+10                                                             
         MVC   TABLIO,SV70ELM            SAVE LAST I/O ELEM                     
         MVC   TABPUBN,PUBNAME                                                  
         MVC   TABZONN,PUBZNAME                                                 
*                                                                               
LET30    DS    0H                        MOVE IN CORRECT SPACE DESCR            
         LA    R2,BUYOUTA                                                       
         USING PPBYOUTD,R2                                                      
         CLI   QMEDIA,C'N'                                                      
         BNE   LET40                                                            
         CLI   PBYOSPC,C' '                                                     
         BH    LET40                                                            
         MVC   TABSPC(7),PBYOUNTS                                               
         B     LET50                                                            
LET40    MVC   TABSPC(20),PBYOSPC                                               
*                                                                               
LET50    CLI   PROGPROF+10,C'X'        NO $                                     
         BE    BUMPTAB                                                          
         L     R0,GROSS                                                         
         L     R1,PGROSS                                                        
         CLI   PROGPROF+10,C'G'        GROSS                                    
         BE    LET55                                                            
         S     R0,AGYCOM                                                        
         S     R1,PAGYCOM                                                       
         CLI   PROGPROF+10,C'N'        NET                                      
         BE    LET55                                                            
         S     R0,CSHDSC                                                        
         S     R1,PCSHDSC                                                       
         CLI   PROGPROF+10,C'2'        NET-CD                                   
         BE    LET55                                                            
         L     R0,GROSS                                                         
         L     R1,PGROSS                                                        
         S     R0,CSHDSC                                                        
         S     R1,PCSHDSC                                                       
         CLI   PROGPROF+10,C'1'        GROSS-CD                                 
         BE    LET55                                                            
         DC    H'0'                    INVALID PROFILE                          
*                                                                               
LET55    ST    R0,TABORD          STORE AMOUNTS IN LETTER TABLE                 
         ST    R1,TABPAID                                                       
*                                                                               
*                                                                               
BUMPTAB  LA    R3,TBLLEN(R3)                  BUMP TABLE AND                    
*                                                                               
         CLC   LETCNT,MAXLET                                                    
         BL    BUMPT5                                                           
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   P(35),=C'*** MAXIMUM INSERTIONS EXCEEDED ***'                    
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 REPORT                                                           
         MVC   P(23),=C'*** REQUEST STOPPED ***'                                
         GOTO1 REPORT                                                           
         MVI   MODE,LBUYREQ                  GO TO NEXT REQ                     
         B     EXT                                                              
*                                                                               
BUMPT5   DS    0H                                                               
         XC    0(TBLLEN,R3),0(R3)             CLEAR                             
         ST    R3,ATABADDR                    STORE CURRENT ADDRESS             
         L     RE,LETCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,LETCNT                                                        
*                                                                               
NEXTBUY  DS    0H                                                               
         B     EXT                                                              
         EJECT                                                                  
         DROP  R2                                                               
         DROP  R3                                                               
         DROP  R4                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
INITIAL  XC    MTHTAB,MTHTAB            LINKED VIA R9.                          
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         L     RF,=V(MTHEND)           RELOCATE ROUTINES                        
         A     RF,RELO                                                          
         ST    RF,VMTHEND                                                       
         L     RF,=V(PRDEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VPRDEND                                                       
         L     RF,=V(PUBEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VPUBEND                                                       
         L     RF,=V(REQEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VREQEND                                                       
         L     RF,=V(CLTEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VCLTEND                                                       
         L     RF,=V(PRINTIT)                                                   
         A     RF,RELO                                                          
         ST    RF,VPRINTIT                                                      
         L     RF,=V(BLDMLST)                                                   
         A     RF,RELO                                                          
         ST    RF,VBLDMLST                                                      
         L     RF,=V(NVWORK)                                                    
         A     RF,RELO                                                          
         ST    RF,VNVWORK                                                       
         L     RF,=V(CLTFRST)                                                   
         A     RF,RELO                                                          
         ST    RF,VCLIFRST                                                      
         L     RF,=V(PUBFIRST)                                                  
         A     RF,RELO                                                          
         ST    RF,VPUBFRST                                                      
         L     RF,=V(PRDTAB)                                                    
         A     RF,RELO                                                          
         ST    RF,APRDTAB                                                       
         L     RF,=V(REPEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VREPEND                                                       
         L     RF,=V(PRNTPUB)                                                   
         A     RF,RELO                                                          
         ST    RF,VPRNTPUB                                                      
         L     RF,=V(PUBFLOAT)                                                  
         A     RF,RELO                                                          
         ST    RF,VPUBFLOT                                                      
*                                                                               
         L     RF,=A(LETRPRT)                                                   
         A     RF,RELO                                                          
         ST    RF,VLETRPRT                                                      
         L     RF,=V(PERVERT)                                                   
         A     RF,RELO                                                          
         ST    RF,VPERVERT                                                      
         L     RF,=V(XSORT)                                                     
         A     RF,RELO                                                          
         ST    RF,VXSORT                                                        
         L     RF,=A(FREPORT)                                                   
         A     RF,RELO                                                          
         ST    RF,AFREPORT                                                      
         L     RF,=V(PRNTOFC)                                                   
         A     RF,RELO                                                          
         ST    RF,VPRNTOFC                                                      
*                                                                               
         XC    DMCB(4),DMCB        NEED ADDRESS OF OFFICER                      
         MVC   DMCB+4(4),=X'D9000A38'                                           
         L     RF,VCOMFACS                                                      
         L     RF,(CCALLOV-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB                                                        
         MVC   VOFFICER,DMCB                                                    
*                                                                               
         LA    R6,BUYOUTA          INITIALIZE PPBYOUT PARAMETER LIST            
         USING PPBYOUTD,R6                                                      
         XC    PBYOINPT(24),PBYOINPT                                            
         LA    R2,PBUYREC                                                       
         L     R3,DATCON                                                        
         LA    R4,GROSS                                                         
         STM   R2,R4,0(R6)                                                      
         MVI   PBYOCTL,X'28'                                                    
*                                                                               
         DROP  R6                                                               
*                                                                               
         MVI   FCRDBUY,C'Y'        RESET TO READ 20 POINTERS                    
         MVI   FCRDACTV,C'Y'       SET TO READ ACTIVE ONLY                      
         MVI   FCGTJOB,C'N'        RESET JOB RECORD READ                        
         MVI   PRDSW,1                                                          
         CLC   QPRODUCT(3),=C'   '                                              
         BNE   *+12                                                             
         MVI   FCRDBUY,X'21'       USE 21 POINTERS                              
         MVI   PRDSW,0             SET DOING PRDS SEPARATELY                    
         JIF   QOPT4,EQ,C' ',OR,QOPT4,EQ,C'A',INTI1,JUMP=N                      
         MVI   FCGTJOB,C'Y'        SO PPG WILL GET JOBRECS                      
*                                                                               
INTI1    DS    0H                                                               
         MVI   FCGTREP,C'N'                                                     
         CLI   QOPT6,C'Y'          SEE IF SHOWING PAYING ADDR                   
         BE    INIT2                                                            
         CLC   QSORT,=C'08'        OR SORTING BY REP                            
         BE    INIT2                                                            
         CLC   QSORT,=C'09'        OR SORTING BY PAY ADDR NAME                  
         BE    INIT2                                                            
         CLI   QOPT7,C'R'          IF DOING REPORT ONLY                         
         BE    INIT3               DON'T NEED TO GET PAYING ADDR                
INIT2    MVI   FCGTREP,C'Y'        SO PPG WILL GET PAYING ADDR                  
INIT3    MVC   FCPDFILT,QOPT1      P=PAID,U=UNPAID                              
         CLI   QOPT1,C' '                                                       
         BNE   *+8                                                              
INTI4    MVI   FCPDFILT,C'N'       RESET TO N                                   
         MVC   PAGE,=H'1'                                                       
         MVI   REQERR,0                                                         
         CLC   SVMEDCLI,QMEDIA     CHK FOR SAME MEDIA/CLIENT                    
         BER   R9                                                               
FRCLI    DS    0H                  FIRST REQUEST FOR CLIENT                     
         L     R6,APRDTAB                                                       
         CLC   QCLIENT,=C'ALL'                                                  
         BE    FRCLI1                                                           
         CLI   QCLIENT,C'*'                                                     
         BE    FRCLI1                                                           
         CLI   QCLIENT,C'$'           OFFICE LIST                               
         BE    FRCLI1                                                           
         CLI   QCLIENT,C'&&'          GROUP                                     
         BE    FRCLI1                                                           
         CLC   QPRODUCT,=C'ALL'        ONLY BUILD TABLE FOR ALL PRDS            
         BE    FRCLI1C                                                          
         CLC   QPRODUCT,=C'   '                                                 
         BE    FRCLI1C                                                          
FRCLI1   MVC   0(3,R6),=X'FFFFFF'                                               
         XC    SVMEDCLI,SVMEDCLI                                                
         B     FRCLIX              PRD TABLE WILL BE BUILT AT CLTFRST           
*                                  IF NECESSARY                                 
*                                                                               
FRCLI1C  DS    0H                                                               
         MVC   PPGKEY,KEY          SAVE PPG'S KEY                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),QCLIENT                                                 
         LA    R4,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     FRCLI5                                                           
*                                                                               
FRCLI2   LA    R4,DMRSEQ                                                        
FRCLI5   BAS   RE,PDIRRD                                                        
         CLC   KEY(7),KEYSAVE                                                   
         BNE   FRCLI20                                                          
         TM    KEY+25,X'80'                                                     
         BO    FRCLI2              BYPASS DELETED PRDS                          
         LA    R4,GETREC                                                        
         LA    R3,PPRDREC                                                       
         BAS   RE,PFILERD                                                       
         MVC   0(3,R6),PPRDKPRD                                                 
         MVC   3(20,R6),PPRDNAME                                                
         LA    R6,23(R6)                                                        
         B     FRCLI2                                                           
*                                                                               
FRCLI20  MVC   0(3,R6),=X'FFFFFF'                                               
         MVC   KEY,PPGKEY                                                       
         LA    R4,DMRDHI                                                        
         BAS   RE,PDIRRD                                                        
         MVC   SVMEDCLI,QMEDIA     SAVE MEDIA/CLIENT                            
FRCLIX   BR    R9                                                               
         EJECT                                                                  
PDIRRD   NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                 PASS DELETES                                 
         DC    H'0'                                                             
PDIRX    XIT                                                                    
*                                                                               
*                                                                               
PFILERD  NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTFILE',KEY+27,         X        
               (R3),(0,DMWORK)                                                  
         CLI   DMCB+8,0                                                         
         BE    PFILX                                                            
         DC    H'0'                                                             
PFILX    XIT                                                                    
         EJECT                                                                  
PROFRST  NTR                                                                    
         CLI   PRDSW,1             SEE IF DOING PRDS SEPARATELY                 
         BE    PRDFX               SKIP PRD PRINT                               
         CLC   QCLIENT,=C'ALL'                                                  
         BE    PRDF                                                             
         CLI   QCLIENT,C'*'        OFFICE REQS                                  
         BE    PRDF                                                             
         CLI   QCLIENT,C'$'        OFFICE LIST                                  
         BE    PRDF                                                             
         CLI   QCLIENT,C'&&'       GROUP REQS                                   
         BE    PRDF                                                             
         B     PRDF0               ALL PRDS - PRD WILL BE IN TABLE              
*                                                                               
PRDF     DS    0H                                                               
         CLI   QPUB,C'0'           SEE IF DOING ONE PUB                         
         BNL   PRDF3               YES- MUST READ PRODUCT                       
PRDF0    L     R6,APRDTAB                                                       
PRDF1    CLC   0(3,R6),=X'FFFFFF'                                               
         BNE   *+6                                                              
         DC    H'0'                PRD NOT FOUND                                
         CLC   0(3,R6),PBUYKPRD                                                 
         BE    PRDF4                                                            
         LA    R6,23(R6)                                                        
         B     PRDF1                                                            
*                                                                               
*                                                                               
PRDF3    DS    0H                                                               
         MVC   PPGKEY,KEY          SAVE PPG KEY                                 
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),PCLTKCLT                                                
         MVC   KEY+7(3),PBUYKPRD                                                
         LA    R4,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         BAS   RE,PDIRRD                                                        
         CLC   KEYSAVE(10),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                PRD NOT FOUND                                
         TM    KEY+25,X'80'                                                     
         BZ    *+6                                                              
         DC    H'0'                FATAL ERROR PRD IS DELETED                   
         LA    R4,GETREC                                                        
         LA    R3,PPRDREC                                                       
         BAS   RE,PFILERD                                                       
PRDF3C   DS    0H                                                               
         MVC   KEY,PPGKEY                                                       
         LA    R4,DMRDHI                                                        
         BAS   RE,PDIRRD                                                        
         B     PRDF5                                                            
*                                                                               
PRDF4    MVC   PPRDNAME(20),3(R6)                                               
         MVC   PPRDKPRD(3),PBUYKPRD                                             
*                                                                               
PRDF5    DS    0H                                                               
         MVC   P+1(7),=C'PRODUCT'                                               
         MVC   P+9(3),PBUYKPRD                                                  
         MVC   P+14(20),PPRDNAME                                                
         IC    R0,LINE                                                          
         AH    R0,=H'3'                                                         
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'P'      NEW PAGE AND FORCE PAGE                       
         GOTO1 VPRINTIT                                                         
         GOTO1 VPRINTIT                                                         
PRDF8    MVC   SAVEPRD,PBUYKPRD                                                 
*                                                                               
PRDFX    XIT                                                                    
         EJECT                                                                  
*              ROUTINE TO FLAG BILLED/TRAFFICED ITEMS                           
TSTBLTR  NTR                                                                    
         LA    R3,P+0                                                           
         CLI   QOPT5,C'Y'                                                       
         BE    *+8                                                              
         LA    R3,P+2               I CAN USE P+2 IF NOT FLAGGING               
*                                   BILLED/TRAFFICKED ITEMS                     
*BPLA    TM    PBDSTAT,X'40'        ALWAYS FLAG MATCHED INSERTIONS              
*BPLA    BZ    *+8                                                              
*BLPA    MVI   0(R3),C'M'                                                       
*                                                                               
         CLI   QOPT5,C'Y'           SEE IF FLAGING BILLED/TRAFFICKED            
         BNE   TSTBX                                                            
         LA    R3,PBDELEM                                                       
         MVI   ELCOD,X'26'                                                      
TSTB1    BAS   RE,NEXTBEL                                                       
         BNE   TSTB4                                                            
         OC    5(3,R3),5(R3)       CK FOR ANY DATE                              
         BZ    TSTB1                                                            
         TM    10(R3),X'C0'        IGNORE REVERSED AND REVERSALS                
         BNZ   TSTB1                                                            
         CLI   ASOFDTE,0                                                        
         BNE   TSTB2                                                            
         MVI   P+1,C'B'                                                         
         B     TSTB4                                                            
*                                                                               
TSTB2    CLC   5(3,R3),ASOFDTE     CHK VS. AS OF DATE                           
         BH    TSTB1                                                            
         MVI   P+1,C'B'                                                         
         B     TSTB4                                                            
*                                                                               
TSTB4    LA    R3,PBDELEM           RESET R3                                    
         MVI   ELCOD,X'70'                                                      
TSTB5    BAS   RE,NEXTBEL                                                       
         BNE   TSTBX                                                            
         OC    2(3,R3),2(R3)                                                    
         BZ    TSTB5                                                            
         CLI   ASOFDTE,0                                                        
         BNE   TSTB6                                                            
         MVI   P+2,C'T'                                                         
         B     TSTBX                                                            
*                                                                               
TSTB6    CLC   2(3,R3),ASOFDTE                                                  
         BH    TSTB5                                                            
         MVI   P+2,C'T'                                                         
         B     TSTBX                                                            
*                                                                               
NEXTBEL  DS    0H                                                               
         CLI   0(R3),0         END OF REC                                       
         BE    NEXTBELX                                                         
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLC   0(1,R3),ELCOD                                                    
         BER   RE                                                               
         B     NEXTBEL                                                          
*                                                                               
NEXTBELX LTR   RE,RE           SET CONDITION CODE NE                            
         BR    RE                                                               
*                                                                               
TSTBX    MVI   LETSW,0                                                          
         CLI   NOLETSW,C'Y'        SEE IF NVTEXT RECORD FOUND                   
         BE    TSTBXX              NO - THEN NO LETTER                          
*                                                                               
         TM    PBDLETR,X'01'       FIRST CHECK NO-LETTER INDICATOR              
         BNZ   TSTBXX              IF 'ON' MEANS NO LETTER                      
*                                                                               
         CLI   PROGPROF+11,C'Y'    SUPPRESS IF MATCH OR ADBUYER INVOICE         
         BNE   TSTBXC              HAS BEEN ENTERED WHOSE                       
*                                  PERIOD COVERS THIS BUY ?                     
*                                                                               
         BAS   RE,CKINV            CHECK FOR INV MATCH INVOICE                  
         CLI   P+3,C'I'          CHECK INVOICE RECEIVED INDICATOR               
         BE    TSTBXX                                                           
*                                                                               
         BAS   RE,CKANV            CHECK FOR ADBUYER INVOICE                    
         CLI   P+3,C'I'          CHECK INVOICE RECEIVED INDICATOR               
         BE    TSTBXX                                                           
*                                                                               
TSTBXC   DS    0H                                                               
*                                  IF PROGPROF+9 = Y                            
*                                  DO LETTER EVEN IF PAY ELEM EXISTS            
         CLI   PROGPROF+9,C'Y'                                                  
         BE    TSTBX0B             DO LETTER IF PARTIALLY PAID                  
*                                  EVEN IF MATCHED                              
*                                                                               
*                                  IF PROGROF+9 IS NOT "Y"                      
*                                  FIRST CHECK MATCHED STATUS                   
TSTBX0   TM    PBDSTAT,X'40'       SEE IF MATCHED TO AN INVOICE                 
         BNZ   TSTBXX              IF ON - NO LETTER                            
*                                                                               
         LA    RE,PBDELEM          NOW CHECK FOR ADBUYER INVOICE                
         USING PBNVELMD,RE                                                      
TSNVLUP  DS    0H                                                               
         CLI   0(RE),PBNVELQ       NEW INVOICE ELEM ?                           
         BNE   TSNVNXT                                                          
         CLI   PBNVMTCH,PBNVMNIQ   ATTACHED TO ADBUYER INVOICE ?                
         BNE   TSTBXX              YES - NO LETTER                              
TSNVNXT  DS    0H                                                               
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0             END OF REC ?                                 
         BE    *+8                 YES - CONTINUE - NO INVOICE ON BUY           
         B     TSNVLUP             LOOK FOR ANOTHER BUY/INVOICE POINTER         
*                                                                               
         DROP  RE                                                               
*                                  NEXT CHECK FOR DATED PAY ELEM                
TSTBX0B  LA    R3,PBDELEM                                                       
         MVI   ELCOD,X'25'         CHECK FOR PAY ELEM WITH DATE                 
TSTBX1   BAS   RE,NEXTBEL                                                       
         BNE   TSTBX4                                                           
         OC    2(3,R3),2(R3)       CK FOR ANY DATE                              
         BZ    TSTBX1                                                           
         CLI   ASOFDTE,0                                                        
         BNE   TSTBX2                                                           
         CLI   PROGPROF+9,C'Y'                                                  
         BE    TSTBX4B      SKIP MATCHED CHECK - DO LETTER                      
         B     TSTBXX              NO LETTERS FOR PAID INSERTIONS               
*                                                                               
TSTBX2   CLC   2(3,R3),ASOFDTE     CHK VS. AS OF DATE                           
         BH    TSTBX1                                                           
         CLI   PROGPROF+9,C'Y'                                                  
         BE    TSTBX4B      SKIP MATCHED CHECK - DO LETTER                      
         B     TSTBXX              NO LETTERS FOR PAID INSERTIONS               
*                                                                               
TSTBX4   DS    0H       GET HERE IF NO DATED PAY ELEMENT IS FOUND               
         TM    PBDSTAT,X'40'       SEE IF MATCHED TO AN INVOICE                 
         BNZ   TSTBXX              IF SO - NO LETTER                            
TSTBX4B  DS    0H                                                               
***                                                                             
***      CHECK PAYABLE DATE VS. PROFILE MAXIMIM AND MINIMUM                     
***                                                                             
***      IF DIFFERENCE BETWEEN TODAY'S DATE AND PAYABLE DATE                    
***      IS LESS THAN MINIMUM OR GREATER THAN MAXIMUM                           
***      DON'T PRODUCE LETTER                                                   
***                                                                             
***      PROGPROF+4 = MAXIMUM UNITS TYPE D=DAYS,M=MONTHS                        
***      PROGPROF+5 = MAXIMUM UNITS 0-255                                       
***                                                                             
***      PROGPROF+7 = MINIMUM UNITS TYPE D=DAYS,M=MONTHS                        
***      PROGPROF+8 = MINIMUM UNITS 0-255                                       
***      PROGPROF+9 = Y = PRODUCE LETTER EVEN IF I FOUND PAY ELEM               
***                   MUST STILL HAVE A PAYABLE AMOUNT                          
***      PROGPROF+10  X = NO $ ON LETTER                                        
***                   G = SHOW GROSS                                            
***                   N = SHOW NET                                              
***                   1 = SHOW GROSS-CD                                         
***                   2 = SHOW NET-CD                                           
***                                                                             
         GOTO1 DATCON,DMCB,(3,PBDPDATE),(0,WORK)     CONVERT PAY DATE           
         GOTO1 DATCON,DMCB,(5,0),(0,WORK+6)          GET TODAY'S DATE           
         GOTO1 VPERVERT,DMCB,WORK,WORK+6                                        
         CLI   PROGPROF+4,C'D'                                                  
         BNE   LET1                                                             
         CLC   DMCB+8(2),=X'00FF'                                               
         BH    TSTBXX                 MUST BE GREATER THAN MAXIMUM              
         CLC   DMCB+9(1),PROGPROF+5          IF NO. OF DAYS INBETWEEN           
         BH    TSTBXX                        IS GREATER THAN PROFILE            
         B     DOLET                                                            
*                                            DO NOT PUT TO TABLE                
LET1     DS    0H                                                               
         CLI   PROGPROF+4,C'M'                                                  
         BE    *+6                                                              
         DS    H'0'                                                             
**NEW 5/16/88                                                                   
         CLI   PROGPROF+5,0         NO MONTHS                                   
         BE    DOLET                SO ALWAYS DO LETTER                         
         CLC   DMCB+14(2),=X'00FF'                                              
         BH    TSTBXX                MUST BE GRATER THAN MAXIMUM                
*                                                                               
         MVC   FULL(2),DMCB+14       SAVE REAL DETLA MONTHS                     
*                                                                               
         LH    R9,DMCB+14                                                       
         SH    R9,=H'1'              ADJUST DELTA MONTHS                        
         CLC   WORK+10(2),WORK+4      CHECK DAYS                                
         BNL   LET1B                 IF DAY LOWER SUBTRACT 2                    
         SH    R9,=H'1'              ADJUST DELTA MONTHS                        
LET1B    STH   R9,DMCB+14                                                       
         CLC   DMCB+15(1),PROGPROF+5                                            
         BH    TSTBXX                                                           
*                                                                               
         MVC   DMCB+14(2),FULL         RESTORE REAL DELTA MONTHS                
*                                                                               
         B     DOLET                                                            
*                                                                               
DOLET    DS    0H                  SEE IF LESS THAN MINIMUM                     
***                        NOTE - DMCB+8 STILL HAS VALUES FROM PERVERT          
***                                                                             
         CLI   PROGPROF+7,0                  NO MINIMUM                         
         BE    DLETOK                                                           
         CLI   PROGPROF+7,C'D'                                                  
         BNE   DLET1                                                            
         CLC   DMCB+8(2),=X'00FF'                                               
         BH    DLETOK                  MUST BE GREATER THAN MINIMUM             
         CLC   DMCB+9(1),PROGPROF+8          IF NO. OF DAYS INBETWEEN           
         BL    TSTBXX                        IS LESS THAN PROFILE               
*                                            DO NOT PUT TO TABLE                
         B     DLETOK                                                           
*                                                                               
DLET1    DS    0H                                                               
         CLI   PROGPROF+7,C'M'                                                  
         BE    *+6                                                              
         DS    H'0'                      INVALID PROFILE                        
**NEW 5/16/88                                                                   
         CLI   PROGPROF+8,0         NO MONTHS                                   
         BE    DLETOK               SO ALWAYS DO LETTER                         
         CLC   DMCB+14(2),=X'00FF'                                              
         BH    TSTBXX                MUST BE GRATER THAN MAXIMUM                
         LH    R9,DMCB+14                                                       
         SH    R9,=H'1'              ADJUST DELTA MONTHS                        
         CLC   WORK+10(2),WORK+4      CHECK DAYS                                
         BNL   DLET1B                IF DAY LOWER SUBTRACT 2                    
         SH    R9,=H'1'              ADJUST DELTA MONTHS                        
DLET1B   STH   R9,DMCB+14                                                       
         CLC   DMCB+15(1),PROGPROF+8                                            
         BL    TSTBXX                                                           
         B     DLETOK                                                           
*                                                                               
DLETOK   MVI   LETSW,C'Y'                    WILL TELL LETTER TO ADD            
         MVI   P+3,C'L'                                                         
*                                            TO TABLE                           
TSTBXX   XIT                                                                    
*                                                                               
         EJECT                                                                  
*                             ROUTINE TO CHECK FOR INV MATCH INVOICE            
CKINV    ST    RE,FULL        SAVE RETURN REGISTER                              
*                                                                               
         L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         MVC   ACONIO2,ACONIO      (A)PCONREC                                   
         DROP  RF                                                               
*                                                                               
         MVC   PPGKEY,KEY     SAVE PPG'S KEY                                    
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY  AGY/MED CLT/PRD                                 
         MVI   KEY+3,X'50'                                                      
         MVC   KEY+10(6),PBUYKPUB                                               
CKINV1   MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         B     CKINV5                                                           
*                                                                               
CKINV2   GOTO1 SEQ                                                              
CKINV5   DS    0H                                                               
         CLC   KEY(14),KEYSAVE    CHECK THRU BASE PUB                           
         BNE   CKINV20            NOT EQUAL EXIT                                
         CLC   KEY+14(2),=X'FFFF' SEE IF ALL ZONE/EDT RECORD                    
         BE    CKINV10                                                          
         CLC   KEY(16),KEYSAVE     PUB MUST MATCH COMPLETELY                    
         BNE   CKINV2                                                           
*                                                                               
CKINV10  DS    0H                                                               
         TM    KEY+25,X'80'     SKIP DELETED                                    
         BO    CKINV2                                                           
*                                                                               
         LA    R4,GETREC                                                        
         L     R3,ACONIO2     PFILERD NEEDS IN R3                               
         BAS   RE,PFILERD                                                       
         L     R6,ACONIO2     READ INTO PCONREC                                 
         LA    R6,33(R6)                                                        
         MVI   ELCOD,X'10'                                                      
         CLI   0(R6),X'10'    SEE IF FIRST ELEMENT IS X'10'                     
         BE    CKINV16                                                          
CKINV15  BAS   RE,LNEXTEL                                                       
         BNE   CKINV2              CHECK ANOTHER RECORD                         
         USING PIMHDREL,R6                                                      
CKINV16  CLC   PBUYKDAT,PIMSTDT    SEE IF BEFORE START                          
         BL    CKINV15                                                          
         CLC   PBUYKDAT,PIMENDDT   OR AFTER END                                 
         BH    CKINV15                                                          
         CLC   PIMEST,=C'000'     SEE IF ESTIMATE PRESENT                       
         BNH   CKINVOK                                                          
         PACK  DUB,PIMEST         IF SO, IT MUST MATCH BUY'S                    
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         CLC   HALF,PBUYKEST                                                    
         BNE   CKINV15                                                          
         B     CKINVOK                                                          
*                                                                               
CKINV20  CLC   KEYSAVE+7(3),=C'***'    SEE IF I'VE CHECKED FOR PRD ***          
         BE    CKINVX                  DONE                                     
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY  AGY/MED CLT/PRD                                 
         MVC   KEY+7(3),=C'***'       TRY FOR A PRD *** RECORD                  
         MVI   KEY+3,X'50'                                                      
         MVC   KEY+10(6),PBUYKPUB                                               
         B     CKINV1                                                           
*                                                                               
CKINVOK  MVI   P+3,C'I'         SET INVOICE RECEIVED                            
CKINVX   MVC   KEY,PPGKEY                                                       
         GOTO1 HIGH                                                             
         L     RE,FULL                                                          
         BR    RE              RETURN                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*                             ROUTINE TO CHECK FOR ADBUYER INVOICE              
CKANV    ST    RE,FULL        SAVE RETURN REGISTER                              
*                                                                               
         L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         MVC   ACONIO2,ACONIO      (A)PCONREC                                   
         DROP  RF                                                               
*                                                                               
         XC    NVHDRDTA,NVHDRDTA   CLEAR INVOICE HDR ELEM DATA                  
         XC    NVCMPDTA,NVCMPDTA   CLEAR COMPARE TO BUY ELEM DATA               
*                                                                               
         MVC   PPGKEY,KEY     SAVE PPG'S KEY                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(07),PBUYKEY  AGY/MED CLT                                     
         MVI   KEY+3,X'B2'         INVOICE PASSIVE POINTER                      
         MVC   KEY+07(6),PBUYKPUB                                               
*                                                                               
CKANV1   DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         B     CKANV5                                                           
*                                                                               
CKANV2   DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
CKANV5   DS    0H                                                               
         TM    KEY+25,X'80'     SKIP DELETED                                    
         BO    CKANV2                                                           
*                                                                               
         CLC   KEY(13),KEYSAVE      CHECK THRU PUB                              
         BNE   CKANVX               NOTHING FOUND EXIT                          
         CLC   KEY+13(3),PBUYKDAT   ARE WE PAST THE INVCE PERIOD START?         
         BH    CKANVX               YES - NOTHING FOUND EXIT                    
         CLC   KEY+16(3),PBUYKDAT   ARE WE PAST THE INVCE PERIOD END?           
         BL    CKANV2               YES - READ NEXT                             
*                                                                               
         XC    B2KEY,B2KEY                                                      
         MVC   B2KEY(L'PNVKEY),KEY  SAVE KEY FOR POSSIBLE CONTINUATION          
*                                                                               
CKANV10  DS    0H                  NOW CHECK INVOICE RECORDS                    
*                                                                               
CK10TOP  DS    0H                                                               
*                                                                               
*            B2 PASSIVE POINTS TO LAST RECORD OF MINIO SET SO NOW MAKE          
*            SURE WE ARE STARTING AT THE FIRST RECORD OF THE SET                
*                                                                               
         LA    R6,B2KEY            POINT TO PASSIVE KEY                         
         USING PNV2KEYD,R6                                                      
         LA    R4,KEY                                                           
         USING PNVRECD,R4          ESTABLISH AS INVOICE RECORD                  
         XC    KEY,KEY                                                          
         MVC   PNVKEY(4),PNV2KEY   AGY/MED/RC                                   
         MVI   PNVKRCD,PNVKRCDQ    REPLACE X'B2' RECORD CODE WITH X'70'         
         MVC   PNVKSER#,PNV2SER#   UNIQUE INVOICE SERIAL NUMBER                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(PNVKELMK-PNVKEY),KEYSAVE                                     
         BE    *+6                                                              
         DC    H'0'                MUST BE FOUND                                
         DROP  R4,R6                                                            
*                                                                               
         LA    R4,GETREC                                                        
         L     R3,ACONIO2                                                       
*                                                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTFILE',KEY+27,         X        
               (R3),(0,DMWORK)                                                  
         CLI   DMCB+8,0                                                         
         BE    CK10REC                                                          
         TM    DMCB+8,X'02'        DELETED ?                                    
         BO    CKANV2              YES - NEXT PASSIVE                           
         DC    H'0'                NO OTHER ERRORS TOLERATED                    
*                                                                               
CK10REC  DS    0H                                                               
         L     R6,ACONIO2     PNVREC HAS BEEN READ INTO PCONREC                 
*                                                                               
         LA    R6,33(R6)                                                        
         MVI   ELCOD,X'10'                                                      
         CLI   0(R6),X'10'    SEE IF FIRST ELEMENT IS X'10'                     
         BE    *+6                                                              
         DC    H'0'                MUST BE FOUND                                
         USING PNVHDRD,R6          INVOICE HEADER ELEMENT                       
         OC    PNVHKSPR,PNVHKSPR                                                
         BZ    *+6                                                              
         DC    H'0'                AND MUST BE INVOICE HEADER                   
*      FIRST SAVE HEADER CLT, PRD, EST, PUB & PERIOD INFO.                      
         MVC   NVHDRCLT,PNVHCLT                                                 
         MVC   NVHDRPRD,PNVHPRD                                                 
         MVC   NVHDREST,PNVHEST                                                 
         MVC   NVHDRPUB,PNVHPUB                                                 
         MVC   NVHDRSTR,PNVHSTRT                                                
         MVC   NVHDREND,PNVHEND                                                 
*                                                                               
         MVC   NVCMPDTA,NVHDRDTA   COPY HDR INFO TO "CHANGEABLE" AREA           
*                                                                               
         DROP  R6                                                               
*                                                                               
         BAS   RE,TSTXCL           SEE IF HDR DATA EXCLUDES BUY                 
         BE    CKANVOK             YES - DONE                                   
*                                  NO                                           
CK20TOP  DS    0H                  CHECK X'20' INVOICE DETAIL ELEMENTS          
         L     R6,ACONIO2          PNVREC HAS BEEN READ INTO PCONREC            
         LA    R6,33(R6)                                                        
         MVI   ELCOD,X'20'                                                      
         CLI   0(R6),X'20'         SEE IF FIRST ELEMENT IS X'20'                
         BE    CK20TST                                                          
*                                                                               
CK20LUP  DS    0H                                                               
*                                                                               
         MVC   NVCMPDTA,NVHDRDTA   COPY HDR INFO TO "CHANGEABLE" AREA           
*                                                                               
         BAS   RE,LNEXTEL                                                       
         BNE   CK20END                                                          
*                                                                               
         USING PNVDTLD,R6                                                       
CK20TST  DS    0H                                                               
         CLI   PNVDKTYP,PNVDKDSQ   DETAIL DESCRIPTION ?                         
         BNE   CK20LUP             NO - CHECK NEXT                              
*                                                                               
         TM    PNVDSTAT,PNVDDLQ    DELETED ?                                    
         BO    CK20LUP             YES - CHECK NEXT                             
*                                                                               
         OC    PNVDPUB,PNVDPUB     PUB THERE ?                                  
         BZ    *+10                NO                                           
         MVC   NVCMPPUB,PNVDPUB    USE IT                                       
         OC    PNVDCLT,PNVDCLT     CLIENT THERE ?                               
         BZ    *+10                NO                                           
         MVC   NVCMPCLT,PNVDCLT    USE IT                                       
         OC    PNVDPRD,PNVDPRD     PRODUCT THERE ?                              
         BZ    *+10                NO                                           
         MVC   NVCMPPRD,PNVDPRD    USE IT                                       
         OC    PNVDEST,PNVDEST     ESTIMATE THERE ?                             
         BZ    *+10                NO                                           
         MVC   NVCMPEST,PNVDEST    USE IT                                       
*                                                                               
         DROP  R6                                                               
*                                                                               
         BAS   RE,TSTXCL           SEE IF DTL DATA EXCLUDES BUY                 
         BE    CKANVOK             YES - DONE                                   
*                                                                               
         B     CK20LUP             LOOK FOR NEXT ELEM                           
*                                                                               
CK20END  DS    0H                                                               
         L     R6,ACONIO2          PNVREC HAS BEEN READ INTO PCONREC            
         USING PNVRECD,R6                                                       
         CLC   PNVKELMK,=7X'FF'    LAST OF THE MINIO SET ?                      
         BNE   CK20NXT             NO - GET THE NEXT RECORD OF SET              
*                                  YES                                          
         XC    KEY,KEY             SET FOR NEXT B2 PASSIVE READ                 
         MVC   KEY(L'PNVKEY),B2KEY                                              
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'PNVKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         B     CKANV2              NEXT PASSIVE                                 
*                                                                               
CK20NXT  DS    0H                                                               
         XC    KEY,KEY             SET UP FOR NEXT INVOICE REC READ             
         MVC   KEY(L'PNVKEY),PNVREC                                             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                REREAD RECORD JUST PROCESSED                 
         CLC   KEY(L'PNVKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         GOTO1 SEQ                                                              
         CLC   KEY(PNVKELMK-PNVKEY),KEYSAVE                                     
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    R4,GETREC                                                        
         L     R3,ACONIO2          PFILERD NEEDS IN R3                          
         BAS   RE,PFILERD                                                       
*                                                                               
         B     CK20TOP             CHECK MORE X'20' DETAIL ELEMENTS             
*                                                                               
*                                                                               
CKANVOK  MVI   P+3,C'I'         SET INVOICE RECEIVED (SKIP LETTER)              
CKANVX   DS    0H                                                               
*                                                                               
         B     CKATSTX              BRANCH AROUND TESTING BELOW                 
*                                                                               
******* TESTING DEATH BELOW  *******************************                    
         CLC   PBUYKCLT,=C'JFS'                                                 
         BNE   CKATSTX                                                          
         CLC   PBUYKPRD,=C'DAI'                                                 
         BNE   CKATSTX                                                          
         CLC   PBUYKPUB(6),=X'0000100701E2'                                     
         BNE   CKATSTX                                                          
         CLC   PBUYKEST,=X'0001'                                                
         BNE   CKATSTX                                                          
         CLC   PBUYKDAT,=X'6A0702'                                              
         BNE   CKATSTX                                                          
         DC    H'0'                                                             
CKATSTX  DS    0H                                                               
******* TESTING ********************************************                    
*                                                                               
         MVC   KEY,PPGKEY                                                       
         GOTO1 HIGH                                                             
         L     RE,FULL                                                          
         BR    RE              RETURN                                           
*                                                                               
         SPACE 3                                                                
*                                                                               
*   IF BUY IS WITHIN THE PERIOD OF THE INVOICE AND WE HAVE A MATCH              
*   ON ALL BUY KEY FIELDS (CLT, PRD, ETC.) TO THESE FIELDS FROM AN              
*   INVOICE, RETURN CC EQUAL (FOR BUY EXCLUSION) TO CALLING ROUTINE             
*   AS THE BUY IS COVERED UNDER THIS INVOICE                                    
*                                                                               
TSTXCL   DS    0H                                                               
         NTR1                                                                   
         CLC   PBUYKDAT,NVCMPSTR   SEE IF BEFORE START                          
         BL    TSTXNEQ             YES - NOT COVERED BY INVOICE                 
         CLC   PBUYKDAT,NVCMPEND   OR AFTER END                                 
         BH    TSTXNEQ             YES - NOT COVERED                            
         CLC   PBUYKCLT,NVCMPCLT   CLIENT EQUAL?                                
         BNE   TSTXNEQ             NO - NOT COVERED                             
         CLC   PBUYKPRD,NVCMPPRD   PRODUCT EQUAL?                               
         BNE   TSTXNEQ             NO - NOT COVERED                             
         CLC   PBUYKEST,NVCMPEST   ESTIMATE EQUAL?                              
         BNE   TSTXNEQ             NO - NOT COVERED                             
         CLC   PBUYKPUB,NVCMPPUB   PUB EQUAL?                                   
         BNE   TSTXNEQ             NO - NOT COVERED                             
*                                  IS COVERED - RETURN EQUAL CC                 
TSTXEQ   DS    0H                                                               
         CR    RB,RB               EQUAL                                        
         B     *+6                                                              
TSTXNEQ  DS    0H                                                               
         LTR   RB,RB               NOT EQUAL                                    
         XIT1                                                                   
*                                                                               
B2KEY    DS    CL32                                                             
*                                                                               
NVHDRDTA DS    0CL(NVHDRXX-NVHDRCLT)     ADB INVOICE HDR ELEM INFO              
NVHDRCLT DS    CL3                                                              
NVHDRPRD DS    CL3                                                              
NVHDREST DS    XL2                                                              
NVHDRPUB DS    XL6                                                              
NVHDRSTR DS    XL3                 PERIOD START DATE                            
NVHDREND DS    XL3                 PERIOD END DATE                              
NVHDRXX  EQU   *                                                                
*                                                                               
NVCMPDTA DS    0CL(NVCMPXX-NVCMPCLT)     ADB INVOICE HDR ELEM INFO              
NVCMPCLT DS    CL3                    ADB INVOICE HDR ELEM INFO                 
NVCMPPRD DS    CL3                                                              
NVCMPEST DS    XL2                                                              
NVCMPPUB DS    XL6                                                              
NVCMPSTR DS    XL3                 PERIOD START DATE                            
NVCMPEND DS    XL3                 PERIOD END DATE                              
NVCMPXX  EQU   *                                                                
*                                                                               
*                                                                               
EXT      XMOD1 1                                                                
         LTORG                                                                  
ACONIO2  DS    A                                                                
*                                                                               
MAXLET   DC    F'4499'         MAXIMUM LETTERS                                  
*                              LETRTAB IS 990000 (220 PER LETTER)               
*                              MAXLET IS 4499 INSTEAD OF 4500                   
*                              SINCE THE PROGRAM CLEARS THE NEXT                
*                              220 BYTES                                        
*                                                                               
*                                                                               
         EJECT                                                                  
PRNTPUB  CSECT                                                                  
         NMOD1 0,PRNTPUB                                                        
         USING PPWORKD,RA                                                       
         USING NVWRKD,R5,R9                                                     
         USING PPFILED,RC                                                       
         L     RC,PPFILEC                                                       
         L     R5,VNVWORK                                                       
         LA    R9,4095(R5)                                                      
         LA    R9,1(R9)                                                         
         CLI   QOPT2,C'Y'     YES= SKIP ON NEW PUB                              
         BNE   *+8                                                              
         MVI   FORCEHED,C'P'  NEW PAGE AND FORCE PAGE                           
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'6'                                                         
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'P'  NEW PAGE AND FORCE PAGE                           
         LA    R7,PPFILED+4095                                                  
         LA    R7,1(R7)                                                         
         USING PPFILED+4096,R7                                                  
         MVC   P+1(17),SPACES                                                   
         MVI   PUBPSW,0                                                         
         MVC   SAVPLIN1,SPACES                                                  
         MVC   SAVPLIN2,SPACES                                                  
         IC    R3,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R3),PBUYKPUB),(0,SAVPLIN1+1)                      
         CLI   FORCEHED,C'P'  NEW PAGE AND FORCE PAGE                           
         BE    SETR3               DONT SKIP A LINE                             
         GOTO1 VPRINTIT                                                         
SETR3    LA    R3,SAVPLIN1+1                                                    
         LA    R4,SAVPLIN2+1                                                    
         SR    R9,R9                                                            
SETR3A   CLI   0(R3),C' '                                                       
         BE    FLOAT                                                            
         MVI   0(R4),C'-'                                                       
         LA    R9,1(R9)                                                         
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         B     SETR3A                                                           
*                                                                               
FLOAT    LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         LA    R9,1(R9)            SAVE DISPLACEMENT                            
         GOTO1 VPUBFLOT,DMCB,PUBREC,(R3)                                        
         MVC   P,SAVPLIN1                                                       
         MVC   PSECOND,SAVPLIN2                                                 
         MVC   0(11,R4),=C'(CONTINUED)'                                         
*                                                                               
         LA    R3,P+1                                                           
         AR    R3,R9                                                            
         LA    R4,PSECOND+1                                                     
         AR    R4,R9                                                            
*                                                                               
         MVC   SAVEPUB,PBUYKPUB                                                 
         CLI   QOPT6,C'Y'          SEE IF PRINTING PAYING ADDR                  
         BE    PRNTP1                                                           
         CLC   QSORT,=C'08'         OR REP SORT                                 
         BE    PRNTP1                                                           
         CLC   QSORT,=C'09'         OR PAY ADDR NAME                            
         BE    PRNTP1                                                           
         GOTO1 VPRINTIT                                                         
         B     PRNTPX                                                           
*                                                                               
PRNTP1   DS    0H                                                               
         CLI   FORCEHED,C'P'                                                    
         BE    PRNTP2                                                           
         IC    R0,SAVELINE                                                      
         CLC   QSORT,=C'08'        SEE IF SORTING BY REP                        
         BNE   *+8                                                              
         AH    R0,=H'1'            FOR REP NUMBER LINE                          
         CLC   QSORT,=C'09'        OR PAY ADDR NAME                             
         BNE   *+8                                                              
         AH    R0,=H'1'            FOR REP NUMBER LINE                          
         CLI   QOPT6,C'Y'          SEE IF SHOWING ADDRS                         
         BNE   PRNTP1C                                                          
         AH    R0,=H'4'                                                         
         CLI   PREPNAME,C' '       SEE IF ADDR FOUND                            
         BH    *+8                                                              
         AH    R0,=H'1'                                                         
PRNTP1C  STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'P'  NEW PAGE AND FORCE PAGE                           
PRNTP2   DS    0H                                                               
         CLI   PREPNAME,C' '       SEE IF ADDR FOUND                            
         BH    PRNTP2C             YES                                          
         MVC   0(20,R4),=C'** PAY PUB DIRECT **'                                
         GOTO1 VPRINTIT                                                         
         CLI   QOPT6,C'Y'          SEE IF SHOWING ADDR                          
         BNE   PRNTP7                                                           
         MVC   0(30,R3),PUBLINE1   P                                            
         MVC   0(30,R4),PUBLINE2   PSECOND                                      
         GOTO1 VPRINTIT                                                         
         LA    R6,PUBREC+33                                                     
PRNTP2A  CLI   0(R6),X'11'                                                      
         BE    PRNTP2B                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             END OF REC                                   
         BE    PRNTP7              NO ATTN OR TELE                              
         B     PRNTP2A                                                          
*                                                                               
PRNTP2B  DS    0H                                                               
         USING PUBSADEL,R6                                                      
         LR    R4,R3                                                            
         CLI   PUBATTN,C' '                                                     
         BNH   PRNTP2B2                                                         
         MVC   0(5,R4),=C'ATTN='                                                
         MVC   6(24,R4),PUBATTN                                                 
         LA    R4,31(R4)                                                        
*                                                                               
PRNTP2B2 CLI   PUBTEL,C' '                                                      
         BNH   PRNTP6              NO TELE                                      
         MVC   0(6,R4),=C'PHONE='                                               
         MVC   7(12,R4),PUBTEL                                                  
         B     PRNTP6                                                           
*                                                                               
         DROP  R6                                                               
         GOTO1 VPRINTIT                                                         
         B     PRNTP7                                                           
*                                                                               
PRNTP2C  DS    0H                                                               
         CLC   QSORT,=C'08'        SEE IF DOING REP SORT                        
         BE    PRNTP2C5                                                         
         CLC   QSORT,=C'09'        OR PAY ADDR NAME                             
         BE    PRNTP2C5                                                         
         B     PRNTP2F                                                          
*                                                                               
PRNTP2C5 CLI   PREPKEY+3,X'11'     SEE IF I FOUND A REP                         
         BNE   PRNTP2F             NO WAS PAY ADDR                              
         MVC   0(4,R4),=C'REP='                                                 
         MVC   5(4,R4),PREPKREP                                                 
         CLI   PREPKREP+4,0                                                     
         BE    PRNTP2D                                                          
         MVI   9(R4),C'.'                                                       
         MVC   10(1,R4),PREPKREP+4    SUFFIX                                    
PRNTP2D  GOTO1 VPRINTIT                                                         
         LR    R4,R3                                                            
*                                                                               
PRNTP2F  MVC   0(30,R4),PREPNAME                                                
         GOTO1 VPRINTIT                                                         
         CLI   QOPT6,C'Y'          SEE IF SHOWING ADDR                          
         BNE   PRNTP7              NO THEN DONE                                 
         CLI   PREPLIN1,C' '                                                    
         BNH   PRNTP3                                                           
         MVC   0(30,R3),PREPLIN1                                                
         GOTO1 VPRINTIT                                                         
*                                                                               
PRNTP3   CLI   PREPLIN2,C' '                                                    
         BNH   PRNTP4                                                           
         MVC   0(30,R3),PREPLIN2                                                
         GOTO1 VPRINTIT                                                         
*                                                                               
PRNTP4   LR    R4,R3                                                            
         CLI   PREPATTN,C' '                                                    
         BNH   PRNTP5                                                           
         MVC   0(5,R4),=C'ATTN='                                                
         MVC   6(20,R4),PREPATTN                                                
         LA    R4,27(R4)                                                        
*                                                                               
PRNTP5   CLI   PREPTEL,C' '                                                     
         BNH   PRNTP6              NO TELEPHONE                                 
         MVC   0(6,R4),=C'PHONE='                                               
         MVC   7(12,R4),PREPTEL                                                 
*                                                                               
PRNTP6   CLC   P,SPACES                                                         
         BE    PRNTP7                                                           
         GOTO1 VPRINTIT                                                         
*                                                                               
PRNTP7   DS    0H                                                               
         GOTO1 VPRINTIT                                                         
PRNTPX   DS    0H                                                               
         MVI   PUBPSW,1            SET PUB PRINTED                              
         XIT1                                                                   
*                                                                               
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
BLDMLST  CSECT                                                                  
         NMOD1 0,BLDMLST                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VNVWORK                                                       
         LA    R9,4095(R5)                                                      
         LA    R9,1(R9)                                                         
         USING NVWRKD,R5,R9                                                     
SPRDOK   MVC   WORK(12),QSTART       SAVE STRT AND END                          
         CLI   QBPDATE,C'B'                                                     
         BE    BILLPAY                                                          
         CLI   QBPDATE,C'P'                                                     
         BE    BILLPAY                                                          
BLDLIST  LA    R6,ACCNUM                SET FOR BCT                             
         LA    R4,MTHTAB                                                        
         MVC   0(4,R4),QSTART                                                   
PUTMTH   PACK  DUB,2(2,R4)                                                      
         AP    DUB,=P'1'                                                        
         CP    DUB,=P'13'                                                       
         BL    SAMEYR                                                           
         SP    DUB,=P'12'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  6(2,R4),DUB+6(2)                                                 
         PACK  DUB,0(2,R4)                                                      
         BAS   RE,BLDMUPYR         ADD 1 TO YR                                  
***      AP    DUB,=P'1'           ADD 1 TO YR                                  
***      OI    DUB+7,X'0F'                                                      
         UNPK  4(2,R4),DUB+6(2)                                                 
         CLC   4(4,R4),QEND                                                     
         BL    NEXTMTH                                                          
         BE    MTHDONE             EXIT                                         
         XC    4(4,R4),4(R4)       CLEAR LAST MTH                               
         B     MTHDONE             EXIT                                         
*                                                                               
SAMEYR   OI    DUB+7,X'0F'                                                      
         UNPK  6(2,R4),DUB+6(2)                                                 
         MVC   4(2,R4),0(R4)                                                    
         CLC   4(4,R4),QEND                                                     
         BL    NEXTMTH                                                          
         BE    MTHDONE                                                          
         XC    4(4,R4),4(R4)                                                    
         B     MTHDONE             EXIT                                         
*                                                                               
NEXTMTH  LA    R4,4(R4)                                                         
         BCT   R6,PUTMTH           ACCNUM MTHS MAX                              
         B     MTHDONE             EXIT                                         
*                                                                               
*                                                                               
*                                                                               
BILLPAY  PACK  DUB,QSTART+2(2)     SET START DATE BACK 6 MONTHS                 
         SP    DUB,=P'6'                                                        
         CP    DUB,=P'0'                                                        
         BNH   CHGSYR                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QSTART+2(2),DUB+6(2)                                             
         B     CHGEND                                                           
*                                                                               
CHGSYR   AP    DUB,=P'12'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  QSTART+2(2),DUB+6(2)                                             
         PACK  DUB,QSTART(2)                                                    
         BAS   RE,BLDMDNYR         SUBTRACT 1 FROM YR                           
***      SP    DUB,=P'1'                                                        
***      OI    DUB+7,X'0F'                                                      
         UNPK  QSTART(2),DUB+6(2)                                               
*                                                                               
CHGEND   PACK  DUB,QEND+2(2)                                                    
         AP    DUB,=P'6'      ADVANCE END DATE 6 MONTHS                         
         CP    DUB,=P'12'                                                       
         BH    CHGEYR                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QEND+2(2),DUB+6(2)                                               
         B     BLDLIST                                                          
*                                                                               
CHGEYR   SP    DUB,=P'12'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  QEND+2(2),DUB+6(2)                                               
         PACK  DUB,QEND(2)                                                      
         BAS   RE,BLDMUPYR         ADD 1 TO YR                                  
***      AP    DUB,=P'1'                                                        
***      OI    DUB+7,X'0F'                                                      
         UNPK  QEND(2),DUB+6(2)                                                 
         B     BLDLIST                                                          
*                                                                               
MTHDONE  MVC   QSTART(12),WORK    RESTORE DATES                                 
         XC    ASOFDTE,ASOFDTE     NOT USED IN NV-WAS "AS OF" DATE              
         XC    ASDATE,ASDATE       FOR PP27                                     
         CLI   QPAY,C' '                                                        
         BE    MTHD1                                                            
*        GOTO1 DTCNV,DMCB,(0,QPAY),(1,ASOFDTE)                                  
         GOTO1 DATCON,DMCB,(0,QPAY),(3,ASOFDTE)                                 
*        GOTO1 DTCNV,DMCB,(1,ASOFDTE),(3,ASDATE)                                
         GOTO1 DATCON,DMCB,(3,ASOFDTE),(5,ASDATE)                               
*                                                                               
MTHD1    XC    MTHTOTS(24),MTHTOTS NOW CLEAR ALL ACCUMS                         
         XC    PRDTOTS(24),PRDTOTS                                              
         LA    R6,6                                                             
         LA    R4,PUBTOTS                                                       
CLRPUB   XC    0(ACCNUM*4,R4),0(R4)                                             
         LA    R4,ACCNUM*4(R4)                                                  
         BCT   R6,CLRPUB                                                        
         LA    R6,6                                                             
         LA    R4,REPTOTS                                                       
CLRREP   XC    0(ACCNUM*4,R4),0(R4)                                             
         LA    R4,ACCNUM*4(R4)                                                  
         BCT   R6,CLRREP                                                        
*                                                                               
         LA    R6,7                                                             
         LA    R4,PROTOTS                                                       
CLRPRO   XC    0(ACCNUM*4,R4),0(R4)                                             
         LA    R4,ACCNUM*4(R4)                                                  
         BCT   R6,CLRPRO                                                        
         XC    PTOTPUBS,PTOTPUBS                                                
         XC    RTOTPUBS,RTOTPUBS                                                
*                                                                               
         LA    R6,7                                                             
         LA    R4,CLTTOTS                                                       
CLRCLT   XC    0(ACCNUM*4,R4),0(R4)                                             
         LA    R4,ACCNUM*4(R4)                                                  
         BCT   R6,CLRCLT                                                        
         LA    R6,7                                                             
         LA    R4,REQTOTS                                                       
CLRREQ   XC    0(ACCNUM*4,R4),0(R4)                                             
         LA    R4,ACCNUM*4(R4)                                                  
         BCT   R6,CLRREQ                                                        
         MVI   MTHACT,0            CLEAR ACTIVITY INDICATORS                    
         MVI   PRDACT,0                                                         
         MVI   PUBACT,0                                                         
         MVI   CLTACT,0                                                         
         MVI   REPACT,0                                                         
         MVI   REQACT,0                                                         
         XC    PRDMTHS,PRDMTHS                                                  
         XC    PUBPRDS,PUBPRDS                                                  
         XC    CTOTPUBS,CTOTPUBS                                                
         XC    RQTOTPUB,RQTOTPUB                                                
*        GOTO1 DTCNV,DMCB,(0,QSTART),(1,REQSTD)                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(3,REQSTD)                                
*        GOTO1 DTCNV,DMCB,(0,QEND),(1,REQENDD)                                  
         GOTO1 DATCON,DMCB,(0,QEND),(3,REQENDD)                                 
*                                                                               
BLMEXT   XIT1                                                                   
*                                                                               
****  INCREMENT YEARS BY 1 FOR "FA = 2000, FB=2010, ETC." DATE FORMATS          
*                                                                               
BLDMUPYR DS    0H                                                               
         NTR1                                                                   
         MVC   STRHIYR,DUB+6       SAVE HI-ORDER OF YR                          
         TM    DUB+7,X'90'         YR ENDING IN 9 ??                            
         BO    BMCYUP              YES                                          
         MVI   DUB+6,X'00'         "CLEAR" HI-ORDER OF YR                       
         AP    DUB,=P'1'           INCREMENT YR                                 
         OI    DUB+7,X'0F'                                                      
         MVC   DUB+6(1),STRHIYR    RESTORE HI-ORDER OF YR                       
         B     BLDMUPX             DONE WITH YR NOT ENDING IN 9                 
*                                                                               
BMCYUP   DS    0H                                                               
         MVI   DUB+7,X'0F'         LO-ORDER OF YR MUST BE 0 HERE                
         ZIC   R0,STRHIYR                                                       
         AH    R0,=H'1'            INCREMENT HI-ORDER OF YR                     
         STC   R0,DUB+6            RESTORE HI-ORDER OF YR                       
*                                                                               
BLDMUPX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
****  DECREMENT YEARS BY 1 FOR "FA = 2000, FB=2010, ETC." DATE FORMATS          
*                                                                               
BLDMDNYR DS    0H                                                               
         NTR1                                                                   
         MVC   STRHIYR,DUB+6       SAVE HI-ORDER OF YR                          
         TM    DUB+7,X'F0'         YR ENDING IN 0 ??                            
         BZ    BMCYDN              YES                                          
         MVI   DUB+6,X'00'         "CLEAR" HI-ORDER OF YR                       
         SP    DUB,=P'1'           DECREMENT YR                                 
         OI    DUB+7,X'0F'                                                      
         MVC   DUB+6(1),STRHIYR    RESTORE HI-ORDER OF YR                       
         B     BLDMDNX             DONE WITH YR NOT ENDING IN 0                 
*                                                                               
BMCYDN   DS    0H                                                               
         MVI   DUB+7,X'9F'         LO-ORDER OF YR MUST BE 9 HERE                
         ZIC   R0,STRHIYR                                                       
         SH    R0,=H'1'            DECREMENT HI-ORDER OF YR                     
         STC   R0,DUB+6            RESTORE HI-ORDER OF YR                       
*                                                                               
BLDMDNX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
STRHIYR  DS    X                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
PRINTIT  CSECT                                                                  
         NMOD1 0,PRINTIT                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VNVWORK                                                       
         USING NVWRKD,R5                                                        
*****                                                                           
         CLI   QOPT7,C'L'         L MEANS LETTERS ONLY                          
         BE    PRINTXZ                                                          
*****                                                                           
         LA    R6,HEAD10+56                                                     
         LA    R7,HEAD11+56                                                     
         LA    R8,HEAD12+56                                                     
         LA    R4,PROGPROF+2                                                    
         LA    R3,2                                                             
*                                                                               
PRNTH5   CLI   0(R4),C'0'                                                       
         BE    PRNTH50                                                          
         CLI   0(R4),C'G'                                                       
         BNE   PRNTH10                                                          
         MVC   5(5,R7),=C'GROSS'                                                
         MVC   5(5,R8),=C'----------'                                           
         B     PRNTH50                                                          
*                                                                               
PRNTH10  CLI   0(R4),C'N'                                                       
         BNE   PRNTH20                                                          
         MVC   7(3,R7),=C'NET'                                                  
         MVC   7(3,R8),=C'----------'                                           
         B     PRNTH50                                                          
*                                                                               
PRNTH20  CLI   0(R4),C'C'                                                       
         BNE   PRNTH30                                                          
         MVC   4(4,R6),=C'CASH'                                                 
         MVC   2(8,R7),=C'DISCOUNT'                                             
         MVC   2(8,R8),=C'----------'                                           
         B     PRNTH50                                                          
*                                                                               
PRNTH30  CLI   0(R4),C'1'                                                       
         BNE   PRNTH40                                                          
         MVC   0(10,R6),=C'GROSS-CASH'                                          
         MVC   1(8,R7),=C'DISCOUNT'                                             
         MVC   0(10,R8),=C'----------'                                          
         B     PRNTH50                                                          
*                                                                               
PRNTH40  CLI   0(R4),C'2'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   2(8,R6),=C'NET-CASH'                                             
         MVC   2(8,R7),=C'DISCOUNT'                                             
         MVC   2(8,R8),=C'----------'                                           
         B     PRNTH50                                                          
*                                                                               
PRNTH50  LA    R6,HEAD10+71                                                     
         LA    R7,HEAD11+71                                                     
         LA    R8,HEAD12+71                                                     
         LA    R4,PROGPROF+3                                                    
         BCT   R3,PRNTH5                                                        
*                                                                               
         MVI   RCSUBPRG,4                                                       
         CLI   PROGPROF,C'I'                                                    
         BNE   PRINT1                                                           
         MVI   RCSUBPRG,6                                                       
PRINT1   CLI   PRDSW,0             SEE IF DOING PRDS SEPARATELY                 
         BE    CKMED                                                            
         CLC   P+1(20),=C' ** CLIENT TOTALS **'                                 
         BE    CKMED                                                            
         CLC   P+1(20),=C' ** REPORT TOTALS **'                                 
         BE    CKMED                                                            
         MVC   HEAD6(7),=C'PRODUCT'                                             
         MVC   HEAD6+9(3),PPRDKPRD                                              
         MVC   HEAD6+13(20),PPRDNAME                                            
CKMED    CLI   QMEDIA,C'N'                                                      
         BE    *+8                                                              
         MVI   RCSUBPRG,2                                                       
CKEST    CLC   QEST,=C'ALL'                                                     
         BE    CKESTX                                                           
         CLC   QEST,=C'   '                                                     
         BE    CKEST2                                                           
*                                                                               
CKEST1   ZIC   R2,RCSUBPRG                                                      
         LA    R2,1(R2)                                                         
         STC   R2,RCSUBPRG         SET SPROG - TO PRINT EST FROM PPG            
         B     CKESTX                                                           
*                                                                               
CKEST2   CLC   QESTEND,=C'   '                                                  
         BE    CKESTX                                                           
         B     CKEST1              PRINT FILTERS                                
*                                                                               
CKESTX   DS    0H                                                               
         CLC   P+2(9),=C'** REPORT'        SEE IF DOING REPORT TOTALS           
         BNE   CKESTX5                                                          
         MVI   RCSUBPRG,20                 HAS NO CLIENT NAME                   
         B     PRINT3                                                           
*                                                                               
CKESTX5  CLC   P+2(9),=C'** CLIENT'        SEE IF DOING CLT TOTS                
         BE    PRINT2                                                           
         CLC   P+1(10),=C'** PRODUCT'        DOING PRD TOTAL LINE               
         BNE   PRINT4                        FOR PRODUCTS SEPARATELY            
PRINT2   MVI   RCSUBPRG,0                                                       
PRINT3   MVC   HEAD10+5(7),=C'BILLING'                                          
         CLI   QBPDATE,C'B'                                                     
         BE    PRINT4                                                           
         MVC   HEAD10+4(9),=C'INSERTION'                                        
*                                                                               
PRINT4   MVC   HEAD8(26),=C'** CASH DISC. PUBS ONLY **'                         
         CLI   QOPT3,C'C'                                                       
         BE    CKPUP                                                            
         MVC   HEAD8(30),=C'** NON CASH DISC. PUBS ONLY **'                     
         CLI   QOPT3,C'N'                                                       
         BE    CKPUP                                                            
         MVC   HEAD8(33),SPACES                                                 
CKPUP    DS    0H                                                               
         CLI   QOPT1,C'U'                                                       
         BE    CKBP                                                             
         DS    0H                                                               
         CLI   QOPT1,C'P'                                                       
         BE    CKBP                                                             
         MVC   HEAD8+75(26),SPACES                                              
CKBP     CLI   QBPDATE,C'B'                                                     
         BE    BILMSG                                                           
         CLI   QBPDATE,C'P'                                                     
         BNE   CKASOF                                                           
         MVC   HEAD4+23(19),=C'** PAYING PERIOD **'                             
         B     CKASOF                                                           
*                                                                               
BILMSG   MVC   HEAD4+23(20),=C'** BILLING PERIOD **'                            
CKASOF   CLI   ASOFDTE,0                                                        
         BE    CHKOFF         AS OF DATE NOT USED                               
         MVC   HEAD5+25(5),=C'AS OF'                                            
         MVC   HEAD5+31(8),ASDATE                                               
*                                                                               
CHKOFF   CLI   QCLIENT,C'$'         OFFICE LIST                                 
         BNE   CHKOFF5                                                          
         MVC   HEAD3(11),=C'OFFICE LIST'                                        
         MVC   HEAD3+12(1),QCLIENT+1                                            
         CLC   P+2(9),=C'** REPORT'        SEE IF DOING REPORT TOTALS           
         BE    PRINTX                                                           
         MVC   HEAD4(6),=C'OFFICE'                                              
*****    MVC   HEAD4+7(1),PCLTOFF                                               
         MVC   HEAD4+9(2),SVPTOFC         SVPTOFC IS PRNTOFC OUTPUT             
         B     PRINTX                                                           
*                                                                               
CHKOFF5  CLI   QCLIENT,C'*'                                                     
         BNE   CHKGRP                                                           
         MVC   HEAD4(6),=C'OFFICE'                                              
*****    MVC   HEAD4+7(1),PCLTOFF                                               
         MVC   HEAD4+9(2),SVPTOFC         SVPTOFC IS PRNTOFC OUTPUT             
         B     PRINTX                                                           
*                                                                               
CHKGRP   CLI   QCLIENT,C'&&'          GROUP                                     
         BNE   PRINTX                                                           
         MVC   HEAD4(5),=C'GROUP'                                               
         MVC   HEAD4+6(1),QCLIENT+1                                             
*                                                                               
PRINTX   CLC   LINE,MAXLINES                                                    
         BL    PRINTX5                                                          
         CLI   PUBPSW,1            SEE IF PUB NAME ALREADY PRINTED              
         BNE   PRINTX5             NO - DONT PRINT CONTINUED MESSAGE            
         MVC   SAVEP,P                                                          
         MVC   SAVPSEC,PSECOND                                                  
         MVC   P,SAVPLIN1                                                       
         MVC   PSECOND,SAVPLIN2                                                 
         GOTO1 REPORT                                                           
         MVC   P,SAVEP                                                          
         MVC   PSECOND,SAVPSEC                                                  
PRINTX5  GOTO1 REPORT                                                           
PRINTXZ  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
MTHEND   CSECT                                                                  
         NMOD1 0,MTHEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VNVWORK                                                       
         LA    R9,4095(R5)                                                      
         LA    R9,1(R9)                                                         
         USING NVWRKD,R5,R9                                                     
         CLI   MTHACT,C'Y'                                                      
         BNE   MTHENDX             NO ACTIVITY                                  
         CLI   QBPDATE,C'B'            IF YES THEN DON'T PRINT MTH              
         BE    MTHEND2C                BREAKS                                   
*                                                                               
*        GOTO1 DTCNV,DMCB,(1,SAVEYMD),(5,P+5)                                   
         GOTO1 DATCON,DMCB,(3,SAVEYMD),(9,P+5)                                  
         MVC   P+11(6),=C'-TOTAL'                                               
         L     R3,MTHINS                                                        
         C     R3,=F'0'                                                         
         BNH   MTHEND1                                                          
         EDIT  (R3),(3,P+28),0                                                  
         MVC   P+32(10),=C'INSERTIONS'                                          
         C     R3,=F'1'                                                         
         BNE   MTHEND1                                                          
         MVI   P+41,C' '                                                        
MTHEND1  CLI   QMEDIA,C'N'                                                      
         BNE   MTHEND2             OMIT LINES FOR MAGS + SUPS                   
         L     R3,MTHLINES                                                      
         LTR   R3,R3                                                            
         BZ    MTHEND2             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   MTHEND1A                                                         
         EDIT  (R3),(9,P+42),2                                                  
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     MTHEND2                                                          
MTHEND1A EDIT  (R3),(6,P+45),0                                                  
         MVI   P+51,C'L'                                                        
         MVI   P+52,C'*'                                                        
MTHEND2  DS    0H                                                               
*****                                                                           
         CLI   PROGPROF+2,C'0'                                                  
         BE    MTHPM                                                            
         CLI   PROGPROF+2,C'N'                                                  
         BNE   MTHPA                                                            
         L     R3,MTHGLAC               GROSS-AC = NET                          
         B     MTHPI                                                            
MTHPA    CLI   PROGPROF+2,C'C'                                                  
         BNE   MTHPB                                                            
         L     R3,MTHCD                                                         
         B     MTHPI                                                            
MTHPB    CLI   PROGPROF+2,C'1'                                                  
         BNE   MTHPC                                                            
         L     R3,MTHGO                                                         
         S     R3,MTHCD                                                         
         B     MTHPI                                                            
MTHPC    CLI   PROGPROF+2,C'2'                                                  
         BNE   MTHPD                                                            
         L     R3,MTHNP                MTHNP = NET-CD                           
         B     MTHPI                                                            
MTHPD    L     R3,MTHGO                PROGPROF+2 MUST BE G GROSS               
*****                                                                           
*        L     R3,MTHGO                                                         
MTHPI    EDIT  (R3),(14,P+53),2,COMMAS=YES,MINUS=YES                            
         LTR   R3,R3                                                            
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
*****                                                                           
MTHPM    CLI   PROGPROF+3,C'0'                                                  
         BE    MTHPS                                                            
         CLI   PROGPROF+3,C'G'                                                  
         BNE   MTHPN                                                            
         L     R3,MTHGO                                                         
         B     MTHPR                                                            
MTHPN    CLI   PROGPROF+3,C'C'                                                  
         BNE   MTHPO                                                            
         L     R3,MTHCD                                                         
         B     MTHPR                                                            
MTHPO    CLI   PROGPROF+3,C'1'                                                  
         BNE   MTHPP                                                            
         L     R3,MTHGO                                                         
         S     R3,MTHCD                                                         
         B     MTHPR                                                            
MTHPP    CLI   PROGPROF+3,C'2'                                                  
         BNE   MTHPQ                                                            
         L     R3,MTHNP                                                         
         B     MTHPR                                                            
MTHPQ    L     R3,MTHGLAC            PROGPROF+3 = N NET                         
*****                                                                           
MTHPR    EDIT  (R3),(14,P+68),2,COMMAS=YES,MINUS=YES                            
         LTR   R3,R3                                                            
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
MTHPS    SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99                                                      
         GOTO1 VPRINTIT                                                         
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
*                                                                               
         CLI   PRDSW,1             SEE IF DOING PRDS SEPARATELY                 
         BE    MTHEND3C                                                         
*    ROLL TO PRD TOTALS                                                         
*                                                                               
MTHEND2C L     R3,PRDMTHS                                                       
         A     R3,=F'1'                                                         
         ST    R3,PRDMTHS                                                       
         LA    R3,PRDINS                                                        
         LA    R4,MTHINS                                                        
         LA    R6,6                                                             
MTHEND3  L     R7,0(R3)                                                         
         A     R7,0(R4)                                                         
         ST    R7,0(R3)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R6,MTHEND3                                                       
         B     MTHEND3X                                                         
*                                                                               
*     ROLL TO PUB ACCUMS                                                        
*                                                                               
MTHEND3C L     R3,PUBMTHS                                                       
         AH    R3,=H'1'                                                         
         ST    R3,PUBMTHS                                                       
*                                                                               
*MTHEND3X GOTO1 DTCNV,DMCB,(1,SAVEYMD),(0,WORK)                                 
MTHEND3X GOTO1 DATCON,DMCB,(3,SAVEYMD),(0,WORK)                                 
         LA    R3,MTHTAB                                                        
         LA    R4,0                                                             
COMPDAT  CLC   WORK(4),0(R3)                                                    
         BE    MTHEND4                                                          
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         CLC   0(4,R3),=4X'00'                                                  
         BNE   COMPDAT                                                          
         DC    H'0'                MTH NOT FOUND IN LIST                        
*                                                                               
MTHEND4  LA    R3,6                                                             
         LA    R6,PUBINS                                                        
         LA    R7,MTHINS                                                        
         LA    R6,0(R4,R6)                                                      
MTHEND5  L     R8,0(R6)                                                         
         A     R8,0(R7)                                                         
         ST    R8,0(R6)                                                         
         LA    R6,ACCNUM*4(R6)                                                  
         LA    R7,4(R7)                                                         
         BCT   R3,MTHEND5                                                       
         XC    MTHTOTS(24),MTHTOTS      CLEAR  MTH ACCUMS                       
         XC    SAVEYMD,SAVEYMD                                                  
         MVI   MTHACT,0                                                         
MTHENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PRDEND   CSECT                                                                  
         NMOD1 0,PRDEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VNVWORK                                                       
         LA    R9,4095(R5)                                                      
         LA    R9,1(R9)                                                         
         USING NVWRKD,R5,R9                                                     
         CLI   PRDACT,C'Y'                                                      
         BNE   PRDENDX                                                          
         GOTO1 VMTHEND                                                          
*                                                                               
         CLI   PRDSW,1             SEE IF DOING PRDS SEPARATELY                 
         BE    PRDEND10                                                         
         L     R3,PUBPRDS                                                       
         A     R3,=F'1'                                                         
         ST    R3,PUBPRDS                                                       
         CLC   PRDMTHS,=F'1'                                                    
         BNH   CLRPRD                                                           
         MVC   P+2(20),=C'** PRODUCT TOTALS **'                                 
         L     R3,PRDINS                                                        
         LTR   R3,R3                                                            
         BZ    NOINS               NO INSERTIONS                                
         EDIT  (R3),(3,P+28),0                                                  
         MVC   P+32(10),=C'INSERTIONS'                                          
         C     R3,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+41,C' '                                                        
NOINS    CLI   QMEDIA,C'N'                                                      
         BNE   PRDEND1                  SKIP LINES                              
         L     R3,PRDLINES                                                      
         LTR   R3,R3                                                            
         BZ    PRDEND1             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   PRDENDA                                                          
         EDIT  (R3),(9,P+42),2                                                  
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     PRDEND1                                                          
PRDENDA  EDIT  (R3),(6,P+45),0                                                  
         MVI   P+51,C'L'                                                        
         MVI   P+52,C'*'                                                        
PRDEND1  DS    0H                                                               
*****                                                                           
         CLI   PROGPROF+2,C'0'                                                  
         BE    PRTPG                                                            
         CLI   PROGPROF+2,C'N'                                                  
         BNE   PRTPA                                                            
         L     R3,PRDGLAC              GROSS-AC = NET                           
         B     PRTPF                                                            
PRTPA    CLI   PROGPROF+2,C'C'                                                  
         BNE   PRTPB                                                            
         L     R3,PRDCD                                                         
         B     PRTPF                                                            
PRTPB    CLI   PROGPROF+2,C'1'                                                  
         BNE   PRTPC                                                            
         L     R3,PRDGO                                                         
         S     R3,PRDCD                                                         
         B     PRTPF                                                            
PRTPC    CLI   PROGPROF+2,C'2'                                                  
         BNE   PRTPD                                                            
         L     R3,PRDNP                PRDNP= NET-CD                            
         B     PRTPF                                                            
PRTPD    L     R3,PRDGO                                                         
*****                                                                           
PRTPF    EDIT  (R3),(14,P+53),2,COMMAS=YES,MINUS=YES                            
         LTR   R3,R3                                                            
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
*****                                                                           
PRTPG    CLI   PROGPROF+3,C'0'                                                  
         BE    PRTPQ                                                            
         CLI   PROGPROF+3,C'N'                                                  
         BNE   PRTPH                                                            
         L     R3,PRDGLAC                                                       
         B     PRTPN                                                            
PRTPH    CLI   PROGPROF+3,C'C'                                                  
         BNE   PRTPJ                                                            
         L     R3,PRDCD                                                         
         B     PRTPN                                                            
PRTPJ    CLI   PROGPROF+3,C'1'                                                  
         BNE   PRTPK                                                            
         L     R3,PRDGO                                                         
         S     R3,PRDCD                                                         
         B     PRTPN                                                            
PRTPK    CLI   PROGPROF+3,C'2'                                                  
         BNE   PRTPM                                                            
         L     R3,PRDNP                                                         
         B     PRTPN                                                            
PRTPM    L     R3,PRDGO         PROGPROF+3 MUST BE G GROSS                      
*****                                                                           
PRTPN    EDIT  (R3),(14,P+68),2,COMMAS=YES,MINUS=YES                            
         LTR   R3,R3                                                            
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
PRTPQ    SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99                                                      
         GOTO1 VPRINTIT                                                         
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
         B     CLRPRD                                                           
*                                                                               
PRDEND10 DS    0H                  PRODUCTS SEPARATELY                          
         GOTO1 VPUBEND                                                          
         MVI   PUBPSW,0            SO I WON'T PRINT PUB CONTINUED MSG           
         MVI   FORCEHED,C'P'     NEW PAGE AND FORCE PAGE                        
         MVC   P+1(21),=C'** PRODUCT TOTALS **'                                 
         GOTO1 VPRINTIT                                                         
         LA    R8,ACCNUM                                                        
         LA    R4,0                                                             
PRDEND15 LA    R2,PROINS                                                        
         LA    R2,0(R4,R2)                                                      
         LA    R7,6                                                             
PRDEND17 CLC   0(4,R2),=F'0'                                                    
         BNE   PACTV                                                            
         LA    R2,ACCNUM*4(R2)                                                  
         BCT   R7,PRDEND17                                                      
PRDEND18 LA    R4,4(R4)                                                         
         BCT   R8,PRDEND15                                                      
         B     PRDEND20                                                         
*                                                                               
PACTV    LA    R1,MTHTAB                                                        
         LA    R1,0(R4,R1)                                                      
         MVC   WORK(4),0(R1)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         LA    R1,PROPUBS                                                       
         LA    R1,0(R4,R1)                                                      
         L     R3,0(R1)                                                         
         EDIT  (R3),(4,P+15),0                                                  
         MVC   P+20(4),=C'PUBS'                                                 
         C     R3,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+23,C' '                                                        
         LA    R1,PROINS                                                        
         LA    R1,0(R4,R1)                                                      
         L     R3,0(R1)                                                         
         LTR   R3,R3                                                            
         BZ    NOPINS                                                           
         EDIT  (R3),(5,P+24),0                                                  
         MVC   P+30(10),=C'INSERTIONS'                                          
         C     R3,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
NOPINS   LA    R1,ACCNUM*4(R1)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   PACTV4                                                           
         L     R3,0(R1)                                                         
         LTR   R3,R3                                                            
         BZ    PACTV4                                                           
         CLI   PROGPROF,C'I'                                                    
         BNE   PACTV3                                                           
         EDIT  (R3),(9,P+42),2                                                  
         MVI   P+51,C'I'                                                        
         B     PACTV4                                                           
PACTV3   EDIT  (R3),(6,P+44),0                                                  
         MVI   P+50,C'L'                                                        
PACTV4   DS    0H                                                               
         BAS   RE,PACTV5                                                        
         B     PRDEND18                                                         
*                                                                               
*                                                                               
*****                                                                           
PACTV5   NTR1                                                                   
         LA    R7,P+53                                                          
         LA    R4,PROGPROF+2                                                    
         LA    R6,2                                                             
         LA    R1,ACCNUM*4(R1)                                                  
PACTV8   LR    R2,R1                                                            
*                                                                               
         CLI   0(R4),C'0'                                                       
         BE    PACTV96                                                          
         CLI   0(R4),C'G'                                                       
         BE    PACTV90                                                          
*                                                                               
         CLI   0(R4),C'N'                                                       
         BNE   PACTV10                                                          
         LA    R2,ACCNUM*4(R2)          POINT TO G-AC = NET                     
         B     PACTV90                                                          
*                                                                               
PACTV10  CLI   0(R4),C'C'                                                       
         BNE   PACTV20                                                          
         LA    R2,ACCNUM*4(R2)                                                  
         LA    R2,ACCNUM*4(R2)                                                  
         B     PACTV90                                                          
*                                                                               
PACTV20  CLI   0(R4),C'1'                                                       
         BNE   PACTV30                                                          
         L     R3,0(R2)                                                         
         LA    R2,ACCNUM*4(R2)                                                  
         LA    R2,ACCNUM*4(R2)                                                  
         L     R0,0(R2)                                                         
         SR    R3,R0                                                            
         B     PACTV91                                                          
*                                                                               
PACTV30  CLI   0(R4),C'2'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,ACCNUM*4(R2)                                                  
         LA    R2,ACCNUM*4(R2)                                                  
         LA    R2,ACCNUM*4(R2)                                                  
         B     PACTV90                                                          
*                                                                               
PACTV90  L     R3,0(R2)                                                         
PACTV91  EDIT  (R3),(14,(R7)),2,COMMAS=YES,MINUS=YES                            
PACTV96  LA    R7,P+68                                                          
         LA    R4,1(R4)                                                         
         BCT   R6,PACTV8                                                        
         GOTO1 VPRINTIT                                                         
         XIT1  1                                                                
*****                                                                           
*                                                                               
*                                                                               
PRDEND20 MVC   P+5(5),=C'TOTAL'                                                 
         L     R2,PTOTPUBS                                                      
         EDIT  (R2),(4,P+15),0                                                  
         MVC   P+20(4),=C'PUBS'                                                 
         C     R2,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+23,C' '                                                        
         L     R2,PROINS                                                        
         LA    R3,ACCNUM-1                                                      
         LA    R4,PROINS+4                                                      
PRDEND22 A     R2,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R3,PRDEND22                                                      
         LTR   R2,R2                                                            
         BZ    NOPTINS                                                          
         EDIT  (R2),(5,P+24),0                                                  
         MVC   P+30(10),=C'INSERTIONS'                                          
         C     R2,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
NOPTINS  DS    0H                                                               
         LA    R6,PROLINES                                                      
         LA    R7,5                                                             
         LA    R8,TOTALS                                                        
PRDEND23 L     R2,0(R6)                                                         
         CVD   R2,DUB                                                           
         LA    R3,ACCNUM-1                                                      
         LA    R4,4(R6)                                                         
PRDEND24 L     R0,0(R4)                                                         
         CVD   R0,DOUBLE                                                        
         AP    DUB,DOUBLE                                                       
         LA    R4,4(R4)                                                         
         BCT   R3,PRDEND24                                                      
         MVC   0(8,R8),DUB         SAVE RESULT IN TOTALS                        
         LA    R8,8(R8)                                                         
         LA    R6,ACCNUM*4(R6)                                                  
         BCT   R7,PRDEND23                                                      
*                                                                               
         LA    R2,TOTALS                                                        
         CLI   QMEDIA,C'N'                                                      
         BNE   PRDEND26                                                         
*                                                                               
         CP    0(8,R2),=P'0'        BE SURE I HAVE SOME                         
         BE    PRDEND26                                                         
*                                                                               
         CLI   PROGPROF,C'I'                                                    
         BNE   PRDEND25                                                         
         EDIT  (P8,0(R2)),(10,P+41),2                                           
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     PRDEND26                                                         
PRDEND25 EDIT  (P8,0(R2)),(7,P+43),0                                            
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
*                                                                               
*****                                                                           
PRDEND26 DS    0H                                                               
         LA    R7,P+53                                                          
         LA    R4,PROGPROF+2                                                    
         LA    R8,2                                                             
         LA    R2,8(R2)                                                         
*                                                                               
PRDT8    LR    R3,R2                                                            
         CLI   0(R4),C'0'                                                       
         BE    PRDT96                                                           
         CLI   0(R4),C'G'                                                       
         BE    PRDT90                                                           
*                                                                               
         CLI   0(R4),C'N'                                                       
         BNE   PRDT10                                                           
         LA    R3,8(R3)               GLAC = NET                                
         B     PRDT90                                                           
*                                                                               
PRDT10   CLI   0(R4),C'C'                                                       
         BNE   PRDT20                                                           
         LA    R3,16(R3)                                                        
         B     PRDT90                                                           
*                                                                               
PRDT20   CLI   0(R4),C'1'                                                       
         BNE   PRDT30                                                           
         ZAP   DUB(8),0(8,R3)                                                   
         ZAP   DOUBLE(8),16(8,R3)                                               
         SP    DUB,DOUBLE                                                       
         LA    R3,DUB                                                           
         B     PRDT90                                                           
*                                                                               
PRDT30   CLI   0(R4),C'2'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,24(R3)                  NET-CD                                
         B     PRDT90                                                           
*                                                                               
PRDT90   EDIT  (P8,00(R3)),(14,(R7)),2,COMMAS=YES,MINUS=YES                     
         CP    00(8,R3),=P'0'                                                   
         BL    *+8                                                              
         MVI   13(R7),C'*'                                                      
PRDT96   LA    R7,P+68                                                          
         LA    R4,1(R4)                                                         
         BCT   R8,PRDT8                                                         
*****                                                                           
         GOTO1 VPRINTIT                                                         
*                                                                               
*                                  IF DOING PRDS SEPERATELY                     
*                                  CLEAR REP/ADR ACCUMS                         
         LA    R3,REPINS                                                        
         LA    R4,7                                                             
PRDEND30 XC    0(ACCNUM*4,R3),0(R3)                                             
         LA    R3,ACCNUM*4(R3)                                                  
         BCT   R4,PRDEND30                                                      
         XC    RTOTPUBS,RTOTPUBS                                                
*                                  CLEAR PRODUCT ACCUMS                         
         LA    R3,PROINS                                                        
         LA    R4,7                                                             
PRDEND32 XC    0(ACCNUM*4,R3),0(R3)                                             
         LA    R3,ACCNUM*4(R3)                                                  
         BCT   R4,PRDEND32                                                      
         XC    PTOTPUBS,PTOTPUBS                                                
         MVI   PUBACT,0                                                         
**                                                                              
*                               IF DOING PRDS WITHIN PUB                        
*                               CLEAR PRDTOTS                                   
CLRPRD   XC    PRDTOTS(24),PRDTOTS                                              
         XC    PRDMTHS,PRDMTHS                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         XC    SAVEYMD,SAVEYMD                                                  
         MVI   MTHACT,0                                                         
         MVI   PRDACT,0                                                         
PRDENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PUBEND   CSECT                                                                  
         NMOD1 0,PUBEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VNVWORK                                                       
         LA    R9,4095(R5)                                                      
         LA    R9,1(R9)                                                         
         USING NVWRKD,R5,R9                                                     
         MVI   PUBSW,0                                                          
         CLI   PUBACT,C'Y'                                                      
         BNE   PUBENDX                  NO ACTIVITY                             
         GOTO1 VMTHEND                                                          
         CLI   PRDSW,1             SEE IF DOING PRDS SEPARATELY                 
         BE    PUBEND3             YES                                          
         GOTO1 VPRDEND                                                          
         CLC   PUBPRDS,=F'1'                                                    
         BNH   PUBROLL                                                          
         B     PUBE10                                                           
*                                                                               
PUBE5    CLC   PUBMTHS,=F'1'                                                    
         BNH   PUBROLL                                                          
PUBE10   SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'3'   NEED 4 LINES                                          
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'P'     NEW PAGE AND FORCE PAGE                        
         GOTO1 VPRINTIT                                                         
         MVC   P+2(16),=C'** PUB TOTALS **'                                     
         GOTO1 VPRINTIT                                                         
         XC    PUBMTHS,PUBMTHS                                                  
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
PUBEND1  LA    R6,PUBINS                                                        
         LA    R6,0(R4,R6)                                                      
         LA    R7,6                                                             
PUBEND2  CLC   0(4,R6),=F'0'                                                    
         BE    *+12                                                             
         BAS   RE,PRTMTH                                                        
         B     *+12                                                             
         LA    R6,ACCNUM*4(R6)                                                  
         BCT   R7,PUBEND2                                                       
         LA    R4,4(R4)                                                         
         BCT   R3,PUBEND1                                                       
         B     PUBEND3                                                          
         EJECT                                                                  
*                                                                               
PRTMTH   NTR1                                                                   
         L     R8,PUBMTHS                                                       
         A     R8,=F'1'                                                         
         ST    R8,PUBMTHS                                                       
         LA    R8,MTHTAB       R4 HAS MTH DISP                                  
         LA    R8,0(R4,R8)                                                      
         MVC   WORK(4),0(R8)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         LA    R2,PUBINS                                                        
         LA    R2,0(R4,R2)                                                      
         CLC   0(4,R2),=F'0'                                                    
         BE    NOPBINS             NO INSERTIONS                                
         EDIT  (4,(R2)),(4,P+25),0                                              
         MVC   P+30(10),=C'INSERTIONS'                                          
         C     R0,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
NOPBINS  LA    R2,ACCNUM*4(R2)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   PRTMTH1                                                          
         CLC   0(4,R2),=F'0'                                                    
         BE    PRTMTH1             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   PRTMTH1A                                                         
         EDIT  (4,(R2)),(9,P+42),2                                              
         MVI   P+51,C'I'                                                        
         B     PRTMTH1                                                          
PRTMTH1A EDIT  (4,(R2)),(6,P+45),0                                              
         MVI   P+51,C'L'                                                        
*****                                                                           
PRTMTH1  DS    0H                                                               
         LA    R7,P+53                                                          
         LA    R4,PROGPROF+2                                                    
         LA    R8,2                                                             
         LA    R2,ACCNUM*4(R2)                                                  
PRTMT8   LR    R1,R2                                                            
*                                                                               
         CLI   0(R4),C'0'                                                       
         BE    PRTMT96                                                          
         CLI   0(R4),C'G'                                                       
         BE    PRTMT90                                                          
*                                                                               
         CLI   0(R4),C'N'                                                       
         BNE   PRTMT10                                                          
         LA    R1,ACCNUM*4(R1)            GLAC=NET                              
         B     PRTMT90                                                          
*                                                                               
PRTMT10  CLI   0(R4),C'C'                                                       
         BNE   PRTMT20                                                          
         LA    R1,ACCNUM*4(R1)                                                  
         LA    R1,ACCNUM*4(R1)                                                  
         B     PRTMT90                                                          
*                                                                               
PRTMT20  CLI   0(R4),C'1'                                                       
         BNE   PRTMT30                                                          
         L     R3,0(R1)                                                         
         LA    R1,ACCNUM*4(R1)                                                  
         LA    R1,ACCNUM*4(R1)                                                  
         L     R6,0(R1)                                                         
         SR    R3,R6                                                            
         B     PRTMT91                                                          
*                                                                               
PRTMT30  CLI   0(R4),C'2'            NET-CD                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,ACCNUM*4(R1)                                                  
         LA    R1,ACCNUM*4(R1)                                                  
         LA    R1,ACCNUM*4(R1)                                                  
         B     PRTMT90                                                          
*                                                                               
PRTMT90  L     R3,0(R1)                                                         
PRTMT91  EDIT  (R3),(14,(R7)),2,COMMAS=YES,MINUS=YES                            
PRTMT96  LA    R7,P+68                                                          
         LA    R4,1(R4)                                                         
         BCT   R8,PRTMT8                                                        
*****                                                                           
*                                                                               
         GOTO1 VPRINTIT                                                         
         XIT1  1              RETURN                                            
         EJECT                                                                  
*                                                                               
PUBEND3  CLC   PUBMTHS,=F'1'                                                    
         BNH   PUBROLL                                                          
         MVC   P+5(5),=C'TOTAL'                                                 
         CLI   PRDSW,0             SEE IF COMBINING PRDS                        
         BE    *+10                YES                                          
         MVC   P+2(15),=C'** PUB TOTAL **'                                      
         L     R3,PUBINS                                                        
         LA    R6,ACCNUM-1                                                      
         LA    R2,PUBINS                                                        
PUBEND4  A     R3,4(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R6,PUBEND4                                                       
         LTR   R3,R3                                                            
         BZ    NOPBTINS            NO INSERTIONS                                
         EDIT  (R3),(5,P+24),0                                                  
         MVC   P+30(10),=C'INSERTIONS'                                          
         C     R3,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
NOPBTINS DS    0H                                                               
         LA    R6,PUBLINES                                                      
         LA    R7,5                                                             
         LA    R8,DMCB                                                          
PUBEND5  L     R2,0(R6)                                                         
         LA    R3,ACCNUM-1                                                      
         LA    R4,4(R6)                                                         
PUBEND6  A     R2,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R3,PUBEND6                                                       
         ST    R2,0(R8)                                                         
         LA    R8,4(R8)                                                         
         LA    R6,ACCNUM*4(R6)                                                  
         BCT   R7,PUBEND5                                                       
*                                                                               
         LA    R2,DMCB                                                          
         CLI   QMEDIA,C'N'                                                      
         BNE   PUBEND7                                                          
*                                                                               
         OC    0(4,R2),0(R2)      BE SURE I HAVE SOME                           
         BZ    PUBEND7                                                          
*                                                                               
         CLI   PROGPROF,C'I'                                                    
         BNE   PUBEND6A                                                         
         EDIT  (B4,0(R2)),(9,P+42),2                                            
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     PUBEND7                                                          
PUBEND6A EDIT  (B4,0(R2)),(6,P+45),0                                            
         MVI   P+51,C'L'                                                        
         MVI   P+52,C'*'                                                        
*                                                                               
*****                                                                           
PUBEND7  DS    0H                                                               
         LA    R7,P+53                                                          
         LA    R4,PROGPROF+2                                                    
         LA    R8,2                                                             
         LA    R2,4(R2)                                                         
*                                                                               
PUBT8    LR    R3,R2                                                            
         CLI   0(R4),C'0'                                                       
         BE    PUBT96                                                           
         CLI   0(R4),C'G'                                                       
         BE    PUBT90                                                           
*                                                                               
         CLI   0(R4),C'N'                                                       
         BNE   PUBT10                                                           
         LA    R3,4(R3)             GLAC=NET                                    
         B     PUBT90                                                           
*                                                                               
PUBT10   CLI   0(R4),C'C'                                                       
         BNE   PUBT20                                                           
         LA    R3,8(R3)                                                         
         B     PUBT90                                                           
*                                                                               
PUBT20   CLI   0(R4),C'1'                                                       
         BNE   PUBT30                                                           
         L     R6,0(R3)                                                         
         L     R0,8(R3)                                                         
         SR    R6,R0                                                            
         B     PUBT91                                                           
*                                                                               
PUBT30   CLI   0(R4),C'2'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,12(R3)             NET-CD                                     
         B     PUBT90                                                           
*                                                                               
PUBT90   L     R6,0(R3)                                                         
PUBT91   EDIT  (R6),(14,(R7)),2,COMMAS=YES,MINUS=YES                            
         LTR   R6,R6        '                                                   
         BL    *+8                                                              
         MVI   13(R7),C'*'                                                      
PUBT96   LA    R7,P+68                                                          
         LA    R4,1(R4)                                                         
         BCT   R8,PUBT8                                                         
*****                                                                           
         SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99      ALLWAYS PRINT TOTAL                             
         GOTO1 VPRINTIT                                                         
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
*                                                                               
*     ROLL TO CLT TOTALS                                                        
*                                                                               
PUBROLL  LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
PUBENDA  LA    R6,PUBINS                                                        
         LA    R6,0(R4,R6)                                                      
         LA    R7,6                                                             
PUBENDB  CLC   0(4,R6),=F'0'                                                    
         BE    *+12                                                             
         BAS   R8,BUMPPUB                                                       
         B     *+12                                                             
         LA    R6,ACCNUM*4(R6)                                                  
         BCT   R7,PUBENDB                                                       
         LA    R4,4(R4)                                                         
         BCT   R3,PUBENDA                                                       
         B     PUBENDC                                                          
*                                                                               
BUMPPUB  LA    R2,CLTPUBS    CLT/MTH PUB  TOTALS                                
         LA    R2,0(R4,R2)                                                      
         L     R0,0(R2)                                                         
         A     R0,=F'1'                                                         
         ST    R0,0(R2)                                                         
         LA    R2,REQPUBS    REQ/MTH PUB  TOTALS                                
         LA    R2,0(R4,R2)                                                      
         L     R0,0(R2)                                                         
         A     R0,=F'1'                                                         
         ST    R0,0(R2)                                                         
         CLI   PRDSW,0             SEE IF COMBINING PRDS                        
         BER   R8                                                               
         LA    R2,PROPUBS          PRD/MTH PUB TOTS                             
         LA    R2,0(R4,R2)                                                      
         L     R0,0(R2)                                                         
         A     R0,=F'1'                                                         
         ST    R0,0(R2)                                                         
         BR    R8                                                               
*                                                                               
PUBENDC  LA    R2,ACCNUM*6                                                      
         LA    R3,CLTINS                                                        
         LA    R4,PUBINS                                                        
PUBEND11 L     R8,0(R3)                                                         
         A     R8,0(R4)                                                         
         ST    R8,0(R3)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R2,PUBEND11                                                      
         L     R8,CTOTPUBS         ADD 1 TO PUB ACCUM                           
         AH    R8,=H'1'                                                         
         ST    R8,CTOTPUBS                                                      
         LA    R2,ACCNUM*6                                                      
         LA    R3,REPINS                                                        
         LA    R4,PUBINS                                                        
PUBENDD  L     R8,0(R3)                                                         
         A     R8,0(R4)                                                         
         ST    R8,0(R3)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R2,PUBENDD                                                       
**                                                                              
         CLI   ONECLI,C'Y'                                                      
         BE    PUBENDEX                                                         
PUBENDE  LA    R2,ACCNUM*6                                                      
         LA    R3,REQINS                                                        
         LA    R4,PUBINS                                                        
PUBENDE1 L     R8,0(R3)                                                         
         A     R8,0(R4)                                                         
         ST    R8,0(R3)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R2,PUBENDE1                                                      
         L     R8,RQTOTPUB         ADD 1 TO PUB ACCUM                           
         AH    R8,=H'1'                                                         
         ST    R8,RQTOTPUB                                                      
**                                                                              
PUBENDEX CLI   PRDSW,0             SEE IF COMBINING PRDS                        
         BE    PUBEND15                                                         
*                                                                               
         L     R8,PTOTPUBS                                                      
         AH    R8,=H'1'                                                         
         ST    R8,PTOTPUBS                                                      
PUBEND12 LA    R2,ACCNUM*6                                                      
         LA    R3,PROINS                                                        
         LA    R4,PUBINS                                                        
PUBEND13 L     R8,0(R3)                                                         
         A     R8,0(R4)                                                         
         ST    R8,0(R3)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R2,PUBEND13                                                      
*                                                                               
*                                  CLEAR  PUB ACCUMS                            
PUBEND15 LA    R6,6                                                             
         LA    R7,PUBINS                                                        
PUBEND17 XC    0(ACCNUM*4,R7),0(R7)                                             
         LA    R7,ACCNUM*4(R7)                                                  
         BCT   R6,PUBEND17                                                      
         XC    PUBPRDS,PUBPRDS                                                  
         XC    PUBMTHS,PUBMTHS                                                  
         XC    SAVEPUB,SAVEPUB                                                  
         XC    SAVEYMD,SAVEYMD                                                  
         MVI   PUBACT,0                                                         
         MVI   MTHACT,0                                                         
         CLI   PRDSW,1             SEE IF DOING PRDS SEPARATELY                 
         BE    PUBENDX             YES                                          
         XC    PRDMTHS,PRDMTHS                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         MVI   PRDACT,0                                                         
PUBENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
REPEND   CSECT                                                                  
         NMOD1 0,REPEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VNVWORK                                                       
         LA    R9,4095(R5)                                                      
         LA    R9,1(R9)                                                         
         USING NVWRKD,R5,R9                                                     
         GOTO1 VMTHEND                                                          
*                                                                               
REPE10   SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'3'   NEED 4 LINES                                          
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'P'     NEW PAGE AND FORCE PAGE                        
         GOTO1 VPRINTIT                                                         
         LA    R7,PPFILED+4095                                                  
         LA    R7,1(R7)                                                         
         USING PPFILED+4096,R7                                                  
*                                                                               
         MVI   PUBPSW,0         SO PUB CONTINUED MESSAGE WON'T                  
*                               PRINT IN REP TOTALS                             
*                                                                               
         CLI   PREPKEY+3,X'11'                                                  
         BNE   REPE15                                                           
         MVC   P+2(16),=C'** REP TOTALS **'                                     
         B     REPE20                                                           
REPE15   CLC   PREPNAME,SPACES                                                  
         BE    REPEND15                     GO TO CLEAR TOTALS                  
         MVC   P+2(16),=C'** ADR TOTALS **'                                     
REPE20   GOTO1 VPRINTIT                                                         
         XC    PUBMTHS,PUBMTHS                                                  
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
REPEND1  LA    R6,REPINS                                                        
         LA    R6,0(R4,R6)                                                      
         LA    R7,6                                                             
REPEND2  CLC   0(4,R6),=F'0'                                                    
         BE    *+12                                                             
         BAS   RE,REPMTH                                                        
         B     *+12                                                             
         LA    R6,ACCNUM*4(R6)                                                  
         BCT   R7,REPEND2                                                       
         LA    R4,4(R4)                                                         
         BCT   R3,REPEND1                                                       
         B     REPEND3                                                          
         EJECT                                                                  
*                                                                               
REPMTH   NTR1                                                                   
         L     R8,PUBMTHS                                                       
         A     R8,=F'1'                                                         
         ST    R8,PUBMTHS                                                       
         LA    R8,MTHTAB       R4 HAS MTH DISP                                  
         LA    R8,0(R4,R8)                                                      
         MVC   WORK(4),0(R8)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         LA    R2,REPINS                                                        
         LA    R2,0(R4,R2)                                                      
         CLC   0(4,R2),=F'0'                                                    
         BE    XNOPBINS             NO INSERTIONS                               
         EDIT  (4,(R2)),(4,P+25),0                                              
         MVC   P+30(10),=C'INSERTIONS'                                          
         C     R0,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
XNOPBINS LA    R2,ACCNUM*4(R2)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   REPMTH1                                                          
         CLC   0(4,R2),=F'0'                                                    
         BE    REPMTH1             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   REPMTH1A                                                         
         EDIT  (4,(R2)),(9,P+42),2                                              
         MVI   P+51,C'I'                                                        
         B     REPMTH1                                                          
REPMTH1A EDIT  (4,(R2)),(6,P+45),0                                              
         MVI   P+51,C'L'                                                        
*****                                                                           
REPMTH1  DS    0H                                                               
         LA    R7,P+53                                                          
         LA    R4,PROGPROF+2                                                    
         LA    R8,2                                                             
         LA    R2,ACCNUM*4(R2)                                                  
REPMT8   LR    R1,R2                                                            
*                                                                               
         CLI   0(R4),C'0'                                                       
         BE    REPMT96                                                          
         CLI   0(R4),C'G'                                                       
         BE    REPMT90                                                          
*                                                                               
         CLI   0(R4),C'N'                                                       
         BNE   REPMT10                                                          
         LA    R1,ACCNUM*4(R1)           GLAC=NET                               
         B     REPMT90                                                          
*                                                                               
REPMT10  CLI   0(R4),C'C'                                                       
         BNE   REPMT20                                                          
         LA    R1,ACCNUM*4(R1)                                                  
         LA    R1,ACCNUM*4(R1)                                                  
         B     REPMT90                                                          
*                                                                               
REPMT20  CLI   0(R4),C'1'                                                       
         BNE   REPMT30                                                          
         L     R3,0(R1)                                                         
         LA    R1,ACCNUM*4(R1)                                                  
         LA    R1,ACCNUM*4(R1)                                                  
         L     R6,0(R1)                                                         
         SR    R3,R6                                                            
         B     REPMT91                                                          
*                                                                               
REPMT30  CLI   0(R4),C'2'                   NET-CD                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,ACCNUM*4(R1)                                                  
         LA    R1,ACCNUM*4(R1)                                                  
         LA    R1,ACCNUM*4(R1)                                                  
         B     REPMT90                                                          
*                                                                               
REPMT90  L     R3,0(R1)                                                         
REPMT91  EDIT  (R3),(14,(R7)),2,COMMAS=YES,MINUS=YES                            
REPMT96  LA    R7,P+68                                                          
         LA    R4,1(R4)                                                         
         BCT   R8,REPMT8                                                        
*****                                                                           
*                                                                               
         GOTO1 VPRINTIT                                                         
         XIT1  1              RETURN                                            
         EJECT                                                                  
*                                                                               
REPEND3  MVC   P+5(5),=C'TOTAL'                                                 
         L     R3,REPINS                                                        
         LA    R6,ACCNUM-1                                                      
         LA    R2,REPINS                                                        
REPEND4  A     R3,4(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R6,REPEND4                                                       
         LTR   R3,R3                                                            
         BZ    NOPBTINX            NO INSERTIONS                                
         EDIT  (R3),(5,P+24),0                                                  
         MVC   P+30(10),=C'INSERTIONS'                                          
         C     R3,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
NOPBTINX DS    0H                                                               
         LA    R6,REPLINES                                                      
         LA    R7,5                                                             
         LA    R8,DMCB                                                          
REPEND5  L     R2,0(R6)                                                         
         LA    R3,ACCNUM-1                                                      
         LA    R4,4(R6)                                                         
REPEND6  A     R2,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R3,REPEND6                                                       
         ST    R2,0(R8)                                                         
         LA    R8,4(R8)                                                         
         LA    R6,ACCNUM*4(R6)                                                  
         BCT   R7,REPEND5                                                       
*                                                                               
         LA    R2,DMCB                                                          
         CLI   QMEDIA,C'N'                                                      
         BNE   REPEND7                                                          
*                                                                               
         OC    0(4,R2),0(R2)     BE SURE I HAVE SOME                            
         BZ    REPEND7                                                          
*                                                                               
         CLI   PROGPROF,C'I'                                                    
         BNE   REPEND6A                                                         
         EDIT  (B4,0(R2)),(9,P+42),2                                            
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     REPEND7                                                          
REPEND6A EDIT  (B4,0(R2)),(6,P+45),0                                            
         MVI   P+51,C'L'                                                        
         MVI   P+52,C'*'                                                        
*                                                                               
*****                                                                           
REPEND7  DS    0H                                                               
         LA    R7,P+53                                                          
         LA    R4,PROGPROF+2                                                    
         LA    R8,2                                                             
         LA    R2,4(R2)                                                         
*                                                                               
REPT8    LR    R3,R2                                                            
         CLI   0(R4),C'0'                                                       
         BE    REPT96                                                           
         CLI   0(R4),C'G'                                                       
         BE    REPT90                                                           
*                                                                               
         CLI   0(R4),C'N'                                                       
         BNE   REPT10                                                           
         LA    R3,4(R3)              GLAC=NET                                   
         B     REPT90                                                           
*                                                                               
REPT10   CLI   0(R4),C'C'                                                       
         BNE   REPT20                                                           
         LA    R3,8(R3)                                                         
         B     REPT90                                                           
*                                                                               
REPT20   CLI   0(R4),C'1'                                                       
         BNE   REPT30                                                           
         L     R6,0(R3)                                                         
         L     R0,8(R3)                                                         
         SR    R6,R0                                                            
         B     REPT91                                                           
*                                                                               
REPT30   CLI   0(R4),C'2'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,12(R3)                NET-CD                                  
         B     REPT90                                                           
*                                                                               
REPT90   L     R6,0(R3)                                                         
REPT91   EDIT  (R6),(14,(R7)),2,COMMAS=YES,MINUS=YES                            
         LTR   R6,R6        '                                                   
         BL    *+8                                                              
         MVI   13(R7),C'*'                                                      
REPT96   LA    R7,P+68                                                          
         LA    R4,1(R4)                                                         
         BCT   R8,REPT8                                                         
*****                                                                           
         SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99      ALLWAYS PRINT TOTAL                             
         GOTO1 VPRINTIT                                                         
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
*                                                                               
*                                                                               
REPROLL  L     R8,RTOTPUBS                                                      
         AH    R8,=H'1'                                                         
         ST    R8,RTOTPUBS                                                      
*                                                                               
*                                  CLEAR  REP ACCUMS                            
REPEND15 LA    R6,6                                                             
         LA    R7,REPINS                                                        
REPEND17 XC    0(ACCNUM*4,R7),0(R7)                                             
         LA    R7,ACCNUM*4(R7)                                                  
         BCT   R6,REPEND17                                                      
         XC    PUBPRDS,PUBPRDS                                                  
         XC    PUBMTHS,PUBMTHS                                                  
         XC    SAVEPUB,SAVEPUB                                                  
         XC    SAVEYMD,SAVEYMD                                                  
         MVI   PUBACT,0                                                         
         MVI   MTHACT,0                                                         
         XC    PRDMTHS,PRDMTHS                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         MVI   PRDACT,C'Y'                                                      
*                                                                               
REPENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
CLTEND   CSECT                                                                  
         NMOD1 0,CLTEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VNVWORK                                                       
         LA    R9,4095(R5)                                                      
         LA    R9,1(R9)                                                         
         USING NVWRKD,R5,R9                                                     
*                                                                               
         MVC   FAXOPT,SVFAXOPT  RESTORE "REAL' FAXOPT SAVED AT FBUYCLI          
*                                                                               
         CLI   CLTACT,C'Y'                                                      
         BNE   CLTENDX                                                          
         GOTO1 VMTHEND                                                          
         CLI   PRDSW,0             SEE IF COMBINING PRDS                        
         BE    CLTE5               YES                                          
         GOTO1 VPUBEND                                                          
         GOTO1 VPRDEND                                                          
         B     CLTE10                                                           
*                                                                               
CLTE5    DS    0H                                                               
         GOTO1 VPRDEND                                                          
         GOTO1 VPUBEND                                                          
*                                                                               
CLTE10   CLI   PRDSW,0             SEE IF COMBINING PRDS                        
         BE    CLTE15              YES                                          
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    CLTE15                                                           
         B     CLTEN12X            ONE PRD - SKIP CLTTOTALS                     
CLTE15   MVI   FORCEHED,C'P'     NEW PAGE AND FORCE PAGE                        
         MVI   PUBPSW,0            SO I WON'T PRINT PUB CONTINUED MSG           
         MVC   P+1(20),=C' ** CLIENT TOTALS **'                                 
         GOTO1 VPRINTIT                                                         
         LA    R8,ACCNUM                                                        
         LA    R4,0                                                             
CLTEND1  LA    R2,CLTINS                                                        
         LA    R2,0(R4,R2)                                                      
         LA    R7,6                                                             
CLTEND2  CLC   0(4,R2),=F'0'                                                    
         BNE   ACTIVITY                                                         
         LA    R2,ACCNUM*4(R2)                                                  
         BCT   R7,CLTEND2                                                       
CLTEND3  LA    R4,4(R4)                                                         
         BCT   R8,CLTEND1                                                       
         B     CLTEND5        GO TO TOTALS                                      
*                                                                               
ACTIVITY LA    R1,MTHTAB                                                        
         LA    R1,0(R4,R1)                                                      
         MVC   WORK(4),0(R1)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         CLI   PRDSW,1             SEE IF DOING PRDS SEPARATELY                 
         BE    ACTV5                                                            
         LA    R1,CLTPUBS                                                       
         LA    R1,0(R4,R1)                                                      
         L     R3,0(R1)                                                         
         EDIT  (R3),(4,P+15),0                                                  
         MVC   P+20(4),=C'PUBS'                                                 
         C     R3,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+23,C' '                                                        
ACTV5    LA    R1,CLTINS                                                        
         LA    R1,0(R4,R1)                                                      
         L     R3,0(R1)                                                         
         LTR   R3,R3                                                            
         BZ    NOCINS              NO INSERTIONS                                
         EDIT  (R3),(5,P+24),0                                                  
         MVC   P+30(10),=C'INSERTIONS'                                          
         C     R3,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
NOCINS   LA    R1,ACCNUM*4(R1)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   ACTIV0                                                           
         L     R3,0(R1)                                                         
         LTR   R3,R3                                                            
         BZ    ACTIV0              NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   ACTV6                                                            
         EDIT  (R3),(9,P+42),2                                                  
         MVI   P+51,C'I'                                                        
         B     ACTIV0                                                           
ACTV6    EDIT  (R3),(6,P+44),0                                                  
         MVI   P+50,C'L'                                                        
ACTIV0   DS    0H                                                               
         BAS   RE,ACTIV1                                                        
         B     CLTEND3                                                          
*                                                                               
*                                                                               
*****                                                                           
ACTIV1   NTR1                                                                   
         LA    R7,P+53                                                          
         LA    R4,PROGPROF+2                                                    
         LA    R6,2                                                             
         LA    R1,ACCNUM*4(R1)                                                  
ACTIV8   LR    R2,R1                                                            
*                                                                               
         CLI   0(R4),C'0'                                                       
         BE    ACTIV96                                                          
         CLI   0(R4),C'G'                                                       
         BE    ACTIV90                                                          
*                                                                               
         CLI   0(R4),C'N'                                                       
         BNE   ACTIV10                                                          
         LA    R2,ACCNUM*4(R2)                                                  
         B     ACTIV90                                                          
*                                                                               
ACTIV10  CLI   0(R4),C'C'                                                       
         BNE   ACTIV20                                                          
         LA    R2,ACCNUM*4(R2)                                                  
         LA    R2,ACCNUM*4(R2)                                                  
         B     ACTIV90                                                          
*                                                                               
ACTIV20  CLI   0(R4),C'1'                                                       
         BNE   ACTIV30                                                          
         L     R3,0(R2)                                                         
         LA    R2,ACCNUM*4(R2)                                                  
         LA    R2,ACCNUM*4(R2)                                                  
         L     R0,0(R2)                                                         
         SR    R3,R0                                                            
         B     ACTIV91                                                          
*                                                                               
ACTIV30  CLI   0(R4),C'2'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,ACCNUM*4(R2)         NET-CD                                   
         LA    R2,ACCNUM*4(R2)                                                  
         LA    R2,ACCNUM*4(R2)                                                  
         B     ACTIV90                                                          
*                                                                               
ACTIV90  L     R3,0(R2)                                                         
ACTIV91  EDIT  (R3),(14,(R7)),2,COMMAS=YES,MINUS=YES                            
ACTIV96  LA    R7,P+68                                                          
         LA    R4,1(R4)                                                         
         BCT   R6,ACTIV8                                                        
         GOTO1 VPRINTIT                                                         
         XIT1  1                                                                
*****                                                                           
*                                                                               
*                                                                               
CLTEND5  MVC   P+5(5),=C'TOTAL'                                                 
         CLI   PRDSW,1             SEE IF DOING PRDS SEPARATELY                 
         BE    CLTEND5C            YES                                          
         L     R2,CTOTPUBS                                                      
         EDIT  (R2),(4,P+15),0                                                  
         MVC   P+20(4),=C'PUBS'                                                 
         C     R2,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+23,C' '                                                        
CLTEND5C L     R2,CLTINS                                                        
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTINS+4                                                      
CLTEND6  A     R2,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R3,CLTEND6                                                       
         LTR   R2,R2                                                            
         BZ    NOCTINS             NO INSERTIONS                                
         EDIT  (R2),(5,P+24),0                                                  
         MVC   P+30(10),=C'INSERTIONS'                                          
         C     R2,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
NOCTINS  DS    0H                                                               
         LA    R6,CLTLINES                                                      
         LA    R7,5                                                             
         LA    R8,TOTALS                                                        
CLTEND23 L     R2,0(R6)                                                         
         CVD   R2,DUB                                                           
         LA    R3,ACCNUM-1                                                      
         LA    R4,4(R6)                                                         
CLTEND24 L     R0,0(R4)                                                         
         CVD   R0,DOUBLE                                                        
         AP    DUB,DOUBLE                                                       
         LA    R4,4(R4)                                                         
         BCT   R3,CLTEND24                                                      
         MVC   0(8,R8),DUB         SAVE RESULT IN TOTALS                        
         LA    R8,8(R8)                                                         
         LA    R6,ACCNUM*4(R6)                                                  
         BCT   R7,CLTEND23                                                      
*                                                                               
         LA    R2,TOTALS                                                        
         CLI   QMEDIA,C'N'                                                      
         BNE   CLTEND26                                                         
*                                                                               
         CP    0(8,R2),=P'0'      BE SURE I HAVE SOME                           
         BE    CLTEND26                                                         
*                                                                               
         CLI   PROGPROF,C'I'                                                    
         BNE   CLTEND25                                                         
         EDIT  (P8,0(R2)),(10,P+41),2                                           
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     CLTEND26                                                         
CLTEND25 EDIT  (P8,0(R2)),(7,P+43),0                                            
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
*                                                                               
*****                                                                           
CLTEND26 DS    0H                                                               
         LA    R7,P+53                                                          
         LA    R4,PROGPROF+2                                                    
         LA    R8,2                                                             
         LA    R2,8(R2)                                                         
*                                                                               
CLTT8    LR    R3,R2                                                            
         CLI   0(R4),C'0'                                                       
         BE    CLTT96                                                           
         CLI   0(R4),C'G'                                                       
         BE    CLTT90                                                           
*                                                                               
         CLI   0(R4),C'N'                                                       
         BNE   CLTT10                                                           
         LA    R3,8(R3)          GLAC=NET                                       
         B     CLTT90                                                           
*                                                                               
CLTT10   CLI   0(R4),C'C'                                                       
         BNE   CLTT20                                                           
         LA    R3,16(R3)                                                        
         B     CLTT90                                                           
*                                                                               
CLTT20   CLI   0(R4),C'1'                                                       
         BNE   CLTT30                                                           
         ZAP   DUB(8),0(8,R3)                                                   
         ZAP   DOUBLE(8),16(8,R3)                                               
         SP    DUB,DOUBLE                                                       
         LA    R3,DUB                                                           
         B     CLTT90                                                           
*                                                                               
CLTT30   CLI   0(R4),C'2'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,24(R3)             NET-CD                                     
         B     CLTT90                                                           
*                                                                               
CLTT90   EDIT  (P8,00(R3)),(14,(R7)),2,COMMAS=YES,MINUS=YES                     
         CP    00(8,R3),=P'0'                                                   
         BL    *+8                                                              
         MVI   13(R7),C'*'                                                      
CLTT96   LA    R7,P+68                                                          
         LA    R4,1(R4)                                                         
         BCT   R8,CLTT8                                                         
*****                                                                           
         GOTO1 VPRINTIT                                                         
*                                                                               
CLTEN12X XC    MTHACT(4),MTHACT                                                 
         MVI   REPACT,0                                                         
         XC    PRDMTHS,PRDMTHS                                                  
         XC    PUBMTHS,PUBMTHS                                                  
         XC    PUBPRDS,PUBPRDS                                                  
         LA    R3,CLTINS                                                        
         LA    R4,7                                                             
CLTEND13 XC    0(ACCNUM*4,R3),0(R3)                                             
         LA    R3,ACCNUM*4(R3)                                                  
         BCT   R4,CLTEND13                                                      
         XC    CTOTPUBS,CTOTPUBS                                                
         LA    R3,REPINS              ALSO CLEAR REP TOTALS                     
         LA    R4,7                                                             
CLTEND16 XC    0(ACCNUM*4,R3),0(R3)                                             
         LA    R3,ACCNUM*4(R3)                                                  
         BCT   R4,CLTEND16                                                      
         XC    RTOTPUBS,RTOTPUBS                                                
*                                                                               
CLTENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
REQEND   CSECT                                                                  
         NMOD1 0,REQEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VNVWORK                                                       
         LA    R9,4095(R5)                                                      
         LA    R9,1(R9)                                                         
         USING NVWRKD,R5,R9                                                     
         CLI   REQACT,C'Y'                                                      
         BNE   REQENDX                                                          
         CLI   ONECLI,C'Y'           ONE CLIENT NO REQ TOTALS                   
         BE    REQEN12X                                                         
REQE15   MVI   FORCEHED,C'P'     NEW PAGE AND FORCE PAGE                        
         MVI   PUBPSW,0            SO I WON'T PRINT PUB CONTINUED MSG           
         MVI   PCLTREC,0           NO MEDIA NAME OVERRIDE FOR TOTALS            
         MVC   P+1(20),=C' ** REPORT TOTALS **'                                 
         GOTO1 VPRINTIT                                                         
         LA    R8,ACCNUM                                                        
         LA    R4,0                                                             
REQEND1  LA    R2,REQINS                                                        
         LA    R2,0(R4,R2)                                                      
         LA    R7,6                                                             
REQEND2  CLC   0(4,R2),=F'0'                                                    
         BNE   REQIVITY                                                         
         LA    R2,ACCNUM*4(R2)                                                  
         BCT   R7,REQEND2                                                       
REQEND3  LA    R4,4(R4)                                                         
         BCT   R8,REQEND1                                                       
         B     REQEND5        GO TO TOTALS                                      
*                                                                               
REQIVITY LA    R1,MTHTAB                                                        
         LA    R1,0(R4,R1)                                                      
         MVC   WORK(4),0(R1)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         CLI   PRDSW,1             SEE IF DOING PRDS SEPARATELY                 
         BE    REQV5                                                            
         LA    R1,REQPUBS                                                       
         LA    R1,0(R4,R1)                                                      
         L     R3,0(R1)                                                         
         EDIT  (R3),(4,P+15),0                                                  
         MVC   P+20(4),=C'PUBS'                                                 
         C     R3,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+23,C' '                                                        
REQV5    LA    R1,REQINS                                                        
         LA    R1,0(R4,R1)                                                      
         L     R3,0(R1)                                                         
         LTR   R3,R3                                                            
         BZ    NORINS              NO INSERTIONS                                
         EDIT  (R3),(5,P+24),0                                                  
         MVC   P+30(10),=C'INSERTIONS'                                          
         C     R3,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
NORINS   LA    R1,ACCNUM*4(R1)                                                  
REQIV0   DS    0H                                                               
         BAS   RE,REQIV1                                                        
         B     REQEND3                                                          
*                                                                               
*                                                                               
*****                                                                           
REQIV1   NTR1                                                                   
         LA    R7,P+53                                                          
         LA    R4,PROGPROF+2                                                    
         LA    R6,2                                                             
         LA    R1,ACCNUM*4(R1)                                                  
REQIV8   LR    R2,R1                                                            
*                                                                               
         CLI   0(R4),C'0'                                                       
         BE    REQIV96                                                          
         CLI   0(R4),C'G'                                                       
         BE    REQIV90                                                          
*                                                                               
         CLI   0(R4),C'N'                                                       
         BNE   REQIV10                                                          
         LA    R2,ACCNUM*4(R2)         GLAC=NET                                 
         B     REQIV90                                                          
*                                                                               
REQIV10  CLI   0(R4),C'C'                                                       
         BNE   REQIV20                                                          
         LA    R2,ACCNUM*4(R2)                                                  
         LA    R2,ACCNUM*4(R2)                                                  
         B     REQIV90                                                          
*                                                                               
REQIV20  CLI   0(R4),C'1'                                                       
         BNE   REQIV30                                                          
         L     R3,0(R2)                                                         
         LA    R2,ACCNUM*4(R2)                                                  
         LA    R2,ACCNUM*4(R2)                                                  
         L     R0,0(R2)                                                         
         SR    R3,R0                                                            
         B     REQIV91                                                          
*                                                                               
REQIV30  CLI   0(R4),C'2'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,ACCNUM*4(R2)         NET-CD                                   
         LA    R2,ACCNUM*4(R2)                                                  
         LA    R2,ACCNUM*4(R2)                                                  
         B     REQIV90                                                          
*                                                                               
REQIV90  L     R3,0(R2)                                                         
REQIV91  EDIT  (R3),(14,(R7)),2,COMMAS=YES,MINUS=YES                            
REQIV96  LA    R7,P+68                                                          
         LA    R4,1(R4)                                                         
         BCT   R6,REQIV8                                                        
         GOTO1 VPRINTIT                                                         
         XIT1  1                                                                
*****                                                                           
*                                                                               
*                                                                               
REQEND5  MVC   P+5(5),=C'TOTAL'                                                 
         CLI   PRDSW,1             SEE IF DOING PRDS SEPARATELY                 
         BE    REQEND5C            YES                                          
         L     R2,RQTOTPUB                                                      
         EDIT  (R2),(4,P+15),0                                                  
         MVC   P+20(4),=C'PUBS'                                                 
         C     R2,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+23,C' '                                                        
REQEND5C L     R2,REQINS                                                        
         LA    R3,ACCNUM-1                                                      
         LA    R4,REQINS+4                                                      
REQEND6  A     R2,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R3,REQEND6                                                       
         LTR   R2,R2                                                            
         BZ    NORQINS             NO INSERTIONS                                
         EDIT  (R2),(5,P+24),0                                                  
         MVC   P+30(10),=C'INSERTIONS'                                          
         C     R2,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
NORQINS  DS    0H                                                               
         LA    R6,REQLINES                                                      
         LA    R7,5                                                             
         LA    R8,TOTALS                                                        
REQEND23 L     R2,0(R6)                                                         
         CVD   R2,DUB                                                           
         LA    R3,ACCNUM-1                                                      
         LA    R4,4(R6)                                                         
REQEND24 L     R0,0(R4)                                                         
         CVD   R0,DOUBLE                                                        
         AP    DUB,DOUBLE                                                       
         LA    R4,4(R4)                                                         
         BCT   R3,REQEND24                                                      
         MVC   0(8,R8),DUB         SAVE RESULT IN TOTALS                        
         LA    R8,8(R8)                                                         
         LA    R6,ACCNUM*4(R6)                                                  
         BCT   R7,REQEND23                                                      
*                                                                               
         LA    R2,TOTALS                                                        
*                               NOTE - CAN'T SHOW LINES OR INCHES               
*                                      SINCE ACCUM MAY BE A MIXTURE             
*                                                                               
*****                                                                           
REQEND26 DS    0H                                                               
         LA    R7,P+53                                                          
         LA    R4,PROGPROF+2                                                    
         LA    R8,2                                                             
         LA    R2,8(R2)                                                         
*                                                                               
REQT8    LR    R3,R2                                                            
         CLI   0(R4),C'0'                                                       
         BE    REQT96                                                           
         CLI   0(R4),C'G'                                                       
         BE    REQT90                                                           
*                                                                               
         CLI   0(R4),C'N'                                                       
         BNE   REQT10                                                           
         LA    R3,8(R3)              GLAC=NET                                   
         B     REQT90                                                           
*                                                                               
REQT10   CLI   0(R4),C'C'                                                       
         BNE   REQT20                                                           
         LA    R3,16(R3)                                                        
         B     REQT90                                                           
*                                                                               
REQT20   CLI   0(R4),C'1'                                                       
         BNE   REQT30                                                           
         ZAP   DUB(8),0(8,R3)                                                   
         ZAP   DOUBLE(8),16(8,R3)                                               
         SP    DUB,DOUBLE                                                       
         LA    R3,DUB                                                           
         B     REQT90                                                           
*                                                                               
REQT30   CLI   0(R4),C'2'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,24(R3)             NET-CD                                     
         B     REQT90                                                           
*                                                                               
REQT90   EDIT  (P8,00(R3)),(14,(R7)),2,COMMAS=YES,MINUS=YES                     
         CP    00(8,R3),=P'0'                                                   
         BL    *+8                                                              
         MVI   13(R7),C'*'                                                      
REQT96   LA    R7,P+68                                                          
         LA    R4,1(R4)                                                         
         BCT   R8,REQT8                                                         
*****                                                                           
         GOTO1 VPRINTIT                                                         
*                                                                               
*****                                                                           
REQEN12X XC    MTHACT(5),MTHACT                                                 
         MVI   REPACT,0                                                         
         XC    PRDMTHS,PRDMTHS                                                  
         XC    PUBMTHS,PUBMTHS                                                  
         XC    PUBPRDS,PUBPRDS                                                  
         LA    R3,REQINS                                                        
         LA    R4,7                                                             
REQEND13 XC    0(ACCNUM*4,R3),0(R3)                                             
         LA    R3,ACCNUM*4(R3)                                                  
         BCT   R4,REQEND13                                                      
         XC    RQTOTPUB,RQTOTPUB                                                
*                                                                               
REQENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
CLTFRST  CSECT                                                                  
         NMOD1 0,CLTFRST                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VNVWORK                                                       
         USING NVWRKD,R5                                                        
         MVI   FORCEHED,C'P'     NEW PAGE AND FORCE PAGE                        
         XC    SAVEYMD,SAVEYMD                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         XC    SAVEPUB,SAVEPUB                                                  
         OC    PROGPROF,PROGPROF                                                
         BNZ   CLIF0                                                            
         MVC   P(20),=C'NO NV PROFILE SET UP'                                   
CLIFERR  GOTO1 REPORT                                                           
         MVI   MODE,LBUYREQ        CAN'T PROCESS GO TO NEXT REQ                 
         B     CLIEXT                                                           
*                                                                               
CLIF0    DS    0H               CHECK FOR NVTEXT RECORD                         
*                                  NEED TO READ PROFILE HERE                    
         MVC   PROFKEY,=CL12'P000'                                              
         MVC   PROFKEY+4(3),QAGENCY                                             
         MVC   PROFKEY+7(3),PCLTKCLT                                            
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   PROFKEY+10,C'*'                                                  
         MVC   PROFKEY+11(1),PCLTOFF                                            
         GOTO1 GETPROF,DMCB,PROFKEY,SYSPROF,DATAMGR                             
*                                                                               
         MVC   PROFKEY+2(2),=C'PW'       GET PW PROFILE                         
         GOTO1 (RF),(R1),,PWPROF                                                
*                                                                               
         MVC   SVFAXOPT,FAXOPT    SAVE FAXOPT FRO REQUEST                       
         CLI   PWPROF+2,C'Y'    SEE IF NV FAXING ALLOWED                        
         BE    *+8                                                              
         MVI   FAXOPT,C'N'      SET OFF FAXING                                  
*                               CHECK FOR NVTEXT RECORD                         
         CLI   PROGPROF+10,0                                                    
         BNE   *+8                                                              
         MVI   PROGPROF+10,C'X'   SET DEFAULT FOR NO $ ON LETTERS               
*                                                                               
*        PRINT OFFICE CODE                                                      
*                                                                               
         GOTOR VPRNTOFC,DMCB,PCLTOFF,SVPTOFC,VOFFICER,QAGENCY,VCOMFACS          
*                                                                               
*SMY*    GOTO1 VOFFOUT,DMCB,PCLTOFF,VHEXOUT,(C'L',SVPTOFC)                      
*SMY*    CLI   0(R1),X'FF'                                                      
*SMY*    BNE   *+6                                                              
*SMY*    DC    H'0'                                                             
*                                                                               
         MVI   NOLETSW,C'N'                                                     
         MVI   ONECLI,C'N'                                                      
         MVC   PPGKEY,KEY        SAVE PPG'S KEY                                 
         CLI   QOPT7,C'R'        SEE IF DOING REPORT ONLY                       
         BE    CLIF0X            DON'T NEED TO LOOK FOR NVTEXT RECORD           
         XC    KEY,KEY                      READ NVTEXT LETTER FIRST            
         MVC   KEY(7),PCLTKAGY                                                  
         MVI   KEY+3,X'41'                  IF DOESN'T EXIST                    
         MVC   KEYSAVE,KEY                                                      
         LA    R4,DMRDHI                                                        
         BAS   RE,DIRRD                                                         
         CLC   KEY(7),KEYSAVE                                                   
         BE    CLIF0X                FOUND                                      
*                                                                               
         MVC   KEYSAVE+4(3),=X'FF4040'      THEN READ FOR OFFICE                
         MVC   KEYSAVE+5(1),PCLTOFF         IF DOESN'T EXIST                    
         MVC   KEY,KEYSAVE                                                      
         BAS   RE,DIRRD                                                         
         CLC   KEY(7),KEYSAVE                                                   
         BE    CLIF0X                OFFICE NVTEXT FOUND                        
*                                                                               
         MVC   KEYSAVE+4(3),=X'FFFFFF'      THEN READ FOR MEDIA                 
         MVC   KEY,KEYSAVE                  (ALL CLIENTS)                       
         BAS   RE,DIRRD                     IF DOESN'T EXIST                    
         CLC   KEY(7),KEYSAVE               BLOWUP                              
         BE    CLIF0X                                                           
         MVI   NOLETSW,C'Y'         CAN'T DO LETTERS- NO NVTEXT REC             
         B     CLIF0X                                                           
*                                                                               
*              FIRST BUILD LIST OF PRD AND NAMES                                
CLIF0X   CLC   QCLIENT,=C'ALL'                                                  
         BE    CLIF1                                                            
         CLI   QCLIENT,C'*'        OFFICE REQS                                  
         BE    CLIF1                                                            
         CLI   QCLIENT,C'$'        OFFICE LIST                                  
         BE    CLIF1                                                            
         CLI   QCLIENT,C'&&'       GROUP REQS                                   
         BE    CLIF1                                                            
         MVI   ONECLI,C'Y'         SET ON PROCESSING ONE CLIENT SW              
         B     PPGEXT                                                           
*                                                                               
CLIF1    DS    0H                                                               
         L     R6,APRDTAB                                                       
*                                                                               
         CLI   QPUB,C'0'           SEE IF DOING ONE PUB                         
         BNL   CLIF6               YES - ONLY READ PRDS WHEN NEEDED             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),PCLTKCLT                                                
         LA    R4,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     CLIF3                                                            
*                                                                               
CLIF2    LA    R4,DMRSEQ                                                        
CLIF3    BAS   RE,DIRRD                                                         
         CLC   KEY(7),KEYSAVE                                                   
         BNE   CLIF6                                                            
*                                                                               
*                                                                               
CLIF4    LA    R4,GETREC                                                        
         LA    R3,PPRDREC                                                       
         BAS   RE,FILERD                                                        
CLIF5    MVC   0(3,R6),PPRDKPRD                                                 
         MVC   3(20,R6),PPRDNAME                                                
         LA    R6,23(R6)                                                        
         B     CLIF2                                                            
*                                                                               
CLIF6    MVC   0(3,R6),=X'FFFFFF'  SET END OF TABLE                             
         B     PPGEXT                                                           
*                                                                               
DIRRD    NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    DIRX                                                             
         CLI   DMCB+8,X'02'         PASS DELETES                                
         BE    DIRX                                                             
         DC    H'0'                                                             
DIRX     XIT                                                                    
*                                                                               
FILERD   NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTFILE',KEY+27,         X        
               (R3),(0,DMWORK)                                                  
         CLI   DMCB+8,0                                                         
         BE    FILX                                                             
         CLI   DMCB+8,X'02'            PASS DELETES                             
         BE    FILX                                                             
         DC    H'0'                                                             
FILX     XIT                                                                    
*                                                                               
PPGEXT   MVC   KEY,PPGKEY                                                       
         LA    R4,DMRDHI                                                        
         BAS   RE,DIRRD                                                         
CLIEXT   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PUBFIRST CSECT                                                                  
         NMOD1 0,PUBFRST                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VNVWORK                                                       
         USING NVWRKD,R5                                                        
         MVI   PUBSW,0                                                          
         CLI   QOPT3,C' '                                                       
         BE    SETFLT              DOING ALL PUBS                               
         LA    R7,PPFILED+4095                                                  
         LA    R7,1(R7)                                                         
         USING PPFILED+4096,R7                                                  
         LA    R3,PUBREC+33                                                     
         USING PUBGENEL,R3                                                      
         CLI   0(R3),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                NO NAME ELEMENT                              
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),X'20'                                                      
         BE    HAVEL       HAVE PROD ELEMENT                                    
         CLI   QOPT3,C'C'                                                       
         BE    GETNEXT                                                          
         B     SETFLT                                                           
*                                                                               
HAVEL    DS    0H                                                               
         OC    PUBCDDAT,PUBCDDAT   CHK FOR EFF DATE                             
         BNZ   HAVEL5              IF PRESENT THIS IS OR WAS A CD PUB           
         CP    PUBCD,=P'0'                                                      
         BE    NCHDIS              NO CASH DISCOUNT                             
HAVEL5   CLI   QOPT3,C'C'                                                       
         BE    SETFLT                                                           
         B     GETNEXT                                                          
*                                                                               
NCHDIS   CLI   QOPT3,C'C'                                                       
         BE    GETNEXT                                                          
         B     SETFLT                                                           
*                                                                               
GETNEXT  MVI   PUBSW,1             DON'T PROCESS THIS PUB                       
         B     PUBFEXT                                                          
*                                                                               
         DROP  R7                                                               
         DROP  R3                                                               
SETFLT   DS    0H                                                               
PUBFEXT  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
LETRPRT  CSECT                                                                  
         NMOD1 0,LETRPRT                                                        
         USING PPWORKD,RA                                                       
         NI    DMINBTS,X'F7'       NO DELETES IN THIS ROUTINE                   
*                                                                               
         L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         MVC   ACONIO1,ACONIO      (A)PCONREC                                   
         DROP  RF                                                               
*                                                                               
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VNVWORK                                                       
         LA    R9,1(R5)                                                         
         LA    R9,4095(R9)                                                      
         USING NVWRKD,R5,R9                                                     
*                                                                               
*     XSORT LETRTAB HERE                                                        
         OC    LETCNT,LETCNT                                                    
         BZ    LETRXIT                    NO LETTERS                            
         L     R3,LETCNT                                                        
         L     R2,ALETRTAB                                                      
         GOTO1 VXSORT,DMCB,0(R2),(R3),TBLLEN,140,0                              
*                                                                               
         MVI   FAXRUN,C'N'                                                      
         MVI   FAXFND,C'N'      SET OFF FAX FOUND INDICATOR                     
*                                                                               
LTPT01   L     R3,ALETRTAB                                                      
         USING TABDSC,R3                                                        
         MVI   RCSUBPRG,10                                                      
         XC    SVTAB,SVTAB                 SPACE TO SAVE PREVIOUSLY             
*                                          PROCESSED TABLE ENTRY                
LTPT10   OC    0(TBLLEN,R3),0(R3)          IF ZEROS NO MORE ENTRIES             
         BZ    LTPT50                                                           
         CLI   FAXRUN,C'Y'       SEE IF DOING FAX RUN THRU LETTERS              
         BNE   LTPT12                                                           
         OC    0(12,R3),0(R3)    CHK FOR FAX                                    
         BZ    LTPT35            SKIP LETTERS WITH NO FAX NUMBER                
         MVC   MYFAX,0(R3)                                                      
         B     LTPT13                                                           
*                                                                               
LTPT12   OC    0(12,R3),0(R3)    SEE IF FAX NUMBER PRESENT                      
         BZ    *+8                                                              
         MVI   FAXFND,C'Y'       SET ON FAX FOUND INDICATOR                     
*                                                                               
LTPT13   DS    0H                                                               
         CLC   0(141,R3),SVTAB   SEE IF DUPLICATE                               
         BE    LTPT35             YES - SKIP                                    
         CLC   0(126,R3),SVTAB   FAX,MED,NAME AND ADDRESS,CLT IS SAME           
         BE    LTPT30                      KEEP GOING ON SAME LETTER            
         XC    SVTAB,SVTAB                 MUST CLEAR FOR NEW LETTER            
         BAS   RE,PRTHEAD                  ELSE PRINT HEADLINES                 
         BAS   RE,PRTBODY                  PRINT BODY                           
LTPT30   BAS   RE,PRTINS                   PRINT BUYS                           
         MVC   SVTAB,0(R3)                 SAVE THIS ONE FOR NEXT ENTRY         
LTPT35   LA    R3,TBLLEN(R3)               BUMP TABLE                           
         B     LTPT10                      BRANCH TO PROCESS NEXT ENTRY         
*                                                                               
LTPT50   DS    0H                                                               
         CLI   FAXRUN,C'Y'          SEE IF I JUST FINISHED FAX RUN              
         BE    LTPT60               IF YES - MUST CLOSE FAX PRNTQUE             
*                                   THEN EXIT                                   
         CLI   FAXFND,C'Y'          SEE IF ANY FAX NUMBER FOUND                 
         BNE   LETRXIT              IF NO - THEN DONE                           
         MVI   FAXRUN,C'Y'                                                      
*                                                                               
         MVI   FORCEFUT,C'Y'        GET ME TO BOTTOM OF PAGE                    
         GOTO1 REPORT                                                           
*                                                                               
         MVI   RCWHATPR,X'02'       SET TO PRINTER 2                            
         MVI   FTIME,C'Y'           SET FIRST TIME SWITCH                       
         MVI   LINE,1                                                           
*                                   TO FAX THE LETTERS                          
         B     LTPT01      GO FAX THE LETTERS THAT HAVE A FAX NUMBER            
*                                                                               
LTPT60   DS    0H          END OF FAX RUN (WHEN I HAVE FAXED LETTERS)           
         MVC   P,SPACES                                                         
         MVC   P(26),=C'*** END OF DDS MESSAGE ***'                             
         MVI   LINE,1      SO THIS WILL NEVER CAUSE A NEW PAGE                  
         BAS   RE,LREPT                                                         
*                                                                               
*                                                                               
LETRXIT  OI    DMINBTS,X'08'                                                    
         XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
PRTHEAD  NTR1                                                                   
         MVI   FORCEHED,C'P'     GO TO NEW PAGE AND FORCE PAGE                  
         MVI   LINE,1                                                           
         MVI   RCSUBPRG,10                 SET SPROG                            
         MVC   PAGE,=H'1'                  RESET PAGE FOR EACH LETTER           
         MVC   P,SPACES                    CLEAR PRINT LINES                    
         MVC   PSECOND,SPACES                                                   
         CLI   FAXRUN,C'Y'                                                      
         BE    PRTHD2                                                           
         OC    TABFAX,TABFAX                                                    
         BZ    PRTHD2                                                           
         MVC   P+24(29),=C'** DO NOT SEND THIS LETTER **'                       
         MVC   PSECOND+24(27),=C'** THIS LETTER WAS FAXED TO'                   
         MVC   PSECOND+52(12),TABFAX                                            
         MVC   PSECOND+65(2),=C'**'                                             
         CLC   TABFAX(3),=C'FX='   SEE IF CONTROL FAX RECORD                    
         BNE   PRTHD2                                                           
         MVI   SPACING,3                                                        
         BAS   RE,LREPT                    SPACE DOWN FROM TOP OF PAGE          
         BAS   RE,LGETFAX                                                       
         CLC   LTOFAX,SPACES                                                    
         BNE   PRTHD1                                                           
         MVC   P+24(39),=C'** CONTROL FILE FAX RECORD NOT FOUND **'             
         B     PRTHD1X                                                          
*                                                                               
PRTHD1   DS    0H                                                               
         MVC   P+24(7),=C'** FAX='                                              
         MVC   P+32(16),LTOFAX                                                  
         MVC   P+49(2),=C'**'                                                   
PRTHD1X  BAS   RE,LREPT                    SPACE DOWN FROM TOP OF PAGE          
         B     PRTHD5                                                           
*                                                                               
*                                                                               
PRTHD2   MVI   SPACING,3                                                        
         BAS   RE,LREPT                    SPACE DOWN FROM TOP OF PAGE          
*                                                                               
*                                                                               
PRTHD5   GOTO1 DATCON,DMCB,(5,0),(5,P+69)      TODAY'S DATE                     
         MVI   SPACING,3                                                        
*                                                                               
*        CHECK PROFILE FOR MORE LINES TO SKIP                                   
*                                                                               
         CLI   PROGPROF+12,0                                                    
         BE    PRTHD5X                                                          
*                                                                               
         BAS   RE,LREPT                                                         
         ZIC   R2,PROGPROF+12                                                   
*                                                                               
PRTHD5A  STC   R2,SPACING                                                       
         CH    R2,=H'3'       SEE IF LESS THAT 3                                
         BNH   PRTHD5X                                                          
         MVI   SPACING,3                                                        
         BAS   RE,LREPT                                                         
         SH    R2,=H'3'                                                         
         CH    R2,=H'0'                                                         
         BNH   PRTHD5XX       NONE WITH SKIPPING                                
         B     PRTHD5A                                                          
*                                                                               
PRTHD5X  BAS   RE,LREPT                                                         
*                                                                               
PRTHD5XX MVC   P+7(30),TABNAME                 MOVE IN HEADLINE FIELDS          
         MVC   PSECOND+7(6),=C'ATTN: '                                          
         MVC   PSECOND+13(20),TABATTN                                           
         BAS   RE,LREPT                                                         
         MVC   P+7(30),TABAD1                                                   
         BAS   RE,LREPT                                                         
         MVC   P+7(30),TABAD2                                                   
         MVI   SPACING,2                                                        
         BAS   RE,LREPT                                                         
*                                                                               
         BAS   RE,GETCLNT                                                       
*                                                                               
         MVC   P+7(9),=C'CLIENT - '                                             
         MVC   P+16(3),TABCLI                                                   
         MVC   P+21(20),PCLTNAME                                                
         BAS   RE,LREPT                                                         
         MVI   SPACING,3                                                        
         BAS   RE,LREPT                                                         
PRTHDX   XIT1  1                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
PRTBODY  NTR1                                                                   
         XC    KEY,KEY                      READ NVTEXT LETTER FIRST            
         MVC   KEY(2),PBUYKAGY                                                  
         MVC   KEY+2(1),TABMED              FOR SPECIFIC CLIENT                 
         MVI   KEY+3,X'41'                  IF DOESN'T EXIST                    
         MVC   KEY+4(3),TABCLI                                                  
         MVC   KEYSAVE,KEY                                                      
         LA    R6,DMRDHI                                                        
         BAS   RE,LDIRRD                                                        
         CLC   KEY(7),KEYSAVE                                                   
         BE    PRTB10                                                           
*                                                                               
         MVC   KEYSAVE+4(3),=X'FF4040'      THEN READ FOR OFFICE                
         MVC   KEYSAVE+5(1),PCLTOFF         IF DOESN'T EXIST                    
         MVC   KEY,KEYSAVE                                                      
         BAS   RE,LDIRRD                                                        
         CLC   KEY(7),KEYSAVE                                                   
         BE    PRTB10                                                           
*                                                                               
         MVC   KEYSAVE+4(3),=X'FFFFFF'      THEN READ FOR MEDIA                 
         MVC   KEY,KEYSAVE                  (ALL CLIENTS)                       
         BAS   RE,LDIRRD                    IF DOESN'T EXIST                    
         CLC   KEY(7),KEYSAVE               BLOWUP                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRTB10   LA    R6,GETREC                                                        
*NOP*    LA    R7,PCONREC           READING PNVTREC INTO PCONREC AREA           
         L     R7,ACONIO1           (A)PCONREC                                  
         BAS   RE,LFILERD                                                       
         L     R4,ACONIO1           (A)PCONREC                                  
*NOP*    LA    R4,PCONREC+33                                                    
         LA    R4,33(R4)                                                        
PRTB20   CLI   0(R4),X'00'                                                      
         BE    PRTB40                                                           
         CLI   0(R4),X'40'             FIND 40 ELEMENTS                         
         BNE   PRTB30                                                           
         ZIC   R1,1(R4)                DECREMENT ELEMENT LENGTH BY 2            
         BCTR  R1,R0                   AND USE THIS LENGTH TO MOVE              
         BCTR  R1,R0                   TEXT TO PRINT LINE                       
         BCTR  R1,R0                   TEXT TO PRINT LINE                       
         LR    R6,R4                   NEED FOR EXECUTE STATEMENT               
         CLI   2(R4),C'+'              WAS SPACING SPECIFIED IN COMMENT         
         BNE   PRTB25                                                           
         MVC   SPACING,3(R4)           IF YES DO SPACING                        
         NI    SPACING,X'0F'                                                    
         CLI   SPACING,3                                                        
         BNH   *+8                                                              
         MVI   SPACING,3                                                        
         CLI   SPACING,0                                                        
         BNE   *+8                                                              
         MVI   SPACING,1                                                        
         MVI   P,0                                                              
         BAS   RE,LREPT                PRINT SPACING                            
         BCTR  R1,R0                   DECREMENT FOR +N                         
         BCTR  R1,R0                                                            
         LA    R6,2(R6)                BUMP UP FOR EXECUTED MOVE                
PRTB25   LTR   R1,R1                                                            
         BNM   PRTB26                                                           
         MVI   P,0                                                              
         BAS   RE,LREPT                                                         
         B     PRTB30                                                           
*                                                                               
PRTB26   EX    R1,PRTBEXC                                                       
         BAS   RE,LREPT                                                         
*                                                                               
PRTB30   SR    R0,R0                   BUMP AND GET NEXT ELEMENT                
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     PRTB20                                                           
*                                                                               
PRTBEXC  MVC   P+7(0),2(R6)                                                     
*                                                                               
PRTB40   MVI   SPACING,3                                                        
         BAS   RE,LREPT                                                         
         BAS   RE,PRTPUB             PRINT PUB NUMBER AND NAME                  
         BAS   RE,PRTCOLH            PRINT COLUMN HEADERS                       
         XC    SVTAB,SVTAB           SO WON'T PRINT PUB TWICE                   
PRTBDX   XIT1  1                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
PRTINS   NTR1                                                                   
         OC    SVTAB,SVTAB           IF FIRST TIME THROUGH HAVE ALREADY         
         BZ    PRTIN10               PRINTED PUB AND COLUMN HEADERS             
*                                                                               
         CLC   TABPUB,SVPUB          SEE IF SAME PUB                            
         BE    PRTIN3                                                           
         CLI   LINE,52               NEW PUB THEN I NEED 6 LINES                
         BH    PRTIN4                                                           
         BAS   RE,PRTPUB                                                        
         B     PRTIN10                                                          
*                                                                               
PRTIN3   CLI   LINE,57               IF GOING TO A NEW PAGE NEED TO             
         BL    PRTIN10               PRINT PUB AND COLUMN HEADERS               
PRTIN4   MVI   FORCEHED,C'P'     NEW PAGE AND FORCE PAGE                        
         BAS   RE,PRTPUB                                                        
         BAS   RE,PRTCOLH                                                       
         B     PRTIN10                                                          
*                                                                               
*                                                                               
PRTIN10  DS    0H                                                               
         CLI   PROGPROF+10,C'X'           SEE IF SHOWING $                      
         BNE   PRTINS50                   YES - NEW FORMAT                      
*                                         OLD FORMAT                            
PRTIN10C XC    FULL,FULL                  PUT ESTIMATE NO TO PRINT LINE         
         MVC   FULL+2(2),TABEST                                                 
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         MVC   HALF,DUB+6                                                       
         OI    HALF+1,X'0F'                                                     
         UNPK  FULL,HALF                                                        
         MVC   P+28(3),FULL+1                                                   
*                                                                               
         BAS   RE,GETPRD                      MOVE PRODUCT CODE AND             
         MVC   P+2(3),TABPRD                  NAME TO PRINT LINE                
         MVI   P+5,C'-'                                                         
         MVC   P+6(20),PPRDNAME                                                 
*                                                                               
         CLI   TABINS+2,0                SEE IF MONTHLY INSERTION               
         BNE   TABINS12                                                         
         GOTO1 DATCON,DMCB,(3,TABINS),(6,P+32)    INSERTION DATE TO             
         CLI   TABSUBL,X'01'                                                    
         BNH   PRTINS15                                                         
         MVI   P+38,C'-'                                                        
         ZIC   R0,TABSUBL               SUB LINE                                
         CVD   R0,DUB                                                           
         CP    DUB,=P'100'                                                      
         BL    PRTIN11                                                          
         DP    DUB,=P'10'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  P+40(1),DUB+7(1)                                                 
         ZAP   DUB,DUB(6)                                                       
         SP    DUB,=P'10'                                                       
         CVB   R1,DUB                                                           
         LA    R1,ALPHTAB(R1)                                                   
         MVC   P+39(1),0(R1)                                                    
         B     PRTINS15                                                         
*                                                                               
PRTIN11  OI    DUB+7,X'0F'                                                      
         UNPK  P+39(2),DUB                                                      
         B     PRTINS15                                                         
*                                                                               
TABINS12 GOTO1 DATCON,DMCB,(3,TABINS),(5,P+32)    INSERTION DATE TO             
         CLI   TABSUBL,X'01'                                                    
         BNH   PRTINS15                                                         
         MVI   P+40,C'-'                                                        
         ZIC   R0,TABSUBL               SUB LINE                                
         CVD   R0,DUB                                                           
         CP    DUB,=P'100'                                                      
         BL    PRTIN13                                                          
         DP    DUB,=P'10'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  P+42(1),DUB+7(1)                                                 
         ZAP   DUB,DUB(6)                                                       
         SP    DUB,=P'10'                                                       
         CVB   R1,DUB                                                           
         LA    R1,ALPHTAB(R1)                                                   
         MVC   P+41(1),0(R1)                                                    
         B     PRTINS15                                                         
*                                                                               
PRTIN13  OI    DUB+7,X'0F'                                                      
         UNPK  P+41(2),DUB                                                      
         B     PRTINS15                                                         
*                                                                               
PRTINS15 MVC   P+44(20),TABSPC                    HEADLINE                      
         CLI   TABLIO,0                                                         
         BE    PRTINS30                                                         
         CLI   TABLIO,X'01'           MEANS I DIDN'T FIND ANY                   
         BE    PRTINS30                                                         
         CLI   PROGPROF+1,C'R'        ONLY ON REPORT                            
         BE    PRTINS30                                                         
*        GOTO1 DTCNV,DMCB,(1,TABLIO+2),(0,P+66)                                 
         GOTO1 DATCON,DMCB,(3,TABLIO+2),(0,P+66)                                
         MVC   P+65(1),TABMED                                                   
         MVI   P+66,C'-'                                                        
         MVI   P+72,C'-'                                                        
         MVC   HALF,TABLIO+5                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+73(4),DUB                                                      
*                                                                               
         CLI   TABLIO+10,C'N'                                                   
         BNE   PRTINS20                                                         
         MVC   P+78(3),=C'NEW'                                                  
         B     PRTINS30                                                         
*                                                                               
PRTINS20 CLI   TABLIO+10,C'C'                                                   
         BNE   PRTINS25                                                         
         MVC   P+78(3),=C'CHA'                                                  
         B     PRTINS30                                                         
*                                                                               
PRTINS25 CLI   TABLIO+10,C'D'                                                   
         BNE   PRTINS30                                                         
         MVC   P+78(3),=C'CAN'                                                  
PRTINS30 B     PRTINS90                                                         
*                                                                               
PRTINS50 DS    0H               NEW FORMAT (WITH $'S)                           
         EDIT  (B4,TABORD),(12,P+65),2,COMMAS=YES,MINUS=YES                     
         OC    TABPAID,TABPAID   CHECK FOR PAYMENT                              
         BZ    PRTINS55          REST SAME AS OLD                               
         LA    R6,PSECOND+65                                                    
         EDIT  (B4,TABPAID),(12,0(R6)),2,COMMAS=YES,MINUS=YES,FLOAT=$           
         MVI   0(R1),C'('                                                       
         MVC   0(12,R6),WORK+5                                                  
         MVI   11(R6),C')'                                                      
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   11(2,R6),=C'-)'                                                  
*                                                                               
PRTINS52 CLI   0(R6),C'('                                                       
         BE    PRTINS53                                                         
         LA    R6,1(R6)                                                         
         B     PRTINS52                                                         
*                                                                               
PRTINS53 SH    R6,=H'11'      R6 POINTS TO (                                    
*                             FLOAT BEFORE AMOUNT                               
         MVC   0(10,R6),=C'PREV. PAID'                                          
*                                                                               
PRTINS55 CLI   TABLIO,0         SEE IF I CAN USE LAST I/O COLUMNS               
         BNE   PRTINS60                                                         
         B     PRTIN10C          REST SAME AS OLD                               
*                                                                               
*                                MUST PUT LAST I/O INFO IN PSECOND              
PRTINS60 DS    0H                                                               
         CLI   TABLIO,X'01'           MEANS I DIDN'T FIND ANY                   
         BE    PRTINS70                                                         
         CLI   PROGPROF+1,C'R'        ONLY ON REPORT                            
         BE    PRTINS70                                                         
         MVC   PSECOND+28(9),=C'LAST I/O='                                      
*        GOTO1 DTCNV,DMCB,(1,TABLIO+2),(0,PSECOND+38)                           
         GOTO1 DATCON,DMCB,(3,TABLIO+2),(0,PSECOND+38)                          
         MVC   PSECOND+37(1),TABMED                                             
         MVI   PSECOND+38,C'-'                                                  
         MVI   PSECOND+44,C'-'                                                  
         MVC   HALF,TABLIO+5                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PSECOND+45(4),DUB                                                
*                                                                               
         CLI   TABLIO+10,C'N'                                                   
         BNE   PRTINS62                                                         
         MVC   PSECOND+49(3),=C'NEW'                                            
         B     PRTINS70                                                         
*                                                                               
PRTINS62 CLI   TABLIO+10,C'C'                                                   
         BNE   PRTINS64                                                         
         MVC   PSECOND+49(3),=C'CHA'                                            
         B     PRTINS70                                                         
*                                                                               
PRTINS64 CLI   TABLIO+10,C'D'                                                   
         BNE   PRTINS70                                                         
         MVC   PSECOND+49(3),=C'CAN'                                            
PRTINS70 MVI   TABLIO,X'01'        SO IT WON'T PRINT AGAIN                      
         B     PRTIN10C                                                         
*                                                                               
PRTINS90 BAS   RE,LREPT                                                         
PRTINSX  XIT1  1                                                                
*                                                                               
ALPHTAB  DC    C'ABCDEFGHIJKLMNOP',X'FF'                                        
         EJECT                                                                  
*                                                                               
*                                                                               
PRTCOLH  NTR1                                  PRINT COLUMN HEADERS             
         CLI   PROGPROF+10,C'X'       SEE IF SHOWING $                          
         BNE   PRTCOL10               NEW FORMAT                                
*                                                                               
         CLI   TABLIO,0                                                         
         BE    PRTCOL2                                                          
         MVC   P+65(8),=C'LAST I/O'                                             
         MVC   PSECOND+65(8),=C'-----------'                                    
*                                                                               
PRTCOL2  MVC   P+2(7),=C'PRODUCT'                                               
         MVC   P+28(3),=C'EST'                                                  
         MVC   P+32(8),=C'INS DATE'                                             
         MVC   P+44(5),=C'SPACE'                                                
         MVC   PSECOND+2(7),=C'-----------'                                     
         MVC   PSECOND+28(3),=C'-----------'                                    
         MVC   PSECOND+32(8),=C'-----------'                                    
         MVC   PSECOND+44(5),=C'-----------'                                    
         B     PRTCOL20                                                         
*                                                                               
PRTCOL10 DS    0H                NEW FORMAT                                     
*                                LAST I/O INFO WILL DISPLAY                     
*                                UNDER ESTIMATE                                 
         MVC   P+73(3),=C'NET'                                                  
         MVC   PSECOND+73(3),=C'---'                                            
         CLI   PROGPROF+10,C'N'       NET                                       
         BE    PRTCOL2                                                          
         MVC   P+71(5),=C'GROSS'                                                
         MVC   PSECOND+71(5),=C'-----'                                          
         CLI   PROGPROF+10,C'G'       GROSS                                     
         BE    PRTCOL2                                                          
         MVC   P+70(6),=C'NET-CD'                                               
         MVC   PSECOND+70(6),=C'------'                                         
         CLI   PROGPROF+10,C'2'       NET-CD                                    
         BE    PRTCOL2                                                          
         MVC   P+68(8),=C'GROSS-CD'                                             
         MVC   PSECOND+68(8),=C'--------'                                       
         CLI   PROGPROF+10,C'1'       GROSS-CD                                  
         BE    PRTCOL2                                                          
         DC    H'0'              BAD PROFILE                                    
*                                                                               
PRTCOL20 BAS   RE,LREPT                                                         
PRTCOLX  XIT1  1                                                                
*                                                                               
PRTPUB   NTR1                                          PRINT PUB NAME           
         CLI   FORCEHED,C'P'                                                    
         BE    PRTP1                                                            
         BAS   RE,LREPT                                AND NUMBER               
*                                                                               
PRTP1    MVC   P+7(20),TABPUBN                                                  
         CLC   TABZONN,=20C' '                                                  
         BNH   PRTP2                                                            
         MVC   PSECOND+7(20),TABZONN                                            
*                                                                               
PRTP2    LA    R4,P+27                                                          
PRTP2A   CLI   0(R4),C' '                                                       
         BH    PRTP3                                                            
         SH    R4,=H'1'                                                         
         B     PRTP2A                                                           
*                                                                               
PRTP3    MVI   2(R4),C'('                                                       
         LA    R4,3(R4)                                                         
PRTP4    IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),TABPUB),(C'S',(R4))                           
         LA    R4,14(R4)                                                        
PRTP4B   CLI   0(R4),C' '                                                       
         BH    PRTP4C                                                           
         SH    R4,=H'1'                                                         
         B     PRTP4B                                                           
PRTP4C   MVI   1(R4),C')'                                                       
         BAS   RE,LREPT                                                         
*                                                                               
         CLC   TABPUB,SVPUB         SEE IF SAME PUB                             
         BNE   PRTP5                                                            
         MVC   P+7(11),=C'(CONTINUED)'                                          
         BAS   RE,LREPT                                                         
*                                                                               
PRTP5    BAS   RE,LREPT                                                         
PRTPXIT  XIT1  1                                                                
         EJECT                                                                  
*                                                                               
LREPT    NTR1                                                                   
         CLI   FAXRUN,C'Y'     SEE IF FAXING                                    
         BE    LREPT5                                                           
         GOTO1 REPORT                                                           
         B     LREPTX                                                           
*                                                                               
LREPT5   GOTO1 FREPORT         SPECIAL INTERFACE TO "REAL" REPORT               
*                              FOR FAXING                                       
LREPTX   XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
GETCLNT  NTR1                           READ CLIENT FROM TABLE INFO             
         LA    R4,5                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),PBUYKAGY                                                  
         MVC   KEY+2(1),TABMED                                                  
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),TABCLI                                                  
         LA    R6,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         BAS   RE,LDIRRD                                                        
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,GETREC                                                        
         LA    R7,PCLTREC                                                       
         BAS   RE,LFILERD                                                       
GETCXIT  XIT1  1                                                                
*                                                                               
*                                                                               
GETPRD   NTR1                              READ PRODUCT FROM TABLE INFO         
         XC    KEY,KEY                                                          
         MVC   KEY(2),PBUYKAGY                                                  
         MVC   KEY+2(1),TABMED                                                  
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),TABCLI                                                  
         MVC   KEY+7(3),TABPRD                                                  
         LA    R6,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         BAS   RE,LDIRRD                                                        
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,GETREC                                                        
         LA    R7,PPRDREC                                                       
         BAS   RE,LFILERD                                                       
GETPXIT  XIT1  1                                                                
*                                                                               
*                                                                               
LDIRRD   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    LDIRX                                                            
         DC    H'0'                                                             
LDIRX    XIT1  1                                                                
*                                                                               
LFILERD  NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),=C'PRTFILE',KEY+27,         X        
               (R7),(0,DMWORK)                                                  
         CLI   DMCB+8,0                                                         
         BE    LFILX                                                            
         DC    H'0'                                                             
LFILX    XIT1  1                                                                
         EJECT                                                                  
*                                                                               
LGETFAX  NTR1                                                                   
         MVC   LTOFAX,SPACES                                                    
         MVC   LSIOKEY,KEY      SAVE KEY                                        
LGETFAXB XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTFXKEY,R4                                                       
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,QAGENCY                                                  
         MVC   CTFXCODE,TABFAX+3                                                
         OC    CTFXCODE,SPACES      SINCE TABFAX MAY NOT HAVE SPACES            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',KEY,LIO                    
         CLC   LIO(18),KEYSAVE     COMPARE 7-BYTE FAX CODE                      
         BNE   LGETFAXX                                                         
         LA    R4,LIO                                                           
         LA    R6,CTFXEL1                                                       
         B     LGETFX4                                                          
         SPACE 1                                                                
LGETFX2  ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         SPACE 1                                                                
LGETFX4  CLI   0(R6),0                                                          
         BE    LGETFAXX                                                         
         CLI   0(R6),CTFX1ELQ                                                   
         BE    LGETFXNO                                                         
         B     LGETFX2                                                          
         SPACE 1                                                                
         USING CTFX1EL,R6                                                       
LGETFXNO ZIC   R1,CTFX1LEN         FAX NUMBER                                   
         SH    R1,=H'3'                                                         
         CH    R1,=H'24'                                                        
         BL    *+8                                                              
         LA    R1,24                                                            
         EX    R1,*+8                                                           
         B     LGETFX2                                                          
         MVC   LTOFAX(0),CTFX1NUM                                               
         SPACE 1                                                                
*                                                                               
LGETFAXX MVC   KEY,LSIOKEY     RESTORE MY KEY                                   
         XIT                                                                    
         DROP  R4                                                               
         DROP  R6                                                               
*        FREPORT LTORG                                                          
         LTORG                                                                  
*                                                                               
LTOFAX   DS    CL16                                                             
LSIOKEY  DS    CL32         SAVED KEY                                           
*                                                                               
ACONIO1  DS    A            ADDRESS OF PCONREC FROM PPG (ACONIO)                
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
         DS    0F                 SO SVORD AND SVPAID WILL ALIGN                
SVTAB    DS    0CL220             SAVE PREVIOUSLY PROCESSED TABLE ENTRY         
SVFAX    DS    CL12                                                             
SVMED    DS    CL1                                                              
SVNAME   DS    CL30                                                             
SVAD1    DS    CL30                                                             
SVAD2    DS    CL30                                                             
SVATTN   DS    CL20                                                             
SVCLI    DS    CL3                                                              
SVPUB    DS    CL6                                                              
SVPRD    DS    CL3                                                              
SVEST    DS    XL2                                                              
SVINS    DS    XL3                INSERTION DATE                                
SVSUBL   DS    XL1                SUB LINE                                      
SVSPC    DS    CL20                                                             
SVLIO    DS    CL11                                                             
SVPUBN   DS    CL20                                                             
SVZONN   DS    CL20                                                             
SVORD    DS    CL4                                                              
SVPAID   DS    CL4                                                              
*                                                                               
LIO      DS    CL500        FOR CONTROL FAX RECORD                              
         EJECT                                                                  
*                          SPECIAL REPORT FOR FAXING                            
FREPORT  CSECT                                                                  
         NMOD1 0,FREPORT                                                        
         LA    R8,FREPWORK                                                      
         USING PPREPD,R8                                                        
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VNVWORK                                                       
         LA    R9,4095(R5)                                                      
         LA    R9,1(R9)                                                         
         USING NVWRKD,R5,R9                                                     
         SPACE 3                                                                
***      JUST PUT AGYNAME AND ADDR INTO HEADLINES                               
*                                                                               
         MVC   MYHEAD1,SPACES                                                   
         MVC   MYHEAD2,SPACES                                                   
         MVC   MYHEAD1+25(33),PAGYNAME                                          
         MVC   MYHEAD2+25(33),PAGYADDR                                          
*                                                                               
         EJECT                                                                  
*                   CONTROL OF HEADLINE PRINTING                                
         SPACE 3                                                                
PC6      CLI   FORCEHED,C'N'                                                    
         BE    PC8                                                              
         MVI   FORCEHED,C'N'                                                    
         B     PC10                                                             
         SPACE 2                                                                
PC8      CLC   LINE,MAXLINES                                                    
         BL    PC12                                                             
         SPACE 2                                                                
PC10     MVI   FORCEMID,C'Y'                                                    
*                                                                               
PC10D    DS    0H                                                               
         MVI   FORCEHED,C'N'     TRICK "REAL" REPORT                            
*                                SO IT WON'T PRINT HEADLINES                    
         CLI   FTIME,C'Y'                                                       
         BE    PC10D5                                                           
         B     PC10P                                                            
*                                                                               
PC10D5   DS    0H                                                               
*                            NOTE - NO SKIP TO CHANNEL 1 NEEDED                 
*                            IF FAXING  (/PAGE WILL CAUSE NEW PAGE)             
         MVI   FTIME,C'N'                                                       
         MVC   FRSVFAX,MYFAX  SAVE THIS FAX NUMBER SO I CAN CHECK FOR           
*                             A CHANGE OF FAX NUMBER                            
         MVC   MYPLINE,SPACES                                                   
*******  MVC   WUORIG,AGYORIG        NO-OPED                                    
*                                                                               
         MVC   WU9FB(5),=C'*HDR*'                                               
         MVC   WUDESTID(4),=C'FAX '                                             
         MVC   WUDESTID+5(12),MYFAX                                             
*                                                                               
         CLC   MYFAX(5),=C'MB=62'       CHECK FOR EASY-LINK MAIL BOX            
         BNE   PC10F                    (THEY ALL MUST BEGIN WITH 62)           
         MVC   WUDESTID(17),SPACES                                              
         MVC   WUDESTID(8),MYFAX+3      MOVE MAIL BOX NUMBER                    
         B     PC10F5                                                           
*                                                                               
PC10F    CLC   MYFAX(3),=C'FX='    CHECK FOR CONTROL FILE FAX CODE              
         BNE   PC10F5                                                           
         MVC   WUDESTID(17),SPACES                                              
         MVC   WUDESTID(6),=C'FXKEY='                                           
         MVC   WUDESTID+6(9),MYFAX+3                                            
         B     PC10F5                                                           
*                                                                               
* * *    BNE   PC10F5                                                           
* * *    BAS   RE,GETFAX                                                        
* * *    CLC   TOFAX,SPACES        FAX RECORD NOT FOUND                         
* * *    BE    PC10F5              LEAVE FX=XXXXX IN WUDESTID                   
* * *    MVC   WUDESTID+5(16),TOFAX                                             
*        B     PC10F5                                                           
*                                                                               
PC10F5   MVC   WUBILL(1),QMEDIA                                                 
         MVC   WUBILL+1(3),QCLIENT                                              
         MVC   WUBILL+4(3),QPRODUCT                                             
*                                                                               
         MVC   WUDEST(12),MYFAX                                                 
*                                                                               
         OC    WUBILL(7),SPACES                                                 
         MVI   WUEND,C'.'                                                       
         MVC   WUEND+1(7),WUEND                                                 
         MVC   SVP,P                                                            
         MVC   SVP2,PSECOND                                                     
         MVC   SVSPACE,SPACING                                                  
         MVC   PSECOND,SPACES      JUST IN CASE                                 
         MVI   SPACING,1                                                        
         MVC   P,MYPLINE                                                        
         GOTO1 REPORT             GO TO "REAL" REPORT                           
*                                                                               
         BAS   RE,SETEDT          GO SET EDICT DATA IN MYPLINE                  
*                                                                               
PC10G    DS    0H                                                               
         MVC   P,MYPLINE                                                        
         GOTO1 REPORT             GO TO "REAL" REPORT                           
*                                                                               
PC10L    DS    0H                                                               
*                            THIS SHOULD SUPPRESS EASYLINK HEADER               
*                            MESSAGE                                            
         MVC   MYPLINE,SPACES                                                   
         MVC   MYPLINE(10),=C'/FORM/PAGE'                                       
*                                                                               
         MVC   P,MYPLINE                                                        
         GOTO1 REPORT             GO TO "REAL" REPORT                           
         B     PC10X              PC10X RESTORES P LINES                        
*                                 AND PRINTS THEM                               
PC10P    DS    0H                                                               
         MVC   SVP,P                SAVE P AND PSECOND                          
         MVC   SVP2,PSECOND                                                     
         MVC   SVSPACE,SPACING                                                  
         MVC   PSECOND,SPACES      JUST IN CASE                                 
         MVI   SPACING,1                                                        
*                                                                               
         CLC   FRSVFAX,MYFAX        CHECK FOR NEW FAX NUMBER                    
         BE    PC10P8                                                           
         MVC   MYPLINE,SPACES                                                   
         MVC   MYPLINE(26),=C'*** END OF DDS MESSAGE ***'                       
         MVC   P,MYPLINE                                                        
         MVI   LINE,1         SO THIS WILL NEVER CAUSE A NEW PAGE               
         GOTO1 REPORT             GO TO "REAL" REPORT                           
*                                                                               
         MVC   FRSVFAX,MYFAX             SAVE FAX NUMBER                        
*                                                                               
         MVC   MYPLINE,SPACES                                                   
*******  MVC   WUORIG,AGYORIG            NO-OPED                                
*                                                                               
         MVC   WU9FB(5),=C'*HDR*'                                               
*                                                                               
         MVC   WUDESTID(4),=C'FAX '                                             
         MVC   WUDESTID+5(12),MYFAX                                             
*                                                                               
         CLC   MYFAX(5),=C'MB=62'       CHECK FOR EASY LINK MAIL BOX            
         BNE   PCP02                    (THEY ALL MIST BEGIN WITH 62)           
         MVC   WUDESTID(17),SPACES                                              
         MVC   WUDESTID(8),MYFAX+3                                              
         B     PCP05                                                            
*                                                                               
PCP02    CLC   MYFAX(3),=C'FX='    CHECK FOR CONTROL FILE FAX CODE              
         BNE   PCP05                                                            
         MVC   WUDESTID(17),SPACES                                              
         MVC   WUDESTID(6),=C'FXKEY='                                           
         MVC   WUDESTID+6(9),MYFAX+3                                            
         B     PCP05                                                            
*                                                                               
****     BNE   PCP05                                                            
****     BAS   RE,GETFAX                                                        
****     CLC   TOFAX,SPACES        FAX RECORD NOT FOUND                         
****     BE    PCP05               LEAVE FX=XXXXX IN WUDESTID                   
****     MVC   WUDESTID+5(16),TOFAX                                             
****     B     PCP05                                                            
*                                                                               
*                                                                               
PCP05    MVC   WUBILL(1),QMEDIA                                                 
         MVC   WUBILL+1(3),QCLIENT                                              
         MVC   WUBILL+4(3),QPRODUCT                                             
*                                                                               
         MVC   WUDEST(12),MYFAX                                                 
*                                                                               
         OC    WUBILL(7),SPACES                                                 
         MVI   WUEND,C'.'                                                       
         MVC   WUEND+1(7),WUEND                                                 
*                                                                               
         MVC   P,MYPLINE                                                        
         GOTO1 REPORT             GO TO "REAL" REPORT                           
*                                                                               
         BAS   RE,SETEDT           SET EDICT DATA IN MYPLINE                    
*                                                                               
*                                                                               
         MVC   P,MYPLINE                                                        
         GOTO1 REPORT             GO TO "REAL" REPORT                           
         MVC   MYPLINE,SPACES                                                   
         MVC   MYPLINE(10),=C'/FORM/PAGE'                                       
         MVC   P,MYPLINE                                                        
         GOTO1 REPORT             GO TO "REAL" REPORT                           
         B     PC10X                                                            
*                                                                               
*                                                                               
PC10P8   MVC   MYPLINE,SPACES                                                   
         MVC   MYPLINE(5),=C'/PAGE'                                             
         MVC   P,MYPLINE                                                        
         MVI   LINE,1   SO THIS WON'T CAUSE REPORT TO GO TO NEW PAGE            
         GOTO1 REPORT             GO TO "REAL" REPORT                           
*                                                                               
PC10X    DS    0H                 I MUST PRINT MY OWN HEADLINES                 
         MVI   LINE,3      NOT REALLY SURE WHY THIS WORKS                       
*                          BUT IT IS NEEDED FOR THE FAXED LETTER                
*                          TO LOOK LIKE THE NON FAXED LETTER                    
         MVC   P,MYHEAD1                                                        
         MVC   PSECOND,MYHEAD2                                                  
         GOTO1 REPORT                                                           
         GOTO1 REPORT             SKIP A LINE                                   
*                                                                               
         MVC   P,SVP              RESTORE "REAL' P AND PSECOND                  
         MVC   PSECOND,SVP2                                                     
         MVC   SPACING,SVSPACE    RESTORE "REAL' SPACING                        
*                              SINCE ALL THE ABOVE ARE NOT "REAL"               
*                              LINES                                            
*                                                                               
PC12     DS    0H                                                               
         GOTO1 REPORT          FINALLY GO THE "REAL" REPORT                     
*                              WITH "REAL" PRINT LINES                          
         XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
SETEDT   NTR1                                                                   
         MVC   MYPLINE,SPACES                                                   
         MVC   MYPLINE(5),=C'++DDS'                                             
         MVC   MYPLINE+6(8),=C'PPVNVTRN'                                        
         MVC   PPEDMED,QMEDIA                                                   
         MVC   PPEDCLT,QCLIENT                                                  
         MVC   PPEDPRD,QPRODUCT                                                 
         MVC   PPEDEST,QEST                                                     
         MVC   PPEDREQ,QUESTOR                                                  
*                                                                               
         CLI   QPUB,C' '          SEE IF PUB GIVEN                              
         BNH   SETEDX                                                           
         XC    REPWORK(6),REPWORK                                               
         MVC   REPWORK+10(11),QPUB                                              
*                                                                               
         CLC   QPUB+8(3),=C'ZZZ'          SEE IF DOING ALL ZONES/EDTS           
         BNE   *+10                                                             
         MVC   REPWORK+18(3),SPACES                                             
*                                                                               
         GOTO1 PUBVAL,DMCB,QPUB,REPWORK                                         
*                                                                               
         CLC   QPUB+8(3),=C'ZZZ'          SEE IF DOING ALL ZONES/EDTS           
         BNE   *+10                                                             
         MVC   REPWORK+4(2),=X'FFFF'                                            
*                                                                               
         GOTO1 PUBEDIT,DMCB,REPWORK,PPEDPUB                                     
*                                                                               
SETEDX   XIT1                                                                   
         EJECT                                                                  
GETFAX   NTR1                                                                   
         MVC   TOFAX,SPACES                                                     
         MVC   SIOKEY,KEY       SAVE KEY                                        
GETFAXB  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTFXKEY,R4                                                       
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,QAGENCY                                                  
         MVC   CTFXCODE,MYFAX+3                                                 
         OC    CTFXCODE,SPACES      SINCE MYFAX MAY NOT HAVE SPACES             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',KEY,IO                     
         CLC   IO(18),KEYSAVE      COMPARE 7-BYTE FAX CODE                      
         BNE   GETFAXX                                                          
         LA    R4,IO                                                            
         LA    R6,CTFXEL1                                                       
         B     GETFX4                                                           
         SPACE 1                                                                
GETFX2   ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         SPACE 1                                                                
GETFX4   CLI   0(R6),0                                                          
         BE    GETFAXX                                                          
         CLI   0(R6),CTFX1ELQ                                                   
         BE    GETFXNO                                                          
         B     GETFX2                                                           
         SPACE 1                                                                
         USING CTFX1EL,R6                                                       
GETFXNO  ZIC   R1,CTFX1LEN         FAX NUMBER                                   
         SH    R1,=H'3'                                                         
         CH    R1,=H'24'                                                        
         BL    *+8                                                              
         LA    R1,24                                                            
         EX    R1,*+8                                                           
         B     GETFX2                                                           
         MVC   TOFAX(0),CTFX1NUM                                                
         SPACE 1                                                                
*                                                                               
GETFAXX  MVC   KEY,SIOKEY      RESTORE MY KEY                                   
         XIT                                                                    
*        FREPORT LTORG                                                          
         LTORG                                                                  
*                                                                               
FRSVFAX  DS    CL12                                                             
TOFAX    DS    CL16                                                             
SIOKEY   DS    CL32         SAVED KEY                                           
IO       DS    CL500        FOR CONTROL FAX RECORD                              
*                                                                               
         EJECT                                                                  
         DS    0D                                                               
FREPWORK DS    1000C                                                            
*                   DSECT FOR THIS MODULE                                       
         SPACE 3                                                                
PPREPD   DSECT                                                                  
REPDUB   DS    D                                                                
REPWORK  DS    CL40                                                             
CONTROL  DS    CL4                                                              
PROGSW   DS    CL1                                                              
FRDMCB   DS    6F                                                               
**FAX                                                                           
MYPLINE  DS    CL132                                                            
*                                                                               
         ORG   MYPLINE                                                          
WUORIG   DS    CL4             WAS AGYORIG                                      
WU9FB    DS    CL5             OLD - WAS USED BY GRAFNET (/9FB)                 
WUDESTID DS    CL24            C'FAX  AAA-NNN-NNNN'  (FAX NUMBER)               
         DS    CL5             SPARE                                            
WUDEST   DS    CL16            FORMATTED DESTINATION  (FAX NUMBER)              
*                                                                               
WUBILL   DS    CL20            QMED/QCLIENT/QPRD                                
WUEND    DS    CL8                                                              
*                                                                               
         ORG   MYPLINE+15                                                       
       ++INCLUDE PPEDICT                                                        
         ORG                                                                    
MYHEAD1  DS    CL132                                                            
MYHEAD2  DS    CL132                                                            
*                                                                               
SVP      DS    CL132             USED TO SVAVE "REAL" PRINT LINES               
SVP2     DS    CL132                                                            
SVSPACE  DS    XL1               SAVE "REAL" SPACING                            
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
*        NV REPORT WORK AREA                                                    
NVWORK   CSECT                                                                  
         DS    9200C                                                            
       ++INCLUDE NVWORKDF                                                       
LETRTAB  CSECT                                                                  
         DC    990000X'00'    NOTE-CAN ONLY HOLD 4500 INSERTIONS                
*                             4500 X 220= 880000                                
*                             WHEN CHANGING - CHANGE MAXLET ALSO                
*                                                                               
TABDSC   DSECT                                                                  
TABLEN   DS    0CL220                                                           
TABFAX   DS    CL12                                                             
TABMED   DS    CL1                                                              
TABNAME  DS    CL30                                                             
TABAD1   DS    CL30                                                             
TABAD2   DS    CL30                                                             
TABATTN  DS    CL20                                                             
TABCLI   DS    CL3                                                              
TABPUB   DS    CL6                                                              
TABPRD   DS    CL3                                                              
TABEST   DS    XL2                                                              
TABINS   DS    XL3                                                              
TABSUBL  DS    XL1                                                              
TABSPC   DS    CL20                                                             
TABLIO   DS    CL11                                                             
TABPUBN  DS    CL20                                                             
TABZONN  DS    CL20                                                             
*                       ROGPROF+10  G=GROSS,N=NET,1=G-CD,2=N=-CD                
TABORD   DS    CL4                  ORDERED                                     
TABPAID  DS    CL4                  PAID                                        
*                                                                               
TBLLEN   EQU   220                                                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPNEWFILE                                                      
         PRINT ON                                                               
*                                                                               
PNVTREC  DSECT                     *** PRINTPAK - NVTEST  RECORD ***            
PNVTKEY  DS    0CL25                                                            
PNVTKAGY DS    CL2       A         AGENCY CODE                                  
PNVTKMED DS    CL1       A         MEDIA CODE                                   
PNVTKRCD DS    X'41'     B         RECORD CODE                                  
PNVTKCLI DS    CL3       AN        CLIENT CODE                                  
         DS    CL3                 SPARE                                        
         DS    15X'00'   B         REMAINDER OF KEY                             
*                                                                               
PNVTLEN  DS    CL2       B         RECORD LENGTH                                
*                                                                               
PNVTCTRL DS    CL2       B         CONTROL BYTES                                
         DS    CL4       B         DISK ADDRESS FOR LINKED RECORDS              
*                                                                               
PNVTCELM DS    0C                                                               
         DS    X'40'     B         ELEMENT CODE                                 
         DS    CL1       B         ELEMENT LENGTH (VARIABLE)                    
PNVTDT   DS    0C        AN        COMMENT LINE DATA                            
*                                  THERE CAN BE A MAX OF 16 ELEMENTS            
*                                  EACH ELEMENT CAN HAVE A MAX OF 70            
*                                  BYTES                                        
*                                  SPACE LINES = LENGTH OF 3 AND                
*                                  FIRST BYTE OF PNVTDT = BINARY ZERO           
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
*                                                                               
       ++INCLUDE CTGENFAX                                                       
*                                                                               
       ++INCLUDE PINVREC                                                        
*                                                                               
       ++INCLUDE PPGENPNV                                                       
*                                                                               
       ++INCLUDE PPGENPBNV                                                      
*                                                                               
       ++INCLUDE DDCOMFACSD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031PPREPNV02 07/09/14'                                      
         END                                                                    
