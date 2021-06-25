*          DATA SET ACREP8A02  AT LEVEL 026 AS OF 01/31/05                      
*PHASE AC8A02A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE UNDERLIN                                                               
         TITLE 'ANALYSIS OF INCOME SUSPENSE'                                    
AC8A02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC8A**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
         SPACE 1                                                                
         USING AC8AD,RC                                                         
         LA    RC,SPACEND                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING AC8A02+4096,R9                                                   
         SPACE 4                                                                
**********************************************************************          
*        READS SJ AND SK TRANSACTIONS                                *          
*                                                                    *          
*        IF REQUESTED OFF OF SJ LEDGER MONACC WILL READ SJ AND       *          
*        THE PROGRAM WILL READ SK.                                   *          
*        IF REQUESTED OFF SK, MONACC READS SK AND THE PROGAM READS   *          
*        SJ.                                                         *          
*                                                                    *          
*        OPTION 1     'D'  PRINT THE DIFFERENCES FOUND BETWEEN SJ    *          
*                          AND SK ONLY.                              *          
*                                                                    *          
*                                                                    *          
*        OPTION 2     'P'  DDS ONLY OPTION TO MAKE THE CORRECTING    *          
*                          POSTINGS TO SK SI.                        *          
*                          CAN ONLY BE RUN WHEN REQUESTED OFF THE SJ *          
*                          LEDGER ( THE WHOLE LEDGER ONLY).          *          
*                                                                    *          
**********************************************************************          
         EJECT                                                                  
*              ROUTINE FOR RUN FIRST                                            
         SPACE 1                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   REQF                                                             
         SPACE 1                                                                
         LA    RE,RELOTAB          RELOCATE MY A TYPES                          
         LA    R1,ATYPES                                                        
RELOOP   L     RF,0(RE)                                                         
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
         SPACE 1                                                                
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)            SAVE RC                                      
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX            STORE ADDR OF BOX ROUTINE                    
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
         SPACE 1                                                                
         MVI   LINE,1              SET LINE NUMBER                              
*                                                                               
         SR    R0,R0               GET MAIN STORAGE                             
         LA    R4,MAINTAB                                                       
*                                                                               
RNF03    CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    RNF05                                                            
         A     R0,0(R4)            ADD THE LENGTH OF EACH TABLE                 
         LA    R4,L'MAINTAB(R4)                                                 
         B     RNF03                                                            
*                                                                               
RNF05    ST    R0,MAINLEN          SAVE LENGTH OF TABLE                         
         GETMAIN  R,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,MAINBGN          START OF AREA                                
         LA    R4,MAINTAB                                                       
*                                                                               
RNF07    L     R3,4(R4)                                                         
         ST    R1,0(R3)            A(START OF THIS TABLE)                       
         L     R0,0(R4)            LENGTH OF THIS TABLE                         
         AR    R1,R0               R1 TO NEXT AREA                              
         LA    R4,L'MAINTAB(R4)                                                 
         CLI   0(R4),X'FF'                                                      
         BNE   RNF07                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FOR REQUEST FIRST                                        
         SPACE 1                                                                
REQF     CLI   MODE,REQFRST                                                     
         BNE   PROCAC                                                           
*                                                                               
         MVI   RCSUBPRG,0                                                       
         MVI   REPORTSW,C'N'               NOT DOING REPORTS YET                
*                                                                               
         LA    R1,COUNTS                   CLEAR DET LINE COUNTS                
         LA    R0,COUNTNUM                                                      
REQF00   ZAP   0(3,R1),=P'0'                                                    
         LA    R1,3(R1)                                                         
         BCT   R0,REQF00                                                        
*                                                                               
         CLI   QLEDGER,C'J'                                                     
         BE    *+8                                                              
         MVI   RCSUBPRG,1                                                       
*                                                                               
         L     R4,ADCMPNAM              ** COMPANY NAME FOR HEADLINES**         
         LA    R5,COMPNAM                                                       
         USING ACNAMED,R4                                                       
         MVC   0(36,R5),SPACES                                                  
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ACNMNAME                                                 
         SPACE 1                                                                
         MVI   POSTCOST,C'N'                                                    
         L     R4,ADCMPEL                  IS COMPANY ON COSTING                
         USING ACCOMPD,R4                                                       
         TM    ACMPSTAT,X'10'                                                   
         BZ    *+8                                                              
         MVI   POSTCOST,C'Y'                                                    
*                                       ** INIT FOR SORT **                     
         SPACE 1                                                                
         XC    ALSORT,ALSORT               CLEAR A(LAST SORT)                   
         LA    R3,SORTCRD1                 SORT CARD FOR SJ                     
         CLI   QLEDGER,C'J'                REQUESTED OFF SJ                     
         BE    *+8                         YES - CONTINUE                       
         LA    R3,SORTCRD2                 SORT CARD FOR SK                     
         SPACE 1                                                                
         LA    R1,SRTLNQ                   SORT RECORD LENGTH                   
         CVD   R1,DUB                      CONVERT REC LEN TO CHARS             
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(3),DUB+6(2)                                           
         GOTO1 SORTER,DMCB,(R3),RECCARD,0                                       
         SPACE 1                                                                
*                                       ** CLEAR CODE /NAME TABLES **           
         USING BIND,R5                                                          
         L     R5,AINCLST                                                       
         XC    BININ,BININ                 CLEAR TABLE                          
         L     R5,ACLILST                                                       
         XC    BININ,BININ                                                      
         L     R5,APRDLST                                                       
         XC    BININ,BININ                                                      
         L     R5,AJOBLST                                                       
         XC    BININ,BININ                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAY)                                 
*                                                                               
         CLI   QOPT2,C'P'                  MAKE POSTINGS                        
         BNE   REQF99                                                           
         CLI   QLEDGER,C'J'                ONLY ON FULL SJ REQUEST              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   QACCOUNT,C' '                                                    
         BE    REQF02                                                           
         MVI   QOPT2,C' '                  TURN OFF POSTING OPTION              
         B     REQF99                                                           
*                                                                               
REQF02   DC    0H'0'              ** OPEN WORKER POSTING FILE **                
*                                                                               
         MVC   ID(2),ORIGINUM                                                   
         PACK  DUB(2),RCDATE+3(3)                                               
         MVC   ID+6(1),DUB                                                      
         MVI   ID+7,C'P'                                                        
         MVC   ID+2(3),=C'A8A'             WORK FILE ID                         
         MVC   COMMAND,=CL6'OPEN'                                               
         BAS   RE,FILE                                                          
*                                          MOS FROM TODAY'S DATE                
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK)                                  
         MVC   MOS(1),WORK+1                                                    
         MVC   MOS+1(1),WORK+3                                                  
         CLI   WORK+2,C'1'                                                      
         BNE   REQF04                                                           
         MVI   MOS+1,C'A'                                                       
         CLI   WORK+3,C'0'                                                      
         BE    REQF04                                                           
         MVI   MOS+1,C'B'                                                       
         CLI   WORK+3,C'1'                                                      
         BE    REQF04                                                           
         MVI   MOS+1,C'C'                                                       
REQF04   GOTO1 (RF),(R1),(0,WORK),(1,DATPK) TODAY'S DATA PACKED                 
*                                                                               
         LA    R1,GROUPTAB                 12 GROUP NAME TABLE                  
         MVI   0(R1),X'FF'                 MARK THE END                         
         ZAP   TAPECNT,=P'0'               CLEAR WORK REC COUNT                 
         ZAP   TAPECASH,=P'0'              AND WORK DEBIT COUNT                 
         ZAP   SIREF,=P'0'                 AND SI TRNSREF COUNT                 
REQF99   B     XIT                                                              
         EJECT                                                                  
PROCAC   CLI   MODE,PROCACC                                                     
         BNE   TRANS                                                            
         SPACE 1                                                                
         LA    R6,SRTWRK                                                        
         USING SRTD,R6                                                          
         BAS   RE,CLERSORT              *CLEAR SORT WORK AREA*                  
         SPACE 3                                                                
*                              *************************************            
*                              *   *TABLE OF REQUESTED ACCOUNTS*   *            
*                              *                                   *            
*                              * EITHER SJ JOBS OR SK INCOME       *            
*                              * ACCOUNTS DEPENDING ON WHICH LEDGER*            
*                              * REQUESTED.                        *            
*                              *************************************            
         LA    R5,WORK                                                          
         MVC   WORK,SPACES               CLEAR TAB ENTRY WRK AREA               
         L     R4,ADACC                                                         
         MVC   0(15,R5),0(R4)            MOVE ACCOUNT CODE INTO TAB             
         MVI   ELCODE,X'20'              NAME ELEMENT                           
         BAS   RE,GETEL                                                         
         BNE   PROC02                    NO NAME ELEMENT                        
         SPACE 1                                                                
         USING ACNAMED,R4                                                       
         ZIC   R1,ACNMLEN                NAME                                   
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   15(0,R5),ACNMNAME                                                
PROC02   L     R3,AINCLST                TABLE OF SK INCOME ACCOUNTS            
         CLI   QLEDGER,C'J'                                                     
         BNE   PROC08                                                           
         CLI   QOPT2,C'P'                MAKE CORRECTING POSTINGS               
         BNE   PROC07                    NO - SKIP X'24' EL LOOKUP              
         L     R2,ADPROFIL               ADDR OF COMPOSITE PROFLE               
         USING ACPROFD,R2                                                       
         CLI   ACPREL,X'24'              MAKE SURE YOU GOT A X'24' EL           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   COSTCODE,ACPRCOST         SAVE THE 1C COST CODE                  
         MVC   ANALCODE,ACPRUNIT         CLIENT OFFICE CODE                     
PROC07   L     R3,AJOBLST                TABLE OF SJ JOBS                       
*                                                                               
PROC08   DS    0H                                                               
         GOTO1 BINADD,DMCB,(R5),(R3)                                            
         B     XIT                                                              
         EJECT                                                                  
TRANS    CLI   MODE,PROCTRNS                                                    
         BNE   LOOKUP                                                           
         LA    R6,SRTWRK               SORT WORK AREA                           
         L     R4,ADTRANS              ADDR OF TRANSACTION                      
         BAS   RE,CLERSORT             *CLEAR SORT WORK AREA*                   
         USING TRANSD,R4                                                        
         CLI   0(R4),X'44'                                                      
         BNE   XIT                                                              
         LR    R3,R4                                                            
         SH    R3,DATADISP                                                      
         USING ACKEYD,R3                                                        
         CLI   QLEDGER,C'J'            REQUESTED OFF SJ                         
         BNE   TRNS20                  NO - REQ OFF SK                          
*                                                                               
         CLC   ACKEYCON+1(2),=C'SK'    IS CONTRA UL SK                          
         BE    TRNS04                  YES - WE WANT IT                         
         CLC   ACKEYCON+1(2),=C'1R'    IS CONTRA UL 1R                          
         BE    TRNS03                  YES - INVESTIGATE FURTHER                
         CLC   ACKEYWRK,=C'99'         IS IT WORKCODE 99                        
         BNE   XIT                     NO - WE DONT WANT IT                     
         CLI   TRNSLEN,121                                                      
         BNE   XIT                                                              
         OC    TRNSNARR+37(2),TRNSNARR+37  DATE SK POSTED TO SI                 
         BNZ   XIT                                                              
         OC    TRNSNARR+45(6),TRNSNARR+45  IF SK AMT ZERO WE DONT WANT          
         BZ    XIT                                                              
         CP    TRNSNARR+45(6),=P'0'                                             
         BZ    XIT                                                              
*                                                                               
         L     R2,ADCOMP               LOOK UP MEDIA TO GET SK(SI)              
         AH    R2,DATADISP                                                      
TRNS02   CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                    MISSING MEDIA ELEMENT                    
         CLI   0(R2),X'11'                                                      
         BE    TRNS02C                                                          
*                                                                               
TRNS02A  ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     TRNS02                                                           
*                                                                               
         USING ACMEDIAD,R2                                                      
TRNS02C  CLC   ACMDCODE,ACKEYACC+9     MATCH JOB LETTER TO MEDIA RECRD          
         BNE   TRNS02A                                                          
         MVC   SRTCNTRA(2),=C'SK'                                               
         MVC   SRTCNTRA+2(12),ACMDCOMM+3  MOVE IN SK ACCOUNT                    
         B     TRNS04                                                           
*                                                                               
TRNS03   L     R4,ADTRANS              (CONTRA OF 1R )                          
         MVI   ELCODE,X'4C'            LOOK FOR REAL SK CONTRA                  
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         MVC   SRTCNTRA,SPACES                                                  
         ZIC   R1,1(R4)                FOUND TRSDESCD ELEMENT                   
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRTCNTRA(0),TRSDACCS-TRSDESCD(R4) REAL SK ACCOUNT                
*                                                                               
         CLC   SRTCNTRA(2),=C'SK'                                               
         BNE   XIT                     NOT SK CONTRA                            
*                                                                               
TRNS04   OC    ACDTUSED,ACDTUSED       FULLY BILLED                             
         BNZ   XIT                     YES - WE DONT WANT IT                    
*                                                                               
TRNS06   L     R4,ADTRANS              ADDR OF TRANSACTION                      
         MVI   ELCODE,X'60'            LOOK FOR TRANS STATUS EL                 
         BAS   RE,NEXTEL                                                        
         BNE   TRNS07                                                           
         USING TRSTATD,R4                                                       
         TM    TRSTATUS,X'40'          ITEM FOR JOINT VENTURE SKIP IT           
         BO    XIT                                                              
*                                                                               
         USING ACMD,R4                                                          
TRNS07   ZAP   TOTAL77,=P'0'                                                    
         L     R4,AMONACC                                                       
         L     R4,ACMAPRO2                                                      
         CLI   0(R4),PTAELQ        X'77' - PTA ELEMENT                          
         B     TRNS08A                                                          
*                                                                               
         USING PTAELD,R4                                                        
TRNS08   MVI   ELCODE,PTAELQ                                                    
         BAS   RE,NEXTEL                                                        
TRNS08A  BNE   TRNS09B                                                          
*                                                                               
         TM    PTASTAT1,PTASPEND   SKIP PENDING                                 
         BO    TRNS08                                                           
         CLI   PTATYPE,PTATWOF     IS THIS A WRITEOFF?                          
         BE    TRNS08B             YES - SKIP IT                                
         CLI   PTATYPE,PTATWOFR                                                 
         BE    TRNS08B                                                          
         CLI   PTATYPE,PTATRAL     ALLOCATED TO BILL?                           
         BNE   TRNS08              NO - USE IT                                  
*                                                                               
TRNS08B  AP    TOTAL77,PTANET                                                   
         B     TRNS08                                                           
         DROP  R4                                                               
*                                                                               
TRNS09B  L     R4,ADTRANS                                                       
         CP    TOTAL77,TRNSAMNT-TRANSD(L'TRNSAMNT,R4)                           
         BE    XIT                                                              
         EJECT                                                                  
*                                    *** REQUESTED OFF SJ LEDGER ***            
TRNS10   L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         MVC   SRTCODE,ACKEYACC+3      CLT/PROD/JOB                             
         OC    SRTCNTRA,SRTCNTRA       DID WE FIND THRU X'4C'                   
         BNZ   *+10                    YES- WE ALREADY HAVE SK CNTRA            
         MVC   SRTCNTRA,ACKEYCON+1     SK CONTRA                                
         CLI   QOPT2,C'P'              MAKE POSTINGS                            
         BNE   TRNS14                  NO - DONT BOTHER WITH COST POST          
         MVC   SRTANAL,ANALCODE        ANALYSIS CODE                            
         CLI   POSTCOST,C'Y'           IS COMPANY ON COSTING                    
         BNE   TRNS14                  NO - DONT BOTHER 12 ACCOUNT              
         MVC   SRTCOST,COSTCODE        MOVE IN 1C COSTING ACCOUNT               
*                                                                               
*                                      * LOOK UP 12 COSTING GROUP *             
         L     R7,ACREC                                                         
         MVC   0(42,R7),SPACES                                                  
         MVC   0(1,R7),RCCOMPFL        COMPANY                                  
         MVC   1(14,R7),SRTCNTRA       SK ACCOUNT                               
         L     R5,AINCLST              LOOK IN TABLE FIRST                      
*                                                                               
         USING BIND,R5                                                          
         MVC   DMCB+8(16),BININ        MOVE IN PARMS 3,4,5,6                    
         L     R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(R7),(R2)                                           
         CLI   DMCB,0                                                           
         BNE   TRNS12                                                           
         L     R5,DMCB                                                          
         USING INCCDE,R5                                                        
         MVC   SRT12,INCCOST                                                    
         B     TRNS12A                                                          
*                                                                               
TRNS12   MVI   2(R7),C'I'              MAKE IT SI                               
         BAS   RE,READ                 READ FOR THE SI ACCOUNT                  
         L     R4,ACREC                                                         
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACSTATD,R4                                                       
         MVC   SRT12,ACSTCOST          12 COSTING GROUP                         
         MVC   WORK,SPACES             CLEAR TAB ENTRY WORK AREA                
         USING INCCDE,R5                                                        
         LA    R5,WORK                                                          
         MVC   INCKEY(1),RCCOMPFL      COMPANY                                  
         MVC   INCKEY+1(14),SRTCNTRA   SK ACCOUNT                               
         MVC   INCCOST,ACSTCOST        12 COSTING GROUP                         
         GOTO1 BINADD,DMCB,(R5),AINCLST                                         
*                                                                               
TRNS12A  L     R5,AMONACC                                                       
         USING ACMD,R5                                                          
         L     R1,ACMALTN               ADDR LAST TRANS READ BY MONACC          
         L     R7,ACREC                                                         
         MVC   0(42,R7),SPACES                                                  
         MVC   0(42,R7),0(R1)                                                   
         BAS   RE,READ                 RESET DATAMGR FOR MONACC                 
*                                                                               
TRNS14   L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         MVC   SRTDATE,TRNSDATE        TRANSACTION DATE                         
         MVC   SRTREF,TRNSREF          AND REFERENCE NUMBER                     
         CLC   ACKEYWRK,=C'99'         WORKCODE 99                              
         BNE   TRNS14B                                                          
         ZAP   DUB,TRNSNARR+45(6)      SK AMT                                   
         B     TRNS14C                                                          
TRNS14B  ZAP   DUB,TRNSAMNT                                                     
         TM    TRNSSTAT,X'80'          IS IT A DEBIT                            
         BO    *+10                    YES- CONTINUE                            
         MP    DUB,=P'-1'              NO - MAKE IT MINUS                       
TRNS14C  AP    SRTAMNT,DUB             ADD AMOUNT TO SORTREC                    
         DROP  R5                                                               
*                                                                               
         USING ACMD,R4                                                          
         L     R4,AMONACC                                                       
         L     R4,ACMAPRO2                                                      
         CLI   0(R4),PTAELQ        X'77' - PTA ELEMENT                          
         BNE   TRNSLAST            NONE - PUT RECORD TO SORT                    
*                                                                               
         USING PTAELD,R4                                                        
         MVI   ELCODE,PTAELQ       LOOK FOR PARTIAL BILLS                       
         B     *+12                                                             
TRNS16   BAS   RE,NEXTEL                                                        
         BNE   TRNSLAST            NONE - PUT RECORD TO SORT                    
*                                                                               
         TM    PTASTAT1,PTASPEND   PENDING - SKIP?                              
         BO    TRNS16                                                           
         OC    PTANET,PTANET       SKIP ZERO BALANCE                            
         BZ    TRNS16                                                           
         CLI   PTATYPE,PTATWOF     IGNORE W/O'S                                 
         BE    TRNS18                                                           
         CLI   PTATYPE,PTATWOFR    AND W/O RECOVERY                             
         BE    TRNS18                                                           
         CLI   PTATYPE,PTATRAL     ALLOCATED TO BILL?                           
         BNE   TRNS16              NO - SKIP                                    
*                                                                               
TRNS18   AP    SRTPART,PTANET      ADD PARTIAL BILL TO SORT REC                 
         B     TRNS16                                                           
         DROP  R4                                                               
*                                    *** REQUESTED OFF SK LEDGER ***            
TRNS20   CLC   ACKEYCON+1(2),=C'SJ'    IS CONTRA UL SJ                          
         BNE   RNL00                   NO - WE DONT WANT IT                     
         L     R4,ADTRANS              ADDR OF TRANSACTION                      
         USING TRANSD,R4                                                        
         MVC   SRTCODE,ACKEYCON+3      CLT/PROD/JOB                             
         MVC   SRTCNTRA,ACKEYACC+1     SK CONTRA                                
         MVC   SRTDATE,TRNSDATE        TRANSACTION DATE                         
         MVC   SRTREF,TRNSREF          AND REFERENCE NUMBER                     
         ZAP   DUB,TRNSAMNT                                                     
         TM    TRNSSTAT,X'80'          IS IT A CREDIT                           
         BZ    *+10                    YES- CONTINUE                            
         MP    DUB,=P'-1'              NO - MAKE IT MINUS                       
         AP    SRTSKAMT,DUB            ADD AMOUNT TO SORTREC                    
TRNSLAST BAS   RE,PUTSORT                                                       
         BAS   RE,CLERSORT             *CLEAR SORT WORK AREA*                   
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                      ********************************         
*                                      * IF REQUESTED OFF SJ 'LOOKUP' *         
*                                      * WILL READ SK.                *         
*                                      * IF REQUESTED OFF SK 'LOOKUP' *         
*                                      * WILL READ SJ.                *         
*                                      ********************************         
LOOKUP   CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         LA    R6,SRTWRK                                                        
         USING SRTD,R6                                                          
         BAS   RE,CLERSORT                                                      
         USING ACKEYD,R7                                                        
         L     R7,ACREC                                                         
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES   CLEAR KEY                   
         MVC   ACKEYACC(1),RCCOMPFL            MOVE IN COMPANY                  
         MVC   ACKEYACC+1(2),=C'SK'            U/L SK                           
         CLI   QLEDGER,C'J'                    REQUESTED OFF SJ                 
         BE    *+10                            YES - THEN READ SK               
         MVC   ACKEYACC+1(2),=C'SJ'            NO  - READ SJ                    
         MVI   ACKEYACC+3,X'41'                                                 
*                                                                               
LOKUP01  BAS   RE,HIGH                                                          
         B     *+8                                                              
LOKUP03  BAS   RE,SEQ                                                           
         CLC   ACKEYACC(3),SAVEKEY             SAME CO/U/L                      
         BE    LOKUP06                         YES CONTINUE                     
         OC    SRTWRK(SRTDLNQ),SRTWRK                                           
         BE    *+8                                                              
         BAS   RE,PUTSORT                      PUT LAST ONE TO SORT             
         B     REPORT                                                           
*                                                                               
LOKUP06  CLI   QLEDGER,C'J'                    REQUESTED OFF SJ                 
         BNE   LOKUP08                         NO - LOOK FOR SK CTRA            
         CLC   ACKEYCON+1(2),=C'SJ'            IS CONTRA SJ                     
         BNE   LOKUP03                         NO - READ NEXT RECORD            
         B     LOKUP10                                                          
*                                                                               
LOKUP08  CLC   ACKEYCON+1(2),=C'SK'            IS CONTRA SK                     
         BE    LOKUP08E                        YES - CONTINUE                   
         CLC   ACKEYCON+1(2),=C'1R'            IS CONTRA UL 1R                  
         BE    LOKUP08D                        YES- INVESTIGATE FURTHER         
*                                                                               
         CLC   ACKEYWRK,=C'99'                 IS IT WORKCODE 99                
         BNE   LOKUP03                         NO - WE DONT WANT IT             
         L     R4,ACREC                                                         
         MVI   ELCODE,X'44'                                                     
         BAS   RE,GETEL                                                         
         BNE   LOKUP03                                                          
         USING TRANSD,R4                                                        
         CLI   TRNSLEN,121                                                      
         BNE   LOKUP03                                                          
         OC    TRNSNARR+37(2),TRNSNARR+37      DATE SK POSTED TO SI             
         BNZ   LOKUP03                                                          
         OC    TRNSNARR+45(6),TRNSNARR+45      SK AMT '0' WE DONT WANT          
         BZ    LOKUP03                                                          
         CP    TRNSNARR+45(6),=P'0'                                             
         BZ    LOKUP03                                                          
*                                                                               
         L     R2,ADCOMP                       LOOK UP MEDIA GET SK(SI)         
         AH    R2,DATADISP                                                      
LOKUP08A CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                            MISSING MEDIA ELEMENT            
         CLI   0(R2),X'11'                                                      
         BE    LOKUP08C                                                         
*                                                                               
LOKUP08B ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     LOKUP08A                                                         
*                                                                               
         USING ACMEDIAD,R2                                                      
LOKUP08C CLC   ACMDCODE,ACKEYACC+9     MATCH JOB LETTER TO MEDIA RECRD          
         BNE   LOKUP08B                                                         
         MVC   SRTCNTRA(2),=C'SK'                                               
         MVC   SRTCNTRA+2(12),ACMDCOMM+3  MOVE IN SK ACCOUNT                    
         B     LOKUP08E                                                         
         DROP  R2                                                               
*                                                                               
LOKUP08D L     R4,ACREC                        ADDR OF TRANSACTION              
         MVI   ELCODE,X'4C'                    LOOK FOR REAL SK CONTRA          
         BAS   RE,GETEL                                                         
         BNE   LOKUP03                         NO - READ NEXT REC               
         MVC   SRTCNTRA,SPACES                                                  
         ZIC   R1,1(R4)                        FOUND TRSDESCD ELEMENT           
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRTCNTRA(0),TRSDACCS-TRSDESCD(R4) REAL SK ACCOUNT                
*                                                                               
         CLC   SRTCNTRA(2),=C'SK'                                               
         BNE   LOKUP26                         NOT SK CONTRA                    
*                                                                               
LOKUP08E OC    ACDTUSED,ACDTUSED               FULLY BILLED                     
         BNZ   LOKUP26                         YES - WE DONT WANT IT            
*                                                                               
LOKUP10  L     R5,AJOBLST                      ADDR OF JOB CODE TAB             
         CLI   QLEDGER,C'J'                                                     
         BE    *+8                                                              
         L     R5,AINCLST                      ADDR OF SK ACCOUNTS              
         USING BIND,R5                                                          
         MVC   WORK(15),ACKEYCON               CONTRA INTO WORK                 
         OC    SRTCNTRA,SRTCNTRA               DID WE FIND SK CONTRA            
         BZ    *+10                            NO - USE ACKEYCON                
         MVC   WORK+1(14),SRTCNTRA             FROM X'4C' OR WC 99              
*                                                                               
         MVC   DMCB+8(16),BININ                MOVE IN PARMS 3,4,5,6            
         L     R3,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,WORK,(R3)                                           
         CLI   DMCB,0                          DO WE WANT THIS ACCOUNT          
         BNE   LOKUP26                         NOT FOUND                        
*                                                                               
         LA    R6,SRTWRK                                                        
         USING SRTD,R6                                                          
*                                                                               
         CLI   QLEDGER,C'J'                    REQUESTED OFF SJ                 
         BNE   LOKUP16                         NO - LOOK FOR SK CTRA            
*                                              ********************             
*                                              *** READING  S K ***             
*                                              ********************             
         L     R4,ACREC                                                         
         MVI   ELCODE,X'44'                    TRANSACTION ELEMENTS             
         BAS   RE,GETEL                                                         
         BNE   LOKUP03                                                          
         USING TRANSD,R4                                                        
*                                                                               
         ZAP   DUB,TRNSAMNT                                                     
         TM    TRNSSTAT,X'80'                  IS IT A CREDIT                   
         BZ    *+10                            YES- CONTINUE                    
         MP    DUB,=P'-1'                      NO - MAKE IT MINUS               
         AP    SRTSKAMT,DUB                    SK POSTING INTO SRTREC           
         MVC   SRTCODE,ACKEYCON+3              CLI/PRD/JOB IN SORTREC           
         MVC   SRTCNTRA,ACKEYACC+1             SK ACCOUNT INTO SRTREC           
         MVC   SRTDATE,TRNSDATE                TRANSACTION DATE                 
         MVC   SRTREF,TRNSREF                  AND REFERENCE NUMBER             
         MVC   SRTANAL,TRNSANAL                AND ANALYSIS CODE                
         B     LOKUP25                                                          
*                                                                               
LOKUP16  L     R4,ACREC                        ********************             
         MVI   ELCODE,X'44'                    *** READING  S J ***             
         BAS   RE,GETEL                        ********************             
         BNE   LOKUP26                                                          
         USING TRANSD,R4                                                        
         MVC   SRTCODE,ACKEYACC+3              CLT/PROD/JOB                     
         OC    SRTCNTRA,SRTCNTRA               DID WE FIND THRU X'4C'           
         BNZ   *+10                            YES-ALREADY HAVE CNTRA           
         MVC   SRTCNTRA,ACKEYCON+1             SK CONTRA                        
         MVC   SRTDATE,TRNSDATE                TRANSACTION DATE                 
         MVC   SRTREF,TRNSREF                  AND REFERENCE NUMBER             
         CLC   ACKEYWRK,=C'99'                 WORKCODE 99                      
         BNE   LOKUP16C                                                         
         ZAP   DUB,TRNSNARR+45(6)              SK AMT                           
         B     LOKUP16E                                                         
LOKUP16C ZAP   DUB,TRNSAMNT                                                     
         TM    TRNSSTAT,X'80'                  IS IT A DEBIT                    
         BO    *+10                            YES- CONTINUE                    
         MP    DUB,=P'-1'                      NO - MAKE IT MINUS               
LOKUP16E AP    SRTAMNT,DUB                     ADD AMOUNT TO SORTREC            
*                                                                               
* CALL PRORATA TO CONVERT TO X'77' - ELM RETURNED IN ACMAPRO2                   
*                                                                               
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         GOTO1 ACMAPRAT,DMCB,(X'C0',ACREC),0,ADCOMFAC,0,ACMAPROB,      X        
               ACMAPRO2                                                         
*                                                                               
         L     R4,ACMAPRO2                                                      
         CLI   0(R4),PTAELQ        X'77' - PTA ELEMENT                          
         BNE   LOKUP22                                                          
         DROP  R2                                                               
*                                                                               
         USING PTAELD,R4                                                        
         MVI   ELCODE,PTAELQ       LOOK FOR PARTIAL BILLS                       
         B     *+12                                                             
LOKUP20  BAS   RE,NEXTEL                                                        
         BNE   LOKUP22             NONE - PUT RECORD TO SORT                    
*                                                                               
         TM    PTASTAT1,PTASPEND   PENDING - SKIP?                              
         BO    LOKUP20                                                          
         OC    PTANET,PTANET       SKIP ZERO BALANCE                            
         BZ    LOKUP20                                                          
         CLI   PTATYPE,PTATWOF     IGNORE W/O'S                                 
         BE    LOKUP21                                                          
         CLI   PTATYPE,PTATWOFR    AND W/O RECOVERY                             
         BE    LOKUP21                                                          
         CLI   PTATYPE,PTATRAL     ALLOCATED TO BILL?                           
         BNE   LOKUP20             NO - SKIP                                    
*                                                                               
LOKUP21  AP    SRTPART,PTANET      ADD PARTIAL BILL TO SORT REC                 
         B     LOKUP20                                                          
*                                                                               
LOKUP22  L     R4,ACREC                        ADDR OF TRANSACTION              
         MVI   ELCODE,X'60'                    LOOK FOR TRANS STATUS EL         
         BAS   RE,GETEL                                                         
         BNE   LOKUP25                                                          
         USING TRSTATD,R4                                                       
         TM    TRSTATUS,X'40'                  JOINT VENTURE SKIP IT            
         BO    LOKUP26                                                          
*                                                                               
LOKUP25  BAS   RE,PUTSORT                                                       
LOKUP26  BAS   RE,CLERSORT                                                      
         B     LOKUP03                                                          
         DROP  R7                                                               
         EJECT                                                                  
*                                      ** PRINTED REPORT **                     
REPORT   DS    0H                                                               
         MVI   REPORTSW,C'Y'              WE'RE UP TO THE REPORTS               
         LA    R1,ACCUMS                  CLEAR ALL REPORT TOT ACCUMS           
         LA    R0,BUKCOUNT                                                      
RPT0     ZAP   0(7,R1),=P'0'                                                    
         LA    R1,7(R1)                                                         
         BCT   R0,RPT0                                                          
         SPACE 1                                                                
         OC    ALSORT,ALSORT              IS THERE A LAST SORT ADDR             
         BZ    RPT99                      NO DATA                               
         SPACE 1                                                                
         LA    R5,LSTWRK                  CLEAR LAST WORK AREA                  
         XC    LSTWRK(SRTLNQ),LSTWRK                                            
         ZAP   SRTAMNT-SRTD(L'SRTAMNT,R5),=P'0'                                 
         ZAP   SRTPART-SRTD(L'SRTPART,R5),=P'0'                                 
*        ZAP   SRTTODAY-SRTD(L'SRTTODAY,R5),=P'0'                               
         ZAP   SRTSKAMT-SRTD(L'SRTSKAMT,R5),=P'0'                               
         BAS   RE,CLERSORT                                                      
         MVC   SJNAMES,XSPACES            CLEAR SAVED SJ NAMES                  
         MVC   SKNAMES,XSPACES            CLEAR SAVED SK NAMES                  
         SPACE 1                                                                
         MVC   PAGE,=H'1'                 SET PAGE TO ONE                       
         MVI   TOTSW,C'1'                 SET FOR JOB TOTAL                     
         SPACE 1                                                                
         MVC   SAVSJCOD,SPACES            CLEAR SAVED SJ CODE                   
         MVC   COMP1,RCCOMPFL             COMPANY                               
         MVC   COMP1+1(2),=C'SJ'          U/L SJ                                
         MVC   SAVSKCOD,SPACES            CLEAR SAVED SK CODE                   
         MVC   COMP2,RCCOMPFL             COMPANY                               
         MVC   COMP2+1(2),=C'SK'          U/L SK                                
         SPACE 2                                                                
         USING SRTD,R6                                                          
RPT2     GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         ST    R6,ALSORT                  ADDRESS OF LAST SORT                  
         LTR   R6,R6                                                            
         BZ    RPT9                       END OF RECORDS FROM SORT              
         MVC   SRTWRK(SRTLNQ),0(R6)       SAVE CURRENT SORT RECORD              
         OC    LSTWRK(SRTKLNQ),LSTWRK     DO I HAVE ONE SAVED                   
         BNZ   RPT5                       YES - CONTINUE                        
         MVC   LSTWRK(SRTLNQ),SRTWRK      NO  - SAVE THIS ONE                   
         B     RPT2                       AND GET NEXT                          
         SPACE 1                                                                
RPT5     CLC   LSTWRK(SRTDLNQ),SRTWRK     SAME KEY                              
         BNE   RPT8                       NO - PROCESS SAVED ONE                
         SPACE 1                                                                
         LA    R5,LSTWRK                  YES - ADD'EM UP                       
         SPACE 1                                                                
         OC    SRTCOST-SRTD(L'SRTCOST,R5),SRTCOST-SRTD(R5)                      
         BNZ   *+10                                                             
         MVC   SRTCOST-SRTD(L'SRTCOST,R5),SRTCOST       COSTING ACCT            
         OC    SRT12-SRTD(L'SRT12,R5),SRT12-SRTD(R5)                            
         BNZ   *+10                                                             
         MVC   SRT12-SRTD(L'SRT12,R5),SRT12             12 ACCT                 
         OC    SRTANAL-SRTD(L'SRTANAL,R5),SRTANAL-SRTD(R5)                      
         BNZ   *+10                                                             
         MVC   SRTANAL-SRTD(L'SRTANAL,R5),SRTANAL       OFFICE CODE             
         SPACE 1                                                                
         AP    SRTAMNT-SRTD(L'SRTAMNT,R5),SRTAMNT                               
         AP    SRTPART-SRTD(L'SRTPART,R5),SRTPART                               
*        AP    SRTTODAY-SRTD(L'SRTTODAY,R5),SRTTODAY                            
         AP    SRTSKAMT-SRTD(L'SRTSKAMT,R5),SRTSKAMT                            
         B     RPT2                       AND GET NEXT                          
*                                                                               
RPT8     LA    R1,LSTWRK                  MAKE SURE RECORD HAS DATA             
         LA    R1,SBUKLOC(R1)                                                   
         LA    R0,SBUKCONT                                                      
RPT8A    CP    0(6,R1),=P'0'                                                    
         BNZ   RPT9                                                             
         LA    R1,6(R1)                                                         
         BCT   R0,RPT8A                                                         
         B     RPT10                                                            
RPT9     CLI   QOPT1,C'D'                 PRINT DIFFERENCES ONLY OPTION         
         BNE   RPT9B                                                            
*                                            TRANS-BILLED-SK AMT=DIFFER         
         ZAP   DUB,SRTAMNT-SRTD(L'SRTAMNT,R5)                                   
         SP    DUB,SRTPART-SRTD(L'SRTPART,R5)                                   
         SP    DUB,SRTSKAMT-SRTD(L'SRTSKAMT,R5)                                 
         CP    DUB,=P'0'                  IS THERE A DIFFERENCE                 
         BE    RPT10                      NO - WE DONT WANT                     
*        SP    DUB,SRTTODAY-SRTD(L'SRTTODAY,R5) YES- SUBTRACT OUT TODAY         
*        CP    DUB,=P'0'                  IS THERE STILL A DIFFERENCE           
*        BE    RPT10                      NO - WE DONT WANT                     
*                                                                               
RPT9B    BAS   RE,PRNT                    PROCESS SAVED RECORD                  
RPT10    OC    ALSORT,ALSORT              IS IT END OF FILE                     
         BZ    RPT90                      YES - PRINT FINAL TOTALS              
         SPACE 2                                                                
         CLC   SRTCODE-SRTD(12,R5),SRTCODE SAME JOB                             
         BE    RPT12                       YES - CONTINUE                       
         MVI   TOTSW,C'1'                  NO  - SET FOR JOB TOTAL              
         BAS   RE,ACCTOT                                                        
         ZAP   JOBCOUNT,=P'0'              SET DET PRINT LINES TO ZERO          
         SPACE 1                                                                
RPT12    CLC   SRTCODE-SRTD(6,R5),SRTCODE  SAME PROD                            
         BE    RPT15                       YES - CONTINUE                       
         MVI   TOTSW,C'2'                  NO  - SET FOR PROD TOTAL             
         BAS   RE,ACCTOT                                                        
         ZAP   PRDCOUNT,=P'0'              SET DET PRINT LINES TO ZERO          
         SPACE 1                                                                
RPT15    CLC   SRTCODE-SRTD(3,R5),SRTCODE  SAME CLIENT                          
         BE    RPT18                       YES - CONTINUE                       
         MVI   TOTSW,C'3'                  NO  - SET FOR CLIENT TOTAL           
         BAS   RE,ACCTOT                                                        
         ZAP   CLTCOUNT,=P'0'              SET DET PRINT LINES TO ZERO          
         SPACE 1                                                                
RPT18    CLI   QLEDGER,C'J'                                                     
         BE    RPT40                                                            
         CLC   SRTCNTRA-SRTD(14,R5),SRTCNTRA SAME INCOME ACCOUNT                
         BE    RPT22                       YES - CONTINUE                       
         MVI   TOTSW,C'4'                  NO  - SET FOR INCOME TOTAL           
         BAS   RE,ACCTOT                                                        
         ZAP   INCCOUNT,=P'0'              SET INC PRINT LINES TO ZERO          
RPT22    CLC   SRTCNTRA-SRTD(3,R5),SRTCNTRA SAME INCOME TYPE                    
         BE    RPT40                       YES - CONTINUE                       
         MVI   TOTSW,C'5'                  NO  - SET FOR TYPE TOTAL             
         BAS   RE,ACCTOT                                                        
         ZAP   TYPCOUNT,=P'0'              SET INC PRINT LINES TO ZERO          
         SPACE 1                                                                
RPT40    MVC   LSTWRK(SRTLNQ),SRTWRK       SAVE THIS ONE                        
         B     RPT2                        AND GET NEXT.                        
         SPACE 3                                                                
*                                      *FINAL TOTALS*                           
         SPACE 1                                                                
RPT90    BAS   RE,ACCTOT                   JOB TOTAL                            
         MVI   TOTSW,C'2'                  PRD TOTAL                            
         BAS   RE,ACCTOT                                                        
         MVI   TOTSW,C'3'                  CLT TOTAL                            
         BAS   RE,ACCTOT                                                        
         CLI   QLEDGER,C'J'                SKIP SK TOTALS IF REQD SJ            
         BE    RPT99                                                            
         MVI   TOTSW,C'4'                  SK INC ACCT TOTAL                    
         BAS   RE,ACCTOT                                                        
         MVI   TOTSW,C'5'                  SK INC TYPE TOTAL                    
         BAS   RE,ACCTOT                                                        
         SPACE 1                                                                
RPT99    MVI   TOTSW,C'6'                  SET FOR REPORT TOTAL                 
         BAS   RE,ACCTOT                                                        
         GOTO1 SORTER,DMCB,=C'END'                                              
         CLI   QLEDGER,C'J'                LEDGER SJ                            
         BNE   RPT999                                                           
         CLI   QOPT2,C'P'                  MAKE CORRECTING POSTINGS             
         BNE   RPT999                                                           
         CP    TAPECNT,=P'0'               NO WORKER FILE RECORDS               
         BE    RPT999                                                           
         BAS   RE,CLOSEWK                  TOTAL AND CLOSE WORKER FILE          
RPT999   B     XIT                                                              
         EJECT                                                                  
RNL00    CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         LM    R0,R1,MAINLEN                                                    
         FREEMAIN R,LV=(0),A=(1)                                                
         B     XIT                                                              
         EJECT                                                                  
PRNT     NTR1                                                                   
*                                ***      PRINT THE DETAIL LINE    ***          
*                                *** REFRESH NAMES WHERE NECESSARY ***          
         LA    R6,LSTWRK                                                        
         MVC   XP,XSPACES                                                       
         SPACE 1                                                                
         USING ACKEYD,R7                                                        
         L     R7,ACREC                                                         
         SPACE 1                                                                
*                                         ************************              
*                                         ***  SK NAME LOOKUP  ***              
*                                         ************************              
         SPACE 1                                                                
PRNT02   CLI   QLEDGER,C'J'               SKIP SK NAME LOOKUP IF SJ RPT         
         BE    PRNT15                                                           
         SPACE 1                                                                
         CLC   SKACCT,SRTCNTRA            SK ACCOUNT SAME AS SAVED              
         BE    PRNT15                     YES- DONT REFRESH SK NAMES            
         CLC   SKTYPE,SRTSKTYP            SK INC TYPE SAME AS SAVED             
         BE    PRNT05                     YES- SKIP TYPE NAME CHECK             
         SPACE 1                                                                
         MVC   SKTYPE(12),SPACES                                                
         MVC   SKTYPE,SRTSKTYP                                                  
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES        CLEAR KEY              
         MVC   ACKEYACC(4),SAVSKCOD            MOVE IN CO/UL/TYPE               
         SPACE 1                                                                
         BAS   RE,READ                    READ FOR THAT RECORD                  
         CLI   WORK,C'*'                  ACCOUNT MISSING MESSAGE               
         BE    *+8                                                              
         BAS   RE,NAMEOUT                                                       
         MVC   TYPNAME,WORK               CLIENT NAME                           
         SPACE 1                                                                
PRNT05   MVC   SKINCOM,SRTSKACT           NEW INCOME CODE                       
         L     R5,AINCLST                 ADDR OF INCLIST                       
         USING BIND,R5                                                          
         MVC   DMCB+8(16),BININ           MOVE IN PARMS 3,4,5,6                 
         L     R3,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,SAVSKCOD,(R3)                                       
         CLI   DMCB,0                     DID WE FIND THIS INC ACT              
         BE    *+6                        YES - CONTINUE                        
         DC    H'0'                       NOT FOUND                             
         SPACE 1                                                                
         L     R5,DMCB                                                          
         USING INCCDE,R5                                                        
         MVC   INCNAME,INCNAM             NEW INCOME ACCT NAME                  
         BAS   RE,HEADUP                  HEAD UP NEW PAGE                      
         MVC   CLIENT(12),SPACES                                                
         MVC   SJNAMES,SPACES                                                   
         EJECT                                                                  
*                                         ************************              
*                                         ***  SJ NAME LOOKUP  ***              
*                                         ************************              
         SPACE 1                                                                
PRNT15   CLC   CLIENT(12),SRTCODE         SJ CLT/PRD/JOB SAME AS SAVED          
         BE    PRNT20                     YES- DONT REFRESH SJ NAMES            
         CLC   CLIENT(6),SRTCLT           IS IT SAME CLT/PRD                    
         BE    PRNT17                     YES- SKIP TO JOB NAME CHECK           
         SPACE 1                                                                
         CLC   CLIENT,SRTCLT              SAME CLIENT                           
         BE    PRNT16                                                           
         MVC   CLIENT,SRTCLT              NEW CLIENT CODE                       
         SPACE 1                                                                
*                                         LOOK IN TABLE FIRST                   
         L     R5,ACLILST                 ADDR OF CLILIST                       
         USING BIND,R5                                                          
         MVC   DMCB+8(16),BININ           MOVE IN PARMS 3,4,5,6                 
         L     R3,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,SRTCLT,(R3)                                         
         CLI   DMCB,0                                                           
         BNE   PRNT15A                    NOT FOUND IN TABLE                    
         L     R5,DMCB                                                          
         USING CLICDE,R5                                                        
         MVC   CLTNAME,CLINAM             SJ CLIENT NAME FROM TAB               
         MVC   CLIENT,CLIKEY              SJ CLIENT CODE FROM TAB               
         B     PRNT15C                                                          
         SPACE 1                                                                
*                                         NOT IN TAB - LOOK IT UP               
PRNT15A  MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES       CLEAR KEY               
         MVC   ACKEYACC(6),SAVSJCOD       MOVE IN CO/UL/CLIENT                  
         BAS   RE,READ                    READ FOR THAT RECORD                  
         CLI   WORK,C'*'                  ACCOUNT MISSING MESSAGE               
         BE    *+8                                                              
         BAS   RE,NAMEOUT                                                       
         MVC   CLTNAME,WORK               CLIENT NAME                           
         SPACE 1                                                                
         MVC   WORK,SPACES                CLEAR TAB ENTRY WORK AREA             
         USING CLICDE,R5                                                        
         LA    R5,WORK                                                          
         MVC   CLINAM,CLTNAME             UPDATE TABLE                          
         MVC   CLIKEY,SRTCLT                                                    
         GOTO1 BINADD,DMCB,(R5),ACLILST                                         
         SPACE 1                                                                
PRNT15C  CLI   QLEDGER,C'J'               REQUESTED OFF SJ                      
         BE    PRNT16                                                           
         SPACE 1                                                                
         ZIC   RF,LINE                    PRESENT LINE                          
         AH    RF,=H'5'                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   *+8                                                              
         BAS   RE,HEADUP                  HEAD UP NEW PAGE                      
         SPACE 1                                                                
         MVC   XP+1(6),=C'CLIENT'                                               
         MVC   XP+8(3),CLIENT             NEW CLIENT TITLE                      
         MVC   XP+12(36),CLTNAME                                                
         GOTO1 UNDERLIN,DMCB,(44,XP+1),(X'BF',XPSECOND+1)                       
         GOTO1 ACREPORT                                                         
         SPACE 2                                                                
PRNT16   MVC   PRODUCT,SRTPROD            NEW PRODUCT CODE                      
*                                         LOOK IN TABLE FIRST                   
         L     R5,APRDLST                 ADDR OF PRDLIST                       
         USING BIND,R5                                                          
         MVC   DMCB+8(16),BININ           MOVE IN PARMS 3,4,5,6                 
         L     R3,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,SRTCLT,(R3)                                         
         CLI   DMCB,0                                                           
         BNE   PRNT16A                    NOT FOUND IN TABLE                    
         L     R5,DMCB                                                          
         USING PRDCDE,R5                                                        
         MVC   PRDNAME,PRDNAM             SJ PRODUCT NAME FROM TAB              
         MVC   PRODUCT,PRDKEY+3           SJ PRODUCT CODE FROM TAB              
         B     PRNT16C                                                          
         SPACE 1                                                                
PRNT16A  MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES                               
         MVC   ACKEYACC(9),SAVSJCOD       KEY FOR READ                          
         BAS   RE,READ                    READ FOR THAT RECORD                  
         CLI   WORK,C'*'                  ACCOUNT MISSING MESSAGE               
         BE    *+8                                                              
         BAS   RE,NAMEOUT                                                       
         MVC   PRDNAME,WORK               PRODUCT NAME                          
         SPACE 1                                                                
         MVC   WORK,SPACES                CLEAR TAB ENTRY WORK AREA             
         USING PRDCDE,R5                                                        
         LA    R5,WORK                                                          
         MVC   PRDNAM,PRDNAME             UPDATE TABLE                          
         MVC   PRDKEY,SRTCLT                                                    
         GOTO1 BINADD,DMCB,(R5),APRDLST                                         
         SPACE 2                                                                
PRNT16C  CLI   QLEDGER,C'J'              SKIP HEADUP IF SK REPORT ORDER         
         BNE   *+12                                                             
         BAS   RE,HEADUP                  HEAD UP NEW PAGE                      
         B     PRNT18                                                           
         SPACE 1                                                                
         ZIC   RF,LINE                    PRESENT LINE                          
         AH    RF,=H'5'                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   *+8                                                              
         BAS   RE,HEADUP                  HEAD UP NEW PAGE                      
         SPACE 1                                                                
         MVC   XP+1(7),=C'PRODUCT'                                              
         MVC   XP+9(3),PRODUCT            NEW PRODUCT TITLE                     
         MVC   XP+13(36),PRDNAME                                                
         GOTO1 UNDERLIN,DMCB,(44,XP+1),(X'BF',XPSECOND+1)                       
         GOTO1 ACREPORT                                                         
         B     PRNT18                                                           
         SPACE 2                                                                
PRNT17   GOTO1 ACREPORT                   SKIP A LINE FOR 1ST NEW JOB           
PRNT18   MVC   JOB,SRTJOB                 NEW JOB CODE                          
         L     R5,AJOBLST                 ADDR OF CODELIST                      
         USING BIND,R5                                                          
         MVC   DMCB+8(16),BININ           MOVE IN PARMS 3,4,5,6                 
         L     R3,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,SAVSJCOD,(R3)                                       
         CLI   DMCB,0                     DID WE FIND THIS JOB                  
         BNE   PRNT18A                    NOT FOUND IN TABLE                    
         SPACE 1                                                                
         L     R5,DMCB                                                          
         USING JOBCDE,R5                                                        
         MVC   JOBNAME,JOBNAM             NEW JOB NAME                          
         B     PRNT20                                                           
         SPACE 1                                                                
PRNT18A  MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES                               
         MVC   ACKEYACC(15),SAVSJCOD      KEY FOR READ                          
         BAS   RE,READ                    READ FOR THAT RECORD                  
         CLI   WORK,C'*'                  ACCOUNT MISSING MESSAGE               
         BE    *+8                                                              
         BAS   RE,NAMEOUT                                                       
         MVC   JOBNAME,WORK               JOBNAME                               
         SPACE 1                                                                
         MVC   WORK,SPACES                CLEAR TAB ENTRY WORK AREA             
         USING JOBCDE,R5                                                        
         LA    R5,WORK                                                          
         MVC   JOBNAM,JOBNAME             UPDATE TABLE                          
         MVC   JOBKEY,SRTCLT                                                    
         GOTO1 BINADD,DMCB,(R5),AJOBLST                                         
         EJECT                                                                  
*                                  * PRINT THE DETAIL LINE *                    
*                                    *  UPDATE TOTALS *                         
         USING PLINED,R7                                                        
PRNT20   LA    R7,PLINWRK1                                                      
         MVC   PLINWRK1(PLINLNQ),XSPACES                                        
         SPACE 1                                                                
         MVC   PLINACT,SRTJOB             SJ JOB NUMBER                         
         MVC   PLINAME,JOBNAME                   NAME                           
         CLI   QLEDGER,C'J'                                                     
         BNE   *+10                                                             
         MVC   PLINCNTA,SRTCNTRA          SK ACCOUNT NUMBER                     
         GOTO1 DATCON,DMCB,(1,SRTDATE),(8,PLINDATE)                             
         MVC   PLINREF,SRTREF                                                   
         SPACE 1                                                                
         EDIT  (P6,SRTAMNT),(12,PLINAMNT),2,MINUS=YES                           
         SPACE 2                                                                
*                                        TRANS AMT - BILLED = UNBILLED          
         SPACE 1                                                                
         ZAP   UNBILLED,SRTAMNT                                                 
         SP    UNBILLED,SRTPART                                                 
         CP    UNBILLED,SRTSKAMT         UNBILLED THE SAME AS SK AMNT           
         BE    PRNT22                    YES - THERE IS NO DIFFERENCE           
*        SP    UNBILLED,SRTTODAY         NO - THEY TAKING OUT TODAYS            
*        AP    SRTPART,SRTTODAY          ADD TODAYS BILLED TO BILLED            
PRNT22   DS    0H                                                               
         EDIT  (P6,UNBILLED),(12,PLINUNBL),2,MINUS=YES                          
         EDIT  (P6,SRTSKAMT),(12,PLINSK),2,MINUS=YES                            
*                                        UNBILLED - SK AMT = DIFFER             
         SP    UNBILLED,SRTSKAMT                                                
         CP    UNBILLED,=P'0'            NO DIFFERENCE                          
         BE    PRNT23                                                           
         CLI   QLEDGER,C'J'              LEDGER SJ                              
         BNE   PRNT23                                                           
         CLI   QOPT2,C'P'                MAKE CORRECTING POSTINGS               
         BNE   PRNT23                                                           
         ZAP   DIFFER,UNBILLED                                                  
         MP    DIFFER,=P'-1'             REVERSE SIGN                           
         BAS   RE,FIXIT                                                         
         SPACE 1                                                                
PRNT23   DS    0H                                                               
         EDIT  (P6,UNBILLED),(12,PLINTOT),2,MINUS=YES                           
         SPACE 1                                                                
         ZIC   RF,LINE                    PRESENT LINE                          
         AH    RF,=H'5'                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   *+8                                                              
         BAS   RE,HEADUP                  HEAD UP NEW PAGE                      
         SPACE 1                                                                
*                                         MOVE MY WORK LINE                     
         LA    R1,XP                           TO XP                            
         MVC   0(PLINLNQ,R1),PLINWRK1                                           
         GOTO1 ACREPORT                   AND PRINT IT                          
         SPACE 1                                                                
         AP    JOBCOUNT,=P'1'             ADD 1 TO PRINT LINE COUNTERS          
         AP    PRDCOUNT,=P'1'                                                   
         AP    CLTCOUNT,=P'1'                                                   
         CLI   QLEDGER,C'J'                                                     
         BE    PRNT29                                                           
         AP    INCCOUNT,=P'1'                                                   
         AP    TYPCOUNT,=P'1'                                                   
         SPACE 2                                                                
*                                         * UPDATE TOTALS*                      
*                                                                               
PRNT29   LA    R0,LEVCOUNT                NUMBER OF REPORT LEVEL TOTALS         
         LA    R1,ACCUMS                                                        
PRNT30   AP    0(7,R1),SRTAMNT            ADD TRANS AMOUNT TO TOTAL             
         AP    7(7,R1),SRTPART                AMT BILLED                        
         AP    14(7,R1),SRTSKAMT              SK AMNT                           
         LA    R1,TOTLEN(R1)              NEXT LEVEL OF ACCUMS                  
         BCT   R0,PRNT30                                                        
         SPACE 1                                                                
PRNT35   MVC   PLINWRK1(PLINLNQ),XSPACES  CLEAR MY WORK LINES                   
         B     XIT                                                              
         EJECT                                                                  
ACCTOT   NTR1                        ** REPORT TOTALS **                        
         USING PLINED,R7                                                        
         USING TOTALD,R5                                                        
         LA    R7,PLINWRK1                                                      
         MVC   PLINWRK1(PLINLNQ),XSPACES                                        
         MVC   PLINACT(10),=C'TOTALS FOR'                                       
         CLI   TOTSW,C'1'                 IS IT A SJ JOB TOTAL                  
         BE    ACTOT02                                                          
         CLI   TOTSW,C'2'                            PROD TOTAL                 
         BE    ACTOT05                                                          
         CLI   TOTSW,C'3'                            CLIENT TOTAL               
         BE    ACTOT08                                                          
         CLI   TOTSW,C'4'                 IS IT A SK INCOME TOTAL               
         BE    ACTOT12                                                          
         CLI   TOTSW,C'5'                            TYPE TOTAL                 
         BE    ACTOT16                                                          
         SPACE 1                                                                
         CLI   TOTSW,C'6'                 IS IT A REPORT TOTAL                  
         BE    ACTOT18                                                          
         DC    H'0'                       TOTSW AT UNKNOWN VALUE                
         SPACE 2                                                                
*                                        ***JOB TOTAL***                        
         SPACE 1                                                                
ACTOT02  LA    R5,JBTOT                  ADDR OF JOB ACCUMS                     
         LA    R6,BUKCONT1               NUMBER ACCUM BUCKETS TO CLEAR          
         ZAP   COUNTIT,JOBCOUNT          NUMBER OF PRINTED JOBS                 
         SPACE 1                                                                
         MVC   PLINACT+11(6),JOB         JOB CODE                               
         MVC   PLINACT+18(26),JOBNAME    NAME                                   
         B     ACTOT20                   PUT OUT TOTALS                         
         SPACE 1                                                                
*                                        ***PRODUCT TOTAL***                    
         SPACE 1                                                                
ACTOT05  LA    R5,PDTOT                  ADDR OF PROD ACCUMS                    
         LA    R6,BUKCONT2               NUMBER ACCUM BUCKETS TO CLEAR          
         ZAP   COUNTIT,PRDCOUNT          NUMBER  PRINTED FOR PROD               
         MVC   PLINACT+11(3),PRODUCT     PRODUCT CODE                           
         MVC   PLINACT+15(29),PRDNAME    NAME                                   
         B     ACTOT20                   PUT OUT TOTALS                         
         SPACE 1                                                                
*                                        ***CLIENT TOTAL***                     
         SPACE 1                                                                
ACTOT08  LA    R5,CLTOT                  ADDR OF CLIENT ACCUMS                  
         LA    R6,BUKCONT3               NUMBER ACCUM BUCKETS TO CLEAR          
         ZAP   COUNTIT,CLTCOUNT          NUMBER  PRINTED FOR CLIENT             
         MVC   PLINACT+11(3),CLIENT      CLIENT CODE                            
         MVC   PLINACT+15(29),CLTNAME    NAME                                   
         B     ACTOT20                   PUT OUT TOTALS                         
         SPACE 1                                                                
*                                        ***INC ACCT TOTAL***                   
         SPACE 1                                                                
ACTOT12  LA    R5,INTOT                  ADDR OF INC TYPE ACCUMS                
         LA    R6,BUKCONT4               NUMBER ACCUM BUCKETS TO CLEAR          
         ZAP   COUNTIT,INCCOUNT          NUMBER  PRINTED FOR INC ACCT           
         MVC   PLINACT+11(3),SKINCOM     SK INC ACCT CODE                       
         MVC   PLINACT+14(29),INCNAME    NAME                                   
         B     ACTOT20                   PUT OUT TOTALS                         
         SPACE 1                                                                
*                                        ***INC TYPE TOTAL***                   
         SPACE 1                                                                
ACTOT16  LA    R5,TYTOT                  ADDR OF INC TYPE ACCUMS                
         LA    R6,BUKCONT5               NUMBER ACCUM BUCKETS TO CLEAR          
         ZAP   COUNTIT,TYPCOUNT          NUMBER  PRINTED FOR INC TYPE           
         MVC   PLINACT+11(1),SKTYPE      SK INC TYPE CODE                       
         MVC   PLINACT+14(29),TYPNAME    NAME                                   
         B     ACTOT20                   PUT OUT TOTALS                         
         SPACE 1                                                                
*                                        ***REPORT TOTAL***                     
         SPACE 1                                                                
ACTOT18  LA    R5,RPTOT                  ADDR OF REPORT TOTAL ACCUMS            
         LA    R6,BUKCOUNT               NUMBER ACCUM BUCKETS TO CLEAR          
         MVC   PLINACT+11(6),=C'REPORT'  REPORT TOTAL                           
         SPACE 1                                                                
ACTOT20  DS    0H                                                               
         CLI   TOTSW,C'6'                IS IT A REPORT TOTAL                   
         BE    ACTOT20H                  YES - CONTINUE                         
         CP    COUNTIT,=P'1'             MORE THAN 1 LINE AT LEVEL              
         BH    ACTOT20H                  YES - PRINT TOTAL                      
         SPACE 1                                                                
ACTOT20G ZIC   RF,LINE                   PRESENT LINE                           
         AH    RF,=H'3'                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   *+8                                                              
         BAS   RE,HEADUP                 HEAD UP NEW PAGE                       
         B     ACTOT35                   CLEAR ACCUMS                           
         SPACE 1                                                                
ACTOT20H DS    0H                                                               
         EDIT  (P7,TOTAMT),(12,PLINAMNT),2,MINUS=YES                            
         ZAP   UNBILLED,TOTAMT              TRANSACTION AMOUNT                  
         SP    UNBILLED,TOTPART            MINUS WHATS BEEN BILLED              
         EDIT  (P6,UNBILLED),(12,PLINUNBL),2,MINUS=YES                          
         EDIT  (P7,TOTSKAMT),(12,PLINSK),2,MINUS=YES                            
         SP    UNBILLED,TOTSKAMT                                                
         EDIT  (P6,UNBILLED),(12,PLINTOT),2,MINUS=YES                           
         SPACE 1                                                                
*                                      * PRINT THE TOTAL LINE *                 
ACTOT28  DS    0H                                                               
         GOTO1 ACREPORT                SKIP A LINE                              
         ZIC   RF,LINE                 PRESENT LINE                             
         AH    RF,=H'5'                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   ACTOT30                                                          
         BAS   RE,HEADUP               HEAD UP NEW PAGE                         
         SPACE 1                                                                
ACTOT30  MVC   XP(PLINLNQ),PLINWRK1    MOVE MY WORK LINE                        
*        MVI   SPACING,2                                                        
         GOTO1 ACREPORT                AND PRINT IT                             
         SPACE 1                                                                
*                                      * CLEAR LEVEL ACCUMS *                   
ACTOT35  ZAP   0(7,R5),=P'0'                                                    
         LA    R5,7(R5)                                                         
         BCT   R6,ACTOT35                                                       
         SPACE 1                                                                
         MVC   PLINWRK1(PLINLNQ),XSPACES CLEAR MY WORK LINE                     
         SPACE 1                                                                
         MVI   TOTSW,C'1'              RESET TO JOB TOTAL                       
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*                                         ** HEADLINES **                       
HEADUP   NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         CLI   QLEDGER,C'J'                   REQUESTED OFF SJ                  
         BNE   HEADUP02                       NO - FILL IN SK HEADINGS          
         MVC   XHEAD3+26(36),COMPNAM          COMPANY   NAME                    
         MVC   XHEAD4+17(3),CLIENT            SJ CLIENT CODE                    
         MVC   XHEAD4+26(36),CLTNAME                    NAME                    
         CLI   QOPT2,C'P'                     POST THE DIFFERENCES              
         BNE   *+10                                                             
         MVC   XHEAD5+125(12),=C'*** LIVE ***'                                  
         MVC   XHEAD5+17(3),PRODUCT           SJ PRODUCT CODE                   
         MVC   XHEAD5+26(36),PRDNAME                     NAME                   
         B     HEADUP03                                                         
HEADUP02 MVC   XHEAD3+29(36),COMPNAM          COMPANY   NAME                    
         MVC   XHEAD4+17(1),SKTYPE            SK INCOME TYPE                    
         MVC   XHEAD4+29(36),TYPNAME                    NAME                    
         MVC   XHEAD5+17(11),SKINCOM           SK INCOME ACCT                   
         MVC   XHEAD5+29(36),INCNAME                     NAME                   
HEADUP03 DS    0H                                                               
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         SPACE 3                                                                
NAMEOUT  NTR1                             ** NAME LOOKUP **                     
         L     R4,ACREC                                                         
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'               NO NAME ELEMENT -SOMETHINGS WRONG             
         USING ACNAMED,R4                                                       
         MVC   WORK(36),SPACES                                                  
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   WORK(0),ACNMNAME                                                 
         SPACE 3                                                                
PUTSORT  NTR1                             ** PUT RECORD TO SORT **              
         USING SRTD,R6                                                          
         LA    R6,SRTWRK                                                        
         LR    R1,R6                         ADDR OF SORT REC INTO R1           
         LA    R1,SBUKLOC(R1)                START OF COL BUCKETS               
         LA    R0,SBUKCONT                   NUMBER OF BUCKETS INTO R0          
         SPACE 1                                                                
PUT00    CP    0(6,R1),=P'0'                 CONTAIN PACKED ZEROS               
         BNZ   PUT99                         NOT ZERO-WE WANT IT                
         LA    R1,6(R1)                      BUMP TO NEXT BUCKET                
         BCT   R0,PUT00                                                         
         B     XIT                                                              
         SPACE 1                                                                
PUT99    GOTO1 SORTER,DMCB,=C'PUT',(R6)                                         
         MVI   ALSORT,1                      ACTIVITY SWITCH                    
         B     XIT                                                              
         EJECT                                                                  
*              ADD ITEM TO BINSRCH TABLE                                        
*              P1                  A(ITEM TO BE ADDED)                          
*              P2                  A(TABLE)                                     
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   NTR1                                                                   
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ              NUMBER LENGTH,KEY,MAX              
         L     R6,BINTABLE                   A(TABLE)                           
         L     R4,0(R1)                      A(ITEM)                            
         GOTO1 BINSRCH,DMCB,(X'01',(R4)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                          TABLE IS FULL                      
         MVC   BININ,DMCB+8                  UPDATE COUNT                       
         B     XIT                                                              
         SPACE 4                                                                
CLERSORT NTR1                    **  CLEAR SORT RECORD AREA **                  
         USING SRTD,R6                                                          
         LA    R6,SRTWRK                                                        
         XC    SRTWRK(SRTLNQ),SRTWRK                                            
         ZAP   SRTAMNT,=P'0'                                                    
         ZAP   SRTPART,=P'0'                                                    
*        ZAP   SRTTODAY,=P'0'                                                   
         ZAP   SRTSKAMT,=P'0'                                                   
         B     XIT                                                              
         EJECT                                                                  
*              DATAMGR INTERFACE                                                
         SPACE 1                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'            READ HIGH                          
         MVC   SAVEKEY,0(R7)                                                    
         B     GTREC                                                            
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'            READ SEQUENTIAL                    
         MVC   SAVEKEY,0(R7)                                                    
         B     GTREC                                                            
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'            A SPECIFIC READ                    
         MVC   WORK(36),SPACES                                                  
*                                                                               
GTREC    NTR1                                                                   
         L     R7,ACREC                                                         
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',(R7),(R7)                       
         CLI   DMCB+8,0                      TEST FOR ERRORS                    
         BE    GTREC1                                                           
         CLI   REPORTSW,C'Y'                 ARE WE UP TO REPORTS               
         BE    *+6                                                              
         DC    H'0'                          DIE IF ERRORS FOUND                
         MVC   WORK(19),=C'**ACCOUNT MISSING**'                                 
GTREC1   B     XIT                                                              
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              MAKE CORRECTING POSTINGS TO WORKER FILE                          
FIXIT    NTR1                                                                   
         USING PSHEADD,R3                                                       
         USING SRTD,R6                                                          
         USING TRANSD,R2                                                        
         MVI   REPORTSW,C'N'               SHUT OFF REP SWITCH ALLOWS           
*                                          DATMGR TO DIE IF ACCT NOT            
*                                          FOUND                                
         LA    R6,LSTWRK                   SORT REC                             
         LA    R3,AREA+4                                                        
         LA    R2,AREA+74                                                       
         XC    AREA(200),AREA              CLEAR MY POSTING AREA                
         XC    AREA+200(100),AREA+200                                           
         MVC   PSHDEL(2),=X'5046'                                               
         MVC   PSHDACC(1),RCCOMPFL         COMPANY                              
         MVC   PSHDACC+1(14),SRTCNTRA      SK ACCOUNT                           
         MVC   PSHDANAL,SPACES                                                  
         MVC   PSHDSBAC(1),RCCOMPFL        SJ JOB CONTRA                        
         MVC   PSHDSBAC+1(2),=C'SJ'                                             
         MVC   PSHDSBAC+3(12),SRTCODE                                           
         MVC   PSHDSBNM,JOBNAME                                                 
*                                   **DEBIT SK CONTRA JOB**                     
         MVC   TRNSEL(2),=X'441D'                                               
         MVC   TRNSDATE,SRTDATE                                                 
         MVC   TRNSREF,SRTREF                                                   
         MVI   TRNSSBRF,0                                                       
         MVI   TRNSTYPE,6                                                       
         MVC   TRNSBTCH,SPACES                                                  
         MVC   TRNSBTCH(2),MOS                                                  
         ZAP   TRNSAMNT,DIFFER             THE DIFFERENCE                       
         MVC   TRNSANAL(1),SRTANAL                                              
         MVI   TRNSANAL+1,C' '                                                  
         MVI   TRNSNARR,C' '                                                    
         MVI   TRNSSTAT,X'80'              DEBIT                                
         BAS   RE,PUTAPE                                                        
         SPACE 1                                                                
         MVI   PSHDACC+2,C'I'        **CREDIT SI, CONTRA PRODUCT**              
         MVC   PSHDSBAC+3(12),SPACES                                            
         MVC   PSHDSBAC+3(6),SRTCODE       CLI/PRODUCT                          
         MVC   PSHDSBNM,PRDNAME            PRODUCT NAME                         
         MVI   TRNSSTAT,0                  SET TO A CREDIT                      
         MVC   TRNSDATE,DATPK              TODAY'S DATE                         
         AP    SIREF,=P'1'                 ADD ONE TO SEQ REF COUNT             
         UNPK  TRNSREF,SIREF               REFERENCE IS SEQUENTIAL              
         OI    TRNSREF+5,X'F0'                                                  
         LA    R1,X'1D'(R2)                ADD MEMO OF '0' TO SI REVER          
         MVC   0(3,R1),=X'5009C7'                                               
         ZAP   3(6,R1),=P'0'                                                    
         MVI   9(R1),0                                                          
         BAS   RE,PUTAPE                                                        
         EJECT                                                                  
         LA    R1,X'1D'(R2)                KILL MEMO ITEM                       
         MVI   0(R1),0                                                          
         CLI   POSTCOST,C'Y'               COMPANY ON COST                      
         BNE   FIXIT99                                                          
         SPACE 1                                                                
         OC    SRT12,SRT12                 DO WE HAVE A 12 ACCOUNT              
         BNZ   FIXIT02                                                          
         L     R7,ACREC                                                         
         USING ACKEYD,R7                                                        
         SPACE 1                                                                
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES       CLEAR KEY               
         MVC   ACKEYACC,PSHDACC           MOVE IN SI ACCOUNT                    
         BAS   RE,READ                    READ FOR THAT RECORD                  
         L     R4,ACREC                                                         
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACSTATD,R4                                                       
         MVC   SRT12,ACSTCOST             12 COSTING GROUP                      
         SPACE 1                                                                
FIXIT02  CLI   SRT12,C' '            ** CREDIT 12 INCOME CONTRA 1C **           
         BNE   *+8                                                              
         MVI   SRT12,C'3'                 DEFAULT                               
         MVC   PSHDACC+1(14),SPACES                                             
         MVC   PSHDACC+1(2),=C'12'                                              
         MVC   PSHDACC+3(1),SRT12                                               
         OC    SRTCOST,SRTCOST             DO WE HAVE THE 1C ACCOUNT            
         BNZ   FIXIT02G                                                         
         L     R7,ACREC                                                         
         USING ACKEYD,R7                                                        
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES       CLEAR KEY               
         MVC   ACKEYACC(1),RCCOMPFL        SJ JOB                               
         MVC   ACKEYACC+1(2),=C'SJ'                                             
         MVC   ACKEYACC+3(12),SRTCODE                                           
FIXIT02A BAS   RE,READ                     READ FOR THAT RECORD                 
         L     R4,ACREC                                                         
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL                                                         
         BNE   FIXIT02B                                                         
         USING ACPROFD,R4                                                       
         OC    ACPRCOST,ACPRCOST                                                
         BNZ   FIXIT02F                    FOUND THE COSTING ACCOUNT            
FIXIT02B CLI   ACKEYACC+9,C' '             IS THERE A JOB                       
         BE    FIXIT02C                    NO - CHECK FOR PROD                  
         MVC   ACKEYACC+9(6),SPACES        CLEAR JOB                            
         B     FIXIT02A                    READ FOR PRODUCT LEVEL               
FIXIT02C CLC   ACKEYACC+6(9),SPACES        IS THE A PRODUCT                     
         BE    FIXIT02D                    NO - CHECK FOR CLIENT                
         MVC   ACKEYACC+6(9),SPACES        CLEAR PRODUCT                        
         B     FIXIT02A                    READ FOR CLIENT LEVEL                
FIXIT02D DC    H'0'                   NO X'24' FOUND                            
FIXIT02F MVC   SRTCOST,ACPRCOST            MOVE IN TO SORTREC                   
FIXIT02G MVC   PSHDSBAC,SRTCOST            CONTRA A/C CLIENT                    
         MVC   PSHDSBNM,CLTNAME            CLIENT NAME                          
         BAS   RE,PUTAPE                                                        
         SPACE 1                                                                
         MVC   PSHDSBAC,PSHDACC      ** DEBIT 1C COSTING CONTRA 12 **           
         MVC   PSHDACC,SRTCOST                                                  
         MVC   PSHDSBNM,SPACES                                                  
         LA    R5,GROUPTAB                 12 GROUP NAME TABLE                  
         LA    R0,MAX12                    MAX ENTRIES                          
FIXIT03  CLI   0(R5),X'FF'                 END OF TAB?                          
         BE    FIXIT03B                    YES - NOT FOUND ADD TO TAB           
         CLC   0(1,R5),SRT12                                                    
         BNE   FIXIT03A                    NO- CONTINUE THE SEARCH              
         MVC   PSHDSBNM,1(R5)              NAME                                 
         B     FIXIT04                                                          
FIXIT03A LA    R5,ENTLNQ(R5)                                                    
         BCT   R0,FIXIT03                                                       
         DC    H'0'                        TABLE IS FULL                        
         SPACE 1                                                                
FIXIT03B L     R7,ACREC                                                         
         USING ACKEYD,R7                                                        
         SPACE 1                                                                
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES       CLEAR KEY               
         MVC   ACKEYACC(6),PSHDSBAC       MOVE IN 12 ACCOUNT                    
         BAS   RE,READ                    READ FOR THAT RECORD                  
         BAS   RE,NAMEOUT                                                       
         MVC   0(1,R5),SRT12              ADD 12 ACCOUNT TO TAB                 
         MVC   1(36,R5),WORK              AND NAME                              
         LA    R1,ENTLNQ(R5)                                                    
         MVI   0(R1),X'FF'                MARK NEW END OF THE TABLE             
         MVC   PSHDSBNM,WORK               NAME                                 
FIXIT04  MVI   TRNSSTAT,X'80'              SET TO BE DEBIT                      
         BAS   RE,PUTAPE                                                        
FIXIT99  MVI   REPORTSW,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
PUTAPE   NTR1                                                                   
         TM    TRNSSTAT,X'80'            IS IT A DEBIT                          
         BZ    *+10                                                             
         AP    TAPECASH,TRNSAMNT         ADD TO TOTAL DEBITS                    
         AP    TAPECNT,=P'1'             UPDATE RECORD COUNT                    
         SR    R3,R3                                                            
         IC    R3,1(R2)                  LENGTH OF EL INTO R3                   
PUTTAPEB AR    R2,R3                     POINT R2 TO BEG OF NEXT EL             
         CLI   0(R2),0                   THE END                                
         BE    PUTTAPEC                  YES - SET LENGTH FOR HEADER            
         IC    R3,1(R2)                  NO - LENGTH INTO R3                    
         LTR   R3,R3                     CHECK ZERO LENGTH ELEMENT              
         BNZ   PUTTAPEB                                                         
         MVI   0(R2),0                                                          
PUTTAPEC DS    0H                                                               
         LA    R2,1(R2)                  SET LENGTH FOR HEADER                  
         LA    R3,AREA                                                          
         SR    R2,R3                                                            
         STH   R2,AREA                                                          
         SPACE 1                                                                
PUTAPE2  BAS   RE,ADDPOST                                                       
         B     XIT                                                              
         SPACE 4                                                                
*              WRITE LAST POSTING FILE RECORD AND CLOSE                         
         SPACE 1                                                                
CLOSEWK  NTR1                                                                   
         XC    AREA(200),AREA                CLEAR MY POSTING AREA              
         XC    AREA+200(100),AREA+200                                           
         MVC   AREA(06),=X'00210000521D'                                        
         MVC   AREA+6(14),=C'8A SI TRANSFER'                                    
         MVC   AREA+21(6),TAPECNT                                               
         MVC   AREA+27(6),TAPECASH                                              
         BAS   RE,ADDPOST                                                       
         BAS   RE,CLOSPOST                                                      
         B     XIT                                                              
         EJECT                                                                  
*              WORKER INTERFACE                                                 
         SPACE 1                                                                
ADDPOST  MVC   COMMAND,=CL6'ADD'                                                
         B     FILE                                                             
         SPACE 1                                                                
CLOSPOST MVC   COMMAND,=CL6'CLOSE'                                              
         B     FILE                                                             
         SPACE 1                                                                
FILE     NTR1                                                                   
         LA    R3,AREA                                                          
         L     R4,POSTBUF                                                       
         CLI   RCPOSTNG,C'N'                                                    
         BE    XIT                                                              
*                                                                               
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
         TM    DMCB+8,X'C0'                                                     
         BZ    XIT                                                              
*                                  ERROR ON ADD TO WORKER FILE                  
         MVC   P,SPACES                                                         
         MVC   P(42),=C'*WORKER FILE FULL - STOP ALL FILE MARKERS*'             
         TM    DMCB+8,X'80'                                                     
         BO    *+10                                                             
         MVC   P(42),=C'*WORKER FILE DISK ERROR - CANNOT ADD ID = '             
         L     R2,DMCB+8           ADDRESS OF KEY                               
         MVC   DOUBLE(2),0(R2)                                                  
         LH    R3,DOUBLE                                                        
         CVD   R3,DOUBLE                                                        
         UNPK  DOUBLE+2(6),DOUBLE                                               
         OI    DOUBLE+7,X'F0'                                                   
         MVC   P+42(3),DOUBLE+5                                                 
         MVC   P+45(23),=C', REPLY = ''OK'' FOR DUMP'                           
WKERR2   GOTO1 LOGIO,WORK,1,(68,P)                                              
         GOTO1 (RF),(R1),0,(2,DUB)                                              
         CLC   DUB(2),=C'OK'                                                    
         BNE   WKERR2                                                           
         DC    H'0'                NOW DIE                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              CONSTANTS                                                        
         SPACE 1                                                                
RELOTAB  DS    0A                                                               
         DC    A(RECORD)                                                        
         DC    A(POSTBUFC)                                                      
         DC    A(INCLIST)                                                       
         DC    A(CLILIST)                                                       
         DC    A(PRDLIST)                                                       
         DC    A(JOBLIST)                                                       
         DC    V(SORTER)                                                        
         DC    V(UNDERLIN)                                                      
         DC    X'FF'                                                            
         SPACE 1                                                                
SORTCRD1 DC    CL80'SORT FIELDS=(1,35,A),FORMAT=BI,WORK=1'                      
SORTCRD2 DC    CL80'SORT FIELDS=(13,14,A,1,12,A,27,9,A),FORMAT=BI,WORK=X        
               1'                                                               
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)'                             
*                                                                               
*                                                                               
INCNUM   EQU   2500                                                             
CLINUM   EQU   5000                                                             
PRDNUM   EQU   6000                                                             
JOBNUM   EQU   90000                                                            
*                                                                               
MAINTAB  DS    0D                                                               
         DC    AL4((((INCNUM*INCLEN)+7)/8)*8),A(INCTAB)                         
         DC    AL4((((CLINUM*CLILEN)+7)/8)*8),A(CLITAB)                         
         DC    AL4((((PRDNUM*PRDLEN)+7)/8)*8),A(PRDTAB)                         
         DC    AL4((((JOBNUM*JOBLEN)+7)/8)*8),A(JOBTAB)                         
         DC    X'FF'                                                            
         EJECT                                                                  
*              BOX ROUTINES (HOOK)                                              
         SPACE 2                                                                
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC           RESTORE REG C                                 
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXCOLS(198),XSPACES                                             
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+8,C'T'      SET ROWS                                     
         MVI   BOXROWS+12,C'M'                                                  
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS,C'L'        SET LH MARGIN                                
         CLI   QLEDGER,C'J'                                                     
         BNE   *+8                                                              
         MVI   BOXCOLS+45,C'C'                                                  
         MVI   BOXCOLS+62,C'C'                                                  
         MVI   BOXCOLS+72,C'C'                                                  
         MVI   BOXCOLS+80,C'C'                                                  
         MVI   BOXCOLS+94,C'C'                                                  
         MVI   BOXCOLS+108,C'C'                                                 
         MVI   BOXCOLS+122,C'C'                                                 
         MVI   BOXCOLS+136,C'R'                                                 
         SPACE 1                                                                
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
         SPACE 2                                                                
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
RECORD   DS    0D                                                               
         DS    CL42                                                             
         DS    CL1000                                                           
         SPACE 1                                                                
POSTBUFC DS    0D                                                               
         DC    4500X'00'                                                        
         SPACE 1                                                                
INCLIST  DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(INCLEN)         RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(L'INCKEY)       KEY LENGTH                                   
         DC    AL4(INCNUM)         MAX IN TABLE                                 
INCTAB   DS    AL4(0)              A(INCTAB)                                    
         SPACE 1                                                                
CLILIST  DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(CLILEN)         RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(L'CLIKEY)       KEY LENGTH                                   
         DC    AL4(CLINUM)         MAX IN TABLE                                 
CLITAB   DS    AL4(0)              A(CLITAB)                                    
         SPACE 1                                                                
PRDLIST  DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(PRDLEN)         RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(L'PRDKEY)       KEY LENGTH                                   
         DC    AL4(PRDNUM)         MAX IN TABLE                                 
PRDTAB   DS    AL4(0)              A(PRDTAB)                                    
         SPACE 1                                                                
JOBLIST  DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(JOBLEN)         RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(L'JOBKEY)       KEY LENGTH                                   
         DC    AL4(JOBNUM)         MAX IN TABLE                                 
JOBTAB   DS    AL4(0)              A(JOBTAB)                                    
         EJECT                                                                  
*              DSECT FOR STORAGE AREA                                           
         SPACE 1                                                                
AC8AD    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
ACREC    DS    A                                                                
POSTBUF  DS    A                                                                
AINCLST  DS    A                                                                
ACLILST  DS    A                                                                
APRDLST  DS    A                                                                
AJOBLST  DS    A                                                                
SORTER   DS    V                                                                
UNDERLIN DS    V                                                                
         SPACE 1                                                                
ADBOX    DS    A                                                                
NUMNON   DS    CL1                                                              
COMMAND  DS    CL6                                                              
ID       DS    CL16                                                             
ELCODE   DS    CL1                                                              
POSTCOST DS    CL1                 IS COMPANY ON COSTING                        
TODAY    DS    CL2                 TODAY'S DATE COMPRESSED                      
MOS      DS    CL2                 MONTH OF SERVICE FROM TODAY'S DATE           
DATPK    DS    CL3                 TODAY'S DATE PACKED                          
COMPNAM  DS    CL36                COMPANY NAME FOR HEADLINES                   
COUNTS   DS    0C                                                               
JOBCOUNT DS    PL3                 COUNT DETAIL LINES BEFORE JOB TOT            
PRDCOUNT DS    PL3                 COUNT DETAIL LINES BEFORE PRD TOT            
CLTCOUNT DS    PL3                 COUNT DETAIL LINES BEFORE CLT TOT            
INCCOUNT DS    PL3                 COUNT DETAIL LINES BEFORE INC TOT            
TYPCOUNT DS    PL3                 COUNT DETAIL LINES BEFORE TYP TOT            
COUNTIT  DS    PL3                 COMMON COUNT AREA FOR ACCTOT                 
COUNTNUM EQU   (*-COUNTS)/3        NUMBER OF COUNTS KEPT                        
         SPACE 1                                                                
MAINLEN  DS    F                   LENGTH OF GETMAIN AREA                       
MAINBGN  DS    F                   A(START OF GET MAIN)                         
*                                                                               
SAVSJCOD DS    0CL15                                                            
COMP1    DS    XL1                 COMPANY                                      
         DS    CL2                 UNIT S LEDGER J                              
CLIENT   DS    CL3                 SJ CLIENT                                    
PRODUCT  DS    CL3                 SJ PRODUCT                                   
JOB      DS    CL6                 SJ JOB                                       
         SPACE 1                                                                
SAVSKCOD DS    0CL15                                                            
COMP2    DS    XL1                 COMPANY                                      
SKACCT   DS    0CL14                                                            
         DS    CL2                 UNIT S LEDGER K                              
SKTYPE   DS    CL1                 SK INCOME TYPE                               
SKINCOM  DS    CL11                SK INCOME ACCOUNT                            
         SPACE 1                                                                
SAVEKEY  DS    CL42                                                             
         SPACE 1                                                                
COSTCODE DS    CL15                1C COSTING ACCOUNT                           
ANALCODE DS    CL1                 SJ OFFICE CODE                               
         SPACE 1                                                                
SJNAMES  DS    0CL108                                                           
CLTNAME  DS    CL36                SJ CLIENT NAME                               
PRDNAME  DS    CL36                   PRODUCT NAME                              
JOBNAME  DS    CL36                   JOB NAME                                  
         SPACE 1                                                                
SKNAMES  DS    0CL72                                                            
TYPNAME  DS    CL36                SK INCOME TYPE NAME                          
INCNAME  DS    CL36                   INCOME NAME                               
         SPACE 1                                                                
ALSORT   DS    A                   A(LAST SORT RECORD)                          
SRTWRK   DS    (SRTLNQ)C           WORK AREA FOR SORT RECORD                    
LSTWRK   DS    (SRTLNQ)C           WORK AREA FOR LAST RECORD                    
PLINWRK1 DS    (PLINLNQ)C          WORK AREA FOR PRINT LINE                     
         SPACE 1                                                                
ENTLNQ   EQU   37                  1 BYTE 12 GROUP , 36 NAME                    
MAX12    EQU   37                  MAX NUMBER OF 12 ACCOUNTS                    
GROUPTAB DS    (MAX12*ENTLNQ)C     THE 12 GROUP TABLE                           
         SPACE 1                                                                
         DS    0F                                                               
AREA     DS    CL300                                                            
         SPACE 1                                                                
TAPECNT  DS    PL6                 NUMBER OF WORKER RECS                        
TAPECASH DS    PL6                 DEBIT VALUE OF POSTINGS                      
SIREF    DS    PL4                 SEQUENTIAL COUNT FOR SI TRNSREF              
TOTSW    DS    CL1                 LEVEL OF TOTAL SWITCH                        
REPORTSW DS    CL1                 TELL 'READ' WE'RE DOING REPORTS              
UNBILLED DS    PL6                 UNBILLED WORK AREA                           
DIFFER   DS    PL6                 DIFFERENCE WORK AREA                         
TOTAL77  DS    PL6                 TOTAL OF PARTIALLY BILLED                    
         SPACE 3                   **REPORT TOTAL ACCUMS**                      
ACCUMS   DS    0C                                                               
*                                  OVERALL REPORT TOTAL                         
RPTOT    DS    0C                                                               
RPAMT    DS    PL7                 SJ TRANS AMOUNTS                             
RPPART   DS    PL7                 UNBILLED AMOUNT                              
RPSKAMT  DS    PL7                 SK AMOUNTS                                   
TOTLEN   EQU   *-ACCUMS                                                         
COUNT    EQU   (*-ACCUMS)/7        NUMBER OF ACCUMS IN LEVEL                    
         SPACE 1                                                                
*                                  (SK) INCOME TYPE ACCUMS                      
TYTOT    DS    0C                                                               
TYAMT    DS    PL7                                                              
TYPART   DS    PL7                                                              
TYSKAMT  DS    PL7                                                              
         SPACE 1                                                                
*                                  (SK) INCOME ACCUMS                           
INTOT    DS    0C                                                               
INAMT    DS    PL7                                                              
INPART   DS    PL7                                                              
INSKAMT  DS    PL7                                                              
         SPACE 1                                                                
*                                  (SJ) CLIENT ACCUMS                           
CLTOT    DS    0C                                                               
CLAMT    DS    PL7                                                              
CLPART   DS    PL7                                                              
CLSKAMT  DS    PL7                                                              
         SPACE 1                                                                
*                                  (SJ) PRODUCT ACCUMS                          
PDTOT    DS    0C                                                               
PDAMT    DS    PL7                                                              
PDPART   DS    PL7                                                              
PDSKAMT  DS    PL7                                                              
         SPACE 1                                                                
*                                  (SJ) JOB ACCUMS                              
JBTOT    DS    0C                                                               
JBAMT    DS    PL7                                                              
JBPART   DS    PL7                                                              
JBSKAMT  DS    PL7                                                              
         SPACE 1                                                                
BUKCOUNT EQU   (*-ACCUMS)/7        NUMBER OF ACCUMS                             
BUKCONT1 EQU   (*-JBTOT)/7         CLEAR ACCUMS WHEN JOB CHANGES                
BUKCONT2 EQU   (*-PDTOT)/7                           PROD                       
BUKCONT3 EQU   (*-CLTOT)/7                           CLT                        
BUKCONT4 EQU   (*-INTOT)/7                           INCOME ACCT                
BUKCONT5 EQU   (*-TYTOT)/7                           INCOME TYPE                
LEVCOUNT EQU   (*-ACCUMS)/TOTLEN   NUMBER OF LEVEL TOTALS                       
         SPACE 2                                                                
         EJECT                                                                  
*              DSECT FOR SORT RECORD                                            
         SPACE 1                                                                
SRTD     DSECT                                                                  
SRTKEY   DS    0C                                                               
SRTCODE  DS    0CL12               SJ CLI/PROD/JOB                              
SRTCLT   DS    CL3                 CLIENT         (SJ)                          
SRTPROD  DS    CL3                 PRODUCT        (SJ)                          
SRTJOB   DS    CL6                 JOB            (SJ)                          
SRTCNTRA DS    0CL14               CONTRA                                       
SRTULSK  DS    CL2                 U/L SK         (SK)                          
SRTSKTYP DS    CL1                 TYPE OF INCOME (SK)                          
SRTSKACT DS    CL11                INCOME ACCT    (SK)                          
SRTKLNQ  EQU   *-SRTKEY            SORT KEY LENGTH                              
SRTDATE  DS    CL3                 TRANS DATE PACKED YMD                        
SRTREF   DS    CL6                 REFERENCE NUMBER                             
SRTDLNQ  EQU   *-SRTKEY            RECORD DISCRIPTION LENGTH                    
SRTCOST  DS    CL15                1C COSTING ACCOUNT FOR THIS JOB              
SRT12    DS    CL1                 12 INCOME ACCOUNT FOR SK (SI) ACCT           
SRTANAL  DS    CL1                 OFFICE  CODE                                 
SBUKLOC  EQU   *-SRTD                                                           
SRTAMNT  DS    PL6                 TRANS AMOUNT                                 
SRTPART  DS    PL6                 PARTIAL BILLED AMOUNTS (X'77')               
*RTTODAY DS    PL6                 BILLED TODAY                                 
SRTSKAMT DS    PL6                 SK AMNT                                      
SBUKCONT EQU   (*-SRTAMNT)/6       NUMBER OF BUCKETS                            
SRTLNQ   EQU   *-SRTKEY            RECORD LENGTH                                
         EJECT                                                                  
*              DSECT FOR PRINT LINE                                             
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL1                                                              
PLINACT  DS    CL6                 CLIENT /PROD/JOB                             
         DS    CL2                                                              
PLINAME  DS    CL36                NAME                                         
         DS    CL2                                                              
PLINCNTA DS    CL14                SK CONTRA ACCOUNT                            
         DS    CL2                                                              
PLINDATE DS    CL8                 TRANSACTION DATE                             
         DS    CL3                                                              
PLINREF  DS    CL6                 REFERENCE NUMBER                             
         DS    CL2                                                              
PLINAMNT DS    CL12                TRANSACTION                                  
         DS    CL2                                                              
PLINUNBL DS    CL12                TOTAL UNBILLED                               
         DS    CL2                                                              
PLINSK   DS    CL12                SK AMOUNT                                    
         DS    CL2                                                              
PLINTOT  DS    CL12                TOTAL UNBILLED - SK AMOUNT                   
PLINLNQ  EQU   *-PLINED            LENGTH OF LINE                               
*              DSECT FOR BINSRCH PARAMETERS                                     
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINTABLE DS    0CL1                                                             
         SPACE 2                                                                
*              DSECT FOR SK INCOME ACCOUNT LIST TABLE                           
INCCDE   DSECT                                                                  
INCKEY   DS    CL15                                                             
INCNAM   DS    CL36                                                             
INCCOST  DS    CL1                 12 COSTING GROUP                             
INCLEN   EQU   *-INCKEY                                                         
         SPACE 2                                                                
*              DSECT FOR SJ CLIENT  LIST TABLE                                  
CLICDE   DSECT                                                                  
CLIKEY   DS    CL6                                                              
CLINAM   DS    CL36                                                             
CLILEN   EQU   *-CLIKEY                                                         
         SPACE 2                                                                
*              DSECT FOR SJ PRODUCT LIST TABLE                                  
PRDCDE   DSECT                                                                  
PRDKEY   DS    CL9                                                              
PRDNAM   DS    CL36                                                             
PRDLEN   EQU   *-PRDKEY                                                         
         SPACE 2                                                                
*              DSECT FOR SJ JOB LIST TABLE                                      
JOBCDE   DSECT                                                                  
JOBKEY   DS    CL15                                                             
JOBNAM   DS    CL36                                                             
JOBLEN   EQU   *-JOBKEY                                                         
         SPACE 2                                                                
*              DSECT FOR TOTAL LINE ACCUMS                                      
TOTALD   DSECT                                                                  
TOTAMT   DS    PL7          TOTAL FOR TRANSACTION AMOUNT                        
TOTPART  DS    PL7                    PART THATS BILLED                         
TOTSKAMT DS    PL7                    SK AMOUNTS                                
         SPACE 1                                                                
*        DDLOGOD                                                                
*        ACGENBOTH                                                              
*        ACGENFILE                                                              
*        ACGENPOST                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        ACMASTD                                                                
*        ACBIGPRNTD                                                             
*        DDBIGBOX                                                               
*        DDCNTRL                                                                
*        DDREPXTRAD                                                             
*        DDREPMASTD                                                             
*        DDBOXEQUS                                                              
*        DDREMOTED                                                              
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026ACREP8A02 01/31/05'                                      
         END                                                                    
