*          DATA SET ACREPXA02  AT LEVEL 020 AS OF 01/21/03                      
*PHASE ACXA02A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE ADDTRN                                                                 
*INCLUDE CONVMOS                                                                
*INCLUDE GETCAP                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
         TITLE 'TIME FIX - ADDS POSTINGS USING RECOVERY TAPES'                  
ACXA02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXA**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXAD,RC                                                         
         MVI   FCRESET,C'Y'                                                     
         CLI   MODE,RUNFRST                                                     
         BNE   ACC10                                                            
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)                                     
         ZAP   CHACNT,=P'0'             RECORD CHANGED CNT TO O                 
         ZAP   NOCHACNT,=P'0'           NO CHANGE COUNT                         
         ZAP   LOCKCNT,=P'0'            LOCKED ACCOUNTS COUNT                   
         MVI   FCRDACC,C'Y'             READ ACCOUNTS ONLY                      
         MVI   FCRDTRNS,C'N'                                                    
         MVI   FCRDHIST,C'N'                                                    
         MVC   VPRNTBL,PRNTBL                                                   
*                                                                               
         XC    LASTCPY,LASTCPY                                                  
         ZAP   TOTDR,=P'0'                                                      
         ZAP   TOTCR,=P'0'                                                      
         ZAP   HOURS,=P'0'                                                      
         ZAP   RECADD,=P'0'                                                     
         ZAP   RECCHA,=P'0'                                                     
ACC10    DS    0H                                                               
*        BAS   RE,FIX1                                                          
*        BAS   RE,FIX2                                                          
*        BAS   RE,FIX3                                                          
         BAS   RE,FIX4                                                          
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         BAS   RE,PRNTOT                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FIX 1 WAS TO ADD MISSING TRANSACTIONS TO THE FILE                   *         
***********************************************************************         
         SPACE 1                                                                
FIX1     NTR1  ,                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   XIT                                                              
         BAS   RE,PRCRCV           PROCESS RECOVERY                             
FIX1A    CLI   EOFRCV,C'Y'                                                      
         BE    XIT                                                              
         BAS   RE,GETCPY           GET COMPANY STATS AND LEDGER LENGTH          
         BAS   RE,GETPER           GET PERSON ID                                
         BAS   RE,GETPRD           GET CALENDAR PERIOD NUMBER                   
         BAS   RE,GTPROF           GET PROFILES                                 
         BAS   RE,CALLTTRN         ADD TRANSACTIONS                             
         B     FIX1A                                                            
         EJECT                                                                  
***********************************************************************         
* FIX 2 WAS TO RESTORE TIME ELMENTS FROM RECOVERY                     *         
***********************************************************************         
         SPACE 1                                                                
FIX2     NTR1  ,                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   XIT                                                              
FIX2A    BAS   RE,PRCRCV           PROCESS RECOVERY                             
         CLI   EOFRCV,C'Y'                                                      
         BE    XIT                                                              
         BAS   RE,ADDTIM           ADD TIME ELEMENT                             
         B     FIX2A                                                            
         EJECT                                                                  
***********************************************************************         
* FIX 3 WAS TO CONVERT 1R TRANSACTIONS TO TIME ELEMENTS               *         
***********************************************************************         
         SPACE 1                                                                
FIX3     NTR1  ,                                                                
         CLI   MODE,PROCACC                                                     
         BNE   XIT                                                              
         LA    R2,DKEY                                                          
         USING TRNRECD,R2                                                       
         L     RF,ADACC            SET ACCOUNT CODE                             
         MVC   TRNKEY,0(RF)                                                     
         BAS   RE,HIGH                                                          
         BAS   RE,GETTRN           FIND AND FIX 1R RECORDS                      
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FIX 4 WAS TO DELETE TMS 8B ELEMENTS IF SJ POSTING WAS A REVERSAL    *         
***********************************************************************         
         SPACE 1                                                                
FIX4     NTR1  ,                                                                
         CLI   MODE,PROCACC                                                     
         BNE   XIT                                                              
         LA    R2,DKEY                                                          
         USING TRNRECD,R2                                                       
         L     RF,ADACC            SET ACCOUNT CODE                             
         MVC   TRNKEY,0(RF)                                                     
         BAS   RE,HIGH                                                          
         BAS   RE,GETTMS           FIND AND DELETE TMS ELEMENTS                 
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
********************************************************************            
*   PROCESS INPUT FILE     *                                                    
********************************************************************            
*                                                                               
PRCRCV   NTR1  ,                                                                
         NOP   PRCRCV3                                                          
         MVI   *-3,X'F0'                                                        
         OPEN  (RECVIN,(INPUT))                                                 
PRCRCV3  LA    RE,RDATA                                                         
         LA    RF,RDATALNQ                                                      
         LR    R0,RE                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(0)                                                       
*                                                                               
         CLI   RFILTY,X'6A'        ACCMST                                       
         BNE   PRCRCV3                                                          
         CLI   RPRG,X'1D'          COST ONLY                                    
         BNE   PRCRCV3                                                          
         TM    RRECTY,X'80'        OLD RECORD LOCATION DELETE                   
         BO    PRCRCV3             SKIP IT                                      
         BAS   RE,XFLTR            SPECIAL FILTER ROUTINE                       
         BNE   PRCRCV3                                                          
*                                                                               
OUTREC   DS    0H                                                               
         GOTO1 ACREPORT                                                         
         MVC   P(20),=CL20'RECOVERY RECORD'                                     
         GOTO1 ACREPORT                                                         
         MVC   P(5),=C'FILE='                                                   
         GOTO1 HEXOUT,DMCB,RFILTY,P+5,1                                         
         MVC   P+9(5),=C'TRNS='                                                 
         CLI   RRECTY,X'01'        COPY?                                        
         BNE   *+10                                                             
         MVC   P+14(4),=C'COPY'                                                 
         CLI   RRECTY,X'02'        CHANGE?                                      
         BNE   *+10                                                             
         MVC   P+14(4),=C'CHG '                                                 
         CLI   RRECTY,X'03'        ADD?                                         
         BNE   *+10                                                             
         MVC   P+14(4),=C'ADD '                                                 
         MVC   P+20(6),=C'TERM#='                                               
         LA    R2,P+26                                                          
         EDIT  RTRM,(5,(R2))                                                    
         GOTO1 HEXOUT,DMCB,RTIME,TEMPTIME,4                                     
         MVC   P+33(5),=C'TIME='                                                
         MVC   P+38(2),TEMPTIME+1                                               
         MVI   P+40,C':'                                                        
         MVC   P+41(2),TEMPTIME+3                                               
         MVI   P+43,C':'                                                        
         MVC   P+44(2),TEMPTIME+5                                               
         MVC   P+48(7),=C'DSKADD='                                              
         GOTO1 HEXOUT,DMCB,RVCHR,P+55,4                                         
         MVC   P+65(5),=C'DATE='                                                
         GOTO1 DATCON,DMCB,(3,RDATE),(11,P+70)                                  
         MVC   P+80(5),=C'USER='                                                
         LA    R2,P+85                                                          
         EDIT  RUSER,(4,(R2))                                                   
         GOTO1 ACREPORT                                                         
         LH    R0,RECVHDR-4                                                     
         GOTO1 PRNTBL,DMCB,0,RECVHDR,C'DUMP',(R0),FORMAT                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*                                                                               
ENDRCV   CLOSE (RECVIN,)                                                        
         MVI   EOFRCV,C'Y'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SPECIAL FILTER ROUTINE FOR TODAYS SPECIFIC PROBLEM                  *         
***********************************************************************         
         SPACE 1                                                                
XFLTR    NTR1  ,                                                                
         CLI   RRECTY,X'02'        CHANGE                                       
         BNE   XFLTN                                                            
         USING TRNRECD,R5                                                       
         LA    R5,RECVHDR+24                                                    
         CLI   TRNKCPY,X'BE'       HLTO                                         
         BNE   XFLTN                                                            
         CLC   TRNKUNT(14),=CL14'1RYAPF001109'                                  
         BNE   XFLTN                                                            
         CLC   TRNKULC(14),=CL14'1CTEBHWMGEN'                                   
         BNE   XFLTN                                                            
         CLC   TRNKDATE,=X'970613'                                              
         BNE   XFLTN                                                            
         TM    TRNRSTAT,X'80'                                                   
         BO    XFLTY                                                            
*                                                                               
XFLTN    LTR   RB,RB                                                            
         B     *+6                                                              
XFLTY    CR    RB,RB                                                            
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GET COMPANY STATS AND 1R AND SJ LEVEL LENGTHS                       *         
***********************************************************************         
         SPACE 1                                                                
GETCPY   NTR1                                                                   
         USING TRNRECD,R5                                                       
         LA    R5,RECVHDR+24                                                    
         CLC   LASTCPY,TRNKCPY     SAME COMPANY                                 
         BE    GCPYX                                                            
*                                                                               
         OC    LASTCPY,LASTCPY                                                  
         BZ    *+8                                                              
         BAS   RE,PRNTOT                                                        
         MVC   LASTCPY,TRNKCPY                                                  
         ZAP   TOTDR,=P'0'                                                      
         ZAP   TOTCR,=P'0'                                                      
*                                                                               
         USING TTRND,R7                                                         
         LA    R7,TTRNBLK          TIMETRN CONTROL BLOCK                        
         XC    TTRNBLK,TTRNBLK     TIMETRN CONTROL BLOCK                        
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,KEY2                                                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,TRNKCPY     COMPANY                                      
         MVC   SVKEY,KEY2                                                       
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,KEY2,KEY2                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY2(L'ACTKEY),SVKEY                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DA1,ACTKDA                                                       
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA1,IO1,WORK                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,IO1                                                           
         USING CPYELD,R3                                                        
         LA    R3,ACTRFST                                                       
         CLI   0(R3),CPYELQ        X'10'                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    SVCPYSTS(SVCPYLNQ),SVCPYSTS                                      
         MVC   TTCPYST1,CPYSTAT1   STATUS #1                                    
         MVC   TTCPYST2,CPYSTAT2   STATUS #2                                    
         MVC   TTCPYST3,CPYSTAT3   STATUS #3                                    
         MVC   TTCPYST4,CPYSTAT4   STATUS #4                                    
*                                                                               
         MVC   SVCPYST1,CPYSTAT1   SAVE ALL STATUS BYTES                        
         MVC   SVCPYST2,CPYSTAT2   FOR ADTRANS BLOCK                            
         MVC   SVCPYST3,CPYSTAT3                                                
         MVC   SVCPYST4,CPYSTAT4                                                
*                                                                               
         CLI   CPYLN,CPYLN2Q                                                    
         BL    GCPY02                                                           
         MVC   TTCPYST5,CPYSTAT5   STATUS #5                                    
         MVC   TTCPYST6,CPYSTAT6   STATUS #6                                    
         MVC   TTCPYST7,CPYSTAT7   STATUS #7                                    
         MVC   TTCPYST8,CPYSTAT8   STATUS #8                                    
                                                                                
         MVC   SVCPYST5,CPYSTAT5                                                
         MVC   SVCPYST6,CPYSTAT6                                                
         MVC   SVCPYST7,CPYSTAT7                                                
         MVC   SVCPYST8,CPYSTAT8                                                
                                                                                
GCPY02   CLI   CPYLN,CPYLN3Q                                                    
         BL    GCPY04                                                           
         MVC   TTCPYST9,CPYSTAT9   STATUS #9                                    
         MVC   TTCPYSTA,CPYSTATA   STATUS #10                                   
         MVC   SVCPYST9,CPYSTAT9                                                
         MVC   SVCPYSTA,CPYSTATA                                                
*                                                                               
GCPY04   LA    R6,KEY2                                                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,TRNKCPY     COMPANY                                      
         MVC   ACTKUNT(2),=C'1R'                                                
         MVC   SVKEY,KEY2                                                       
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,KEY2,KEY2                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY2(L'ACTKEY),SVKEY                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DA1,ACTKDA                                                       
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA1,IO1,WORK                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IO1                                                           
         USING ACLELD,R3                                                        
         LA    R3,ACTRFST                                                       
GETLDG3  CLI   0(R3),0                                                          
         BNE   *+6                 TEST EOR                                     
         DC    H'0'                                                             
         CLI   ACLEL,ACLELQ        TEST HIERARCHY ELEMENT                       
         BE    GETLDG10                                                         
         SR    R0,R0                                                            
         IC    R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     GETLDG3                                                          
*                                                                               
GETLDG10 DS    0H                                                               
         LA    R6,4                                                             
         AH    R3,=Y(ACLLN1Q)      BUMP TO FIRST LENGTH                         
         LA    R2,TT1RLNQS                                                      
GETLDG12 MVC   0(1,R2),0(R3)                                                    
         LA    R3,L'ACLVALS(R3)    NEXT LENGTH                                  
         LA    R2,1(R2)                                                         
         BCT   R6,GETLDG12                                                      
         MVC   BC1RLNQS,TT1RLNQS                                                
*                                                                               
         LA    R6,KEY2                                                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,TRNKCPY     COMPANY                                      
         MVC   ACTKUNT(2),=C'SJ'                                                
         MVC   SVKEY,KEY2                                                       
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,KEY2,KEY2                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY2(L'ACTKEY),SVKEY                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DA1,ACTKDA                                                       
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA1,IO1,WORK                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IO1                                                           
         USING ACLELD,R3                                                        
         LA    R3,ACTRFST                                                       
GETLDG30 CLI   0(R3),0                                                          
         BNE   *+6                 TEST EOR                                     
         DC    H'0'                                                             
         CLI   ACLEL,ACLELQ        TEST HIERARCHY ELEMENT                       
         BE    GETLDG40                                                         
         SR    R0,R0                                                            
         IC    R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     GETLDG30                                                         
*                                                                               
GETLDG40 DS    0H                                                               
         LA    R6,4                                                             
         AH    R3,=Y(ACLLN1Q)      BUMP TO FIRST LENGTH                         
         LA    R2,TTSJLNQS                                                      
GETLDG42 MVC   0(1,R2),0(R3)                                                    
         LA    R3,L'ACLVALS(R3)    NEXT LENGTH                                  
         LA    R2,1(R2)                                                         
         BCT   R6,GETLDG42                                                      
         MVC   BCSJLNQS,TTSJLNQS                                                
GCPYX    B     XIT                                                              
         DROP  R5,R6,R7,R3                                                      
         EJECT                                                                  
***********************************************************************         
* GET PERSON                                                          *         
***********************************************************************         
         SPACE 1                                                                
GETPER   NTR1                                                                   
         USING TRNRECD,R5                                                       
         LA    R5,RECVHDR+24                                                    
         XC    BCPIDNO,BCPIDNO     CLEAR PERSON ID #                            
*                                                                               
         LA    R6,KEY2                                                          
         XC    KEY2,KEY2                                                        
         USING PERRECD,R6                                                       
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    X'0F' PERSON RECORD                          
         MVC   PERKCPY,TRNKCPY                                                  
         LA    R2,TRNKACT                                                       
         SR    R1,R1                                                            
         IC    R1,BC1RLNQ3                                                      
         AR    R2,R1                                                            
         LA    R3,12                                                            
         SR    R3,R1                                                            
         SH    R3,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R3,*+4                                                           
         MVC   PERKCODE(0),0(R2)                                                
         MVC   SVKEY,KEY2                                                       
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,KEY2,KEY2                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY2(L'ACTKEY),SVKEY                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DA1,PERKDA                                                       
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA1,IO1,WORK                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IO1                                                           
         AH    R6,=Y(ACTRFST-ACTRECD)                                           
         SR    R1,R1                                                            
VALPRS10 IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         BE    VALPRSNX                                                         
         USING PIDELD,R6                                                        
         CLI   0(R6),PIDELQ        X'D8' PERSON ID ELEMENT                      
         BNE   VALPRS10                                                         
         MVC   BCPIDNO,PIDNO       PERSON ID NUMBER                             
*                                                                               
VALPRSNX B     XIT                                                              
         DROP  R6,R5                                                            
         EJECT                                                                  
***********************************************************************         
* GET PERIOD NUMBER                                                   *         
***********************************************************************         
         SPACE 1                                                                
GETPRD   NTR1                                                                   
         XC    PRDNUM,PRDNUM                                                    
         USING TRNRECD,R5                                                       
         LA    R5,RECVHDR+24                                                    
         MVC   PRDENDTE,TRNKDATE   IF PASSED SPECIFIC DATE USE IT               
*                                                                               
         USING CASRECD,R6          R6=A(CALENDAR RECORD)                        
         LA    R6,KEY2                                                          
         XC    KEY2,KEY2                                                        
         MVI   CASPTYP,CASPTYPQ    X'3E'                                        
         MVI   CASPSUB,CASPSUBQ    X'0C'                                        
         MVC   CASPCPY,TRNKCPY                                                  
         MVC   CASPEDTE,PRDENDTE                                                
         MVC   SVKEY,KEY2                                                       
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,KEY2,KEY2                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY2(3),SVKEY                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PRDENDTE,CASPSDTE                                                
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   PRDENDTE,CASPEDTE                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   DA1,CASPDA                                                       
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA1,IO1,WORK                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IO1                                                           
         AH    R6,=Y(ACTRFST-ACTRECD)                                           
GPRD110  CLI   0(R6),0                                                          
         BE    GPRDX                                                            
         CLI   0(R6),TMPELQ        X'88' TS PERIODS ELEMENT                     
         BE    GPRD300                                                          
GPRD120  SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     GPRD110                                                          
*                                                                               
         USING TMPELD,R6                                                        
GPRD300  CLC   PRDENDTE,TMPSTART    *** GET CALENDAR BY DATE ***                
         BL    GPRD120                                                          
         CLC   PRDENDTE,TMPEND                                                  
         BH    GPRD120                                                          
         MVC   PRDNUM,TMPNUMB     SAVE PERIOD NUMBER                            
GPRDX    B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* GET PROFILES                                                        *         
***********************************************************************         
         SPACE 1                                                                
GTPROF   NTR1                                                                   
         USING TRNRECD,R5                                                       
         LA    R5,RECVHDR+24                                                    
*                                                                               
         LA    R0,IO1                                                           
         L     R1,=A(COBLOCKX-COBLOCK)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING COBLOCKD,R3                                                      
         LA    R3,IO1                                                           
         MVC   COADM,DATAMGR       PASS A(DATA MANAGER)                         
         MVC   COBKEY(COBKEYLN),SPACES                                          
         MVC   COKCPY,TRNKCPY                                                   
         MVC   COKMTHD,SPACES                                                   
         MVC   COKOFC,SV1ROFFC                                                  
         MVC   COKDPT(L'SV1RDPT),SV1RDPT                                        
         MVC   COKSDT(L'SV1RSDPT),SV1RSDPT                                      
         MVC   COKPER,SV1RPRSN                                                  
         GOTO1 =V(GETCAP),BCDMCB,COBLOCK                                        
         CLI   COSTATUS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TTRND,R7                                                         
         LA    R7,TTRNBLK          TIMETRN CONTROL BLOCK                        
         MVI   TTPROF1,TTPR1SAL                                                 
         MVI   TTPROF2,TTPR1SAL                                                 
         CLI   COBRTE,COOPTYQ                                                   
         BE    *+8                                                              
         MVI   TTPROF1,TTPR1CST                                                 
         CLI   CORRTE,COOPTYQ                                                   
         BE    *+8                                                              
         MVI   TTPROF2,TTPR1CST                                                 
         B     XIT                                                              
         DROP  R5,R3,R7                                                         
         EJECT                                                                  
***********************************************************************         
* CALL TIMETRN TO MAKE POSTINGS                                       *         
***********************************************************************         
         SPACE 1                                                                
CALLTTRN NTR1                                                                   
         USING TRNRECD,R5                                                       
         LA    R5,RECVHDR+24                                                    
         USING TTRND,R2                                                         
         LA    R2,TTRNBLK          TIMETRN CONTROL BLOCK                        
CTT10    MVC   TTDMGR,DATAMGR      A(DATAMGR)                                   
         MVC   TTCFACS,ADCOMFAC    A(COMFACS)                                   
         L     R1,=V(RECUP)                                                     
         ST    R1,TTRECUP                                                       
         L     R1,=V(ADDTRN)                                                    
         ST    R1,TTADDTRN         A(ADDTRN)                                    
         MVC   TTSORT,XSORT        A(XSORT)                                     
         L     R1,=A(BUFF1)                                                     
         ST    R1,TTBUFF1                                                       
         L     R1,=A(BUFF2)                                                     
         ST    R1,TTBUFF2                                                       
         MVC   TTBUFFLN,=Y(BCBUFLNQ)                                            
         MVC   TTUSERID,RUSER      USER ID NUMBER                               
*                                                                               
         MVC   TTACCODE,TRNKCPY    1R ACCOUNT CODE                              
         MVC   TTYYMMDD,TRNKDATE   PERIOD ENDING DATE                           
         MVC   TTPERIOD,PRDNUM     PERIOD NUMBER                                
*                                                                               
         MVC   TTTID,=CL8'DDS'     TERMINAL ID                                  
         MVC   TTPIDNO,BCPIDNO     PERSONAL ID #                                
         MVC   TTSECPID,BCPIDNO    USER PID #                                   
         MVC   TTCTRY,RCCTRY       COUNTRY CODE                                 
*                                                                               
         MVI   TTACTION,TTBLDB4    ACTION = BUILD ORIGINAL BUFFER               
         CLI   RRECTY,X'01'        COPY?                                        
         BE    CTT50                                                            
         MVI   TTACTION,TTPOST     BUILD CHANGED BUFFER AND POST                
         CLI   RRECTY,X'02'        CHANGE?                                      
         BE    CTT50                                                            
         CLI   RRECTY,X'03'        ADD?                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,TTBUFF1          CLEAR BUFFER 1                               
         LH    RF,TTBUFFLN                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR BUFFER AREA                            
         L     R1,TTBUFF1                                                       
         MVI   0(R1),X'FF'         AND SET EOF                                  
*                                                                               
CTT50    DS    0H                                                               
         GOTO1 =A(TIMETRN),BCDMCB,TTRNBLK,(RC)                                  
         CLI   TTRETURN,TTROK                                                   
         BE    XIT                                                              
         DC    H'0'                IF DIED HERE THEN BUFFER TOO SMALL           
         DROP  R2,R5               MUST MAKE BCBUFF1 & BCBUFF2 BIGGER           
         EJECT                                                                  
***********************************************************************         
* PRINT AGENCY TOTALS                                                 *         
***********************************************************************         
         SPACE 1                                                                
PRNTOT   NTR1                                                                   
         MVC   P(17),=C'TOTAL DEBITS    ='                                      
         EDIT  TOTDR,(10,P+20),2,MINUS=YES                                      
         GOTO1 ACREPORT                                                         
         MVC   P(17),=C'TOTAL CREDITS   ='                                      
         EDIT  TOTCR,(10,P+20),2,MINUS=YES                                      
         GOTO1 ACREPORT                                                         
         MVC   P(17),=C'TOTAL HOURS     ='                                      
         EDIT  HOURS,(10,P+20),2,MINUS=YES                                      
         GOTO1 ACREPORT                                                         
         MVC   P(17),=C'RECORDS ADDED   ='                                      
         EDIT  RECADD,(10,P+20)                                                 
         GOTO1 ACREPORT                                                         
         MVC   P(17),=C'RECORDS CHANGED ='                                      
         EDIT  RECCHA,(10,P+20)                                                 
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD TIME ELEMENTS TO EXISTING RECORDS                               *         
***********************************************************************         
         SPACE 1                                                                
ADDTIM   NTR1  ,                                                                
         USING TIMRECD,R5                                                       
         LA    R5,RECVHDR+24                                                    
         SR    R0,R0                                                            
         LA    R2,TIMRFST                                                       
ADDTIM3  CLI   0(R2),0                                                          
         BE    XIT                                                              
         CLI   0(R2),X'8B'                                                      
         BNE   ADDTIM9                                                          
         USING TIMELD,R2                                                        
         CLI   TIMETYP,TIMEINP     FIND FIRST CLUSTER                           
         BNE   ADDTIM9                                                          
         MVC   OLDSEQ,TIMSEQ       SAVE SEQUENCE                                
         SR    R1,R1                                                            
         IC    R1,TIMLN            GET TOTAL LENGTH                             
         LR    RE,R2                                                            
*                                                                               
ADDTIM5  SR    RF,RF                                                            
         IC    RF,1(RE)                                                         
         AR    RE,RF               RE TO NEXT ELEMENT                           
         CLC   TIMSEQ-TIMELD(L'TIMSEQ,RE),OLDSEQ                                
         BNE   ADDTIM7                                                          
         IC    RF,TIMLN-TIMELD(RE)                                              
         AR    R1,RF               ADD LENGTH OF NEXT IN CLUSTER                
         B     ADDTIM5                                                          
*                                                                               
ADDTIM7  MVC   DKEY,TIMKEY                                                      
         BAS   RE,UPDTIM                                                        
*                                                                               
ADDTIM9  IC    R0,TIMLN            BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         B     ADDTIM3                                                          
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE EXISTING TIME RECORD                                         *         
*  DKEY  =  KEY OF TMS RECORD                                         *         
*  R2    =  A(TIME ELEMENT)                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMELD,R2                                                        
UPDTIM   NTR1  ,                                                                
         MVI   DMACT,C' '          CLEAR ACTION FLAG                            
         LA    R5,DKEY                                                          
         USING TIMRECD,R5                                                       
         MVI   TIMKSBR,0                                                        
         BAS   RE,HIGH                                                          
         LA    R5,DIR                                                           
         CLC   TIMKEY(TIMKSBR-TIMKEY),DKEY   GET FIRST TIME RECORD              
         BE    UPDTIM3                                                          
         L     R5,AIO2             NOT FOUND - BUILD FIRST TMS RECORD           
         XC    0(100,R5),0(R5)                                                  
         MVC   TIMKEY,DKEY                                                      
         MVI   TIMRRI1,TIMKRI1Q                                                 
         MVI   TIMRRI2,TIMKRI2Q                                                 
         MVI   TIMRRI3,TIMKRI3Q                                                 
         MVI   TIMRRI4,TIMKRI4Q                                                 
         LA    R1,TIMRFST-TIMRECD  BEGIN NEW RECORD                             
         STCM  R1,3,TIMRLEN                                                     
         OC    TIMRLMOS,TIMRLMOS                                                
         BZ    *+14                                                             
         CLC   TIMRLMOS,TIMMOA-TIMELD(R2)                                       
         BL    *+10                                                             
         MVC   TIMRLMOS,TIMMOA-TIMELD(R2)  SET LOWEST  MOA                      
         CLC   TIMRHMOS,TIMMOA-TIMELD(R2)                                       
         BH    *+10                                                             
         MVC   TIMRHMOS,TIMMOA-TIMELD(R2)  SET HIGHEST MOA                      
         MVI   DMACT,C'A'          ADD RECORD                                   
         B     UPDTIM4                                                          
*                                                                               
UPDTIM3  MVC   NEWSEQ,TIMKSBR      SAVE HIGHEST SUBREF                          
         BAS   RE,SEQ                                                           
         CLC   TIMKEY(TIMKSBR-TIMKEY),DKEY   GET NEXT TIME RECORD               
         BE    UPDTIM3                                                          
*                                                                               
         LA    R5,DKEY                                                          
         MVC   TIMKSBR,NEWSEQ      READ LAST RECORD                             
         BAS   RE,READ                                                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                WHERE THE FUCK DID IT GO                     
*                                                                               
         L     R3,AIO2                                                          
         BAS   RE,GET              GET THE RECORD                               
         BAS   RE,DMPGET                                                        
*                                                                               
UPDTIM4  L     R5,AIO2                                                          
         SR    R0,R0                                                            
         LA    R4,TIMRFST                                                       
         MVI   NEWSEQ,0                                                         
*                                                                               
UPDTIM5  CLI   0(R4),0                                                          
         BE    UPDTIM9                                                          
         CLI   0(R4),X'8B'                                                      
         BNE   UPDTIM7                                                          
         CLI   TIMETYP-TIMELD(R4),TIMEINP  GET LAST SEQUENCE NUMBER             
         BNE   UPDTIM7                                                          
         MVC   NEWSEQ,TIMSEQ-TIMELD(R4)   SAVE SEQUENCE                         
UPDTIM7  IC    R0,TIMLN-TIMELD(R4)                                              
         AR    R4,R0                                                            
         B     UPDTIM5                                                          
*                                                                               
UPDTIM9  IC    R0,NEWSEQ                                                        
         AH    R0,=H'1'                                                         
         STC   R0,NEWSEQ          BUMP THE SEQUENCE NUMBER                      
         CLI   DMACT,C'A'          TEST NEW RECORD                              
         BE    UPDTIM11                                                         
*                                                                               
         MVI   DMACT,C'P'          DEFAULT IS PUTREC                            
         SR    R1,R1                                                            
         ICM   R1,3,TIMRLEN        TEST RECORD LENGTH                           
         SR    RF,RF                                                            
         IC    RF,TIMLN            LENGTH OF NEW ELEMENT                        
         AR    R1,RF                                                            
         CH    R1,=H'1990'                                                      
         BNH   UPDTIM11                                                         
         MVC   TIMRLEN,=Y(TIMRFST-TIMRECD)                                      
         SR    R0,R0                                                            
         IC    R0,TIMKSBR          MUST ADD NEW RECORD                          
         AH    R0,=H'1'                                                         
         STC   R0,TIMKSBR                                                       
         XC    TIMRFST(5),TIMRFST                                               
         MVI   DMACT,C'A'          SET ADDREC                                   
*                                                                               
UPDTIM11 MVC   TIMSEQ,NEWSEQ       SET NEW REQUENCE                             
         CLI   TIMETYP,TIMEINP                                                  
         BNE   *+10                                                             
         AP    HOURS,TIMHRS                                                     
         GOTO1 ADDEL,DMCB,(R5),(R2)                                             
         SR    R0,R0                                                            
         IC    R0,1(R2)            GET NEXT ELEMENT                             
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    UPDTIM13                                                         
         CLC   TIMSEQ,OLDSEQ       TEST SAME CLUSTER                            
         BE    UPDTIM11                                                         
*                                                                               
UPDTIM13 LA    RF,PUT              SET PUTREC                                   
         CLI   DMACT,C'P'                                                       
         BE    *+8                                                              
         LA    RF,ADD                                                           
         BASR  RE,RF                                                            
         BAS   RE,DMPPUT                                                        
         B     XIT                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FIND 1R TRANSACTIONS THAT SHOULD BE TMS RECORDS                     *         
***********************************************************************         
         SPACE 1                                                                
GETTRN   NTR1  ,                                                                
GETTRN3  BAS   RE,SEQ                                                           
         CLC   DIR(15),DKEY         TEST 1R ACCOUNT                             
         BNE   XIT                                                              
         LA    R2,DIR                                                           
         USING TRNRECD,R2                                                       
         CLC   TRNKDATE,=X'970101'                                              
         BL    GETTRN3                                                          
         TM    TRNKSTAT,TRNSDRFT                                                
         BO    GETTRN3                                                          
         CLI   TRNKSTYP,57                                                      
         BE    *+12                                                             
         CLI   TRNKSTYP,34                                                      
         BNE   GETTRN3                                                          
*                                                                               
         L     R3,AIO1                                                          
         BAS   RE,GET              GET RECORD INTO IO1                          
         BAS   RE,DMPTRN           DITTO THE INPUT TRANSACTION                  
         LA    RF,RTAB             RF=A(1R RECORD ELEMENT LIST)                 
         BAS   RE,SETA             SET ADDRESS OF KEY ELEMENTS                  
         BAS   RE,PRTTRN           PRINT ELEMENT DATA                           
*                                                                               
         L     R3,AIO2                                                          
         BAS   RE,JOB              GET THE SJ TRANSACTION                       
         BAS   RE,DMPJOB                                                        
         LA    RF,JTAB                                                          
         BAS   RE,SETA             SET ELEMENT ADDRESSES                        
         BAS   RE,PRTJOB           PRINT ELEMENT DATA                           
*                                                                               
         BAS   RE,BTIML            BUILD TIME ELEMENT                           
         L     R2,AIO1                                                          
         MVC   DKEY,TRNKEY                                                      
         LA    R2,DKEY                                                          
         MVC   TRNKREF,=C'*TIME*'                                               
         LA    R2,ELEM             R2=A(NEW ELEMENT)                            
         BAS   RE,UPDTIM           ADD TIMEL TO TMS RECORD                      
         BAS   RE,PRTTMS           PRINT TMS ELEMENT                            
*                                                                               
         L     R2,AIO1                                                          
         MVC   DKEY,TRNKEY         READ OLD 1R DIRECTORY                        
         BAS   RE,READ                                                          
         CLC   DKEY,DIR                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,DIR                                                           
         OI    TRNKSTAT,X'80'      DELETE OLD POINTER                           
         BAS   RE,WRT                                                           
         LA    R3,DIR                                                           
         BAS   RE,DMPDIR                                                        
         B     GETTRN3                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FIND TMS ELEMENTS THAT CAN BE DELETED                               *         
***********************************************************************         
         SPACE 1                                                                
GETTMS   NTR1  ,                                                                
GETTMS3  BAS   RE,SEQ                                                           
         CLC   DIR(15),DKEY         TEST 1R ACCOUNT                             
         BNE   XIT                                                              
         LA    R2,DIR                                                           
         USING TIMRECD,R2                                                       
         CLC   TIMKPEDT,=X'970613'                                              
         BNE   GETTMS3                                                          
         CLC   TIMKREF,=C'*TIME*'                                               
         BNE   GETTMS3                                                          
*                                                                               
         L     R3,AIO1                                                          
         MVC   TMSDA,DA                                                         
         BAS   RE,GET              GET RECORD INTO IO1                          
         L     R2,AIO1                                                          
GETTMS4  LA    R4,TIMRFST                                                       
         USING TIMELD,R4                                                        
         SR    R0,R0                                                            
*                                                                               
GETTMS5  CLI   TIMEL,0                                                          
         BE    GETTMS20                                                         
         CLI   TIMEL,TIMELQ                                                     
         BNE   GETTMS15                                                         
         CLI   TIMETYP,TIMEINP                                                  
         BNE   GETTMS15                                                         
*                                                                               
****** SPECIAL FILTERING ******                                                 
         CLI   TIMTTYP,TIMTCB      CLIENT BILLING                               
         BNE   GETTMS15                                                         
         CLC   TIMTSK,=C'84'       WORKCODE 84                                  
         BNE   GETTMS15                                                         
         CP    TIMAMNT,=P'6500'    $65.00                                       
         BE    *+14                                                             
         CP    TIMAMNT,=P'-6500'                                                
         BNE   GETTMS15                                                         
*******************************                                                 
*                                                                               
         L     R3,AIO2                                                          
         BAS   RE,GETSJ            TEST THE SJ TRANSACTION                      
         BNE   GETTMS15            CAN'T BE DELETED                             
         AP    HOURS,TIMHRS                                                     
         AP    TOTDR,TIMAMNT                                                    
         BAS   RE,DELTMS           DELETE THE TMS ELEMENT                       
         B     GETTMS5             SEC                                          
*                                                                               
GETTMS15 IC    R0,TIMLN                                                         
         AR    R4,R0                                                            
         B     GETTMS5                                                          
*                                                                               
GETTMS20 MVC   DKEY,TIMKEY         READ ORIGINAL TMS RECORD                     
         BAS   RE,READ                                                          
         B     GETTMS3                                                          
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* GET SJ TRANSACTION FROM TIMELD                                      *         
***********************************************************************         
         SPACE 1                                                                
GETSJ    NTR1  ,                                                                
         USING TIMELD,R4                                                        
         LA    R2,DKEY                                                          
         USING TRNRECD,R2                                                       
         MVC   DKEY,SPACES                                                      
         MVC   TRNKCPY,RCCOMPFL                                                 
         MVC   TRNKULA,TIMACC                  CLI/PROD/JOB                     
         MVC   TRNKWORK,TIMTSK                 TASK CODE                        
         L     RF,AIO1                                                          
         MVC   TRNKCULC,TIMKCULA-TIMKEY(RF)    CONTRA IS 1R                     
         MVC   TRNKDATE,TIMKPEDT-TIMKEY(RF)    DATE                             
*******SPECIAL FILTER CODE ******                                               
         MVC   TRNKREF,=CL6'T027'                                               
*******************************                                                 
         MVI   TRNKSBR,0                                                        
         BAS   RE,READ             GET SJ RECORD                                
         B     *+8                                                              
*                                                                               
GETSJ3   BAS   RE,SEQ                                                           
         CLC   DIR(TRNKSBR-TRNKEY),DKEY                                         
         BNE   XIT                                                              
         LA    R2,DIR                                                           
*        CLI   TRNKSTYP,49                                                      
*        BE    *+12                                                             
*        CLI   TRNKSTYP,34                                                      
*        BNE   GETSJ3                                                           
         BAS   RE,GET                                                           
         BAS   RE,DMPJOB                                                        
         LA    RF,JTAB                                                          
         BAS   RE,SETA             SET ELEMENT ADDRESSES                        
         L     RF,APRTJ            GET RATE ELEMENT                             
*        CLC   TIMLINE#,PRTLINE#-PRTELD(RF) MATCH LINE NUMBER                   
*        BNE   GETSJ3                                                           
         L     R5,ATRNJ                                                         
         USING TRNELD,R5                                                        
         CP    TRNAMNT,TIMAMNT     MUST MATCH AMOUNT                            
         BNE   GETSJ3                                                           
         TM    TRNSTAT,TRNSREV     TEST REVERSE                                 
         BNO   GETSJ3                                                           
         CR    RB,RB                                                            
         B     XIT                                                              
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* DELETE TMS ELEMENT                                                  *         
***********************************************************************         
         SPACE 1                                                                
DELTMS   NTR1  ,                                                                
         USING TIMELD,R4                                                        
DELTMS3  MVC   WORK,SPACES                                                      
         MVC   WORK(3),=C'DA='                                                  
         GOTO1 HEXOUT,DMCB,TMSDA,WORK+3,4,0                                     
         MVC   WORK+15(3),=C'AT='                                               
         LR    RF,R4               RF=ADDRESS OF ELEMENT                        
         L     R1,AIO1                                                          
         SR    RF,R1               LESS START                                   
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+18(4),DUB                                                   
         LA    R6,WORK                                                          
         SR    RF,RF                                                            
         IC    RF,TIMLN                                                         
         GOTO1 PRNTBL,DMCB,(30,(R6)),(R4),C'DUMP',(RF),=C'2D'                   
         MVI   0(R4),X'FF'                                                      
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'FF',AIO1),0,0                        
         CLI   0(R4),0                                                          
         BE    XIT                                                              
         CLI   TIMETYP,TIMEINP                                                  
         BE    XIT                                                              
         B     DELTMS3                                                          
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* GET POSTING INFO FROM SJ TRANSACTION                                *         
***********************************************************************         
         SPACE 1                                                                
JOB      NTR1  ,                                                                
         LA    R2,DKEY                                                          
         USING TRNRECD,R2                                                       
         MVC   DKEY,SPACES                                                      
         L     R5,APCIR                                                         
         USING PCIELD,R5                                                        
         MVC   TRNKCULA,PCIPRJT    CLI/PROD/JOB                                 
         MVC   TRNKWORK,PCITSK       TASK CODE                                  
         L     RF,AIO1                                                          
         MVC   TRNKCULC,TRNKCULA-TRNKEY(RF)    CONTRA IS 1R                     
         MVC   TRNKDATE,TRNKDATE-TRNKEY(RF)    DATE                             
         MVC   TRNKREF,TRNKREF-TRNKEY(RF)      REFERENCE                        
         MVI   TRNKSBR,0                                                        
         BAS   RE,READ             GET SJ RECORD                                
         B     *+8                                                              
*                                                                               
JOB3     BAS   RE,SEQ                                                           
         CLC   DIR(TRNKSBR-TRNKEY),DKEY                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,DIR                                                           
         CLI   TRNKSTYP,57                                                      
         BE    *+12                                                             
         CLI   TRNKSTYP,34                                                      
         BNE   JOB3                                                             
         BAS   RE,GET                                                           
         LA    R4,TRNRFST-TRNRECD(R3)                                           
         USING TRNELD,R4                                                        
         CP    TRNAMNT,JOBAMNT     MUST MATCH AMOUNT                            
         BNE   JOB3                                                             
         B     XIT                                                              
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD TIMEL FROM TRANSACTION                                        *         
***********************************************************************         
         SPACE 1                                                                
BTIML    NTR1  ,                                                                
BTIML7   L     R4,ATRNR                                                         
         USING TRNELD,R4                                                        
         LA    R7,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING TIMELD,R7                                                        
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMLN,TIMILN2Q                                                   
         MVI   TIMETYP,TIMEINP                                                  
*                                                                               
         L     R5,APCIR                                                         
         USING PCIELD,R5                                                        
         MVC   TIMACC,PCIPRJT+1    SJ ACCOUNT                                   
         MVC   TIMTSK,PCITSK       TASK CODE                                    
         MVC   TIMOFF,TRNOFFC      OFFICE                                       
         MVI   TIMTTYP,TIMTCB      BILLABLE                                     
         OI    TIMIND,TIMIADJ      ADJUSTMENT                                   
*                                                                               
         L     R5,ATRSR                                                         
         USING TRSELD,R5                                                        
         MVC   TIMMOA,TRSPMOS      MONTH OF ACTIVITY                            
         GOTO1 DATCON,DMCB,(2,TRSDATE),(1,TIMADAT)                              
*                                                                               
         L     R5,APRTR                                                         
         USING PRTELD,R5                                                        
         ZAP   TIMHRS,PRTHOUR      HOURS                                        
         ZAP   TIMRATE,PRTRATE     RATE                                         
         MVC   TIMREFF,PRTSTRT     RATE EFFECTIVE DATE                          
         TM    PRTSTAT,PRTSADJ     TEST RATE WAS ADJUSTED                       
         BZ    *+8                                                              
         OI    TIMRBSTA,TIMRBADJ                                                
*                                                                               
         ICM   R6,15,ADACCSTA      GET NEXT LINE NUMBER                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING RSTELD,R6                                                        
         SR    R1,R1                                                            
         ICM   R1,3,RSTSNUM                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,TIMLINE#       NEXT TS #                                    
         STCM  R1,3,RSTSNUM                                                     
         MVI   MODE,WRITACC        UPDATE ACCOUNT                               
         DROP  R6                                                               
*                                                                               
         L     R4,ATRNJ            JOB TRANSACTION                              
         ZAP   TIMAMNT,TRNAMNT                                                  
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,7,ASPDJ+1       A(SPECIAL POSTING ELEMENT)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING SPDEL,R5                                                         
         MVC   TIMINC,SPACES                                                    
         SR    R1,R1                                                            
         IC    R1,SPDLN            GET INCOME ACCOUNT                           
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   TIMINC(0),SPDACCS                                                
         B     XIT                                                              
         DROP  R4,R5,R7                                                         
         EJECT                                                                  
***********************************************************************         
* SET ADDRESSES OF KEY ELEMENTS                                       *         
* ON ENTRY R3=A(RECORD)                                               *         
*          RF=A(ELEMENT ADDRESS LIST)                                 *         
***********************************************************************         
         SPACE 1                                                                
SETA     NTR1  ,                                                                
         LR    RE,RF                                                            
         LR    R2,R3                                                            
         XC    1(3,RF),1(RF)                                                    
         LA    RF,4(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   *-14                                                             
*                                                                               
         USING TRNRECD,R2                                                       
         LA    R4,TRNRFST                                                       
         SR    R0,R0                                                            
SETA3    CLI   0(R4),0                                                          
         BE    XIT                                                              
         LR    RF,RE                                                            
SETA5    CLC   0(1,R4),0(RF)       TEST ELEMENT CODE                            
         BNE   *+8                                                              
         STCM  R4,7,1(RF)          SAVE ADDRESS OF ELEMENT                      
         LA    RF,4(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   SETA5                                                            
         IC    R0,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         B     SETA3                                                            
*                                                                               
RTAB     DS    0F                  1R RECORD ELEMENTS                           
ATRNR    DC    X'44',AL3(0)        TRANSACTION                                  
APRTR    DC    X'40',AL3(0)        RATE ELEMENT                                 
APCIR    DC    X'51',AL3(0)        PROJECT CONTROL                              
ATRSR    DC    X'60',AL3(0)        TRANSACTION                                  
         DC    X'FF'                                                            
*                                                                               
JTAB     DS    0F                  SJ RECORD ELEMENTS                           
ATRNJ    DC    X'44',AL3(0)        TRANSACTION                                  
APRTJ    DC    X'40',AL3(0)        RATE ELEMENT                                 
ASPDJ    DC    X'4C',AL3(0)        SPECIAL POSTING                              
         DC    X'FF'                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
HIGH     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
READ     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SEQ      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
WRT      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
GET      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,(R3),DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
ADD      LR    R0,RE                                                            
         XC    DA,DA                                                            
         AP    RECADD,=P'1'                                                     
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,(R3),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PUT      LR    R0,RE                                                            
         AP    RECCHA,=P'1'                                                     
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,(R3),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT ELEMENT DATA                                                  *         
***********************************************************************         
         SPACE 1                                                                
PRTTRN   NTR1  ,                   INPUT TRANSACTION                            
         L     R5,APRTR                                                         
         USING PRTELD,R5                                                        
         MVC   P+1(6),=C'HOURS='                                                
         EDIT  PRTHOUR,(8,P+7),2,MINUS=YES,ALIGN=LEFT                           
         MVC   P+20(5),=C'RATE='                                                
         EDIT  PRTRATE,(8,P+25),2,MINUS=YES,ALIGN=LEFT                          
         ZAP   DUB,PRTHOUR         HOURS                                        
         MP    DUB,PRTRATE         X RATE                                       
         SRP   DUB,64-2,5                                                       
         ZAP   JOBAMNT,DUB                                                      
         MVC   P+35(7),=C'AMOUNT='                                              
         EDIT  JOBAMNT,(8,P+42),2,MINUS=YES,ALIGN=LEFT                          
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*                                                                               
PRTJOB   NTR1  ,                   JOB TRANSACTION                              
         L     R5,ATRNJ                                                         
         USING TRNELD,R5                                                        
         MVC   P+1(7),=C'AMOUNT='                                               
         EDIT  TRNAMNT,(9,P+8),2,MINUS=YES,ALIGN=LEFT                           
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*                                                                               
PRTTMS   NTR1  ,                   INPUT TRANSACTION                            
         LA    R5,ELEM                                                          
         USING TIMELD,R5                                                        
         MVC   P+1(6),=C'HOURS='                                                
         EDIT  TIMHRS,(8,P+7),2,MINUS=YES,ALIGN=LEFT                            
         MVC   P+20(5),=C'RATE='                                                
         EDIT  TIMRATE,(8,P+25),2,MINUS=YES,ALIGN=LEFT                          
         MVC   P+35(7),=C'AMOUNT='                                              
         EDIT  TIMAMNT,(8,P+42),2,MINUS=YES,ALIGN=LEFT                          
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* TRACE SOME RECORDS                                                  *         
***********************************************************************         
         SPACE 1                                                                
DMPTRN   NTR1  ,                   INPUT TRANSACTION                            
         MVI   DMPSW,C'N'                                                       
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         MVI   DMPSW,C'Y'                                                       
         AP    PDUMP,=P'1'                                                      
         LA    R6,=C'TRN'                                                       
         B     DMPREC                                                           
*                                                                               
DMPJOB   NTR1  ,                                                                
         LA    R6,=C'JOB'                                                       
         B     DMPREC                                                           
*                                                                               
DMPGET   NTR1  ,                                                                
         LA    R6,=C'GET'                                                       
         B     DMPREC                                                           
*                                                                               
DMPPUT   NTR1  ,                                                                
         LA    R6,=C'PUT'                                                       
         CLI   DMACT,C'A'                                                       
         BNE   *+8                                                              
         LA    R6,=C'ADD'                                                       
*                                                                               
DMPREC   SR    RF,RF                                                            
         ICM   RF,3,TIMRLEN-TIMRECD(R3)                                         
         B     DMPX                                                             
*                                                                               
DMPDIR   NTR1  ,                    DUMP DIRECTORY                              
         LA    R6,=C'DIR'                                                       
         LA    RF,L'DIR                                                         
DMPX     CLI   DMPSW,C'Y'                                                       
         BNE   XIT                                                              
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R3),C'DUMP',(RF),=C'2D'                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
GETEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'G',ACCMST),((R4),(R2)),((R5),(R3))                 
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO DELETE AN ELEMENT                                     
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',ACCMST),((R4),(R2)),((R5),(R3))                 
         B     XIT                                                              
         SPACE 1                                                                
*              ROUTINE TO ADD AN ELEMENT                                        
         SPACE 1                                                                
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
         SPACE 1                                                                
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),(R3)                               
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
GETREC   DC    CL8'GETREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
         SPACE 1                                                                
         EJECT                                                                  
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
         DC    X'FF'                                                            
*                                                                               
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
AIO3     DC    A(IO3)                                                           
         SPACE 1                                                                
CHACNT   DC    PL6'0'              COUNT OF RECORDS CHANGED                     
NOCHACNT DC    PL6'0'              COUNT OF RECORDS NOT CHANGED                 
LOCKCNT  DC    PL6'0'              COUNT OF RECORDS LOCKED                      
TOT      DC    PL6'0'                                                           
CHANGED  DC    CL1'N'              WRITE FLAG                                   
         SPACE 1                                                                
DKEY     DS    CL(L'ACCKEY)                                                     
DIR      DS    CL64                                                             
DA       DS    F                                                                
TMSDA    DS    F                                                                
         SPACE 1                                                                
EOFRCV   DC    C'N'                                                             
OLDSEQ   DC    X'00'                                                            
NEWSEQ   DC    X'00'                                                            
JOBAMNT  DS    PL6                                                              
*                                                                               
DMACT    DS    C                                                                
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'200'                                                         
DMPSW    DS    C                                                                
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
FORMAT   DC    C'1D'                                                            
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,                        X        
               MACRF=GM,EODAD=ENDRCV                                            
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 1                                                                
RDATA    DS    2000C                                                            
RDATALNQ EQU   2000                                                             
*                                                                               
IO1      DS    CL2000                                                           
IO2      DS    CL2000                                                           
IO3      DS    CL2000                                                           
*                                                                               
BUFF1    DS    CL2000                                                           
BUFF2    DS    CL2000                                                           
*                                                                               
         EJECT                                                                  
*******************************************************************             
*  TIME TRN TO ADD TRANSACTIONS                                                 
*******************************************************************             
         DS    0H                                                               
TIMETRN  NMOD1 0,TIM,R8                                                         
         USING TTRND,R9                                                         
         L     R9,0(R1)            PARAMETER 1 =A(CONTROL BLOCK)                
         L     RC,4(R1)                                                         
*                                                                               
         MVI   TTRETURN,TTROK                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*        LH    R1,=Y(ACTRFST-ACTRECD)                                           
*        STH   R1,DATADISP                                                      
         ICM   R1,15,TTCFACS       A(COMFACS)                                   
         BNZ   *+6                                                              
         DC    H'0'                MISSING ADDRESS OF COMFACS                   
*                                                                               
         TM    TTACTION,TTBLDB4    BUILD ORIGINAL BUFFER                        
         BNO   TIME20                                                           
         BAS   RE,BUILD                                                         
         B     EXIT                                                             
*                                                                               
TIME20   TM    TTACTION,TTCMPARE                                                
         BO    *+12                                                             
         TM    TTACTION,TTPOST     POST TRANSACTIONS                            
         BNO   TIME30                                                           
         TM    TTACTION,TTBLDTMS   DONT BUILD 2ND BUFFER IF PREBUILT            
         BO    *+8                 BUFFER IS BEING PASSED.                      
         BAS   RE,BUILD                                                         
         BAS   RE,POST                                                          
         B     EXIT                                                             
*                                                                               
TIME30   OI    TTRETURN,TTRACTN    *** ERROR - NO ACTION SPECIFIED ***          
         B     EXIT                                                             
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* BUILD BUFFER                                                        *         
***********************************************************************         
         SPACE 1                                                                
BUILD    NTR1                                                                   
         MVC   BCABUFF,TTBUFF1     USE BUFFER#1 FOR BUILD                       
         TM    TTACTION,TTBLDB4                                                 
         BO    *+10                                                             
         MVC   BCABUFF,TTBUFF2     USE BUFFER#2 FOR BUILD/POST                  
*                                                                               
         USING TMSD,R4                                                          
         ICM   R4,15,BCABUFF       R4=A(BUFFER)                                 
         BNZ   *+6                                                              
         DC    H'0'                INVALID BUFFER ADDRESS                       
         LH    R1,TTBUFFLN                                                      
         SH    R1,=H'1'                                                         
         AR    R1,R4                                                            
         ST    R1,AEOB             A(END OF BUFFER)                             
         ST    R1,ANEXT            A(NEXT AVAILABLE STORAGE)                    
         L     RE,BCABUFF                                                       
         LH    RF,TTBUFFLN                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR BUFFER AREA                            
         MVI   0(R4),X'FF'         MARK END OF TABLE                            
         XC    TMSNUM,TMSNUM       CLEAR TABLE ENTRY COUNT                      
*                                                                               
         LR    R6,R5               REC AT R5                                    
         LH    R1,=Y(ACTRFST-ACTRECD)                                           
         AR    R6,R1                                                            
BLD300   CLI   0(R6),0             END OF RECORD                                
         BE    BLDX                                                             
         CLI   0(R6),TIMELQ        X'8B'                                        
         BNE   BLD400                                                           
         CLI   TIMETYP-TIMEL(R6),TIMEINP  BRANCH ON START OF CLUSTER            
         BE    BLD500                     & SKIP TAX/NARRATIVE ITEMS            
BLD400   SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     BLD300                                                           
*                                                                               
BLD500   MVC   TMSOFFC,TIMKOFF-TIMKEY(R5)  OFFICE CODE                          
         MVC   TMSCNTRA,TIMKULC-TIMKEY(R5)   CONTRA ACCOUNT                     
*                                                                               
         USING TIMELD,R2                                                        
         LR    R2,R6               R5=A(CURRENT X'8B' DETAIL ITEM)              
BLD600   CLI   TIMEL,TIMELQ                                                     
         BE    *+6                                                              
         DC    H'0'                NOT X'8B' ELEMENT                            
*                                                                               
         CLI   TIMETYP,TIMEINP     INPUT DETAIL ITEM                            
         BNE   BLD650                                                           
         MVC   TMSACC,TIMACC       SJ OR 1N ACCOUNT                             
         MVC   TMSTSK,TIMTSK       TASK                                         
         MVC   TMSTTYP,TIMTTYP     TYPE OF TIME                                 
         MVC   TMSLINE#,TIMLINE#   LINE #                                       
         MVC   TMSIND,TIMIND       TIME INDICATOR                               
         MVC   TMSMOA,TIMMOA       MONTH OF ACTIVITY                            
         MVC   TMSSTAT,TIMSTAT     STATUS                                       
         ZAP   TMSHRS,TIMHRS       HOURS                                        
         ZAP   TMSRATE,=P'0'                                                    
*&&UK*&& ZAP   TMSCRATE,=P'0'                                                   
         CLI   TIMLN,TIMILN2Q                                                   
         BL    BLD700                                                           
         ZAP   TMSRATE,TIMRATE     RATE                                         
         MVC   TMSRSTA,TIMRBSTA    BILLABLE TIME STATUS                         
         MVC   TMSREFF,TIMREFF     RATE EFFECTIVE DATE                          
         MVC   TMSINC,TIMINC       INCOME ACCOUNT                               
*&&UK*&& ZAP   TMSCRATE,TIMCRATE   COST RATE                                    
*&&UK*&& MVC   TMSCREFF,TIMCREFF   COST RATE EFFECTIVE DATE                     
         B     BLD700                                                           
*                                                                               
BLD650   CLI   TIMETYP,TIMETAX     TAX DETAIL ITEM                              
         BE    *+12                                                             
         CLI   TIMETYP,TIMENAR     NARRATIVE ELEMENT                            
         BNE   BLD700                                                           
         L     R3,ANEXT            A(END OF NEXT STORAGE ENTRY)                 
         SR    R1,R1                                                            
         IC    R1,TIMLN            LENGTH OF TAX ENTRY                          
         SR    R3,R1                                                            
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   0(0,R3),TIMEL       COPY ELEMENT                                 
         ST    R3,ANEXT                                                         
         LA    RF,TMSATAX                                                       
         CLI   TIMETYP,TIMETAX                                                  
         BE    *+8                                                              
         LA    RF,TMSANARR                                                      
         STCM  R3,15,0(RF)                                                      
*                                                                               
BLD700   SR    R1,R1               LOOP THROUGH ENTIRE CLUSTER                  
         IC    R1,1(R2)                                                         
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         CLI   TIMEL,TIMELQ                                                     
         BNE   BLD710                                                           
         CLI   TIMETYP,TIMETAX     TAX DETAIL ITEM                              
         BE    BLD600                                                           
         CLI   TIMETYP,TIMENAR     NARRATIVE ITEM                               
         BE    BLD600                                                           
*                                                                               
BLD710   LA    R4,TMSLNQ(R4)       BUMP TO NEXT TABLE ENTRY                     
         MVI   0(R4),X'FF'                                                      
         SR    R1,R1                                                            
         ICM   R1,3,TMSNUM                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,TMSNUM         BUMP TABLE COUNT                             
         L     R1,ANEXT            A(AVAILABLE BUFFER SPACE)                    
         CR    R4,R1                                                            
         BL    BLD400                                                           
         OI    TTRETURN,TTRBUFFR   BUFFER IS FILLED                             
         B     EXIT                                                             
*                                                                               
BLDX     SR    R3,R3                                                            
         ICM   R3,3,TMSNUM                                                      
         BZ    EXIT                                                             
         GOTO1 TTSORT,BCDMCB,BCABUFF,(R3),TMSLNQ,TMSKSRTQ,0                     
         B     EXIT                                                             
         DROP  R4,R2                                                            
         EJECT                                                                  
***********************************************************************         
* CREATE TRANSACTIONS/UPDATE BUCKETS ACTION                           *         
***********************************************************************         
         SPACE 1                                                                
POST     NTR1                                                                   
         XC    STATUS,STATUS       CLEAR STATUS BYTE                            
*                                                                               
         USING TMSD,R4                                                          
         ICM   R4,15,TTBUFF1       R4=A(ORIGINAL RECORD BUFFER)                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),X'FF'         IF ORIGINAL BUFFER IS EMPTY                  
         BNE   *+8                 THEN ADD NEW POSTINGS                        
         OI    STATUS,STADDNEW                                                  
*                                                                               
         ICM   R6,15,TTBUFF2       R6=A(NEW RECORD BUFFER)                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),X'FF'         IF NEW BUFFER IS EMPTY THEN                  
         BNE   *+8                 THEN BACKOUT OLD POSTINGS                    
         OI    STATUS,STBCKOUT                                                  
*                                                                               
POST30   ST    R4,BCAORIG          SAVE A(CURRENT ORIGINAL ITEM)                
         ST    R6,BCANEW           SAVE A(CURRENT NEW ITEM)                     
         TM    STATUS,STADDNEW+STBCKOUT                                         
         BO    EXIT                IF BOTH ARE SET THEN DONE                    
         TM    STATUS,STADDNEW                                                  
         BO    POSTADD                                                          
         TM    STATUS,STBCKOUT                                                  
         BO    POSTNEG                                                          
*                                                                               
*              COMPARE KEY - LINE#                                              
*                                                                               
         CLC   TMSLINE#,TMSLINE#-TMSD(R6)                                       
         BL    POSTNEG                                                          
         BH    POSTADD                                                          
*                                                                               
*              COMPARE ELEMENT DATA                                             
*                                                                               
POST35   DS    0H                                                               
         CLC   TMSKEY(TMSKLNQ),TMSKEY-TMSD(R6)                                  
         BNE   POSTNEG                                                          
         CLC   TMSKDATA(TMSKDL2Q),TMSKDATA-TMSD(R6)                             
         BNE   POSTNGAD                                                         
*                                                                               
*              CHECK FOR CHANGE IN NARRATIVE                                    
*                                                                               
         MVC   BCWORK,SPACES                                                    
         USING TIMELD,R5                                                        
         SR    R5,R5                                                            
         ICM   R5,15,TMSANARR      A(NARRATIVE)                                 
         BZ    POST40                                                           
         SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         SH    R1,=Y(TIMHLNQ+1)    LENGTH OF HEADER                             
         BM    POST40                                                           
         EX    R1,*+4                                                           
         MVC   BCWORK(0),TIMNARR                                                
         OC    BCWORK,SPACES                                                    
*                                                                               
POST40   MVC   BCWORK2,SPACES                                                   
         USING TIMELD,R5                                                        
         SR    R5,R5                                                            
         ICM   R5,15,TMSANARR-TMSD(R6)                                          
         BZ    POST50                                                           
         SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         SH    R1,=Y(TIMHLNQ+1)    LENGTH OF HEADER                             
         BM    POST50                                                           
         EX    R1,*+4                                                           
         MVC   BCWORK2(0),TIMNARR                                               
         OC    BCWORK2,SPACES                                                   
*                                                                               
POST50   CLC   BCWORK,BCWORK2      DID NARRATIVE CHANGE                         
         BNE   POSTNGAD                                                         
*                                                                               
*              CHECK FOR CHANGE IN TAX INFORMATION                              
*                                                                               
         XC    BCWORK,BCWORK                                                    
         USING TIMELD,R5                                                        
         SR    R5,R5                                                            
         ICM   R5,15,TMSATAX       A(TAX ENTRY)                                 
         BZ    POST60                                                           
         SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         SH    R1,=H'1'                                                         
         BM    POST60                                                           
         EX    R1,*+4                                                           
         MVC   BCWORK(0),0(R5)                                                  
*                                                                               
POST60   XC    BCWORK2,BCWORK2                                                  
         USING TIMELD,R5                                                        
         SR    R5,R5                                                            
         ICM   R5,15,TMSATAX-TMSD(R6)                                           
         BZ    POST70                                                           
         SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         SH    R1,=H'1'                                                         
         BM    POST70                                                           
         EX    R1,*+4                                                           
         MVC   BCWORK2(0),0(R5)                                                 
*                                                                               
POST70   CLC   BCWORK,BCWORK2      DID TAX INFORMATION CHANGE                   
         BNE   POSTNGAD                                                         
         BAS   RE,NEXTNEW          NOTHING CHANGED SO BUMP BOTH                 
         BAS   RE,NEXTORIG         CLUSTER POINTERS                             
         B     POST30                                                           
*                                                                               
POSTNGAD TM    TTACTION,TTCMPARE                                                
         BO    POSTCMP                                                          
         BAS   RE,BACKOUT          BACK OUT OLD POSTINGS                        
         BAS   RE,ADDPOST          ADD NEW POSTINGS                             
         BAS   RE,NEXTORIG         BUMP TO NEXT CLUSTER IN ORIG RECORD          
         BAS   RE,NEXTNEW          BUMP TO NEXT CLUSTER IN NEW RECORD           
         B     POST30                                                           
*                                                                               
POSTNEG  TM    TTACTION,TTCMPARE                                                
         BO    POSTCMP                                                          
         BAS   RE,BACKOUT          BACK OUT POSTING FROM ORIG CLUSTER           
         BAS   RE,NEXTORIG         AND ONLY BUMP BUFFER                         
         B     POST30                                                           
*                                                                               
POSTADD  TM    TTACTION,TTCMPARE                                                
         BO    POSTCMP                                                          
         BAS   RE,ADDPOST          ADD NEW POSTINGS                             
         BAS   RE,NEXTNEW          BUMP TO NEXT CLUSTER IN NEW RECORD           
         B     POST30                                                           
*                                                                               
POSTCMP  MVI   TTRETURN,TTRUPDTE   ITEMS HAVE CHANGED - FORCE UPDATE            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUMP TO NEXT CLUSTER IN NEW RECORD BUFFER                           *         
***********************************************************************         
         SPACE 1                                                                
NEXTNEW  LA    R6,TMSLNQ(R6)                                                    
         CLI   0(R6),X'FF'                                                      
         BNE   *+8                                                              
         OI    STATUS,STBCKOUT     NO MORE IN RECORD-JUST NEGATE BUFFER         
         BR    RE                                                               
***********************************************************************         
* BUMP TO NEXT CLUSTER IN ORIGINAL RECORD BUFFER                      *         
***********************************************************************         
         SPACE 1                                                                
NEXTORIG LA    R4,TMSLNQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   *+8                                                              
         OI    STATUS,STADDNEW     NO MORE IN BUFFER-JUST ADD RECORD            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* MAKE NEGATIVE POSTINGS FROM BUFFER                                  *         
***********************************************************************         
         SPACE 1                                                                
BACKOUT  NTR1                                                                   
         OI    STATUS,STORIGNL+STMINUS                                          
         BAS   RE,GOTRNS                                                        
         NI    STATUS,X'FF'-STORIGNL-STMINUS                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD POSTINGS FROM CLUSTER                                           *         
***********************************************************************         
         SPACE 1                                                                
ADDPOST  NTR1                                                                   
         OI    STATUS,STANEW                                                    
         BAS   RE,GOTRNS           BUILD TRANACTIONS                            
         NI    STATUS,X'FF'-STANEW                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD TRANSACTIONS                                                  *         
***********************************************************************         
         SPACE 1                                                                
GOTRNS   NTR1                                                                   
         USING TMSD,R4             R4=A(CURRENT TABLE ENTRY)                    
         L     R4,BCAORIG                                                       
         TM    STATUS,STORIGNL                                                  
         BO    *+8                                                              
         L     R4,BCANEW                                                        
*                                                                               
         MVC   BC1ROFC,SPACES      ISOLATE 1R OFFICE CODE                       
         SR    R1,R1                                                            
         IC    R1,TT1RLNQ1                                                      
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                1R LEDGER STRUCTURE MESSED UP                
         EX    R1,*+4                                                           
         MVC   BC1ROFC(0),TTACKACT                                              
*                                                                               
         MVI   BCLNARR,0                                                        
         XC    BCNARR,BCNARR                                                    
         USING TIMELD,R5                                                        
         SR    R5,R5                                                            
         ICM   R5,15,TMSANARR                                                   
         BZ    GOTRN10                                                          
         SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         SH    R1,=Y(TIMHLNQ+1)                                                 
         EX    R1,*+4                                                           
         MVC   BCNARR(0),TIMNARR                                                
         LA    R1,1(R1)                                                         
         STC   R1,BCLNARR                                                       
*                                                                               
GOTRN10  ZAP   BCDUB,TMSRATE       CALCULATE TOTAL POSTING AMOUNT               
*&&UK                                                                           
         LA    RE,TTPROF1          PROF1 FOR B - TIME                           
         CLI   TMSTTYP,TMSTCB      ARE WE DOING B - TIME                        
         BE    *+8                                                              
         LA    RE,TTPROF2          PROF2 FOR N AND R TIME                       
         TM    0(RE),TTPR1CST      USE COST RATE INSTEAD OF SALES RATE          
         BNO   *+10                                                             
         ZAP   BCDUB,TMSCRATE                                                   
*&&                                                                             
         MP    BCDUB,TMSHRS                                                     
         SRP   BCDUB,64-2,5                                                     
         ZAP   BCAMOUNT,BCDUB                                                   
         MVC   BCMOA,TMSMOA        MONTH OF ACTIVITY                            
*                                                                               
         MVC   BCREF,SPACES        CREATE BATCH REFERENCE #                     
         MVI   BCREF,C'T'                                                       
         SR    R1,R1               PERIOD NUMBER INTO CHARS 2-4                 
         IC    R1,TTPERIOD         OF BATCH REFERENCE                           
         CVD   R1,BCDUB                                                         
         OI    BCDUB+7,X'0F'                                                    
         UNPK  BCREF+1(3),BCDUB+6(2)                                            
*                                                                               
         LA    R0,BCACCTS          CLEAR ACCOUNT/NAME AREA                      
         LA    R1,BCACCTQ                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   BC1RACCT,TTACKUL    VALIDATE 1R ACCOUNT                          
         GOTO1 GETACT,BC1RACCT                                                  
         MVC   BC1RNAME,BCACNAME                                                
*                                                                               
         MVC   BCSJACCT,TMSACC     VALIDATE SJ OR 1N ACCOUNT                    
         GOTO1 GETACT,BCSJACCT                                                  
         MVC   BCSJNAME,BCACNAME                                                
*                                                                               
         CLC   TMSACC(2),=C'SJ'    GET SJ CLIENT NAME                           
         BNE   GOTRN15                                                          
         MVC   BCACCODE,SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,TTSJLNQ1         LENGTH OF 1ST LEVEL                          
         LA    R1,2(R1)            ADD TWO FOR U/L 'SJ'                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   BCACCODE(0),TMSACC                                               
         GOTO1 GETACT,BCACCODE                                                  
         MVC   BCSJCLNM,BCACNAME                                                
*                                                                               
GOTRN15  MVC   BCCNTRA,TMSCNTRA    VALIDATE 1C ACCOUNT                          
         GOTO1 GETACT,BCCNTRA                                                   
         MVC   BCCNTRNM,BCACNAME                                                
*                                                                               
         CLI   TMSTTYP,TMSTCB      ONLY VALIDATE SI/SK/12 FOR B TIME            
         BNE   GOTRN20                                                          
         MVC   BCSIACCT,TMSINC     VALIDATE SI OR SK ACCOUNT                    
         GOTO1 GETACT,BCSIACCT                                                  
         MVC   BCSINAME,BCACNAME                                                
*                                                                               
         CLC   BCSIACCT(2),=C'SI'                                               
         BNE   GOTRN20                                                          
         MVC   BC12ACCT,SPACES                                                  
         MVC   BC12ACCT(2),=C'12'  VALIDATE 12 ACCOUNT                          
         MVC   BC12ACCT+2(1),BCACCOST                                           
         CLC   BCACANAL,SPACES                                                  
         BNH   *+10                                                             
         MVC   BC12ACCT+2(12),BCACANAL                                          
         GOTO1 GETACT,BC12ACCT                                                  
         MVC   BC12NAME,BCACNAME                                                
*                                                                               
*              *** DR 1R/1C - ONLY UPDATE BUCKETS (CLIENT TIME) ***             
*              *** DR 1R/1N - ONLY UPDATE BUCKETS (NONCLIENT TIME) ***          
*                                                                               
GOTRN20  DS    0H                                                               
         BAS   RE,XCIO                                                          
         OI    POSTSTAT,POST1R     1R POSTING                                   
*                                                                               
         USING TRNRECD,R2                                                       
         LA    R2,TRNIO                                                         
         MVC   TRNKCPY,TTACKCPY                                                 
         MVC   TRNKULA,BC1RACCT    1R ACCOUNT                                   
         MVC   TRNKOFF,TMSOFFC     CLIENT OFFICE                                
         MVC   TRNKCCPY,TTACKCPY                                                
         MVC   TRNKULC,BCCNTRA     COSTING ACCOUNT                              
         CLI   TMSTTYP,TMSTNC      IF POSTING NON-CLIENT TIME                   
         BNE   *+10                                                             
         MVC   TRNKOFF,BC1ROFC     OFFICE IS 1R OFFICE CODE                     
         TM    TTCPYST4,CPYSOFF2                                                
         BO    *+10                                                             
         MVC   TRNKOFF,SPACES      OLD OFFICES = NO OFFICE IN KEY               
         MVC   TRNKDATE,TTYYMMDD                                                
         MVC   TRNKREF,BCREF                                                    
         GOTO1 ELEM44,(R4)                                                      
         GOTO1 ELEM50,(R4)                                                      
         GOTO1 ELEM60,(R4)                                                      
         OI    STATUS,STBUCKET     ONLY UPDATE BUCKETS                          
         GOTO1 GOADDTRN,BCCNTRNM                                                
         NI    STATUS,X'FF'-STBUCKET                                            
         NI    POSTSTAT,X'FF'-POST1R                                            
*                                                                               
*              *** DR SJ/1R ***                                                 
*                                                                               
GOTRN30  DS    0H                                                               
         CLI   TMSTTYP,TMSTCN      CONTINUE IF CLIENT TIME                      
         BH    EXIT                                                             
         CLC   BCSJJOB,SPACES      NO SJ POSTING IF NO JOB                      
         BNH   EXIT                                                             
         BAS   RE,XCIO                                                          
         OI    POSTSTAT,POSTSJ     SJ POSTING                                   
*                                                                               
         USING TRNRECD,R2                                                       
         LA    R2,TRNIO                                                         
         MVC   TRNKCPY,TTACKCPY                                                 
         MVC   TRNKULA,BCSJACCT    SJ ACCOUNT                                   
         MVC   TRNKWORK,TMSTSK     TSK CODE                                     
         MVC   TRNKCCPY,TTACKCPY                                                
         MVC   TRNKULC,BC1RACCT    1R ACCOUNT                                   
         MVC   TRNKDATE,TTYYMMDD                                                
         MVC   TRNKREF,BCREF                                                    
         GOTO1 ELEM44,(R4)                                                      
         GOTO1 ELEM2C,(R4)                                                      
         GOTO1 ELEM40,(R4)                                                      
         GOTO1 ELEM4C,(R4)                                                      
*&&UK*&& GOTO1 ELEM50,(R4)                                                      
*&&UK*&& GOTO1 ELEM50R,(R4)                                                     
         GOTO1 ELEM60,(R4)                                                      
         GOTO1 ELEM65,(R4)                                                      
*&&UK*&& GOTO1 ELEMC0,(R4)                                                      
         GOTO1 ELEMD6,(R4)                                                      
         GOTO1 ELEMD8,(R4)                                                      
         GOTO1 GOADDTRN,BC1RNAME   CALL ADDTRN                                  
         NI    POSTSTAT,X'FF'-POSTSJ                                            
*                                                                               
*              *** CR SI/SJ ***                                                 
*                                                                               
GOTRN40  DS    0H                                                               
         CLI   TMSTTYP,TMSTCB      EXIT IF NOT B-TIME                           
         BNE   EXIT                                                             
         BAS   RE,XCIO                                                          
         OI    POSTSTAT,POSTSI     SI POSTING                                   
*                                                                               
         USING TRNRECD,R2                                                       
         LA    R2,TRNIO                                                         
         MVC   TRNKCPY,TTACKCPY                                                 
         MVC   TRNKULA,BCSIACCT    INCOME ACCOUNT                               
         MVC   TRNKOFF,TMSOFFC     CLIENT OFFICE                                
         TM    TTCPYST4,CPYSOFF2                                                
         BO    *+10                                                             
         MVC   TRNKOFF,SPACES      OLD OFFICES = NO OFFICE IN KEY               
         MVC   TRNKCCPY,TTACKCPY                                                
         MVC   TRNKULC,BCSJACCT    SJ ACCOUNT                                   
         MVC   TRNKDATE,TTYYMMDD                                                
         MVC   TRNKREF,BCREF                                                    
         GOTO1 ELEM44,(R4)                                                      
*&&US*&& GOTO1 ELEM1A,(R4)         MEDIA INTERFACE ELEMENT ('7B' IN UK)         
*&&UK*&& GOTO1 ELEM50G,(R4)                                                     
         GOTO1 ELEM60,(R4)                                                      
*&&UK*&& GOTO1 ELEM7B,(R4)         SOURCE ELEMENT - ('1A' IN US)                
         GOTO1 ELEMC0,(R4)                                                      
         GOTO1 ELEMC5,(R4)                                                      
         GOTO1 ELEMD6,(R4)                                                      
         GOTO1 ELEMD8,(R4)                                                      
         GOTO1 GOADDTRN,BCSJNAME                                                
         NI    POSTSTAT,X'FF'-POSTSI                                            
         CLC   BCSIACCT(2),=C'SI'  ONLY MAKE COSTING POSTINGS IF                
         BNE   GOTRN70             POSTING TO SI - SKIP FOR U/L=SK              
*                                                                               
*              *** DR 1C/12 ***                                                 
*                                                                               
GOTRN50  DS    0H                                                               
         BAS   RE,XCIO                                                          
         OI    POSTSTAT,POST1C                                                  
*                                                                               
         USING TRNRECD,R2                                                       
         LA    R2,TRNIO                                                         
         MVC   TRNKCPY,TTACKCPY                                                 
         MVC   TRNKULA,BCCNTRA     COSTING ACCOUNT                              
         MVC   TRNKOFF,TMSOFFC     CLIENT OFFICE                                
         TM    TTCPYST4,CPYSOFF2                                                
         BO    *+10                                                             
         MVC   TRNKOFF,SPACES      OLD OFFICES = NO OFFICE IN KEY               
         MVC   TRNKCCPY,TTACKCPY                                                
         MVC   TRNKULC,BC12ACCT    ANALYSIS ACCOUNT                             
         MVC   TRNKDATE,TTYYMMDD                                                
         MVC   TRNKREF,BCREF                                                    
         GOTO1 ELEM44,(R4)                                                      
         GOTO1 ELEM60,(R4)                                                      
         GOTO1 ELEMD6,(R4)                                                      
         GOTO1 ELEMD8,(R4)                                                      
         GOTO1 GOADDTRN,BC12NAME                                                
         NI    POSTSTAT,X'FF'-POST1C                                            
*                                                                               
*              *** CR 12/1C ***                                                 
*                                                                               
GOTRN60  DS    0H                                                               
         BAS   RE,XCIO                                                          
         OI    POSTSTAT,POST12     12 POSTING                                   
*                                                                               
         USING TRNRECD,R2                                                       
         LA    R2,TRNIO                                                         
         MVC   TRNKCPY,TTACKCPY                                                 
         MVC   TRNKULA,BC12ACCT    ANALYSIS ACCOUNT                             
         MVC   TRNKOFF,TMSOFFC     CLIENT OFFICE                                
         TM    TTCPYST4,CPYSOFF2                                                
         BO    *+10                                                             
         MVC   TRNKOFF,SPACES      OLD OFFICES = NO OFFICE IN KEY               
         MVC   TRNKCCPY,TTACKCPY                                                
         MVC   TRNKULC,BCCNTRA     COSTING ACCOUNT                              
         MVC   TRNKDATE,TTYYMMDD                                                
         MVC   TRNKREF,BCREF                                                    
         GOTO1 ELEM44,(R4)                                                      
         GOTO1 ELEM60,(R4)                                                      
         GOTO1 ELEMD6,(R4)                                                      
         GOTO1 ELEMD8,(R4)                                                      
         GOTO1 GOADDTRN,BCCNTRNM                                                
         NI    POSTSTAT,X'FF'-POST12                                            
*                                                                               
*              *** TAX POSTINGS ***                                             
*                                                                               
GOTRN70  DS    0H                                                               
         USING TIMELD,R5                                                        
         SR    R5,R5                                                            
         ICM   R5,15,TMSATAX       ANY TAX POSTINGS TO MAKE                     
         BZ    GOTRNX                                                           
*                                                                               
         MVC   BCTXWC,TIMTWC      TAX WORKCODE                                  
         MVC   BCTXLOC,TIMTLOC    LOCALITY CODE                                 
         ZAP   BCTXBAS,TIMTBAS    TAX BASIS AMOUNT                              
         SR    R3,R3                                                            
         IC    R3,TIMTMINI        # TAX MINI ELEMENTS                           
*                                                                               
GOTRN75  MVC   BCTXEFF,TIMTEFF     RATE EFFECTIVE DATE                          
         ZAP   BCTXRATE,TIMTRATE   TAX RATE                                     
         ZAP   BCDUB,BCTXBAS       CALCULATE POSTING AMOUNT                     
         MP    BCDUB,BCTXRATE                                                   
         SRP   BCDUB,64-6,5                                                     
         ZAP   BCTXAMNT,BCDUB                                                   
*                                                                               
*              *** DR SJ/CREDIT ACCOUNT ***                                     
*                                                                               
         MVC   BCACCODE,TIMTACC    CREDIT ACCOUNT                               
         GOTO1 GETACT,BCACCODE                                                  
         OI    POSTSTAT,POSTTXSJ   SJ TAX POSTING                               
         BAS   RE,XCIO                                                          
         USING TRNRECD,R2                                                       
         LA    R2,TRNIO                                                         
         MVC   TRNKCPY,TTACKCPY                                                 
         MVC   TRNKULA,BCSJACCT    SJ ACCOUNT                                   
         MVC   TRNKWORK,TMSTSK     TSK CODE                                     
         MVC   TRNKCCPY,TTACKCPY                                                
         MVC   TRNKULC,BCACCODE    CREDIT ACCCOUNT                              
         MVC   TRNKDATE,TTYYMMDD                                                
         MVC   TRNKREF,BCREF                                                    
         GOTO1 ELEM44,(R4)                                                      
         GOTO1 ELEM5F,(R4)                                                      
         GOTO1 ELEM60,(R4)                                                      
         GOTO1 GOADDTRN,BCACNAME   CALL ADDTRN                                  
         NI    POSTSTAT,X'FF'-POSTTXSJ                                          
*                                                                               
         BAS   RE,XCIO                                                          
         OI    POSTSTAT,POSTTXCR   CREDIT POSTING                               
         USING TRNRECD,R2                                                       
         LA    R2,TRNIO                                                         
         MVC   TRNKCPY,TTACKCPY                                                 
         MVC   TRNKULA,BCACCODE    CREDIT ACCOUNT                               
         MVC   TRNKOFF,TMSOFFC     CLIENT OFFICE                                
         TM    TTCPYST4,CPYSOFF2                                                
         BO    *+10                                                             
         MVC   TRNKOFF,SPACES      OLD OFFICES = NO OFFICE IN KEY               
         MVC   TRNKCCPY,TTACKCPY                                                
         MVC   TRNKULC,SPACES                                                   
         MVC   TRNKULC(5),BCSJACCT    SJ CLIENT CODE                            
         MVC   TRNKDATE,TTYYMMDD                                                
         MVC   TRNKREF,BCREF                                                    
         GOTO1 ELEM44,(R4)                                                      
         GOTO1 ELEM23,(R4)                                                      
         GOTO1 ELEM5F,(R4)                                                      
         GOTO1 ELEM60,(R4)                                                      
         GOTO1 GOADDTRN,BCACNAME   CALL ADDTRN                                  
         NI    POSTSTAT,X'FF'-POSTTXCR                                          
         LA    R5,TIMTMINQ(R5)                                                  
         BCT   R3,GOTRN75                                                       
*                                                                               
GOTRNX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* X'1A' MEDIA TRANSFER ELEMENT - US ONLY                              *         
***********************************************************************         
         SPACE 1                                                                
ELEM1A   NTR1                                                                   
         USING TMSD,R4             R4=A(CURRENT TABLE ENTRY)                    
         LR    R4,R1                                                            
         CLC   BCSIACCT(2),=C'SI'  ONLY ADD TO SI POSTING                       
         BNE   EXIT                DONT ADD TO SK POSTING                       
*                                                                               
         USING MDTELD,R2                                                        
         LA    R2,ELEM             MEDIA TRANSFER ELEMENT IN US                 
         XC    ELEM,ELEM                                                        
         MVI   MDTEL,MDTELQ        X'1A' ELEMENT                                
         MVI   MDTLN,MDTLNQ                                                     
         MVI   MDTSYS,MDTSPROD     C'J' - PRODUCTION                            
         MVC   MDTMED,BCSJMED      MEDIA CODE                                   
         MVC   MDTCLI(12),BCSJCLI  SJ CLI/PRD/JOB                               
         MVC   MDTMOS,TMSMOA       MOA                                          
         MVC   MDTDSCP,BCSJNAME    SJ ACCOUNT NAME                              
         ZAP   BCDUB,BCAMOUNT                                                   
         CVB   R0,BCDUB                                                         
         STCM  R0,15,MDTCOM        INCOME (COMMISSION)                          
         STCM  R0,15,MDTINTL       INCOME (INTERNAL)                            
         BAS   RE,ADDELEM                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* X'23' OTHERS ELEMENT                                                *         
***********************************************************************         
         SPACE 1                                                                
ELEM23   NTR1                                                                   
         USING TMSD,R4             R4=A(CURRENT TABLE ENTRY)                    
         LR    R4,R1                                                            
*                                                                               
         USING OTHELD,R2                                                        
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   OTHEL,OTHELQ        X'23'                                        
         MVI   OTHLN,OTHLN1Q                                                    
         MVC   OTHNUM(13),SPACES                                                
         MVC   OTHNUM(3),BCSJACCT+5    PRODUCT CODE                             
         MVC   OTHNUM+6(6),BCSJACCT+8  JOB CODE                                 
         BAS   RE,ADDELEM                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* X'2C' SPECIAL POSTING ELEMENT                                       *         
***********************************************************************         
         SPACE 1                                                                
ELEM2C   NTR1                                                                   
         USING TMSD,R4             R4=A(CURRENT TABLE ENTRY)                    
         LR    R4,R1                                                            
*                                                                               
         USING SPAELD,R2                                                        
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   SPAEL,SPAELQ        X'2C'                                        
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATCCST    COSTING ACCOUNT                              
         MVC   SPAAULA,TMSCNTRA                                                 
         BAS   RE,ADDELEM                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* X'40' PERSONNEL RATE ELEMENT                                        *         
***********************************************************************         
         SPACE 1                                                                
ELEM40   NTR1                                                                   
         USING TMSD,R4             R4=A(CURRENT TABLE ENTRY)                    
         LR    R4,R1                                                            
*                                                                               
         USING PRTELD,R2                                                        
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   PRTEL,PRTELQ        X'40'                                        
         MVI   PRTLN,PRTLNQ                                                     
         MVC   PRTSTRT,TMSREFF     EFFECTIVE DATE FOR RATE                      
         ZAP   PRTRATE,TMSRATE     RATE                                         
         ZAP   PRTHOUR,TMSHRS      HOURS                                        
*&&US*&& MVC   PRTLINE#,TMSLINE#   TMS LINE #                                   
         TM    STATUS,STMINUS      MAKING NEGATIVE POSTING                      
         BNO   ELEM40A                                                          
         ZAP   BCDUB,PRTHOUR                                                    
         MP    BCDUB,=P'-1'                                                     
         ZAP   PRTHOUR,BCDUB                                                    
*                                                                               
ELEM40A  DS    0H                                                               
*&&US                                                                           
         MVI   PRTSTAT,PRTSNOTQ    N-TIME                                       
         CLI   TMSTTYP,TMSTCR      R-TIME                                       
         BNE   *+8                                                              
         MVI   PRTSTAT,PRTSRTEQ                                                 
         CLI   TMSTTYP,TMSTCB      B-TIME                                       
         BNE   *+8                                                              
         MVI   PRTSTAT,PRTSBILQ                                                 
         TM    TMSRSTA,TMSRADJ                                                  
         BNO   *+8                                                              
         OI    PRTSTAT,PRTSADJ     RATE WAS ADJUSTED                            
*&&                                                                             
         BAS   RE,ADDELEM                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* X'44' TRANSACTION ELEMENT                                           *         
***********************************************************************         
         SPACE 1                                                                
ELEM44   NTR1                                                                   
         USING TMSD,R4             R4=A(CURRENT TABLE ENTRY)                    
         LR    R4,R1                                                            
*                                                                               
         USING TRNELD,R2                                                        
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TRNEL,TRNELQ        X'44'                                        
         MVC   TRNDATE,TTYYMMDD    TRANSACTION DATE                             
         MVC   TRNREF,BCREF                                                     
         MVI   TRNTYPE,49          TYPE 49                                      
*                                                                               
         MVC   TRNMOS,TMSMOA                                                    
         MVC   TRNMOS,TMSMOA       CONVERT YYMM -> YM EDCDIC                    
         OI    TRNMOS,X'F0'        EX/  X'9402' -> C'42'                        
         SR    R1,R1                                                            
         IC    R1,TRNMOS+1                                                      
         LA    RF,X'F0'                                                         
         TM    TRNMOS+1,X'10'                                                   
         BNO   *+8                                                              
         LA    RF,X'B1'                                                         
         AR    R1,RF                                                            
         STC   R1,TRNMOS+1                                                      
*                                                                               
         SR    R1,R1               PERIOD NUMBER INTO LAST 4 CHARS              
         IC    R1,TTPERIOD         OF BATCH REFERENCE                           
         CVD   R1,BCDUB                                                         
         OI    BCDUB+7,X'0F'                                                    
         UNPK  TRNBREF(4),BCDUB+5(3)                                            
*                                                                               
         TM    POSTSTAT,POSTSJ              SJ POSTING                          
         BNO   *+8                                                              
         OI    TRNSTAT,TRNSAUTH             ONLY AUTHORIZED USERS               
         ZAP   TRNAMNT,=P'0'                                                    
         TM    POSTSTAT,POSTTXSJ+POSTTXCR   ALL TIME IS NON COMMISSION          
         BO    *+8                          EXCEPT TAX ITEMS                    
         OI    TRNSTAT,TRNSNOCM                                                 
         TM    POSTSTAT,POST1R+POSTSJ+POST1C+POSTTXSJ                           
         BZ    *+8                                                              
         OI    TRNSTAT,TRNSDR      DEBIT POSTING FOR 1R/SJ/1C                   
         TM    POSTSTAT,POST1R                                                  
         BO    ELEM44A                                                          
         CLI   TMSTTYP,TMSTCB      ONLY PUT AMOUNT IN BILLABLE POSTINGS         
         BNE   ELEM44A                                                          
         ZAP   TRNAMNT,BCAMOUNT    AMOUNT                                       
         TM    POSTSTAT,POSTTXSJ+POSTTXCR                                       
         BZ    *+10                                                             
         ZAP   TRNAMNT,BCTXAMNT                                                 
         TM    STATUS,STMINUS      MAKING NEGATIVE POSTING                      
         BNO   ELEM44A                                                          
         ZAP   BCDUB,TRNAMNT                                                    
         MP    BCDUB,=P'-1'                                                     
         ZAP   TRNAMNT,BCDUB                                                    
*                                                                               
ELEM44A  MVC   TRNOFFC,TMSOFFC      EITHER OFFICE OR TASK                       
         TM    POSTSTAT,POSTSJ                                                  
         BNO   *+10                                                             
         MVC   TRNOFFC,TMSTSK                                                   
         SR    R1,R1                                                            
         ICM   R1,1,BCLNARR                                                     
         BZ    ELEM44B                                                          
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   TRNNARR(0),BCNARR   COPY NARRATIVE INTO X'44' ELEM               
         LA    R1,1(R1)                                                         
*                                                                               
ELEM44B  LA    RF,TRNLN1Q          CALCULATE ELEMENT LENGTH                     
         LA    RF,0(R1,RF)                                                      
         STC   RF,TRNLN                                                         
*                                                                               
         TM    TRNSTAT,TRNSDR                                                   
         BNO   *+14                                                             
         AP    TOTDR,TRNAMNT                                                    
         B     *+10                                                             
         AP    TOTCR,TRNAMNT                                                    
         BAS   RE,ADDELEM                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* X'4C' SUBSIDIARY POSTING ELEMENT                                    *         
***********************************************************************         
         SPACE 1                                                                
ELEM4C   NTR1                                                                   
         USING TMSD,R4             R4=A(CURRENT TABLE ENTRY)                    
         LR    R4,R1                                                            
         CLC   BCSIACCT,SPACES     DONT ADD MEMO ELEMENT IF NO INC ACCT         
         BNH   EXIT                                                             
*                                                                               
         USING SPDELD,R2                                                        
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   SPDEL,SPDELQ        X'4C'                                        
         MVC   SPDACCS,BCSIACCT    INCOME ACCOUNT                               
         LA    R1,SPDLN1Q                                                       
         LA    R1,L'BCSIACCT(R1)                                                
         STC   R1,SPDLN                                                         
         BAS   RE,ADDELEM                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* X'50' SUBSIDIARY CASH ELEMENT (HOURS)                               *         
***********************************************************************         
         SPACE 1                                                                
ELEM50   NTR1                                                                   
         USING TMSD,R4             R4=A(CURRENT TABLE ENTRY)                    
         LR    R4,R1                                                            
*                                                                               
         USING SCIELD,R2                                                        
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   SCIEL,SCIELQ        X'50'                                        
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITHOUR    HOURS                                        
*&&UK                                                                           
         TM    POSTSTAT,POSTSJ                                                  
         BZ    ELEM50A                                                          
         MVI   SCITYPE,SCITSJHR                                                 
         ZAP   SCIAMNT,TMSHRS      HOURS                                        
         B     ELEM50B                                                          
*&&                                                                             
ELEM50A  CLI   TMSTTYP,TMSTNC      NON CLIENT 1N TIME                           
         BNE   *+18                DONT GENERATE HOUR BUCKETS FOR               
         CLC   TMSCNTRA+2(2),=C'L '       LEAVE OF ABSENCE                      
         BNE   *+8                                                              
         MVI   SCITYPE,SCITLOAT    C'X' - LOA BUCKET TYPE                       
         ZAP   SCIAMNT,TMSHRS                                                   
*                                                                               
ELEM50B  TM    STATUS,STMINUS                                                   
         BNO   ELEM50C                                                          
         ZAP   BCDUB,SCIAMNT                                                    
         MP    BCDUB,=P'-1'                                                     
         ZAP   SCIAMNT,BCDUB                                                    
*                                                                               
ELEM50C  BAS   RE,ADDELEM                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* X'50' SUBSIDIARY CASH ELEMENT (SALES OR COST RATE MEMO)             *         
***********************************************************************         
         SPACE 1                                                                
ELEM50R  NTR1                                                                   
         USING TMSD,R4             R4=A(CURRENT TABLE ENTRY)                    
         LR    R4,R1                                                            
*                                                                               
         USING SCIELD,R2                                                        
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   SCIEL,SCIELQ        X'50'                                        
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITCRAT    COST RATE - TYPE 'C'                         
         ZAP   BCDUB,TMSRATE                                                    
*                                                                               
         LA    RE,TTPROF1          PROF1 FOR B - TIME                           
         CLI   TIMTTYP,TMSTCB      ARE WE DOING B - TIME                        
         BE    *+8                                                              
         LA    RE,TTPROF2          PROF2 FOR N AND R TIME                       
         TM    0(RE),TTPR1CST      CARRY OPPOSITE INFO ON JOB POSTING           
         BO    *+10                                                             
         ZAP   BCDUB,TMSCRATE                                                   
         MP    BCDUB,TMSHRS                                                     
         SRP   BCDUB,64-2,5                                                     
         ZAP   SCIAMNT,BCDUB                                                    
*                                                                               
         TM    STATUS,STMINUS      REVERSE AMOUNT IF BACKING OUT                
         BNO   ELEM50RX                                                         
         ZAP   BCDUB,SCIAMNT                                                    
         MP    BCDUB,=P'-1'                                                     
         ZAP   SCIAMNT,BCDUB                                                    
*                                                                               
ELEM50RX BAS   RE,ADDELEM                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* X'50' SUBSIDIARY CASH ELEMENT (GROSS DOLLARS BUCKET)                *         
***********************************************************************         
         SPACE 1                                                                
*&&UK                                                                           
ELEM50G  NTR1                                                                   
         USING TMSD,R4             R4=A(CURRENT TABLE ENTRY)                    
         LR    R4,R1                                                            
*                                                                               
         USING SCIELD,R2                                                        
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   SCIEL,SCIELQ        X'50'                                        
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGRSS    GROSS AMOUNT                                 
         ZAP   SCIAMNT,=P'0'                                                    
         BAS   RE,ADDELEM                                                       
         B     EXIT                                                             
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* X'5F' SALES TAX ELEMENT                                             *         
***********************************************************************         
         SPACE 1                                                                
ELEM5F   NTR1                                                                   
         USING TMSD,R4             R4=A(CURRENT TABLE ENTRY)                    
         LR    R4,R1                                                            
*                                                                               
         USING SUTELD,R2                                                        
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   SUTEL,SUTELQ        X'5F'                                        
         MVI   SUTLN,SUTLN2Q                                                    
         MVC   SUTEFF,BCTXEFF      EFFECTIVE DATE                               
         ZAP   SUTRTE,BCTXRATE     TAX RATE                                     
         ZAP   SUTBAS,BCTXBAS      BASIS AMOUNT                                 
         MVC   SUTLOC,SPACES                                                    
         MVC   SUTLOC(L'BCTXLOC),BCTXLOC     LOCALITY CODE                      
         BAS   RE,ADDELEM                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* X'60' TRANSACTION STATUS ELEMENT                                    *         
***********************************************************************         
         SPACE 1                                                                
ELEM60   NTR1                                                                   
         USING TMSD,R4             R4=A(CURRENT TABLE ENTRY)                    
         LR    R4,R1                                                            
*                                                                               
         USING TRSELD,R2                                                        
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TRSEL,TRSELQ        X'60'                                        
         MVI   TRSLN,TRSLNQ                                                     
         L     RF,TTCFACS          COMPRESSED ACTIVITY DATE                     
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),BCDMCB,(5,0),(2,TRSDATE)                                    
         MVC   TRSEFDT,TRSDATE     EFFECTIVE DATE                               
         MVC   TRSPMOS,TMSMOA                                                   
         MVC   TRSUSER,TTUSERID    USER ID                                      
         TM    TMSIND,TMSIADJ      TIMESHEET ADJUSTED                           
         BNO   *+8                                                              
         OI    TRSSTAT2,TRSSTADJ                                                
         TM    TRSSTAT2,TRSSTMSS+TRSSTADJ                                       
         BNZ   *+8                                                              
         OI    TRSSTAT2,TRSSTIME   REGULAR                                      
         BAS   RE,ADDELEM                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* X'65' ANALYZED OFFICE ELEMENT                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TMSD,R4             R4=A(CURRENT TABLE ENTRY)                    
ELEM65   NTR1                                                                   
         LR    R4,R1                                                            
*                                                                               
         CLI   TMSOFFC+1,C' '      ONE BYTE OR TWO BYTE OFFICE CODE             
         BE    ELEM65X             ONLY ADDING ELEM FOR TWO BYTE OFF            
*                                                                               
         USING ANOELD,R2                                                        
         LA    R2,ELEM                                                          
         MVI   ANOEL,ANOELQ        X'65' - OFFICE ELEMENT                       
         MVI   ANOLN,ANOLNQ                                                     
         MVI   ANOTYPE,ANOTCLI     C'C' - CLIENT OFFICE                         
         MVC   ANOOFFC,TMSOFFC     CLIENT OFFICE CODE                           
         BAS   RE,ADDELEM                                                       
ELEM65X  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* X'7B' SOURCE ELEMENT - UK ONLY                                      *         
***********************************************************************         
         SPACE 1                                                                
ELEM7B   NTR1                                                                   
         USING TMSD,R4             R4=A(CURRENT TABLE ENTRY)                    
         LR    R4,R1                                                            
         CLC   BCSIACCT(2),=C'SI'  ONLY ADD TO SI POSTING                       
         BNE   EXIT                DONT ADD TO SK POSTING                       
*                                                                               
         USING SORELD,R2                                                        
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   SOREL,SORELQ        X'7B' - SOURCE ELEMENT                       
         MVI   SORLN,SORALNQ                                                    
         MVI   SORSYS,SORSACC      C'A' - ACCOUNT SYSTEM                        
         MVC   SORAULA,BCSJACCT    SJ U/L/CLI/PRD/JOB                           
         BAS   RE,ADDELEM                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* X'C0' ANALYSIS POINTER ELEMENT                                      *         
***********************************************************************         
         SPACE 1                                                                
ELEMC0   NTR1                                                                   
         USING TMSD,R4             R4=A(CURRENT TABLE ENTRY)                    
         LR    R4,R1                                                            
*                                                                               
         USING APEELD,R2                                                        
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   APEEL,APEELQ        X'C0'                                        
*                                                                               
         MVI   APENUM,2            # MINI ELEMENTS                              
         OI    APENSTAT,APENSDR                                                 
         LA    R1,BC1RACCT+L'BC1RACCT-1                                         
         CLI   0(R1),X'40'                                                      
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    RE,BC1RACCT                                                      
         SR    R1,RE                                                            
         EX    R1,*+4                                                           
         MVC   APENACT(0),BC1RACCT 1R ACCOUNT - DR                              
         LA    R1,APELN2Q+1(R1)                                                 
         STC   R1,APENLEN                                                       
         LA    R2,0(R1,R2)                                                      
*                                                                               
         OI    APENSTAT,APENSDR                                                 
         LA    R1,BCCNTRA+L'BCCNTRA-1                                           
         CLI   0(R1),X'40'                                                      
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    RE,BCCNTRA                                                       
         SR    R1,RE                                                            
         EX    R1,*+4                                                           
         MVC   APENACT(0),BCCNTRA  1C ACCOUNT - DR                              
         LA    R1,APELN2Q+1(R1)                                                 
         STC   R1,APENLEN                                                       
         LA    R2,0(R1,R2)                                                      
*                                                                               
         CLC   BC12ACCT,SPACES                                                  
         BNH   ELEMC0A                                                          
         LA    RF,ELEM                                                          
         SR    R1,R1                                                            
         IC    R1,APENUM-APEELD(RF)                                             
         LA    R1,1(R1)                                                         
         STC   R1,APENUM-APEELD(RF)                                             
         LA    R1,BC12ACCT+L'BC12ACCT-1                                         
         CLI   0(R1),X'40'                                                      
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    RE,BC12ACCT                                                      
         SR    R1,RE                                                            
         EX    R1,*+4                                                           
         MVC   APENACT(0),BC12ACCT 12 ACCOUNT - CR                              
         LA    R1,APELN2Q+1(R1)                                                 
         STC   R1,APENLEN                                                       
         LA    R2,0(R1,R2)                                                      
*                                                                               
ELEMC0A  TM    POSTSTAT,POSTSJ     CARRY INCOME ACCOUNT ON SJ POSTINGS          
         BZ    ELEMC0B             BUT ONLY IN EUROPE                           
         CLC   BCSIACCT,SPACES                                                  
         BNH   ELEMC0B                                                          
         LA    RF,ELEM                                                          
         SR    R1,R1                                                            
         IC    R1,APENUM-APEELD(RF)                                             
         LA    R1,1(R1)                                                         
         STC   R1,APENUM-APEELD(RF)                                             
         LA    R1,BCSIACCT+L'BCSIACCT-1                                         
         CLI   0(R1),X'40'                                                      
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    RE,BCSIACCT                                                      
         SR    R1,RE                                                            
         EX    R1,*+4                                                           
         MVC   APENACT(0),BCSIACCT SI ACCOUNT - CR                              
         LA    R1,APELN2Q+1(R1)                                                 
         STC   R1,APENLEN                                                       
         LA    R2,0(R1,R2)                                                      
*                                                                               
ELEMC0B  LA    R2,APELN1Q(R2)                                                   
         LA    RF,ELEM                                                          
         SR    R2,RF                                                            
         STC   R2,APELN-APEELD(RF) STORE ELEMENT LENGTH                         
         BAS   RE,ADDELEM                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* X'C5' FREEFORM DATA ELEMENT TO CARRY WORKCODE                       *         
***********************************************************************         
         SPACE 1                                                                
ELEMC5   NTR1                                                                   
         USING TMSD,R4             R4=A(CURRENT TABLE ENTRY)                    
         LR    R4,R1                                                            
*                                                                               
         USING RFLELD,R2                                                        
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   RFLEL,RFLELQ        X'C5'                                        
         MVI   RFLLN,RFLLNQ+L'TMSTSK                                            
         MVI   RFLTYPE,RFLWC                                                    
         MVC   RFLDATA(L'TMSTSK),TMSTSK                                         
         BAS   RE,ADDELEM                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* X'D6' TERMINAL ID ELEMENT                                           *         
***********************************************************************         
         SPACE 1                                                                
ELEMD6   NTR1                                                                   
         OC    TTTID,TTTID                                                      
         BZ    EXIT                                                             
*                                                                               
         USING TIDELD,R2                                                        
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TIDEL,TIDELQ        X'D6'                                        
         MVI   TIDLN,TIDLNQ                                                     
         MVC   TID,TTTID                                                        
         BAS   RE,ADDELEM                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* X'D8' PERSON ID ELEMENT                                             *         
***********************************************************************         
         SPACE 1                                                                
ELEMD8   NTR1                                                                   
         OC    TTSECPID,TTSECPID                                                
         BZ    EXIT                                                             
*                                                                               
         USING PIDELD,R2                                                        
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   PIDEL,PIDELQ        X'D8'                                        
         MVI   PIDLN,PIDLNQ                                                     
*        MVC   PIDNO,TTPIDNO       PERSONAL PID (PERSON TS BELONGS TO)          
         MVC   PIDNO,TTSECPID      USER'S PID # (PERSON WHO ENTERED TS)         
         BAS   RE,ADDELEM                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE POSTING ACCOUNTS                                           *         
***********************************************************************         
         SPACE 1                                                                
GETACT   NTR1                                                                   
         BAS   RE,XCIO                                                          
         MVC   BCACNAME,SPACES     ACCOUNT NAME                                 
         MVC   BCACANAL,SPACES     12 ANALYSIS ACCOUNT                          
         MVC   BCACCOST,SPACES     COSTING BYTE                                 
*                                                                               
         USING ACTRECD,R2                                                       
         LA    R2,KEY2                                                          
         MVC   ACTKEY(L'KEY2),SPACES                                            
         MVC   ACTKCPY,TTACKCPY                                                 
         MVC   ACTKULA,0(R1)                                                    
         GOTO1 TTDMGR,BCDMCB,(0,DMREAD),=A(ACCDIR),KEY2,KEY2,0                  
         CLI   BCDMCB+8,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,ACTKDA                                                        
         GOTO1 TTDMGR,BCDMCB,=A(GETREC),=A(ACCMST),(R3),TRNIO,BCWORK            
         CLI   BCDMCB+8,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,TRNIO                                                         
         LA    R2,ACTRFST                                                       
GETA10   CLI   0(R2),0                                                          
         BE    EXIT                                                             
*                                                                               
         USING NAMELD,R2           *** ACCOUNT NAME ***                         
GETA20   CLI   0(R2),NAMELQ        X'20'                                        
         BNE   GETA30                                                           
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         BM    GETA50                                                           
         EX    R1,*+4                                                           
         MVC   BCACNAME(0),NAMEREC                                              
*                                                                               
         USING SPAELD,R2           *** ANALYSIS ACCOUNT ***                     
GETA30   CLI   0(R2),SPAELQ        X'2C'                                        
         BNE   GETA40                                                           
         CLI   SPATYPE,SPATANAL                                                 
         BNE   *+10                                                             
         MVC   BCACANAL,SPAAANAL                                                
*                                                                               
         USING RSTELD,R2           *** COSTING BYTE ***                         
GETA40   CLI   0(R2),RSTELQ        X'30'                                        
         BNE   *+10                                                             
         MVC   BCACCOST,RSTCOSTG                                                
*                                                                               
GETA50   SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     GETA10                                                           
         EJECT                                                                  
***********************************************************************         
* ADDTRNS INTERFACE                                                   *         
***********************************************************************         
         SPACE 1                                                                
GOADDTRN NTR1                                                                   
         MVC   BCACNAME,0(R1)      R1=A(CONTRA ACCOUNT NAME)                    
*                                                                               
         LA    RE,TRNBLK           CLEAR ADDTRN BLOCK                           
         LA    RF,TRNBLKL                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         OI    TRNINDS,TRNICONV    NEW FILE FORMAT                              
         TM    STATUS,STBUCKET     ONLY UPDATING BUCKETS?                       
         BNO   *+8                                                              
         OI    TRNINDS2,TRNIUBKO+TRNIUOFC                                       
         MVC   TRNCOMF,TTCFACS     A(COMFACS)                                   
         LA    R1,TRNIO                                                         
         ST    R1,TRNREC           A(TRANS RECORD)                              
         MVC   TRNBMOS,BCMOA       MOS                                          
         MVC   TRNBSEQN,=X'0000'                                                
         MVC   TRNPUSER,TTUSERID                                                
         MVC   TRNCACNM,BCACNAME                                                
         MVC   TRNCPYS1,SVCPYST1   COMPANY STATUS BYTE #1                       
         MVC   TRNCPYS2,SVCPYST2   COMPANY STATUS BYTE #2                       
         MVC   TRNCPYS3,SVCPYST3   COMPANY STATUS BYTE #3                       
         MVC   TRNCPYS4,SVCPYST4   COMPANY STATUS BYTE #4                       
         MVC   TRNCPYS5,SVCPYST5   COMPANY STATUS BYTE #5                       
         MVC   TRNCPYS6,SVCPYST6   COMPANY STATUS BYTE #6                       
         MVC   TRNCPYS7,SVCPYST7   COMPANY STATUS BYTE #7                       
         MVC   TRNCPYS8,SVCPYST8   COMPANY STATUS BYTE #8                       
         MVC   TRNCPYS9,SVCPYST9   COMPANY STATUS BYTE #9                       
         MVC   TRNCPYSA,SVCPYSTA   COMPANY STATUS BYTE #A                       
                                                                                
         LA    R0,PALAREA                                                       
         STCM  R0,15,TRNPAL                                                     
                                                                                
         L     RF,TTCFACS          COMPRESSED EFFECTIVE DATE                    
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),BCDMCB,(1,TTYYMMDD),(2,TRNEFDT)                             
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BNE   *+8                                                              
         OI    TRNINDS,TRNIWRNO                                                 
*                                                                               
         MVC   P(20),=CL20'ADD TRANSACTION'                                     
         GOTO1 ACREPORT                                                         
         TM    STATUS,STBUCKET     ONLY UPDATING BUCKETS?                       
         BNO   GOA10                                                            
         MVC   P(20),=CL20'BUCKET UPDATE ONLY'                                  
         GOTO1 ACREPORT                                                         
GOA10    L     R3,TRNREC                                                        
         USING TRNRECD,R3                                                       
         SR    R0,R0                                                            
         ICM   R0,3,TRNRLEN                                                     
         GOTO1 VPRNTBL,DMCB,0,(R3),C'DUMP',(R0),=C'1D'                          
         GOTO1 ACREPORT                                                         
         DROP  R3                                                               
*                                                                               
         GOTO1 TTADDTRN,TRNBLK                                                  
         CLI   TRNERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 TTDMGR,BCDMCB,=C'DMUNLK',=C'ACCDIR'                              
         GOTO1 TTDMGR,BCDMCB,=C'DMUNLK',=C'ACCMST'                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD ELEMENT IN ELEM TO RECORD IN TRNIO                              *         
***********************************************************************         
         SPACE 1                                                                
ADDELEM  NTR1                                                                   
         L     RF,TTCFACS                                                       
         L     RF,CHELLO-COMFACSD(RF)                                           
         GOTO1 (RF),BCDMCB,(C'P',=C'ACCMST  '),TRNIO,ELEM,             X        
               (0,=C'ADD=END')                                                  
         CLI   BCDMCB+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CLEAR TRANSACTION RECORD AREA                                       *         
***********************************************************************         
         SPACE 1                                                                
XCIO     NTR1                                                                   
         LA    R0,TRNIO                                                         
         LH    R1,=Y(L'TRNIO)                                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
         EJECT                                                                  
ACXAD    DSECT                                                                  
DA1      DS    XL4                                                              
AIO      DS    A                                                                
VPRNTBL  DS    A                                                                
TABCNT   DS    H                                                                
LASTCPY  DS    XL1                                                              
KEY2     DS    CL60                                                             
SVKEY    DS    CL60                                                             
TODAY2   DS    CL2                                                              
START3   DS    CL3                                                              
END3     DS    CL3                                                              
START2   DS    CL2                                                              
END2     DS    CL2                                                              
ACTIVITY DS    CL1                                                              
TEMPTIME DS    2F                                                               
USER     DS    XL2                                                              
TRNUM    DS    XL2                                                              
BCBUFLNQ EQU   2000                                                             
*                                                                               
SVCPYSTS DS    0C                                                               
SVCPYST1 DS    CL1                                                              
SVCPYST2 DS    CL1                                                              
SVCPYST3 DS    CL1                                                              
SVCPYST4 DS    CL1                                                              
SVCPYST5 DS    CL1                                                              
SVCPYST6 DS    CL1                                                              
SVCPYST7 DS    CL1                                                              
SVCPYST8 DS    CL1                                                              
SVCPYST9 DS    CL1                                                              
SVCPYSTA DS    CL1                                                              
SVCPYLNQ EQU   *-SVCPYSTS                                                       
*                                                                               
PALAREA  DS    XL20                P&L BUCKET AREA                              
                                                                                
TOTCR    DS    PL8                                                              
TOTDR    DS    PL8                                                              
HOURS    DS    PL8                                                              
RECADD   DS    PL4                                                              
RECCHA   DS    PL4                                                              
*                                                                               
BC1RLNQS DS    0XL4                                                             
BC1RLNQ1 DS    XL1                 L'LEVEL A                                    
BC1RLNQ2 DS    XL1                 L'LEVEL A+B                                  
BC1RLNQ3 DS    XL1                 L'LEVEL A+B+C                                
BC1RLNQ4 DS    XL1                 L'LEVEL A+B+C+D                              
BCSJLNQS DS    0XL4                                                             
BCSJLNQ1 DS    XL1                 L'LEVEL A                                    
BCSJLNQ2 DS    XL1                 L'LEVEL A+B                                  
BCSJLNQ3 DS    XL1                 L'LEVEL A+B+C                                
BCSJLNQ4 DS    XL1                 L'LEVEL A+B+C+D                              
BCPIDNO  DS    XL2                                                              
BCTID    DS    CL8                                                              
PRDENDTE DS    PL3                                                              
PRDNUM   DS    XL1                                                              
SV1RPRSN DS    CL8                 PERSON CODE                                  
SV1ROFFC DS    CL2                 OFFICE CODE                                  
SV1RDPT  DS    CL3                 DEPARTMENT CODE                              
SV1RSDPT DS    CL3                 SUB DEPARTMENT CODE                          
*                                                                               
BCDUB    DS    D                                                                
BCDMCB   DS    6F                                                               
AEOB     DS    A                   END OF BUFFER                                
ANEXT    DS    A                                                                
*                                                                               
BCABUFF  DS    A                   A(CURRENT BUFFER)                            
BCAORIG  DS    A                   A(ORIGINAL ITEM)                             
BCANEW   DS    A                   A(NEW ITEM)                                  
*                                                                               
TMSNUM   DS    XL2                 NUMBER ENTRIES IN TMS TABLE                  
*                                                                               
STATUS   DS    XL1                                                              
STBCKOUT EQU   X'80'               BACK OUT OLD POSTING                         
STADDNEW EQU   X'40'               ADD NEW POSTING                              
STMINUS  EQU   X'20'               USE THE NEGATIVE OF THE RATE                 
STBUCKET EQU   X'10'               UPDATE BUCKETS ONLY                          
STORIGNL EQU   X'01'               USE ORIGINAL CLUSTER                         
STANEW   EQU   X'02'               USE NEW CLUSTER                              
STADIFF  EQU   X'04'               POST DIFFERENCE AMOUNT                       
*                                                                               
POSTSTAT DS    XL1                                                              
POST1R   EQU   X'80'               MAKING THE 1R POSTING- BUCKETS ONLY          
POSTSJ   EQU   X'40'               MAKING THE SJ POSTING                        
POSTSI   EQU   X'20'               MAKING THE SI POSTING                        
POST1C   EQU   X'10'               MAKING THE 1C POSTING                        
POST12   EQU   X'08'               MAKING THE 12 POSTING                        
POSTTXSJ EQU   X'04'               MAKING THE SJ POSTING - TAX POSTING          
POSTTXCR EQU   X'02'               MAKING THE CR POSTING - TAX POSTING          
*                                                                               
BCACCODE DS    CL14                ACCOUNT CODE                                 
BCACNAME DS    CL36                ACCOUNT NAME                                 
BCAMOUNT DS    PL6                 POSTING AMOUNT                               
BC1ROFC  DS    CL2                 1R OFFICE                                    
BCACANAL DS    CL12                ANALYSIS ACCOUNT                             
BCACCOST DS    CL1                 COSTING BYTE                                 
BCMOA    DS    PL3                 MONTH OF ACTIVITY                            
BCREF    DS    CL6                 TRANSACTION REFERENCE #                      
BCTXWC   DS    CL2                 TAX WORKCODE                                 
BCTXLOC  DS    CL8                 TAX LOCALITY                                 
BCTXBAS  DS    PL6                 TAX BASIS                                    
BCTXEFF  DS    PL3                 TAX EFFECTIVE DATE                           
BCTXRATE DS    PL4                 TAX RATE                                     
BCTXAMNT DS    PL6                 TAX AMOUNT                                   
BCLNARR  DS    XL1                 L'NARRATIVE                                  
BCNARR   DS    CL60                NARRATIVE                                    
*                                                                               
BCACCTS  DS    0CL14                                                            
BC1RACCT DS    CL14                1R ACCOUNT                                   
BC1RNAME DS    CL36                1R ACCOUNT NAME                              
BCSJACCT DS    0CL14               SJ OR 1N ACCOUNT                             
BCSJUL   DS    CL2                                                              
BCSJCLI  DS    CL3                                                              
BCSJPRD  DS    CL3                                                              
BCSJMED  DS    0CL1                MEDIA CODE                                   
BCSJJOB  DS    CL6                                                              
BCSJNAME DS    CL36                SJ OR 1N ACCOUNT NAME                        
BCSJCLNM DS    CL36                SJ CLIENT NAME                               
BCSIACCT DS    CL14                SI OR SK ACCOUNT                             
BCSINAME DS    CL36                SI OR SK ACCOUNT NAME                        
BCCNTRA  DS    CL14                1C OR 1N ACCOUNT                             
BCCNTRNM DS    CL36                1C OR 1N ACCOUNT NAME                        
BC12ACCT DS    CL14                12 ACCOUNT                                   
BC12NAME DS    CL36                12 ACCOUNT NAME                              
BCCRACCT DS    CL14                TAX - CREDIT ACCOUNT                         
BCCRNAME DS    CL36                TAX - CREDIT ACCOUNT NAME                    
BCACCTQ  EQU   *-BCACCTS                                                        
*                                                                               
         EJECT                                                                  
TTRNBLK  DS    CL(TTRNLNQ)         TIMETRN CONTROL BLOCK                        
TTRNBUF  DS    CL(TRNBUFLN)        TIMETRN BUFFER                               
TRNBUFLN EQU   2048                                                             
TEND     DS    0C                                                               
*                                                                               
ELEM     DS    CL255                                                            
BCWORK   DS    CL132                                                            
BCWORK2  DS    CL132                                                            
       ++INCLUDE ACADDTRND                                                      
TRNIO    DS    CL2048              TRANSACTION IO AREA - 2K                     
         EJECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* ++INCLUDES                                                          *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE ACTIMETRND                                                     
COBLOCKD DSECT                                                                  
       ++INCLUDE ACCAPBLOCK                                                     
       ++INCLUDE ACTIMEBLK                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*  ACREPWORKD                                                                   
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020ACREPXA02 01/21/03'                                      
         END                                                                    
