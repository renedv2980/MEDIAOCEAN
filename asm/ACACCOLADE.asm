*          DATA SET ACACCOLADE AT LEVEL 044 AS OF 04/19/16                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 046711.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE ACCOLADA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE ACSAVE                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE CHOPPER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE DECODE                                                                 
*INCLUDE DUMPOUT                                                                
*INCLUDE EXPAND                                                                 
*INCLUDE GETLOGO                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE LOGON                                                                  
*INCLUDE LOGOC                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE DMISLDDS                                                               
         TITLE 'Account system DUMP and LOAD program'                           
         PRINT NOGEN                                                            
ACCOLADE CSECT                                                                  
         ENTRY UTL                                                              
         NBASE 0,ACCOLADE,RA,R9,WORK=V(REGSAVE)                                 
         LA    RC,4095(R9)                                                      
         LA    RC,1(RC)                                                         
         DROP  RB,RA,R9                                                         
                                                                                
         USING ACCOLADE,RB,RA,R9,RC                                             
         USING ACCWORKD,R6         R6=A(GLOBAL W/S)                             
         L     R6,AWORK                                                         
ACC0     ST    R1,ACOMRG           SAVE MVS SUPV INFO                           
         LA    R3,DMPLIST                                                       
         LR    R2,RB                                                            
ACC01    ST    R2,0(R3)                                                         
         L     R2,=V(DUMMY)                                                     
         ST    R2,4(R3)                                                         
         OI    4(R3),X'80'         END OF LIST                                  
         GOTO1 =V(STXITER),DMCB,A(DMPLIST)                                      
*                                                                               
         L     R1,ACOMRG                                                        
         L     R1,0(R1)                                                         
         LH    R2,0(R1)                                                         
         LTR   R2,R2               R2=L'PARM DATA                               
         BZ    ACC0C                                                            
         CHI   R2,8                                                             
         BNH   *+8                                                              
         LA    R2,8                                                             
         LA    R1,2(R1)            R1=A(PARM DATA)                              
         LA    RF,UPSITAB                                                       
ACC0A    CLI   0(R1),C'0'                                                       
         BE    ACC0B                                                            
         CLI   0(R1),C'1'                                                       
         BNE   ACC0C                                                            
         OC    UPSIVAL,0(RF)                                                    
ACC0B    LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R2,ACC0A                                                         
ACC0C    XC    APHASE,APHASE       CLEAR EXTERNAL LOAD POINT ADDR               
         B     ACC0X                                                            
UPSITAB  DC    X'8040201008040201'                                              
ACC0X    EQU   *                                                                
         GOTO1 VDATCON,DMCB,(5,DUB),(10,DATEVAL)                                
         DATE  DUB,DATE=NO                                                      
         MVC   CTRYVAL(2),DUB+6                                                 
         EJECT                                                                  
***********************************************************************         
*              READ AND VALIDATE CONTROL CARDS                                  
***********************************************************************         
ACC1     L     R8,VCPRINT          R8=A(PRINTER CSECT)                          
         USING DPRINT,R8                                                        
         MVC   TITLE+16(27),=C'LIST OF INPUT CONTROL CARDS'                     
*                                                                               
ACC2     GOTO1 VCARDS,DMCB,CDAREA,=C'RE00'                                      
         CLC   CDAREA(2),=C'/*'                                                 
         BE    ACCVAL                                                           
*                                                                               
ACC3     LA    R2,PARMTAB          SEARCH PARMTAB FOR PARAMETER                 
         SR    R3,R3                                                            
*                                                                               
ACC4     CLI   0(R2),X'FF'                                                      
         BE    ACCERR                                                           
         IC    R3,0(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   1(0,R2),CDAREA                                                   
         BE    *+12                                                             
         LA    R2,L'PARMTAB(R2)                                                 
         B     ACC4                                                             
         MVC   FULL+1(3),11(R2)                                                 
         L     R2,FULL                                                          
         LA    R3,CDAREA+2(R3)                                                  
         BR    R2                                                               
*                                                                               
ACCIN    CLI   SWIN,0              INPUT=XXXX(,NNN)                             
         BNE   ACCERR                                                           
         CLC   0(4,R3),=C'DISK'                                                 
         BNE   *+12                                                             
         OI    SWIN,X'80'          SET INPUT=DISK                               
         B     ACCPRT                                                           
         CLC   0(4,R3),=C'TAPE'                                                 
         BNE   ACCERR                                                           
         LA    R2,SWIN                                                          
         LA    R3,4(R3)                                                         
         L     R4,ATINT                                                         
         B     ACCSYS                                                           
*                                                                               
ACCOUT   CLI   SWOUT,0             OUTPUT=XXXX(,NNN)                            
         BNE   ACCERR                                                           
         CLC   0(4,R3),=C'DISK'                                                 
         BNE   *+12                                                             
         OI    SWOUT,X'80'         SET OUTPUT=DISK                              
         B     ACCPRT                                                           
         CLC   0(4,R3),=C'TAPE'                                                 
         BNE   ACCERR                                                           
         LA    R2,SWOUT                                                         
         LA    R3,4(R3)                                                         
         L     R4,ATOUT                                                         
         B     ACCSYS                                                           
*                                                                               
ACCOPY   CLI   SWCOPY,0            COPY=TAPE(,NNN)                              
         BNE   ACCERR                                                           
         CLC   0(4,R3),=C'TAPE'                                                 
         BNE   ACCERR                                                           
         LA    R2,SWCOPY                                                        
         LA    R3,4(R3)                                                         
         L     R4,ATCOPY                                                        
*                                                                               
ACCSYS   CLI   0(R3),C','          CHECK IF OVERRIDE SYS NUMBER                 
         BNE   ACCSYS2                                                          
         MVC   DUB(3),=4C'0'                                                    
         MVZ   DUB(3),1(R3)                                                     
         CLC   DUB(3),=4C'0'                                                    
         BNE   ACCERR                                                           
         PACK  DUB,1(3,R3)                                                      
         CVB   R1,DUB                                                           
         CHI   R1,255                                                           
         BH    ACCERR                                                           
         LTR   R1,R1                                                            
         BZ    ACCERR                                                           
*                                                                               
ACCSYS2  OI    0(R2),X'40'         SET INPUT/OUTPUT=TAPE                        
         B     ACCPRT                                                           
*                                                                               
ACCMODE  CLI   SWMODE,0            MODE=XXXXXX                                  
         BNE   ACCERR                                                           
         CLC   0(4,R3),=C'COPY'                                                 
         BNE   *+12                                                             
         OI    SWMODE,X'80'                                                     
         B     ACCMODE2                                                         
         CLC   0(6,R3),=C'REPORT'                                               
         BNE   ACCERR                                                           
         OI    SWMODE,X'40'                                                     
ACCMODE2 OI    SWMODE,X'20'        SET MODE=XXXXXXX NEW                         
         B     ACCPRT                                                           
*                                                                               
ACCLOAD  CLI   SWLOAD,0            LOAD=XXXXXXXX                                
         BNE   ACCERR                                                           
         OI    SWLOAD,X'80'        SET LOAD=YES                                 
         MVC   PHASE,0(R3)                                                      
         L     RF,APHASE                                                        
         GOTO1 =V(LOADER),DMCB,PHASE,(RF)                                       
         L     RF,4(R1)                                                         
         LTR   RF,RF                                                            
         BZ    ACCERR                                                           
         ST    RF,APHASE           SAVE A(LOADED PHASE)                         
         NI    DMPLIST+4,X'FF'-X'80'                                            
         ST    RF,DMPLIST+8        DUMP LOADED PHASE                            
         A     RF,0(R1)            LENGTH OF PHASE                              
         ST    RF,DMPLIST+12                                                    
         OI    DMPLIST+12,X'80'    END OF LIST                                  
         GOTO1 =V(STXITER),DMCB,A(DMPLIST)                                      
         B     ACCPRT                                                           
*                                                                               
ACCFILE  L     R4,AACCOUNT         FILE=XXXXXXX                                 
         MVC   22(7,R4),0(R3)      PATCH FILE NAME                              
         L     R4,=A(ACCOUNX)      FILE=XXXXXXX FOR 3390                        
         MVC   22(7,R4),0(R3)      PATCH FILE NAME                              
         CLC   0(6,R3),ACCHST                                                   
         BNE   *+8                                                              
         OI    SWGEN,X'40'         IGNORE MISSING GENERATION                    
         B     ACCPRT                                                           
*                                                                               
ACCONID  MVC   DUB(4),=4C'0'       CONTROL=NNNN                                 
         MVZ   DUB(4),0(R3)                                                     
         CLC   DUB(4),=4C'0'                                                    
         BNE   ACCERR                                                           
         PACK  DUB,0(4,R3)                                                      
         CVB   R1,DUB                                                           
         STH   R1,CONID            SET CONTROL ID NUMBER                        
         OI    SWLOGO,X'80'        SET LOGO=YES                                 
         B     ACCPRT                                                           
*                                                                               
ACCSTRT  OI    SWSTRT,X'80'        START=XXXXXXXX                               
         LA    R4,KEYSTRT                                                       
         B     ACCKEYS                                                          
*                                                                               
ACCEND   OI    SWEND,X'80'         END=XXXXXXXX                                 
         LA    R4,KEYEND                                                        
*                                                                               
ACCKEYS  GOTO1 VDECODE,DMCB,(42,(R3)),(C' ',(R4))                               
         CLI   8(R1),0                                                          
         BNE   ACCERR                                                           
         B     ACCPRT                                                           
*                                                                               
ACCBAL   CLI   SWBAL,0             BALCHECK=YES/NO/CUL                          
         BNE   ACCERR                                                           
         CLC   0(2,R3),=C'NO'                                                   
         BE    ACCPRT                                                           
         CLC   0(3,R3),=C'YES'                                                  
         BNE   *+12                                                             
         OI    SWBAL,X'80'                                                      
         B     ACCPRT                                                           
         MVC   BALLEDG,0(R3)                                                    
         OI    SWBAL,X'81'                                                      
         B     ACCPRT                                                           
*                                                                               
ACCSEQS  OI    SWSEQ,X'80'         SEQERR=IGNORE                                
         B     ACCPRT                                                           
*                                                                               
ACCORID  LA    R4,SWOVER           OVERRIDE=XXX                                 
         B     ACCALL                                                           
*                                                                               
ACCGEN   LA    R4,SWGEN            GENUP=XXX                                    
         B     ACCALL                                                           
*                                                                               
ACCREP   LA    R4,SWREP            REPORT=XXX                                   
         B     ACCALL                                                           
*                                                                               
ACCDET   LA    R4,SWDEL            DELETE=XXX                                   
         MVC   DELSW,0(R3)                                                      
         B     ACCALL                                                           
*                                                                               
ACCCNT   LA    R4,SWCNT            COUNT=XXX                                    
         B     ACCALL                                                           
*                                                                               
ACCREC   LA    R4,SWREC            RECCHECK=XXX                                 
         B     ACCALL                                                           
*                                                                               
ACCDIT   LA    R4,SWDIT            DITTO=XXX                                    
         B     ACCALL                                                           
*                                                                               
ACCLOGO  LA    R4,SWLOGO           LOGO=XXX                                     
         B     ACCALL                                                           
*                                                                               
ACCHIST  LA    R4,SWHIST      HISTORY=XXX(SEGREGATE TALENT HISTORY)             
         B     ACCALL                                                           
*                                                                               
ACCTRK   CLC   0(2,R3),=C'NO'      NO-OP THE FULL TRACK READ                    
         BNE   ACCERR                                                           
         MVI   IOPARM6,0                                                        
         B     ACCPRT                                                           
*                                                                               
ACCDDSIO L     RE,=V(DDSIO)                                                     
         MVC   0(8,RE),0(R3)       MOVE DDSIO LOAD MODULE NAME                  
         B     ACCPRT                                                           
*                                                                               
ACCDVICE CLC   0(4,R3),=C'3390'                                                 
         BNE   ACCERR                                                           
         L     RE,=A(ACCOUNX)      DCB FOR 3390                                 
         ST    RE,AACCOUNT                                                      
         ST    RE,IOPARM4                                                       
         ST    RE,IOPARMD                                                       
         B     ACCPRT                                                           
*                                                                               
ACCDATE  GOTO1 VDATVAL,DMCB,(0,0(R3)),WORK     DATE=MM/DD/YY                    
         OC    DMCB(4),DMCB                                                     
         BZ    ACCERR                                                           
         GOTO1 VDATCON,DMCB,(0,WORK),(10,DATEVAL)                               
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         MVC   SPECDATE(5),=C'DATE='                                            
         MVC   SPECDATE+5(6),WORK       YYMMDD                                  
         B     ACCPRT                                                           
*                                                                               
ACCPEEL  DS    0H                                                               
         MVC   DUB(2),=4C'0'                                                    
         MVZ   DUB(2),0(R3)        MUST BE NUMBERS 00-99                        
         CLC   DUB(2),=4C'0'                                                    
         BNE   ACCERR                                                           
         PACK  DUB,0(2,R3)                                                      
         CVB   R2,DUB                                                           
         LNR   R2,R2                                                            
         GOTO1 VDATCON,DMCB,(5,DUB),(0,DUB)                                     
         GOTO1 =V(ADDAY),DMCB,DUB,WORK,(R2)                                     
         GOTO1 VDATCON,DMCB,(0,WORK),(2,PEELDATE)                               
         B     ACCPRT                                                           
*                                                                               
ACCALL   CLI   0(R4),0             XXXXXXXX=YES/NO                              
         BNE   ACCERR                                                           
         CLC   0(2,R3),=C'NO'                                                   
         BE    ACCPRT                                                           
         CLC   0(3,R3),=C'YES'                                                  
         BNE   ACCERR                                                           
         OI    0(R4),X'80'                                                      
         B     ACCPRT                                                           
*                                                                               
ACCPRT   MVC   P(80),CDAREA                                                     
         GOTO1 VPRINTER                                                         
         B     ACC2                                                             
         EJECT                                                                  
***********************************************************************         
*              HANDLE CONTROL CARD ERRORS                                       
***********************************************************************         
ACCERR   MVC   P(80),CDAREA                                                     
         MVC   P+80(20),=C'INVALID CONTROL CARD'                                
         GOTO1 VPRINTER                                                         
         GOTO1 VLOGIO,DMCB,1,(20,CDAREA)                                        
         GOTO1 (RF),(R1),1,=C'INVALID CONTROL CARD'                             
         GOTO1 (RF),(R1),1,=C'PLEASE CORRECT,IGNORE OR CANCEL'                  
         MVC   CDAREA,SPACES                                                    
         GOTO1 (RF),(R1),0,(20,CDAREA)                                          
         CLC   CDAREA(6),=C'IGNORE'                                             
         BE    ACC2                                                             
         CLC   CDAREA(6),=C'CANCEL'                                             
         BNE   ACC3                                                             
         MVI   DUB,0                                                            
         B     ACCVERR             INVALID CONTROL CARD                         
         EJECT                                                                  
***********************************************************************         
*              SEE IF PARAMETERS ARE LOGICALLY VALID                            
***********************************************************************         
ACCVAL   MVI   DUB,1                                                            
         CLI   SWIN,0              INPUT MUST BE SPEC                           
         BE    ACCVERR                                                          
         CLI   SWCOPY,X'00'        IF COPY=TAPE                                 
         BE    *+12                                                             
         TM    SWOUT,X'40'         OUTPUT=TAPE, MUST BE USED                    
         BZ    ACCVERR                                                          
         TM    SWMODE,X'80'                                                     
         BZ    *+16                                                             
         MVI   DUB,2                                                            
         CLI   SWOUT,0             OUTPUT MUST BE SPEC IF MODE=COPY             
         BE    ACCVERR                                                          
         TM    SWMODE,X'40'                                                     
         BZ    *+16                                                             
         MVI   DUB,3                                                            
         CLI   SWOUT,0             OUTPUT MUST NOT BE SPEC IF MODE=REP          
         BNE   ACCVERR                                                          
         MVI   DUB,4                                                            
         CLI   SWMODE,0            MODE MUST BE SPEC                            
         BE    ACCVERR                                                          
         MVI   DUB,5                                                            
         TM    SWIN,X'80'                                                       
         BZ    *+12                                                             
         TM    SWOUT,X'80'         DISK CAN NOT BE INPUT & OUTPUT SPEC          
         BO    ACCVERR                                                          
         B     ACCSET                                                           
*                                                                               
ACCVERR  SR    R1,R1               OUTPUT ERROR MESSAGE                         
         IC    R1,DUB                                                           
         MH    R1,=H'40'                                                        
         LA    R1,ERRTAB(R1)                                                    
         MVC   P(40),0(R1)                                                      
         GOTO1 VPRINTER                                                         
         B     EOJ                                                              
         EJECT                                                                  
***********************************************************************         
*              BUILD CODE HOOK LIST FROM REQUESTED OPTIONS                      
***********************************************************************         
ACCSET   LA    RE,SWTTAB                                                        
         MVI   DUB,0                                                            
ACCSET0  CLI   0(RE),X'FF'                                                      
         BE    ACCSETX                                                          
         CLI   0(RE),0                                                          
         BE    ACCSET8                                                          
         MVC   DUB+1(3),1(RE)      X'80' HOOK                                   
         TM    0(RE),X'80'                                                      
         BO    *+10                                                             
         MVC   DUB+1(3),4(RE)      X'40' HOOK                                   
         OC    DUB(4),DUB                                                       
         BZ    ACCSET8             HOOK NOT REQUIRED                            
         L     R1,DUB              A(HOOK TABLE ENTRY)                          
         LM    R2,R4,NEXTHOOK                                                   
         SR    R5,R5                                                            
         LA    R7,4(R1)                                                         
         IC    R5,0(R1)            L'FIRST TIME HOOKS                           
         LTR   R5,R5                                                            
         BZ    ACCSET2                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R7)       SET FIRST                                    
         LA    R2,0(R5,R2)                                                      
         LA    R7,0(R5,R7)                                                      
*                                                                               
ACCSET2  IC    R5,1(R1)            L'PROCESS HOOKS                              
         LTR   R5,R5                                                            
         BZ    ACCSET4                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R7)       SET PROCESS                                  
         LA    R3,0(R5,R3)                                                      
         LA    R7,0(R5,R7)                                                      
*                                                                               
ACCSET4  IC    R5,2(R1)            L'LAST TIME HOOKS                            
         LTR   R5,R5                                                            
         BZ    ACCSET6                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R7)       SET LAST                                     
         LA    R4,0(R5,R4)                                                      
         LA    R7,0(R5,R7)                                                      
ACCSET6  STM   R2,R4,NEXTHOOK                                                   
*                                                                               
ACCSET8  LA    RE,L'SWTTAB(RE)     BUMP TO NEXT ENTRY                           
         B     ACCSET0                                                          
ACCSETX  MVC   INPDEV,SWIN         SET I/O DEVICES IN WORK                      
         MVC   OUTDEV,SWOUT                                                     
         B     FIRST                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO HANDLE READING OF TAPES                                           
***********************************************************************         
TINFRST  ST    R7,SAVER7           FIRST TIME                                   
         BAS   R1,OPNTINT                                                       
         BAS   R7,TINPROC                                                       
         MVI   TINSW,C'Y'                                                       
         L     R1,AIOAREA                                                       
         CLC   4(42,R1),KEYSTRT                                                 
         BNL   *+8                                                              
         MVI   WRITE,X'FF'                                                      
         L     R7,SAVER7                                                        
         BR    R7                                                               
*                                                                               
TINPROC  L     R1,ATINT            PROCESS                                      
         L     R0,AIOAREA                                                       
         GET   (1),(0)                                                          
         MVI   WRITE,0                                                          
         AP    PHINCNT,=P'1'                                                    
         CLI   TINSW,C'Y'                                                       
         BNER  R7                                                               
         L     R1,AIOAREA          CHECK FOR START/END KEYS                     
         CLC   4(42,R1),KEYSTRT                                                 
         BL    TINPROC                                                          
         CLC   4(42,R1),KEYEND                                                  
         BH    *+6                                                              
         BR    R7                                                               
         SP    PHINCNT,=P'1'                                                    
         B     LAST                                                             
*                                                                               
TINLAST  BAS   R1,CLSTINT          LAST TIME                                    
         BR    R7                  PER OPERATIONS REMOVE CONSOLE MSG            
*                                                                               
TINLAST2 GOTO1 VLOGIO,DMCB,1,=C'ANY MORE ACC INPUT TAPES'                       
         GOTO1 (RF),(R1),0,(3,MSGAREA)                                          
         CLI   MSGAREA,C'N'                                                     
         BER   R7                                                               
         CLI   MSGAREA,C'Y'                                                     
         BE    TINLAST4                                                         
         GOTO1 (RF),(R1),1,=C'INVALID REPLY'                                    
         B     TINLAST2                                                         
*                                                                               
TINLAST4 BAS   R1,OPNTINT                                                       
         B     PROCESS                                                          
         EJECT                                                                  
***********************************************************************         
* Routines to handle reading of disk                                            
***********************************************************************         
DINFRST  ST    R7,SAVER7           FIRST TIME                                   
         GOTO1 VISDDS,IOPARM1                                                   
*                                  SET UP FOR CACHE MEMORY                      
         L     R7,AACCOUNT                                                      
         USING ISDTF,R7                                                         
         LA    R1,ISFADCB          GET A(DCB ADDRESS)                           
         LA    R0,3                SET 3 FOR DUMP                               
         LNR   R0,R0               NEGATIVE VALUE TO SUPERVISOR                 
*&&US*&& SVC   247                                                              
         DROP  R7                                                               
         MVC   IOPARM1,VISREAD                                                  
         BAS   R7,DINPROC                                                       
         MVC   IOPARM1,VISRDSEQ                                                 
         L     R7,SAVER7                                                        
         BR    R7                                                               
*                                                                               
DINPROC  LA    R1,IOPARM1          PROCESS                                      
         L     RF,VISDDS                                                        
         BASR  RE,RF                                                            
         MVI   WRITE,0                                                          
         L     R1,AIOAREA                                                       
         LA    R2,4(R1)                                                         
         USING ACKEYD,R2                                                        
         MVC   HALF,ACLENGTH                                                    
         LH    R2,HALF                                                          
         LA    R2,4(R2)                                                         
         XC    0(4,R1),0(R1)                                                    
         STH   R2,0(R1)                                                         
         OC    IOPARM3(2),IOPARM3                                               
         BNZ   DINPROC2                                                         
         AP    PHINCNT,=P'1'                                                    
         CLC   4(42,R1),LASTKEY    CHECK READ SEQUENCE                          
         BNH   DINPROC4                                                         
         MVC   LASTKEY,4(R1)                                                    
         TM    SWEND,X'80'                                                      
         BZR   R7                                                               
         CLC   4(42,R1),KEYEND                                                  
         BH    *+6                                                              
         BR    R7                                                               
         SP    PHINCNT,=P'1'                                                    
         B     LAST                                                             
*                                                                               
DINPROC2 TM    IOPARM3+1,X'04'     EOF                                          
         BO    LAST                                                             
         MVC   MSGAREA,=CL40'UNRECOVERABLE DISK ERROR KEY -'                    
         B     BADREC                                                           
*                                                                               
DINPROC4 GOTO1 VLOGIO,DMCB,1,=C'**ACCOLADE** RECORD OUT OF SEQUENCE'            
         AP    SEQERR,=P'1'                                                     
         AP    PHBDCNT,=P'1'                                                    
         MVC   MSGAREA,=CL40'RECORD OUT OF SEQUENCE KEY -'                      
         MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         MVC   WIDTH,=H'60'                                                     
         BAS   RE,LASTKP           PRINT LASTKEY                                
         BAS   RE,HEXREC           AND CURRENT RECORD                           
         XC    LASTKEY,LASTKEY                                                  
         BR    R7                                                               
*                                                                               
DINLAST  DC    0H'0'                                                            
         BR    R7                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO CHECK GENERATION RECORD. THIS CODE IS ALWAYS EXECUTED             
* AFTER INITIAL READ FROM FILE.                                                 
***********************************************************************         
GENCHCK  CLI   PROCREC,HEADER      IS RECORD TYPE=GENERATION                    
         BE    GENCHCK2                                                         
         TM    SWGEN,X'40'         IGNORE FOR ACCHST                            
         BOR   R7                                                               
         MVI   DUB,7                                                            
         MVC   MSGAREA,=CL40'GENERATION RECORD MISSING KEY -'                   
         TM    SWGEN,X'80'         GENUP OPTION REQUESTED                       
         BO    GENCHCK4                                                         
         TM    SWOUT,X'80'         OUTPUT=DISK REQUIRES GENERATION REC          
         BO    GENCHCK4                                                         
         TM    SWSTRT,X'80'        IF START=KEY REQUESTED IGNORE                
         BOR   R7                                                               
         TM    SWIN,X'80'          INPUT=DISK REQUIRES GENERATION REC           
         BO    GENCHCK4                                                         
         BR    R7                  OTHERWISE OK                                 
*                                                                               
GENCHCK2 L     R2,AIOAREA          GET GENERATION ELEMENT                       
         LA    R2,4(R2)                                                         
         USING ACKEYD,R2                                                        
         LA    R2,ACRECORD                                                      
         USING ACCONTD,R2                                                       
***********************************************************************         
*         GET LAST UPDATE - LESS 5 DAYS - TO KEEP OLD BATCH HEADERS             
***********************************************************************         
         GOTO1 VDATCON,DMCB,(2,ACCONUPS),(0,DUB)                                
         LA    R0,5                                                             
         LNR   R0,R0                                                            
         GOTO1 =V(ADDAY),DMCB,DUB,WORK,(R0)                                     
         GOTO1 VDATCON,DMCB,(0,WORK),(1,ACUPDTE3)                               
*                                                                               
         ZAP   GENNIN,ACCONCOT     GET INPUT GENERATION NUMBER                  
         ZAP   GENOUT,GENNIN                                                    
         MVC   ACTDATE,ACCONUPS    SET ACTIVITY DATE TO UPDATE DATE             
         TM    SWOUT,X'80'         TEST IF OUTPUT=DISK SPECIFIED                
         BZ    GENCHK2A                                                         
         MVC   ACTDATE,ACCONLOD    SET ACTIVITY DATE TO LAST LOAD DATE          
         MVC   ACCONLOD,ACCONUPS   SET LOAD DATE FROM LAST UPDATE               
*                                                                               
GENCHK2A TM    SWGEN,X'80'         TEST GENUP=YES SPECIFIED                     
         BZR   R7                                                               
         AP    GENOUT,=P'1'                                                     
         AP    ACCONCOT,=P'1'                                                   
         BR    R7                                                               
*                                                                               
GENCHCK4 GOTO1 VLOGIO,DMCB,1,=C'**ACCOLADE** GENERATION RECORD MISSING'         
         B     BADREC                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO CHECK PHYSICAL CONSISTENCY OF RECORDS.                            
***********************************************************************         
RECPROC  ST    R7,SAVER7           PROCESS                                      
         L     R2,AIOAREA                                                       
         LA    R2,4(R2)            R2=A(RECORD)                                 
         USING ACKEYD,R2                                                        
         LA    R3,ACRECORD         R3=A(FIRST ELEMENT)                          
         MVC   HALF,ACLENGTH                                                    
         LH    R5,HALF             R5=L'RECORD                                  
         LA    R4,0(R5,R2)                                                      
         BCTR  R4,0                R4=A(END OF RECORD)                          
         SR    R1,R1                                                            
         MVI   DIE,C'N'                                                         
         MVC   MSGAREA,=CL40'RECORD LENGTH LE MINIMUM KEY -'                    
         CH    R5,RECMIN                                                        
         BL    BADREC                                                           
         MVC   MSGAREA,=CL40'RECORD LENGTH GR MAXIMUM KEY -'                    
         CH    R5,RECMAX                                                        
         BH    BADREC                                                           
         XC    1(24,R4),1(R4)                                                   
*                                                                               
RECPROC2 CLI   0(R3),0             ELEMENT CODE=ZERO                            
         BE    RECPROC4                                                         
         CLI   1(R3),0             ELEMENT LENGTH=ZERO                          
         BE    RECPROC6                                                         
         IC    R1,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R1                                                            
         CR    R3,R4               OUT OF SCOPE OF RECORD                       
         BH    RECPROC8                                                         
         BNE   RECPROC2                                                         
         MVI   0(R3),0             SET END OF RECORD                            
         B     RECPROCX                                                         
*                                                                               
RECPROC4 LA    R4,1(R4)                                                         
         SR    R4,R2               R4=LENGTH OF RECORD                          
         STH   R4,HALF                                                          
         MVC   ACLENGTH,HALF       SET NEW LENGTH OF RECORD                     
         L     R2,AIOAREA                                                       
         LA    R4,4(R4)                                                         
         STH   R4,0(R2)                                                         
         B     RECPROCX                                                         
*                                                                               
RECPROC6 MVC   MSGAREA,=CL40'ELEMENT LENGTH OF ZERO DETECTED KEY -'             
         BCTR  R4,0                                                             
         CR    R3,R4                                                            
         BNE   BADREC                                                           
         MVI   0(R3),0                                                          
         B     RECPROC4                                                         
*                                                                               
RECPROC8 LA    R4,1(R4)                                                         
         CR    R3,R4                                                            
         BE    RECPROC4                                                         
         MVC   MSGAREA,=CL40'ELEMENT CHAIN ERROR DETECTED KEY -'                
         B     BADREC                                                           
*                                                                               
RECPROCX MVI   DIE,C'Y'            RESET FORCED CANCEL BYTE                     
         L     R7,SAVER7           RETURN                                       
         BR    R7                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET WHAT TYPE OF RECORD THAT HAS BEEN READ AND EXTRACT ANY         
* PERTINENT VALUES FROM IT. THIS CODE IS EXECUTED AFTER EVERY READ.             
***********************************************************************         
RECTYPE  XC    ABALEL(08),ABALEL                                                
         MVC   RECNAME,SPACES                                                   
         L     R2,AIOAREA                                                       
         LH    R1,0(R2)                                                         
         LA    R1,0(R2,R1)                                                      
         XC    0(24,R1),0(R1)      CLEAR END OF RECORD                          
         LA    R2,4(R2)                                                         
         USING ACKEYD,R2                                                        
         MVI   PROCREC,TAXRULES                                                 
         CLI   0(R2),X'05'                                                      
         BER   R7                                                               
         MVI   PROCREC,BSOURCE                                                  
         CLI   0(R2),X'06'                                                      
         BER   R7                                                               
         MVI   PROCREC,OFFICE                                                   
         CLI   0(R2),X'01'                                                      
         BER   R7                                                               
         MVI   PROCREC,MEDINTR                                                  
         CLI   0(R2),X'08'                                                      
         BER   R7                                                               
         MVI   PROCREC,MEDIA                                                    
         CLI   0(R2),X'09'                                                      
         BER   R7                                                               
         MVI   PROCREC,ANALYSIS                                                 
         CLI   0(R2),X'0A'                                                      
         BER   R7                                                               
         MVI   PROCREC,BATCH                                                    
         CLI   0(R2),X'0B'                                                      
         BER   R7                                                               
         MVI   PROCREC,COMMENT                                                  
         CLI   0(R2),X'0C'                                                      
         BER   R7                                                               
         MVI   PROCREC,ERROR                                                    
         CLI   0(R2),X'0E'                                                      
         BER   R7                                                               
         MVI   PROCREC,BILRATE                                                  
         CLI   0(R2),X'19'                                                      
         BER   R7                                                               
         MVI   PROCREC,PRODORD                                                  
         CLI   0(R2),X'1A'                                                      
         BER   R7                                                               
         MVI   PROCREC,BUDGET                                                   
         CLI   0(R2),X'1B'                                                      
         BER   R7                                                               
         MVI   PROCREC,LIST                                                     
         CLI   0(R2),X'1D'                                                      
         BER   R7                                                               
         MVI   PROCREC,FEECTRL                                                  
         CLI   0(R2),X'1F'                                                      
         BER   R7                                                               
         MVI   PROCREC,FEEAREA                                                  
         CLI   0(R2),X'20'                                                      
         BER   R7                                                               
         MVI   PROCREC,FEEPCNT                                                  
         CLI   0(R2),X'21'                                                      
         BER   R7                                                               
         MVI   PROCREC,CAST                                                     
         CLI   0(R2),X'22'                                                      
         BER   R7                                                               
         MVI   PROCREC,CYCLE                                                    
         CLI   0(R2),X'23'                                                      
         BER   R7                                                               
         MVI   PROCREC,GUAR                                                     
         CLI   0(R2),X'24'                                                      
         BER   R7                                                               
         MVI   PROCREC,AGPTR                                                    
         CLI   0(R2),X'25'                                                      
         BER   R7                                                               
         MVI   PROCREC,AGENT                                                    
         CLI   0(R2),X'26'                                                      
         BER   R7                                                               
         MVI   PROCREC,GFEST                                                    
         CLI   0(R2),X'27'                                                      
         BER   R7                                                               
         MVI   PROCREC,MISCWITH                                                 
         CLI   0(R2),X'28'                                                      
         BER   R7                                                               
         MVI   PROCREC,W4ALPHA                                                  
         CLI   0(R2),X'29'                                                      
         BER   R7                                                               
         MVI   PROCREC,BILRATE                                                  
         CLI   0(R2),X'2A'                                                      
         BER   R7                                                               
         MVI   PROCREC,XCOMEST                                                  
         CLI   0(R2),X'2B'                                                      
         BER   R7                                                               
         MVI   PROCREC,GENPROD                                                  
         CLI   0(R2),X'2C'                                                      
         BER   R7                                                               
         MVI   PROCREC,GENACCT                                                  
         CLI   0(R2),X'2D'                                                      
         BER   R7                                                               
         MVI   PROCREC,BILLTRN     MEDIA BILLING TRANSFER                       
         CLI   0(R2),X'2F'                                                      
         BER   R7                                                               
         MVI   PROCREC,COSTACC     COST ACCOUNTING                              
         CLI   0(R2),X'3E'                                                      
         BER   R7                                                               
         MVI   PROCREC,REQUEST     REQUEST                                      
         CLI   0(R2),X'3F'                                                      
         BER   R7                                                               
         MVI   PROCREC,CHKAUTH                                                  
         CLI   0(R2),X'10'                                                      
         BER   R7                                                               
         MVI   PROCREC,COMPANY                                                  
         CLC   1(41,R2),SPACES                                                  
         BE    RECTYPE4                                                         
         MVI   PROCREC,UNIT                                                     
         CLC   2(40,R2),SPACES                                                  
         BE    RECTYPE4                                                         
         MVI   PROCREC,LEDGER                                                   
         CLC   3(39,R2),SPACES                                                  
         BE    RECTYPE4                                                         
         MVI   PROCREC,ACCHIGH                                                  
         CLC   15(27,R2),SPACES                                                 
         BE    RECTYPE4                                                         
         MVI   PROCREC,HEADER                                                   
         OC    0(41,R2),0(R2)                                                   
         BZR   R7                                                               
         MVI   PROCREC,TRAILER                                                  
         CLI   0(R2),X'FF'                                                      
         BNE   RECTYPE2                                                         
         CLC   1(41,R2),0(R2)                                                   
         BER   R7                                                               
         MVI   PROCREC,UNKNOWN                                                  
         BR    R7                                                               
*                                                                               
RECTYPE2 MVI   PROCREC,UNKNOWN                                                  
         CLI   0(R2),X'40'                                                      
         BLR   R7                                                               
         MVI   PROCREC,TRNSACTN                                                 
         LA    R3,ACRECORD                                                      
         CLI   0(R3),X'44'                                                      
         BER   R7                                                               
         MVI   PROCREC,HISTORY                                                  
         CLI   0(R3),X'43'                                                      
         BER   R7                                                               
         CLI   0(R3),X'45'                                                      
         BER   R7                                                               
         MVI   PROCREC,OTHERS                                                   
         BR    R7                                                               
*                                                                               
RECTYPE4 LA    R3,ACRECORD         EXTRACT SOME VALUES                          
         SR    R4,R4                                                            
*                                                                               
RECTYPE6 CLI   0(R3),0                                                          
         BER   R7                                                               
         CLI   0(R3),X'20'         NAME ELEMENT                                 
         BNE   RECTYPE8                                                         
         IC    R4,1(R3)                                                         
         CH    R4,=H'38'                                                        
         BNH   *+8                                                              
         LH    R4,=H'38'                                                        
         SH    R4,=H'3'                                                         
         BM    RECTYPE8                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   RECNAME(0),2(R3)    SAVE NAME                                    
*                                                                               
RECTYPE8 CLI   0(R3),X'22'         ADDRESS ELEMENT                              
         BNE   *+12                                                             
         ST    R3,AADDEL                                                        
         B     RECTYPEA                                                         
         CLI   0(R3),X'32'         BALANCE ELEMENT                              
         BNE   *+12                                                             
         ST    R3,ABALEL                                                        
         MVI   PROCREC,ACCLOW                                                   
*                                                                               
RECTYPEA IC    R4,1(R3)                                                         
         AR    R3,R4                                                            
         B     RECTYPE6                                                         
         EJECT                                                                  
***********************************************************************         
* REPORT (DAILY TRIAL BALANCE) ROUTINES                                         
***********************************************************************         
REPFRST  ST    R7,SAVER7           FIRST TIME                                   
         BAS   R7,SETPRINT                                                      
         L     R7,SAVER7                                                        
         MVC   TITLE+15(19),=C'DAILY TRIAL BALANCE'                             
         MVC   MID1+40(7),=C'COMPANY'                                           
         MVI   MID2,X'00'                                                       
         MVC   SUB1(15),=C'UNIT AND LEDGER'                                     
         MVC   SUB2(15),=15C'-'                                                 
         MVC   SUB1+52(23),=C'BALANCE          DEBITS'                          
         MVC   SUB2+52(23),=C'B/FRWD           ------'                          
         MVC   SUB1+84(23),=C'CREDITS         PRESENT'                          
         MVC   SUB2+84(23),=C'-------         BALANCE'                          
         BR    R7                                                               
*                                                                               
REPLAST  MVI   REPTHIS,4           LAST TIME                                    
         B     REPPROCC                                                         
*                                                                               
REPPROC  L     R2,AIOAREA          PROCESS                                      
         LA    R2,4(R2)                                                         
         CLI   PROCREC,SPECIALS                                                 
         BNLR  R7                                                               
         CLI   PROCREC,ACCLOW                                                   
         BE    REPPROC2                                                         
         CLI   PROCREC,TRNSACTN                                                 
         BE    REPPROC4                                                         
         CLI   PROCREC,COMPANY                                                  
         BE    REPPROC6                                                         
         CLI   PROCREC,UNIT                                                     
         BE    REPPROC8                                                         
         CLI   PROCREC,LEDGER                                                   
         BE    REPPROCA                                                         
         BR    R7                                                               
*                                                                               
REPPROC2 L     R3,ABALEL           HANDLE ACCOUNT                               
         LTR   R3,R3                                                            
         BZR   R7                                                               
         USING ACBALD,R3                                                        
         TM    ACSTATUS,X'80'                                                   
         BOR   R7                                                               
         AP    ACFORWD,ACBLFRWD                                                 
         AP    ACBLDEB,ACBLDR                                                   
         AP    ACBLCRD,ACBLCR                                                   
         BR    R7                                                               
*                                                                               
REPPROC4 LA    R3,ACRECORD         HANDLE TRANSACTION                           
         USING TRANSD,R3                                                        
         B     REPPRC4A                                                         
*                                                                               
REPPRC4A LA    R4,ACDELCRD                                                      
         OC    ACDTPEEL,ACDTPEEL   TEST IF PEELED-OFF                           
         BZ    REPPRC4C                                                         
         AP    PEELRECS,=P'1'      COUNT PEELED                                 
         CLC   ACDTPEEL,ACTDATE    SINCE LAST LOAD/UPDATE                       
         BL    REPPRC4C                                                         
         TM    TRNSSTAT,X'80'      TEST FOR DEBIT                               
         BZ    *+8                                                              
         LA    R4,8(R4)                                                         
         AP    0(8,R4),TRNSAMNT                                                 
*                                                                               
REPPRC4C GOTO1 VDATCON,DMCB,(1,TRNSDATE),(2,WORK)                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         LA    R4,ACCRPOS                                                       
*                                  FIND ACTIVITY ELEMENT                        
REPPRC4E IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    REPPRC4G                                                         
         CLI   0(RE),X'60'                                                      
         BNE   REPPRC4E                                                         
         MVC   WORK(2),2(RE)                                                    
*                                                                               
REPPRC4G TM    SWOUT,X'80'         TEST IF LOADING TO DISK                      
         BZ    *+16                                                             
         CLC   WORK(2),ACTDATE     YES - DATE HIGH IS NEW POSTING               
         BH    REPPRC4H                                                         
         BR    R7                                                               
         CLC   WORK(2),ACTDATE     NO - DATE HIGH OR EQUAL                      
         BNL   REPPRC4H                                                         
         BR    R7                                                               
*                                                                               
REPPRC4H TM    TRNSSTAT,X'80'      TEST FOR DEBIT                               
         BZ    *+8                                                              
         LA    R4,8(R4)                                                         
         AP    0(8,R4),TRNSAMNT                                                 
         BR    R7                                                               
         DROP  R3                                                               
*                                                                               
REPPROC6 LA    R3,REPCOMP                                                       
         MVI   REPTHIS,3                                                        
         B     REPPROCC                                                         
*                                                                               
REPPROC8 LA    R3,REPUNIT                                                       
         MVI   REPTHIS,2                                                        
         B     REPPROCC                                                         
*                                                                               
REPPROCA LA    R3,REPLEDG                                                       
         MVI   REPTHIS,1                                                        
*                                                                               
REPPROCC ST    R3,FULL                                                          
         LA    R3,REPACTNS                                                      
*                                                                               
REPPROCD CLI   0(R3),X'FF'                                                      
         BER   R7                                                               
         CLC   REPPREV(2),0(R3)                                                 
         BE    *+12                                                             
         LA    R3,L'REPACTNS(R3)                                                
         B     REPPROCD                                                         
         MVI   DUB,0                                                            
         MVC   DUB+1(3),2(R3)                                                   
         L     RE,DUB                                                           
         BR    RE                                                               
*                                                                               
REPACT1  BAS   RE,REPTOTC          COMPANY/COMPANY                              
         B     REPPROCF                                                         
*                                                                               
REPACT2  BAS   RE,REPTOTU          UNIT/COMPANY                                 
         BAS   RE,REPTOTC                                                       
         B     REPPROCF                                                         
*                                                                               
REPACT3  BAS   RE,REPTOTU          UNIT/UNIT                                    
         B     REPPROCF                                                         
*                                                                               
REPACT4  BAS   RE,REPTOTL          LEDGER/COMPANY                               
         BAS   RE,REPTOTU                                                       
         BAS   RE,REPTOTC                                                       
         ZAP   LINE,=P'99'                                                      
         B     REPPROCF                                                         
*                                                                               
REPACT5  BAS   RE,REPTOTL          LEDGER/UNIT                                  
         BAS   RE,REPTOTU                                                       
         B     REPPROCF                                                         
*                                                                               
REPACT6  BAS   RE,REPTOTL          LEDGER/LEDGER                                
         B     REPPROCF                                                         
*                                                                               
REPACT7  BAS   RE,REPTOTL          ANY/LAST TIME                                
         BAS   RE,REPTOTU                                                       
         BAS   RE,REPTOTC                                                       
         MVI   DIVBYTE,1           END LOZENGE FOR PREVIOUS COMPANY             
         BAS   RE,DIVIDE                                                        
         MVC   DIVID,CONID         SET CONTROL ID NUMBER                        
         MVI   DIVBYTE,0           START LOZENGE FOR CONTROL                    
         BAS   RE,DIVIDE                                                        
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         BAS   RE,REPTOTF                                                       
         BR    R7                                                               
*                                                                               
REPPROCF L     R3,FULL                                                          
         MVC   2(36,R3),RECNAME                                                 
         CLI   REPTHIS,1                                                        
         BNE   *+10                                                             
         MVC   0(1,R3),2(R2)                                                    
         CLI   REPTHIS,2                                                        
         BNE   *+10                                                             
         MVC   0(1,R3),1(R2)                                                    
         CLI   REPTHIS,3                                                        
         BNE   REPPROCG                                                         
         MVC   0(1,R3),0(R2)                                                    
         CLI   REPSWTCH,0                                                       
         BE    *+12                                                             
         MVI   DIVBYTE,1           END LOZENGE FOR PREVIOUS COMPANY             
         BAS   RE,DIVIDE                                                        
         MVI   REPSWTCH,1                                                       
         BAS   RE,REPDIV           SET NEW COMPANY VALUES                       
         MVI   DIVBYTE,0           START LOZENGE FOR THIS COMPANY               
         BAS   RE,DIVIDE                                                        
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         GOTO1 VHEXOUT,DMCB,REPCOMP,MID1+48,1,=C'TOG'                           
         MVC   MID1+51(36),REPCOMP+2                                            
*                                                                               
REPPROCG MVC   REPPREV,REPTHIS                                                  
         L     R2,AREPACCS                                                      
         SR    R3,R3                                                            
         IC    R3,REPTHIS                                                       
         MVC   0(128,R2),ZEROE8                                                 
         LA    R2,128(R2)                                                       
         BCT   R3,*-10                                                          
         BR    R7                                                               
         EJECT                                                                  
***********************************************************************         
* REPORT TOTAL ROUTINES                                                         
***********************************************************************         
REPTOTL  ST    RE,SAVER7           THIS CHEATS A BIT                            
         BAS   RE,REPADDS                                                       
         GOTO1 VCHOPPER,DMCB,(38,REPLEDG),(20,REPRNT3),(C'P',2)                 
         GOTO1 (RF),(R1),(38,REPUNIT),(20,REPRNT1),(C'P',2)                     
         L     RE,SAVER7                                                        
         LA    R4,1                                                             
         B     REPRINT                                                          
*                                                                               
REPTOTU  MVC   REPRNT1(16),=C'TOTAL FOR UNIT -'                                 
         MVC   REPRNT1+17(1),REPUNIT                                            
         LA    R4,2                                                             
         B     REPRINT                                                          
*                                                                               
REPTOTC  MVC   REPRNT1(19),=C'TOTAL FOR COMPANY -'                              
         ST    RE,SAVER7                                                        
         GOTO1 VHEXOUT,DMCB,REPCOMP,REPRNT1+20,1,=C'TOG'                        
         L     RE,AREPACCS                                                      
         LA    RE,256(RE)                                                       
         CP    120(8,RE),=P'0'      TEST IF COMPANY IN BALANCE                  
         BE    REPTOTD                                                          
         L     RF,AERRNEXT         NO - ADD ENTRY TO ERROR LIST                 
         MVC   0(38,RF),REPCOMP                                                 
         ZAP   38(8,RF),120(8,RE)                                               
         LA    RF,46(RF)                                                        
         ST    RF,AERRNEXT                                                      
         MVI   0(RF),X'FF'                                                      
REPTOTD  L     RE,SAVER7                                                        
         LA    R4,3                                                             
         B     REPRINT                                                          
*                                                                               
REPTOTF  MVC   REPRNT1(14),=C'TOTAL FOR FILE'                                   
*&&US                                                                           
         MP    ACCRHIS,=P'-1'      HISTORY CREDITS ARE NEGATIVE                 
         L     R2,AREPACCS                                                      
         LA    R2,128*3(R2)       R2 TO FILE ACCUMS(LEVEL 4)                    
         AP    08(8,R2),ACDRHIS     ADD TO PREVIOUS GEN                         
         AP    16(8,R2),ACCRHIS                                                 
         LA    R2,32*3(R2)        R2 TO CURRENT GENERATION ACCUMS               
         AP    08(8,R2),ACDRHIS     ADD HISTORY DEBITS                          
         AP    16(8,R2),ACCRHIS     AND CREDITS                                 
*&&                                                                             
         MVC   MID1+40(50),SPACES                                               
         LA    R4,4                                                             
         ST    RE,SAVER7                                                        
         BAS   RE,REPRINT                                                       
*&&US                                                                           
         CP    ACDRHIS,=P'0'       TALENT HISTORY PAYMENTS                      
         BNE   *+14                                                             
         CP    ACCRHIS,=P'0'                                                    
         BE    REPTOTF1                                                         
         MVC   P+25(14),=C'TALENT HISTORY'                                      
         LA    R3,2                                                             
         LA    R4,ACDRHIS                                                       
         LA    R2,P+60                                                          
REPTOTFA CP    0(8,R4),=P'0'                                                    
         BE    REPTOTFB                                                         
         MVC   WORK(19),=X'4040202020202020202020202020214B202060'              
         ED    WORK(19),0(R4)                                                   
         MVC   0(15,R2),WORK+4                                                  
REPTOTFB LA    R4,8(R4)                                                         
         LA    R2,16(R2)                                                        
         BCT   R3,REPTOTFA                                                      
         GOTO1 VPRINTER                                                         
*&&                                                                             
REPTOTF1 L     R2,AERRACCS         R2=A(LIST OF CMPYS OUT OF BALANCE)           
         CLI   0(R2),X'FF'                                                      
         BE    REPTOTFX                                                         
*                                  PRINT ERROR REPORT                           
         MVC   TITLE,SPACES                                                     
         MVC   TITLE+14(32),=C'LIST OF COMPANIES NOT IN BALANCE'                
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         ZAP   LINE,=P'99'                                                      
REPTOTF2 CLI   0(R2),X'FF'         TEST E-O-T                                   
         BE    REPTOTFX                                                         
         GOTO1 VHEXOUT,DMCB,0(R2),P,1                                           
         MVC   P+3(36),2(R2)                                                    
         MVC   WORK(19),=X'4040202020202020202020202020214B202060'              
         ED    WORK(19),38(R2)                                                  
         MVC   P+38(15),WORK+4                                                  
         GOTO1 VPRINTER                                                         
         BASR  RE,RF                                                            
         LA    R2,46(R2)                                                        
         B     REPTOTF2                                                         
REPTOTFX L     RE,SAVER7                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* REPORT PRINTING ROUTINES                                                      
***********************************************************************         
REPRINT  NTR1                                                                   
         MVC   REPRNT2+25(15),=C'TODAYS POSTINGS'                               
         MVC   REPRNT3+25(14),=C'TODAYS DELETES'                                
*&&US                                                                           
         MVC   REPRNT1+25(10),=C'GENERATION'                                    
         MVC   REPRNT4+25(10),=C'GENERATION'                                    
         LA    R2,REPRNT1+36                                                    
         EDIT  (P6,GENNIN),(4,0(R2))                                            
         LA    R2,396(R2)                                                       
         EDIT  (P6,GENOUT),(4,0(R2))                                            
*&&                                                                             
         BCTR  R4,0                                                             
         MH    R4,=H'128'                                                       
         A     R4,AREPACCS                                                      
         LA    R2,REPRNT1+45                                                    
         LA    R3,4                                                             
*                                                                               
REPRINT2 LA    R5,4                                                             
         ST    R2,FULL2                                                         
*                                                                               
REPRINT4 CP    0(8,R4),=P'0'                                                    
         BE    REPRINT6                                                         
         MVC   WORK(19),=X'4040202020202020202020202020214B202060'              
         ED    WORK(19),0(R4)                                                   
         MVC   0(15,R2),WORK+4                                                  
*                                                                               
REPRINT6 LA    R2,16(R2)                                                        
         LA    R4,8(R4)                                                         
         BCT   R5,REPRINT4                                                      
         L     R2,FULL2                                                         
         LA    R2,132(R2)                                                       
         BCT   R3,REPRINT2                                                      
         LA    R2,REPRNT1                                                       
         LA    R3,4                                                             
         CP    LINE,=P'57'                                                      
         BL    *+10                                                             
         ZAP   LINE,=P'99'                                                      
         L     RF,VPRINTER                                                      
         MVC   P,0(R2)                                                          
         MVC   0(132,R2),SPACES                                                 
         BASR  RE,RF                                                            
         LA    R2,132(R2)                                                       
         BCT   R3,*-18                                                          
         BASR  RE,RF                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* BUILD REPORT ACCUMULATORS                                                     
***********************************************************************         
REPADDS  NTR1                                                                   
         L     R2,AREPACCS                                                      
         MVC   0(128,R2),ZEROE8    CLEAR ACCUMS (LEVEL 1)                       
         AP    000(8,R2),ACFORWD                                                
         AP    024(8,R2),ACFORWD                                                
         AP    096(8,R2),ACFORWD                                                
         AP    120(8,R2),ACFORWD                                                
         AP    008(8,R2),ACBLDEB                                                
         AP    024(8,R2),ACBLDEB                                                
         AP    104(8,R2),ACBLDEB                                                
         AP    120(8,R2),ACBLDEB                                                
         AP    040(8,R2),ACDRPOS                                                
         AP    056(8,R2),ACDRPOS                                                
         SP    008(8,R2),ACDRPOS                                                
         SP    024(8,R2),ACDRPOS                                                
         AP    000(8,R2),ACDELCRD                                               
         AP    080(8,R2),ACDELCRD                                               
         SP    016(8,R2),ACDELCRD                                               
         SP    064(8,R2),ACDELCRD                                               
         AP    008(8,R2),ACDRDEL                                                
         AP    064(8,R2),ACDRDEL                                                
         SP    000(8,R2),ACDRDEL                                                
         SP    072(8,R2),ACDRDEL                                                
         SP    016(8,R2),ACBLCRD                                                
         SP    024(8,R2),ACBLCRD                                                
         SP    112(8,R2),ACBLCRD                                                
         SP    120(8,R2),ACBLCRD                                                
         AP    016(8,R2),ACCRPOS                                                
         AP    024(8,R2),ACCRPOS                                                
         SP    048(8,R2),ACCRPOS                                                
         SP    056(8,R2),ACCRPOS                                                
         LA    R3,128(R2)                                                       
         LA    R4,3                                                             
*                                                                               
REPADDS2 LA    R5,16               ADD TO LEVELS 2,3 AND 4                      
         AP    0(8,R3),0(8,R2)                                                  
         LA    R3,8(R3)                                                         
         LA    R2,8(R2)                                                         
         BCT   R5,*-14                                                          
         L     R2,AREPACCS                                                      
         BCT   R4,REPADDS2                                                      
         MVC   ACFORWD(56),ZEROE8                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINES FOR DIVIDER PAGES                                                    
***********************************************************************         
REPDIV   NTR1                                                                   
         L     R2,AIOAREA          EXTRACT VALUES FROM COMPANY RECORD           
         LA    R2,4(R2)                                                         
         USING ACKEYD,R2                                                        
         LA    R2,ACRECORD                                                      
         SR    R3,R3                                                            
*                                                                               
REPDIV2  CLI   0(R2),0                                                          
         BE    REPDIV4                                                          
         CLI   0(R2),X'10'                                                      
         BNE   REPDIV3                                                          
         USING ACCOMPD,R2                                                       
         MVC   DIVID,ACMPID        EXTRACT USER ID NUMBER                       
         NI    ACMPSTA3,X'FF'-X'10' ALWAYS TURN OFF BA=Y                        
         NI    ACMPSTA4,X'FF'-X'40' AND BBD=Y                                   
         B     REPDIV4                                                          
REPDIV3  IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     REPDIV2                                                          
*                                                                               
REPDIV4  XIT1                                                                   
*                                                                               
DIVIDE   NTR1                                                                   
         TM    SWLOGO,X'80'        LOGOS REQUESTED                              
         BZ    DIVIDEND            NO - EXIT                                    
         GOTO1 VGETLOGO,DMCB,DIVID,VLOGOC,VDATAMGR                              
         L     R2,VLOGOC                                                        
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'S'                                                    
         CLI   DIVBYTE,0                                                        
         BE    *+8                                                              
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 VLOGO,DMCB,(R2)                                                  
*                                                                               
DIVIDEND XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO HANDLE UPDATING AND DELETING OF RECORDS                           
***********************************************************************         
DELPROC  L     R2,AIOAREA                                                       
         LA    R2,4(R2)                                                         
         USING ACKEYD,R2                                                        
         CLI   PROCREC,BATCH       DELETE BATCH                                 
         BNE   DELPROC1                                                         
         DROP  R2                                                               
         USING ACBKEYD,R2                                                       
         CLC   ACBKDATE,ACUPDTE3   KEEP BATCHES FROM LAST UPDATE TO             
         BNLR  R7                  SEED FILE                                    
         B     DELPROCI                                                         
         DROP  R2                                                               
         USING ACKEYD,R2                                                        
DELPROC1 CLI   PROCREC,UNKNOWN     DELETE UNKNOWN                               
         BNE   *+18                                                             
         MVC   MSGAREA,=CL40'RECORD TYPE UNKNOWN (DELETED) KEY -'               
         MVI   DIE,C'N'                                                         
         B     BADREC                                                           
         CLI   PROCREC,TRAILER     SAVE HEADER/TRAILER                          
         BER   R7                                                               
         CLI   PROCREC,HEADER                                                   
         BER   R7                                                               
         CLI   PROCREC,ACCLOW      SPECIAL FOR LOW LEVEL ACCOUNTS               
         BNE   *+20                                                             
         MVI   ACCDELSW,0                                                       
         TM    ACSTATUS,X'80'                                                   
         BZ    *+16                                                             
         MVI   ACCDELSW,X'FF'                                                   
         TM    ACSTATUS,X'80'      ALL OTHERS CAN BE DELETED                    
         BO    DELPROCI                                                         
         CLI   ACCDELSW,0                                                       
         BE    *+20                                                             
         CLI   PROCREC,HISTORY     IF LOW LEVEL ACCOUNT WAS DELETED             
         BE    DELPROCI            TIDY-UP TRANSACTIONS/HISTORIES               
         CLI   PROCREC,TRNSACTN                                                 
         BE    DELPROCI                                                         
         LA    R3,ACRECORD                                                      
         SR    R4,R4                                                            
         MVI   BYTE,0              SET TO X'80' IF ELEMENT DELETED              
*                                                                               
DELPROC2 CLI   0(R3),0                                                          
         BE    DELPROCG                                                         
         CLI   0(R3),X'44'         TRANSACTION                                  
         BE    DELPROC4                                                         
         CLI   0(R3),X'11'         MEDIA                                        
         BE    DELPROC6                                                         
         CLI   0(R3),X'21'         NUMBER                                       
         BE    DELPROC8                                                         
         CLI   0(R3),X'41'         HOLD CHECK                                   
         BE    DELPROCA                                                         
         CLI   0(R3),X'31'         ARTIST FEE                                   
         BE    DELPROCC                                                         
         CLI   0(R3),X'54'         OFFICE CHECK NUMBER                          
         BE    DELPROCD                                                         
         CLI   0(R3),ACLIELEQ                                                   
         BE    DELPROCL            LIST TYPE ELEMENT FROM LIST REC.             
         B     DELPROCE                                                         
*                                                                               
DELPROC4 DS    0H                                                               
         B     DELPROC5                                                         
*                                                                               
DELPROC5 OC    ACDTPEEL,ACDTPEEL   TEST IF PEELED OFF                           
         BZR   R7                                                               
*        CLC   ACDTPEEL,PEELDATE                                                
*        BNH   DELPROCI                                                         
         BR    R7                                                               
*                                                                               
DELPROC6 DS    0H                                                               
         B     DELPROCE                                                         
*                                                                               
DELPROC8 DS    0H                                                               
         B     DELPROCE                                                         
*                                                                               
DELPROCA DS    0H                                                               
         B     DELPROCE                                                         
*                                                                               
DELPROCC DS    0H                                                               
         USING ACFEED,R3                                                        
         TM    ACFEST,X'30'                                                     
         BNO   DELPROCE                                                         
         TM    ACFEST,X'08'                                                     
         BO    *+12                                                             
         OI    ACFEST,X'08'                                                     
         B     DELPROCE                                                         
         MVI   0(R3),X'FF'                                                      
         OI    BYTE,X'80'          SET FOR DELETION                             
         B     DELPROCE                                                         
         SPACE 1                                                                
DELPROCD DS    0H                                                               
         USING ACOFFD,R3                                                        
         B     DELPROCE                                                         
*        TM    SWMODE,X'20'        TEST IF MODE=XXXXXX NEW SPECIFIED            
*        BO    DELPROCE                                                         
*        MVC   ACOFBEF,ACOFAFT                                                  
*        B     DELPROCE                                                         
*                                                                               
DELPROCE IC    R4,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R4                                                            
         B     DELPROC2                                                         
*                                                                               
DELPROCG CLI   BYTE,0              ANY ELEMENTS TO BE DELETED ?                 
         BER   R7                                                               
         GOTO1 VHELLO,DMCB,(C'D',=C'ACCOUNT'),(X'FF',(R2)),0                    
         L     R3,12(R1)                                                        
         LA    R3,4(R3)            BUILD NEW RECORD LENGTH                      
         L     R2,AIOAREA                                                       
         STH   R3,0(R2)                                                         
         ST    R7,SAVER7                                                        
         BAS   R7,RECTYPE                                                       
         L     R7,SAVER7                                                        
         BR    R7                                                               
*                                                                               
         USING ACLISTD,R3                                                       
DELPROCL TM    SWOUT,X'80'                                                      
         BZR   R7                  IGNORE LISTS IF NOT DISK O/P.                
         GOTO1 VDATCON,DMCB,(1,ACLIDATE),(2,WORK)                               
         CLC   WORK(2),ACTDATE     IF EXPIRY DATE IS NOT GREATER THAN           
         BNH   DELPROCI            ACTIVITY DATE, DELETE THE LIST.              
         BR    R7                                                               
*                                                                               
DELPROCI MVI   WRITE,X'FF'         SET RECORD FOR DELETION                      
         BR    R7                                                               
         DROP  R2                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO HANDLE BALANCE CHECKING                                           
***********************************************************************         
BALFRST  BAS   R1,OPNBAL           FIRST TIME                                   
         MVI   BALSWT,0            SET FIRST TIME SWITCH                        
         BR    R7                                                               
*                                                                               
BALPROC  ST    R7,SAVER7           PROCESS                                      
         L     R2,AIOAREA                                                       
         LA    R2,4(R2)                                                         
         USING ACKEYD,R2                                                        
         OC    BALLEDG,BALLEDG     SPECIAL CUL REPORT                           
         BZ    *+12                                                             
         CLC   ACKEYACC(3),BALLEDG CHECK KEY=CUL                                
         BNER  R7                                                               
         LA    R3,ACRECORD                                                      
         USING TRANSD,R3                                                        
         CLI   WRITE,X'FF'         IGNORE DELETED RECORDS                       
         BER   R7                                                               
         CLI   PROCREC,ACCLOW      ONLY LOW ACCOUNTS & TRANSACTIONS             
         BE    BALPROC6                                                         
         CLI   PROCREC,TRNSACTN                                                 
         BNER  R7                                                               
         CLI   ACDELSW,C'Y'        IF ACCOUNT IS DELETED                        
         BER   R7                  IGNORE TRANSACTIONS                          
         TM    ACSTATUS,X'80'      IF TRANSACTION DELETED                       
         BOR   R7                  SKIP IT                                      
         CLC   BALKEY,0(R2)        TRANSACTION FOR THIS ACCOUNT ?               
         BE    BALPROC2                                                         
         MVC   BALNAME,=CL36'**TRANSACTION OUT OF SEQUENCE**'                   
         ZAP   BALBDR,=P'0'                                                     
         MVC   BALBDR+8(24),BALBDR                                              
         BAS   R7,BALPROC3                                                      
         BAS   R7,BALPUT                                                        
         AP    PHBDCNT,=P'1'       DUMP OUT BAD RECORDS                         
         MVC   WIDTH,=H'60'                                                     
         BAS   RE,HEXREC                                                        
         L     R7,SAVER7                                                        
         BR    R7                                                               
*                                                                               
BALPROC2 OC    ACDTPEEL,ACDTPEEL   TEST IF A DELETED TRANSACTION                
         BNZR  R7                                                               
         TM    ACSTATUS,X'80'                                                   
         BOR   R7                                                               
         BAS   RE,BALPROC3                                                      
         BR    R7                                                               
*                                                                               
BALPROC3 DS    0H                                                               
*&&US                                                                           
         TM    SWHIST,X'80'                                                     
         BZ    BALPROC4                                                         
         CLC   ACKEYACC+1(2),=C'SN' FOR TALENT PERFORMERS                       
         BE    *+12                                                             
         CLI   ACKEYACC+1,C'T'     AND COMMERCIALS                              
         BNE   BALPROC4                                                         
         TM    TRNSSTAT,X'04'      BILLED EX-SYSTEM                             
         BNO   BALPROC4                                                         
         LA    R4,BALBDR                                                        
         TM    TRNSSTAT,X'80'      DR OR CR                                     
         BO    *+8                                                              
         LA    R4,8(R4)                                                         
         SP    0(8,R4),TRNSAMNT    SUBTRACT FROM BALANCE BUCKET                 
*                                                                               
         LA    R4,ACBLDEB                                                       
         TM    TRNSSTAT,X'80'      DR OR CR                                     
         BO    *+8                                                              
         LA    R4,8(R4)                                                         
         SP    0(8,R4),TRNSAMNT    AND FROM ACCOUNT ACCUM                       
*                                                                               
         LA    R4,ACDRHIS                                                       
         TM    TRNSSTAT,X'80'      DR OR CR                                     
         BO    *+8                                                              
         LA    R4,8(R4)                                                         
         AP    0(8,R4),TRNSAMNT    ADD TO FILE OVERALL ACCUM                    
         BR    R7                                                               
*&&                                                                             
BALPROC4 LA    R4,BALTDR                                                        
         TM    TRNSSTAT,X'80'      DR OR CR                                     
         BO    *+8                                                              
         LA    R4,8(R4)                                                         
         AP    0(8,R4),TRNSAMNT    ADD TO BUCKET                                
         BR    R7                                                               
*                                                                               
BALPROC6 MVI   ACDELSW,C'N'                                                     
         TM    ACSTATUS,X'80'      IGNORE DELETED RECORDS                       
         BNO   *+10                                                             
         MVI   ACDELSW,C'Y'                                                     
         BR    R7                                                               
         L     R3,ABALEL           PROCESS AN ACCOUNT                           
         USING ACBALD,R3                                                        
         LTR   R3,R3                                                            
         BZR   R7                                                               
         CLI   BALSWT,0            FIRST TIME ?                                 
         BNE   *+12                                                             
         MVI   BALSWT,1                                                         
         B     BALPROC8                                                         
         TM    SWBAL,X'01'         NO-OP TEST IF SPECIAL REPORT                 
         BO    *+14                                                             
         CLC   BALBDR(16),BALTDR   COMPARE ACCUMULATED TOTAL WITH BAL           
         BE    BALPROC8                                                         
         BAS   R7,BALPUT                                                        
*                                                                               
BALPROC8 ZAP   BALBDR,ACBLDR       SET-UP FOR NEW ACCOUNT                       
         ZAP   BALBCR,ACBLCR                                                    
         ZAP   BALTDR,=P'0'                                                     
         ZAP   BALTCR,=P'0'                                                     
         MVC   BALKEY,0(R2)                                                     
         MVC   BALNAME,RECNAME                                                  
         L     R7,SAVER7                                                        
         BR    R7                                                               
*                                                                               
BALLAST  ST    R7,SAVER7           LAST TIME                                    
         CLI   BALSWT,0            DID I DO ANYTHING                            
         BER   R7                                                               
         MVC   BALREC(2),=C'/*'    WRITE EOF TO WORK FILE                       
         BAS   R7,BALPUT                                                        
         BAS   R7,BALPNT           REPOSITION FOR READ                          
         BAS   R7,SETPRINT                                                      
         MVC   TITLE+20(20),=C'BALANCE CHECK REPORT'                            
         MVC   SUB1(20),=C'ACCOUNT KEY AND NAME'                                
         MVC   SUB2(20),=20C'-'                                                 
         MVC   SUB1+59(21),=C'---BALANCE ELEMENT---'                            
         MVC   SUB2+59(21),=C'DEBITS        CREDITS'                            
         MVC   SUB1+89(21),=C'----TRANSACTIONS-----'                            
         MVC   SUB2+89(21),=C'DEBITS        CREDITS'                            
*                                                                               
BALLAST2 BAS   R7,BALGET           READ A RECORD                                
         CLC   BALREC(2),=C'/*'    TEST FOR EOF                                 
         BNE   BALLAST3                                                         
         BAS   R1,CLSBAL                                                        
         L     R7,SAVER7                                                        
         BR    R7                                                               
BALLAST3 GOTO1 VHEXOUT,DMCB,BALKEY,P,1,=C'TOG'                                  
         MVC   P+3(14),BALKEY+1                                                 
         MVC   P+18(36),BALNAME                                                 
         LA    R2,P+51                                                          
         LA    R3,4                                                             
         LA    R4,BALBDR                                                        
*                                                                               
BALLAST4 CP    0(8,R4),=P'0'                                                    
         BE    BALLAST6                                                         
         MVC   WORK(19),=X'4040202020202020202020202020214B202060'              
         ED    WORK(19),0(R4)                                                   
         MVC   0(15,R2),WORK+4                                                  
*                                                                               
BALLAST6 LA    R2,15(R2)                                                        
         LA    R4,8(R4)                                                         
         BCT   R3,BALLAST4                                                      
         GOTO1 VPRINTER                                                         
         BASR  RE,RF               DOUBLE SPACE                                 
         B     BALLAST2                                                         
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* RECORD COUNTING ROUTINES                                                      
***********************************************************************         
CNTRPOC  SR    R2,R2               PROCESS                                      
         IC    R2,PROCREC                                                       
         CLI   PROCREC,SPECIALS                                                 
         BL    CNTRPOC2            BASIC RECORD COUNTING                        
         SH    R2,=AL2(SPECIALS)                                                
         SLL   R2,3                                                             
         A     R2,ACNTGEN                                                       
         B     CNTRPOC6                                                         
*                                                                               
CNTRPOC2 CLI   PROCREC,COMPANY     COMPANY RECORD COUNTING                      
         BNE   CNTRPOC4                                                         
         L     R3,ACMPLST                                                       
         L     R4,AIOAREA                                                       
         CLI   0(R3),X'FF'                                                      
         BE    *+8                                                              
         LA    R3,102(R3)                                                       
         MVI   102(R3),X'FF'                                                    
         ST    R3,ACMPLST                                                       
         MVC   0(2,R3),4(R4)       SAVE CMP CODE AND NAME IN TABLE              
         MVC   2(36,R3),RECNAME                                                 
         LA    R3,38(R3)                                                        
         ZAP   0(4,R3),=P'0'       CLEAR ACCUMS                                 
         MVC   4(60,R3),0(R3)                                                   
*                                                                               
CNTRPOC4 SLL   R2,3                                                             
         A     R2,ACMPLST                                                       
         LA    R2,38(R2)                                                        
*                                                                               
CNTRPOC6 AP    0(4,R2),=P'1'       UPDATE INPUT COUNT                           
         CLI   WRITE,X'FF'                                                      
         BNER  R7                                                               
         AP    4(4,R2),=P'1'       UPDATE PURGE COUNT                           
         BR    R7                                                               
*                                                                               
CNTLAST  ST    R7,SAVER7           LAST TIME                                    
         BAS   R7,SETPRINT                                                      
         MVC   TITLE+20(21),=C'LOGICAL RECORD COUNTS'                           
         MVC   MID1(40),=C'-----------------RECORD-----------------'            
         MVC   MID2(40),=C'RECORD TYPE        INPUT  PURGED  OUTPUT'            
         L     R2,ACNTGEN                                                       
         L     R3,=A(GENTAB)                                                    
         BAS   RE,CNTEDIT                                                       
         AP    LGINCNT,WORK(4)                                                  
         AP    LGDECNT,WORK+4(4)                                                
         L     R4,ACNTCMP                                                       
*                                                                               
CNTLAST2 CLI   0(R4),X'FF'                                                      
         BE    CNTLAST4                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(10),=C'COMPANY - '                                             
         GOTO1 VHEXOUT,DMCB,(R4),P+10,1,=C'TOG'                                 
         MVC   P+13(36),2(R4)                                                   
         LA    R2,38(R4)                                                        
         L     R3,=A(CMPTAB)                                                    
         GOTO1 VPRINTER                                                         
         BASR  RE,RF                                                            
         BAS   RE,CNTEDIT                                                       
         LA    R4,102(R4)                                                       
         AP    LGINCNT,WORK(4)                                                  
         AP    LGDECNT,WORK+4(4)                                                
         B     CNTLAST2                                                         
*                                                                               
CNTLAST4 LA    R2,LGINCNT                                                       
         LA    R3,FILTAB                                                        
         GOTO1 VPRINTER                                                         
         BAS   RE,CNTEDIT                                                       
         L     R7,SAVER7                                                        
         BR    R7                                                               
*                                                                               
CNTEDIT  NTR1                                                                   
         ZAP   WORK(4),=P'0'                                                    
         ZAP   WORK+4(4),=P'0'                                                  
         MVI   BYTE,0                                                           
*                                                                               
CNTEDIT2 CLI   0(R3),X'FF'                                                      
         BE    CNTEDITA                                                         
*                                                                               
CNTEDIT4 AP    WORK(4),0(4,R2)                                                  
         AP    WORK+4(4),4(4,R2)                                                
         CLC   0(8,R2),=2PL4'0'                                                 
         BE    CNTEDIT8                                                         
*                                                                               
CNTEDIT6 MVC   P(12),0(R3)                                                      
         ZAP   DUB(4),0(4,R2)                                                   
         SP    DUB(4),4(4,R2)                                                   
         UNPK  P+17(7),0(4,R2)                                                  
         OI    P+23,X'F0'                                                       
         UNPK  P+25(7),4(4,R2)                                                  
         OI    P+31,X'F0'                                                       
         UNPK  P+33(7),DUB(4)                                                   
         OI    P+39,X'F0'                                                       
         GOTO1 VPRINTER                                                         
         CLI   BYTE,0                                                           
         BNE   CNTEDITC                                                         
*                                                                               
CNTEDIT8 LA    R2,8(R2)                                                         
         LA    R3,12(R3)                                                        
         B     CNTEDIT2                                                         
*                                                                               
CNTEDITA CP    LGINCNT,WORK(4)                                                  
         BE    CNTEDITC                                                         
         LA    R2,WORK                                                          
         LA    R3,TOTTAB                                                        
         MVI   BYTE,1                                                           
         B     CNTEDIT6                                                         
*                                                                               
CNTEDITC GOTO1 VPRINTER                                                         
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO DO DITTO OF FILE                                                  
***********************************************************************         
DITFRST  ST    R7,SAVER7           FIRST TIME                                   
         BAS   R7,SETPRINT                                                      
         MVC   TITLE+18(23),=C'ACCOLADE RECORD DISPLAY'                         
         L     R7,SAVER7                                                        
         BR    R7                                                               
*                                                                               
DITPROC  DS    0H                  PROCESS                                      
         MVC   MSGAREA,=CL40'RECORD TYPE=             -'                        
         MVC   WIDTH,=H'100'                                                    
         L     R2,=A(RECTAB)                                                    
*                                                                               
DITPROC2 CLI   0(R2),X'FF'                                                      
         BE    DITPROC4                                                         
         CLC   0(1,R2),PROCREC                                                  
         BE    DITPROC4                                                         
         LA    R2,L'RECTAB(R2)                                                  
         B     DITPROC2                                                         
*                                                                               
DITPROC4 MVC   MSGAREA+12(12),1(R2)                                             
         CLI   0(R2),X'FF'                                                      
         BE    DITPROC6                                                         
         LA    RE,MSGAREA+12                                                    
         EDIT  (B1,0(R2)),(2,0(RE)),FILL=0                                      
DITPROC6 BAS   RE,HEXREC                                                        
         BR    R7                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO HANDLE WRITING OF TAPES                                           
***********************************************************************         
TOUFRST  BAS   R1,OPNTOUT          FIRST TIME                                   
         BR    R7                                                               
*                                                                               
TOUPROC  CLI   WRITE,X'FF'         PROCESS                                      
         BNE   *+16                                                             
         MVI   WRITE,0                                                          
         AP    PHDECNT,=P'1'                                                    
         BR    R7                                                               
         CLI   SWCOPY,0                                                         
         BE    TOUPROC2            NO COPY TAPE                                 
         L     R1,ATCOPY                                                        
         L     R0,AIOAREA                                                       
         PUT   (1),(0)                                                          
TOUPROC2 L     R1,ATOUT                                                         
         L     R0,AIOAREA                                                       
         PUT   (1),(0)                                                          
         AP    PHOUCNT,=P'1'                                                    
         BR    R7                                                               
*                                                                               
TOULAST  BAS   R1,CLSTOUT          LAST TIME                                    
         BR    R7                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO HANDLE WRITING OF DISK                                            
***********************************************************************         
DOUFRST  ST    R7,SAVER7           FIRST TIME                                   
         L     R5,AACCOUNT                                                      
         USING ISDTF,R5                                                         
         MVC   SVISPARE,ISSPARE    SAVE DTF SPARE                               
         MVI   BYTE,0                                                           
         TM    ISFOPEN,X'20'                                                    
         BZ    DOUFRST1            NOT YET OPENED                               
         MVI   BYTE,1              FLAG OPENING FOR SECOND TIME                 
         MVC   IOPARM1,=A(ISCLOSE)                                              
         GOTO1 VISDDS,IOPARM1      CLOSE IT                                     
DOUFRST1 MVC   IOPARM1,=A(ISOPEN)                                               
         GOTO1 VISDDS,IOPARM1                                                   
         CLI   BYTE,1              IF I'M OPENING FOR THE SECOND TIME           
         BNE   DOUFRS1B                                                         
*                                  SET UP FOR CACHE MEMORY                      
         LA    R1,ISFADCB          GET A(DCB ADDRESS)                           
         LA    R0,2                SET 2 FOR LOAD                               
         LNR   R0,R0               NEGATIVE VALUE TO SUPERVISOR                 
*&&US*&& SVC   247                                                              
DOUFRS1B MVC   IOPARM1,VISREAD                                                  
         TM    SWOVER,X'80'                                                     
         BOR   R7                                                               
         MVC   IOPARM2,AIOAREA2                                                 
         BAS   R7,DINPROC          READ GENERATION RECORD                       
         SP    PHINCNT,=P'1'       ADJUST INPUT RECORD COUNT                    
         XC    LASTKEY,LASTKEY                                                  
         MVC   MSGAREA,=CL40'GENERATION RECORD MISSING KEY -'                   
         OC    IOPARM3(2),IOPARM3                                               
         BZ    DOUFRST2                                                         
         GOTO1 VLOGIO,DMCB,1,=C'**ACCOLADE** GENERATION RECORD MISSING'         
         GOTO1 (RF),(R1),1,=C'**ACCOLADE** SEE IO/2 IN DUMP'                    
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
DOUFRST2 DS    0H                  GENERATION RECORD READ OK                    
         L     R2,AIOAREA2                                                      
         USING ACKEYD,R2                                                        
         LA    R2,ACRECORD                                                      
         USING ACCONTD,R2                                                       
         CP    GENOUT,ACCONCOT                                                  
         BNH   *+16                                                             
         OI    SWOVER,X'80'                                                     
         L     R7,SAVER7                                                        
         B     DOUFRST                                                          
         MVI   DUB,7                                                            
         B     ACCVERR                                                          
*                                                                               
DOUPROC  CLI   WRITE,X'FF'         PROCESS                                      
         BNE   *+16                                                             
         AP    PHDECNT,=P'1'                                                    
         MVI   WRITE,0                                                          
         BR    R7                                                               
         L     R2,AIOAREA                                                       
         CLC   4(42,R2),LASTKEY                                                 
         BNH   DOUPROC2                                                         
         MVC   LASTKEY,4(R2)                                                    
         CLI   PROCREC,COMPANY     TEST FOR SPECIAL RECORDS                     
         BL    *+12                                                             
         CLI   PROCREC,LEDGER      TEST LEDGER RECORD                           
         BNE   DOUPROCC                                                         
*                                  SET SPARE FOR CURRENT UNIT/LEDGER            
         LA    R1,SPARETAB                                                      
         MVC   DUB(2),SVISPARE     SET DEFAULT FROM DTF                         
DOUPROCA CLI   0(R1),X'FF'                                                      
         BE    DOUPROCB                                                         
         CLI   1(R1),0             SPECIAL RECORD TYPES                         
         BNE   DOUPRCA3                                                         
         CLC   LASTKEY(1),0(R1)                                                 
         BE    DOUPRCA5                                                         
         B     DOUPRCA4                                                         
DOUPRCA3 CLC   LASTKEY+1(2),0(R1)  MATCH UNIT/LEDGER TO TABLE                   
         BE    DOUPRCA5                                                         
DOUPRCA4 LA    R1,L'SPARETAB(R1)                                                
         B     DOUPROCA                                                         
DOUPRCA5 L     RF,AACCOUNT                                                      
         LH    RF,ISPDLN-ISDTF(RF)                                              
         SR    R0,R0                                                            
         ICM   R0,3,2(R1)          R0=SPARE PERCENTAGE                          
         MR    RE,R0               BLKSIZE*PERCENT/100=SPARE                    
         D     RE,=F'100'                                                       
         STH   RF,DUB                                                           
                                                                                
DOUPROCB L     RF,AACCOUNT                                                      
         MVC   ISSPARE-ISDTF(L'ISSPARE,RF),DUB                                  
DOUPROCC LA    R1,IOPARMA                                                       
         L     RF,VISLDDS                                                       
         BASR  RE,RF                                                            
         OC    IOPARMC(2),IOPARMC                                               
         BNZ   *+24                                                             
         AP    PHOUCNT,=P'1'                                                    
         CLI   PROCREC,TRAILER                                                  
         BNE   *+8                                                              
         MVI   EOFLOAD,C'Y'                                                     
         BR    R7                                                               
         MVC   MSGAREA,=CL40'UNRECOVERABLE DISK ERROR KEY -'                    
         B     BADREC                                                           
*                                                                               
DOUPROC2 GOTO1 VLOGIO,DMCB,1,=C'**ACCOLADE** RECORD OUT OF SEQUENCE'            
         AP    SEQERR,=P'1'                                                     
         MVC   MSGAREA,=CL40'RECORD OUT OF SEQUENCE KEY -'                      
         TM    SWSEQ,X'80'         DONT DIE IF SEQERR=IGNORE                    
         BZ    *+8                                                              
         MVI   DIE,C'N'                                                         
         B     BADREC                                                           
*                                                                               
DOULAST  CLI   EOFLOAD,C'Y'        LAST TIME                                    
         BE    DOULAST2                                                         
*                                                                               
         L     R2,AIOAREA          BUILD EOF RECORD                             
         LA    R2,4(R2)            STUPIDO !                                    
         USING ACKEYD,R2                                                        
         XC    ACKEYACC(255),ACKEYACC                                           
         MVI   ACKEYACC,X'FF'                                                   
         MVC   ACKEYACC+1(41),ACKEYACC                                          
         MVC   ACLENGTH,=H'64'                                                  
         MVC   ACRECORD(2),=X'040E'                                             
         LA    R1,IOPARMA                                                       
         L     RF,VISLDDS                                                       
         BASR  RE,RF                                                            
***********************************************************************         
*                                                                               
*        GOTO1 VLOGIO,DMCB,1,=C'**ACCOLADE** TRAILER RECORD MISSING'            
*        MVC   MSGAREA,=CL40'TRAILER RECORD MISSING LAST KEY WAS -'             
*        B     BADREC                                                           
*                                                                               
***********************************************************************         
         USING ISDTF,R5                                                         
DOULAST2 L     R5,AACCOUNT         A(DTF)                                       
         LA    R1,IOPARM1                                                       
         L     RF,VISDDS                                                        
                                                                                
         TM    ISFOPEN,ISFOOPN     Open?                                        
         BZ    DOULAST3            No, so don't close                           
         MVC   IOPARM1,=A(ISCLOSE) Close and reopen                             
         BASR  RE,RF                                                            
*                                                                               
DOULAST3 MVC   IOPARM1,=A(ISOPEN)                                               
         BASR  RE,RF                                                            
         MVC   IOPARM1,=A(ISREAD)  Need to to a read for last record            
         BASR  RE,RF                                                            
         MVC   IOPARM1,=A(ISFNDEOF)                                             
         BASR  RE,RF               Get and set EOF (ISOVLAST)                   
*                                                                               
         USING UTLD,R2                                                          
         L     R2,=V(UTL)                                                       
         MVC   SVTSYS,TSYS         Save                                         
         MVI   TSYS,X'E6'          Hard code to ACCT System                     
         GOTO1 VDATAMGR,DMCB,DMKEY,ACCHST,(3,ISDATA),(R5),0                     
         MVC   TSYS,SVTSYS         Restore                                      
         DROP  R2                                                               
                                                                                
         MVC   MSGAREA(6),=C'Avail='                                            
         MVC   MSGAREA+14(6),=C' Used='                                         
         MVC   MSGAREA+28(6),=C'Total='                                         
                                                                                
         LA    R4,MSGAREA                                                       
         EDIT  ISATRKS,(6,6(R4)),ALIGN=LEFT,ZERO=NOBLANK                        
         EDIT  ISUTRKS,(6,20(R4)),ALIGN=LEFT,ZERO=NOBLANK                       
         EDIT  ISTTRKS,(6,34(R4)),ALIGN=LEFT,ZERO=NOBLANK                       
         GOTO1 VLOGIO,DMCB,X'FF000001',(40,MSGAREA)                             
         MVC   P(40),MSGAREA                                                    
         GOTO1 VPRINTER                                                         
         BR    R7                                                               
         DROP  R5                                                               
                                                                                
SVTSYS   DS    X                   Save UTLs TSYS                               
ISDATA   DS    0F                                                               
ISATRKS  DS    F                   Available tracks                             
ISUTRKS  DS    F                   Used      tracks                             
ISTTRKS  DS    F                   Total     tracks                             
***********************************************************************         
* Tables                                                                        
***********************************************************************         
SVISPARE DS    XL2                 SAVED DTF ISSPARE VALUE                      
SPARETAB DS    0XL4                                                             
*&&US                                                                           
         DC    C'SJ',AL2(20)       SJ=20 PERCENT                                
         DC    C'SN',AL2(20)       SN=20 PERCENT                                
         DC    C'SP',AL2(20)       SP=20 PERCENT                                
         DC    C'SR',AL2(20)       SR=20 PERCENT                                
         DC    C'SS',AL2(20)       SS=20 PERCENT                                
         DC    C'SV',AL2(20)       SV=20 PERCENT                                
         DC    C'T6',AL2(20)       T6=20 PERCENT                                
         DC    C'1R',AL2(20)       1R=20 PERCENT                                
         DC    X'0B00',AL2(40)     0B=40 PERCENT                                
         DC    X'0C00',AL2(20)     0C=20 PERCENT                                
*&&                                                                             
*&&UK                                                                           
         DC    C'SF',AL2(20)       SF=20 PERCENT                                
         DC    C'SG',AL2(20)       SG=20 PERCENT                                
         DC    C'SJ',AL2(20)       SJ=20 PERCENT                                
         DC    C'SR',AL2(20)       SR=20 PERCENT                                
         DC    C'SV',AL2(20)       SV=20 PERCENT                                
         DC    C'SZ',AL2(20)       SZ=20 PERCENT                                
         DC    C'1C',AL2(20)       1C=20 PERCENT                                
         DC    X'0B00',AL2(40)     0B=40 PERCENT                                
         DC    X'0C00',AL2(20)     0C=20 PERCENT                                
*&&                                                                             
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT RECORD TOTALS & DO END LOGO FOR CONTROL. THIS CODE           
* IS EXECUTED AT THE END OF EVERY RUN.                                          
***********************************************************************         
RECTOTS  ST    R7,SAVER7           LAST TIME                                    
         BAS   R7,SETPRINT                                                      
         MVC   TITLE+19(22),=C'PHYSICAL RECORD COUNTS'                          
         MVC   MID1+2(29),=C'INPUT  PURGED  OUTPUT   ERROR'                     
         MVC   MID2+2(29),=C'-----  ------  ------   -----'                     
         GOTO1 VLOGIO,DMCB,1,(31,MID1)                                          
         GOTO1 (RF),(R1),1,(31,MID2)                                            
         LA    R2,P                                                             
         LA    R3,PHINCNT                                                       
         LA    R4,4                                                             
*                                                                               
RECTOTS2 UNPK  0(7,R2),0(4,R3)     EDIT PHYSICAL TOTALS                         
         OI    6(R2),X'F0'                                                      
         LA    R2,8(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,RECTOTS2                                                      
*                                                                               
         CP    PEELRECS,=P'0'                                                   
         BE    RECTOTS3                                                         
         GOTO1 VPRINTER                                                         
         EDIT  PEELRECS,(7,P)                                                   
         MVC   P+8(27),=CL27'PEELED TRANSACTIONS ON FILE'                       
         GOTO1 VPRINTER                                                         
*                                                                               
RECTOTS3 GOTO1 VLOGIO,DMCB,1,(31,P)                                             
         GOTO1 VPRINTER                                                         
         L     R7,SAVER7                                                        
         MVI   DIVBYTE,1                                                        
         TM    SWREP,X'80'         IS REPORT OPTION ACTIVE ?                    
         BZ    *+8                                                              
         BAS   RE,DIVIDE           YES-DO END LOGO                              
         CP    PHBDCNT,=P'0'                                                    
         BER   R7                                                               
         GOTO1 VLOGIO,DMCB,1,=C'**ACCOLADE** WARNING BAD RECORDS FOUND'         
         BR    R7                                                               
***********************************************************************         
* ROUTINE TO CLEAR DOWN PRINT DSECT                                             
***********************************************************************         
SETPRINT MVC   TITLE,SPACES                                                     
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID3,SPACES                                                      
         MVC   MID4,SPACES                                                      
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   SUB3,SPACES                                                      
         MVC   P,SPACES                                                         
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         BR    R7                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT ERROR MESSAGE AND KEY OF RECORD WHEN AN I/O ERROR            
* OCCURS.                                                                       
***********************************************************************         
BADREC   MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         AP    PHBDCNT,=P'1'                                                    
         MVC   WIDTH,=H'60'                                                     
         BAS   RE,HEXREC                                                        
         CLI   DIE,C'N'            PROGRAM CHECK REQUESTED ?                    
         BE    *+10                                                             
         MVI   DIE,C'F'            SET FORCED KILL MODE                         
         DC    H'0'                                                             
         MVI   DIE,C'Y'                                                         
         B     PROCESS             IGNORE THIS RECORD                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT OUT KEY AND DATA OF RECORD IN HEX FORMAT                     
***********************************************************************         
HEXREC   NTR1                                                                   
         MVC   P(40),MSGAREA                                                    
         LA    R4,P+40                                                          
         CLI   0(R4),C' '                                                       
         BNE   *+10                                                             
         BCTR  R4,0                                                             
         B     *-10                                                             
         LA    R4,2(R4)                                                         
         L     R2,AIOAREA          R2=A(RECORD)                                 
         LH    R3,0(R2)                                                         
         LA    R2,4(R2)                                                         
         SH    R3,=H'4'            R3=L'RECORD                                  
         CH    R3,RECMAX                                                        
         BNH   *+8                                                              
         LH    R3,RECMAX                                                        
         L     R5,AIOAREA2         IOAREA2 USED AS WORK                         
         GOTO1 VHEXOUT,DUB,(R2),(R5),(R3),=C'SEP'                               
         CP    LINE,=P'57'                                                      
         BL    *+10                                                             
         ZAP   LINE,=P'99'                                                      
         MVC   0(49,R4),0(R2)                                                   
         XC    DUB(12),DUB         TRANSLATE CHARACTERS                         
         GOTO1 VDUMPOUT,DUB,(49,(R4))                                           
         GOTO1 VPRINTER            PRINT CHARACTERS                             
         LA    R7,0(R5,R3)         R5=A(ZONES),R7=A(DIGITS)                     
         MVC   0(49,R4),0(R5)                                                   
         BASR  RE,RF               PRINT ZONES                                  
         MVC   0(49,R4),0(R7)                                                   
         BASR  RE,RF               PRINT DIGITS                                 
         BASR  RE,RF                                                            
         LA    R2,49(R2)                                                        
         LA    R5,49(R5)                                                        
         LA    R7,49(R7)                                                        
         SH    R3,=H'49'                                                        
         BM    HEXREC4                                                          
         BZ    HEXREC4                                                          
         LH    R0,WIDTH            R0=W'PRINT LINE                              
         L     RF,VPRINTER                                                      
*                                                                               
HEXREC2  CR    R3,R0               PRINT REST OF RECORD                         
         BH    *+6                                                              
         LR    R0,R3                                                            
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         CP    LINE,=P'57'                                                      
         BL    *+10                                                             
         ZAP   LINE,=P'99'                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R2)                                                    
         ST    R1,FULL                                                          
*                                  TRANSLATE CHARACTERS                         
         GOTO1 VDUMPOUT,DUB,((R0),(R4))                                         
         GOTO1 VPRINTER            PRINT CHARACTERS                             
         L     R1,FULL                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R5)                                                    
         BASR  RE,RF               PRINT ZONES                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R7)                                                    
         BASR  RE,RF               PRINT DIGITS                                 
         BASR  RE,RF               SPACE LINE                                   
         CR    R3,R0                                                            
         BE    HEXREC4                                                          
         AR    R2,R0               BUMP INPUT POINTERS                          
         AR    R5,R0                                                            
         AR    R7,R0                                                            
         SR    R3,R0                                                            
         B     HEXREC2                                                          
*                                                                               
HEXREC4  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT OUT LASTKEY                                                  
***********************************************************************         
LASTKP   NTR1                                                                   
         MVC   P(11),=C'LAST KEY - '                                            
         LA    R4,P+29                                                          
         LA    R2,LASTKEY          R2=A(LASTKEY)                                
         LA    R3,L'LASTKEY        R3=LENGTH                                    
         L     R5,AIOAREA2         IOAREA2 USED AS WORK                         
         GOTO1 VHEXOUT,DUB,(R2),(R5),(R3),=C'SEP'                               
         CP    LINE,=P'57'                                                      
         BL    *+10                                                             
         ZAP   LINE,=P'99'                                                      
         MVC   0(49,R4),0(R2)                                                   
         XC    DUB(12),DUB         TRANSLATE CHARACTERS                         
         GOTO1 VDUMPOUT,DUB,(49,(R4))                                           
         GOTO1 VPRINTER            PRINT CHARACTERS                             
         LA    R7,0(R5,R3)         R5=A(ZONES),R7=A(DIGITS)                     
         MVC   0(49,R4),0(R5)                                                   
         BASR  RE,RF               PRINT ZONES                                  
         MVC   0(49,R4),0(R7)                                                   
         BASR  RE,RF               PRINT DIGITS                                 
         BASR  RE,RF                                                            
         B     HEXREC4                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO HANDLE FILE OPEN AND CLOSE                                        
***********************************************************************         
OPNTINT  STM   RE,R2,SAVEIOR                                                    
         L     R2,ATINT                                                         
         OPEN  ((2),INPUT)                                                      
         LM    RE,R2,SAVEIOR                                                    
         BR    R1                                                               
*                                                                               
CLSTINT  STM   RE,R2,SAVEIOR                                                    
         L     R2,ATINT                                                         
         CLOSE ((2))                                                            
         LM    RE,R2,SAVEIOR                                                    
         BR    R1                                                               
*                                                                               
OPNTOUT  STM   RE,R2,SAVEIOR                                                    
         L     R2,ATOUT                                                         
         OPEN  ((2),OUTPUT)                                                     
         CLI   SWCOPY,0                                                         
         BE    OPNTOUT2            NO COPY TAPE                                 
         L     R2,ATCOPY                                                        
         OPEN  ((2),OUTPUT)                                                     
OPNTOUT2 LM    RE,R2,SAVEIOR                                                    
         BR    R1                                                               
*                                                                               
CLSTOUT  STM   RE,R2,SAVEIOR                                                    
         L     R2,ATOUT                                                         
         CLOSE ((2))                                                            
         CLI   SWCOPY,0                                                         
         BE    CLSTOUT2            NO COPY TAPE                                 
         L     R2,ATCOPY                                                        
         CLOSE ((2))                                                            
CLSTOUT2 DC    0H'0'                                                            
CLSTOUT3 GOTO1 VLOGIO,DMCB,1,=C'OUTPUT TAPES SUCCESSFULLY CLOSED - RESPX        
               OND - OK'                                                        
         GOTO1 (RF),(R1),0,(2,MSGAREA)                                          
         CLC   MSGAREA(2),=C'OK'                                                
         BE    CLSTOUT4                                                         
         B     CLSTOUT3                                                         
CLSTOUT4 LM    RE,R2,SAVEIOR                                                    
         BR    R1                                                               
*                                                                               
OPNBAL   STM   RE,R2,SAVEIOR                                                    
         L     R2,ASORTIN1                                                      
         OPEN  ((R2),(OUTPUT))                                                  
         LM    RE,R2,SAVEIOR                                                    
         BR    R1                                                               
*                                                                               
CLSBAL   STM   RE,R2,SAVEIOR                                                    
         L     R2,ASORTIN1                                                      
         CLOSE ((R2))                                                           
         LM    RE,R2,SAVEIOR                                                    
         BR    R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO HANDLE I/0 TO WORK FILE                                           
***********************************************************************         
BALGET   STM   RE,R2,SAVEIOR       READ FROM WORKFILE                           
         L     R2,ASORTIN1                                                      
         GET   (R2),BALREC                                                      
         BR    R7                                                               
*                                                                               
BALPUT   STM   RE,R2,SAVEIOR       PUT TO WORK FILE                             
         L     R2,ASORTIN1                                                      
         PUT   (R2),BALREC                                                      
         LM    RE,R2,SAVEIOR                                                    
         BR    R7                                                               
*                                                                               
BALPNT   STM   RE,R2,SAVEIOR       POINT TO START OF WORK FILE                  
         L     R2,ASORTIN1                                                      
         CLOSE ((R2))                                                           
         L     R2,ASORTIN1                                                      
         OPEN  ((R2),(INPUT))                                                   
         LM    RE,R2,SAVEIOR                                                    
         BR    R7                                                               
         EJECT                                                                  
***********************************************************************         
* THIS TABLE CONTAINS ALL VALID REQUESTABLE OPTIONS                             
* BYTE 0     - L'KEYWORD                                                        
* BYTE 1-10  - KEYWORD                                                          
* BYTE 11-13 - A(VALIDATION ROUTINE)                                            
***********************************************************************         
         DC    CL8'**TBLS**'                                                    
PARMTAB  DS    0CL14                                                            
         DC    AL1(5),CL10'INPUT     ',AL3(ACCIN)                               
         DC    AL1(6),CL10'OUTPUT    ',AL3(ACCOUT)                              
         DC    AL1(4),CL10'COPY      ',AL3(ACCOPY)                              
         DC    AL1(4),CL10'MODE      ',AL3(ACCMODE)                             
         DC    AL1(8),CL10'OVERRIDE  ',AL3(ACCORID)                             
         DC    AL1(5),CL10'GENUP     ',AL3(ACCGEN)                              
         DC    AL1(6),CL10'REPORT    ',AL3(ACCREP)                              
         DC    AL1(6),CL10'DELETE    ',AL3(ACCDET)                              
         DC    AL1(4),CL10'LOAD      ',AL3(ACCLOAD)                             
         DC    AL1(5),CL10'COUNT     ',AL3(ACCCNT)                              
         DC    AL1(8),CL10'RECCHECK  ',AL3(ACCREC)                              
         DC    AL1(8),CL10'BALCHECK  ',AL3(ACCBAL)                              
         DC    AL1(4),CL10'FILE      ',AL3(ACCFILE)                             
         DC    AL1(5),CL10'START     ',AL3(ACCSTRT)                             
         DC    AL1(3),CL10'END       ',AL3(ACCEND)                              
         DC    AL1(5),CL10'DITTO     ',AL3(ACCDIT)                              
         DC    AL1(7),CL10'CONTROL   ',AL3(ACCONID)                             
         DC    AL1(4),CL10'LOGO      ',AL3(ACCLOGO)                             
         DC    AL1(9),CL10'SEQERR=IGN',AL3(ACCSEQS)                             
         DC    AL1(4),CL10'DATE      ',AL3(ACCDATE)                             
         DC    AL1(4),CL10'PEEL      ',AL3(ACCPEEL)                             
         DC    AL1(7),CL10'HISTORY   ',AL3(ACCHIST)                             
         DC    AL1(5),CL10'TRACK     ',AL3(ACCTRK)                              
         DC    AL1(5),CL10'DDSIO     ',AL3(ACCDDSIO)                            
         DC    AL1(6),CL10'DEVICE    ',AL3(ACCDVICE)                            
         DC    X'FF'                                                            
         SPACE 2                                                                
DMPLIST  DC    10F'0'                                                           
         EJECT                                                                  
***********************************************************************         
* THIS TABLE IS SET BY THE VALIDATION ROUTINES WHEN AN OPTION IS                
* REQUESTED (BYTE 0=X'80' OR X'40').                                            
* BYTE 0     - OPTION SETTING (X'00'=OPTION NOT REQUESTED)                      
* BYTE 1-3   - A(X'80' SETTING HOOKS)                                           
* BYTE 4-6   - A(X'40' SETTING HOOKS)                                           
***********************************************************************         
SWTTAB   DS    0CL7                                                             
*                                                                               
SWIN     DC    X'00',AL3(HOOKDIN),AL3(HOOKTIN)                                  
*                                                                               
SWCHK    DC    X'80',AL3(HOOKGEN),AL3(0)                                        
*                                                                               
SWREC    DC    X'00',AL3(HOOKREC),AL3(0)                                        
*                                                                               
SWREP    DC    X'00',AL3(HOOKREP),AL3(0)                                        
*                                                                               
SWDEL    DC    X'00',AL3(HOOKDEL),AL3(0)                                        
*                                                                               
SWBAL    DC    X'00',AL3(HOOKBAL),AL3(0)                                        
*                                                                               
SWLOAD   DC    X'00',AL3(HOOKLOD),AL3(0)                                        
*                                                                               
SWCNT    DC    X'00',AL3(HOOKCNT),AL3(0)                                        
*                                                                               
SWDIT    DC    X'00',AL3(HOOKDIT),AL3(0)                                        
*                                                                               
SWOUT    DC    X'00',AL3(HOOKOUD),AL3(HOOKOUT)                                  
*                                                                               
SWCON    DC    X'80',AL3(HOOKCON),AL3(0)                                        
*                                                                               
         DC    X'FF'                                                            
*                                  PASSIVE OPTIONS                              
SWOVER   DC    X'00'                                                            
SWGEN    DC    X'00'                                                            
SWMODE   DC    X'00'                                                            
SWSTRT   DC    X'00'                                                            
SWEND    DC    X'00'                                                            
SWLOGO   DC    X'00'                                                            
SWSEQ    DC    X'00'                                                            
SWCOPY   DC    X'00'                                                            
SWHIST   DC    X'00'                                                            
         SPACE 1                                                                
ACDELSW  DS    CL1                                                              
         EJECT                                                                  
***********************************************************************         
* THIS TABLE CONTAINS A LIST OF EXECUTABLE CODE ADDRESSED BY SWTTAB.            
* CODE FROM THIS TABLE IS BUILT INTO AN EXECUTABLE STRING.                      
* BYTE 0     * L'FIRST TIME HOOK INSTRUCTIONS                                   
* BYTE 1     * L'PROCESSING HOOK INSTRUCTIONS                                   
* BYTE 2     * L'LAST TIME HOOK INSTRUCTIONS                                    
* BYTE 4-N   * EXECUTABLE INSTRUCTIONS                                          
***********************************************************************         
         CNOP  0,4                                                              
HOOKDIN  DC    AL1(4,4,4,0)        INPUT=DISK     *                             
         BAS   R7,DINFRST                         *                             
         BAS   R7,DINPROC                         *                             
         BAS   R7,DINLAST                         *                             
*                                                                               
HOOKTIN  DC    AL1(4,4,4,0)        INPUT=TAPE     *                             
         BAS   R7,TINFRST                         *                             
         BAS   R7,TINPROC                         *                             
         BAS   R7,TINLAST                         *                             
*                                                                               
HOOKGEN  DC    AL1(8,4,0,0)        CONTROL HOOKS  *                             
         BAS   R7,RECTYPE                         *                             
         BAS   R7,GENCHCK                         *                             
         BAS   R7,RECTYPE                         *                             
*                                                                               
HOOKREC  DC    AL1(0,4,0,0)        RECCHECK=YES   *                             
         BAS   R7,RECPROC                         *                             
*                                                                               
HOOKREP  DC    AL1(4,4,4,0)        REPORT=YES     *                             
         BAS   R7,REPFRST                         *                             
         BAS   R7,REPPROC                         *                             
         BAS   R7,REPLAST                         *                             
*                                                                               
HOOKDEL  DC    AL1(0,4,0,0)        DELETE=YES     *                             
         BAS   R7,DELPROC                         *                             
*                                                                               
HOOKBAL  DC    AL1(4,4,4,0)        BALCHECK=YES   *                             
         BAS   R7,BALFRST                         *                             
         BAS   R7,BALPROC                         *                             
         BAS   R7,BALLAST                         *                             
*                                                                               
HOOKLOD  DC    AL1(0,8,12,0)       LOAD=XXXXXXXX  *                             
         LR    R1,R6                              *                             
         L     RF,APHASE                          *                             
         BASR  RE,RF                              *                             
         LR    R1,R6                              *                             
         L     RF,APHASE                          *                             
         MVI   OVSWITCH,X'FF'                     *                             
         BASR  RE,RF                              *                             
*                                                                               
HOOKCNT  DC    AL1(0,4,4,0)        COUNT=YES      *                             
         BAS   R7,CNTRPOC                         *                             
         BAS   R7,CNTLAST                         *                             
*                                                                               
HOOKDIT  DC    AL1(4,4,0,0)        DITTO=YES      *                             
         BAS   R7,DITFRST                         *                             
         BAS   R7,DITPROC                         *                             
*                                                                               
HOOKOUD  DC    AL1(4,4,4,0)        OUTPUT=DISK    *                             
         BAS   R7,DOUFRST                         *                             
         BAS   R7,DOUPROC                         *                             
         BAS   R7,DOULAST                         *                             
*                                                                               
HOOKOUT  DC    AL1(4,4,4,0)        OUTPUT=TAPE    *                             
         BAS   R7,TOUFRST                         *                             
         BAS   R7,TOUPROC                         *                             
         BAS   R7,TOULAST                         *                             
*                                                                               
HOOKCON  DC    AL1(4,4,8,0)        CONTROL HOOKS  *                             
         B     PROCESS+4                          *                             
         B     PROCESS                            *                             
         BAS   R7,RECTOTS                         *                             
         B     EOJ                                *                             
*                                                                               
EOJ      DC    0H'0'                                                            
         L     R5,AACCOUNT                                                      
         USING ISDTF,R5                                                         
         TM    ISFOPEN,X'20'                                                    
         BZ    EOJ1                OPENED                                       
         MVC   IOPARM1,=A(ISCLOSE)                                              
         GOTO1 VISDDS,IOPARM1      CLOSE IT                                     
         TM    ISFOPEN,X'20'                                                    
         BZ    EOJ1                OPENED                                       
         DC    H'0'                OH SHIT IT'S STILL OPENED                    
         DROP  R5                                                               
                                                                                
EOJ1     DC    0H'0'                                                            
         GOTO1 =V(PPGPRINT),DMCB,=C'CLOSE'                                      
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* LTORG +                                                                       
***********************************************************************         
ACCHST   DC    CL8'ACCHST'                                                      
DMKEY    DC    CL8'DMKEY'                                                       
                                                                                
         DC    CL8'**LTRG**'                                                    
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              ACCOLADE WORKING STORAGE                                         
***********************************************************************         
         DS    0D                                                               
         DC    CL8'**FRST**'                                                    
FIRST    DC    60X'00'                                                          
         DC    CL8'**PROC**'                                                    
*=================== WHOEVER IS IN HERE NEXT ==========================         
* REMOVE THE NEXT STATEMENT, AND REPLACE IT WITH THE TWO STATEMENTS             
* THAT FOLLOW. OTHERWISE YOU WON'T BE ABLE TO LINK.                             
PROCESS  DC    60X'00'                                                          
**PROCESS  DC    4X'00'              WAS 60X'00'                                
**         DC    56X'00'             WORKAROUND FOR POST-ASSEMBLY CHECK         
*======================================================================         
         DC    CL8'**LAST**'                                                    
LAST     DC    60X'00'                                                          
*                                                                               
         DC    CL8'**PHSE**'                                                    
PHASE    DC    CL8' '                                                           
         DC    CL8'**ACWS**'                                                    
AWORK    DC    A(ACCWORK)                                                       
SAVER7   DS    F                                                                
SAVEIOR  DS    5F                                                               
NEXTHOOK DC    A(FIRST)                                                         
         DC    A(PROCESS)                                                       
         DC    A(LAST)                                                          
RECMIN   DC    H'49'                                                            
RECMAX   DC    H'1000'                                                          
LASTKEY  DC    XL42'00'                                                         
ACCDELSW DC    X'00'                                                            
ACTDATE  DS    XL2                                                              
ACUPDTE3 DS    CL3                                                              
DIE      DC    C'Y'                                                             
EOFLOAD  DC    C'N'                                                             
TINSW    DC    C'N'                                                             
         DS    CL3                                                              
WIDTH    DC    H'90'                                                            
SEQERR   DC    PL4'0'                                                           
PEELRECS DC    PL4'0'              PEELED TRANSACTIONS                          
         EJECT                                                                  
***********************************************************************         
*              WORKING STORAGE FOR BALANCE CHECKING ROUTINES                    
***********************************************************************         
         DC    CL8'**BLRC**'                                                    
BALSWT   DS    C                   FIRST TIME SWITCH                            
BALLEDG  DC    XL3'00'                                                          
BALREC   DS    0CL84                                                            
BALKEY   DS    CL15                ACCOUNT KEY                                  
BALNAME  DS    CL36                ACCOUNT NAME                                 
*                                  ACCOUNT DR/CR                                
BALBDR   DS    PL8                                                              
BALBCR   DS    PL8                                                              
*                                  TRANSACTION DR/CR                            
BALTDR   DS    PL8                                                              
BALTCR   DS    PL8                                                              
         DS    C                   SPARE                                        
         EJECT                                                                  
***********************************************************************         
*              WORKING STORAGE REQUIRED FOR REPORT ROUTINES                     
***********************************************************************         
         DC    CL8'**REPT**'                                                    
ZEROE8   DC    16PL8'0'                                                         
ACFORWD  DC    PL8'0'                                                           
ACBLDEB  DC    PL8'0'                                                           
ACBLCRD  DC    PL8'0'                                                           
ACCRPOS  DC    PL8'0'                                                           
ACDRPOS  DC    PL8'0'                                                           
ACDELCRD DC    PL8'0'                                                           
ACDRDEL  DC    PL8'0'                                                           
*&&US                                                                           
ACDRHIS  DC    PL8'0'              TALENT HISTORY TOTALS                        
ACCRHIS  DC    PL8'0'                                                           
*&&                                                                             
REPPREV  DC    X'00'                                                            
REPTHIS  DC    X'00'                                                            
*                                                                               
AREPACCS DC    A(REPACCS)                                                       
AERRACCS DC    A(ERRACCS)                                                       
AERRNEXT DC    A(ERRACCS)                                                       
*                                                                               
REPCOMP  DC    CL38' '                                                          
REPUNIT  DC    CL38' '                                                          
REPLEDG  DC    CL38' '                                                          
*                                                                               
REPSWTCH DC    X'00'                                                            
*                                                                               
REPRNT1  DC    CL132' '                                                         
REPRNT2  DC    CL132' '                                                         
REPRNT3  DC    CL132' '                                                         
REPRNT4  DC    CL132' '                                                         
*                                                                               
DIVBYTE  DS    C                                                                
DIVID    DC    H'0'                                                             
CONID    DC    H'5'                                                             
*                                                                               
REPACTNS DS    0CL5                                                             
         DC    X'0303',AL3(REPACT1)                                             
         DC    X'0203',AL3(REPACT2)                                             
         DC    X'0202',AL3(REPACT3)                                             
         DC    X'0103',AL3(REPACT4)                                             
         DC    X'0102',AL3(REPACT5)                                             
         DC    X'0101',AL3(REPACT6)                                             
         DC    X'0104',AL3(REPACT7)                                             
         DC    X'0204',AL3(REPACT7)                                             
         DC    X'0304',AL3(REPACT7)                                             
         DC    X'0302',AL3(REPPROCF)                                            
         DC    X'0301',AL3(REPPROCF)                                            
         DC    X'0201',AL3(REPPROCF)                                            
         DC    X'0003',AL3(REPPROCF)                                            
REPACTNX DC    X'FFFF',AL3(0)                                                   
         EJECT                                                                  
***********************************************************************         
*              ERROR LIST                                                       
***********************************************************************         
         DC    CL8'**ERRS**'                                                    
ERRTAB   DS    0CL40                                                            
         DC    CL40'CANCELLED DUE TO OPERATOR OPTION'                           
         DC    CL40'MISSING OR INVALID INPUT PARAMETER'                         
         DC    CL40'MISSING OR INVALID OUTPUT PARAMETER'                        
         DC    CL40'OUTPUT SPECIFIED FOR MODE=REPORT'                           
         DC    CL40'MISSING OR INVALID MODE PARAMETER'                          
         DC    CL40'INCOMPATIBLE INPUT/OUTPUT PARAMETER'                        
         DC    CL40'START=KEY ONLY VALID IF INPUT=DISK'                         
         DC    CL40'LOAD NOT AUTHORIZED (GENERATION)'                           
         EJECT                                                                  
***********************************************************************         
*              WORKING STORAGE FOR LOGICAL COUNTING ROUTINES                    
***********************************************************************         
         DC    CL8'**LGCT**'                                                    
LGINCNT  DC    PL4'0'                                                           
LGDECNT  DC    PL4'0'                                                           
ACNTGEN  DC    A(CNTGEN)                                                        
ACNTCMP  DC    A(CNTCMP)                                                        
ACMPLST  DC    A(CNTCMP)                                                        
*                                                                               
*                                                                               
TOTTAB   DS    0CL12                                                            
         DC    CL12' **TOTALS**'                                                
         DC    X'FF'                                                            
*                                                                               
FILTAB   DS    0CL12                                                            
         DC    CL12'FILE TOTALS'                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              TABLE OF RECORD TYPES                                            
***********************************************************************         
         ENTRY GENTAB                                                           
GENTAB   DS    0CL12                                                            
         DC    CL12'UNKNOWN ? '                                                 
         DC    CL12'HEADER'                                                     
         DC    CL12'TRAILER'                                                    
         DC    CL12'MEDIA'                                                      
         DC    CL12'ANALYSIS'                                                   
         DC    CL12'BATCH'                                                      
         DC    CL12'COMMENT'                                                    
         DC    CL12'ERROR'                                                      
         DC    CL12'PROD. ORDER'                                                
         DC    CL12'BUDGET'                                                     
         DC    CL12'LIST'                                                       
         DC    CL12'FEEPAK CTRL'                                                
         DC    CL12'FEEPAK AREA'                                                
         DC    CL12'FEEPAK PCNT'                                                
         DC    CL12'CAST'                                                       
         DC    CL12'CYCLE'                                                      
         DC    CL12'GUARANTEE'                                                  
         DC    CL12'AGENT PTR'                                                  
         DC    CL12'AGENT'                                                      
         DC    CL12'GF ESTIMATES'                                               
         DC    CL12'MISC WITHHLD'                                               
         DC    CL12'W4 ALPHA'                                                   
         DC    CL12'BILL RATES'                                                 
         DC    CL12'TAL ESTIMATE'                                               
         DC    CL12'GEN PROD'                                                   
         DC    CL12'GEN ACC'                                                    
         DC    CL12'OFFICE'                                                     
         DC    CL12'MEDIA INTERF'                                               
         DC    CL12'TAX RULES'                                                  
         DC    CL12'BILL SOURCE'                                                
         DC    CL12'BILL TRNSFR'                                                
         DC    CL12'COST ACCOUNT'                                               
         DC    CL12'REQUEST'                                                    
         DC    CL12'CHK AUTH'                                                   
         DC    X'FF'                                                            
*                                                                               
         ENTRY CMPTAB                                                           
CMPTAB   DS    0CL12                                                            
         DC    CL12'COMPANY'                                                    
         DC    CL12'UNIT'                                                       
         DC    CL12'LEDGER'                                                     
         DC    CL12'HIGH ACCOUNT'                                               
         DC    CL12'LOW ACCOUNT'                                                
         DC    CL12'HISTORY'                                                    
         DC    CL12'TRANSACTION'                                                
         DC    CL12'OTHERS'                                                     
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              WORKING STORAGE FOR DITTO ROUTINES                               
***********************************************************************         
         ENTRY RECTAB                                                           
         DC    CL8'**RTYP**'                                                    
RECTAB   DS    0CL13                                                            
         DC    AL1(COMPANY),CL12'   COMPANY'                                    
         DC    AL1(UNIT),CL12'   UNIT'                                          
         DC    AL1(LEDGER),CL12'   LEDGER'                                      
         DC    AL1(ACCHIGH),CL12'   ACCHIGH'                                    
         DC    AL1(ACCLOW),CL12'   ACCLOW'                                      
         DC    AL1(HISTORY),CL12'   HISTORY'                                    
         DC    AL1(TRNSACTN),CL12'   TRNSACTN'                                  
         DC    AL1(OTHERS),CL12'   OTHER'                                       
         DC    4CL(L'RECTAB)' '         SPARE FOR NON-SPECIALS                  
RECSPEC  EQU   *                        START OF SPECIAL RECORD TYPES           
         DC    AL1(UNKNOWN),CL12'   UNKNOWN'                                    
         DC    AL1(HEADER),CL12'   HEADER'                                      
         DC    AL1(TRAILER),CL12'   TRAILER'                                    
         DC    AL1(MEDIA),CL12'   MEDIA'                                        
         DC    AL1(ANALYSIS),CL12'   ANALYSIS'                                  
         DC    AL1(BATCH),CL12'   BATCH'                                        
         DC    AL1(COMMENT),CL12'   COMMENT'                                    
         DC    AL1(ERROR),CL12'   ERROR'                                        
         DC    AL1(PRODORD),CL12'   PRD ORDER'                                  
         DC    AL1(BUDGET),CL12'   BUDGET'                                      
         DC    AL1(LIST),CL12'   LIST'                                          
         DC    AL1(FEECTRL),CL12'   FEECTRL'                                    
         DC    AL1(FEEAREA),CL12'   FEEAREA'                                    
         DC    AL1(FEEPCNT),CL12'   FEEPCNT'                                    
         DC    AL1(CAST),CL12'   CAST'                                          
         DC    AL1(CYCLE),CL12'   CYCLE'                                        
         DC    AL1(GUAR),CL12'   GUARANTEE'                                     
         DC    AL1(AGPTR),CL12'   AGENT PTR'                                    
         DC    AL1(AGENT),CL12'       AGENT'                                    
         DC    AL1(GFEST),CL12'GF ESTIMATES'                                    
         DC    AL1(MISCWITH),CL12'MISC WITHHLD'                                 
         DC    AL1(W4ALPHA),CL12'    W4 ALPHA'                                  
         DC    AL1(BILRATE),CL12'  BILL RATES'                                  
         DC    AL1(XCOMEST),CL12'TAL ESTIMATE'                                  
         DC    AL1(GENPROD),CL12'GENERAL PROD'                                  
         DC    AL1(GENACCT),CL12'GENERAL ACCT'                                  
         DC    AL1(OFFICE),CL12'OFFICE'                                         
         DC    AL1(MEDINTR),CL12'MEDIA INTERF'                                  
         DC    AL1(TAXRULES),CL12'TAX RULES'                                    
         DC    AL1(BSOURCE),CL12'BILL SOURCE'                                   
         DC    AL1(BILLTRN),CL12'BILL TRNSFR'                                   
         DC    AL1(COSTACC),CL12'COST ACCOUNT'                                  
         DC    AL1(REQUEST),CL12'REQUEST'                                       
         DC    AL1(CHKAUTH),CL12'   CHK AUTH'                                   
*                                   ADD NEW SPECIAL RECORD TYPES HERE           
         DC    X'FF',CL12'?? UNKNOWN'                                           
NSPECIAL EQU   (*-RECSPEC)/L'RECTAB NUMBER OF SPECIAL RECORD TYPES              
NRECTAB  EQU   (*-RECTAB)/L'RECTAB  NUMBER OF RECORD TYPES                      
***********************************************************************         
*                                                                               
* NOTE:- ANYTHING AFTER THIS POINT SHOULD BE ADDRESSED WITH AN A-TYPE           
*                                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*              GLOBAL WORKING STORAGE                                           
***********************************************************************         
ACCWORK  DS    0D                                                               
         DC    CL8'**VYTP**'                                                    
         DC    V(ISDDS)                                                         
         DC    V(ISLDDS)                                                        
         DC    A(ISREAD)                                                        
         DC    A(ISRDSEQ)                                                       
         DC    A(ISADD)                                                         
         DC    A(ISWRITE)                                                       
         DC    A(ISERASE)                                                       
         DC    V(CARDS)                                                         
         DC    V(CHOPPER)                                                       
         DC    V(CPRINT)                                                        
         DC    V(DATAMGR)                                                       
         DC    V(DECODE)                                                        
         DC    V(DUMPOUT)                                                       
         DC    V(EXPAND)                                                        
         DC    V(GETLOGO)                                                       
         DC    V(HELLO)                                                         
         DC    V(HEXOUT)                                                        
         DC    V(LOGIO)                                                         
         DC    V(LOGO)                                                          
         DC    V(LOGOC)                                                         
         DC    V(PRINT)                                                         
         DC    V(PRINTER)                                                       
         DC    V(DATCON)                                                        
         DC    V(DATVAL)                                                        
         DC    A(ACCOUNT)                                                       
         DC    A(SORTIN1)                                                       
         DC    A(TINT)                                                          
         DC    A(TOUT)                                                          
         DC    A(TCOPY)                                                         
         DC    CL8'**AIOS**'                                                    
         DC    A(IOAREA1)                                                       
         DC    A(IOAREA2)                                                       
         DC    CL8'**AELM**'                                                    
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    CL8'**ARTN**'                                                    
         DC    A(FIRST)                                                         
         DC    A(PROCESS)                                                       
         DC    A(LAST)                                                          
         DC    A(0)                                                             
         DC    CL8'**DMCB**'                                                    
         DC    6F'0'                                                            
         DC    CL8'**DIN***'                                                    
         DC    A(ISOPEN)                                                        
         DC    A(IOAREA1+4)                                                     
         DC    A(0)                                                             
         DC    A(ACCOUNT)                                                       
         DC    A(KEY)                                                           
         DC    X'FF',AL3(TRKBUFF)                                               
         DC    CL8'**DOUT**'                                                    
         DC    A(0)                                                             
         DC    A(IOAREA1+4)                                                     
         DC    A(0)                                                             
         DC    A(ACCOUNT)                                                       
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    CL8'**PHCT**'                                                    
         DC    4PL4'0'                                                          
         DC    CL8'**GENS**'                                                    
         DC    2PL6'0'                                                          
         DC    CL4'DEVS'                                                        
         DC    2X'00'                                                           
         DC    CL8'**NAME**'                                                    
         DC    CL36' '                                                          
         DC    CL8'**KEYS**'                                                    
KEY      DC    41XL1'00',X'01'                                                  
         DC    42XL1'FF'                                                        
         DC    CL8'**TWRK**'                                                    
         DC    CL80' '                                                          
         DC    CL40' '                                                          
         DC    2D'0'                                                            
         DC    2F'0'                                                            
         DC    2H'0'                                                            
         DC    2X'0'                                                            
         DC    CL20' '                                                          
         DC    CL8'**SWCH**'                                                    
         DC    6X'00'                                                           
         DC    CL8'**SUPV**'                                                    
         DC    A(0)                                                             
         DC    CL8' '                                                           
         DC    XL4'00'                                                          
         DC    CL8'**PWRK**'                                                    
         DS    3000C                                                            
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'**DCBS**'                                                    
SORTIN1  DCB   DDNAME=SORTIN1,DSORG=PS,MACRF=(PM,GM),LRECL=84,         *        
               RECFM=F,BLKSIZE=84,BUFNO=1                                       
*                                                                               
TINT     DCB   DDNAME=TINT,DSORG=PS,MACRF=(GM),EODAD=LAST,             *        
               RECFM=VB,LRECL=2048                                              
*                                                                               
TOUT     DCB   DDNAME=TOUT,DSORG=PS,MACRF=(PM),                        *        
               RECFM=VB,LRECL=2048,BLKSIZE=32760                                
*                                                                               
TCOPY    DCB   DDNAME=TCOPY,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,LRECL=2048,BLKSIZE=32760                                
         EJECT                                                                  
*&&UK                                                                           
ACCOUNT  DMIS  KEYLEN=42,RECSIZE=V,BLKSIZE=6233,DEVICE=3350,SPARE=200, *        
               INDSIZE=40000,DSKXTNT=5                                          
*&&                                                                             
*                                                                               
*                                                                               
*&&US                                                                           
*              FOR 3380                                                         
ACCOUNT  DMIS  KEYLEN=42,RECSIZE=V,INDSIZE=0,DEVICE=3380,SPARE=200,    *        
               BLKSIZE=9076,IXPDOV=Y                                            
*                                                                               
*              FOR 3390                                                         
ACCOUNX  DMIS  KEYLEN=42,RECSIZE=V,INDSIZE=0,DEVICE=3390,SPARE=200,    *        
               BLKSIZE=13682,IXPDOV=Y,BIG=20BIT                                 
*&&                                                                             
         SPACE 2                                                                
BLDXTNT  DC    H'0'                                                             
         SPACE 2                                                                
         DC    CL8'**I/O1**'                                                    
IOAREA1  DS    1024C                                                            
         DC    CL8'**I/O2**'                                                    
IOAREA2  DS    2048C                                                            
         DC    CL8'**TKBF**'                                                    
TRKBUFF  DS    62000C                                                           
         EJECT                                                                  
***********************************************************************         
*              OTHER W/S REQUIREMENTS                                           
***********************************************************************         
         DC    CL8'**BCNT**'                                                    
CNTGEN   DC    (2*NSPECIAL)PL4'0'                                               
         DC    CL8'**CCNT**'                                                    
CNTCMP   DC    X'FF'                                                            
*&&UK*&& DS    10200C                                                           
*&&US*&& DS    25000C                                                           
         DC    CL8'**RCNT**'                                                    
REPACCS  DC    64PL8'0'                                                         
         DC    CL8'**ECNT**'                                                    
ERRACCS  DC    X'FF',50XL46'00'                                                 
         DC    CL8'**END***'                                                    
*                                                                               
*                                                                               
         DS    0F                                                               
         DC    C'**UTL*UTL*UTL***'                                              
UTL      DC    F'0'                                                             
         DC    X'0A000000'                                                      
         EJECT                                                                  
* ACACCWORKD                                                                    
       ++INCLUDE ACACCWORKD                                                     
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* DMGREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044ACACCOLADE04/19/16'                                      
         END                                                                    
