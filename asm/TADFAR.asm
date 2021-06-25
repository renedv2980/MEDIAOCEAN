*          DATA SET TADFAR     AT LEVEL 028 AS OF 02/25/15                      
*PHASE TADFARC                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMSECHK                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE SMFOUT                                                                 
         TITLE 'TALENT SYSTEM - RECOVERY DUMP && ACTIVITY REPORT'               
TADFAR   CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE 0,**TADFAR,=V(REGSAVE)                                           
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING TADFAR+4096,R9                                                   
         LA    R7,2048(R9)                                                      
         LA    R7,2048(R7)                                                      
         USING TADFAR+8192,R7                                                   
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
         L     R8,=A(IO1)                                                       
         USING RECDS,R8                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(0,TODAY)                                     
*                                                                               
         MVC   TITLE+20(13),=C'CONTROL CARDS'                                   
         ZAP   LINE,=P'99'                                                      
         B     INIT2                                                            
         LTORG                                                                  
*                                                                               
INIT2    GOTO1 VCARDS,DMCB,CARD,=C'RE00'                                        
         CLC   CARD(2),=C'/*'                                                   
         BE    INIT30                                                           
         MVC   P(72),CARD                                                       
         GOTO1 VPRINTER                                                         
*                                                                               
         CLC   CARD(6),=C'DDSIO='  DDSIO OVER-RIDE                              
         BNE   INIT2A                                                           
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6                                                   
         SHI   RF,4                CHECK TO SEE IF ADDSIO WAS SET               
         OC    0(4,RF),0(RF)                                                    
         BZ    *+6                                                              
         DC    H'00'               TOO LATE MOVE CARD UP                        
         B     INIT2                                                            
*                                                                               
INIT2A   CLC   CARD(7),=C'DSPACE=' SET DSPACE                                   
         BNE   INIT2B                                                           
         MVC   DSPACE,CARD+7                                                    
         L     RF,=A(SSB)                                                       
         USING SSBOFFD,RF                                                       
         MVC   SSODSPAC,DSPACE                                                  
         B     INIT2                                                            
         DROP  RF                                                               
*                                                                               
INIT2B   CLC   CARD(7),=C'TAPE=NO' OUTPUT TAPE COPY                             
         BNE   *+12                                                             
         MVI   OUTAPE,NO                                                        
         B     INIT2                                                            
*                                                                               
         CLC   CARD(5),=C'DATE='   OVERRIDE SYSTEM DATE                         
         BNE   INIT3                                                            
         GOTO1 VDATVAL,DMCB,(0,CARD+5),TODAY  DATE=MM/DD/YY                     
         OC    DMCB(4),DMCB                                                     
         BNZ   *+14                                                             
         MVC   P(17),=CL17'BAD DATE CARD'                                       
         B     INIT21                                                           
         MVC   SPECDATE(5),=C'DATE='                                            
         MVC   SPECDATE+5(6),TODAY YYMMDD                                       
         B     INIT2                                                            
*                                                                               
INIT3    CLC   CARD(6),=C'TODAY='  TODAY ONLY                                   
         BNE   INIT4                                                            
         CLC   CARD+6(3),=C'YES'                                                
         BNE   INIT20                                                           
         MVI   TDYONLY,YES                                                      
         B     INIT2                                                            
*                                                                               
INIT4    CLC   CARD(6),=C'INPUT='  TAPE INPUT                                   
         BNE   INIT5                                                            
         CLC   CARD+6(4),=C'TAPE'                                               
         BNE   INIT20                                                           
         MVI   INTAPE,YES                                                       
         B     INIT2                                                            
*                                                                               
INIT5    CLC   CARD(6),=C'ERASE='  ERASE RECOVERY FILE                          
         BNE   INIT6                                                            
         MVC   ERASE,CARD+6                                                     
         B     INIT2                                                            
*                                                                               
INIT6    CLC   CARD(6),=C'FORCE='  FORCE ERASE                                  
         BNE   INIT7                                                            
         MVC   FORCE,CARD+6                                                     
         B     INIT2                                                            
*                                                                               
INIT7    CLC   CARD(6),=C'COPY=Y'  EXTRA TAPE COPY                              
         BNE   INIT8                                                            
         MVI   COPY,YES                                                         
         B     INIT2                                                            
*                                                                               
INIT8    CLC   CARD(5),=C'FILE='   FILESET INFO                                 
         BNE   INIT9                                                            
         MVC   FILESET,CARD+5                                                   
         MVI   UTL+4,X'10'         SET UTL+4                                    
         CLC   FILESET,=C'TAL1'                                                 
         BE    INIT2                                                            
         MVI   UTL+4,X'20'                                                      
         CLC   FILESET,=C'TAL2'                                                 
         BE    INIT2                                                            
         MVI   UTL+4,X'30'                                                      
         CLC   FILESET,=C'TAL3'                                                 
         BE    INIT2                                                            
         MVI   UTL+4,X'40'                                                      
         CLC   FILESET,=C'TAL4'                                                 
         BE    INIT2                                                            
         B     INIT20                                                           
*                                                                               
INIT9    CLC   CARD(7),=C'GLOBAL=' OVERRIDE VALUE IN DTF                        
         BNE   INIT10                                                           
         MVC   GLOBAL,CARD+7                                                    
         B     INIT2                                                            
*                                                                               
INIT10   EQU   *                                                                
*                                                                               
INIT20   MVC   P(17),=CL17'BAD CONTROL CARD'                                    
*                                                                               
INIT21   GOTO1 VLOGIO,DMCB,1,(17,P)                                             
         GOTO1 VPRINTER                                                         
         B     EOFX                                                             
*                                                                               
INIT30   GOTO1 VDATCON,DMCB,(0,TODAY),(3,TODAYB)                                
*                                                                               
INIT31   CLI   INTAPE,YES          TEST TO OPEN DISK RECOVERY FILE              
         BNE   INIT31A                                                          
         MVI   ERASE,NO                                                         
         B     INIT40                                                           
*                                                                               
         USING DTFPHD,R3                                                        
INIT31A  XC    DMCB(24),DMCB       GET A(DTF) OF RECOVERY FILE IN DMCB4         
         GOTO1 VDATAMGR,DMCB,=C'DTFAD',DMRFILE                                  
         L     R3,DMCB+12          R3=A(DTF)                                    
         ST    R3,ADTF                                                          
         MVC   DTFXNUM,DMCB+12                                                  
         MVC   DTFSNUM,UTL+4                                                    
*                                                                               
         OI    DTFOPEN,DTF_RO      OPEN AS READ ONLY ?                          
         CLI   ERASE,YES                                                        
         BNE   *+8                                                              
         NI    DTFOPEN,255-DTF_RO  OPEN AS READ ONLY                            
*                                                                               
         CLI   GLOBAL,C' '         IF NO GLOBAL=CARD INPUT SET FROM DTF         
         BNE   INIT32                                                           
         TM    DTFFLAG,DTFGLOB                                                  
         BZ    *+8                                                              
         MVI   GLOBAL,YES                                                       
         B     INIT33                                                           
*                                                                               
INIT32   CLI   GLOBAL,NO           TEST IF GLOBAL INPUT                         
         BNE   INIT32A                                                          
         NI    DTFFLAG,255-DTFGLOB                                              
         B     INIT33                                                           
*                                                                               
INIT32A  CLI   GLOBAL,YES                                                       
         BNE   *+8                                                              
         OI    DTFFLAG,DTFGLOB                                                  
*                                                                               
INIT33   XC    DMCB(24),DMCB       CALL DADDS TO OPEN RECOVERY FILE             
         LA    RF,DAOPEN                                                        
         ST    RF,DMCB                                                          
         LARL  RF,IO1                                                           
         ST    RF,DMCB+4                                                        
         ST    R3,DMCB+12                                                       
         LA    RF,DMCB+20          SET A(DISK ADDR)                             
         ST    RF,DMCB+16                                                       
         GOTO1 VDATAMGR,DMCB0,=C'DADDS'                                         
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'00'               FAILED ON OPEN                               
         DROP  R3                                                               
*                                                                               
INIT34   CLI   GLOBAL,YES          MAINTENANCE LOCK IF GLOBAL FILE              
         BNE   INIT40                                                           
         CLI   ERASE,YES                                                        
         BNE   INIT40                                                           
         XC    PARA(24),PARA                                                    
         LA    R0,0                                                             
         CLI   FORCE,YES                                                        
         BNE   *+8                                                              
         LA    R0,X'20'                                                         
         GOTO1 =V(DMSECHK),PARA,((R0),=C'DMSLCK'),0,(R3),(1,0)                  
         L     RE,12(R1)                                                        
         MVC   MSG,0(RE)                                                        
         MVC   RESULT,12(R1)                                                    
         L     RE,16(R1)                                                        
         MVC   FILEINFO,0(RE)                                                   
         MVC   RESERR,16(R1)                                                    
         CLI   RESULT,QIGNORE      0,1,2 OK TO LOAD IS FILE                     
         BL    INIT40                                                           
         BH    INIT35              OPERATOR CANCEL                              
         MVC   P(L'MSG),MSG        OPERATOR/AUTO IGNORE                         
         GOTO1 =V(PRINTER)                                                      
         B     INIT40                                                           
*                                                                               
INIT35   MVC   P(17),=CL17'OPERATOR CANCEL'                                     
         B     INIT21                                                           
*                                                                               
INIT40   CLI   OUTAPE,YES          TEST IF OUTPUT TAPE REQUIRED                 
         BNE   INIT41                                                           
         OPEN  (RCVTAPE,(OUTPUT))                                               
         CLI   COPY,YES            TEST IF TAPE COPY REQUIRED                   
         BNE   INIT41                                                           
         OPEN  (RCVCOPY,(OUTPUT))                                               
*                                                                               
INIT41   CLI   INTAPE,YES          TAPE INPUT - NOT DISK                        
         BNE   PROC                                                             
         OPEN  (TAPEIN,(INPUT))                                                 
         B     PROC                                                             
         EJECT                                                                  
*******************************************************************             
* READ RECOVERY FILE & PUT RECORDS TO OUTPUT RECOVERY DUMP TAPE                 
*******************************************************************             
PROC     CLI   INTAPE,YES                                                       
         BNE   PROC1                                                            
         LR    R0,R8                                                            
         GET   TAPEIN,(0)                                                       
         XR    R1,R1                                                            
         ICM   R1,3,RECLN          LOAD RECORD LENGTH                           
         LR    RF,R1                                                            
         SHI   RF,L'RECVHDR        LESS HEADER LENGTH                           
         TM    RTIME,X'40'         RECOVER EXTENTION                            
         BZ    PROC0B              NO                                           
         LA    RE,RECLN(R1)        POINT TO END OF RECORD                       
         SHI   RE,1                BACK UP FOR EXTENTION LENGTH                 
         LLC   R0,0(,RE)           EXTENTION LENGTH                             
         SHI   RF,R0               LESS EXTENTION LENGTH                        
*                                                                               
PROC0B   CH    RF,MAXLEN           IGNORE LONG RECORDS                          
         BH    PROC                                                             
         B     PROC3                                                            
*                                                                               
PROC1    LARL  R0,RECBUFF                                                       
         GOTO1 VDATAMGR,DMCB,(X'11',DMRSEQ),DMRFILE,DMDA,(R0),ATRKBUFF          
         TM    8(R1),X'80'         TEST E-O-F POSTED                            
         BO    EOF                                                              
         CLI   8(R1),0             TEST ERROR POSTED                            
         BE    PROC2                                                            
         MVC   P(28),=C'DISK ERROR ON RECOVERY FILE='                           
         MVC   P+28(8),DMRFILE                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(17),=C'DA=XXXXXXXX,DMCB='                                      
         GOTO1 VHEXOUT,PARA,DMDA,P+34,,=C'TOG'                                  
         GOTO1 (RF),(R1),DMCB,P+17,20,=C'TOG'                                   
         GOTO1 VPRINTER                                                         
         MVC   P(14),=C'RUN CONTINUING'                                         
         BASR  RE,RF                                                            
         BASR  RE,RF                                                            
         SR    R1,R1               BUMP TO NEXT TRACK                           
         ICM   R1,7,DMDA                                                        
         LA    R1,1(R1)                                                         
         STCM  R1,7,DMDA                                                        
         MVI   DMDA+2,0            SET RECNUM TO ZERO                           
         B     PROC                                                             
*                                                                               
PROC2    LH    R1,DMCB+18          GET RECORD LENGTH                            
         LA    R1,4(R1)                                                         
         XC    RECLN(4),RECLN                                                   
         STH   R1,RECLN                                                         
         LR    RF,R1                                                            
         SHI   RF,L'RECVHDR                                                     
         TM    RTIME,X'40'         RECOVER EXTENTION                            
         BZ    PROC2B                                                           
         LA    RE,RECLN(R1)                                                     
         SHI   RE,1                                                             
         LLC   R0,0(,RE)           EXTENTION LENGTH                             
         SHI   RF,R0                                                            
*                                                                               
PROC2B   CH    RF,MAXLEN           IGNORE LONG RECORDS                          
         BH    PROC                                                             
         LA    RF,RECVHDR                                                       
         SHI   R1,4                                                             
         LARL  R0,RECBUFF                                                       
         LR    RE,R0                                                            
         MOVE  ((RF),(R1)),(RE)    MOVE RECORD TO TAPE BUFFER                   
*                                                                               
PROC3    TM    RRECTY,X'80'        IGNORE POINTER COPIES/CHANGES                
         BO    PROC                                                             
         CLI   RSIN,X'FF'          IGNORE DELETED RECORDS                       
         BE    PROC                                                             
*                                                                               
         CLI   TDYONLY,YES         TODAY ONLY                                   
         BNE   *+14                                                             
         CLC   TODAYB,RDATE                                                     
         BNE   PROC                                                             
*                                                                               
*NOP*    L     R1,COUNT            POP MY COUNT INTO SIN                        
*NOP*    AH    R1,=H'1'                                                         
*NOP*    ST    R1,COUNT                                                         
*NOP*    MVC   RSIN,COUNT                                                       
*                                                                               
         LA    R2,FILETAB                                                       
         USING FILETABD,R2                                                      
PROC4    CLI   FILENUM,0           TEST E-O-T                                   
         BNE   PROC4X                                                           
         MVC   P(30),=CL30'*** BAD FILE NUMBER (99) ***'                        
         GOTO1 VHEXOUT,PARA,RFILTY,P+21,1,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         GOTO1 =V(PRNTBL),PARA,=C'BAD REC',(R8),C'DUMP',256,=C'2D'              
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         B     PROC                DON'T COPY IT                                
*                                                                               
PROC4X   CLC   FILENUM,RFILTY                                                   
         BE    *+12                                                             
         LA    R2,FILETABL(R2)                                                  
         B     PROC4                                                            
         ZIC   R1,RRECTY           UPDATE FILE ACCUMS                           
         SLL   R1,2                                                             
         LA    R1,FILECNTS-L'FILECNTS(R1)                                       
         ICM   RF,15,0(R1)                                                      
         LA    RF,1(RF)                                                         
         STCM  RF,15,0(R1)                                                      
         ICM   RF,15,FILETOTS                                                   
         LA    RF,1(RF)                                                         
         STCM  RF,15,FILETOTS                                                   
         CLI   RFILTY,X'72'        IF THIS IS TALFIL                            
         BNE   *+8                                                              
         BAS   RE,COUNTEM                                                       
         CLI   RFILTY,X'76'                OR CHKFIL                            
         BNE   *+8                                                              
         BAS   RE,COUNTEM             COUNT BY RECORD TYPE                      
*                                                                               
PROC5    DS    0H                                                               
*                                                                               
         CLI   OUTAPE,YES                                                       
         BNE   PROC6                                                            
         PUT   RCVTAPE,(R8)                                                     
         CLI   COPY,YES                                                         
         BNE   PROC6                                                            
         PUT   RCVCOPY,(R8)                                                     
*                                                                               
PROC6    B     PROC                                                             
         EJECT                                                                  
*******************************************************************             
* PRINT RECORD TOTALS BY FILE                                                   
*******************************************************************             
EOF      MVC   TITLE+20(20),=C'RECOVERY FILE COUNTS'                            
         MVC   TITLE+15(4),FILESET                                              
         MVC   MID1(L'MID1L),MID1L                                              
         MVC   MID2(L'MID2L),MID2L                                              
         ZAP   LINE,=P'99'                                                      
*                                                                               
         LA    R2,FILETAB                                                       
         USING FILETABD,R2                                                      
EOF1     CLI   FILENUM,0                                                        
         BE    EOF4                                                             
         MVI   ACTIVITY,YES                                                     
         OC    FILECNTS(FILECNTL),FILECNTS                                      
         BZ    EOF3                                                             
         MVC   P(7),FILENAME                                                    
         LA    R1,P+10                                                          
         LA    RE,FILECNTS                                                      
         LA    RF,FILECNTN                                                      
EOF2     ICM   R0,15,0(RE)                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(9,R1),DUB                                                      
         LA    R1,10(R1)                                                        
         LA    RE,4(RE)                                                         
         BCT   RF,EOF2                                                          
         GOTO1 VPRINTER                                                         
EOF3     LA    R2,FILETABL(R2)                                                  
         B     EOF1                                                             
*                                                                               
EOF4     CLI   ACTIVITY,YES                                                     
         BE    EOF5                                                             
         MVC   P(17),=C'NO ACTIVITY TODAY'                                      
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
EOF5     BAS   RE,PRINT            REPORT BY RECORD TYPE                        
         CLI   INTAPE,YES                                                       
         BNE   EOF6                                                             
         CLOSE (TAPEIN)                                                         
EOF6     CLI   OUTAPE,YES                                                       
         BNE   EOF7                                                             
         CLOSE (RCVTAPE)                                                        
         CLI   COPY,YES                                                         
         BNE   EOF7                                                             
         CLOSE (RCVCOPY)                                                        
*                                                                               
EOF7     CLI   INTAPE,YES          TEST IF DISK FILE INPUT                      
         BE    EOFX                                                             
         CLI   ERASE,YES           TEST RECOVERY FILE TO BE ERASED              
         BNE   EOF11                                                            
*                                                                               
         USING DTFPHD,R3                                                        
EOF9     XC    DMCB(24),DMCB       CALL DADDS TO ERASE THE FILE                 
         LA    RF,WTERASE                                                       
         ST    RF,DMCB                                                          
         L     R3,ADTF             R3=A(DTF)                                    
         ST    R3,DMCB+12                                                       
         LA    RF,DMCB+20          SET A(DISK ADDR)                             
         ST    RF,DMCB+16                                                       
         GOTO1 VDATAMGR,DMCB0,=C'DADDS'                                         
         MVC   HALF,12(R1)         VIA DMDMGR SO 12(R1) NOT 8(R1)               
         NI    HALF+1,X'FB'                                                     
         OC    HALF,HALF           ANY ERROR                                    
         BZ    EOF10                                                            
         MVI   ERASERR,YES                                                      
         MVC   P(29),=C'ERASE ERROR ON RECOVERY FILE='                          
         MVC   P+29(8),DTFDD                                                    
         GOTO1 VHEXOUT,PARA,HALF,P+38,2,=C'TOG'                                 
         GOTO1 VLOGIO,PARA,1,(42,P)                                             
         GOTO1 VPRINTER                                                         
         DROP  R3                                                               
*                                                                               
EOF10    CLI   GLOBAL,YES          IF GLOBAL FILE REDO EOF AND UNLOCK           
         BNE   EOF11                                                            
         CLI   ERASERR,YES                                                      
         BE    EOF10A                                                           
         MVC   DNEXT-DTF(4,R3),=X'FFFFFFFF'                                     
         LA    RF,CHKEOF           CALL DADDS TO FORCE RESET OF EOF             
         ST    RF,DMCB                                                          
         GOTO1 VDATAMGR,DMCB0,=C'DADDS'                                         
                                                                                
EOF10A   GOTO1 =V(DMSECHK),PARA,(X'00',=C'DMSUNL'),0,(R3),(1,0)                 
*                                                                               
EOF11    XC    DMCB(24),DMCB       CALL DADDS TO CLOSE RECOVERY FILE            
         LA    RF,DACLOSE                                                       
         ST    RF,DMCB                                                          
         LARL  RF,IO1                                                           
         ST    RF,DMCB+4                                                        
         L     R3,ADTF             R3=A(DTF)                                    
         ST    R3,DMCB+12                                                       
         LA    RF,DMCB+20          SET A(DISK ADDR)                             
         ST    RF,DMCB+16                                                       
         GOTO1 VDATAMGR,DMCB0,=C'DADDS'                                         
         CLI   ERASERR,YES                                                      
         BNE   EOFX                                                             
         ABEND 661                                                              
*                                                                               
EOFX     XBASE                                                                  
         EJECT                                                                  
*******************************************************************             
*              ROUTINE TO ANALYZE BY RECORD TYPE                                
*******************************************************************             
COUNTEM  NTR1                                                                   
         LA    R2,RECVHDR+24       R2=A(RECORD)                                 
         USING TLRCD,R2                                                         
         LA    R3,RECTBL           R3=A(RECORD TYPE TABLE)                      
         LA    R4,RECTBTAL         R4=A(TOTAL LINE) FOR TALFIL                  
         CLI   RFILTY,X'76'                                                     
         BNE   *+8                                                              
         LA    R4,RECTBCHK                           OR CHKFIL                  
*                                                                               
COUNT1   CLC   0(2,R3),=X'FFFF'    TEST END OF TABLE                            
         BE    COUNT1D                                                          
                                                                                
         CLI   0(R3),X'24'         SPECIAL RECORDS                              
         BE    COUNT1B               MATCH ON 2 BYTES                           
         CLI   1(R3),0                                                          
         BNE   COUNT1B                                                          
                                                                                
COUNT1A  CLC   0(1,R2),0(R3)       TEST MATCH ON RECORD TYPE                    
         B     COUNT1C                                                          
COUNT1B  CLC   0(2,R2),0(R3)       TEST MATCH ON FIRST 2 BYTES                  
COUNT1C  BE    COUNT2                                                           
*                                                                               
         LA    R3,L'RECTBL(R3)                                                  
         B     COUNT1                                                           
*                                                                               
COUNT1D  L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   P(7),=C'UNKNOWN'                                                 
         MVC   P+10(32),0(R2)                                                   
         GOTO1 =V(HEXOUT),DMCB,(R2),P+60,32,=C'TOG'                             
*******  GOTO1 =V(PRINTER)                                                      
*                                                                               
COUNT2   CLI   0(R2),TLW4NCDQ      W4 RECORD?                                   
         BNE   COUNT2B                                                          
         USING TLW4PD,R2                                                        
         TM    TLW4NSTA,TLW4NSAK   WANT TO RECORD AKA NAMES                     
         USING TLRCD,R2                                                         
         BNO   COUNT2B                                                          
         LA    R3,L'RECTBL(R3)                                                  
*                                                                               
COUNT2B  LR    R6,R2               NOW LOOK FOR PAYMENT DETAILS                 
         ZAP   DUB,=P'0'                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   COUNT4                                                           
         USING TAPDD,R6                                                         
         L     R1,TAPDGRS          PICK UP GROSS                                
         CVD   R1,DUB                                                           
*                                                                               
COUNT4   CLI   RRECTY,2                                                         
         BE    COCHG                                                            
         BH    COADD                                                            
         SPACE 1                                                                
         CLI   RFILTY,X'72'       COPIES                                        
         BNE   COUNT6                                                           
         SP    FILECASH,DUB                                                     
         SP    CHGCASH,DUB                                                      
         B     COUNT8                                                           
*                                  DELETED COPIES                               
COUNT6   SP    FILECHKS,DUB                                                     
         SP    CHGCHKS,DUB                                                      
*                                  DELETED COPIES                               
COUNT8   CLI   TLRCSTAT,X'80'                                                   
         BNO   XIT                                                              
         L     RF,28(R3)                                                        
         BCTR  RF,0                        - DELETES                            
         ST    RF,28(R3)                                                        
         L     RF,32(R3)                                                        
         LA    RF,1(RF)                    + TOTAL                              
         ST    RF,32(R3)                                                        
         L     RF,28(R4)                                                        
         BCTR  RF,0                        - DELETES                            
         ST    RF,28(R4)                                                        
         L     RF,32(R4)                                                        
         LA    RF,1(RF)                    + TOTAL                              
         ST    RF,32(R4)                                                        
         CLI   RFILTY,X'72'                                                     
         BNE   COCPY2                                                           
         AP    FILECASH,DUB                                                     
         SP    DELCASH,DUB                                                      
         B     XIT                                                              
*                                                                               
COCPY2   AP    FILECHKS,DUB                                                     
         SP    DELCHKS,DUB                                                      
         B     XIT                                                              
*                                                                               
COCHG    CLI   RFILTY,X'72'       CHANGES                                       
         BNE   COUNT10                                                          
         AP    FILECASH,DUB                                                     
         AP    CHGCASH,DUB                                                      
         B     COUNT12                                                          
*                                                                               
COUNT10  AP    FILECHKS,DUB                                                     
         AP    CHGCHKS,DUB                                                      
*                                  DELETED CHANGES                              
COUNT12  CLI   TLRCSTAT,X'80'                                                   
         BNO   XIT                                                              
         L     RF,28(R3)                                                        
         LA    RF,1(RF)                    + DELETES                            
         ST    RF,28(R3)                                                        
         L     RF,32(R3)                                                        
         BCTR  RF,0                        - TOTAL                              
         ST    RF,32(R3)                                                        
         L     RF,28(R4)                                                        
         LA    RF,1(RF)                    + DELETES                            
         ST    RF,28(R4)                                                        
         L     RF,32(R4)                                                        
         BCTR  RF,0                        - TOTAL                              
         ST    RF,32(R4)                                                        
         CLI   RFILTY,X'72'                                                     
         BNE   COCHG2                                                           
         SP    FILECASH,DUB                                                     
         AP    DELCASH,DUB                                                      
         B     XIT                                                              
*                                                                               
COCHG2   SP    FILECHKS,DUB                                                     
         AP    DELCHKS,DUB                                                      
         B     XIT                                                              
*                                  ADDS                                         
COADD    L     RF,24(R3)                                                        
         LA    RF,1(RF)                    + ADDS                               
         ST    RF,24(R3)                                                        
         L     RF,32(R3)                                                        
         LA    RF,1(RF)                    + TOTAL                              
         ST    RF,32(R3)                                                        
         L     RF,24(R4)                                                        
         LA    RF,1(RF)                    + ADDS                               
         ST    RF,24(R4)                                                        
         L     RF,32(R4)                                                        
         LA    RF,1(RF)                    + TOTAL                              
         ST    RF,32(R4)                                                        
         CLI   RFILTY,X'72'                                                     
         BNE   COADD2                                                           
         AP    FILECASH,DUB                                                     
         AP    ADDCASH,DUB                                                      
         B     XIT                                                              
*                                                                               
COADD2   AP    FILECHKS,DUB                                                     
         AP    ADDCHKS,DUB                                                      
*                                                                               
XIT      XIT1                                                                   
                                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*******************************************************************             
*              ROUTINE TO PRINT OUT TOTALS AT EOJ                               
*******************************************************************             
         SPACE 3                                                                
PRINT    NTR1                                                                   
         L     RA,=V(CPRINT)       PRINT HEADING                                
         USING DPRINT,RA                                                        
         ZAP   LINE,=P'99'                                                      
         MVC   MID1,HDRL1                                                       
         MVC   MID2,ULINE                                                       
         LA    R3,RECTBL           R3=A(RECORD TABLE ENTRY)                     
*                                                                               
PRINT1   OC    20(16,R3),20(R3)    TEST TOTAL RECORD COUNTER                    
         BNZ   PRINT1A             DON'T PRINT IF ZERO                          
         CLC   0(2,R3),=X'FFFF'    SPACE BEFORE TOTALS                          
         BE    PRINT1B                                                          
         B     PRINT3                                                           
*                                                                               
PRINT1A  CLC   0(2,R3),=X'FFFF'    SPACE BEFORE TOTALS                          
         BNE   PRINT1B                                                          
         GOTO1 =V(PRINTER)                                                      
         SPACE 1                                                                
PRINT1B  MVC   PLNAME,4(R3)                                                     
         LA    R4,24(R3)           R4=A(RECORD COUNTER)                         
         LA    R5,PLRACT           R5=A(PLINE DISPLAY FIELD)                    
         LA    R0,3                                                             
*                                                                               
PRINT2   L     RF,0(R4)            TEST RECORD COUNTER                          
         LTR   RF,RF                                                            
         BNZ   PRINT2A                                                          
         MVC   0(8,R5),=8C'0'      SET DOTS IF ZERO                             
         B     PRINT2B                                                          
         SPACE 1                                                                
PRINT2A  CVD   RF,DUB                                                           
         UNPK  DUB1,DUB                                                         
         OI    DUB1+7,C'0'                                                      
         MVC   0(8,R5),DUB1                                                     
PRINT2B  LA    R4,4(R4)            BUMP TO NEXT RECORD COUNTER                  
         LA    R5,9(R5)                                                         
         BCT   R0,PRINT2                                                        
PRINT2C  MVC   P,PLINE                                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PRINT3   CLC   0(4,R3),=4X'FF'     EXIT IF END OF TABLE                         
         BE    PRINTX                                                           
         LA    R3,L'RECTBL(R3)                                                  
         B     PRINT1                                                           
*                                                                               
PRINTX   GOTO1 =V(PRINTER)         PRINT FILE TOTALS AT END                     
         MVC   P(17),=C'CASH FROM TALFILE'                                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P(17),=C'-----------------'                                      
         GOTO1 =V(PRINTER)                                                      
         BASR  RE,RF                                                            
         MVC   P(17),=C'TOTAL ADDS      ='                                      
         EDIT  (P8,ADDCASH),(14,P+18),2,MINUS=YES                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P(17),=C'TOTAL CHANGES   ='                                      
         EDIT  (P8,CHGCASH),(14,P+18),2,MINUS=YES                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P(17),=C'TOTAL DELETES   ='                                      
         EDIT  (P8,DELCASH),(14,P+18),2,MINUS=YES                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P(17),=C'NET FILE CHANGE ='                                      
         EDIT  (P8,FILECASH),(14,P+18),2,MINUS=YES                              
         GOTO1 =V(PRINTER)                                                      
         SPACE 1                                                                
         GOTO1 =V(PRINTER)                                                      
         MVC   P(17),=C'CASH FROM CHKFILE'                                      
         GOTO1 =V(PRINTER)                                                      
         MVC   P(17),=C'-----------------'                                      
         GOTO1 =V(PRINTER)                                                      
         BASR  RE,RF                                                            
         MVC   P(17),=C'TOTAL ADDS      ='                                      
         EDIT  (P8,ADDCHKS),(14,P+18),2,MINUS=YES                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P(17),=C'TOTAL CHANGES   ='                                      
         EDIT  (P8,CHGCHKS),(14,P+18),2,MINUS=YES                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P(17),=C'TOTAL DELETES   ='                                      
         EDIT  (P8,DELCHKS),(14,P+18),2,MINUS=YES                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P(17),=C'NET FILE CHANGE ='                                      
         EDIT  (P8,FILECHKS),(14,P+18),2,MINUS=YES                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         BAS   RE,GOSMF                                                         
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* PUT OUT SMF OUTFILE DEBITS/CREDITS BY COMPANY                      *          
**********************************************************************          
         USING SMFBRECD,R2                                                      
GOSMF    NTR1                                                                   
         CLI   ERASE,YES           ONLY PUT OUT SMF FOR REAL RECOVERY           
         BNE   GOSMFXIT                                                         
*                                                                               
         LA    R2,SMFREC                                                        
         XC    SMFREC,SMFREC                                                    
         LHI   RF,SMFBRLNQ           LENGTH OF SMF DATA RECORD                  
         STH   RF,SMFBLEN                                                       
         MVC   SMFBDRS(6*L'SMFBMNY),=6PL8'0'                                    
         MVC   SMFBUSER,SPACES                                                  
         MVC   SMFBTXT,SPACES                                                   
         MVC   SMFBTXT1,SPACES                                                  
         MVC   SMFBTXT2,SPACES                                                  
         MVI   SMFBDSPC,C' '                                                    
         MVI   SMFBTYPE,C'M'       MONEY                                        
         MVC   SMFBSRCE,=CL4'RCVR'                                              
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(15,SMFBDATE)   JULIAN 0CYYDDDF            
*                                                                               
         USING UTLD,RE                                                          
         ICM   RE,15,=V(UTL)                                                    
         BZ    *+10                                                             
         MVC   SMFBSENO,TSYS       SYSTEM NUMBER                                
*                                                                               
         USING SSBOFFD,RE                                                       
         ICM   RE,15,=V(SSB)                                                    
         BZ    *+10                                                             
         MVC   SMFBDSPC,SSODSPAC   DATA SPACE VALUE                             
         DROP  RE                                                               
*----------------------------------------------------------------------         
         MVI   SMFBTYPE,C'C'       COUNTER                                      
         LA    R3,RECTBTAL         TOTAL RECORDS TABLE ENTRY                    
         AHI   R3,24               BUMP PAST HEADER AND TEXT                    
         MVC   SMFBTXT,=CL8'RCV CNTS'                                           
         ICM   R5,15,0(R3)                                                      
         CVD   R5,TEMPPCKD                                                      
         ZAP   SMFBCNT1,TEMPPCKD             TALFILE ADDS                       
*                                                                               
         LA    R3,RECTBCHK         TOTAL RECORDS TABLE ENTRY                    
         AHI   R3,24               BUMP PAST HEADER AND TEXT                    
         ICM   R5,15,0(R3)                                                      
         CVD   R5,TEMPPCKD                                                      
         ZAP   SMFBCNT2,TEMPPCKD             CHKFILE ADDS                       
         BAS   RE,PUTSMF                                                        
*                                                                               
         MVI   SMFBTYPE,C'M'       MONEY                                        
         MVC   SMFBTXT,=CL8'RCV CASH'                                           
         ZAP   SMFBMNY1,FILECASH             TALFILE CASH                       
         ZAP   SMFBMNY2,FILECHKS             CHKFILE CASH                       
         BAS   RE,PUTSMF                                                        
*                                                                               
GOSMFXIT B     XIT                                                              
*                                                                               
PUTSMF   NTR1                                                                   
         ICM   RF,15,=V(SMFOUT)                                                 
         BZ    GOSMFXIT                                                         
         GOTO1 (RF),SMFPARM,12,SMFREC                                           
*                                                                               
         LA    R5,SMFREC                                                        
         LA    R6,SMFBRECX-SMFBRECD                                             
         GOTO1 =V(PRNTBL),DMCB,=C'SMFREC',(R5),C'DUMP',(R6),=C'1D'              
         B     XIT                                                              
                                                                                
         DROP  R2                                                               
         EJECT                                                                  
*******************************************************************             
*              CONSTANTS ETC                                                    
*******************************************************************             
FILECASH DC    PL8'0'                                                           
DELCASH  DC    PL8'0'                                                           
CHGCASH  DC    PL8'0'                                                           
ADDCASH  DC    PL8'0'                                                           
         SPACE 1                                                                
FILECHKS DC    PL8'0'                                                           
DELCHKS  DC    PL8'0'                                                           
CHGCHKS  DC    PL8'0'                                                           
ADDCHKS  DC    PL8'0'                                                           
         SPACE 1                                                                
DATADISP DC    H'40'                                                            
ERASERR  DC    AL1(NO)             ERASE ERROR                                  
         SPACE 1                                                                
HDRL1    DC   CL132'RECORD TYPE          ADDS  DELETES  BALANCE'                
ULINE    DC   CL132'---------------- -------- -------- --------'                
         SPACE 2                                                                
PLINE    DC    CL132' '                                                         
         ORG   PLINE                                                            
PLNAME   DS    CL16                                                             
         DS    CL1                                                              
PLRACT   DS    CL8                                                              
         DS    CL1                                                              
PLRLAP   DS    CL8                                                              
         DS    CL1                                                              
PLRDEL   DS    CL8                                                              
         ORG                                                                    
       ++INCLUDE TALDRECTB                                                      
         EJECT                                                                  
*******************************************************************             
* W/S FOR MODULE                                                                
*******************************************************************             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
FF       EQU   X'FF'                                                            
                                                                                
DUB      DS    D                                                                
DUB1     DS    D                                                                
WORK     DS    XL64                                                             
CARD     DC    CL80' '                                                          
MSG      DS    CL60' '                                                          
DMCB0    DS    F                                                                
DMCB     DS    6F                                                               
PARA     DS    6F                                                               
ADTF     DC    F'0'                                                             
HALF     DS    H                                                                
ELCODE   DS    XL1                                                              
TODAY    DS    CL6                 YYMMDD                                       
TODAYB   DS    CL3                 BINARY                                       
VCARDS   DC    V(CARDS)                                                         
VDATAMGR DC    V(DATAMGR)                                                       
VDATCON  DC    V(DATCON)                                                        
VDATVAL  DC    V(DATVAL)                                                        
VPRINTER DC    V(PRINTER)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
VLOGIO   DC    V(LOGIO)                                                         
ATRKBUFF DC    A(TRKBUFF)                                                       
MAXLEN   DC    H'4096'                                                          
TDYONLY  DC    AL1(NO)                                                          
INTAPE   DC    AL1(NO)                                                          
ERASE    DC    AL1(NO)                                                          
FORCE    DC    C' '                                                             
GLOBAL   DC    C' '                                                             
DSPACE   DC    C' '                                                             
RESULT   DC    X'00'                                                            
RESERR   DC    X'00'                                                            
COPY     DC    AL1(NO)                                                          
DMRFILE  DC    C'TALRCV '                                                       
*MFLIST  DC    C'NTALRCV X'                                                     
*MSYS    DC    C'TAL     '                                                      
*MOPEN   DC    C'DMOPEN  '                                                      
*MCLSE   DC    C'DMCLSE  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
DMDA     DC    F'0'                                                             
COUNT    DC    F'0'                                                             
OUTAPE   DC    AL1(YES)                                                         
ACTIVITY DC    AL1(NO)                                                          
FILESET  DC    C'TAL1'                                                          
SMFPARM  DS    6F                                                               
TEMPPCKD DC    PL8'0'                                                           
*                                                                               
         DS    0D                                                               
         DC    4CL8'*SMFREC*'                                                   
SMFREC   DS    XL(SMFBRECX-SMFBRECD) SMF BALANCE RECORD                         
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
MID1L    DC    C'FILE         COPIES   CHANGES      ADDS     TOTAL'             
MID2L    DC    C'----      --------- --------- --------- ---------'             
*                XXXXXXX   999999999 999999999 999999999 999999999              
         SPACE 1                                                                
FILETAB  DS    0XL24                                                            
         DC    X'71',C'TALDIR ',16X'00'                                         
         DC    X'72',C'TALFIL ',16X'00'                                         
         DC    X'73',C'TALREQ ',16X'00'                                         
         DC    X'75',C'CHKDIR ',16X'00'                                         
         DC    X'76',C'CHKFIL ',16X'00'                                         
FILETABX DC    X'00'                                                            
         SPACE 1                                                                
         DS    0F                                                               
FILEINFO DS    0XL16               RETURNED FILE INFO FROM V(DMDDNAME)          
         DS    X                   RESERVED                                     
SENUM    DS    X                   SE NUMBER                                    
SENAME   DS    CL5                 SE SHORT NAME                                
SEFLAG1  DS    X                   SE FLAGS                                     
FILNUM   DS    X                   FILE NUMBER                                  
FILFLAG1 DS    X                   FILE FLAGS                                   
FILFLAG2 DS    X                   FILE FLAGS                                   
         DS    X                   N/D                                          
FILADTF  DS    AL4                 FILE A(DTF)                                  
*                                                                               
DSNINFO  DS    0XL4                                                             
DDNDSP   DS    X                   DISP TO DDNAME SIGNIFICANT CHR(S)            
DSNDSP   DS    X                   DISP TO DSN SIGNIFICANT CHR(S)               
         DS    XL2                 N/D                                          
*                                                                               
DDNAME   DS    CL8                 FILE DD NAME                                 
DDSDSN   DS    CL20                DATA SET NAME FROM DDS DYNDD TABLE           
*                                                                               
         DS    0F                                                               
SYSINFO  DS    XL64                RETURNED SYSTEM INFO FROM V(LOCKSPC)         
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**SSB***'                                                      
SSB      DC    XL2'00',X'FF',XL5'00'                                            
         DC    XL248'00'                                                        
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'**UTL***'                                                      
UTL      DC    256X'00'                                                         
         SPACE 1                                                                
QOK      EQU   0                   FILE ERASED                                  
QOKGLOB  EQU   1                   FILE ERASED GLOBAL                           
QIGNORE  EQU   2                   OPERATOR IGNORED ERROR                       
QCANCEL  EQU   3                   OPERATOR CANCELLED                           
QDSKERR  EQU   4                   DISK ERROR DURING ERASE                      
*NOTDONE EQU   5                   FILE NOT ERASED (WRITE=NO)                   
         SPACE 1                                                                
*DMGREQUS                                                                       
       ++INCLUDE DMGREQUS                                                       
         EJECT                                                                  
*******************************************************************             
* DCB'S FOR TAPE FILES & I/O BUFFERS                                            
*******************************************************************             
RCVTAPE  DCB   DDNAME=RCVTAPE,DSORG=PS,RECFM=VB,LRECL=8200,            X        
               BLKSIZE=27648,MACRF=(PM)                                         
         SPACE 1                                                                
RCVCOPY  DCB   DDNAME=RCVCOPY,DSORG=PS,RECFM=VB,LRECL=8200,            X        
               BLKSIZE=27648,MACRF=(PM)                                         
         SPACE 1                                                                
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,RECFM=VB,LRECL=8200,             X        
               BLKSIZE=0,MACRF=(GM),EODAD=EOF                                   
*                                                                               
IO1      DS    13312C                                                           
RECBUFF  DS    13312C                                                           
TRKBUFF  DS    (64*1024)C                                                       
         EJECT                                                                  
*******************************************************************             
* DSECT TO COVER RECOVERY HEADER                                                
*******************************************************************             
RECDS    DSECT                                                                  
RECLN    DS    XL2                 TOTAL RECORD LENGTH                          
         DS    XL2                 N/D                                          
       ++INCLUDE DMRCVRHDR                                                      
                                                                                
*******************************************************************             
* DSECT TO COVER FILE TABLE                                                     
*******************************************************************             
FILETABD DSECT                                                                  
FILENUM  DS    XL1                 FILE NUMBER                                  
FILENAME DS    CL7                 FILE NAME                                    
FILECNTS DS    0XL4                                                             
FILECPYS DS    XL4                 N'COPIES                                     
FILECHAS DS    XL4                 N'CHANGES                                    
FILEADDS DS    XL4                 N'ADDS                                       
FILETOTS DS    XL4                 TOTAL N'RECORDS                              
FILECNTN EQU   (*-FILECNTS)/L'FILECNTS                                          
FILECNTL EQU   *-FILECNTS                                                       
FILETABL EQU   *-FILETABD                                                       
         SPACE 1                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
* FASSBOFF                                                                      
         PRINT OFF                                                              
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* DMDTFPH                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
* DDSMFFBAL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSMFFBAL                                                      
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028TADFAR    02/25/15'                                      
         END                                                                    
