*          DATA SET MPDFAR     AT LEVEL 007 AS OF 09/23/11                      
*PHASE MPDFARA                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
         TITLE 'PLANNING SYSTEM - RECOVERY DUMP && ACTIVITY REPORT'             
MPDFAR   CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE 0,**BUDFAR,=V(REGSAVE)                                           
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
         LA    R8,IO1                                                           
         USING RECDS,R8                                                         
*                                                                               
         DATE  TODAY                                                            
*                                                                               
INIT2    GOTO1 VCARDS,DMCB,CARD,=C'RE00'                                        
         CLC   CARD(2),=C'/*'                                                   
         BE    INIT16                                                           
*                                                                               
         CLC   CARD(7),=C'TAPE=NO' OUTPUT TAPE COPY                             
         BNE   *+12                                                             
         MVI   OUTAPE,C'N'                                                      
         B     INIT2                                                            
*                                                                               
         CLC   CARD(5),=C'DATE='   OVERRIDE SYSTEM DATE                         
         BNE   INIT3                                                            
         GOTO1 VDATVAL,DMCB,(0,CARD+5),TODAY  DATE=MM/DD/YY                     
         OC    DMCB(4),DMCB                                                     
         BNZ   *+14                                                             
         MVC   P(17),=CL17'BAD DATE CARD'                                       
         B     INIT14                                                           
         MVC   SPECDATE(5),=C'DATE='                                            
         MVC   SPECDATE+5(6),TODAY YYMMDD                                       
         B     INIT2                                                            
*                                                                               
INIT3    CLC   CARD(6),=C'TODAY='  TODAY ONLY                                   
         BNE   INIT4                                                            
         CLC   CARD+6(3),=C'YES'                                                
         BNE   INIT12                                                           
         MVI   TDYONLY,C'Y'                                                     
         B     INIT2                                                            
*                                                                               
INIT4    CLC   CARD(6),=C'INPUT='  TAPE INPUT                                   
         BNE   INIT5                                                            
         CLC   CARD+6(4),=C'TAPE'                                               
         BNE   INIT12                                                           
         MVI   INTAPE,C'Y'                                                      
         B     INIT2                                                            
*                                                                               
INIT5    CLC   CARD(6),=C'ERASE='  ERASE RECOVERY FILE                          
         BNE   INIT6                                                            
         MVC   ERASE,CARD+6                                                     
         B     INIT2                                                            
*                                                                               
INIT6    CLC   CARD(6),=C'COPY=Y'  EXTRA TAPE COPY                              
         BNE   INIT7                                                            
         MVI   COPY,C'Y'                                                        
         B     INIT2                                                            
*                                                                               
INIT7    CLC   CARD(6),=C'DDSIO='  DDSIO=XXX... TO SET THE DDSIO                
         BNE   INIT8                                                            
         L     RF,=V(DDSIO)        OVERRIDE DATAMGR LOAD MODULE NAME            
         MVC   0(8,RF),CARD+6                                                   
         B     INIT2                                                            
*                                                                               
INIT8    CLC   CARD(7),=C'DSPACE=' DSPACE=X TO SET THE DATA SPACE               
         BNE   INIT12                                                           
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),CARD+7                                     
         B     INIT2                                                            
*                                                                               
INIT12   MVC   P(17),=CL17'BAD CONTROL CARD '                                   
         MVC   P+17(80),CARD                                                    
*                                                                               
INIT14   GOTO1 VLOGIO,DMCB,1,(17,P)                                             
         GOTO1 VPRINTER                                                         
         B     EOFX                                                             
*                                                                               
INIT16   GOTO1 VDATCON,DMCB,(0,TODAY),(3,TODAYB)                                
         CLI   OUTAPE,C'Y'         TEST IF OUTPUT TAPE REQUIRED                 
         BNE   INIT18                                                           
         OPEN  (RCVTAPE,(OUTPUT))                                               
         CLI   COPY,C'Y'           TEST IF TAPE COPY REQUIRED                   
         BNE   INIT18                                                           
         OPEN  (RCVCOPY,(OUTPUT))                                               
*                                                                               
INIT18   CLI   INTAPE,C'Y'         TAPE INPUT - NOT DISK                        
         BNE   INIT20                                                           
         OPEN  (TAPEIN,(INPUT))                                                 
         B     PROC                                                             
*                                                                               
INIT20   GOTO1 VDATAMGR,DMCB,DMOPEN,DMSYS,DMFLIST,IO1                           
         B     PROC                                                             
         EJECT                                                                  
* READ RECOVERY FILE & PUT RECORDS TO OUTPUT RECOVERY DUMP TAPE                 
*                                                                               
PROC     CLI   INTAPE,C'Y'                                                      
         BNE   PROC1                                                            
         LR    R0,R8                                                            
         GET   TAPEIN,(0)                                                       
         CLC   0(2,R8),MAXLEN      IGNORE LONG RECORDS                          
         BH    PROC                                                             
         B     PROC3                                                            
*                                                                               
PROC1    L     R0,=A(RECBUFF)                                                   
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
         LH    R3,DMDA             BUMP TO NEXT TRACK                           
         LA    R3,1(R3)                                                         
         STH   R3,DMDA                                                          
         MVI   DMDA+2,0            SET RECNUM TO ZERO                           
         B     PROC                                                             
*                                                                               
PROC2    LH    R1,DMCB+18          GET RECORD LENGTH                            
         LA    R1,4(R1)                                                         
         XC    RECLN(4),RECLN                                                   
         STH   R1,RECLN                                                         
         CH    R1,MAXLEN           IGNORE LONG RECORDS                          
         BH    PROC                                                             
         LA    RF,RECVHDR                                                       
         SH    R1,=H'4'                                                         
         LR    RE,R0                                                            
         MOVE  ((RF),(R1)),(RE)    MOVE RECORD TO TAPE BUFFER                   
*                                                                               
PROC3    TM    RRECTY,X'80'        IGNORE POINTER COPIES/CHANGES                
         BO    PROC                                                             
         CLI   RSIN,X'FF'          IGNORE DELETED RECORDS                       
         BE    PROC                                                             
*                                                                               
         CLI   TDYONLY,C'Y'        TODAY ONLY                                   
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
         BE    *+10                                                             
         CLC   FILENUM,RFILTY                                                   
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
*                                                                               
PROC5    CLI   OUTAPE,C'Y'                                                      
         BNE   PROC6                                                            
         PUT   RCVTAPE,(R8)                                                     
         CLI   COPY,C'Y'                                                        
         BNE   PROC6                                                            
         PUT   RCVCOPY,(R8)                                                     
*                                                                               
PROC6    B     PROC                                                             
         EJECT                                                                  
* PRINT RECORD TOTALS BY FILE                                                   
*                                                                               
EOF      MVC   TITLE+20(20),=C'RECOVERY FILE COUNTS'                            
         MVC   MID1(L'MID1L),MID1L                                              
         MVC   MID2(L'MID2L),MID2L                                              
         ZAP   LINE,=P'99'                                                      
*                                                                               
         LA    R2,FILETAB                                                       
         USING FILETABD,R2                                                      
EOF1     CLI   FILENUM,X'FF'                                                    
         BE    EOF4                                                             
         OC    FILECNTS(FILECNTL),FILECNTS                                      
         BZ    EOF3                                                             
         MVI   ACTIVITY,C'Y'                                                    
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
EOF4     CLI   ACTIVITY,C'Y'                                                    
         BE    EOF5                                                             
         MVC   P(17),=C'NO ACTIVITY TODAY'                                      
         GOTO1 VPRINTER                                                         
EOF5     CLI   INTAPE,C'Y'                                                      
         BNE   EOF6                                                             
         CLOSE (TAPEIN)                                                         
EOF6     CLI   OUTAPE,C'Y'                                                      
         BNE   EOF7                                                             
         CLOSE (RCVTAPE)                                                        
         CLI   COPY,C'Y'                                                        
         BNE   EOF7                                                             
         CLOSE (RCVCOPY)                                                        
*                                                                               
EOF7     CLI   ERASE,C'Y'          TEST RECOVERY FILE TO BE ERASED              
         BNE   EOFX                                                             
         GOTO1 =V(DMOD000),PARA,V(DMEXT),0,0,(X'54',0),PARA+20,0                
         GOTO1 =V(DADDS),(R1),V(WTERASE)                                        
*                                                                               
EOFX     XBASE                                                                  
         DROP  R8                                                               
         EJECT                                                                  
* W/S FOR MODULE                                                                
*                                                                               
DUB      DS    D                                                                
WORK     DS    XL64                                                             
CARD     DS    CL80                                                             
DMCB     DS    6F                                                               
PARA     DS    6F                                                               
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
MAXLEN   DC    H'2048'                                                          
TDYONLY  DC    C'N'                                                             
INTAPE   DC    C'N'                                                             
ERASE    DC    C'N'                                                             
COPY     DC    C'N'                                                             
DMRFILE  DC    C'MPLRCV '                                                       
DMFLIST  DC    C'NMPLRCV X'                                                     
DMSYS    DC    C'MPL    '                                                       
DMOPEN   DC    C'OPEN'                                                          
DMRSEQ   DC    C'DMRSEQ'                                                        
DMDA     DC    F'0'                                                             
COUNT    DC    F'0'                                                             
OUTAPE   DC    C'Y'                                                             
ACTIVITY DC    C'N'                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
MID1L    DC    C'FILE         COPIES   CHANGES      ADDS    TOTALS'             
MID2L    DC    C'----         ------   -------      ----    ------'             
*                XXXXXXX   999999999 999999999 999999999 999999999              
         SPACE 1                                                                
FILETAB  DS    0XL24                                                            
         DC    X'51',C'MPLDIR ',16X'00'                                         
         DC    X'52',C'MPLFIL ',16X'00'                                         
         DC    X'53',C'MPLREQ ',16X'00'                                         
         DC    X'56',C'MPQDRA ',16X'00'                                         
         DC    X'57',C'MPQFLA ',16X'00'                                         
         DC    X'58',C'MPRDRA ',16X'00'                                         
         DC    X'59',C'MPRFLA ',16X'00'                                         
         DC    X'5A',C'BUDDIR ',16X'00'                                         
         DC    X'5B',C'BUDFIL ',16X'00'                                         
         DC    X'5C',C'MPRDRB ',16X'00'                                         
         DC    X'5D',C'MPRFLB ',16X'00'                                         
         DC    X'00',C'UNKNOWN',16X'00'                                         
FILETABX DC    X'FF'                                                            
         EJECT                                                                  
* DCB'S FOR TAPE FILES & I/O BUFFERS                                            
*                                                                               
RCVTAPE  DCB   DDNAME=RCVTAPE,DSORG=PS,RECFM=VB,LRECL=8200,            X        
               BLKSIZE=27648,MACRF=(PM)                                         
                                                                                
RCVCOPY  DCB   DDNAME=RCVCOPY,DSORG=PS,RECFM=VB,LRECL=8200,            X        
               BLKSIZE=27648,MACRF=(PM)                                         
                                                                                
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,RECFM=VB,LRECL=8200,             X        
               BLKSIZE=0,MACRF=(GM),EODAD=EOF                                   
                                                                                
IO1      DS    2048C                                                            
RECBUFF  DS    2048C                                                            
TRKBUFF  DS    48000C                                                           
                                                                                
         DS    0D                                                               
SSB      DC    X'0000FF',X'40',4X'00',CL8' ',32X'00',A(0),204X'00'              
UTL      DC    F'0',X'05',XL3'00',XL56'00'                                      
         EJECT                                                                  
* DSECT TO COVER RECOVERY HEADER                                                
*                                                                               
RECDS    DSECT                                                                  
RECLN    DS    XL2                 TOTAL RECORD LENGTH                          
         DS    XL2                 N/D                                          
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 1                                                                
* DSECT TO COVER FILE TABLE                                                     
*                                                                               
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
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007MPDFAR    09/23/11'                                      
         END                                                                    
