*          DATA SET DDVERIFY   AT LEVEL 003 AS OF 03/05/13                      
*PHASE VERIFYA                                                                  
*INCLUDE FATABOFF                                                               
*INCLUDE CARDS                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE SORTER                                                                 
*INCLUDE DMDMGRL                                                                
*                                                                               
         TITLE 'RECOVERY FILE VERFY PROGRAM '                                   
VERIFY   CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**VERIFY,=V(REGSAVE),R9,R8                           
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
*                                                                               
         BAS   RE,PRINTI                                                        
         BAS   RE,INIT                                                          
         BAS   RE,OPENS                                                         
         BAS   RE,PUTSORT                                                       
         BAS   RE,GETSORT                                                       
         MVC   PLINE,SPACES                                                     
         MVC   PLINE(19),=C'000000 COPY ERRORS '                                
         EDIT  (P4,COPYERR),(6,PLINE),FILL=0                                    
         BAS   RE,PRINTL                                                        
         MVC   PLINE(19),=C'000000 FILE ERRORS '                                
         EDIT  (P4,FILEERR),(6,PLINE),FILL=0                                    
         BAS   RE,PRINTL                                                        
*                                                                               
XMOD1    L     RD,SAVERD                                                        
         BAS   RE,PRINTX                                                        
         SR    R1,R1                                                            
         CP    COPYERR,=P'0'                                                    
         BE    *+8                                                              
         LA    R1,4                                                             
         CP    FILEERR,=P'0'                                                    
         BE    *+8                                                              
         LA    R1,4                                                             
XBASE    XBASE RC=(R1)                                                          
*                                                                               
EXIT1    XIT1                                                                   
         EJECT                                                                  
************************************************************                    
*        INIT TABLES                                       *                    
************************************************************                    
INIT     NTR1                                                                   
         ZAP   COPYERR,=P'0'                                                    
         ZAP   FILEERR,=P'0'                                                    
         LA    R1,TITLE1           PRINT PARAMETER CARDS TITLE                  
         BAS   RE,PRINTT                                                        
         LA    R3,IOAREA                                                        
*                                                                               
INIT010  GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    EXIT1                                                            
         MVC   PLINE+1(80),0(R3)                                                
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
         LR    R2,R3                                                            
*                                                                               
INIT011  CLC   0(6,R2),=C'DDSIO='  DDSIO=XXX... TO SET THE DDSIO                
         BNE   INIT012                                                          
         L     RF,=V(DDSIO)        OVERRIDE DATAMGR LOAD MODULE NAME            
         MVC   0(8,RF),6(R2)                                                    
         B     INIT010                                                          
*                                                                               
INIT012  CLC   0(7,R2),=C'DSPACE=' DSPACE=X TO SET THE DATA SPACE               
         BNE   INIT013                                                          
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),7(R2)                                      
         B     INIT010                                                          
*                                                                               
INIT013  CLC   0(6,R2),=C'COPY=Y'                                               
         BNE   *+12                                                             
         MVI   COPY,C'Y'                                                        
         B     INIT010                                                          
*                                                                               
INIT014  CLC   0(7,R2),=C'SYSTEM='                                              
         BNE   CERRCARD                                                         
         LA    R2,7(R2)                                                         
         LR    R1,R2                                                            
INIT020  CLI   0(R1),C' '                                                       
         BE    INIT030                                                          
         CLI   0(R1),C','                                                       
         BE    INIT030                                                          
         LA    R1,1(R1)                                                         
         B     INIT020                                                          
*                                                                               
INIT030  SR    R1,R2                                                            
         BCTR  R1,0                                                             
         BAS   RE,VALSYS                                                        
*                                                                               
         LA    R1,VALTAB                                                        
INIT040  CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   FOVSYS,0(R1)                                                     
         BE    INIT050                                                          
         LA    R1,L'VALTAB(R1)                                                  
         B     INIT040                                                          
*                                                                               
INIT050  MVC   DMFLIST+1(7),3(1)                                                
         MVC   DMFLIST+9(7),11(R1)                                              
         MVC   DMFLIST+17(7),19(R1)                                             
         MVC   FIL1,3(R1)                                                       
         MVC   FIL2,10(R1)                                                      
         MVC   KEYLEN,1(R1)                                                     
         MVC   STATLEN,18(R1)                                                   
         EDIT  (B1,KEYLEN),(2,SORTCARD+16),FILL=0                               
*                                                                               
         L     RF,=A(UTL)                                                       
         MVC   4(1,RF),FSESYS                                                   
         B     EXIT1                                                            
         EJECT                                                                  
************************************************************                    
*        OPEN FILES AND SORT                               *                    
************************************************************                    
OPENS    NTR1                                                                   
         LA    R2,IOAREA                                                        
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
*                                                                               
         GOTO1 DATAMGR,DMCB,OPEN,FSENAME,DMFLIST,IOAREA                         
         LA    R5,RCVTAPE                                                       
         OPEN  ((R5),OUTPUT)       OPEN OUTPUT TAPE                             
         B     EXIT1                                                            
         EJECT                                                                  
************************************************************                    
*        PUT ALL REQUIRED RECORD TO SORT                   *                    
************************************************************                    
PUTSORT  NTR1                                                                   
         SR    R3,R3                                                            
         LA    R2,IOAREA                                                        
READREC  LA    R3,1(R3)                                                         
         GOTO1 DATAMGR,DMCB,(X'10',DMRSEQ),DMRFIL,DMDA,8(R2),A(TRK)             
         TM    DMCB+8,X'80'                                                     
         BO    READRECX            EOF                                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
*                                                                               
         CLC   FIL1(1),8(R2)       FILTER ON DIR FIL PAIR                       
         BE    *+14                                                             
         CLC   FIL2(1),8(R2)                                                    
         BNE   READREC                                                          
         TM    9(R2),X'80'                                                      
         BO    READREC                                                          
         CLC   12(4,R2),=X'00000001' IGNORE OFFLINE                             
         BE    READREC                                                          
*                                                                               
         LH    R1,DMCB+18          SET RECORD LENGTH                            
         OR    R1,R1                                                            
         BZ    READREC                                                          
         XC    0(8,R2),0(R2)                                                    
         ST    R3,4(R2)            SET RECORD SEQ NUM                           
         LA    R1,8(R1)                                                         
         STH   R1,0(R2)            SET SECORD LENGTH                            
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R2)                                     
         B     READREC                                                          
*                                  HANDLE RECOVERY FILE DISK ERRORS             
READRECX B     EXIT1                                                            
*                                                                               
         EJECT                                                                  
************************************************************                    
*        READ BACK FROM SORT AND REPORT ERRORS             *                    
************************************************************                    
GETSORT  NTR1                                                                   
         MVC   LASTKEY,SPACES                                                   
GETS010  GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     R3,DMCB+4                                                        
         LTR   R3,R3               CC=EQ AT EOF                                 
         BZ    GETSORTX                                                         
         CLI   COPY,C'Y'           DO WE JUST WANT A COPY                       
         BNE   GETS020                                                          
         PUT   RCVTAPE,(R3)        WRITE OUT COPY                               
         B     GETS010                                                          
*                                                                               
GETS020  SR    RF,RF               TEST FOR NEW KEY                             
         IC    RF,KEYLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   LASTKEY(0),32(R3)   THIS KEY SAME AS LASTKEY?                    
         BNE   GETS080                                                          
*                                                                               
         CLI   9(R3),X'02'         JUST SAVE IT IF CHANGE                       
         BE    GETS990                                                          
         CLI   9(R3),X'03'         IF ITS AN ADD FUCK IT                        
         BE    GETS010                                                          
*        DC    H'0'                                                             
         CLI   9(R3),X'01'         IF ITS NOT COPY "FUCKING HELL"               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R7,R7               OK ITS A COPY                                
         SR    RF,RF                                                            
         L     R6,=A(IOAREA2)                                                   
         LR    RE,R3                                                            
         ICM   RF,3,0(RE)                                                       
         ICM   R7,3,0(R6)                                                       
         CR    R7,RF                                                            
         BNE   ERROR1                                                           
         CLC   8(1,R3),FIL1        IS IT A DA RECORD                            
         BE    GETS030                                                          
*                                                                               
         LA    R1,32               32 FOR HEADER                                
         SR    RF,RF                                                            
         IC    RF,KEYLEN           + KEY LENGTH                                 
         AR    R1,RF                                                            
         LA    R1,2(R1)            + 2 FOR LENGTH                               
         IC    RF,STATLEN          + STATUS LENGTH                              
         AR    R1,RF                                                            
         LR    RF,R1               RF,R1=OFFSET TO LINKAGE                      
         AR    RF,R6                                                            
         AR    R1,RE                                                            
         XC    0(4,R1),0(R1)       ZAP LINKAGES TO ZERO                         
         XC    0(4,RF),0(RF)                                                    
*                                                                               
GETS030  SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         LA    R6,32(R6)           CUT OFF HEADERS FOR COMPARE                  
         LA    RE,32(RE)                                                        
         SH    R7,=H'32'                                                        
         SH    RF,=H'32'                                                        
         CLCL  R6,RE               COMPARE PREV ADD/CHA WITH COPY               
         BNE   ERROR1                                                           
         B     GETS990                                                          
*                                                                               
ERROR1   L     R6,=A(IOAREA2)                                                   
         PUT   RCVTAPE,(R6)                                                     
         PUT   RCVTAPE,(R3)                                                     
         AP    COPYERR,=P'1'       BUMP COPY ERRORS                             
         B     GETS990                                                          
*                                                                               
GETS080  CLC   LASTKEY,SPACES      FIRST TIME ONLY                              
         BE    GETS990                                                          
         EJECT                                                                  
************************************************************                    
*        VERIFY LAST RECORD WITH FILE                      *                    
************************************************************                    
         L     R6,=A(IOAREA2)                                                   
*                                                                               
         CLI   9(R6),2             IGNORE IF NOT A CHANGE                       
         BNE   GETS990                                                          
         CLC   8(1,R6),FIL1        DMREAD DIRECTORY RECORDS                     
         BE    GETS100                                                          
         CLC   8(1,R6),FIL2        GETREC FOER FILE RECORDS                     
         BE    GETS090                                                          
         DC    H'0'                                                             
*                                                                               
GETS090  MVC   DA,20(R6)                                                        
         GOTO1 DATAMGR,DMCB,GETREC,FIL2+1,DA,IOAREA,DMWORK                      
         CLI   8(R1),0                                                          
         BNE   ERROR2                                                           
         LA    RE,IOAREA                                                        
         SR    R1,R1                                                            
         IC    R1,KEYLEN                                                        
         AR    R1,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,0(R1)          PICK UP RECLEN                               
         SR    R7,R7                                                            
         ICM   R7,3,0(R6)                                                       
         SH    R7,=H'32'                                                        
         LA    R6,32(R6)                                                        
         LR    R7,RF               RECOVERY RECORD LEN CAN BE WRONG             
         CR    R7,RF                                                            
         LR    R1,RF                                                            
         BNE   ERROR2                                                           
*        LR    R5,R6                                                            
*        LR    RA,RE                                                            
         CLCL  R6,RE               COMPARE PREV WITH FILE                       
         BNE   ERROR2                                                           
         BE    GETS990                                                          
         DC    H'0'                                                             
*                                                                               
GETS100  MVC   IOAREA(64),32(R6)                                                
         GOTO1 DATAMGR,DMCB,(X'01',DMREAD),FIL1+1,IOAREA,IOAREA,DMWORK          
         CLI   8(R1),0                                                          
         BNE   ERROR2                                                           
         LA    RE,IOAREA                                                        
         SR    RF,RF                                                            
         ICM   RF,3,DMCB+22        PICK UP RECLEN                               
         SR    R7,R7                                                            
         ICM   R7,3,0(R6)                                                       
         SH    R7,=H'32'                                                        
         LA    R6,32(R6)                                                        
         CR    R7,RF                                                            
         BNE   ERROR2                                                           
         CLCL  R6,RE               COMPARE PREV WITH FILE                       
         BNE   ERROR2                                                           
         B     GETS990                                                          
*                                                                               
ERROR2   L     R6,=A(IOAREA2)                                                   
         PUT   RCVTAPE,(R6)                                                     
         AP    FILEERR,=P'1'       BUMP FILE ERRORS                             
         B     GETS990                                                          
*                                                                               
GETS990  SR    R7,R7               COPY IO1 TO IO2                              
         SR    RF,RF                                                            
         L     R6,=A(IOAREA2)                                                   
         LR    RE,R3                                                            
         ICM   RF,3,0(RE)                                                       
         LR    R7,RF                                                            
         MVCL  R6,RE                                                            
*                                                                               
         MVC   LASTKEY,32(R3)      SAVE LASTKEY                                 
*                                                                               
         B     GETS010             GET NEXT                                     
*                                                                               
GETSORTX CLOSE RCVTAPE                                                          
         B     EXIT1                                                            
         EJECT                                                                  
*************************************************************                   
*        GET FSESYS FROM 0(R2) (R1)=EX LEN                  *                   
*************************************************************                   
VALSYS   NTR1                                                                   
         LR    RF,R1                                                            
         L     R4,=V(SELIST)       MUST HAVE SYSFACS                            
         BAS   RE,SETBXLE          SET BXLE                                     
         USING SELISTD,R4                                                       
VALSYS0  EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SENAME      TEST NAME                                    
         BE    VALSYS1                                                          
         BXLE  R4,R0,VALSYS0       NEXT                                         
         B     CERRSYS             ERROR SE SYS NOT FOUND                       
*                                                                               
VALSYS1  MVC   FSESYS,SESYS        SET NUMBER                                   
         MVC   FOVSYS,SEOVSYS                                                   
         MVC   FSENAME,SENAME                                                   
         B     EXIT1                                                            
         DROP  R4                                                               
SETBXLE  LH    R0,0(R4)            SET BXLE                                     
         L     R1,2(R4)                                                         
         LA    R4,6(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        PRINT ROUTINES                                     *                   
*************************************************************                   
PRINTI   ST    RE,SAVERE                                                        
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=P'1'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
*                                                                               
PRINTX   ST    RE,SAVERE           CLOSE PRINT                                  
         CLOSE SYSPRINT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        CARD VALIDATION ERROR                              *                   
*************************************************************                   
CERRSYS  LA    R1,=C'INVALID SYSTEM  '                                          
         B     CERRX                                                            
CERRCARD LA    R1,=C'INVALID CARD    '                                          
         B     CERRX                                                            
*                                                                               
CERRX    LA    RF,PLINE+1                                                       
CERRX1   MVC   0(1,RF),0(R2)                                                    
         CLI   0(RF),C' '                                                       
         BE    CERRX2                                                           
         CLI   0(RF),C','                                                       
         BE    CERRX2                                                           
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         B     CERRX1                                                           
*                                                                               
CERRX2   LA    RF,1(RF)                                                         
         MVC   0(13,RF),=C'*** ERROR ***'                                       
         LA    RF,14(RF)                                                        
         MVC   0(16,RF),0(R1)                                                   
         BAS   RE,PRINTL                                                        
*                                                                               
         B     XMOD1                                                            
         EJECT                                                                  
************************************************************                    
*        CONSTANTS                                         *                    
************************************************************                    
DMRFIL   DC    CL8'RECOVERY'                                                    
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
GETREC   DC    CL8'GETREC'                                                      
OPEN     DC    CL8'OPEN'                                                        
MAXLINE  DC    P'60'                                                            
COPY     DC    C'N'                                                             
*                                                                               
DATAMGR  DC    V(DATAMGR)                                                       
*                                                                               
DMFLIST  DC    C'N...... '                                                      
         DC    C'N...... '                                                      
         DC    C'N...... '                                                      
         DC    C'NCTFILE '                                                      
         DC    C'X'                                                             
DMDA     DC    F'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
************************************************************                    
*        TABLE OF SYSTEMS AND FILES                        *                    
************************************************************                    
*                                                                               
* VALTAB       X'SYS',AL1(KEYLEN),AL1(STATLEN)                                  
*              X'EX#',CL7'DIRNAM'                                               
*              X'EX#',CL7'FILNAM'                                               
*              X'00 ',CL7'RCVNAM'                                               
*                                                                               
VALTAB   DS    0CL26                                                            
         DC    X'04',AL1(20)                                                    
         DC    X'41',C'MEDDIR '                                                 
         DC    X'42',C'MEDFIL '                                                 
         DC    X'08',C'MEDRCV '                                                 
         DC    X'05',AL1(32)                                                    
         DC    X'51',C'MPLDIR '                                                 
         DC    X'52',C'MPLFIL '                                                 
         DC    X'04',C'MPLRCV '                                                 
         DC    X'06',AL1(42)                                                    
         DC    X'69',C'ACCDIR '                                                 
         DC    X'6A',C'ACCMST '                                                 
         DC    X'08',C'ACCRCV '                                                 
         DC    X'07',AL1(32)                                                    
         DC    X'71',C'FEEDIR '                                                 
         DC    X'72',C'FEEFIL '                                                 
         DC    X'04',C'FEERCV '                                                 
         DC    X'09',AL1(32)                                                    
         DC    X'91',C'MBADIR '                                                 
         DC    X'92',C'MBAFIL '                                                 
         DC    X'08',C'MBARCV '                                                 
         DC    X'0A',AL1(32)                                                    
         DC    X'AE',C'GENDIR '                                                 
         DC    X'AF',C'GENFIL '                                                 
         DC    X'04',C'CTRCVR '                                                 
         DC    X'FF'                                                            
         EJECT                                                                  
*************************************************************                   
*        DCBS AND SORT CARDS                                *                   
*************************************************************                   
SORTCARD DC    C'SORT FIELDS=(33,??,A,27,03,A,05,04,A),FORMAT=BI,WORK=1*        
                '                                                               
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(2048,,,,) '                              
*                                                                               
RCVTAPE  DCB   DDNAME=RCVTAPE,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=27648,LRECL=8200,BUFNO=2                                 
*                                                                               
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(133)          
         EJECT                                                                  
*************************************************************                   
*        PRINT TITLE                                        *                   
*************************************************************                   
TITLE    DC    CL133' '                                                         
         ORG   TITLE                                                            
         DC    C'1'                                                             
         DC    C'                                 '                             
         DC    C'                                 '                             
         DC    C'                                 '                             
         DC    C'                                 '                             
         ORG                                                                    
TITLE1   DC    CL133' '                                                         
         ORG   TITLE1                                                           
         DC    C'1',X'40'                                                       
         DC    C'-------------------------------------------'                   
         DC    C'------------- PARAMETER CARDS -------------'                   
         DC    C'-------------------------------------------'                   
         ORG                                                                    
SPACES   DC    133C' '                                                          
         EJECT                                                                  
************************************************************                    
*        UTL AND SSB                                       *                    
************************************************************                    
         DS    0D                                                               
SSB      DC    X'0000FF',X'40',4X'00',CL8' ',32X'00',A(0),204X'00'              
UTL      DC    F'0',X'0A',XL3'00',XL56'00'                                      
         EJECT                                                                  
************************************************************                    
*        TRACK BUFFER AND IO AREAS                         *                    
************************************************************                    
IOAREA   DS    4096C                                                            
IOAREA2  DS    4096C                                                            
*                                                                               
TRK      CSECT                                                                  
         DS    60000C                                                           
         EJECT                                                                  
************************************************************                    
*        WORKING STORAGE DSECT                             *                    
************************************************************                    
WORKD    DSECT                                                                  
SAVERD   DS    F                                                                
SAVERE   DS    F                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
KEYLEN   DS    X                                                                
STATLEN  DS    X                                                                
DMCB     DS    6F                                                               
*                                                                               
FIL1     DS    XL8                                                              
FIL2     DS    XL8                                                              
FSESYS   DS    X                                                                
FOVSYS   DS    X                                                                
FSENAME  DS    CL7                                                              
*                                                                               
COPYERR  DS    PL4                                                              
FILEERR  DS    PL4                                                              
LINE     DS    PL2                                                              
PAGE     DS    PL4                                                              
PLINE    DS    CL133                                                            
*                                                                               
LASTKEY  DS    XL64                                                             
WORK     DS    CL32                                                             
DMWORK   DS    12D                                                              
DA       DS    F                                                                
WORKX    EQU   *                                                                
         EJECT                                                                  
************************************************************                    
*        OTHER DSECTS                                      *                    
************************************************************                    
*FADSECTS                                                                       
       ++INCLUDE FADSECTS                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DDVERIFY  03/05/13'                                      
         END                                                                    
