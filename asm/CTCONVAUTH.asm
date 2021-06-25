*          DATA SET CTCONVAUTH AT LEVEL 106 AS OF 05/01/02                      
*          DATA SET CTCONVAUTH AT LEVEL 101 AS OF 27/07/94                      
*PHASE CONVAUTH                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HEXOUT                                                                 
*INCLUDE FATABOFF                                                               
         TITLE 'CONVAUTH - CONVERT ID/PASSWORD RECORDS'                         
CONVAUT  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**CONV**,RA,WORK=A(WORKC),CLEAR=YES                  
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
*                                                                               
         L     R9,VCPRINT          INITIALISE PRINTER                           
         USING DPRINT,R9                                                        
         MVC   TITLE(23),=C'CONTROL FILE CONVERSION'                            
         GOTO1 VPRINTER                                                         
*                                                                               
         BAS   RE,INIT             OPEN FILES ECT                               
*                                                                               
         BAS   RE,MAIN             MAIN CONVERSION                              
*                                                                               
XBASE    XBASE                     PROG EXIT                                    
*                                                                               
EXITN    LTR   RB,RB               EXIT CC=NEQ                                  
         B     EXIT                                                             
EXITY    CR    RB,RB               EXIT CC=EQU                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        INITIALISATION / OPEN FILES                        *                   
*************************************************************                   
         SPACE 1                                                                
INIT     NTR1                                                                   
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 ADATAMGR,DMCB,DMOPEN,CONTROL,WORK,IO                             
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)    OPEN TAPEOUT                                 
         B     EXITY                                                            
         EJECT                                                                  
*************************************************************                   
*        MAIN                                               *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
*                                                                               
         LA    R2,IO                                                            
         USING CTIKEY,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         GOTO1 ADATAMGR,DMCB,DMRDHI,CTFILE,CTIKEY,CTIKEY                        
*                                                                               
MAIN010  GOTO1 ADATAMGR,DMCB       GET FIRST/NEXT RECORD                        
         LA    R0,DMRSEQ                                                        
         ST    R0,DMCB                                                          
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'         TEST FOR EOF                                 
         BNZ   MAIN090                                                          
         DC    H'0'                OTHER ERRORS ARE DEADLY                      
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,CTILEN         GET RECORD LEN                               
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL           SET TAPE IO LEN                              
*                                                                               
         CLC   CTIKTYP(2),=C'3P'                                                
         BE    MAIN010                                                          
*        CLI   CTIKTYP,C'T'        CONVERT TERMINAL RECS                        
*        BE    MAIN020                                                          
*        CLI   CTIKTYP,C'0'        CONVERT PERSONAL AUTH RECS                   
*        BE    MAIN020                                                          
*                                                                               
         B     MAIN080             PUT THE REST BACK TO TAPE                    
*                                                                               
MAIN020  MVI   GTSYSN,X'04'        SET MEDIA SYSTEM CODE                        
         BAS   RE,GETSYS           GET SYSTEM ELEMENT                           
         BNE   MAIN080             NOT FOUND SO WRITE BACK                      
         L     R3,ASYSEL                                                        
*                                                                               
         MVI   FPROGN,X'14'        PROG=REQ                                     
         BAS   RE,FPROG                                                         
         BNE   MAIN036                                                          
*                                                                               
         CLC   FPROGA,=XL2'0000'   CHECK PROG=NO                                
         BE    MAIN040                                                          
         BNE   *+14                                                             
*                                                                               
MAIN036  CLC   FPALLA,=XL2'0000'   NOT FOUND CHECK ALL=NO                       
         BE    MAIN037                                                          
         B     MAIN080                                                          
*                                                                               
MAIN037  MVI   FPROGN,X'04'        LOOK FOR ANF                                 
         BAS   RE,FPROG                                                         
         BNE   MAIN040                                                          
         CLC   FPROGA,=XL2'0000'   CHECK PROG=NO                                
         BE    MAIN040                                                          
         B     MAIN080                                                          
*                                                                               
MAIN040  EQU   *                                                                
         BAS   RE,PLINE            PRINT DETAILS                                
*                                                                               
MAIN080  PUT   TAPEOUT,IOL         PUT TO TAPE                                  
         B     MAIN010                                                          
*                                                                               
MAIN090  CLOSE (TAPEOUT)           CLOSE TAPE & EXIT                            
         B     XBASE                                                            
         EJECT                                                                  
*************************************************************                   
*        GET   SYSTEM ELEMENT      R2=A(RECORD)             *                   
*        ENTRY GTSYSN=SYSTEM NUMBER                         *                   
*        EXIT  CC=EQU ASYSEL=A(SYSTEM ELEMENT)              *                   
*************************************************************                   
         SPACE 1                                                                
GETSYS   NTR                                                                    
*                                                                               
GETS010  LA    R3,CTIDATA                                                       
         SR    R0,R0                                                            
         USING CTSYSD,R3                                                        
GETS020  CLI   CTSYSEL,0           END OF RECORD                                
         BE    GETS090                                                          
         CLI   CTSYSEL,CTSYSELQ    IS THIS A SYSTEM ELEMENT                     
         BNE   GETS080                                                          
         CLC   CTSYSNUM,GTSYSN     IS THIS OUR SYSTEM ELEMENT                   
         BNE   GETS080                                                          
*                                                                               
         TM    5(R3),X'80'         TEST WE HAVE CONVERTED RECORD                
         BNO   EXITN                                                            
*                                                                               
         ST    R3,ASYSEL           SAVE A(ELEMENT)                              
         B     EXITY               EXIT CC=EQU                                  
*                                                                               
GETS080  IC    R0,CTSYSLEN         TRY NEXT ELEMENT                             
         AR    R3,R0                                                            
         B     GETS020                                                          
*                                                                               
GETS090  B     EXITN               EXIT CC=NEQ                                  
         DROP  R3                                                               
         EJECT                                                                  
*************************************************************                   
*        ADD SYSTEM ELEMENT / R2=A(RECORD) ADDS WITH ALL=N  *                   
*        ENTRY GTSYSN=SYSTEM NUMBER                         *                   
*        EXIT  CC=EQU ASYSEL=A(SYSTEM ELEMENT)              *                   
*************************************************************                   
         SPACE 1                                                                
ADDSYS   NTR                                                                    
         LA    R3,WORK             TOO ADVENTUROUS FOR NOW                      
         USING CTSYSD,R3                                                        
         B     EXITN                                                            
         EJECT                                                                  
*************************************************************                   
*        FIND PROGRAM ENTRY                                 *                   
*        ENTRY R3=A(SYSTEM ELEMENT)  EXIT CC=NEQ NOT FOUND  *                   
*        FPROGN=PGM NUMBER                                  *                   
*        FPROGA=PGM AUTH ENTRY RETURNED IF CC=EQU           *                   
*        FPALLA =ALL PROGRAM AUTH                           *                   
*************************************************************                   
         SPACE 1                                                                
         USING CTSYSD,R3                                                        
FPROG    NTR1                                                                   
         XC    FPROGA,FPROGA       CLEAR RETURN VALUES                          
         XC    FPALLA,FPALLA                                                    
*                                                                               
         CLI   CTSYSEL,CTSYSELQ    MAKE SURE WE HAVE A SYS ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FPALLA,CTSYSALL     SET ALL AUTH                                 
         CLI   CTSYSLEN,CTSYSPGM-CTSYSD                                         
         BE    EXITN               ALL=XXXX ELEMENT ONLY                        
*                                                                               
         SR    R0,R0               GET LEN-16 INTO R0                           
         IC    R0,CTSYSLEN                                                      
         SH    R0,=H'16'                                                        
*                                                                               
         LA    R5,CTSYSPGM                                                      
FPRG010  CLC   0(1,R5),FPROGN      FIND PROGRAM ENTRY                           
         BNE   FPRG020                                                          
         MVC   FPROGA,1(R5)        SET PROGA AUTH                               
         B     EXITY                                                            
*                                                                               
FPRG020  LA    R5,3(R5)                                                         
         SH    R0,=H'3'                                                         
         BP    FPRG010             TRY NEXT PGM                                 
         B     EXITN               ALL PGMS SEARCHED                            
         DROP  R3                                                               
         EJECT                                                                  
*************************************************************                   
*        SET PROGRAM ENTRY                                  *                   
*        ENTRY R3=A(SYSTEM ELEMENT)  EXIT CC=NEQ NOT FOUND  *                   
*        FPROGN=PGM NUMBER TO SET                           *                   
*        FPROGA=PGM AUTH ENTRY TO BE SET (AUTH IS OR'D IN)  *                   
*************************************************************                   
         SPACE 1                                                                
         USING CTSYSD,R3                                                        
SPROG    NTR1                                                                   
*                                                                               
         CLI   CTSYSEL,CTSYSELQ    MAKE SURE WE HAVE A SYS ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CTSYSLEN,CTSYSPGM-CTSYSD                                         
         BE    EXITN               ALL=XXXX ELEMENT ONLY                        
*                                                                               
         SR    R0,R0               GET LEN-16 INTO R0                           
         IC    R0,CTSYSLEN                                                      
         SH    R0,=H'16'                                                        
*                                                                               
         LA    R5,CTSYSPGM                                                      
SPRG010  CLC   0(1,R5),FPROGN      FIND PROGRAM ENTRY                           
         BNE   SPRG020                                                          
**TEMP   OC    1(2,R5),FPROGA      OR PROGA AUTH                                
         MVC   1(2,R5),FPROGA      MOVE PROGA AUTH                              
         B     EXITY                                                            
*                                                                               
SPRG020  LA    R5,3(R5)                                                         
         SH    R0,=H'3'                                                         
         BP    SPRG010             TRY NEXT PGM                                 
         B     EXITN               ALL PGMS SEARCHED                            
         DROP  R3                                                               
         EJECT                                                                  
*************************************************************                   
*        ADD PROGRAM ENTRY / R2=IO & TAPEREC MUST BE AT IOL *                   
*        ENTRY R3=A(SYSTEM ELEMENT) CC=NEQ ALREADY EXISTS   *                   
*        FPROGN=PGM NUMBER TO ADD   PROG IS ADDED AS AUTH=N *                   
*************************************************************                   
         SPACE 1                                                                
         USING CTSYSD,R3                                                        
ADDPROG  NTR1                                                                   
*                                                                               
         CLI   CTSYSEL,CTSYSELQ    MAKE SURE WE HAVE A SYS ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CTSYSLEN,CTSYSPGM-CTSYSD                                         
         BNE   *+12                ALL=XXXX ELEMENT ONLY                        
         LA    R5,CTSYSPGM                                                      
         B     ADPRG015                                                         
*                                                                               
         SR    R0,R0               GET LEN-16 INTO R0                           
         IC    R0,CTSYSLEN                                                      
         SH    R0,=H'16'                                                        
*                                                                               
         LA    R5,CTSYSPGM                                                      
ADPRG010 CLC   0(1,R5),FPROGN      FIND PROGRAM ENTRY                           
         BL    ADPRG020                                                         
         BE    EXITN               BAD NEWS IF FOUND                            
*                                                                               
ADPRG015 SR    RF,RF                                                            
         ICM   RF,3,CTILEN         RE=REC LEN                                   
         LR    RE,R5                                                            
         SR    RE,R2               RE=POS SO FAR                                
         SR    RF,RE               RF=REMAINING LEN                             
         LA    RF,1(RF)            ADD 1 FOR FINAL ZERO                         
*                                                                               
         GOTO1 MOVE,PARM,(R5),(0,3(R5)),(RF)                                    
         XC    0(3,R5),0(R5)                                                    
         MVC   0(1,R5),FPROGN                                                   
*                                                                               
         SR    RE,RE               ADD 3 TO ELEMENT LEN                         
         IC    RE,CTSYSLEN                                                      
         LA    RE,3(RE)                                                         
         STC   RE,CTSYSLEN                                                      
*                                                                               
         SR    RE,RE               ADD 3 TO RECLEN                              
         ICM   RE,3,CTILEN                                                      
         LA    RE,3(RE)                                                         
         STCM  RE,3,CTILEN                                                      
*                                                                               
         LA    RE,4(RE)            RESET TAPE IOL                               
         SLL   RE,16                                                            
         STCM  RE,15,IOL                                                        
*                                                                               
         B     EXITY               EXIT OK                                      
*                                                                               
ADPRG020 LA    R5,3(R5)                                                         
         SH    R0,=H'3'                                                         
         BP    ADPRG010            TRY NEXT PGM                                 
         B     ADPRG015            ALL PGMS SEARCHED                            
         DROP  R3                                                               
         EJECT                                                                  
*************************************************************                   
*        PRINT REC R2=A(RECORD)                             *                   
*************************************************************                   
         SPACE 1                                                                
PLINE    NTR                                                                    
         MVC   PLINED,SPACES                                                    
*                                                                               
         USING CTIREC,R2                                                        
         CLI   CTIKTYP,CTIKTYPQ    TEST FOR ID RECORD                           
         BNE   PLI100                                                           
         MVC   PLRTYPE,=CL11'USER ID '                                          
*                                                                               
         OC    CTIKID(1),CTIKID    USER ID TEXT                                 
         BZ    *+14                                                             
         MVC   PLRID(10),CTIKID                                                 
         B     PLI300                                                           
*                                                                               
         MVC   PLRID(4),=C'ID=#'   USER ID NUMBER                               
         EDIT  (B2,CTIKNUM),(6,PLRID+4),ALIGN=LEFT                              
         B     PLI300                                                           
*                                                                               
         USING CTTREC,R2                                                        
PLI100   CLI   CTTKTYP,CTTKTYPQ                                                 
         BNE   PLI200                                                           
         MVC   PLRTYPE,=CL11'TERMINAL'                                          
*                                                                               
         OC    CTTKTID,CTTKTID     TERMINAL LUID                                
         BZ    *+14                                                             
         MVC   PLRID(8),CTTKTID    LUID                                         
         B     PLI300                                                           
*                                                                               
         MVC   PLRID(10),CTTKPASS  TERMINAL PASSWORD                            
         B     PLI300                                                           
*                                                                               
         USING CT0REC,R2                                                        
PLI200   CLI   CT0KTYP,CT0KTEQU                                                 
         BNE   PLI300                                                           
         MVC   PLRTYPE,=CL11'PASSWORD'                                          
*                                                                               
         MVC   PLAGY,CT0KAGY                                                    
*                                                                               
         OC    CT0KEYS(20),CT0KEYS                                              
         BNZ   PLI210                                                           
         MVC   PLRID(4),=C'PW=#'   PASSWORD NUMBER                              
         EDIT  (B2,CT0KNUM),(6,PLRID+4),ALIGN=LEFT                              
         B     PLI300                                                           
*                                                                               
PLI210   OC    CT0KEYS(12),CT0KEYS                                              
         BNZ   *+14                                                             
         MVC   PLRID(10),CT0KCODE  PASSWORD                                     
         B     PLI300                                                           
*                                                                               
         MVC   PLRID+00(02),CT0KOFFC                                            
         MVC   PLRID+03(18),CT0KLAST                                            
         MVC   PLRID+22(01),CT0KFI                                              
         MVC   PLRID+24(01),CT0KMI                                              
         B     *+8                                                              
*                                                                               
PLI300   B     EXITY                                                            
         BAS   RE,DISPSYS                                                       
         MVC   PLRPROG,BLOCK1                                                   
         MVC   P,PLINED            COPY PLINE                                   
         GOTO1 VPRINTER                                                         
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
*        DISPLAY SYSTEM PROGRAM AUTHS                       *                   
*************************************************************                   
         SPACE 1                                                                
DISPSYS  NTR1                                                                   
         LA    R8,BLOCK1                                                        
         MVC   BLOCK1,SPACES                                                    
         MVI   GTSYSN,X'04'        SET MEDIA SYSTEM CODE                        
         BAS   RE,GETSYS           GET SYSTEM ELEMENT                           
         BNE   DISPSYSX            NOT FOUND SO EXIT                            
*                                                                               
         L     R3,ASYSEL           R3=SYSTEM ELEMENT                            
         USING CTSYSEL,R3                                                       
*                                                                               
         XC    APGMS,APGMS         SET UP GETSE CALL                            
         XC    SYSSE,SYSSE                                                      
         MVC   SYSOV,CTSYSNUM                                                   
         BAS   RE,GETSE                                                         
*                                                                               
DISPSYS1 LA    R4,CTSYSPGM         R4=A(AUTHS)                                  
         ZIC   R9,CTSYSLEN         R9=PGM NUMBER                                
         LA    R4,CTSYSALL                                                      
         MVC   PGNAME,=CL4'ALL '   DO ALL= FIRST                                
         B     DISPSYS4                                                         
*                                                                               
DISPSYS2 CH    R9,=H'16'                                                        
         BL    DISPSYSX                                                         
         MVI   0(R8),C','          PUT IN THE ","                               
         LA    R8,1(R8)                                                         
         MVC   PROGRAM,0(R4)                                                    
         LA    R4,1(R4)                                                         
         BAS   RE,GETPGAN          GET PROGRAM NAME                             
         CLI   PROGRAM,X'FF'       SET TO X'FF' IF N/F                          
         BE    DISPSYS6                                                         
*                                                                               
DISPSYS4 EQU   *                                                                
         MVC   0(4,R8),PGNAME      PUT NAME=                                    
         LA    R8,3(R8)                                                         
         CLI   0(R8),C' '          NAME CAN BE 3 OR 4                           
         BE    *+8                                                              
         LA    R8,1(R8)                                                         
         MVI   0(R8),C'='                                                       
*                                                                               
         MVI   1(R8),C'Y'          DEFAULT TO NAME=Y                            
         CLC   0(2,R4),=X'000F'                                                 
         BE    DISPSYS6                                                         
         MVI   1(R8),C'N'          NEXT TRY NAME=N THEN NAME=XXXX               
         CLC   0(2,R4),=X'0000'                                                 
         BE    DISPSYS6                                                         
         GOTO1 =V(HEXOUT),PARM,(R4),1(R8),2,=C'TOG'                             
         LA    R8,3(R8)                                                         
*                                                                               
DISPSYS6 LA    R8,2(R8)                                                         
         LA    R4,2(R4)                                                         
         SH    R9,=H'3'                                                         
         B     DISPSYS2            NEXT PROGRAM                                 
*                                                                               
DISPSYSX B     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
*************************************************************                   
*        GET SYSTEM ENTRY AND PGM LIST                      *                   
*************************************************************                   
         SPACE 1                                                                
GETSE    NTR1                                                                   
         L     R4,=V(SELIST)       MUST HAVE FATABOFF                           
         BAS   RE,SETBXLE                                                       
         USING SELISTD,R4                                                       
GETSE0   CLI   SYSSE,0             IF SYSSE IS ZERO                             
         BNE   GETSE1                                                           
         CLC   SYSOV,SEOVSYS       TEST GTSYS WITH OVSYS                        
         BE    GETSE2                                                           
GETSE1   CLC   SYSSE,SESYS         TEST SE NUMBER                               
         BE    GETSE2                                                           
         BXLE  R4,R0,GETSE0        NEXT                                         
         MVI   SYSOV,0                                                          
         XC    SYSN,GTSYSN                                                      
         B     EXITN               ERROR EXIT NOT FOUND                         
*                                                                               
GETSE2   MVC   SYSOV,SEOVSYS       FOUND                                        
         MVC   SYSN,SENAME         SET NAME                                     
         MVC   APGMS,SEPGMS        SAVE A(PGMS)                                 
         B     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
*************************************************************                   
*        GET PROGRAM NAME                                   *                   
*************************************************************                   
         SPACE 1                                                                
GETPGAN  NTR1                                                                   
         MVC   PGNAME,=C'P/NN'                                                  
         ICM   R3,15,APGMS                                                      
         BZ    GETPGANX                                                         
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING PGMLSTD,R3                                                       
*                                                                               
GETPGAN2 CLC   PGMNUM,PROGRAM      MATCH ON PROGRAM NUMBER                      
         BE    GETPGANY                                                         
         BXLE  R3,RE,GETPGAN2                                                   
         GOTO1 =V(HEXOUT),PARM,PROGRAM,PGNAME+2,1                               
         B     GETPGANX                                                         
GETPGANY MVC   PGNAME,PGMNAME                                                   
GETPGANX B     EXITY                                                            
         EJECT                                                                  
*************************************************************                   
*        NON DESTRUCTIVE MOVE ROUTINE                       *                   
*************************************************************                   
         SPACE 1                                                                
MOVE     NTR1                                                                   
         L     R2,0(R1)            P1 SOURCE                                    
         L     R4,4(R1)            P2 DEST                                      
         L     R3,8(R1)            P3 LENGTH                                    
         CR    R4,R2               IS IT DOWNWARDS ?                            
         BH    RMOVE                                                            
NMOVE    LR    R5,R3                                                            
         MVCL  R4,R2               NO SO MVCL                                   
         B     EXITY                                                            
EXMOVE   MVC   0(0,R4),0(R2)                                                    
RMOVE    LR    R5,R4                                                            
         SR    R5,R2               R5=OFFSET                                    
         CR    R5,R3               WILL IT OVERLAP ?                            
         BNL   NMOVE                                                            
         CH    R5,=H'256'                                                       
         BNH   M1                                                               
         LH    R5,=H'256'                                                       
M1       AR    R2,R3               GO TO END                                    
         AR    R4,R3                                                            
M0       CR    R3,R5               IS LENGTH < BLOCK                            
         BNL   M2                                                               
         LR    R5,R3               IF YES BLOCK = LENGTH                        
M2       SR    R2,R5                                                            
         SR    R4,R5               BACK 1 BLOCK                                 
         BCTR  R5,0                                                             
         EX    R5,EXMOVE           MOVE 1 BLOCK                                 
         LA    R5,1(R5)                                                         
         SR    R3,R5               SUB BLOCK FROM LENGTH                        
         BNE   M0                                                               
XIT      B     EXITY                                                            
         EJECT                                                                  
SETBXLE  LH    R0,0(R4)            SET BXLE                                     
         L     R1,2(R4)                                                         
         LA    R4,6(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
COMFACS  DS    0A                                                               
ADATAMGR DC    V(DATAMGR)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
         SPACE 1                                                                
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
         SPACE 1                                                                
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
         SPACE 1                                                                
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAGS    DS    X                                                                
*                                                                               
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
*                                                                               
FPROGN   DS    X                                                                
FPROGA   DS    XL2                                                              
FPALLA   DS    XL2                                                              
GTSYSN   DS    X                                                                
ASYSEL   DS    A                                                                
SAVAUTH  DS    XL2                                                              
*                                                                               
APGMS    DS    A                                                                
SYSN     DS    CL8                                                              
SYSSE    DS    X                                                                
SYSOV    DS    X                                                                
PROGRAM  DS    X                                                                
PGNAME   DS    CL4                                                              
*                                                                               
WORK     DS    XL256                                                            
BLOCK1   DS    XL256                                                            
*                                                                               
PLINED   DS    0CL166              PRINT LINE DSECT                             
PLRTYPE  DS    CL11                RECORD TYPE                                  
         DS    CL1                                                              
PLAGY    DS    CL2                 AGENCY                                       
         DS    CL1                                                              
PLRID    DS    CL25                KEY IDENTIFIER                               
         DS    CL1                                                              
PLRSYS   DS    CL3                 SYSTEM                                       
         DS    CL2                                                              
PLRPROG  DS    CL120               PROGRAM AUTHS                                
*                                                                               
IOL      DS    F                                                                
IO       DS    2000X                                                            
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
WORKC    CSECT                     ** WORKING STORAGE POOL **                   
         DS    (64*1024)X                                                       
         SPACE 1                                                                
*FADSECTS                                                                       
       ++INCLUDE FADSECTS                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'106CTCONVAUTH05/01/02'                                      
         END                                                                    
