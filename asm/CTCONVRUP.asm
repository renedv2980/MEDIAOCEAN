*          DATA SET CTCONVRUP  AT LEVEL 091 AS OF 02/12/98                      
*PHASE CONVRUP                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HEXOUT                                                                 
         TITLE 'CONVRUP - REPORT ON USER PROFILE SETTINGS'                      
CONVRUP  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**CRUP**,RA,WORK=A(WORKC),CLEAR=YES                  
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
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,WORK,IO                             
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
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,CTIKEY,CTIKEY                        
*                                                                               
MAIN010  GOTO1 VDATAMGR,DMCB       GET FIRST/NEXT RECORD                        
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
         CLI   CTIKTYP,C'I'        CONVERT ID RECS                              
         BE    MAIN020                                                          
         CLI   CTIKTYP,C'T'        CONVERT TERMINAL RECS                        
         BE    MAIN020                                                          
         CLI   CTIKTYP,C'0'        CONVERT PERSONAL AUTH RECS                   
         BE    MAIN020                                                          
*                                                                               
         B     MAIN080             PUT THE REST BACK TO TAPE                    
*                                                                               
MAIN020  MVI   GTSYSN,X'0A'        SET CONTROL SYSTEM                           
         BAS   RE,GETSYS           GET SYSTEM ELEMENT                           
         BNE   MAIN080             NOT FOUND SO IGNORE                          
*                                                                               
         L     R3,ASYSEL                                                        
         MVI   FPROGN,X'03'        PROG=PROFILE                                 
         BAS   RE,FPROG                                                         
         BNE   MAIN080             NOT FOUND SO IGNORE                          
         OC    FPROGA,FPROGA                                                    
         BZ    MAIN022                                                          
         TM    FPROGA,X'10'                                                     
         BZ    MAIN080                                                          
         B     MAIN040                                                          
MAIN022  TM    FPALLA,X'10'                                                     
         BZ    MAIN080                                                          
         DC    H'0'                                                             
         B     MAIN040                                                          
*                                                                               
MAIN030  L     R3,ASYSEL                                                        
         MVI   FPROGN,X'08'        PROG=FUS                                     
         BAS   RE,FPROG                                                         
         BNE   MAIN080             NOT FOUND SO IGNORE                          
         OC    FPROGA,FPROGA                                                    
         BZ    MAIN032                                                          
         TM    FPROGA,X'10'                                                     
         BZ    MAIN080                                                          
         B     MAIN040                                                          
MAIN032  TM    FPALLA,X'10'                                                     
         BZ    MAIN080                                                          
         B     MAIN040                                                          
*                                                                               
MAIN040  BAS   RE,PLINE            PRINT DETAILS                                
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
         MVC   PLRID(10),CTTKTID   LUID                                         
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
*                                                                               
PLI300   MVC   P,PLINED            COPY PLINE                                   
         GOTO1 VPRINTER                                                         
         B     EXITY                                                            
         DROP  R2                                                               
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
         B     XIT                                                              
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
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
COMFACS  DS    0A                                                               
VDATAMGR DC    V(DATAMGR)                                                       
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
WORK     DS    XL256                                                            
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'091CTCONVRUP 02/12/98'                                      
         END                                                                    
