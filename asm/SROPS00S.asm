*          DATA SET SROPS00S   AT LEVEL 177 AS OF 05/01/02                      
*PHASE T10F00A                                                                  
*INCLUDE GETBOOK                                                                
         TITLE '$OPS - HANDLE OPERATOR COMMANDS'                                
OPS      CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**$OPS**,RA,R9,CLEAR=YES,RR=RE                       
         USING WORKD,RC            RC=A(W/S)                                    
         ST    RD,SAVERD                                                        
         ST    RE,RELO                                                          
         MVC   SRPARS,0(R1)        SAVE S/R PARAMETER LIST                      
         L     R7,SRPAR1                                                        
         USING SYSFACD,R7          R9=A(SYSFACS)                                
         L     R8,VSSB                                                          
         USING SSBD,R8             R8=A(SSB)                                    
*                                                                               
         OI    SSBOPECB,X'80'      SET SROPS IN PROGRESS                        
*                                                                               
         MVC   SYSNAME(3),SSBSYSNA                                              
         MVI   SYSNAME+3,C'+'                                                   
         TM    SSBSTAT4,SSBSAOR    AM I AN AOR                                  
         BZ    *+8                                                              
         MVI   SYSNAME+3,C'A'      SET FACXXXA                                  
         MVC   ATCB,SSBTKADR                                                    
         MVC   ATIA,SRPAR2                                                      
         MVC   AUTL,SRPAR3                                                      
         L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         OI    TPRGIND,X'14'       SET CONVERTED MAX IOS                        
         LA    RF,MYPGMLST                                                      
         STCM  RF,7,TASVC          SET DUMMY PGMLST ENTRY ALSO                  
         DROP  R1                                                               
*                                                                               
         MVC   OPFACID,=C'+FACPAK+'                                             
         MVC   OPFACID+4(3),SYSNAME                                             
*                                                                               
         L     R1,SRPAR4                                                        
         USING COMFACSD,R1                                                      
         MVC   ASCANNER,CSCANNER                                                
         DROP  R1                                                               
*                                                                               
         L     R1,=A(IOAREA-WORKD)                                              
         AR    R1,RC                                                            
         ST    R1,AIOAREA                                                       
         L     R1,=A(BUFF1-WORKD)                                               
         AR    R1,RC                                                            
         ST    R1,ABUFF1                                                        
         L     R1,=A(BUFF2-WORKD)                                               
         AR    R1,RC                                                            
         ST    R1,ABUFF2                                                        
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'UPDID'                                          
         L     R1,12(R1)                                                        
         MVC   MYUPDID,0(R1)       GET UPDID FOR SYSTEM                         
*                                                                               
         L     R1,SSBOPECB         TEST FO OPER ECB POSTED                      
         TM    0(R1),X'40'                                                      
         BNZ   OPS050                                                           
OPS040   BAS   RE,DMAIN            MAIN DSPACE COMMS                            
         B     XMOD1                                                            
*                                                                               
OPS050   BAS   RE,MAIN             MAIN OPER COMMS                              
*                                                                               
XMOD1    L     RD,SAVERD                                                        
         BAS   RE,CLEANUP                                                       
         NI    SSBOPECB,255-X'C0'  RESET SROPS IN PROGRESS                      
         SAC   0                                                                
*                                                                               
         XMOD1                                                                  
*                                                                               
XITEQU   CR    RB,RB                                                            
         B     XIT1                                                             
XITNEQ   LTR   RB,RB                                                            
*                                                                               
XIT1     XIT1                                                                   
*                                                                               
CLEANUP  NTR1                      CLEAR OPS COMMAND                            
         SAC   0                                                                
         L     RF,SSBACOMM                                                      
         USING COMLIST,RF                                                       
         ICM   R2,15,COMCIBPT      A(CIB)                                       
         BZ    XIT1                                                             
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         DROP  RF                                                               
*                                                                               
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM                                       *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
*                                                                               
         L     RF,SSBACOMM         SET RF TO COMM BLOCK                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BNE   MAIN010                                                          
         OI    SSBSTAT1,SSBSEOJ    SET EOJ                                      
*                                                                               
         MVC   OPMSGTXT,OPER3      FORCED END OF JOB                            
         BAS   RE,WTO                                                           
         B     MAINX                                                            
*                                                                               
MAIN010  CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    *+10                YES                                          
         BAS   RE,CLEANUP          WHAT DID THE OPERATOR DO?                    
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1               GET DATA LEN IN R1                           
         ICM   R1,3,CIBDATLN                                                    
         BCTR  R1,0                                                             
         MVC   CARD,SPACES         MOVE DATA TO WORK AND SCAN IT                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CARD(0),CIBDATA                                                  
*                                                                               
         BAS   RE,VALCARD          VALIDATE CARD INPUT                          
         BNE   MAIN070             NEQ SO TRY GROUP COMMAND                     
*                                                                               
         TM    FLAG,X'80'          TEST FOR THIS FACPAK ONLY                    
         BZ    MAIN050                                                          
         SR    R1,R1                                                            
         ICM   R1,3,ACTNUM         EXECUTE ACTION NOW                           
         SLL   R1,2                                                             
         EX    0,MAIN100(R1)                                                    
         B     MAINX                                                            
*                                                                               
MAIN050  BAS   RE,SETCOM           SEND COMMAND TO DSPACE                       
         B     MAINX                                                            
         EJECT                                                                  
*************************************************************                   
*        CHECK FOR GROUP COMMAND IN COMMAND RECORD          *                   
*************************************************************                   
         SPACE 1                                                                
MAIN070  LA    R4,IOAREA           BUILD BOOK KEY IN WORK                       
         USING CTJREC,R4                                                        
         XC    CTJKEY,CTJKEY                                                    
         MVI   CTJKTYP,CTCOTYPQ    SET COMMAND TYPE                             
         MVC   CTJKID,COMMAND                                                   
MAIN080  L     RF,=V(GETBOOK)                                                   
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,IOAREA,CARD,VDATAMGR                                   
         TM    8(R1),X'80'                                                      
         BO    MAINX               EOF                                          
         CLI   8(R1),0                                                          
         BNE   ERRX                UNKNOWN COMMAND                              
         MVC   CARD+70(10),SPACES                                               
*                                                                               
         CLI   CARD,C' '           IGNORE BLANKS AND STARS                      
         BE    MAIN080                                                          
         CLI   CARD,C'*'                                                        
         BE    MAIN080                                                          
*                                                                               
         BAS   RE,VALCARD          VALIDATE CARD INPUT                          
         BE    *+6                                                              
         DC    H'0'                                                             
         BNE   ERRX                                                             
*                                                                               
         TM    FLAG,X'80'          TEST FOR THIS FACPAK ONLY                    
         BO    ERRX                                                             
*                                                                               
         BAS   RE,SETCOM           SEND COMMAND TO DSPACE                       
         B     MAIN080                                                          
*                                                                               
MAINX    B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        EXECUTE OPERATOR COMMANDS                          *                   
*************************************************************                   
         SPACE 1                                                                
MAIN100  DC    F'0'                ACTION TABLE                                 
*                                                                               
         BAS   RE,SETEOJ           ADV COMMANDS                                 
         BAS   RE,SETCAN                                                        
         BAS   RE,SETDUMP                                                       
         EJECT                                                                  
*************************************************************                   
*        MAIN DSPACE COMMAND PROGRAM                        *                   
*************************************************************                   
         SPACE 1                                                                
DMAIN    NTR1                                                                   
*                                                                               
         LAM   R2,R2,SSBALET       PICK UP ALET                                 
         SR    R2,R2                                                            
         USING DMDHDR,R2                                                        
         SAC   512                                                              
         L     R2,DHACOMM          SET R2 TO COMMS BLOCK                        
         LA    R0,128                                                           
*                                                                               
         USING DSCOMM,R2                                                        
DMAIN010 CLC   SSBSYSIX,DSCDEST    IS IT FOR ME                                 
         BE    DMAIN020                                                         
         B     DMAIN090                                                         
*                                                                               
*NOP     TM    SSBSTAT4,SSBSAOR    AM I AN AOR                                  
*        BZ    DMAIN090                                                         
*        MVC   BYTE,SSBSTAT4       SET THE X'80'                                
*        OI    BYTE,X'80'                                                       
*        CLC   BYTE,DSCDEST        AND TEST IT AGAIN                            
*        BNE   DMAIN090                                                         
*                                                                               
DMAIN020 MVC   DATA(16),DSCDATA    COPY DATA TO DATA                            
         SR    R1,R1                                                            
         ICM   R1,3,DSCCOMM        R1=ACTION NUMBER                             
         BZ    ERRX                                                             
         CH    R1,=Y(MAXACT)                                                    
         BNH   *+10                                                             
         BAS   RE,CLEANUP          ACTION NUMBER EXCEEDED                       
         DC    H'0'                                                             
*                                                                               
         STH   R1,ACTNUM           SAVE ACTION NUMBER                           
*                                                                               
         SLL   R1,2                * 4 FOR EXECUTE                              
         SAC   0                                                                
*                                                                               
         EX    0,ACTION(R1)        GO DO IT                                     
*                                                                               
         SAC   512                                                              
         LAM   R2,R2,SSBALET                                                    
         OI    DSCFLAG,DSCDONEQ    FLAG IT AS DONE                              
         XC    0(32,R2),0(R2)      CLEAR THE F'IN LOT                           
*                                                                               
DMAIN090 LA    R2,32(,R2)          POINT TO NEXT COMM ENTRY                     
         BCT   R0,DMAIN010         NEXT COMMAND                                 
*                                                                               
DMAINX   B     XIT1                                                             
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
*        EXECUTE COMMANDS                                   *                   
*************************************************************                   
         SPACE 1                                                                
ACTION   DC    F'0'                                                             
         DC    F'0'                NOT DATASPACED                               
         DC    F'0'                ..                                           
         DC    F'0'                ..                                           
         BAS   RE,DSSTRT           4                                            
         BAS   RE,DSSTOP           5                                            
         BAS   RE,DSOPEN           6                                            
         BAS   RE,DSCLOS           7                                            
         BAS   RE,DSQUI            8                                            
         BAS   RE,DSUNQU           9                                            
         BAS   RE,DSREAD           10                                           
         BAS   RE,DSWRIT           11                                           
         BAS   RE,DSBROAD          12                                           
         BAS   RE,DSOPEN           13  QOPEN                                    
*                                                                               
MAXACT   EQU   13                  DONT FORGET TO SET THIS                      
         EJECT                                                                  
*************************************************************                   
*        PRESET WORKING STORAGE                             *                   
*************************************************************                   
         SPACE 1                                                                
DSSTRT   ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         BAS   RE,FINDSYS          SET UP SYSTEM                                
         L     RE,SAVERE                                                        
         B     UPSYS                                                            
*                                                                               
DSSTOP   ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         BAS   RE,FINDSYS          SET UP SYSTEM                                
         L     RE,SAVERE                                                        
         B     DNSYS                                                            
*                                                                               
DSOPEN   ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         MVC   FILNUM,DATA+2                                                    
         BAS   RE,FINDSYS          SET UP SYSTEM                                
         BAS   RE,FINDFIL                                                       
         L     RE,SAVERE                                                        
         B     QOPEN                                                            
*                                                                               
DSCLOS   ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         MVC   FILNUM,DATA+2                                                    
         BAS   RE,FINDSYS          SET UP SYSTEM                                
         BAS   RE,FINDFIL                                                       
         L     RE,SAVERE                                                        
         B     QCLOSE                                                           
*                                                                               
DSQUI    ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         MVC   FILNUM,DATA+2                                                    
         BAS   RE,FINDSYS          SET UP SYSTEM                                
         BAS   RE,FINDFIL                                                       
         L     RE,SAVERE                                                        
         B     QUION                                                            
*                                                                               
DSUNQU   ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         MVC   FILNUM,DATA+2                                                    
         BAS   RE,FINDSYS          SET UP SYSTEM                                
         BAS   RE,FINDFIL                                                       
         L     RE,SAVERE                                                        
         B     QUIOFF                                                           
*                                                                               
DSREAD   ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         BAS   RE,FINDSYS          SET UP SYSTEM                                
         L     RE,SAVERE                                                        
         B     ROSYS                                                            
*                                                                               
DSWRIT   ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         BAS   RE,FINDSYS          SET UP SYSTEM                                
         L     RE,SAVERE                                                        
         B     RWSYS                                                            
*                                                                               
DSBROAD  ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         MVC   BRDNUM,DATA+2                                                    
         BAS   RE,FINDSYS          SET UP SYSTEM                                
         L     RE,SAVERE                                                        
         B     BCAST                                                            
*                                                                               
         EJECT                                                                  
*************************************************************                   
*        SET CANCEL OR DUMP                                 *                   
*************************************************************                   
         SPACE 1                                                                
SETCAN   OI    SSBSTAT1,SSBSEOJ    SET SSB TO EOJ FOR NODUMP                    
SETDUMP  OI    SSBSTAT3,SSBCANCL   FORCE SYSTEM CANCEL                          
         BAS   RE,CLEANUP                                                       
         DC    H'0'                THIS WILL ABEND SYSTEM                       
         EJECT                                                                  
*************************************************************                   
*        STOP ALL ACTIVE SYSTEMS AND SET EOJ                *                   
*************************************************************                   
         SPACE 1                                                                
SETEOJ   NTR1                                                                   
         MVC   OPMSGTXT,OPER2      EOJ ACCEPTED                                 
         BAS   RE,WTO                                                           
*                                                                               
         OI    SSBSTAT1,SSBSEOJ    SET EOJ                                      
*                                                                               
         L     R6,VSELIST                                                       
         LH    R4,0(R6)            SET BXLE REGS                                
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
*                                                                               
         USING SELISTD,R6                                                       
STOP010  ST    R6,ASENTRY          SET A(SENTRY)                                
         TM    SEIND,SEISTRT                                                    
         BNO   STOP020             IF SYSTEM STARTED                            
         CLI   SESYS,1                                                          
         BE    STOP020             AND NOT SERVICE                              
*                                                                               
         BAS   RE,DNSYS            CALL DOWN SYS                                
*                                                                               
STOP020  BXLE  R6,R4,STOP010       BXLE TO NEXT SYSTEM                          
*                                                                               
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        BUILD COMMAND FOR DATASPACE                        *                   
*************************************************************                   
         SPACE 1                                                                
SETCOM   NTR1                                                                   
*                                                                               
         XC    WORK,WORK           BUILD COMLINE IN WORK                        
         LA    R4,WORK                                                          
         USING DSCOMM,R4                                                        
         MVC   DSCSORC,SSBSYSIX    FROM ME                                      
         MVI   DSCSORC+1,0                                                      
         MVC   DSCDEST,ADNUM       TO ADNUM (ZERO FOR ALL)                      
         MVI   DSCDEST+1,0                                                      
         MVC   DSCCOMM,ACTNUM      SET ACTION                                   
*                                                                               
         MVC   DSCDATA(2),SYNUM    SET SYS/FIL INFO IN DATA                     
         MVC   DSCDATA+2(1),FILNUM                                              
*                                                                               
         TM    FLAG,X'40'          IS FILENUM REQUIRED                          
         BZ    *+12                                                             
         CLI   FILNUM,0                                                         
         BE    NODATA                                                           
*                                                                               
         OC    DSCDATA,DSCDATA     MUST BE SOME DATA                            
         BZ    NODATA                                                           
*                                                                               
         TIME  BIN                 SAVE TIME IN DSCTIME                         
         ST    R0,DSCTIME                                                       
*                                                                               
         SR    R2,R2               SET TO DATASPACE                             
         USING DMDSHDR,R2                                                       
         LAM   R2,R2,SSBALET                                                    
         LAM   R3,R3,SSBALET                                                    
         SAC   512                                                              
*                                                                               
SETCOM1  SR    R2,R2                                                            
         ICM   R1,15,SYGROUP       NO GROUP SO JUST DO ONE SYSTEM               
         BZ    SETCOM1A                                                         
         CLI   0(R1),X'FF'         END OF GROUP?                                
         BE    SETCOMX                                                          
         MVC   DSCDATA+1(1),0(R1)  MOVE SYSTEM FROM GROUP                       
*                                                                               
SETCOM1A L     R3,DHAADVS          SET R3 TO ADV SYS BLOCK                      
         LA    R0,32                                                            
SETCOM2  CLI   0(R3),0             EMPTY ADV ENTRY                              
         BE    SETCOM6                                                          
*                                                                               
         CLI   DSCDEST,0           DEST ZERO SEND TO ALL                        
         BE    SETCOM3                                                          
         CLC   DSCDEST(1),10(R3)   DOES DEST MATCH                              
         BNE   SETCOM6                                                          
*                                                                               
SETCOM3  SR    R2,R2               POINT TO DSPACE DHACOMM                      
         LA    RE,128                                                           
         L     R2,4(,R2)                                                        
SETCOM4  L     RF,=F'-1'           SET TO FFS TO GRAB ENTRY                     
         SR    R1,R1                                                            
         CS    R1,RF,0(R2)                                                      
         BNE   SETCOM5                                                          
         MVC   0(32,R2),WORK                                                    
         MVC   6(1,R2),10(R3)      SEND TO THIS ADV                             
*                                                                               
         MVC   WORK1,0(R3)                                                      
         SAC   0                   GO POST THE SSBOPECB                         
         BAS   RE,POSTIT                                                        
         SAC   512                                                              
*                                                                               
         B     SETCOM6                                                          
*                                                                               
SETCOM5  LA    R2,32(,R2)                                                       
         BCT   RE,SETCOM4          ALL ENTRIES FULL                             
         BAS   RE,CLEANUP                                                       
         DC    H'0'                                                             
*                                                                               
SETCOM6  LA    R3,32(,R3)                                                       
         BCT   R0,SETCOM2          ALL 32 DONE                                  
*                                                                               
         ICM   R1,15,SYGROUP       TEST FOR GROUP                               
         BZ    SETCOMX                                                          
         LA    R1,1(R1)                                                         
         CLI   0(R1),X'FF'         TEST FOR END OF GROUP                        
         BE    SETCOMX                                                          
         ST    R1,SYGROUP                                                       
         B     SETCOM1                                                          
*                                                                               
SETCOMX  LAM   R3,R3,=F'0'         MUST CLEAR THIS                              
         SAC   0                                                                
         B     XIT1                                                             
*                                                                               
NODATA   MVC   OPMSGTXT,OPER9      MISSING DATA                                 
         BAS   RE,WTO                                                           
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        DO X MEMORY POST TO FACPAK (DETAILS IN CARD)       *                   
*************************************************************                   
         SPACE 1                                                                
POSTIT   NTR1                                                                   
         LH    R4,WORK1+12         ASID IS AT +12                               
         L     R2,WORK1+16         SSBOPECB IS AT +16                           
         LOCASCB ASID=(R4)                                                      
         LR    R3,R1                                                            
         LTR   RF,RF                                                            
         BNZ   NOPOST                                                           
         USING ASCB,R3                                                          
         CLC   ASCBASCB,=C'ASCB'                                                
         BNE   NOPOST                                                           
         L     R4,ASCBASSB                                                      
*                                                                               
         LA    RE,*+10                                                          
         O     RE,=X'80000000'                                                  
         BSM   0,RE                SWITCH TO 31-BIT MODE                        
         L     R4,ASSBJSAB-ASSB(R4) R4 = A(JSAB)                                
         USING JSAB,R4                                                          
         CLC   WORK1(8),JSABJBNM                                                
         LA    RE,*+6              SWITCH BACK TO 24 BT MODE                    
         BSM   0,RE                                                             
         BNE   NOPOST                                                           
*                                                                               
         LA    R5,99               SET COMPLETION CODE                          
         POST  (R2),(R5),ASCB=(R3),LINKAGE=SYSTEM,ECBKEY=8,MF=(E,POSTX)         
         B     XIT1                                                             
NOPOST   MVC   WORK1+10(8),WORK1                                                
         MVC   WORK1+2(8),WORK1+10                                              
         MVC   WORK1+10(18),=C' SYSTEM NOT POSTED'                              
         MVC   WORK1+0(2),=H'26'                                                
         WTO   TEXT=WORK1                                                       
         B     XIT1                                                             
POSTX    POST  ERRET=DEAD,ECBKEY=YES,MF=L                                       
DEAD     DC    H'0'                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
*************************************************************                   
*        START A SYSTEM                                     *                   
*************************************************************                   
         SPACE 1                                                                
UPSYS    NTR1                                                                   
         CLC   0(8,R1),=C'READONLY'                                             
         BE    *+8                                                              
         BAS   RE,RWSYS            MAKE SURE WRITES ENABLED                     
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         OC    SEFILES,SEFILES     ANY FILES THIS SE                            
         BZ    UPOK                NO - EXIT                                    
         TM    SEIND,SEISTRT       MAKE SURE SYSTEM IS DOWN                     
         BZ    UPS0                                                             
         BAS   RE,DNSYS            IF NOT FORGET IT                             
*                                                                               
UPS0     BAS   RE,SETSYS           FRIG TCB ENTRY FOR SYSCHA                    
         XC    DMCB(24),DMCB                                                    
         LA    R2,WORK             POINT TO DUMMY DMCB                          
         GOTO1 VDMOD000,DMCB,VFINDSYS,(SESYS,0)                                 
         L     R1,4(R1)            R1=A(SYSFILES LIST)                          
         LH    R0,2(R1)                                                         
         LA    R1,4(R1)                                                         
         XC    AREQUEST,AREQUEST                                                
*                                  CLEAR ISCILAST FOR ALL I/S FILES             
UPS1     L     RE,4(R1)                                                         
         TM    0(R1),X'01'         I/S FILE ?                                   
         BZ    *+10                                                             
         USING ISDTF,RE                                                         
         XC    ISCILAST,ISCILAST                                                
         TM    0(R1),X'20'         TEST REQUEST FILE                            
         BZ    *+8                                                              
         ST    RE,AREQUEST                                                      
         LA    R1,8(R1)                                                         
         BCT   R0,UPS1                                                          
         DROP  RE                                                               
*                                  OPEN SEFILES                                 
         BAS   RE,SINGLE                                                        
         GOTO1 VDMOD000,DMCB,VOPENSYS,(SESYS,IOAREA)                            
         BAS   RE,MULTI                                                         
         OC    DMCB+8(2),DMCB+8                                                 
         BZ    UPS2                                                             
*                                                                               
         MVC   DUB(2),DMCB+8       SAVE ERROR BYTES                             
         MVC   DMCB(4),VCLSESYS                                                 
         GOTO1 VDMOD000,DMCB       CLOSE SYSTEM FILES                           
         MVC   DMCB+8(2),DUB                                                    
         BAS   RE,RELSYS           RESET TCB ENTRY                              
         TM    DMCB+9,X'80'        INVALID CPU ID                               
         BNO   *+10                                                             
         MVC   OPCMND,=C'INVLID CPU'                                            
         TM    DMCB+9,X'40'        MISSING CPU ID                               
         BNO   *+10                                                             
         MVC   OPCMND,=C'MISSNG CPU'                                            
         TM    DMCB+9,X'20'        ACTIVE IN ANOTHER PARTITION                  
         BNO   *+14                                                             
         MVC   OPCMND,=C'ACTIVE IAP'                                            
         B     *+10                                                             
         BAS   RE,CLEANUP                                                       
         DC    H'0'                                                             
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID(7),SPACES                                                
         BAS   RE,WTO                                                           
         B     UPX                                                              
*                                  LOAD & GO TO RECOVERY RESTORE                
UPS2     EQU   *                                                                
         GOTO1 VCALLOV,DMCB,IOAREA,X'D9010100'                                  
         CLI   4(R1),X'FF'                                                      
         BNE   *+10                                                             
         BAS   RE,CLEANUP                                                       
         DC    H'0'                                                             
         LA    RF,IOAREA                                                        
         XC    DMCB(12),DMCB                                                    
         GOTO1 (RF),(R1),,(SESYS,(R7)),0                                        
         CLI   8(R1),0                                                          
         BE    UPS3                                                             
         BAS   RE,CLEANUP                                                       
         DC    H'0'                                                             
         WTO   '*RCVR ERROR*'                                                   
*                                                                               
UPS3     ICM   R1,15,AREQUEST      TEST SYSTEM REQUEST FILE ERASED              
         BZ    UPS4                                                             
         CLC   DNEXT-DTFPHD(L'DNEXT,R1),=X'00010000'                            
         BNE   UPS4                                                             
         SH    R1,=H'4'                                                         
         ICM   R1,15,0(R1)         YES - POINT TO REQUEST ADDRESS LIST          
         BZ    UPS4                                                             
*                                                                               
         LH    RE,0(R1)            SET UP FOR REQUEST LIST BXLE                 
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         XC    2(6,R1),2(R1)       AND CLEAR REQUEST POINTERS                   
         BXLE  R1,RE,*-6                                                        
*                                                                               
UPS4     BAS   RE,RELSYS           RESET TCB ENTRY                              
*                                                                               
UPOK     NI    SEIND,X'FC'                                                      
         OI    SEIND,X'80'         SET SE UP                                    
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID(7),SPACES                                                
         MVC   OPCMND,STARTED                                                   
         BAS   RE,WTO                                                           
*                                                                               
UPX      B     XIT1                                                             
         DROP  R6                                                               
         EJECT                                                                  
*************************************************************                   
*        STOP A SYSTEM                                      *                   
*************************************************************                   
         SPACE 1                                                                
DNSYS    NTR1                                                                   
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         TM    SEIND,X'80'         IGNORE IT IF ALREADY DOWN                    
         BZ    XIT1                                                             
         MVC   OPSYSID(7),0(R6)                                                 
         MVC   OPFILID(7),SPACES                                                
         MVC   OPCMND,STOPPED                                                   
         OC    SEFILES,SEFILES     ANY FILES                                    
         BZ    DNOK                NO - XIT1                                    
         BAS   RE,SYSWAIT                                                       
*                                                                               
         OI    SEIND,SEINOP        SET SE NO-OP                                 
         NI    SEIND,X'65'         TURN-OFF OP,SRO,QUI & STOP                   
*                                                                               
         BAS   RE,QOFFALL                                                       
*                                  CLOSE SE FILES                               
DN4      BAS   RE,SETSYS                                                        
         BAS   RE,SINGLE                                                        
         XC    DMCB(24),DMCB                                                    
         LA    R2,WORK             POINT TO DUMMY DMCB                          
         GOTO1 VDMOD000,DMCB,VCLSESYS,(SESYS,IOAREA)                            
         BAS   RE,MULTI                                                         
         BAS   RE,RELSYS                                                        
*                                                                               
DNOK     BAS   RE,WTO                                                           
         B     XIT1                                                             
         DROP  R6                                                               
         EJECT                                                                  
*************************************************************                   
*        LOOP UNTIL SYSTEM AT ASENTRY IS INACTIVE           *                   
*************************************************************                   
         SPACE 1                                                                
SYSWAIT  NTR1                                                                   
         L     R2,ASENTRY                                                       
         USING SELISTD,R2                                                       
         OI    SEIND,X'01'         SET 01 TO SUSPEND ALL INPUT                  
         L     R3,=F'600'          LOOP COUNT                                   
*                                                                               
SWAIT1   L     R6,VTCB             CHECK SE NOT ACTIVE IN ANOTHER TASK          
         LH    R4,0(R6)            SET BXLE                                     
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
         USING TCBD,R6                                                          
*                                                                               
SWAIT2   CLC   TCBSYS,SESYS        LOOK FOR ACTIVE TASK                         
         BE    SWAIT8                                                           
*                                                                               
         LA    R1,TCBSWTAB         LOOK IN SWITCH TAB                           
         LA    R0,TCBSWMAX                                                      
SWAIT5   CLC   0(1,R1),TCBSYS      FIND THIS SYSTEM                             
         BNE   SWAIT6                                                           
         OC    11(8,R1),11(R1)     NO RECOVERY IS OK                            
         BZ    SWAIT7                                                           
         B     SWAIT8              RECOVERY FOUND SO WAIT                       
SWAIT6   LA    R1,TCBSWLEN(R1)                                                  
         BCT   R0,SWAIT5           NEXT SWTAB ENTRY                             
*                                                                               
SWAIT7   BXLE  R6,R4,SWAIT2                                                     
         NI    SEIND,255-X'01'     RESET INDICATOR                              
         B     SWAIT9                                                           
*                                                                               
*        WAIT FOR 1/10 SEC. AFTER 60 SECONDS GIVE UP                            
*                                                                               
SWAIT8   GOTO1 VTICTOC,DMCB,C'WAIT',F'3840'                                     
*                                                                               
         BCT   R3,SWAIT1                                                        
         NI    SEIND,255-X'01'     RESET INDICATOR                              
         BAS   RE,CLEANUP          FUCK THIS I'VE HAD ENOUGH                    
         MVC   OPMSGTXT,OPER7      SYSTEM IS BUSY                               
         BAS   RE,WTO                                                           
         B     XMOD1                                                            
*                                                                               
SWAIT9   B     XIT1                                                             
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
*        TURN ON/OFF ALL QUIESCE BITS FOR A SYSTEM          *                   
*************************************************************                   
         SPACE 1                                                                
QOFFALL  MVI   BYTE,0                                                           
         B     QALL                                                             
QONALL   MVI   BYTE,1                                                           
*                                                                               
QALL     NTR1                                                                   
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         L     R4,SEFILES                                                       
         L     R4,0(R4)                                                         
         MVC   HALF,2(R4)          SAVE N'FILES IN HALF                         
         LA    R4,4(R4)                                                         
*                                                                               
QALL010  SR    R6,R6                                                            
         ICM   R6,7,5(R4)                                                       
         USING ISDTF,R6                                                         
*                                                                               
         CLI   BYTE,1              TURN OFF ALL QUIESCE BITS                    
         BE    *+8                                                              
         NI    ISFOPEN,255-ISFOQUIE                                             
*                                                                               
         CLI   BYTE,0              TURN ON ALL QUIESCE BITS                     
         BE    *+8                                                              
         OI    ISFOPEN,ISFOQUIE                                                 
*                                                                               
         LA    R4,8(R4)            NEXT FILE                                    
         LH    RF,HALF             DEC FILE COUNT                               
         BCT   RF,*+8                                                           
         B     QOFFX                                                            
         STH   RF,HALF                                                          
         B     QALL010                                                          
*                                                                               
QOFFX    B     XIT1                                                             
         DROP  R6                                                               
         EJECT                                                                  
*************************************************************                   
*        SET & RESET TCB ENTRY                              *                   
*************************************************************                   
         SPACE 1                                                                
SETSYS   NTR1  ,                   SET TCB ENTRY FOR SYSCHA                     
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         L     R5,SSBTKADR                                                      
         USING TCBD,R5                                                          
         MVC   SAVETCB(8),TCBDTFS                                               
         MVC   SAVETCB+8(1),TCBSYS                                              
         MVC   TCBDTFS(8),SEFILES                                               
         MVC   TCBSYS,SESYS                                                     
         GOTO1 VDTFIOA,DMCB,(C'I',(R5))                                         
         B     XIT1                                                             
         SPACE 1                                                                
RELSYS   NTR1  ,                   RESET TCB ENTRY                              
         L     R5,SSBTKADR                                                      
         USING TCBD,R5                                                          
         MVC   TCBDTFS(8),SAVETCB                                               
         MVC   TCBSYS,SAVETCB+8                                                 
         GOTO1 VDTFIOA,DMCB,(C'I',(R5))                                         
         B     XIT1                                                             
         DROP  R6                                                               
         SPACE 2                                                                
*************************************************************                   
*        FIND ASENTRY FROM SYS DATA                         *                   
*************************************************************                   
         SPACE 1                                                                
FINDSYS  NTR1                                                                   
         L     R6,VSELIST          START AT BEGINING OF SELIST                  
         USING SELISTD,R6                                                       
         LH    R4,0(R6)            SET BXLE                                     
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
         SR    R1,R1                                                            
         ICM   R1,1,SYNAME         PREPARE FOR NAME SEARCH                      
         BCTR  R1,0                                                             
*                                                                               
FINDS10  OC    SYNUM,SYNUM                                                      
         BNE   FINDS020                                                         
*                                                                               
         EX    R1,*+8              FIND SELECTED SYS                            
         B     *+10                                                             
         CLC   SENAME(0),SYNAME+1                                               
         BE    FINDSX              GOT IT                                       
         BNE   FINDS090                                                         
*                                                                               
FINDS020 CLC   SYNUM+1(1),SESYS    TEST NUMBER                                  
         BE    FINDSX                                                           
*                                                                               
FINDS090 BXLE  R6,R4,FINDS10                                                    
         B     XITNEQ                                                           
*                                                                               
FINDSX   ST    R6,ASENTRY          SAVE ASENTRY                                 
         MVC   SYNAME+1(7),SENAME                                               
         MVI   SYNAME,7                                                         
         MVI   SYNUM,0                                                          
         MVC   SYNUM+1(1),SESYS                                                 
         B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        FIND FILE FROM FILENAME ALONE                      *                   
*************************************************************                   
         SPACE 1                                                                
FSYSFIL  NTR1                                                                   
         ICM   R6,15,ASENTRY       IS SYSTEM ALREADY FOUND                      
         BNZ   FSYSF01                                                          
*                                                                               
         L     R6,VSELIST          START AT BEGINING OF SELIST                  
         USING SELISTD,R6                                                       
         LH    R4,0(R6)            SET BXLE                                     
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
*                                                                               
         USING SELISTD,R6                                                       
FSYSF01  ICM   R3,15,SEFILES       SET R3 TO FILES                              
         BZ    FSYSF090                                                         
         L     R3,0(R3)                                                         
         SR    R0,R0                                                            
         ICM   R0,3,2(R3)          R0 = NUMBER OF FILES                         
         BZ    FSYSF090                                                         
         LA    R3,4(R3)                                                         
*                                                                               
         SR    R1,R1                                                            
FSYSF02  ICM   R1,7,5(R3)                                                       
         CLC   FILNAM,22(R1)                                                    
         BNE   FSYSF03                                                          
*                                                                               
         SR    RE,RE               CHECK THIS IS THE NATIVE SYSTEM              
         ICM   RE,1,18(R1)                                                      
         BNZ   *+8                                                              
         IC    RE,3(R3)                                                         
         SRL   RE,4                                                             
         CLM   RE,1,SEOVSYS                                                     
         BE    FSYSF10                                                          
*                                                                               
FSYSF03  LA    R3,8(R3)                                                         
         BCT   R0,FSYSF02          NEXT FILE                                    
*                                                                               
FSYSF090 OC    ASENTRY,ASENTRY     ARE WE ON AN INDIVIDUAL SYS                  
         BNZ   *+8                                                              
         BXLE  R6,R4,FSYSF01       NEXT SYSTEM                                  
*                                                                               
         XC    ADTF,ADTF           NOT FOUND CLEAR DTF                          
         XC    FFLAGS,FFLAGS                                                    
         B     XITNEQ                                                           
*                                                                               
FSYSF10  ST    R6,ASENTRY          SAVE ASENTRY                                 
         MVC   SYNAME+1(7),SENAME                                               
         MVI   SYNAME,7                                                         
         MVI   SYNUM,0                                                          
         MVC   SYNUM+1(1),SESYS                                                 
         MVC   FILNUM,3(R3)                                                     
         MVC   FFLAGS,0(R3)        SET FLAGS AND DTF ADDR                       
         ST    R1,ADTF                                                          
*                                                                               
         B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        FIND ADTF FOR FILE IN FILNUM                       *                   
*************************************************************                   
         SPACE 1                                                                
FINDFIL  NTR1                                                                   
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         L     R4,SEFILES                                                       
         L     R4,0(R4)                                                         
         SR    R0,R0                                                            
         ICM   R0,3,2(R4)                                                       
         LA    R4,4(R4)                                                         
*                                                                               
FINDF01  CLC   FILNUM,3(R4)                                                     
         BE    FINDF02                                                          
         LA    R4,8(R4)                                                         
         BCT   R0,FINDF01                                                       
         BAS   RE,CLEANUP                                                       
         DC    H'0'                                                             
*                                                                               
FINDF02  MVC   FFLAGS,0(R4)                                                     
         SR    R6,R6                                                            
         ICM   R6,7,5(R4)                                                       
         ST    R6,ADTF                                                          
*                                                                               
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        QUIESCE A FILE     (SET QUIESCE BIT ON FOR R/O)    *                   
*************************************************************                   
         SPACE 1                                                                
QUION    MVI   BYTE,0                                                           
         B     QUIES                                                            
QUIOFF   MVI   BYTE,1                                                           
*                                                                               
QUIES    NTR1                                                                   
         L     R6,ADTF             GET A(DTF)                                   
         USING ISDTF,R6                                                         
         TM    ISFOPEN,ISFOOPN     IGNORE IF CLOSED                             
         BZ    QUIX                                                             
*                                                                               
         BAS   RE,SYSWAIT          WAIT FOR SYSTEM                              
*                                                                               
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID(7),ISFFID                                                
         CLI   BYTE,0              IF BYTE=0 QUIESCE                            
         BNE   QUIES010                                                         
*                                                                               
         OI    ISFOPEN,ISFOQUIE                                                 
         MVC   OPCMND,QUIESCED     QUIESCE                                      
         BAS   RE,WTO                                                           
         B     QUIX                                                             
*                                                                               
QUIES010 NI    ISFOPEN,255-ISFOQUIE                                             
         MVC   OPCMND,UNQUI        UNQUIESCE                                    
         BAS   RE,WTO                                                           
*                                                                               
QUIX     BAS   RE,SECHECK                                                       
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        OPEN A FILE                                        *                   
*************************************************************                   
         SPACE 1                                                                
QOPEN    NTR1                                                                   
         L     R6,ADTF             QUIESCE OPEN A FILE                          
         USING ISDTF,R6                                                         
         TM    ISFOPEN,ISFOOPN                                                  
         BNZ   XIT1                FILE ALREADY OPEN DUMMY                      
         L     R1,ASENTRY                                                       
         TM    9(R1),X'80'         SYSTEM MUST BE OPEN TOO                      
         BZ    XIT1                                                             
*                                                                               
         BAS   RE,SYSWAIT          WAIT FOR SYSTEM TO BE FREE                   
         BAS   RE,SETSYS                                                        
*                                                                               
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID(7),ISFFID                                                
         CLC   ACTNUM,=H'13'       TEST OPEN OR QOPEN                           
         BNE   *+14                                                             
         OI    ISFOPEN,ISFOQUIE                                                 
         MVC   OPCMND,QOPENED                                                   
         CLC   ACTNUM,=H'6'                                                     
         BNE   *+14                                                             
         NI    ISFOPEN,255-ISFOQUIE                                             
         MVC   OPCMND,OPENED                                                    
         BAS   RE,SINGLE           SINGLE THREAD                                
         TM    ISFTYPE,ISFTIS                                                   
         BZ    QO1                                                              
         GOTO1 VDATAMGR,DMCB,ISDDS,8,IOAREA,0,(R6),0,0                          
         B     QOX                                                              
*                                                                               
         DROP  R6                                                               
         USING DTFPHD,R6                                                        
QO1      MVC   FULL,FIRSTDA                                                     
         GOTO1 VDATAMGR,DMCB,DADDS,DAOPEN,IOAREA,0,(R6),FULL,0                  
*                                                                               
         MVC   FULL,FIRSTDA        SET TO 00010100                              
         TM    DTFTYPE,DTFTBIG                                                  
         BZ    *+10                                                             
         MVC   FULL,=X'00004101'   OR 00004101 FOR BIG FILES                    
         TM    FFLAGS,SFHDR                                                     
         BZ    QO1A                IF NO HEADER GO READ FOR EOF                 
*                                                                               
         GOTO1 (RF),(R1),DADDS,RDID,IOAREA,0,(R6),FULL,0                        
         MVC   FULL,IOAREA+92                                                   
         TM    DTFTYPE,DTFTBIG     RE TEST AFTER HEADER READ                    
         BZ    QO1A                                                             
         SR    R0,R0               ADJUST DA IF BIG FILE                        
         ICM   R0,3,IOAREA+92                                                   
         CLI   IOAREA+94,X'FF'                                                  
         BE    *+8                                                              
         ICM   R0,4,IOAREA+94                                                   
         SLL   R0,32-18                                                         
         ST    R0,FULL                                                          
*                                                                               
QO1A     GOTO1 (RF),(R1),DADDS,ADDADR,IOAREA,0,(R6),FULL,0                      
QOX      BAS   RE,RELSYS                                                        
         BAS   RE,MULTI            OK TO MULTI TASK AGAIN                       
         BAS   RE,WTO                                                           
         BAS   RE,SECHECK                                                       
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        CLOSE A FILE                                       *                   
*************************************************************                   
         SPACE 1                                                                
QCLOSE   NTR1                                                                   
         L     R6,ADTF             QUIESCE CLOSE A FILE                         
         USING ISDTF,R6                                                         
         TM    ISFOPEN,ISFOOPN                                                  
         BZ    QCXX                FILE ALREADY CLOSED DUMMY                    
         BAS   RE,SYSWAIT          WAIT FOR SYSTEM TO BE FREE                   
*                                                                               
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID(7),ISFFID                                                
         OI    ISFOPEN,ISFOQUIE    LET THE WORLD KNOW I DID IT                  
*                                                                               
         TM    ISFTYPE,ISFTIS                                                   
         BZ    QC1                                                              
*                                                                               
         GOTO1 VDATAMGR,DMCB,ISDDS,9,IOAREA,0,(R6),0,0                          
         B     QCX                                                              
*                                                                               
QC1      GOTO1 VDATAMGR,DMCB,DADDS,DACLOSE,IOAREA,0,(R6),X'00010101',0          
QCX      MVC   OPCMND,CLOSED                                                    
         BAS   RE,WTO                                                           
         BAS   RE,SECHECK                                                       
QCXX     B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        CHECK SELIST ENTRYS QUIESCE BIT IS CORRECT         *                   
*************************************************************                   
         SPACE 1                                                                
SECHECK  NTR1                                                                   
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         MVC   AFILES,SEFILES                                                   
         MVC   AFILEX,SEFILEX                                                   
         NI    SEIND,255-SEIQUIES                                               
         L     R4,AFILES                                                        
         L     R4,0(R4)                                                         
         MVC   HALF,2(R4)          SAVE N'FILES IN HALF                         
         LA    R4,4(R4)                                                         
*                                                                               
SECHK01  SR    R1,R1                                                            
         ICM   R1,7,5(R4)                                                       
         USING ISDTF,R1                                                         
*                                                                               
         TM    ISFOPEN,ISFOQUIE                                                 
         BZ    *+12                                                             
         OI    SEIND,SEIQUIES                                                   
         B     XIT1                                                             
*                                                                               
         LA    R4,8(R4)            NEXT FILE                                    
         LH    RF,HALF             DEC FILE COUNT                               
         BCT   RF,*+8                                                           
         B     XIT1                                                             
         STH   RF,HALF                                                          
         B     SECHK01                                                          
         DROP  R1,R6                                                            
         EJECT                                                                  
*************************************************************                   
*        QUIESCE OR UNQUIESCE A WHOLE SYSTEM                *                   
*************************************************************                   
         SPACE 1                                                                
SQUIES   NTR1                                                                   
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         OC    SEFILES,SEFILES     ANY FILES                                    
         BZ    SQUOKX              NO - EXIT                                    
         BAS   RE,SYSWAIT                                                       
*                                                                               
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID(7),SPACES                                                
         MVC   OPCMND,STARTED                                                   
         TM    SEIND,SEIQUIES                                                   
         BZ    SQU010                                                           
*                                                                               
         BAS   RE,QOFFALL                                                       
         NI    SEIND,255-SEIQUIES                                               
         MVC   OPCMND,UNQUI                                                     
         B     SQUOK                                                            
*                                                                               
SQU010   BAS   RE,QONALL                                                        
         OI    SEIND,SEIQUIES                                                   
         MVC   OPCMND,QUIESCED                                                  
         B     SQUOK                                                            
*                                                                               
SQUOK    BAS   RE,WTO                                                           
SQUOKX   B     XIT1                                                             
         DROP  R6                                                               
*************************************************************                   
*        SET & RESET SINGLE THREAD MODE                     *                   
*************************************************************                   
         SPACE 1                                                                
SINGLE   ST    RE,SAVERE                                                        
         L     RE,VSSB                                                          
         MVI   SSBMTIND-SSBD(RE),0      DISABLE MULTI-TASKING WAITS             
         GOTO1 VTICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
MULTI    ST    RE,SAVERE                                                        
         GOTO1 VTICTOC,DUB,C'RSET' RESET TIMERS                                 
         L     RE,VSSB                                                          
         MVI   SSBMTIND-SSBD(RE),C'M'   ENABLE MULTI-TASKING WAITS              
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* START/READONLY THE SYSTEM IN SENUMB                                 *         
* EXIT WITH CC=NEQ ON ERROR WITH MSGNUM SET                           *         
***********************************************************************         
         SPACE 1                                                                
ROSYS    NTR1                                                                   
         L     R2,ASENTRY                                                       
         USING SELISTD,R2                                                       
         OC    SEFILES,SEFILES     ANY FILES                                    
         BZ    ROOK                NO - EXIT                                    
*                                                                               
         BAS   RE,SYSWAIT          WAIT FOR SYSTEM TO BE FREE                   
*                                  SET SE FILES TO READ-ONLY                    
RO4      OI    SEIND,SEISETRO                                                   
         L     R3,SEFILES                                                       
         L     R3,0(R3)            POINT TO SYSTEM FILE LIST                    
         SR    RF,RF                                                            
         SR    R1,R1                                                            
         ICM   RF,3,2(R3)          RF = NUMBER OF FILES                         
         LA    R3,4(R3)                                                         
         MVC   BYTE,3(R3)          EXT NUMBER OF 1ST FILE                       
         NI    BYTE,X'F0'          REMOVE LS BITS                               
RO4A     MVC   FULL(1),3(R3)       EXT NUMBER OF THIS FILE                      
         NI    FULL,X'F0'          REMOVE LS BITS                               
         CLC   FULL(1),BYTE        ARE THEY FROM SAME SYSTEM                    
         BNE   RO4B                                                             
         ICM   R1,7,5(R3)                                                       
         OI    36(R1),X'80'        SET READ-ONLY IN DTF                         
RO4B     LA    R3,8(R3)            NEXT DTF                                     
         BCT   RF,RO4A                                                          
*                                                                               
         GOTO1 VTICTOC,DUB,C'SSET'                                              
         L     RE,ASENTRY                                                       
         MVC   OPSYSID(L'SENAME),SENAME-SELISTD(RE)                             
         MVC   OPCMND(7),=C'RD-ONLY'                                            
         BAS   RE,WTO                                                           
*                                                                               
         TM    SEIND,SEISTRT       IS SYSTEM UP AND RUNNING                     
         BNZ   RODEQ                                                            
         GOTO1 UPSYS,=C'READONLY'  NO SO START IT                               
         B     ROOK                                                             
*                                                                               
RODEQ    SR    RE,RE                                                            
         IC    RE,SESYS                                                         
         CVD   RE,DUB              NO BUILD ENQUEUE ID                          
         UNPK  DUB(4),DUB+6(2)                                                  
         OI    DUB+3,X'F0'                                                      
         MVC   UPDMIN+4(4),DUB                                                  
         XC    DUB(12),DUB                                                      
         LA    R2,UPDMAJ                                                        
         LA    RE,UPDMIN                                                        
         DEQ   ((R2),(RE),8,SYSTEM),RET=HAVE                                    
*                                                                               
ROOK     B     XIT1                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* SET SYSTEM TO UPDATIVE                                              *         
***********************************************************************         
         SPACE 1                                                                
RWSYS    NTR1                                                                   
         L     R5,ASENTRY                                                       
         USING SELISTD,R5                                                       
         OC    SEFILES,SEFILES     ANY FILES THIS SE                            
         BZ    RWX                 NO - EXIT                                    
         TM    SEIND,SEISETRO      IS IT SET READ-ONLY                          
         BNO   RWX                                                              
UPENABLE NI    SEIND,255-SEISETRO                                               
         L     R3,SEFILES                                                       
         L     R3,0(R3)            POINT TO SYSTEM FILE LIST                    
         SR    RF,RF                                                            
         SR    R1,R1                                                            
         ICM   RF,3,2(R3)          RF = NUMBER OF FILES                         
         LA    R3,4(R3)                                                         
         MVC   BYTE,3(R3)          EXT NUMBER OF 1ST FILE                       
         NI    BYTE,X'F0'          REMOVE LS BITS                               
UPEN1    MVC   FULL(1),3(R3)       EXT NUMBER OF THIS FILE                      
         NI    FULL,X'F0'          REMOVE LS BITS                               
         CLC   FULL(1),BYTE        ARE THEY FROM SAME SYSTEM                    
         BNE   UPEN2                                                            
         ICM   R1,7,5(R3)                                                       
         NI    36(R1),X'FF'-X'80'  CLR READ-ONLY IN DTF                         
UPEN2    LA    R3,8(R3)            NEXT DTF                                     
         BCT   RF,UPEN1                                                         
         B     RWOK                                                             
*                                                                               
RWOK     EQU   *                                                                
*                                                                               
RWX      B     XIT1                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* SEND DIRECT BROADCAST TO TERMINALS                                  *         
***********************************************************************         
         SPACE 1                                                                
BCAST    NTR1                                                                   
         L     R6,VUTL                                                          
         LH    R4,0(R6)            SET BXLE REGS                                
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
*                                                                               
         USING UTLD,R6                                                          
BC010    OC    SYNUM,SYNUM         IS THIS FOR ALL SYSTEMS                      
         BZ    *+14                                                             
         CLC   SYNUM+1(1),TSYS     ARE THEY CONNECTED TO THIS SYSTEM            
         BNE   BC050                                                            
         MVC   TBRSYS,BRDNUM       SEND THEM THE BROADCAST                      
         OI    TSTAT2,TSTATBCP                                                  
*                                                                               
BC050    BXLE  R6,R4,BC010         BXLE TO NEXT TERMINAL                        
*                                                                               
BCX      B     XIT1                                                             
         DROP  R6                                                               
         EJECT                                                                  
*************************************************************                   
*        VALIDATE OPER INPUT                                *                   
*************************************************************                   
         SPACE 1                                                                
VALCARD  NTR1                                                                   
*                                                                               
         XC    ACTNUM,ACTNUM       CLEAR RETURNED VALUES                        
         XC    ADNAME,ADNAME                                                    
         XC    ADNUM,ADNUM                                                      
         XC    SYNAME,SYNAME                                                    
         XC    SYNUM,SYNUM                                                      
         XC    ASENTRY,ASENTRY                                                  
         XC    FFLAGS,FFLAGS                                                    
         XC    FILNUM,FILNUM                                                    
         XC    FILNAM,FILNAM                                                    
         XC    ADTF,ADTF                                                        
*                                                                               
         GOTO1 ASCANNER,DMCB,(C'C',CARD),(8,SCANBLK),0                          
         SR    R3,R3                                                            
         ICM   R3,1,4(R1)          TEST FOR NO INPUT                            
         BZ    VALCARDX                                                         
*                                                                               
VALC010  LA    R2,SCANBLK          POINT TO SCANNER BLOCK                       
         SR    R1,R1                                                            
         ICM   R1,1,0(R2)          ERROR IF NOT INPUT                           
         BZ    VALC022                                                          
         LA    RF,COMTAB           SEARCH COMTAB                                
VALC020  CLM   R1,1,10(RF)                                                      
         BL    VALC021             REJECT IF < MIN LENGTH                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),12(R2)      COMPARE COMMAND TEXT                         
         BE    VALC023                                                          
         LA    R1,1(R1)                                                         
VALC021  LA    RF,16(RF)                                                        
         CLI   8(RF),X'FF'         TEST FOR EOT                                 
         BNE   VALC020                                                          
*                                                                               
VALC022  MVC   COMMAND,12(R2)      SAVE COMMAND TEXT                            
         LTR   RB,RB               RETURN CC NEQ                                
         B     XIT1                                                             
*                                                                               
VALC023  MVC   ACTNUM,8(RF)        SAVE ACTION NUMBER                           
         MVC   FLAG,11(RF)                                                      
         B     VALC100                                                          
         EJECT                                                                  
************************************************************                    
*        VALIDATE ADV NAME                                 *                    
************************************************************                    
         SPACE 1                                                                
VALC100  LA    R2,32(R2)           NEXT SCAN FIELD (ADV NAME)                   
         BCT   R3,*+8                                                           
         B     VALCARDX                                                         
*                                                                               
         CLI   0(R2),0             NO ADV NAME IS OK                            
         BE    VALC200                                                          
         CLI   0(R2),3             4 IS ADVN                                    
         BNL   *+12                                                             
         CLI   0(R2),1             1 IS ADVCHR                                  
         BNE   VALC190                                                          
*                                                                               
         L     R1,SSBAFID          GET FACID TAB                                
VALC110  CLI   0(R2),1                                                          
         BNE   *+14                                                             
         CLC   12(1,R2),7(R1)      1 CHR TEST                                   
         B     *+10                                                             
         CLC   12(4,R2),0(R1)      4 CHR TEST                                   
         BE    VALC120                                                          
         LA    R1,8(R1)                                                         
         CLI   0(R1),X'FF'         TEST EOT                                     
         BNE   VALC110                                                          
         B     VALC190                                                          
*                                                                               
VALC120  MVC   ADNUM,4(R1)         SAVE ADV NUM & NAME                          
         MVC   ADNAME,0(R1)                                                     
         B     VALC200                                                          
*                                                                               
VALC190  MVC   OPMSGTXT,OPER4      UNKNOWN ADV SYSTEM                           
         BAS   RE,WTO                                                           
         B     XMOD1                                                            
         EJECT                                                                  
************************************************************                    
*        VALIDATE SE SYSTEM NAME                           *                    
************************************************************                    
         SPACE 1                                                                
VALC200  LA    R2,32(R2)           NEXT SCAN FIELD (SE SYS)                     
         BCT   R3,*+8                                                           
         B     VALCARDX                                                         
*                                                                               
         XC    SYGROUP,SYGROUP                                                  
         SR    R1,R1               GET LEN IN R1                                
         ICM   R1,1,0(R2)                                                       
         BZ    VALC300             NO SYSTEM IS OK FOR NOW                      
         CLI   0(R2),4                                                          
         BL    VALC290             BUT < 4 CHRS FOR COMPARE IS NOT              
         STC   R1,SYNAME                                                        
         MVC   SYNAME+1(7),12(R2)                                               
         BAS   RE,FINDSYS                                                       
         BE    VALC300                                                          
*                                                                               
VALC230  XC    SYNUM,SYNUM         SET UP FOR GROUP                             
         L     R1,8(RE)                                                         
         A     R1,RELO                                                          
         ST    R1,SYGROUP                                                       
         B     VALC300                                                          
*                                                                               
VALC290  MVC   OPMSGTXT,OPER5      UNKNOWN SYSTEM                               
         BAS   RE,WTO                                                           
         B     XMOD1                                                            
         EJECT                                                                  
************************************************************                    
*        VALIDATE FILENAME                                 *                    
************************************************************                    
         SPACE 1                                                                
VALC300  CLC   ACTNUM,=H'12'       NEXT SCAN FIELD (FILENAME)                   
         BE    VALC400                                                          
         LA    R2,32(R2)           NEXT SCAN FIELD (FILENAME)                   
         BCT   R3,*+8                                                           
         B     VALCARDX                                                         
*                                                                               
         SR    R1,R1               GET LEN IN R1                                
         ICM   R1,1,0(R2)                                                       
         BZ    VALCARDX            NO FILENAME OK FOR NOW                       
         CLI   0(R2),4                                                          
         BL    VALC390             BUT < 4 CHRS FOR COMPARE IS NOT              
*                                                                               
         MVC   FILNAM,12(R2)                                                    
         BAS   RE,FSYSFIL          VALIDATE FILENAME                            
         BE    VALCARDX                                                         
*                                                                               
VALC390  MVC   OPMSGTXT,OPER6      UNKNOWN FILENAME                             
         BAS   RE,WTO                                                           
         B     XMOD1                                                            
*                                                                               
VALCARDX CR    R1,R1               EXIT EQU                                     
         B     XIT1                                                             
         EJECT                                                                  
************************************************************                    
*        VALIDATE BROADCAST                                *                    
************************************************************                    
         SPACE 1                                                                
VALC400  LA    R2,32(R2)           NEXT SCAN FIELD (FILENAME)                   
         BCT   R3,*+8                                                           
         B     VALC490                                                          
*                                                                               
         SR    R1,R1               GET LEN IN R1                                
         ICM   R1,1,0(R2)                                                       
         BZ    VALC490             NO BROADCAST                                 
         LA    RF,BRDTABL                                                       
         BCTR  R1,0                                                             
*                                                                               
VALC410  EX    R1,*+8              SCAN THE TABLE FOR BROADCAST EQU             
         B     *+10                                                             
         CLC   0(0,RF),12(R2)                                                   
         BE    VALC420                                                          
         LA    RF,8(RF)                                                         
         CLI   7(RF),X'FF'                                                      
         BNE   VALC410                                                          
         B     VALC490                                                          
*                                                                               
VALC420  MVC   BRDNUM,7(RF)        SAVE THE NUMBER                              
         B     VALCARDX                                                         
*                                                                               
VALC490  MVC   OPMSGTXT,OPER8      UNKNOWN BROADCAST                            
         BAS   RE,WTO                                                           
         B     XMOD1                                                            
         EJECT                                                                  
************************************************************                    
*        ERRORS                                            *                    
************************************************************                    
         SPACE 1                                                                
ERR1     MVC   OPFILID,=CL8'IS BUSY'                                            
         MVC   OPCMND,=CL8'NO EOJ  '                                            
         BAS   RE,WTO                                                           
         B     XMOD1                                                            
*                                                                               
ERRX     BAS   RE,CLEANUP                                                       
         MVC   OPMSGTXT,OPER1      UNKNOWN COMMAND                              
         BAS   RE,WTO                                                           
         B     XMOD1                                                            
         EJECT                                                                  
******************************************************                          
*        WRITE TO OPERATOR                           *                          
******************************************************                          
         SPACE 1                                                                
WTO      NTR1                                                                   
         OC    OPMSG,SPACES                                                     
         GOTO1 VTICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 VDMOD000,DMCB,VWCTYPE,OPMSG,OPMSGL,C'LVL1'                       
         GOTO1 VTICTOC,DUB,C'RSET' RESET TIMERS                                 
         MVC   OPMSGTXT,SPACES                                                  
         B     XIT1                                                             
         EJECT                                                                  
*                                                                               
OPMSG    DS    0CL40  '+FACPAK+ SSSSSSSS FFFFFFFF CCCCCCCCCC                    
OPFACID  DS    CL8                                                              
         DS    CL1                                                              
OPMSGTXT DS    0CL31                                                            
OPSYSID  DS    CL8                                                              
         DS    CL1                                                              
OPFILID  DS    CL8                                                              
         DS    CL1                                                              
OPCMND   DS    CL10                                                             
         DS    CL3                                                              
OPMSGL   EQU   *-OPMSG                                                          
         EJECT                                                                  
*************************************************************                   
*        OPERATOR MESSAGES                                 *                    
*************************************************************                   
         SPACE 1                                                                
OPER1    DC    CL31'UNKNOWN COMMAND'                                            
OPER2    DC    CL31'EOJ COMMAND ACCEPTED'                                       
OPER3    DC    CL31'FORCED END OF JOB   '                                       
OPER4    DC    CL31'UNKNOWN ADV SYSTEM  '                                       
OPER5    DC    CL31'UNKNOWN SE SYSTEM   '                                       
OPER6    DC    CL31'UNKNOWN FILENAME    '                                       
OPER7    DC    CL31'SYSTEM IS BUSY - RETRY '                                    
OPER8    DC    CL31'UNKNOWN BROADCAST      '                                    
OPER9    DC    CL31'MISSING DATA           '                                    
         EJECT                                                                  
       ++INCLUDE FACOMTAB                                                       
         EJECT                                                                  
*************************************************************                   
*        BROADCAST TABLE                                   *                    
*************************************************************                   
         SPACE 1                                                                
BRDTABL  DC    CL7'READ5  ',AL1(03)                                             
         DC    CL7'READ10 ',AL1(04)                                             
         DC    CL7'DOWN330',AL1(05)                                             
         DC    CL7'READ10 ',AL1(06)                                             
         DC    CL7'XXXXXXX',AL1(FF)                                             
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS AND LTORG                               *                    
*************************************************************                   
         SPACE 1                                                                
         CNOP  0,8                                                              
MYUPDID  DS    0CL16                                                            
UPDMAJ   DC    C'DMGR    '                                                      
UPDMIN   DC    C'SNUM0000'                                                      
*                                                                               
MYPGMLST DC    CL7'SROPS00',X'14',X'1F',X'00',AL1(000),9X'00'                   
*                                                                               
QCLOSED  DC    CL8'QCLOSED'                                                     
OPENED   DC    CL10'OPENED'                                                     
CLOSED   DC    CL10'CLOSED'                                                     
QUIESCED DC    CL10'QUIESCED'                                                   
UNQUI    DC    CL10'UNQUIESCED'                                                 
QOPENED  DC    CL10'QOPENED'                                                    
STARTED  DC    CL10'STARTED'                                                    
STOPPED  DC    CL10'STOPPED'                                                    
QSTARTED DC    CL10'QSTARTED'                                                   
DMREAD   DC    CL7'DMREAD'                                                      
DMWRT    DC    CL7'DMWRT'                                                       
DADDS    DC    CL7'DADDS'                                                       
ISDDS    DC    CL7'ISDDS'                                                       
FIRSTDA  DC    X'00010101'                                                      
SPACES   DC    CL80' '                                                          
*                                                                               
DAOPEN   EQU   14                                                               
RDID     EQU   01                                                               
ADDADR   EQU   11                                                               
DACLOSE  EQU   15                                                               
*                                                                               
SFHDR    EQU   X'02'                                                            
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE DSECT                              *                   
*************************************************************                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
ASENTRY  DS    A                                                                
RELO     DS    A                                                                
*                                                                               
SRPARS   DS    0XL32                                                            
SRPAR1   DS    A                                                                
SRPAR2   DS    A                                                                
SRPAR3   DS    A                                                                
SRPAR4   DS    A                                                                
SRPAR5   DS    A                                                                
SRPAR6   DS    A                                                                
SRPAR7   DS    A                                                                
SRPAR8   DS    A                                                                
*                                                                               
ASCANNER DS    A                                                                
*                                                                               
ATIA     DS    A                                                                
AUTL     DS    A                                                                
ATCB     DS    A                                                                
ASELIST  DS    A                                                                
AIOAREA  DS    A                                                                
ABUFF1   DS    A                                                                
ABUFF2   DS    A                                                                
*                                                                               
AREQUEST DS    A                                                                
AFILES   DS    A                                                                
AFILEX   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
FLAG     DS    C                                                                
DMCB     DS    6F                                                               
*                                                                               
SYSNAME  DS    CL4                 3 CHR FACPAK NAME                            
SAVETCB  DS    XL10                                                             
COMMAND  DS    CL10                                                             
*                                                                               
ACTNUM   DS    XL2                 OPER ACTION NUMBER                           
*                                                                               
ADNAME   DS    XL3                 ADV NAME                                     
ADNUM    DS    XL1                 ADV NUMBER                                   
*                                                                               
SYNAME   DS    CL8                 SYS NAME                                     
SYNUM    DS    XL2                 SYS NUM                                      
SYGROUP  DS    A                   A(GROUP LIST)                                
*                                                                               
FFLAGS   DS    XL2                 FILE FLAGS                                   
FILNUM   DS    X                   FILE EXT NUMBER                              
         ORG   *-1                                                              
BRDNUM   DS    X                   BROADCAST NUMBER                             
FILNAM   DS    CL8                 FILE NAME                                    
ADTF     DS    A                   DTF FOR FILE                                 
*                                                                               
WORK     DS    XL80                                                             
WORK1    DS    XL80                                                             
CARD     DS    XL80                                                             
DATA     DS    XL16                                                             
*                                                                               
SCANBLK  DS    CL256                                                            
*                                                                               
IOAREA   DS    4096C               IO AREA                                      
BUFF1    DS    14336C              BUFFER 1                                     
BUFF2    DS    14336C              BUFFER 2                                     
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
         EJECT                                                                  
* DMDSHDR                                                                       
* DMDTFIS                                                                       
* DMDTFPH                                                                       
* DDCOMFACS                                                                     
* CTGENFILE                                                                     
* FADSECTS                                                                      
       ++INCLUDE DMDSHDR                                                        
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DMDTFPH                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FADSECTS                                                       
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'177SROPS00S  05/01/02'                                      
         END                                                                    
