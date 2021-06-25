*          DATA SET PETST00    AT LEVEL 001 AS OF 09/22/05                      
*&&      SET   NOP=N                                                            
*PHASE TE0900A                                                                  
         TITLE 'PETST00 - TEST MODULE'                                          
PETEST   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**TST**,RA,RR=R4,CLEAR=Y                             
         USING WORKD,RC                                                         
         ST    R4,RELO                                                          
         ST    RD,SAVERD                                                        
         MVC   SVPARMS,0(R1)       SAVE S/R PARM LIST                           
         L     R8,ACOMFACS                                                      
         USING COMFACSD,R8         R8=A(COMFACS)                                
*                                                                               
         GOTO1 CPROTOFF            NO STORAGE PROTECT                           
*                                                                               
         GOTO1 CGETFACT,DMCB,(X'80',USRID),F#TUSER                              
         GOTO1 (RF),(R1),(X'80',AGYID),F#TAGY                                   
         GOTO1 (RF),(R1),(X'80',TRMID),F#TNUM                                   
         GOTO1 (RF),(R1),(X'80',OFFCD),F#TOFFCD                                 
*                                                                               
         L     R9,ATWA                                                          
         USING PETSTFFD,R9         R9=A(TWA)                                    
         LR    R7,R9                                                            
         AH    R7,=H'4096'         R7=SAVE STORAGE                              
         USING SAVED,R7                                                         
         CLI   FRSTTIME,0                                                       
         BNE   *+12                                                             
         MVI   FRSTTIME,1                                                       
         MVI   INSCREEN,X'FF'                                                   
*                                                                               
         LA    R1,TSTACTH          SET INITIAL CURSOR POS                       
         ST    R1,CURSOR                                                        
         XC    XCTL,XCTL                                                        
         XC    DA,DA                                                            
*                                                                               
         L     R1,=A(IOAREA-WORKD)                                              
         AR    R1,RC                                                            
         ST    R1,AIOAREA                                                       
*                                                                               
         L     R1,=A(BUFF2-WORKD)                                               
         AR    R1,RC                                                            
         ST    R1,ABUFF2                                                        
         MVC   ABUFF1,ATIA                                                      
*                                                                               
         SR    RF,RF               ARE WE ONLINE OR OFFLINE                     
         ICM   RF,15,CSWITCH                                                    
         BZ    OFFLINE                                                          
*                                                                               
         GOTO1 CSWITCH,DMCB,X'FEFFFFFF',0                                       
         L     RF,0(R1)                                                         
         USING SYSFACD,RF                                                       
         L     R1,VSSB                                                          
         DROP  RF                                                               
*                                                                               
         ST    R1,MYSSB                                                         
         L     R1,SSBTKADR-SSBD(R1)                                             
         MVC   LUID,TCBSYM-TCBD(R1)                                             
         L     R1,MYSSB                                                         
         MVC   SYSNAME,SSBSYSNA-SSBD(R1)                                        
         SR    RF,RF                                                            
         IC    RF,SSBSYSIX-SSBD(R1)                                             
         SRL   RF,4                                                             
         LA    RF,X'C0'(RF)                                                     
         STC   RF,SYSNAME+3                                                     
*                                                                               
         MVI   ONLINE,C'Y'                                                      
         B     MAIN                                                             
*                                                                               
OFFLINE  MVI   ONLINE,C'N'                                                      
         CLI   TWACOMMN,1                                                       
         BE    EXITX                                                            
         MVI   MODE,C'C'                                                        
         B     MAIN020                                                          
*                                                                               
XMOD     CLI   OPTN,C'D'           CAUSE A DUMP                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
XMOD1    CLI   OPTN,C'A'           CAUSE A DC H'0' ABEND                        
         BNE   *+12                                                             
         DC    H'0',C'$ABEND'                                                   
*                                                                               
XMOD2    CLI   OPTN,C'M'           BRING THE F'IN LOT DOWN                      
         BNE   XMOD3                                                            
         L     R1,CLIMACC          I'M A CRAPPER                                
         MVC   24(4,R1),=C'CRAP'                                                
         B     EXITX                                                            
         L     R1,MYSSB                                                         
         XC    SSBTKADR-SSBD(4,R1),SSBTKADR-SSBD(R1)                            
         DC    H'0'                                                             
*                                                                               
XMOD3    EQU   *                                                                
*                                                                               
EXITX    L     R1,CURSOR           SET CURSOR POS                               
         OI    6(R1),X'40'                                                      
         XMOD1                                                                  
*                                                                               
XIT1     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM MAKE LOTS OF PERSON UPDATES           *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     OI    TSTMSGH+6,X'80'                                                  
*                                                                               
*        BAS   RE,ACCUPD                                                        
*                                                                               
MAIN1    BAS   RE,VALACT           VALIDATE ACTION                              
         CLI   MODE,X'FF'                                                       
         BE    ERR2                INVALID INPUT                                
*                                                                               
         BAS   RE,VALOPT           VALIDATE OPTION                              
         CLI   OPTN,X'FF'                                                       
         BE    ERR2                INVALID INPUT                                
*                                                                               
         CLI   ONLINE,C'N'         ARE WE RUNNING OFFLINE NOW                   
         B     MAIN020                                                          
*                                                                               
         LA    R1,TSTOFFH          VALIDATE ONLINE/OFFLINE                      
         ST    R1,CURSOR                                                        
         CLI   TSTOFFH+5,0         ANY OFFLINE INFO                             
         BE    MAIN020                                                          
         CLI   TSTOFFH+5,3         ANY OFFLINE INFO                             
         BNE   ERR2                                                             
         MVI   MODE,C'O'           THEN SET OFFLINE MODE                        
         BAS   RE,REPORT                                                        
         B     MAINX                                                            
*                                                                               
*        CLI   MODE,C'B'           TEST FOR BUY                                 
*        BNE   *+12                                                             
*        BAS   RE,BUY                                                           
*        B     MAINX                                                            
*                                                                               
MAIN020  LA    R1,TSTIN1H          VALIDATE STARTKEY                            
         ST    R1,CURSOR                                                        
*                                                                               
         CLI   MODE,C'S'           TEST FOR SWAP                                
         BNE   *+12                                                             
         BAS   RE,SWAP                                                          
         B     MAINX                                                            
*                                                                               
         CLI   MODE,C'T'           MODE REPORT                                  
         BNE   *+12                                                             
         BAS   RE,REPORT                                                        
         B     MAINX                                                            
*                                                                               
         CLI   MODE,C'C'           TEST FOR CHANGE                              
         BNE   *+12                                                             
         BAS   RE,CHANGE                                                        
         B     MAINX                                                            
*                                                                               
         CLI   MODE,C'E'           TEST FOR EXTEND                              
         BNE   *+12                                                             
         BAS   RE,CHANGE                                                        
         B     MAINX                                                            
*                                                                               
         CLI   MODE,C'R'           TEST FOR RESET                               
         BNE   *+12                                                             
         BAS   RE,CHANGE                                                        
         B     MAINX                                                            
*                                                                               
         CLI   MODE,C'A'           TEST FOR ADD                                 
         BNE   *+12                                                             
         BAS   RE,ADD                                                           
         B     MAINX                                                            
*                                                                               
         CLI   MODE,C'D'           TEST FOR DELETE                              
         BNE   *+12                                                             
         BAS   RE,CHANGE                                                        
         B     MAINX                                                            
*                                                                               
         CLI   MODE,C'L'           TEST FOR LIST                                
         BNE   *+12                                                             
         BAS   RE,LIST                                                          
         B     MAINX                                                            
*                                                                               
MAINX    B     XMOD                                                             
         EJECT                                                                  
*************************************************************                   
*        INITIAL RECORD CREATION                            *                   
*************************************************************                   
         SPACE 1                                                                
ADD      NTR1                                                                   
*                                                                               
         MVI   SCREEN,X'FF'        LOAD FF SCREEN IF NECESSARY                  
         BAS   RE,LOADSCR                                                       
*                                                                               
         LA    R1,TSTIN1H          VALIDATE STARTKEY                            
         BAS   RE,VALKEY                                                        
         BZ    ERR2                                                             
         MVC   KEY1,WORK                                                        
         LA    R1,TSTIN3H          VALIDATE ENDKEY                              
         BAS   RE,VALKEY                                                        
         BNZ   *+14                                                             
         MVC   KEY2,KEY1           SET ENDKEY TO STARTKEY IF MISSING            
         B     *+10                                                             
         MVC   KEY2,WORK                                                        
*                                                                               
         LA    R1,TSTIN5H          VALIDATE ABEND/COMMIT OPTIONS                
         BAS   RE,VALABN                                                        
         CLI   ABND,X'FF'                                                       
         BE    ERR2                                                             
*                                                                               
         LA    R1,TSTIN1H                                                       
         ST    R1,CURSOR                                                        
         XC    HALF,HALF                                                        
         XC    HALF1,HALF1                                                      
*                                                                               
ADD050   XC    IOAREA(255),IOAREA                                               
         MVC   IOAREA+0(4),KEY1                                                 
         MVC   IOAREA+32(4),KEY1+32                                             
         MVC   IOAREA+36(2),=X'003F'                                            
         MVC   IOAREA+44(2),=X'0112'                                            
         MVC   IOAREA+46(16),=CL16'RECORD ADDED '                               
*                                                                               
ADD010   GOTO1 CDATAMGR,DMCB,ADDREC,PERFIL,DA,AIOAREA,IOWORK                    
         BNE   ERR10                                                            
         LH    R0,HALF             BUMP NUMBER OF ADDS                          
         AHI   R0,1                                                             
         STH   R0,HALF                                                          
         LH    R0,HALF1            BUMP NUMBER OF ADDS BETWEEN COMMITS          
         AHI   R0,1                                                             
         STH   R0,HALF1                                                         
         OC    COMCNT,COMCNT       COMMIT EVERY N RECORDS SPECIFIED             
         BZ    ADD015                                                           
         CLC   HALF1,COMCNT                                                     
         BL    ADD015                                                           
         XC    HALF1,HALF1                                                      
         GOTO1 CDATAMGR,DMCB,=C'COMMIT'                                         
*                                                                               
ADD015   SR    R1,R1                                                            
         ICM   R1,15,IOAREA+32                                                  
         AHI   R1,1                                                             
         STCM  R1,15,IOAREA+32                                                  
         C     R1,KEY2+32                                                       
         BH    ADD020                                                           
*                                                                               
         LA    R1,TSTIN2H          WAIT FOR TIME IN IN2                         
         BAS   RE,WAIT                                                          
         B     ADD010                                                           
*                                                                               
ADD020   LA    R1,TSTIN4H          WAIT FOR TIME IN IN4                         
         BAS   RE,WAIT                                                          
*                                                                               
         CLI   ABND,C'D'           DUMP REQUIRED                                
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   ABND,C'M'           MAXI REQUIRED                                
         BNE   ADD030                                                           
         L     R1,MYSSB                                                         
         XC    SSBTKADR-SSBD(4,R1),SSBTKADR-SSBD(R1)                            
         DC    H'0'                                                             
*                                                                               
ADD030   MVC   TSTMSG(60),=CL60'XXXX RECORDS ADDED XXXXXXXX'                    
         LH    R1,HALF                                                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TSTMSG(4),DUB                                                    
         GOTO1 CHEXOUT,PARM,DA,TSTMSG+19,4,0                                    
         LA    R1,TSTACTH                                                       
         ST    R1,CURSOR                                                        
         B     XMOD                                                             
         EJECT                                                                  
************************************************************                    
*        CHANGE SPECIFIED RECORDS                          *                    
************************************************************                    
         SPACE 1                                                                
CHANGE   NTR1                                                                   
         CLI   ONLINE,C'N'                                                      
         BE    CHA001                                                           
         MVI   SCREEN,X'FF'                                                     
         BAS   RE,LOADSCR                                                       
*                                                                               
CHA001   LA    R1,TSTIN1H          VALIDATE STARTKEY                            
         BAS   RE,VALKEY                                                        
         BZ    ERR2                                                             
         MVC   KEY1,WORK                                                        
         LA    R1,TSTIN3H          VALIDATE ENDKEY                              
         BAS   RE,VALKEY                                                        
         BNZ   *+14                                                             
         MVC   KEY2,KEY1           SET ENDKEY TO STARTKEY IF MISSING            
         B     *+10                                                             
         MVC   KEY2,WORK                                                        
         LA    R1,TSTIN6H          VALIDATE STARTKEY                            
         BAS   RE,VALKEY                                                        
         BZ    ERR2                                                             
         MVC   KEY3,WORK                                                        
         LA    R1,TSTIN8H          VALIDATE ENDKEY                              
         BAS   RE,VALKEY                                                        
         BNZ   *+14                                                             
         MVC   KEY4,KEY3           SET ENDKEY TO STARTKEY IF MISSING            
         B     *+10                                                             
         MVC   KEY4,WORK                                                        
*                                                                               
CHNG000  B     CHNG005             SKIP LOCKUP CALLS                            
*                                                                               
         XC    WORK1,WORK1         LOCKUP LOCK 4CHR ID                          
         MVC   WORK1+3(2),=X'0001'                                              
         MVC   WORK1+5(4),KEY1                                                  
         GOTO1 CLOCKUP,DMCB,(C'L',WORK1),ACOMFACS,(C'Y',EXTRA)                  
         CLI   5(R1),0                                                          
         BE    CHNG005                                                          
                                                                                
         CLI   4(R1),1                                                          
         BE    ERR11                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
************************************************************                    
*        READ HI FOR KEY IN KEY1                           *                    
************************************************************                    
         SPACE 1                                                                
CHNG005  MVC   KEY,KEY1            RD HI FOR FIRST                              
         GOTO1 CDATAMGR,DMCB,(X'00',DMRDHI),PERDIR,KEY,KEY                      
         CLI   DMCB+8,2                                                         
         BE    *+14                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHNG010  CLC   KEY(36),KEY2        TEST WITH ENDKEY                             
         BH    CHNG050                                                          
         MVC   DA,KEY+38           GET DISK ADDRESS                             
         SPACE 1                                                                
************************************************************                    
*        GET RECORD FOR UPDATE                             *                    
************************************************************                    
         SPACE 1                                                                
         GOTO1 CDATAMGR,DMCB,(X'88',GETREC),PERFIL,DA,AIOAREA,IOWORK            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,TSTIN2H          WAIT FOR TIME IN IN2                         
         BAS   RE,WAIT                                                          
*                                                                               
         CLI   MODE,C'D'           IS THIS DELETE MODE                          
         BE    *+12                                                             
*                                                                               
         BAS   RE,UPDATE           UPDATE THE RECORD                            
         B     *+12                                                             
*                                                                               
         OI    IOAREA+38,X'80'     OR DELETE REC AND INDEX                      
         OI    KEY+36,X'80'                                                     
*                                                                               
         SPACE 1                                                                
************************************************************                    
*        PUT UPDATED RECORD BACK                           *                    
************************************************************                    
         SPACE 1                                                                
         GOTO1 CDATAMGR,DMCB,PUTREC,PERFIL,DA,IOAREA,IOWORK                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHNG020  CLI   MODE,C'D'           ARE WE DELETING                              
         BNE   CHNG020X                                                         
         GOTO1 CDATAMGR,DMCB,DMWRT,PERDIR,KEY,KEY                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHNG020X EQU   *                                                                
         SPACE 1                                                                
************************************************************                    
*        READ SEQ FOR NEXT                                 *                    
************************************************************                    
         SPACE 1                                                                
         GOTO1 CDATAMGR,DMCB,(X'00',DMRSEQ),PERDIR,KEY,KEY                      
         CLI   DMCB+8,2                                                         
         BE    *+14                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     CHNG010                                                          
         SPACE 1                                                                
************************************************************                    
*        WAIT FOR TIME IN IN4                              *                    
************************************************************                    
         SPACE 1                                                                
CHNG050  LA    R1,TSTIN4H          WAIT FOR TIME IN IN4                         
         BAS   RE,WAIT                                                          
*                                                                               
         CLC   TSTIN5(1),=C'Y'     WAS DUMP=Y                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TSTIN5(4),=C'MAXI'  WAS DUMP=MAXI                                
         BNE   CHNG051                                                          
         L     R1,MYSSB                                                         
         XC    SSBTKADR-SSBD(4,R1),SSBTKADR-SSBD(R1)                            
         DC    H'0'                                                             
         SPACE 1                                                                
*************************************************************                   
*        LOCKUP STUFF                                       *                   
*************************************************************                   
         SPACE 1                                                                
CHNG051  B     CHNG070                                                          
*                                                                               
         XC    WORK1,WORK1         LOCKUP UNLOCK 4CHR ID                        
         XC    WORK1,WORK1         LOCKUP UNLOCK 4CHR ID                        
         MVC   WORK1+3(2),=X'0001'                                              
         MVC   WORK1+5(4),KEY1                                                  
         GOTO1 CLOCKUP,DMCB,(C'U',WORK1),ACOMFACS,(C'Y',EXTRA)                  
         CLI   4(R1),0                                                          
         BE    *+14                                                             
                                                                                
         CLI   4(R1),1                                                          
         BE    ERR11                                                            
         DC    H'0'                                                             
*                                                                               
         MVC   KEY,KEY3                                                         
         OC    KEY,KEY                                                          
         BZ    CHNGX                                                            
*                                                                               
         XC    WORK1,WORK1         LOCKUP LOCK 4CHR ID                          
         MVC   WORK1+3(2),=X'0001'                                              
         MVC   WORK1+5(4),KEY3                                                  
         GOTO1 CLOCKUP,DMCB,(C'L',WORK1),ACOMFACS,(C'Y',EXTRA)                  
         CLI   4(R1),0                                                          
         BE    CHNG070                                                          
*                                                                               
         CLI   4(R1),1                                                          
         BE    ERR11                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
************************************************************                    
*        READ HI FOR KEY IN KEY3                           *                    
************************************************************                    
         SPACE 1                                                                
CHNG070  MVC   KEY,KEY3                                                         
         GOTO1 CDATAMGR,DMCB,(X'00',DMRDHI),PERDIR,KEY,KEY                      
         CLI   DMCB+8,2                                                         
         BE    *+14                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DA,KEY+38                                                        
*                                                                               
CHNG080  CLC   KEY(36),KEY4        TEST WITH ENDKEY                             
         BH    CHNG150                                                          
         MVC   DA,KEY+38           GET DISK ADDRESS                             
         SPACE 1                                                                
************************************************************                    
*        GET RECORD FOR UPDATE                             *                    
************************************************************                    
         SPACE 1                                                                
         GOTO1 CDATAMGR,DMCB,(X'88',GETREC),PERFIL,DA,AIOAREA,IOWORK            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,TSTIN7H          WAIT FOR TIME IN IN2                         
         BAS   RE,WAIT                                                          
*                                                                               
         CLI   MODE,C'D'           IS THIS DELETE MODE                          
         BE    *+12                                                             
*                                                                               
         BAS   RE,UPDATE           UPDATE THE RECORD                            
         B     *+12                                                             
*                                                                               
         OI    IOAREA+38,X'80'     OR DELETE REC AND INDEX                      
         OI    KEY+36,X'80'                                                     
*                                                                               
         SPACE 1                                                                
************************************************************                    
*        PUT UPDATED RECORD BACK                           *                    
************************************************************                    
         SPACE 1                                                                
         GOTO1 CDATAMGR,DMCB,PUTREC,PERFIL,DA,AIOAREA,IOWORK                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHNG090  CLI   MODE,C'D'           ARE WE DELETING                              
         BNE   CHNG090X                                                         
         GOTO1 =V(DATAMGR),DMCB,DMWRT,PERDIR,KEY,IOAREA                         
         BE    *+6                                                              
         DC    H'0'                                                             
CHNG090X EQU   *                                                                
         SPACE 1                                                                
************************************************************                    
*        READ SEQ FOR NEXT                                 *                    
************************************************************                    
         SPACE 1                                                                
         GOTO1 CDATAMGR,DMCB,(X'00',DMRSEQ),PERDIR,KEY,KEY                      
         CLI   DMCB+8,2                                                         
         BE    *+14                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     CHNG080                                                          
         SPACE 1                                                                
************************************************************                    
*        WAIT FOR TIME IN IN4                              *                    
************************************************************                    
         SPACE 1                                                                
CHNG150  LA    R1,TSTIN9H          WAIT FOR TIME IN IN4                         
         BAS   RE,WAIT                                                          
*                                                                               
         CLC   TSTINA(1),=C'Y'                                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   TSTINA(4),=C'MAXI'                                               
         BNE   *+16                                                             
         L     R1,MYSSB                                                         
         XC    SSBTKADR-SSBD(4,R1),SSBTKADR-SSBD(R1)                            
         DC    H'0'                                                             
         B     CHNGX                                                            
         SPACE 1                                                                
*************************************************************                   
*        LOCKUP STUFF                                       *                   
*************************************************************                   
         SPACE 1                                                                
         XC    WORK1,WORK1         LOCKUP UNLOCK 4CHR ID                        
         MVC   WORK1+3(2),=X'0001'                                              
         MVC   WORK1+5(4),KEY3                                                  
         GOTO1 CLOCKUP,DMCB,(C'U',WORK1),ACOMFACS,(C'Y',EXTRA)                  
         CLI   4(R1),0                                                          
         BE    *+14                                                             
*                                                                               
         CLI   4(R1),1                                                          
         BE    ERR11                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
*************************************************************                   
*        FINALLY EXIT                                       *                   
*************************************************************                   
         SPACE 1                                                                
CHNGX    GOTO1 CGETFACT,DMCB,(X'80',FULL),F#BIOCNA                              
         SR    RF,RF                                                            
         ICM   RF,7,FULL                                                        
         EDIT  (RF),(5,TSTOUT1)                                                 
         OI    TSTOUT1H+6,X'80'                                                 
         EDIT  (1,TESTBYTE),(3,TSTOUT2)                                         
         OI    TSTOUT2H+6,X'80'                                                 
         MVC   TSTMSG(60),=CL60'RECORDS CHANGED'                                
         LA    R1,TSTACTH                                                       
         ST    R1,CURSOR                                                        
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        UPDATE ROUTINE                                     *                   
*************************************************************                   
         SPACE 1                                                                
UPDATE   NTR1                                                                   
         MVI   SCREEN,X'FF'                                                     
         BAS   RE,LOADSCR                                                       
*                                                                               
         CLI   MODE,C'R'                                                        
         BNE   UPDAT01                                                          
*                                                                               
         MVC   IOAREA+36(2),=X'003F'                                            
         MVC   IOAREA+44(2),=X'0112'                                            
         MVC   IOAREA+46(16),=C'01JAN00 INITIAL '                               
         LA    R1,IOAREA+62                                                     
         B     UPDATE90                                                         
*                                                                               
UPDAT01  GOTO1 CDATCON,DMCB,(5,0),(8,WORK)                                      
         MVC   IOAREA+46(7),WORK   INSERT TODAYS DATE                           
*                                                                               
         TIME  BIN                 EXTRACT TIME FOR UPDATE                      
         ST    R0,FULL                                                          
         BAS   RE,TIMEOUT                                                       
         MVC   IOAREA+36(2),=X'003F'   DEFAULT LENGTH                           
*                                                                               
         MVC   IOAREA+44(2),=X'0112'                                            
         MVC   IOAREA+54(8),WORK1  INSERT TIME NOW                              
*                                                                               
         LA    R1,IOAREA+62        THIS IS WHERE EXTENSION ELEMENTS GO          
*                                                                               
UPDATE05 CLI   0(R1),0                                                          
         BE    UPDATE10                                                         
*                                                                               
         SR    R0,R0               FIND THE END OF THE RECORD                   
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     UPDATE05                                                         
*                                                                               
UPDATE10 CLI   MODE,C'C'           CHANGE                                       
         BE    UPDATE90            DONT EXTEND ANY MORE                         
         CLI   MODE,C'E'                                                        
         BNE   UPDATE90            IF MODE=E EXTEND IT                          
*                                                                               
         MVC   0(16,R1),=X'0210C5E7E3C5D5C440C9E340E2D6D4C5'                    
         LA    R1,16(R1)                                                        
*                                                                               
UPDATE90 LA    RF,IOAREA           SET LENGTH THEN EXIT                         
         MVI   0(R1),0             MAKE SURE WE HAVE A ZERO                     
         LA    R1,1(R1)                                                         
         SR    R1,RF                                                            
         STCM  R1,3,IOAREA+36                                                   
*                                                                               
UPDATEX  B     XIT1                                                             
         EJECT                                                                  
************************************************************                    
*        OFFLINE - GENERATE REQUEST                        *                    
************************************************************                    
         SPACE 1                                                                
REPORT   NTR1                                                                   
*                                                                               
         LA    R1,TSTIN1H          VALIDATE STARTKEY                            
         BAS   RE,VALKEY                                                        
         MVC   KEY1,WORK                                                        
         LA    R1,TSTIN3H          VALIDATE ENDKEY                              
         BAS   RE,VALKEY                                                        
         MVC   KEY2,WORK                                                        
         LA    R1,TSTIN6H          VALIDATE STARTKEY                            
         BAS   RE,VALKEY                                                        
         MVC   KEY3,WORK                                                        
         LA    R1,TSTIN8H          VALIDATE ENDKEY                              
         BAS   RE,VALKEY                                                        
         MVC   KEY4,WORK                                                        
*                                                                               
         OC    KEY1,KEY1                                                        
         BZ    REPXX                                                            
*                                                                               
         LA    R1,SPOOK1                                                        
         USING SPOOK,R1                                                         
*                                                                               
*        FILL IN SPOOK HERE                                                     
*                                                                               
         MVC   SPOOKDID,=C'PER'                                                 
         MVC   SPOOKSYS,=C'PE'                                                  
         MVC   SPOOKEOD,=C'TS'                                                  
         MVC   SPOOKJCL,=C'TS'                                                  
         MVI   SPOOKWEN,2                                                       
         DROP  R1                                                               
*                                                                               
         LA    R1,REQH                                                          
         USING REQHDR,R1                                                        
*                                                                               
*        FILL IN REQUEST HERE                                                   
*                                                                               
         MVI   REQFLAG,8                                                        
         DROP  R1                                                               
*                                                                               
         GOTO1 CREQTWA,DMCB,ATWA,REQH,CDATAMGR,ACOMFACS,SPOOK1,0                
*                                                                               
*        P3 = REPORT DESCRIPTION                                                
*                                                                               
REPXX    SR    RF,RF                                                            
         ICM   RF,7,FULL                                                        
         EDIT  (RF),(5,TSTOUT1)                                                 
         OI    TSTOUT1H+6,X'80'                                                 
         EDIT  (1,TESTBYTE),(3,TSTOUT2)                                         
         OI    TSTOUT2H+6,X'80'                                                 
         B     XIT1                                                             
         EJECT                                                                  
************************************************************                    
*        BUY SIMULATOR                                     *                    
************************************************************                    
         SPACE 1                                                                
BUY      NTR1                                                                   
*                                                                               
         LH    R5,=H'1000'         LOOP 1000 TIMES                              
*                                                                               
         TIME  TU                                                               
         LR    R1,R0               PUT TIME NOW IN R1                           
         SR    R0,R0                                                            
         ST    R1,FULL             SAVE TIME NOW                                
         D     R0,=F'1152000'      DIVIDE BY 30 SEC TUS                         
         LA    R1,1(R1)            +1                                           
         SR    R0,R0                                                            
         M     R0,=F'1152000'      MULT BY 30 SEC TUS                           
         S     R1,FULL                                                          
         ST    R1,FULL             FULL = TIME TO WAIT                          
*                                                                               
         GOTO1 CGETFACT,DMCB,(X'80',FULL),F#WAIT                                
*                                                                               
BUY001   XC    KEY,KEY                                                          
         MVC   KEY(4),=C'BUY1'                                                  
         MVC   KEY+32,=X'00000000'                                              
*                                                                               
BUY005   GOTO1 CDATAMGR,DMCB,(X'80',DMRDHI),PERDIR,KEY,KEY                      
         CLI   DMCB+8,2                                                         
         BE    *+14                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   R1,15,KEY+32                                                     
*        BCTR  R1,0                                                             
*                                                                               
*        XC    IOAREA(255),IOAREA                                               
*        MVC   IOAREA+0(4),=C'BUY2'                                             
*        STCM  R1,15,IOAREA+32                                                  
*        MVC   IOAREA+36(2),=X'003F'                                            
*        MVC   IOAREA+44(2),=X'0112'                                            
*        L     RE,MYSSB                                                         
*        MVC   IOAREA+46(16),=C'----------------'                               
*        MVC   IOAREA+46(4),SSBSYSNA-SSBD(RE)                                   
*        SR    R1,R1                                                            
*        IC    R1,SSBSYSIX-SSBD(RE)                                             
*        SRL   R1,4                                                             
*        LA    R1,X'C0'(R1)                                                     
*        STC   R1,IOAREA+49                                                     
*                                                                               
*        GOTO1 CDATAMGR,DMCB,ADDREC,PERFIL,DA,AIOAREA,IOWORK                    
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         XC    IOAREA(255),IOAREA                                               
         MVC   IOAREA+0(4),=C'BUY1'                                             
         ICM   R1,15,KEY+32                                                     
         BCTR  R1,0                                                             
         STCM  R1,15,IOAREA+32                                                  
         MVC   IOAREA+36(2),=X'003F'                                            
         MVC   IOAREA+44(2),=X'0112'                                            
         L     RE,MYSSB                                                         
         MVC   IOAREA+46(16),=C'----------------'                               
         MVC   IOAREA+52(8),LUID                                                
         MVC   IOAREA+46(4),SSBSYSNA-SSBD(RE)                                   
         SR    R1,R1                                                            
         IC    R1,SSBSYSIX-SSBD(RE)                                             
         SRL   R1,4                                                             
         LA    R1,X'C0'(R1)                                                     
         STC   R1,IOAREA+49                                                     
*                                                                               
         GOTO1 CDATAMGR,DMCB,ADDREC,PERFIL,DA,AIOAREA,IOWORK                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 CDATAMGR,DMCB,=C'DMUNLK',0                                       
*                                                                               
         TIME  TU                                                               
         LR    R1,R0               PUT TIME NOW IN R1                           
         SR    R0,R0                                                            
         ST    R1,FULL             SAVE TIME NOW                                
         D     R0,=F'0038400'      DIVIDE BY 1 SEC TUS                          
         LA    R1,1(R1)            +1                                           
         SR    R0,R0                                                            
         M     R0,=F'0038400'      MULT BY 1 SEC TUS                            
         S     R1,FULL                                                          
         ST    R1,FULL             FULL = TIME TO WAIT                          
*                                                                               
         GOTO1 CGETFACT,DMCB,(X'80',FULL),F#WAIT                                
         BCT   R5,BUY001                                                        
*                                                                               
         B     XIT1                                                             
         EJECT                                                                  
************************************************************                    
*        SWAP RECORD BETWEEN TOR/AOR                       *                    
************************************************************                    
         SPACE 1                                                                
SWAP     NTR1                                                                   
*                                                                               
         LH    R5,=H'5000'         LOOP 5000 TIMES                              
         XC    ERROR,ERROR                                                      
         SR    R3,R3                                                            
         ICM   R3,1,TSTOPTH+5                                                   
         BZ    SWAPX                                                            
         LA    R4,TSTOPT                                                        
         BAS   RE,VALNUM                                                        
         CLI   DUB,X'FF'                                                        
         BE    SWAPX                                                            
         CVB   R1,DUB                                                           
         ST    R1,TIME                                                          
         B     SWAP00                                                           
*                                                                               
         TIME  TU                                                               
         LR    R1,R0               PUT TIME NOW IN R1                           
         SR    R0,R0                                                            
         ST    R1,FULL             SAVE TIME NOW                                
         D     R0,=F'1152000'      DIVIDE BY 30 SEC TUS                         
         LA    R1,1(R1)            +1                                           
         SR    R0,R0                                                            
         M     R0,=F'1152000'      MULT BY 30 SEC TUS                           
         S     R1,FULL                                                          
         ST    R1,FULL             FULL = TIME TO WAIT                          
*                                                                               
         GOTO1 CGETFACT,DMCB,(X'80',FULL),F#WAIT                                
*                                                                               
SWAP00   MVC   DA,=X'00010102'                                                  
*                                                                               
SWAP01   ICM   R1,15,TIME          REDUCE TIME DELAY                            
         BZ    *+8                                                              
         SH    R1,=H'10'                                                        
         ST    R1,TIME                                                          
*                                                                               
         GOTO1 CDATAMGR,DMCB,(X'80',GETREC),PERFIL,DA,AIOAREA,IOWORK            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,MYSSB                                                         
         CLC   IOAREA+46(4),SYSNAME                                             
         BNE   SWAP02                                                           
         L     R1,ERROR                                                         
         LA    R1,1(R1)                                                         
         ST    R1,ERROR                                                         
*                                                                               
SWAP02   MVC   IOAREA+46(4),SYSNAME                                             
*                                                                               
         GOTO1 CDATAMGR,DMCB,PUTREC,PERFIL,DA,AIOAREA,IOWORK                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 CDATAMGR,DMCB,=C'DMUNLK',0                                       
*                                                                               
         OC    TIME,TIME                                                        
         BZ    SWAP03                                                           
         MVC   FULL,TIME                                                        
         GOTO1 CGETFACT,DMCB,(X'80',FULL),F#WAIT                                
*                                                                               
SWAP03   BAS   RE,POSTIT           POST THE OTHER ADV                           
         L     RE,MYSSB                                                         
         LA    R1,SSBVTHNG-SSBD(RE)                                             
         XC    0(4,R1),0(R1)                                                    
*                                                                               
         WAIT  1,ECB=(1)           AND WAIT FOR HIM                             
*                                                                               
         BCT   R5,SWAP01                                                        
*                                                                               
         BAS   RE,POSTIT           POST THE OTHER ADV                           
*                                                                               
SWAPX    SR    RF,RF                                                            
         ICM   RF,15,ERROR                                                      
         EDIT  (RF),(5,TSTOUT1),ZERO=NOBLANK                                    
         OI    TSTOUT1H+6,X'80'                                                 
*                                                                               
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        DO X MEMORY POST TO FACPAK (DETAILS IN CARD)       *                   
*************************************************************                   
         SPACE 1                                                                
POSTIT   NTR1                                                                   
         L     RE,MYSSB                                                         
*                                                                               
         LH    R4,=X'00A4'                                                      
         CLI   SYSNAME+3,C'A'                                                   
         BNE   *+8                                                              
         LH    R4,=X'00D3'                                                      
*                                                                               
         LA    R2,SSBVTHNG-SSBD(RE)                                             
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
         CLC   JSABJBNM(6),=C'FACTTS'                                           
         LA    RE,*+6              SWITCH BACK TO 24 BT MODE                    
         BSM   0,RE                                                             
         BNE   NOPOST                                                           
*                                                                               
         LA    R5,99               SET COMPLETION CODE                          
         POST  (R2),(R5),ASCB=(R3),LINKAGE=SYSTEM,ECBKEY=8,MF=(E,POSTX)         
         B     XIT1                                                             
*                                                                               
NOPOST   MVC   WORK1+10(8),WORK1                                                
         MVC   WORK1+2(8),WORK1+10                                              
         MVC   WORK1+10(18),=C' SYSTEM NOT POSTED'                              
         MVC   WORK1+0(2),=H'26'                                                
         WTO   TEXT=WORK1                                                       
         DC    H'0'                                                             
*                                                                               
POSTX    POST  ERRET=DEAD,ECBKEY=YES,MF=L                                       
DEAD     DC    H'0'                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
**************************************************************                  
*        LIST SPECIFIED RECORDS                              *                  
**************************************************************                  
         SPACE 1                                                                
LIST     NTR1                                                                   
*                                                                               
         MVI   SCREEN,X'FE'                                                     
         BAS   RE,LOADSCR                                                       
         TWAXC TSTLI1H,PROT=Y                                                   
*                                                                               
         XC    WORK,WORK                                                        
         LA    R3,TSTLI1H          TOP OF SCREEN                                
*                                                                               
         LA    R1,TSTIN1H          VALIDATE STARTKEY                            
         BAS   RE,VALKEY                                                        
         BZ    ERR2                                                             
         MVC   KEY1,WORK                                                        
*                                                                               
         LA    R1,TSTIN2H          VALIDATE ENDKEY                              
         BAS   RE,VALKEY                                                        
         BNZ   *+10                                                             
         MVC   WORK,=XL4'FFFFFFFF'                                              
         MVC   KEY2,WORK                                                        
         MVC   KEY,KEY1                                                         
*                                                                               
         GOTO1 CDATAMGR,DMCB,(X'08',DMRDHI),PERDIR,KEY,KEY                      
         CLI   DMCB+8,2                                                         
         BE    *+14                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LIST010  CLC   KEY(36),KEY2        TEST WITH ENDKEY                             
         BH    LISTX                                                            
LIST015  MVC   DA,KEY+38                                                        
*                                                                               
         GOTO1 CDATAMGR,DMCB,(X'08',GETREC),PERFIL,DA,AIOAREA,IOWORK            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   8(4,R3),IOAREA                                                   
         EDIT  (B4,IOAREA+32),(10,13(R3)),ALIGN=LEFT                            
         MVI   12(R3),C','                                                      
         MVC   23(2,R3),=C'--'                                                  
         MVC   26(16,R3),IOAREA+46                                              
*                                                                               
         EDIT  (B2,IOAREA+36),(5,44(R3)),ALIGN=LEFT                             
*                                                                               
         TM    IOAREA+38,X'80'                                                  
         BZ    *+10                                                             
         MVC   23(2,R3),=C'DE'                                                  
*                                                                               
         LA    RF,50(R3)                                                        
         LA    R1,IOAREA+44        FIRST ELEMENT                                
LIST020  CLI   0(R1),0                                                          
         BE    LIST030                                                          
         MVI   0(RF),C'E'          COUNT ELEMENTS                               
         LA    RF,1(RF)                                                         
         SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     LIST020                                                          
*                                                                               
LIST030  OI    6(R3),X'80'                                                      
         LA    R3,86(R3)                                                        
         LA    R1,TSTLIXH                                                       
         CR    R3,R1                                                            
         BNL   LIST050                                                          
*                                                                               
         GOTO1 CDATAMGR,DMCB,(X'00',DMRSEQ),PERDIR,KEY,KEY                      
         CLI   DMCB+8,2                                                         
         BE    LIST040                                                          
         CLI   DMCB+8,X'80'                                                     
         BE    LIST050                                                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LIST040  B     LIST010                                                          
*                                                                               
LIST050  MVC   TSTIN1(4),KEY                                                    
         MVI   TSTIN1+4,C','                                                    
         EDIT  (B4,KEY+32),(10,TSTIN1+5),ALIGN=LEFT                             
         OI    TSTIN1H+1,X'01'                                                  
         OI    TSTIN1H+6,X'80'                                                  
*                                                                               
LISTX    B     XIT1                                                             
         EJECT                                                                  
**************************************************************                  
* VALIDATE MODE AND RETURN SINGLE CHR MODE IN MODE           *                  
**************************************************************                  
         SPACE 1                                                                
VALACT   NTR1                                                                   
*                                                                               
         LA    R1,TSTACTH          SET CURSOR                                   
         ST    R1,CURSOR                                                        
         SR    R0,R0                                                            
         ICM   R0,1,5(R1)                                                       
         BZ    VALACTX                                                          
         MVC   DUB,TSTACT          EXTRACT ACTION                               
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
*                                                                               
         LA    RF,ACTNTBL          FIND ACTION                                  
VALACT01 CLI   0(RF),X'FF'                                                      
         BE    VALACTE                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),DUB         COMPARE FIELD DATA                           
         BE    VALACT02                                                         
         LA    RF,8(RF)                                                         
         B     VALACT01                                                         
*                                                                               
VALACT02 MVC   MODE,7(RF)          SAVE MODE AND EXIT                           
         MVC   TSTACT(7),0(RF)                                                  
         OI    TSTACTH+6,X'80'                                                  
         MVC   SAVP1(7),0(RF)                                                   
         B     VALACTX                                                          
*                                                                               
VALACTE  MVI   MODE,X'FF'          INVALID ACTION                               
*                                                                               
VALACTX  B     XIT1                                                             
         EJECT                                                                  
**************************************************************                  
* VALIDATE OPTION AND SET SINGLE CHR MODE IN OPTN            *                  
**************************************************************                  
         SPACE 1                                                                
VALOPT   NTR1                                                                   
*                                                                               
         LA    R1,TSTOPTH          SET CURSOR                                   
         ST    R1,CURSOR                                                        
         MVI   OPTN,0                                                           
         CLI   TSTOPTH+5,0                                                      
         BE    VALOPTX                                                          
*                                                                               
VALOPT1  CLC   TSTOPT(4),=C'DUMP'  CAUSE A DUMP                                 
         BNE   VALOPT2                                                          
         MVI   OPTN,C'D'                                                        
         B     VALOPTX                                                          
*                                                                               
VALOPT2  CLC   TSTOPT(4),=C'ABND'  CAUSE A DC H'0' ABEND                        
         BE    *+14                                                             
         CLC   TSTOPT(5),=C'ABEND'                                              
         BNE   VALOPT3                                                          
         MVI   OPTN,C'A'                                                        
         B     VALOPTX                                                          
*                                                                               
VALOPT3  CLC   TSTOPT(4),=C'MAXI'  BRING THE F'IN LOT DOWN                      
         BNE   VALOPT4                                                          
         MVI   OPTN,C'M'                                                        
         B     VALOPTX                                                          
*                                                                               
VALOPT4  MVI   OPTN,X'FF'          INVALID OPTION                               
*                                                                               
VALOPTX  B     XIT1                                                             
         EJECT                                                                  
**************************************************************                  
* VALIDATE ABEND AND SET SINGLE CHR MODE IN ABND             *                  
**************************************************************                  
         SPACE 1                                                                
VALABN   NTR1                                                                   
*                                                                               
         ST    R1,CURSOR           SET CURSOR                                   
         LR    R2,R1                                                            
         MVI   ABND,0                                                           
         XC    COMCNT,COMCNT                                                    
         SR    R3,R3                                                            
         ICM   R3,1,5(R2)          GET FIELD LENGTH                             
         BZ    VALABNX                                                          
                                                                                
VALABN1  CLC   8(4,R2),=C'DUMP'    CAUSE A DUMP                                 
         BNE   VALABN2                                                          
         MVI   ABND,C'D'                                                        
         B     VALABNX                                                          
*                                                                               
VALABN2  CLC   8(4,R2),=C'ABND'    CAUSE A DC H'0' ABEND                        
         BE    *+14                                                             
         CLC   8(5,R2),=C'ABEND'                                                
         BNE   VALABN3                                                          
         MVI   ABND,C'A'                                                        
         B     VALABNX                                                          
*                                                                               
VALABN3  CLC   8(4,R2),=C'MAXI'    BRING THE F'IN LOT DOWN                      
         BNE   VALABN4                                                          
         MVI   ABND,C'M'                                                        
         B     VALABNX                                                          
*                                                                               
VALABN4  CLC   8(7,R2),=C'COMMIT=' COMMIT=NN                                    
         BNE   VALABN5                                                          
         AHI   R3,-7                                                            
         BNP   VALABNE                                                          
         LA    R4,15(R2)                                                        
         BAS   RE,VALNUM                                                        
         CLI   DUB,X'FF'                                                        
         BE    VALABNE                                                          
         MVI   ABND,C'C'                                                        
         CVB   R1,DUB                                                           
         STH   R1,COMCNT           SAVE COMMIT COUNT                            
         B     VALABNX                                                          
*                                                                               
VALABN5  B     VALABNE                                                          
*                                                                               
VALABNE  MVI   ABND,X'FF'          SET INVALID FIELD INPUT                      
*                                                                               
VALABNX  B     XIT1                                                             
         EJECT                                                                  
**************************************************************                  
* VALIDATE KEY IN FIELD AT R1 AND MOVE IT TO WORK            *                  
**************************************************************                  
         SPACE 1                                                                
VALKEY   NTR1                                                                   
*                                                                               
         ST    R1,CURSOR                                                        
         XC    WORK,WORK           START WITH ZERO KEY                          
         SR    R0,R0                                                            
         ICM   R0,1,5(R1)                                                       
         BZ    VALKEYX                                                          
         OC    8(12,R1),SPACES                                                  
         MVC   WORK(4),8(R1)       KEY IS XXXX,NNNN                             
         CLI   12(R1),C','                                                      
         BNE   VALK010             OR XXXX                                      
*                                                                               
         SH    R0,=H'5'            GET LENGTH OF NUMERIC PART OF KEY            
         BNP   VALK010                                                          
         LR    R3,R0                                                            
         LA    R4,13(R1)                                                        
         BAS   RE,VALNUM           VALIDATE NUMERIC PART                        
         CLI   DUB,X'FF'                                                        
         BE    VALKEYE                                                          
         CVB   R1,DUB                                                           
         ST    R1,WORK+32          SET NUMERIC PART OF KEY                      
         B     VALKEYX                                                          
*                                                                               
VALK010  MVC   WORK+32(4),=X'00000001'                                          
         B     VALKEYX                                                          
*                                                                               
VALKEYE  XC    WORK,WORK           CLEAR KEY ON ERROR                           
*                                                                               
VALKEYX  OC    WORK(4),WORK        KEY CAN'T BE NULLS                           
         B     XIT1                                                             
         EJECT                                                                  
**************************************************************                  
* VALIDATE NUMBER IN FIELD AT R1 AND WAIT FOR 1/10 SEC X VAL *                  
**************************************************************                  
         SPACE 1                                                                
WAIT     NTR1                                                                   
*                                                                               
         CLI   ONLINE,C'Y'         ON OR OFFLINE                                
         BNE   WAITOFF                                                          
*                                                                               
         SR    R3,R3               VALIDATE NUMBER R1 FIELD                     
         ICM   R3,1,5(R1)                                                       
         BZ    WAITX                                                            
         LA    R4,8(R1)                                                         
         BAS   RE,VALNUM                                                        
         CLI   DUB,X'FF'                                                        
         BE    WAITX                                                            
         CVB   R1,DUB                                                           
         MH    R1,=H'3840'         VAL X 3840 = 1/10 SECOND                     
         ST    R1,FULL                                                          
*                                                                               
         CLI   ONLINE,C'Y'         ON OR OFFLINE                                
         BNE   WAITOFF                                                          
*                                                                               
         GOTO1 CGETFACT,DMCB,(X'80',FULL),F#WAIT                                
         B     WAITX                                                            
*                                                                               
WAITOFF  MVC   WAITIME,TENSEC                                                   
         STIMERM SET,ID=STIMERID,BINTVL=WAITIME,WAIT=YES                        
         LTR   RF,RF                                                            
         BZ    WAITXX                                                           
         DC    H'0'                                                             
*                                                                               
WAITX    EQU   *                                                                
WAITXX   B     XIT1                                                             
*                                                                               
TENSEC   DC    F'1000'                                                          
WAITIME  DC    F'0'                                                             
STIMERID DC    F'0'                                                             
         EJECT                                                                  
***************************************************                             
*        EDIT TUS IN FULL TO WORK HH:MM:SS.SS     *                             
***************************************************                             
         SPACE 1                                                                
TIMEOUT  ST    RE,SAVERE                                                        
         MVC   WORK1(11),=C'00:00:00.00'                                        
         SR    RE,RE                                                            
         L     RF,FULL                                                          
         D     RE,=F'360000'                                                    
         EDIT  (RF),(2,WORK1),FILL=0      HRS                                   
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'6000'                                                      
         EDIT  (RF),(2,WORK1+3),FILL=0    MINS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         EDIT  (RF),(2,WORK1+6),FILL=0    SECS                                  
         EDIT  (RE),(2,WORK1+9),FILL=0    100/SEC                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
*************************************************************                   
*        LOAD OVERLAY SCREEN IF REQUIRED                    *                   
*************************************************************                   
         SPACE 1                                                                
LOADSCR  NTR1                                                                   
         CLC   INSCREEN,SCREEN                                                  
         BE    LOADSX                                                           
         LA    R6,TSTMSGH                                                       
         CLI   SCREEN,X'FF'                                                     
         BE    *+8                                                              
         LA    R6,TSTLOADH                                                      
         MVC   DMCB+4(4),=X'D90E09FF'                                           
         ST    R6,DMCB                                                          
         MVC   DMCB+7(1),SCREEN                                                 
*                                                                               
         GOTO1 CCALLOV,DMCB                                                     
         MVC   INSCREEN,SCREEN                                                  
*                                                                               
         MVC   TSTACT,SAVP1                                                     
         OI    TSTACTH+6,X'80'                                                  
*                                                                               
LOADSX   B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        SWITCH TO ACC AND MAKE UPDATES                     *                   
*************************************************************                   
         SPACE 1                                                                
ACCUPD   NTR1                                                                   
*                                                                               
         MVI   SENUM,X'66'         SWITCH TO ACC0                               
*                                                                               
         SR    R3,R3               VALIDATE NUMBER IN T3                        
         LA    R3,1                                                             
*        IC    R3,TSTIN3H+5                                                     
*        LA    R4,TSTIN3                                                        
*        BAS   RE,VALNUM                                                        
*        CLI   DUB,X'FF'                                                        
*        BE    ACCUPX                                                           
*                                                                               
         GOTO1 CSWITCH,DMCB,(SENUM,X'FFFFFFFF'),0                               
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TSTOPT(4),=C'ABND'                                               
         BNE   *+12                                                             
         DC    H'0',C'$ABEND'                                                   
*                                                                               
         XC    TSTIN3,TSTIN3       CLR AND XMIT                                 
         OI    TSTIN3H+6,X'80'                                                  
*                                                                               
         LA    R4,1                LOOP THIS MANY TIMES                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(6),ACCKEY                                                    
*                                                                               
         GOTO1 CDATAMGR,DMCB,(X'00',DMRDHI),ACCDIR,KEY,KEY                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ACCU010  XI    KEY+42,X'02'                                                     
         MVC   DA,KEY+50                                                        
*                                                                               
         GOTO1 CDATAMGR,DMCB,(X'80',GETREC),ACCMST,DA,AIOAREA,IOWORK            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIOAREA                                                       
         XI    42(R1),X'02'                                                     
*                                                                               
         GOTO1 CDATAMGR,DMCB,PUTREC,ACCMST,DA,AIOAREA,IOWORK                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BCT   R4,ACCU010                                                       
*                                                                               
         MVI   SENUM,X'0E'         SWITCH TO PER1                               
*                                                                               
         GOTO1 CSWITCH,DMCB,(SENUM,X'FFFFFFFF'),0                               
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ACCUPX   XIT1                                                                   
*                                                                               
ACCKEY   DC    X'0241',C'VAAA'                                                  
         EJECT                                                                  
*************************************************************                   
*        TEST SUBROUTINES                                   *                   
*************************************************************                   
         SPACE 1                                                                
DISPFLD  NTR1                                                                   
         LR    RF,R1                                                            
         XC    WORK1,WORK1                                                      
         MVC   WORK1(16),=C'L=??,IP=????????'                                   
         EDIT  (B1,5(RF)),(2,WORK1+2),ZERO=NOBLANK,FILL=0                       
         MVC   BYTE,4(RF)                                                       
         BAS   RE,TITBITS                                                       
         MVC   WORK1+8(8),DUB                                                   
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        COMMON SUBROUTINES BITS IN BYTE TO DUB 1.1.1.1.    *                   
*************************************************************                   
         SPACE 1                                                                
TITBITS  ST    RE,SAVERE           SET BIT PATERN FROM BYTE IN DUB              
         LA    RE,DUB                                                           
         LA    R1,X'80'                                                         
TBITS1   MVI   0(RE),C'.'          DEFAULT TO . FOR CLEAR BIT                   
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    BYTE,0              TEST BIT                                     
         BNO   *+8                                                              
         MVI   0(RE),C'1'          SET TO 1 IF ON                               
         LA    RE,1(RE)                                                         
         SRL   R1,1                NEXT BIT                                     
         LTR   R1,R1                                                            
         BNZ   TBITS1                                                           
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        VALNUM R4=A(FIELD),R3=LEN  EXIT DUB=PACKED OR FF   *                   
*************************************************************                   
         SPACE 1                                                                
       ++INCLUDE DDVALNUM                                                       
         EJECT                                                                  
*************************************************************                   
*        ERROR & INFO EXITS                                 *                   
*************************************************************                   
         SPACE 1                                                                
ERR1     MVC   TSTMSG(60),=CL60'MISSING INPUT FIELD'                            
         B     ERRX                                                             
*                                                                               
ERR2     MVC   TSTMSG(60),=CL60'INVALID INPUT FIELD'                            
         B     ERRX                                                             
*                                                                               
ERR10    MVC   TSTMSG(60),=CL60'I/O ERROR'                                      
         MVC   BYTE,DMCB+8                                                      
         GOTO1 CHEXOUT,PARM,BYTE,TSTMSG+10,1,0                                  
         B     ERRX                                                             
*                                                                               
ERR11    SR    RF,RF                                                            
         ICM   RF,7,5(R1)                                                       
         MVC   TSTMSG(60),=CL60'LOCKED BY '                                     
         MVC   TSTMSG+10(18),32(RF)                                             
         B     ERRX                                                             
*                                                                               
ERRX     ICM   R1,15,CURSOR                                                     
         BZ    *+8                                                              
         OI    6(R1),X'40'                                                      
*                                                                               
         OI    TSTMSGH+6,X'80'                                                  
         L     RD,SAVERD                                                        
         B     XMOD                                                             
*                                                                               
ADDINFO  MVC   TSTMSG(60),=CL60'ADDED REC=        '                             
         GOTO1 CHEXOUT,DMCB,DA,TSTMSG+10,4                                      
*                                                                               
         OI    TSTMSGH+6,X'80'                                                  
         L     RD,SAVERD                                                        
         B     XMOD                                                             
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS & LITERALS                               *                   
*************************************************************                   
         SPACE 1                                                                
SPACES   DC    80C' '                                                           
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
GETREC   DC    CL8'GETREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
PERDIR   DC    CL8'PERDIR'                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
PERFIL   DC    CL8'PERFIL'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
READ     DC    CL8'READ'                                                        
WRITE    DC    CL8'WRITE'                                                       
SEQ      DC    CL8'SEQ'                                                         
RANDOM   DC    CL8'RANDOM'                                                      
DMRSRV   DC    CL8'DMRSRV'                                                      
TEMPEST  DC    CL8'TEMPEST'                                                     
DMREAD   DC    CL8'DMREAD'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
INDEX    DC    CL8'INDEX'                                                       
WRKFIL   DC    CL8'WRKFIL'                                                      
GFILE    DC    CL8'GFILE'                                                       
EXTRA    DC    CL16'PERSON TEST ON  '                                           
         DC    XL16'00'                                                         
*                                                                               
WORKKEY  DC    X'0026',C'TEMP',X'24',C'T',X'0004'                               
*                                                                               
ACTNTBL  DC    CL7'ADD    ',C'A'                                                
         DC    CL7'DELETE ',C'D'                                                
         DC    CL7'CHANGE ',C'C'                                                
         DC    CL7'EXTEND ',C'E'                                                
         DC    CL7'RESET  ',C'R'                                                
         DC    CL7'LIST   ',C'L'                                                
         DC    CL7'REPORT ',C'T'                                                
         DC    CL7'XXXXXXX',C'X'                                                
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        DSECT TO COVER WORKING STORAGE                     *                   
*************************************************************                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
RELO     DS    A                                                                
DUB      DS    D                                                                
DUB2     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
HALF2    DS    H                                                                
BYTE     DS    X                                                                
FLAG1    DS    X                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
SAVERE   DS    A                                                                
CURSOR   DS    A                                                                
SENUM    DS    X                                                                
XCTL     DS    CL24                                                             
HEXWORK  DS    XL52                                                             
WORK1    DS    XL64                                                             
IOWORK   DS    12D                                                              
DA       DS    F                                                                
COMCNT   DS    H                                                                
CHR      DS    X                                                                
MODE     DS    C                   A/D/C/L ADD/DELETE/CHANGE/LIST               
OPTN     DS    C                                                                
ABND     DS    C                                                                
SCREEN   DS    C                                                                
ONLINE   DS    C                                                                
*                                                                               
ERROR    DS    A                                                                
TIME     DS    A                                                                
AIOAREA  DS    A                                                                
MYSSB    DS    A                                                                
DELAY    DS    F                                                                
LUID     DS    D                                                                
SYSNAME  DS    CL4                                                              
*                                                                               
USRID    DS    XL2                 EXTRACT FROM UTL                             
AGYID    DS    XL2                                                              
TRMID    DS    XL2                                                              
OFFCD    DS    X                                                                
*                                                                               
         DS    0F                                                               
SVPARMS  DS    0CL32                                                            
         DS    A                                                                
ATWA     DS    A                                                                
APERFACS DS    A                                                                
ATIA     DS    A                                                                
ACOMFACS DS    A                                                                
AEXTRA   DS    A                                                                
*                                                                               
ATICTOC  DS    A                                                                
AGETCUR  DS    A                                                                
ABUFF1   DS    A                                                                
ABUFF2   DS    A                                                                
*                                                                               
WRKF     DS    CL8                                                              
KEY      DS    CL64                                                             
KEY1     DS    CL64                                                             
KEY2     DS    CL64                                                             
KEY3     DS    CL64                                                             
KEY4     DS    CL64                                                             
*                                                                               
SPOOK1   DS    CL80                                                             
REQH     DS    CL160                                                            
*                                                                               
SAVP1    DS    CL10                                                             
*                                                                               
BUFF2    DS    CL14336                                                          
         ORG   BUFF2                                                            
IOAREA   DS    4096C                                                            
IOAREA2  DS    4096C                                                            
         ORG                                                                    
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        DSECT TO COVER TWA                                 *                   
*************************************************************                   
         SPACE 1                                                                
PETSTFFD DSECT                                                                  
         DS    CL64                                                             
CONHEADH EQU   *                                                                
       ++INCLUDE PETSTFFD                                                       
         ORG     TSTLOADH                                                       
       ++INCLUDE PETSTFED                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
************************************************************                    
*        SAVED STORAGE                                     *                    
************************************************************                    
         SPACE 1                                                                
SAVED    DSECT                                                                  
TESTBYTE DS    C                                                                
INSCREEN DS    X                                                                
FRSTTIME DS    X                                                                
         EJECT                                                                  
REQHDR   DSECT                                                                  
       ++INCLUDE DMREQHDR                                                       
REQUEST  DS    0CL80                                                            
         EJECT                                                                  
* DDCOMFACS                                                                     
* FADSECTS                                                                      
* FAFACTS                                                                       
* DDPARSNIPD                                                                    
* DDGLVXCTLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FASSB                                                          
       ++INCLUDE FATCB                                                          
       ++INCLUDE FAUTL                                                          
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALOCKUPD                                                      
       ++INCLUDE DDPARSNIPD                                                     
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE DMWRKFD                                                        
       ++INCLUDE DDSPOOK                                                        
         PRINT ON                                                               
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLPFMD                                                       
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001PETST00   09/22/05'                                      
         END                                                                    
