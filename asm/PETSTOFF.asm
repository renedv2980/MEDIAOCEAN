*          DATA SET PETSTOFF   AT LEVEL 234 AS OF 05/04/05                      
*PHASE PETSTOFA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLPE                                                                
*INCLUDE FATABOFF                                                               
*                                                                               
         TITLE 'TEST PERSON SYSTEM OFFLINE UPDATES '                            
PETSTOFF CSECT                                                                  
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**PETS**,RA,WORK=A(WORKC),CLEAR=YES                  
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
*                                                                               
         L     R1,=A(IOAREA1-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO1                                                          
*                                                                               
MAIN     BAS   RE,PRINTI           INIT PRINTING                                
         BAS   RE,INIT             OPEN FILES ECT                               
*                                                                               
MAIN001  CLI   MODE,C'C'           MODE=CHANGE                                  
         BE    MAIN010                                                          
         CLI   MODE,C'E'           MODE=EXTEND                                  
         BE    MAIN010                                                          
         CLI   MODE,C'R'           MODE=RESET                                   
         BE    MAIN010                                                          
         CLI   MODE,C'D'           MODE=DELETE                                  
         BE    MAIN010                                                          
         CLI   MODE,C'A'           MODE=ADD                                     
         BE    MAIN020                                                          
*                                                                               
MAIN010  BAS   RE,CHANGE           MAIN CHANGE PROGRAM                          
         B     MAIN990                                                          
*                                                                               
MAIN020  BAS   RE,ADD              MAIN ADD PROGRAM                             
         B     MAIN990                                                          
*                                                                               
MAIN990  BAS   RE,CLOSE            CLOSE FILES ECT                              
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
INIT010  LA    R3,IOAREA                                                        
         GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    INIT020                                                          
         LR    R1,R3                                                            
         BAS   RE,VALCARD                                                       
         B     INIT010                                                          
*                                                                               
INIT020  L     R1,=V(SSB)                                                       
         OI    SSOFLAG1-SSOOFF(R1),SSOFXCPY   TURN OFF SSB COPY                 
         OI    SSOFLAG1-SSOOFF(R1),SSOFRCVR   TURN ON RECOVERY                  
         OI    SSOFLAG1-SSOOFF(R1),SSOFGLOB   TURN ON GLOBAL                    
*                                                                               
         CLI   LOCKER,C'Y'         ARE WE USING LOCKER                          
         BNE   *+8                                                              
         OI    SSOSTAT2-SSOOFF(R1),SSOSLOCK+SSOSROLC                            
*                                                                               
         L     RF,=V(DDSIO)        SET UP DDSIO                                 
         MVC   0(8,RF),DDSIO                                                    
*                                                                               
INIT030  L     R1,=V(UTL)          OPEN PER1 FILES                              
         MVI   4(R1),X'0E'                                                      
         GOTO1 =V(DATAMGR),DMCB,DMOPEN,PERSON,PFILES,IOAREA                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     INIT990                                                          
*                                                                               
INIT040  L     R1,=V(UTL)          OPEN MEDZ FILES                              
         MVI   4(R1),X'14'                                                      
         GOTO1 =V(DATAMGR),DMCB,DMOPEN,MEDIA,MFILES,IOAREA                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INIT050  L     R1,=V(UTL)          PER1 FILES                                   
         MVI   4(R1),X'0E'                                                      
*                                                                               
INIT990  B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        CLOSE FILES                                        *                   
*************************************************************                   
         SPACE 1                                                                
CLOSE    NTR1                                                                   
         GOTO1 =V(DATAMGR),DMCB,DMCLSE,PERSON,0,IOAREA                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                ERRORS ARE DEADLY                            
         B     EXITY                                                            
*                                                                               
         L     R1,=V(UTL)          SWITCH TO MEDZ                               
         MVI   4(R1),X'14'                                                      
         GOTO1 =V(DATAMGR),DMCB,DMCLSE,MEDIA,0,IOAREA                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                ERRORS ARE DEADLY                            
*                                                                               
         B     EXITY                                                            
         EJECT                                                                  
*************************************************************                   
*        ADD                                                *                   
*************************************************************                   
         SPACE 1                                                                
ADD      NTR1                                                                   
*                                                                               
         XC    IOAREA(255),IOAREA                                               
         MVC   IOAREA+0(40),START1                                              
         MVC   IOAREA+36(2),=X'063F'                                            
         MVC   IOAREA+44(2),=X'0112'                                            
         MVC   IOAREA+46(16),=C'01JAN00 ADD OFFL'                               
*                                                                               
ADD010   GOTO1 =V(DATAMGR),DMCB,ADDREC,PERFIL,DA,IOAREA,IOWORK                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1               NEXT KEY                                     
         ICM   R1,15,IOAREA+32                                                  
         LA    R1,1(R1)                                                         
         STCM  R1,15,IOAREA+32                                                  
         CLC   IOAREA(36),END1                                                  
         BNL   ADD020                                                           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,WAIT1          WAIT FOR TIME IN WAIT1                       
         BAS   RE,WAIT                                                          
         B     ADD010                                                           
*                                                                               
ADD020   SR    R1,R1                                                            
         ICM   R1,3,WAIT2          WAIT FOR TIME IN WAIT2                       
         BAS   RE,WAIT                                                          
*                                                                               
         CLI   ABEND1,C'Y'                                                      
         BNE   ADDX                                                             
         ABEND 666                                                              
*                                                                               
ADDX     B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        CHANGE                                             *                   
*************************************************************                   
         SPACE 1                                                                
CHANGE   NTR1                                                                   
         SPACE 1                                                                
*************************************************************                   
*        LOCKUP STUFF                                       *                   
*************************************************************                   
         SPACE 1                                                                
CHNG000  B     CHNG005             SKIP LOCKUP CALLS                            
         XC    WORK1,WORK1         LOCKUP LOCK 4CHR ID                          
         MVC   WORK1+3(2),=X'0001'                                              
         MVC   WORK1+5(4),START1                                                
         XC    DMCB,DMCB                                                        
         GOTO1 =V(DATAMGR),DMCB,=C'LOCKUP',(C'L',WORK1),0,(C'Y',EXTRA)          
         CLI   8(R1),0                                                          
         BE    CHNG005                                                          
                                                                                
         CLI   8(R1),1             ALREADY LOCKED                               
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,1                WAIT FOR 1 SECOND                            
         BAS   RE,WAIT                                                          
         B     CHNG000                                                          
         SPACE 1                                                                
************************************************************                    
*        READ HI FOR KEY IN START1                         *                    
************************************************************                    
         SPACE 1                                                                
CHNG005  MVC   KEY,START1          RD HI FOR FIRST                              
         GOTO1 =V(DATAMGR),DMCB,(X'88',DMRDHI),PERDIR,KEY,KEY                   
         CLI   DMCB+8,2                                                         
         BE    *+14                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHNG010  CLC   KEY,END1            TEST WITH ENDKEY                             
         BNL   CHNG050                                                          
         MVC   DA,KEY+38           GET DISK ADDRESS                             
         SPACE 1                                                                
************************************************************                    
*        GET RECORD FOR UPDATE                             *                    
************************************************************                    
         SPACE 1                                                                
         GOTO1 =V(DATAMGR),DMCB,(X'88',GETREC),PERFIL,DA,IOAREA,IOWORK          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,WAIT1          WAIT FOR TIME IN WAIT1                       
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
         GOTO1 =V(DATAMGR),DMCB,PUTREC,PERFIL,DA,IOAREA,IOWORK                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHNG020  CLI   MODE,C'D'           ARE WE DELETING                              
         BNE   CHNG020X                                                         
         GOTO1 =V(DATAMGR),DMCB,DMWRT,PERDIR,KEY,KEY                            
         BE    *+6                                                              
         DC    H'0'                                                             
CHNG020X EQU   *                                                                
         SPACE 1                                                                
************************************************************                    
*        READ SEQ FOR NEXT                                 *                    
************************************************************                    
         SPACE 1                                                                
         GOTO1 =V(DATAMGR),DMCB,(X'88',DMRSEQ),PERDIR,KEY,KEY                   
         CLI   DMCB+8,2                                                         
         BE    *+14                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     CHNG010                                                          
         SPACE 1                                                                
************************************************************                    
*        WAIT FOR TIME IN WAIT2                            *                    
************************************************************                    
         SPACE 1                                                                
CHNG050  SR    R1,R1                                                            
         ICM   R1,3,WAIT2          WAIT FOR TIME IN WAIT2                       
         BAS   RE,WAIT                                                          
*                                                                               
         CLI   ABEND1,C'Y'                                                      
         BNE   ABENDNO1                                                         
         ABEND 666                                                              
*                                                                               
ABENDNO1 B     CHNG070                                                          
*                                                                               
         L     R1,=V(UTL)          SWITCH TO MEDZ                               
         MVI   4(R1),X'14'                                                      
         MVC   DA,=X'00010101'                                                  
         GOTO1 =V(DATAMGR),DMCB,(X'88',GETREC),MEDFIL,DA,IOAREA,IOWORK          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=V(UTL)          PER1 FILES                                   
         MVI   4(R1),X'0E'                                                      
         B     CHNG070             SKIP LOCKUP                                  
         SPACE 1                                                                
*************************************************************                   
*        LOCKUP STUFF                                       *                   
*************************************************************                   
         SPACE 1                                                                
         XC    WORK1,WORK1         LOCKUP LOCK 4CHR ID                          
         MVC   WORK1+3(2),=X'0001'                                              
         MVC   WORK1+5(4),START1                                                
         XC    DMCB,DMCB                                                        
         GOTO1 =V(DATAMGR),DMCB,=C'LOCKUP',(C'U',WORK1),0,(C'Y',EXTRA)          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHNG060  XC    WORK1,WORK1         LOCKUP LOCK 4CHR ID                          
         MVC   WORK1+3(2),=X'0001'                                              
         MVC   WORK1+5(4),START2                                                
         XC    DMCB,DMCB                                                        
         GOTO1 =V(DATAMGR),DMCB,=C'LOCKUP',(C'L',WORK1),0,(C'Y',EXTRA)          
         CLI   8(R1),0                                                          
         BE    CHNG070                                                          
*                                                                               
         CLI   8(R1),1                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHNG060X LA    R1,1                WAIT FOR 1 SECOND                            
         BAS   RE,WAIT                                                          
         B     CHNG060                                                          
         SPACE 1                                                                
************************************************************                    
*        READ HI FOR KEY IN START2                         *                    
************************************************************                    
         SPACE 1                                                                
CHNG070  MVC   KEY,START2                                                       
         GOTO1 =V(DATAMGR),DMCB,(X'88',DMRDHI),PERDIR,KEY,KEY                   
         CLI   DMCB+8,2                                                         
         BE    *+14                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DA,KEY+38                                                        
*                                                                               
CHNG080  CLC   KEY,END2            TEST WITH ENDKEY                             
         BNL   CHNG150                                                          
         SPACE 1                                                                
************************************************************                    
*        GET RECORD FOR UPDATE                             *                    
************************************************************                    
         SPACE 1                                                                
         GOTO1 =V(DATAMGR),DMCB,(X'88',GETREC),PERFIL,DA,IOAREA,IOWORK          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,WAIT3          WAIT FOR TIME IN WAIT3                       
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
         GOTO1 =V(DATAMGR),DMCB,PUTREC,PERFIL,DA,IOAREA,IOWORK                  
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
         GOTO1 =V(DATAMGR),DMCB,(X'88',DMRSEQ),PERDIR,KEY,KEY                   
         CLI   DMCB+8,2                                                         
         BE    *+14                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     CHNG080                                                          
         SPACE 1                                                                
************************************************************                    
*        WAIT FOR TIME IN WAIT4                            *                    
************************************************************                    
         SPACE 1                                             V                  
CHNG150  SR    R1,R1                                                            
         ICM   R1,3,WAIT4          WAIT FOR TIME IN WAIT4                       
         BAS   RE,WAIT                                                          
*                                                                               
         CLI   ABEND2,C'Y'                                                      
         BNE   ABENDNO2                                                         
         ABEND 666                                                              
*                                                                               
ABENDNO2 B     CHNGX               SKIP LOCKUP STUFF                            
         SPACE 1                                                                
*************************************************************                   
*        LOCKUP STUFF                                       *                   
*************************************************************                   
         SPACE 1                                                                
         XC    WORK1,WORK1         LOCKUP LOCK 4CHR ID                          
         MVC   WORK1+3(2),=X'0001'                                              
         MVC   WORK1+5(4),START2                                                
         XC    DMCB,DMCB                                                        
         GOTO1 =V(DATAMGR),DMCB,=C'LOCKUP',(C'U',WORK1),0,(C'Y',EXTRA)          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
*************************************************************                   
*        FINALLY EXIT                                       *                   
*************************************************************                   
         SPACE 1                                                                
CHNGX    B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        UPDATE ROUTINE                                     *                   
*************************************************************                   
         SPACE 1                                                                
UPDATE   NTR1                                                                   
         GOTO1 =V(DATCON),DMCB,(5,0),(8,WORK)                                   
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
UPDATE05 CLI   MODE,C'R'           RESET                                        
         BE    UPDATE90            WRITE BACK WITH DATE/TIME ONLY               
*                                                                               
         CLI   0(R1),0                                                          
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
*                                                                 1             
UPDATE90 LA    RF,IOAREA           SET LENGTH THEN EXIT                         
         MVI   0(R1),0             MAKE SURE WE HAVE A ZERO                     
         LA    R1,1(R1)                                                         
         SR    R1,RF                                                            
         STCM  R1,3,IOAREA+36                                                   
*                                                                               
UPDATEX  B     EXIT                                                             
*************************************************************                   
*        PARAMETER CARD CONSTANTS                           *                   
*************************************************************                   
         SPACE 1                                                                
START1   DC     XL40'00'                                                        
WAIT1    DC     XL2'00'                                                         
END1     DC     XL40'00'                                                        
WAIT2    DC     XL2'00'                                                         
ABEND1   DC     C'N'                                                            
START2   DC     XL40'00'                                                        
WAIT3    DC     XL2'00'                                                         
END2     DC     XL40'00'                                                        
WAIT4    DC     XL2'00'                                                         
ABEND2   DC     C'N'                                                            
FACPAK   DC     C'X'                                                            
LOCKER   DC     C'N'                                                            
MODE     DC     C'CHANGE'                                                       
DDSIO    DC     C'DDSIO   '                                                     
         EJECT                                                                  
************************************************************                    
*        VALIDATE PARAMETER CARDS                          *                    
************************************************************                    
*                                                                               
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN-1),X'FLAGS',AL3(OUTPUT)           
*                                                                               
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
*                                                                               
         SPACE 1                                                                
CARDTAB  DS    0F                                                               
         DC    C'START1 ',AL1(5,0),X'C000',AL3(VALK1)                           
         DC    C'START2 ',AL1(5,0),X'C000',AL3(VALK2)                           
         DC    C'END1   ',AL1(3,0),X'C000',AL3(VALK3)                           
         DC    C'END2   ',AL1(3,0),X'C000',AL3(VALK4)                           
         DC    C'ABEND1 ',AL1(5,0),X'0000',AL3(ABEND1)                          
         DC    C'ABEND2 ',AL1(5,0),X'0000',AL3(ABEND2)                          
         DC    C'WAIT1  ',AL1(4,3),X'0800',AL3(WAIT1)                           
         DC    C'WAIT2  ',AL1(4,3),X'0800',AL3(WAIT2)                           
         DC    C'WAIT3  ',AL1(4,3),X'0800',AL3(WAIT3)                           
         DC    C'WAIT4  ',AL1(4,3),X'0800',AL3(WAIT4)                           
         DC    C'FACPAK ',AL1(5,1),X'0000',AL3(FACPAK)                          
         DC    C'LOCKER ',AL1(5,1),X'0000',AL3(LOCKER)                          
         DC    C'MODE   ',AL1(3,6),X'0000',AL3(MODE)                            
         DC    C'DDSIO  ',AL1(4,8),X'0000',AL3(DDSIO)                           
         DC    X'0000'          EXTEND                                          
         SPACE 1                                                                
VALCARD  NTR1                                                                   
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITEQU                                                          
*                                                                               
VALC001  LA    R4,CARDTAB                                                       
         ST    R2,CARDR2                                                        
VALC010  SR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,7(R4)                                                         
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         B     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         BE    VALC020                                                          
         LA    R4,14(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         BNE   VALC010                                                          
         B     CERRKEY             ERROR INVALID KEYWORD                        
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
*                                                                               
         LA    RF,VALCDELS         DELIMITER TABLE                              
         B     *+8                                                              
VALC021  LA    RF,5(RF)                                                         
         CLI   0(RF),0                                                          
         BE    CERRDEL             END OF TABLE INVALID DELIMITER               
*                                                                               
         MVC   BYTE,4(RF)          AUTH BIT MUST BE ON                          
         CLI   BYTE,0              EXCEPT WHEN ZERO                             
         BE    *+14                                                             
         NC    BYTE,9(R4)                                                       
         BZ    VALC021                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,2(RF)            GET EX LEN                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(RF)       TEST DELIMITERS                              
         BNE   VALC021                                                          
*                                                                               
         MVC   BYTE,3(RF)          SAVE COMPARE CHR                             
         LA    R2,1(R1,R2)                                                      
         B     VALC025                                                          
*                                                                               
VALCDELS DC    C'= ',AL1(0),X'80',X'00'                                         
         DC    C'>=',AL1(1),X'B0',X'20'                                         
         DC    C'<=',AL1(1),X'D0',X'20'                                         
         DC    C'/=',AL1(1),X'70',X'40'                                         
         DC    C'< ',AL1(0),X'40',X'20'                                         
         DC    C'> ',AL1(0),X'20',X'20'                                         
         DC    X'00'                                                            
*                                                                               
VALC025  LR    R1,R2               GET LEN FOR MOVE                             
VALC026  CLI   0(R1),C','                                                       
         BE    VALC030                                                          
         CLI   0(R1),C' '                                                       
         BE    VALC030                                                          
         CLI   0(R1),0                                                          
         BE    VALC030                                                          
         LA    R1,1(R1)                                                         
         B     VALC026                                                          
*                                                                               
VALC030  SR    R1,R2                                                            
*                                                                               
VALC031  BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,11(R4)         GET ADDRESS FOR MOVE                         
*                                                                               
         TM    9(R4),X'80'         IF ROUTINE                                   
         BZ    *+10                                                             
         BASR  RE,RF               GOTO ROUTINE                                 
         B     VALC090                                                          
*                                                                               
         TM    9(R4),X'04'         IF LIST                                      
         BNO   VALC050                                                          
VALC040  CLI   0(RF),X'FF'         CHECK NOT FULL                               
         BE    CERRMAN                                                          
         CLI   0(RF),0             EMPTY ENTRY                                  
         BE    VALC050                                                          
         SR    R0,R0                                                            
         IC    R0,8(R4)                                                         
         AR    RF,R0                                                            
         TM    9(R4),X'60'         /<=>                                         
         BZ    VALC040                                                          
         LA    RF,1(RF)            ONE MORE FOR CODE                            
         B     VALC040                                                          
*                                                                               
VALC050  TM    9(R4),X'60'         IF /<=>                                      
         BZ    *+14                                                             
         MVC   0(1,RF),BYTE        SAVE COMP CODE                               
         LA    RF,1(RF)                                                         
*                                                                               
         TM    9(R4),X'10'         HEX INPUT                                    
         BNO   VALC060                                                          
         LA    R0,1(R1)            SET R0 HEX INPUT LEN                         
         GOTO1 =V(HEXIN),DMCB,(R2),(RF),(R0)                                    
         ICM   R1,15,12(R1)                                                     
         BZ    CERRHEX                                                          
         B     VALC090                                                          
*                                                                               
VALC060  TM    9(R4),X'08'         DEC INPUT                                    
         BZ    VALC070                                                          
         LR    R4,R2                                                            
         LA    R3,1(R1)                                                         
         BAS   RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         BE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         STH   R1,0(RF)            SAVE HALFWORD (DEFAULT)                      
         B     VALC090                                                          
*                                                                               
VALC070  TM    9(R4),X'02'         TIME INPUT                                   
         BZ    VALC080                                                          
         BAS   RE,VALTIME                                                       
         MVC   0(4,RF),FULL                                                     
         B     VALC090                                                          
*                                                                               
VALC080  CLI   8(R4),0             DONT CARE                                    
         BE    *+12                                                             
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         BNL   CERRMAX                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC090  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         BE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,CARDEND          TEST FOR END OF CARD                         
         BL    VALC090                                                          
*                                                                               
EXITEQU  CR    RB,RB               SET CC EQU                                   
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
CERRSYS  LA    R1,=C'INVALID SYSTEM  '                                          
         B     CERRX                                                            
CERRPRG  LA    R1,=C'INVALID PROGRAM '                                          
         B     CERRX                                                            
CERRSES  LA    R1,=C'INVALID SESYS   '                                          
         B     CERRX                                                            
CERRTIM  LA    R1,=C'INVALID TIME    '                                          
         B     CERRX                                                            
CERRDEC  LA    R1,=C'MUST BE HEX     '                                          
         B     CERRX                                                            
CERRHEX  LA    R1,=C'MUST BE DECIMAL '                                          
         B     CERRX                                                            
CERRKEY  LA    R1,=C'INVALID KEYWORD '                                          
         B     CERRX                                                            
CERRDEL  LA    R1,=C'INVALID DELIMITR'                                          
         B     CERRX                                                            
CERRMAX  LA    R1,=C'VALUE TOO LONG  '                                          
         B     CERRX                                                            
CERRMAN  LA    R1,=C'TOO MANY FILTERS'                                          
         B     CERRX                                                            
*                                                                               
CERRX    L     RD,CARDRD                                                        
         L     R2,CARDR2                                                        
         LA    RF,PLINE+1                                                       
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
EXITNEQ  LTR   RB,RB               SET CC NEQ                                   
         B     EXIT                                                             
         EJECT                                                                  
************************************************************                    
*        VALIDATE KEY FIELDS                               *                    
************************************************************                    
*                                                                               
VALK1    LA    RF,START1                                                        
         B     VALKEY                                                           
VALK2    LA    RF,START2                                                        
         B     VALKEY                                                           
VALK3    LA    RF,END1                                                          
         B     VALKEY                                                           
VALK4    LA    RF,END2                                                          
         B     VALKEY                                                           
*                                                                               
VALKEY   NTR1                                                                   
*                                                                               
         XC    WORK,WORK           START WITH ZERO KEY                          
         MVC   WORK(4),0(R2)       KEY IS RMOR/NNNN                             
         CLI   4(R2),C'/'                                                       
         BNE   CERRKEY                                                          
         SH    R1,=H'4'            SUB5 FOR ID AND /                            
         LR    R3,R1                                                            
         LA    R4,5(R2)                                                         
         BAS   RE,VALNUM           VALIDATE NUMERIC PART                        
         CLI   DUB,X'FF'                                                        
         BE    CERRKEY                                                          
         CVB   R1,DUB                                                           
         ST    R1,WORK+32          SET NUMERIC PART OF KEY                      
         MVC   0(40,RF),WORK                                                    
*                                                                               
VALKEYX  B     EXIT                                                             
*                                                                               
       ++INCLUDE DDVALNUM                                                       
*************************************************************                   
*        GET TIME FROM 0(R2) (R1)=EX LEN  TIME=HH:MM:SS.TU  *                   
*************************************************************                   
         SPACE 1                                                                
VALTIME  NTR1                                                                   
         MVC   HALF,=C'00'         FIRST MAY BE 1:00 OR 02:00                   
         CLI   1(R2),C':'                                                       
         BNE   VALT010                                                          
*                                                                               
         MVC   HALF+1(1),0(R2)     ASSUME 1:00                                  
         LA    R2,2(R2)                                                         
         B     VALT020                                                          
*                                                                               
VALT010  MVC   HALF+0(2),0(R2)     ASSUME 02:00                                 
         LA    R2,3(R2)                                                         
*                                                                               
VALT020  LA    R3,2                PREPARE FULL AND HALD                        
         LA    R4,HALF                                                          
         XC    FULL,FULL                                                        
*                                                                               
         BAS   RE,VALNUM           VALIDATE HOURS                               
         L     RF,=A(60*60*100)                                                 
         BAS   RE,VALTADD                                                       
*                                                                               
         MVC   HALF,0(R2)          VALIDATE MINUTES                             
         BAS   RE,VALNUM                                                        
         L     RF,=A(60*100)                                                    
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C':'          TEST FOR SECS                                
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE SECS                                
         L     RF,=F'100'                                                       
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C'.'          TEST FOR TUS                                 
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE TUS                                 
         LA    RF,1                                                             
         BAS   RE,VALTADD                                                       
         B     EXITEQU                                                          
*                                                                               
VALTADD  CLI   DUB,X'FF'           TEST FOR INVALID NUMERIC                     
         BE    CERRTIM                                                          
         SR    R0,R0               CONVERT AND MULTIPLY BY RF                   
         CVB   R1,DUB                                                           
         MR    R0,RF                                                            
         A     R1,FULL                                                          
         ST    R1,FULL             ADD TO FULL                                  
         BR    RE                                                               
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
         EJECT                                                                  
*************************************************************                   
*        WAIT ROUTINE                                       *                   
*************************************************************                   
         SPACE 1                                                                
WAIT     ST    RE,SAVERE                                                        
         SR    R0,R0                                                            
         M     R0,=F'38400'        * 38400 FOR TUS                              
         ST    R1,FULL                                                          
         STIMER WAIT,TUINTVL=FULL  WAIT 10 SECS THEN TRY AGAIN                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        PRINT ROUTINES                                     *                   
*************************************************************                   
         SPACE 1                                                                
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
*        PRINT TITLE                                        *                   
*************************************************************                   
         SPACE 1                                                                
TITLE    DC    CL166' '                                                         
         ORG   TITLE                                                            
         DC    C'1',X'40'                                                       
         DC    C'                                                  '            
         DC    C'                                                  '            
         DC    C'                    '                                          
         ORG                                                                    
TITLE1   DC    CL166' '                                                         
         ORG   TITLE1                                                           
         DC    C'1',X'40'                                                       
         DC    C'-----------------------------------------------------'         
         DC    C'------------------ PARAMETER CARDS ------------------'         
         DC    C'-----------------------------------------------------'         
         ORG                                                                    
         EJECT                                                                  
*************************************************************                   
*        DCBS                                               *                   
*************************************************************                   
*                                                                               
*        LRECL=(166) USE CHARS=(BX15)                                           
*                                                                               
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS & LTORG                                  *                   
*************************************************************                   
         SPACE 1                                                                
MAXLINE  DC    P'60'                                                            
SPACES   DC    CL166' '                                                         
         SPACE 1                                                                
DMOPEN   DC    CL8'DMOPEN'                                                      
DMCLSE   DC    CL8'DMCLSE'                                                      
DMUNLK   DC    CL8'DMUNLK'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
ADDREC   DC    CL8'ADDREC'                                                      
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
*                                                                               
PERSON   DC    CL8'PERSON'                                                      
MEDIA    DC    CL8'MEDIA '                                                      
PERDIR   DC    CL8'PERDIR'                                                      
PERFIL   DC    CL8'PERFIL'                                                      
MEDFIL   DC    CL8'MEDFIL'                                                      
*                                                                               
PFILES   DC    CL8'UPERDIR '                                                    
         DC    CL8'UPERFIL '                                                    
         DC    CL8'UPERREQ '                                                    
         DC    CL8'UPERRCV '                                                    
         DC    CL8'X       '                                                    
*                                                                               
MFILES   DC    CL8'UMEDDIR '                                                    
         DC    CL8'UMEDFIL '                                                    
         DC    CL8'UMEDREQ '                                                    
         DC    CL8'UMEDRCV '                                                    
         DC    CL8'X       '                                                    
*                                                                               
WAITTIM  DC    C'0060'                                                          
RECSEQ   DC    C'ABCDE'                                                         
EXTRA    DC    CL16'OFFLINE TEST JOB'                                           
         DC    XL16'00'                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SSB      CSECT                                                                  
         DC    256X'00'                                                         
         ORG   SSB                                                              
         DC    XL2'00',X'FF',X'00'    RECOVERY                                  
         DC    5XL4'00000000'                                                   
         DC    A(0)                                                             
         DC    A(0)                                                             
         ORG   SSB+45                                                           
         DC    C'T'                   NO DSPACE "GET FROM PERFIL"               
         ORG   SSB+52                                                           
         DC    A(SSB)                                                           
         ORG                                                                    
*                                                                               
WORKC    CSECT                                                                  
         DS    (64*1024)X             WORKING STORAGE POOL                      
         EJECT                                                                  
*************************************************************                   
*        DSECT TO COVER WORKING STORAGE                     *                   
*************************************************************                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
CARDEND  DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAG1    DS    X                                                                
*                                                                               
DMCB     DS    6F                                                               
WORK     DS    CL255                                                            
WORK1    DS    XL64                                                             
IOW      DS    12D                                                              
DA       DS    F                                                                
*                                                                               
AIO1     DS    A                                                                
*                                                                               
KEY      DS    CL40                                                             
         DS    CL24                                                             
*                                                                               
LINE     DS    PL2                                                              
PAGE     DS    PL4                                                              
PLINE    DS    CL166                                                            
*                                                                               
IOWORK   DS    12D                                                              
*                                                                               
BUFF2    DS    CL14336                                                          
         ORG   BUFF2                                                            
IOAREA   DS    4096C                                                            
IOAREA1  DS    4096C                                                            
         ORG                                                                    
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
************************************************************                    
*        OTHER DSECTS                                      *                    
************************************************************                    
         SPACE 1                                                                
*FASSBOFF                                                                       
       ++INCLUDE FASSBOFF                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'234PETSTOFF  05/04/05'                                      
         END                                                                    
