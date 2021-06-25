*          DATA SET SRLOM00    AT LEVEL 004 AS OF 05/01/02                      
*PHASE T15F00A                                                                  
         TITLE '$LOMAIN - DISPLAY / LOCK / UNLOCK ACC LEDGERS'                  
         PRINT NOGEN                                                            
LOMAIN   CSECT                                                                  
         NMOD1 LOMWRKX-LOMWRK,**$LOM**,RR=RE,CLEAR=YES                          
         ST    RE,RELO                                                          
         USING LOMWRK,RC           RC=A(W/S)                                    
         MVC   SRPARS,0(R1)                                                     
         L     RA,SRPAR6                                                        
         USING T15FFFD,RA          RA=A(TWA)                                    
         L     R9,SRPAR4                                                        
         USING COMFACSD,R9         R9=A(COMFACS)                                
         USING LDGTABD,R4                                                       
*                                                                               
         GOTO1 CCALLOV,DMCB,0,X'D9000A66'                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VSETLOCK,0(R1)      A(ACSETLOCK)                                 
*                                                                               
         L     RF,SRPAR1           RF=A(SYSFACS)                                
         MVC   ASELIST,VSELIST-SYSFACD(RF)                                      
         L     RF,VSSB-SYSFACD(RF) EXTRACT CONNECTED SYSTEM                     
         L     RF,SSBTKADR-SSBD(RF)                                             
         MVC   SAVESYS,TCBSYS-TCBD(RF)                                          
         EJECT                                                                  
         GOTO1 VSETLOCK,DMCB,('PARMATAB',0),(R9)                                
         MVC   LOCKSTAT,DMCB+4     SAVE LOCK TABLE STATUS                       
*                                                                               
         L     R3,DMCB             A(LEDGER LOCKOUT TABLE)                      
         LA    R4,LOMTAB           MY LEDGER TABLE                              
         SR    R5,R5               COUNT NUMBER OF ENTRIES                      
COPYLOOP CLI   0(R3),0             IS THIS ENTRY BEING USED?                    
         BE    *+18                                                             
         MVC   0(LDGTABL,R4),0(R3) YES - COPY ENTRY INTO MY TABLE               
         LA    R4,LDGTABL(R4)                                                   
         LA    R5,1(R5)            INCREMENT NUMBER OF ENTRIES                  
         LA    R3,LDGTABL(R3)                                                   
         CLI   0(R3),LDGTEOTQ      END OF TABLE?                                
         BNE   COPYLOOP            NO                                           
         MVI   0(R4),LDGTEOTQ      MARK END OF MY TABLE                         
         ST    R5,NUMENTRY         SAVE NUMBER OF ENTRIES                       
*                                                                               
         LA    R2,SRVP1H                                                        
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   8(R2),C'D'          DEFAULT ACTION IS DISPLAY                    
         MVI   5(R2),1                                                          
*                                                                               
         LA    RF,ACTTAB           FIND MATCH ON ACTION                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
ACTION   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(RF)                                                    
         BNE   *+14                                                             
         L     RF,8(RF)                                                         
         A     RF,RELO                                                          
         BR    RF                  BRANCH TO APPROPRIATE ROUTINE                
         LA    RF,L'ACTTAB(RF)                                                  
         CLI   0(RF),X'FF'                                                      
         BNE   ACTION                                                           
*                                                                               
         MVC   SRVMSG(26),=C'** ERROR ** INVALID ACTION'                        
         OI    SRVP1H+6,X'40'      SET CURSOR                                   
         B     BYE                                                              
         EJECT                                                                  
DISPLAY  CLI   SRVP2H+5,0          ANY DISPLAY FILTER?                          
         BE    VALP3                                                            
         CLI   SRVP2H+5,1          YES                                          
         BNE   *+20                IT'S AN SENAME                               
         CLI   SRVP2,C'L'                                                       
         BNE   *+12                                                             
         MVI   LOCKFILT,C'Y'       DISPLAY LOCKED ENTRIES ONLY                  
         B     VALP3                                                            
         BAS   RE,VALSENAM         YES -- VALIDATE SENAME                       
         BNE   INVALID                                                          
*                                                                               
VALP3    LA    R2,SRVP3H           IF ERROR, SET CURSOR ON P3                   
         CLI   5(R2),0                                                          
         BE    VALP4                                                            
         CLI   5(R2),1             SORT TABLE ON KEY?                           
         BNE   INVALID                                                          
         CLI   8(R2),C'K'                                                       
         BNE   INVALID                                                          
         GOTO1 CXSORT,DMCB,LOMTAB,NUMENTRY,LDGTABL,L'LDGTKEY,0                  
*                                                                               
VALP4    LA    R4,LOMTAB           MY LEDGER TABLE                              
         LA    R3,1                ASSUME WE DISPLAY STARTING AT ONE            
         LA    R2,SRVP4H                                                        
         CLI   5(R2),0             ANYTHING IN 'START AT' FIELD?                
         BE    DISPIT                                                           
         TM    4(R2),X'08'         YES -- IS IT NUMERIC?                        
         BZ    INVALID                                                          
         ZIC   R1,5(R2)            YES                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SRVP4(0)                                                     
         CVB   R1,DUB                                                           
         LR    R3,R1                                                            
         SH    R1,=H'1'                                                         
         BM    INVALID             THEY ENTERED ZERO                            
         BZ    DISPIT              THEY ENTERED ONE -- START OVER               
         LA    R4,L'LOMTAB(R4)                                                  
         CLI   0(R4),LDGTEOTQ                                                   
         BE    INVALID             END OF TABLE                                 
         BCT   R1,*-12             BUMP TO REQUESTED ENTRY                      
*                                                                               
DISPIT   LA    R2,SRVL1H           COLUMN 1                                     
         BAS   RE,FORMAT                                                        
         LA    R2,SRVL2H           COLUMN 2                                     
         BAS   RE,FORMAT                                                        
*                                                                               
         EDIT  (R3),(5,SRVP4),ALIGN=LEFT                                        
*                                                                               
         OI    SRVTABH+6,X'40'     SET CURSOR                                   
         MVC   SRVMSG(14),=C'DATA DISPLAYED'                                    
         B     BYE                                                              
         EJECT                                                                  
FORMAT   NTR1                                                                   
*                                                                               
         USING LSTD,R2                                                          
FORMAT10 CLI   0(R4),LDGTEOTQ      END OF TABLE?                                
         BNE   *+10                                                             
         SR    R3,R3               YES -- REDISPLAY FROM BEGINNING              
         B     FORMATX                                                          
*                                                                               
         CLI   SENUM,0             ANY SENAME FILTER?                           
         BE    *+14                                                             
         CLC   SENUM,LDGTSEN       YES -- DOES IT MATCH?                        
         BNE   FORMAT20                                                         
         L     R5,ASELIST          YES -- LOOK UP NAME                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
         CLC   LDGTSEN,SESYS       FIND SENAME                                  
         BE    *+10                                                             
         BXLE  R5,R6,*-10                                                       
         DC    H'0'                INVALID SENUM IN TABLE                       
         MVC   LSTSENAM,SENAME                                                  
         DROP  R5                                                               
*                                                                               
         GOTO1 CHEXOUT,DMCB,LDGTCPY,LSTCC,1,=C'TOG'                             
         CLC   =F'2',DMCB+16       COMPANY CODE                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   LSTUL(1),LDGTUNT    UNIT CODE                                    
*                                                                               
         MVC   LSTUL+1(1),LDGTLDG  LEDGER CODE                                  
*                                                                               
         UNPK  DUB(6),LDGTTIME     TIME LAST LOCKED                             
         MVC   LSTTIME(2),DUB                                                   
         MVI   LSTTIME+2,C':'                                                   
         MVC   LSTTIME+3(2),DUB+2                                               
         MVI   LSTTIME+5,C':'                                                   
         MVC   LSTTIME+6(2),DUB+4                                               
         OI    LSTTIME+7,X'F0'                                                  
         MVC   LSTLUID,LDGTLUID    LUID                                         
*                                                                               
         MVI   LSTLOCK,C' '        ASSUME UNLOCKED                              
         CLI   LOCKFILT,C'Y'       DISPLAY LOCKED ONLY?                         
         BNE   *+12                                                             
         TM    LDGTSTAT,LDGTSLOK   YES                                          
         BZ    FORMAT20                                                         
         TM    LDGTSTAT,LDGTSLOK                                                
         BZ    *+8                                                              
         MVI   LSTLOCK,C'*'        LOCKED                                       
*                                                                               
         LA    R2,(2*L'LSTLEN)(R2) BUMP TO NEXT ROW                             
*                                                                               
FORMAT20 LA    R4,L'LOMTAB(R4)     BUMP TO NEXT TABLE ENTRY                     
         LA    R3,1(R3)            INCREMENT 'START AT' NUMBER                  
         LA    RF,SRVSTATH                                                      
         CR    R2,RF                                                            
         BL    FORMAT10            STILL MORE ROOM ON SCREEN                    
         DROP  R2                                                               
*                                                                               
FORMATX  XIT1  REGS=(R3,R4)                                                     
         EJECT                                                                  
LOCK     BAS   RE,VALSENAM         VALIDATE SENAME                              
         BNE   INVALID                                                          
         BAS   RE,VALCC            VALIDATE COMPANY CODE                        
         BNE   INVALID                                                          
         BAS   RE,VALUL            VALIDATE UNIT/LEDGER CODE                    
         BNE   INVALID                                                          
*                                                                               
         GOTO1 CSWITCH,DMCB,(SENUM,X'FFFFFFFF'),0                               
         CLI   DMCB+4,0                                                         
         BE    *+18                                                             
         MVC   SRVMSG(30),=C'** ERROR ** SENAME NOT STARTED'                    
         OI    SRVP2H+6,X'40'      SET CURSOR                                   
         B     BYE                                                              
*                                                                               
         OI    SRVTABH+6,X'40'     SET CURSOR                                   
         GOTO1 VSETLOCK,DMCB,('PARMLOCK',DUB),(R9)                              
         ZIC   RF,DMCB+4           RETURN CODE...                               
         MH    RF,=H'6'            ...TIMES L'MVC...                            
         LA    RF,LOCKMESS(RF)     ...PLUS A(MESSAGES)...                       
         LA    RE,LOCKMESX         ...GIVES A(INSTRUCTION)...                   
         CR    RF,RE               ...WHICH MUST BE IN TABLE                    
         BL    *+6                                                              
         DC    H'0'                                                             
         EX    R0,0(RF)            PUT MESSAGE IN TWA FIELD                     
*                                                                               
         GOTO1 CSWITCH,DMCB,(SAVESYS,X'FFFFFFFF'),0                             
         CLI   DMCB+4,0                                                         
         BE    BYE                                                              
         DC    H'0'                CANNOT SWITCH BACK                           
         SPACE 3                                                                
LOCKMESS MVC   SRVMSG(13),=C'LEDGER LOCKED'                                     
         MVC   SRVMSG(32),=C'** ERROR ** LOCKING IS INHIBITED'                  
         MVC   SRVMSG(35),=C'** WARNING ** CANNOT LOCK THE TABLE'               
         MVC   SRVMSG(31),=C'** ERROR ** INVALID LEDGER CODE'                   
         MVC   SRVMSG(27),=C'** ERROR ** LOCK TABLE FULL'                       
         MVC   SRVMSG(35),=C'** WARNING ** LEDGER ALREADY LOCKED'               
LOCKMESX EQU   *                                                                
         EJECT                                                                  
UNLOCK   BAS   RE,VALSENAM         VALIDATE SENAME                              
         BNE   INVALID                                                          
         BAS   RE,VALCC            VALIDATE COMPANY CODE                        
         BNE   INVALID                                                          
         BAS   RE,VALUL            VALIDATE UNIT/LEDGER CODE                    
         BNE   INVALID                                                          
*                                                                               
         GOTO1 CSWITCH,DMCB,(SENUM,X'FFFFFFFF'),0                               
         CLI   DMCB+4,0                                                         
         BE    *+18                                                             
         MVC   SRVMSG(30),=C'** ERROR ** SENAME NOT STARTED'                    
         OI    SRVP2H+6,X'40'      SET CURSOR                                   
         B     BYE                                                              
*                                                                               
         OI    SRVTABH+6,X'40'     SET CURSOR                                   
         GOTO1 VSETLOCK,DMCB,('PARMTEST',DUB),(R9)                              
         TM    DMCB+4,LDGTSLOK                                                  
         BO    UNLOCK10            LEDGER IS LOCKED -- UNLOCK IT                
         CLI   DMCB+4,0                                                         
         BNE   *+14                LEDGER DOES NOT EXIST                        
         MVC   SRVMSG(37),=C'** WARNING ** LEDGER ALREADY UNLOCKED'             
         B     UNLOCKX                                                          
*                                                                               
         ZIC   RF,DMCB+4           RETURN CODE...                               
         MH    RF,=H'6'            ...TIMES L'MVC...                            
         LA    RF,LOCKMESS(RF)     ...PLUS A(MESSAGES)...                       
         LA    RE,LOCKMESX         ...GIVES A(INSTRUCTION)...                   
         CR    RF,RE               ...WHICH MUST BE IN TABLE                    
         BL    *+6                                                              
         DC    H'0'                                                             
         EX    R0,0(RF)            PUT MESSAGE IN TWA FIELD                     
         B     UNLOCKX                                                          
*                                                                               
UNLOCK10 GOTO1 VSETLOCK,DMCB,('PARMUNLK',DUB),(R9)                              
         CLI   DMCB+4,0            RETURN CODE                                  
         BNE   *+14                                                             
         MVC   SRVMSG(15),=C'LEDGER UNLOCKED'                                   
         B     UNLOCKX                                                          
         CLI   DMCB+4,PARMERR2                                                  
         BE    *+6                                                              
         DC    H'0'                UNEXPECTED RETURN CODE                       
         MVC   SRVMSG(35),=C'** WARNING ** CANNOT LOCK THE TABLE'               
*                                                                               
UNLOCKX  GOTO1 CSWITCH,DMCB,(SAVESYS,X'FFFFFFFF'),0                             
         B     BYE                                                              
         EJECT                                                                  
VALSENAM ST    RE,SAVERE                                                        
         LA    R2,SRVP2H                                                        
         L     R5,ASELIST                                                       
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),SENAME      FIND SENAME                                  
         BE    *+12                                                             
         BXLE  R5,R6,*-18                                                       
         B     NO                  INVALID SENAME GIVEN                         
         MVC   SENUM,SESYS                                                      
         B     YES                                                              
         DROP  R5                                                               
         SPACE 3                                                                
VALCC    ST    RE,SAVERE                                                        
         LA    R2,SRVP3H                                                        
         CLI   5(R2),2                                                          
         BNE   NO                                                               
         GOTO1 CHEXIN,DMCB,8(R2),DUB,2                                          
         CLC   =F'1',DMCB+12       COMPANY CODE                                 
         BNE   NO                                                               
         B     YES                                                              
         SPACE 3                                                                
VALUL    ST    RE,SAVERE                                                        
         LA    R2,SRVP4H                                                        
         CLI   5(R2),2                                                          
         BNE   NO                                                               
         MVC   DUB+1(2),8(R2)      UNIT/LEDGER                                  
         B     YES                                                              
         SPACE 3                                                                
NO       LA    R1,1                                                             
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
STOP     GOTO1 VSETLOCK,DMCB,('PARMSTOP',0),(R9)                                
         MVC   LOCKSTAT,DMCB+4     SAVE LOCK TABLE STATUS                       
         MVC   SRVMSG(20),=C'TABLE STATUS CHANGED'                              
         OI    SRVP1H+6,X'40'      SET CURSOR                                   
         B     BYE                                                              
         SPACE 5                                                                
FREE     GOTO1 VSETLOCK,DMCB,('PARMFREE',0),(R9)                                
         MVC   LOCKSTAT,DMCB+4     SAVE LOCK TABLE STATUS                       
         MVC   SRVMSG(20),=C'TABLE STATUS CHANGED'                              
         OI    SRVP1H+6,X'40'      SET CURSOR                                   
         B     BYE                                                              
         EJECT                                                                  
RELO     DS    F                   RELOCATION FACTOR                            
*                                                                               
INVALID  MVC   SRVMSG(31),=C'** ERROR ** INVALID INPUT FIELD'                   
         OI    6(R2),X'40'         SET CURSOR                                   
*                                                                               
BYE      TM    LOCKSTAT,X'40'      LOCK/UNLOCK IN PROCESS?                      
         BZ    *+14                                                             
         MVC   SRVSTAT(22),=C'LOCK/UNLOCK IN PROCESS'                           
         B     XMOD                                                             
         TM    LOCKSTAT,X'80'      LOCKING INHIBITED?                           
         BZ    *+14                                                             
         MVC   SRVSTAT(17),=C'LOCKING INHIBITED'                                
         B     XMOD                                                             
         MVC   SRVSTAT(15),=C'LOCKING ENABLED'                                  
*                                                                               
XMOD     XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
         DS    0F                                                               
ACTTAB   DS    0XL12               ACTION TABLE                                 
         DC    C'DISPLAY ',A(DISPLAY)                                           
         DC    C'LOCK    ',A(LOCK)                                              
         DC    C'UNLOCK  ',A(UNLOCK)                                            
         DC    C'STOP    ',A(STOP)                                              
         DC    C'FREE    ',A(FREE)                                              
         DC    X'FF'                                                            
         EJECT                                                                  
LOMWRK   DSECT                                                                  
*                                                                               
SRPARS   DS    0CL24               PARAMETERS FROM FACPAK                       
SRPAR1   DS    F                                                                
SRPAR2   DS    F                                                                
SRPAR3   DS    F                                                                
SRPAR4   DS    F                                                                
SRPAR5   DS    F                                                                
SRPAR6   DS    F                                                                
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL32                                                             
SAVERE   DS    F                                                                
VSETLOCK DS    V                   A(ACSETLOCK)                                 
ASELIST  DS    A                   A(SELIST)                                    
NUMENTRY DS    A                   NO. TABLE ENTRIES (A-TYPE FOR GOTO1)         
SENUM    DS    X                   LEDGER SENUM OR SENUM FILTER                 
SAVESYS  DS    X                   SAVED SENUM                                  
LOCKSTAT DS    X                   LOCK TABLE STATUS                            
LOCKFILT DS    C                   'Y' = DISPLAY LOCKED LEDGERS ONLY            
*                                                                               
LOMTAB   DS    1000XL(LDGTABL)     MY LEDGER LOCKOUT TABLE                      
*                                                                               
LOMWRKX  EQU   *                                                                
         SPACE 5                                                                
LSTD     DSECT                                                                  
LSTLEN   DS    0XL44                                                            
         DS    XL8                 FIELD HEADER                                 
LSTLOCK  DS    C                   C'*' IF LOCKED                               
LSTSENAM DS    CL7                 SENAME                                       
         DS    CL2                                                              
LSTCC    DS    CL2                 COMPANY CODE                                 
         DS    CL2                                                              
LSTUL    DS    CL2                 UNIT/LEDGER                                  
         DS    CL2                                                              
LSTTIME  DS    CL8                 TIME LAST LOCKED                             
         DS    CL2                                                              
LSTLUID  DS    CL8                 LOCKING LUID                                 
         EJECT                                                                  
       ++INCLUDE ACSETLOCKD                                                     
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         SPACE 3                                                                
       ++INCLUDE SRLOMFFD                                                       
         SPACE 5                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SRLOM00   05/01/02'                                      
         END                                                                    
