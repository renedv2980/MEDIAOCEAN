*          DATA SET SRTSK00S   AT LEVEL 006 AS OF 05/01/02                      
*PHASE T15200A                                                                  
         TITLE '$TASK - DISPLAY TASK LIST'                                      
*        REGISTER USAGE :-                                                      
*                                                                               
*        R0    WORK                                                             
*        R1    WORK  /  LINKAGE                                                 
*        R2    WORK                                                             
*        R3    WORK                                                             
*        R4    WORK  /  PGMLST ENTRY                                            
*        R5    WORK  /  SELIST ENTRY                                            
*        R6    CURRENT SCREEN LINE                                              
*        R7    CURRENT UTL ENTRY                                                
*        R8    CURRENT TCB ENTRY                                                
*        R9    SYSFACS                                                          
*        RA    TWA                                                              
*        RB    PROGRAM BASE                                                     
*        RC    TASK WORK AREA                                                   
*        RD    SAVE AREA CHAIN                                                  
*        RE    WORK  /  LINKAGE                                                 
*        RF    WORK  /  LINKAGE                                                 
*                                                                               
TASK     CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKL,*$TASK**                                                    
         USING WRKD,RC             RC=A(W/S)                                    
         XC    WRKD(WRKL),WRKD     CLEAR W/S                                    
         ST    RD,SAVERD                                                        
         USING SRPARMD,R1          R1=A(PARMS)                                  
         L     RA,SRPARM6                                                       
         USING SRTSKFFD,RA         RA=A(TWA)                                    
         L     R9,SRPARM1                                                       
         USING SYSFACD,R9          R9=A(SYSFACS)                                
*                                                                               
         L     R2,SRPARM4                                                       
         USING COMFACSD,R2         R2=A(COMFACS)                                
         MVC   VHEXOUT,CHEXOUT     SAVE USEFUL ENTRY POINTS                     
         DROP  R2                  FINISHED WITH COMFACS                        
         DROP  R1                  FINISHED WITH SRPARMS                        
*                                                                               
         GOTO1 VTICTOC,DMCB,C'TGET' GET TIME OF DAY IN TU'S                     
         MVC   TIMENOW,DMCB                                                     
*                                                                               
         L     R2,VSSB             GET VSSB                                     
         USING SSBD,R2                                                          
         LA    R1,MINTAB                                                        
INIT01   CLC   SSBSYSNA,0(R1)      FIND MINIMUM TASK NUMBER                     
         BE    INIT02                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNE   INIT01                                                           
*                                                                               
INIT02   MVC   MINTASK,3(R1)       SET MINIMUM NUMBER                           
         DROP  R2                                                               
         EJECT                                                                  
*              VALIDATE PARAMETERS                                              
*                                                                               
VALPARMS NI    SRVSRVH+6,X'BF'     SET OFF IC ORDER IN SRV INP FIELD            
         OI    SRVP1H+6,X'40'      PUT CURSOR UNDER TASK ID FIELD               
*                                                                               
VALP1    LA    R2,SRVP1H           STARTING TASK ID                             
         CLI   5(R2),0             WAS IT ENTERED ?                             
         BE    VALP19              NO - SET DEFAULT                             
         CLI   5(R2),1             LENGTH 1 BYTE ?                              
         BNE   VALP1A              NO - TRY TASKS=N                             
         B     VALP2               ACCEPT AS IS (TCB SCAN WILL PROVE)           
VALP19   MVI   5(R2),1             DEFAULT LENGTH INPUT                         
         MVI   8(R2),C'1'          DEFAULT START TASK                           
         B     VALP2                                                            
*                                                                               
VALP1A   CLI   5(R2),8             MUST BE 7 OR 8                               
         BH    ERR1                                                             
         CLI   5(R2),7             TO BE TASKS=NN                               
         BL    ERR1                                                             
         CLC   8(6,R2),=C'TASKS='                                               
         BNE   ERR1                                                             
         SR    R3,R3               GET LEN OF NUMBER INTO R3                    
         IC    R3,5(R2)                                                         
         SH    R3,=H'6'            LEN IS FIELD LEN-6                           
         LA    R4,14(R2)                                                        
         BAS   RE,VALNUM           VALIDATE TASK NUMBER                         
         CLI   DUB,X'FF'                                                        
         BE    ERR1                INVALID TASK NUMBER                          
         CVB   R1,DUB                                                           
         LA    R1,1(R1)                                                         
         STC   R1,LASTTASK                                                      
         BAS   RE,SETNOP           GO NOP THIS TASK                             
         MVC   8(8,R2),=CL8'1'                                                  
*                                                                               
VALP2    MVC   TASKSTRT,8(R2)      SAVE TASK ID                                 
*                                                                               
         LA    R2,SRVP2H           FILTER                                       
         LA    R3,FILTDEF          DEFAULT FILTER                               
         NI    SRVP1H+6,X'BF'                                                   
         OI    SRVP2H+6,X'40'      CURSOR AT FILTER                             
         CLI   5(R2),0             FILTER ENTERED ?                             
         BE    VALP29              NO - TAKE DEFAULT                            
         CLI   5(R2),8             FILTER LONGER THAN 8 ?                       
         BH    VALP22              YES - ERROR                                  
         ZIC   R1,5(R2)            FIELD LENGTH                                 
         BCTR  R1,0                MINUS ONE                                    
         LA    R3,FILTTAB          TABLE OF FILTERS                             
VALP21   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(R3)       FILTER IN TABLE ?                            
         BE    VALP29              YES - EXIT LOOP                              
         CLI   9(R3),X'FF'         TABLE END ?                                  
         BE    VALP22              YES - ERROR                                  
         LA    R3,9(,R3)           NEXT ENTRY                                   
         B     VALP21                                                           
VALP22   MVC   SRVL2+20(15),=C'VALID FILTERS -'                                 
         LA    R2,FILTTAB                                                       
         LA    R3,SRVL1H+3*(SRVL2-SRVL1)                                        
         LA    R4,13                                                            
VALP23   MVC   28(8,R3),0(R2)       SET UP LIST OF FILTERS                      
         LA    R2,9(,R2)                                                        
         LA    R3,SRVL2-SRVL1(,R3)                                              
         CLI   0(R2),X'FF'                                                      
         BE    VALP24                                                           
         BCT   R4,VALP23                                                        
VALP24   MVC   0(3,R3),=X'000100'                                               
         B     ERR2                                                             
*                                                                               
VALP29   MVI   5(R2),3                                                          
         MVC   FILTER,8(R3)        SET FILTER TYPE CODE                         
         MVC   8(8,R2),0(R3)       SET FILTER FULL NAME IN SCREEN               
*                                                                               
VALPEND  EQU  *                                                                 
         NI    SRVP2H+6,X'BF'                                                   
         OI    SRVP3H+6,X'40'      CURSOR BEYOND FILTER                         
         EJECT                                                                  
*        LOCATE FIRST TCB REQUESTED FOR DISPLAY                                 
*                                                                               
DISP     LA    R6,SRVL1H-(SRVL2-SRVL1) FIRST SCREEN LINE MINUS 1                
         L     R8,VTCB             POINT TO TCB TABLES                          
         LH    R2,0(R8)            LENGTH TCB ENTRY                             
         L     R3,2(R8)            LAST ENTRY ADDRESS                           
         LA    R8,6(R8)            FIRST ENTRY ADDRESS                          
         USING TCBD,R8                                                          
*                                                                               
FINDFRST CLC   TCBID+6(1),TASKSTRT IS THIS REQUIRED START POINT ?               
         BE    GOTTCB              YES - GO TO IT                               
         BXLE  R8,R2,FINDFRST      LOOK AT NEXT TCB                             
         B     ERR3                REQUESTED TCBID NOT FOUND                    
         SPACE 3                                                                
*        SELECT TCB ACCORDING TO FILTERS                                        
*                                                                               
GOTTCB   ICM   R7,15,TCBUTL        ADDRESS THIS TASKS UTL ENTRY                 
         USING UTLD,R7                                                          
         CLI   TCBSYS,0            ANY PROGRAM ACTIVE ?                         
         BE    INACTIVE            BRANCH IF NONE (INACTIVE)                    
*                                                                               
ACTIVE   TM    FILTER,B'00000010'  DO WE WANT ACTIVE TASKS ?                    
         BO    WANTTCB             YES - PRINT THIS ONE                         
         TM    FILTER,B'00000100'  LOOKING FOR LONG RUNNERS ?                   
         BZ    NEXTTCB             NO - IGNORE THIS                             
         L     R1,TIMENOW          CALCULATE DURATION SO FAR                    
         S     R1,TCBSTTM                                                       
*&&DO*&& C     R1,=A(300*10)       DURATION MORE THAN 10 SECONDS ?              
*&&OS*&& C     R1,=A(100*10)       DURATION MORE THAN 10 SECONDS ?              
         BL    NEXTTCB             NO - IGNORE IT                               
         B     WANTTCB             ELSE GO USE IT                               
INACTIVE TM    FILTER,B'00000001'  DO WE WANT INACTIVE TASKS ?                  
         BZ    NEXTTCB             NO - IGNORE THIS ONE                         
*                                                                               
WANTTCB  LA    R6,SRVL2-SRVL1(,R6) POINT TO NEXT LINE                           
         LA    R1,SRVLAST                                                       
         CR    R6,R1               HAVE WE DONE THE LAST LINE ?                 
         BL    GOTLINE             NO - SKIP                                    
*                                                                               
ENDSCRN  CLC   SRVP2(8),FILTALL    IS IT REQUEST FOR ALL                        
         BNE   EXITNEXT            NO - DONT DO ANY PAGING                      
         MVC   SRVP1(1),TCBID+6    SAVE ID OF NEXT TCB FOR PAGEING              
         B     EXITPAGE            AND GET OUT WITH NEXT PAGE MESSAGE           
         EJECT                                                                  
*        BUILD SCREEN LINE FROM TCB                                             
*                                                                               
         USING SCDSECT,R6                                                       
GOTLINE  EQU   *                                                                
*                                                                               
TASKID   MVI   SCDTID,C'T'         TASK ID                                      
         MVC   SCDTID+1(1),TCBID+6                                              
         SR    R1,R1                                                            
         ICM   R1,7,TCBLOCK+1                                                   
         BZ    TASKID1                                                          
         MVI   SCDTID,C'W'                                                      
         MVI   SCDTID+2,C'.'       SHOW TASK IN WAIT FLAG                       
         CLC   0(5,R1),=C'*TASK'                                                
         BNE   *+10                                                             
         MVC   SCDTID+0(1),6(R1)   SHOW WAIT TASK ID                            
TASKID1  TM    TCBFLAG2,TCBNOPED                                                
         BNO   *+8                                                              
         MVI   SCDTID+2,C'X'       SHOW TASK NOPED POINT                        
TASKIDX  EQU   *                                                                
*                                                                               
         LA    R2,TCBPGMA+1        EDIT A(PGM)                                  
         LA    R3,SCDAPGM                                                       
         BAS   RE,ADDRCONV                                                      
*                                                                               
         LA    R2,TCBWRKA+1        EDIT A(WRK)                                  
         LA    R3,SCDAWRK                                                       
         BAS   RE,ADDRCONV                                                      
*                                                                               
         LA    R2,TCBUTL+1         EDIT A(UTL)                                  
         LA    R3,SCDAUTL                                                       
         BAS   RE,ADDRCONV                                                      
*                                                                               
         LA    R2,TCBTWA+1         EDIT A(TWA)                                  
         LA    R3,SCDATWA                                                       
         BAS   RE,ADDRCONV                                                      
*                                                                               
         MVC   SCDLINE,DOTS        PRIME LINE/TERM WITH DOTS                    
         MVI   SCDSL1,C'/'                                                      
         MVC   SCDTRM,DOTS                                                      
         CLI   TCBLINE,X'40'       IS LINE ADDRESS SET UP ?                     
         BNH   *+10                NO - SKIP                                    
         MVC   SCDLINE,TCBLINE     MOVE IN LINE ADDRESS                         
         CLI   TCBTRM,X'40'        IS TERM SET UP ?                             
         BNH   *+10                                                             
         MVC   SCDTRM,TCBTRM       AND TERMINAL ADDRESS                         
*                                                                               
         MVC   SCDIO,DOTS                                                       
         CLI   TCBSYS,0            TASK ACTIVE ?                                
         BE    TIMER               NO - IGNORE COUNTER                          
         XR    R2,R2                                                            
         ICM   R2,3,TCBIOCNT       ANY IO'S ?                                   
         BZ    TIMER               NO - DONT PRINT                              
         EDIT  (R2),(5,SCDIO)                                                   
*                                                                               
TIMER    MVC   SCDTIMER,DOTS                                                    
         CLI   TCBSYS,0            TASK ACTIVE ?                                
         BE    CPUTIME             NO - IGNORE COUNTER                          
         XR    R2,R2                                                            
         ICM   R2,3,TCBIRPTS       ANY TIMER INTERRUPTS ?                       
         BZ    CPUTIME             NO DONT PRINT                                
         EDIT  (R2),(5,SCDTIMER)                                                
*                                                                               
CPUTIME  MVC   SCDCPU,DOTS                                                      
         CLI   TCBSYS,0            TASK ACTIVE ?                                
         BE    ELAPSED             NO - IGNORE COUNTER                          
         ICM   R1,15,TCBCPUTM      ANY CPU TIME ?                               
         BZ    ELAPSED             NO - DONT PRINT                              
*&&DO                                                                           
         LA    R1,1(,R1)           ROUND                                        
         XR    R0,R0                                                            
         D     R0,=F'3'            GET CPU FROM TU'S TO SECONDS/100             
*&&                                                                             
         EDIT  (R1),(6,SCDCPU-1),2                                              
         CLI   SCDCPU+1,C' '                                                    
         BNE   ELAPSED                                                          
         MVI   SCDCPU+1,C'0'                                                    
*                                                                               
ELAPSED  MVC   SCDELAPS,DOTS                                                    
         CLI   TCBSYS,0            TASK ACTIVE ?                                
         BE    *+12                                                             
         ICM   R0,15,TCBSTTM       YES - GET START TIME                         
         B     *+12                                                             
         ICM   R0,15,TCBNDTM       ELSE USE END TIME (IDLE)                     
         BZ    SYSPROG             IF THATS ZERO FORGET IT                      
         L     R1,TIMENOW          CALCULATE ELAPSED TIME                       
         SR    R1,R0                                                            
*&&DO*&& AH    R1,=H'15'           ROUND                                        
*&&OS*&& AH    R1,=H'5'                                                         
         XR    R0,R0                                                            
*&&DO*&& D     R0,=F'30'           CHANGE ELAPSED TU'S TO SECONDS/10            
*&&OS*&& D     R0,=F'10'           CHANGE ELAPSED SEC/100 TO SEC/10             
         EDIT  (R1),(6,SCDELAPS),1                                              
         CLI   SCDELAPS+3,C' '                                                  
         BNE   SYSPROG                                                          
         MVI   SCDELAPS+3,C'0'                                                  
*                                                                               
SYSPROG  MVC   SPWORK+0(1),TCBSYS                                               
         CLI   TCBSYS,1                                                         
         BNH   *+10                                                             
         MVC   SPWORK+0(1),TCBSWSYS                                             
         MVC   SPWORK+1(1),TCBPRG                                               
         BAS   RE,PGMCHASE         GO EXPAND IT                                 
         LA    R0,10               MOVE FIRST TEN BYTES                         
         CR    R1,R0                                                            
         BNH   *+6                                                              
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SCDSYSPR(0),WORK                                                 
*                                                                               
PQID     LA    RF,SCDENQPQ         POINT TO POSN TO SHOW PRTQ ENQ               
         TM    TCBFLAG1,TCBENQPQ                                                
         BZ    PQIDX                                                            
         MVI   0(RF),C'U'                                                       
         CLI   TCBPQCHR,C'1'                                                    
         BL    *+10                                                             
         MVC   0(1,RF),TCBPQCHR                                                 
PQIDX    EQU   *                                                                
         EJECT                                                                  
*        POINT TO NEXT TCB IN CHAIN                                             
*                                                                               
NEXTTCB  L     R1,VTCB             GET TCB TABLE                                
         LH    R2,0(R1)            TCB ENTRY LENGTH                             
         L     R3,2(R1)            LAST TCB ADDRESS                             
         BXLE  R8,R2,GOTTCB        GO ROUND AGAIN IF ANOTHER TCB                
*                                                                               
         MVC   SRVL2-SRVL1(3,R6),=X'000100' SET TWA END                         
         CLC   SRVP2(8),FILTALL    IS IT REQUEST FOR ALL                        
         BNE   EXITNEXT            NO - DONT RESET START POINT                  
         MVI   SRVP1,C'1'          START AT FIRST TCB NEXT TIME                 
         B     EXITNEXT            EXIT WITH NEXT REQUEST MESSAGE               
         EJECT                                                                  
*                                                                               
*        SUBROUTINE SETNOP - SET NOP POINT IN TCB OF LASTTASK                   
*                                                                               
*                                                                               
SETNOP   NTR1                                                                   
         L     R8,VTCB             POINT TO TCB TABLES                          
         LH    R2,0(R8)            LENGTH TCB ENTRY                             
         L     R3,2(R8)            LAST ENTRY ADDRESS                           
         LA    R8,6(R8)            FIRST ENTRY ADDRESS                          
         LA    R1,1                START TASK 1                                 
         USING TCBD,R8                                                          
*                                                                               
SETNOP1  CLM   R1,1,LASTTASK       IS THIS REQUIRED TASK                        
         BE    SETNOP2             YES - GO TO IT                               
         LA    R1,1(R1)                                                         
         BXLE  R8,R2,SETNOP1       LOOK AT NEXT TCB                             
         CLM   R1,1,LASTTASK       ARE WE RESTORING MAX TASKS                   
         BE    SETNOP3                                                          
         B     ERR5                REQUESTED TCBID NOT FOUND                    
*                                                                               
SETNOP2  CLC   LASTTASK,MINTASK    TEST LASTTASK WITH MINTASK                   
         BL    ERR4                                                             
*                                                                               
         OI    TCBFLAG2,TCBNOPED   SET THIS TASK TO NOP                         
*                                                                               
SETNOP3  L     R8,VTCB             POINT TO TCB TABLES                          
         LH    R2,0(R8)            LENGTH TCB ENTRY                             
         L     R3,2(R8)            LAST ENTRY ADDRESS                           
         LA    R8,6(R8)            FIRST ENTRY ADDRESS                          
         LA    R1,1                                                             
         USING TCBD,R8                                                          
*                                                                               
SETNOP4  CLM   R1,1,LASTTASK           IS THIS REQUIRED TASK                    
         BE    *+8                                                              
         NI    TCBFLAG2,255-TCBNOPED   REMOVE ANY OTHER NOPS                    
         LA    R1,1(R1)                                                         
         BXLE  R8,R2,SETNOP4           LOOK AT NEXT TCB                         
         XIT1                                                                   
         EJECT                                                                  
*        SUBROUTINE ADDRCONV - EDITS F'WORD INTO 6 BYTES                        
*                                                                               
*        R2=A(F'WORD+1), R3=A(O/P), RE=RETURN ADDR                              
*                                                                               
ADDRCONV DS    0H'0'                                                            
         MVC   0(6,R3),DOTS        PRIME WITH DOTS                              
         NC    0(3,R2),0(R2)       IS ADDRESS ZERO ?                            
         BZR   RE                  YES - RETURN                                 
         ST    RE,SAVERE                                                        
         GOTO1 VHEXOUT,DMCB,(R2),(R3),3                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
DOTS     DC    10C'.'                                                           
         SPACE 3                                                                
*        SUBROUTINE PGMCHASE - BUILDS SYSTEM/PHASE FIELD IN WORK                
*                                                                               
*        ENTRY-                                                                 
*              SPWORK=SYS/PRG BYTES                                             
*        EXIT-                                                                  
*              RETURNS SQUASHED 'SSSSSSS/PPPPPPP' IN WORK                       
*              R1=LENGTH OUTPUT FIELD                                           
*                                                                               
PGMCHASE DS    0H'0'                                                            
         CLI   SPWORK,0            IS ANY SYSTEM ACTIVE ?                       
         BNE   NOTIDLE             YES - FIND WHICH                             
         MVC   WORK(10),DOTS       ELSE RETURN DOTS                             
         LA    R1,10                                                            
         BR    RE                                                               
NOTIDLE  ST    RE,SAVERE           SAVE EXIT ADDRESS                            
         GOTO1 VHEXOUT,DMCB,SPWORK,FULL,2  EXPAND SYS/PROG                      
         MVI   WORK,C' '           PRIME WORK AREA                              
         MVC   WORK+1(14),WORK                                                  
         MVI   WORK+7,C'/'                                                      
         L     R5,VSELIST          POINT TO START OF SELIST                     
         LH    R2,0(R5)            PRIME INDICES                                
         L     R3,2(R5)                                                         
         LA    R5,6(,R5)                                                        
         USING SELISTD,R5                                                       
SESCAN   CLC   SESYS,SPWORK        IS THIS THE SYSTEM ?                         
         BE    GOTSE               YES - SKIP                                   
         BXLE  R5,R2,SESCAN        LOOP TO END OF SELIST                        
*                                                                               
BADSE    MVC   WORK(2),FULL        MOVE X'SS' TO SSSSSSS AREA                   
         B     BADPGM                                                           
*                                                                               
GOTSE    LA    R2,SENAME+6         POINT TO END OF SENAME                       
GOTSEL1  CLI   0(R2),X'40'         IS THIS THE LAST NO BLANK ?                  
         BNE   GOTSEL2             YES - SKIP                                   
         BCT   R2,GOTSEL1          ELSE SCAN BACK                               
GOTSEL2  CLI   0(R2),X'F0'         IS LAST CHAR NUMERIC ?                       
         BNL   GOTSEL3             YES - SKIP                                   
         MVC   WORK(4),SENAME      MOVE ONLY FIRST FOUR CHARS                   
         B     GOTSEL4                                                          
GOTSEL3  MVC   WORK(7),SENAME      MOVE WHOLE SENAME                            
*                                                                               
GOTSEL4  L     R4,SEPGMS           PROGRAM NAME LIST FOR THIS SYSTEM            
         USING PGMLSTD,R4                                                       
         LH    R2,0(,R4)           PRIME INDEXES                                
         L     R3,2(,R4)                                                        
         LA    R4,6(,R4)                                                        
PGMSCAN  CLC   PGMNUM,SPWORK+1     IS THIS THE PROGRAM ?                        
         BE    GOTPGM              YES - SKIP                                   
         BXLE  R4,R2,PGMSCAN       LOOP TO END PGMLIST                          
*                                                                               
BADPGM   MVC   WORK+8(2),FULL+2    MOVE X'PP' TO PPPPPPP AREA                   
         B     CHASEND                                                          
*                                                                               
GOTPGM   MVC   WORK+8(7),PGMNAME                                                
         DROP  R5                                                               
         DROP  R4                                                               
CHASEND  LA    R4,WORK             COMPRESS ALL BLANKS FROM WORK                
         LR    R5,R4               SAVE WORK ADDRESS                            
         LR    R1,R4                                                            
         LA    R2,15               BYTE COUNT TO COMPRESS                       
SQUASH1  CLI   0(R4),C' '          BLANK ?                                      
         BE    SQUASH2             YES - IGNORE IT                              
         MVC   0(1,R1),0(R4)       NO MOVE IT                                   
         LA    R1,1(,R1)           POINT TO NEXT TARGET BYTE                    
SQUASH2  LA    R4,1(,R4)           POINT TO NEXT SOURCE BYTE                    
         BCT   R2,SQUASH1          LOOP R2 TIMES                                
         SR    R1,R5               GET LENGTH IN BYTES AFTER SQUASH             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
ERR1     MVC   SRVMSG+12(L'MSG1),MSG1                                           
         B     ERRX                                                             
ERR2     MVC   SRVMSG+12(L'MSG2),MSG2                                           
         B     ERRX                                                             
ERR3     MVC   SRVMSG+12(L'MSG3),MSG3                                           
         B     ERRX                                                             
ERR4     MVC   SRVMSG+12(L'MSG4),MSG4                                           
         B     ERRX                                                             
ERR5     MVC   SRVMSG+12(L'MSG5),MSG5                                           
         B     ERRX                                                             
*                                                                               
ERRX     MVC   SRVMSG(11),=C'** ERROR **'                                       
         L     RD,SAVERD                                                        
         B     EXIT                                                             
*                                                                               
       ++INCLUDE DDVALNUM                                                       
*                                                                               
EXITPAGE MVC   SRVMSG+L'MSG0(L'MSG0P),MSG0P                                     
         B     EXITCOMM                                                         
EXITNEXT MVC   SRVMSG+L'MSG0(L'MSG0N),MSG0N                                     
EXITCOMM MVC   SRVMSG(L'MSG0),MSG0                                              
         L     R5,VSELIST                                                       
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(,R5)                                                        
         USING SELISTD,R5                                                       
         XR    R1,R1                                                            
         AH    R1,SEQLEN                                                        
         BXLE  R5,R6,*-4                                                        
         ST    R1,QLEN                                                          
         DROP  R5                                                               
         LA    R5,SRVMSG+13                                                     
         EDIT  QLEN,(5,(R5)),ALIGN=LEFT                                         
         NC    QLEN,QLEN                                                        
         BNZ   *+8                                                              
         MVI   0(R5),C'0'                                                       
*                                                                               
EXIT     XMOD1 1                                                                
*                                                                               
         EJECT                                                                  
*              LITERALS                                                         
*                                                                               
         LTORG                                                                  
*                                                                               
FILTTAB  DS    0CL9                TABLE OF VALID FILTER OPTIONS                
FILTINAC DC    CL8'INACTIVE',B'00000001'                                        
FILTACT  DC    CL8'ACTIVE  ',B'00000010'                                        
FILTLONG DC    CL8'LONG    ',B'00000100'  LONGER THAN 1 MIN ELAPSED             
FILTALL  DC    CL8'ALL     ',B'11111111'  DEFAULT ENTRY                         
*&&UK                                                                           
FILTDEF  EQU   FILTALL                                                          
*&&                                                                             
*&&US                                                                           
FILTDEF  EQU   FILTACT                                                          
*&&                                                                             
         DC    X'FF'                                                            
*                                                                               
MSG0     DC    C'INPUT QLEN = XXXXX- '  +13(5) = XXXXX                          
MSG0P    DC    C'ENTER TO PAGE'                                                 
MSG0N    DC    C'ENTER NEXT REQUEST'                                            
MSG1     DC    C'INVALID TASK ID'                                               
MSG2     DC    C'INVALID FILTER'                                                
MSG3     DC    C'START TASK NOT FOUND'                                          
MSG4     DC    C'NUMBER OF TASKS LESS THAN MINIMUM'                             
MSG5     DC    C'TASK NOT FOUND'                                                
*&&UK                                                                           
MINTAB   DC    C'AD1',AL1(16)      MINIMUM TASK VALUE                           
         DC    C'AD2',AL1(3)                                                    
         DC    C'AD3',AL1(3)                                                    
         DC    C'NEW',AL1(3)                                                    
         DC    C'TST',AL1(3)                                                    
         DC    C'TTS',AL1(3)                                                    
         DC    C'   ',AL1(16)      DEFAULT IF NOT IN TABLE                      
*&&                                                                             
*&&US                                                                           
MINTAB   DC    C'AD1',AL1(16)      MINIMUM TASK VALUE                           
         DC    C'AD2',AL1(16)                                                   
         DC    C'AD3',AL1(16)                                                   
         DC    C'AD4',AL1(16)                                                   
         DC    C'AD5',AL1(16)                                                   
         DC    C'AD6',AL1(16)                                                   
         DC    C'REP',AL1(16)                                                   
         DC    C'MEL',AL1(3)                                                    
         DC    C'TST',AL1(3)                                                    
         DC    C'   ',AL1(16)      DEFAULT IF NOT IN TABLE                      
*&&                                                                             
         EJECT                                                                  
*              DSECT TO COVER W/S                                               
*                                                                               
WRKD     DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
SPWORK   DS    CL2                                                              
SAVERE   DS    F                                                                
SAVERD   DS    F                                                                
DMCB     DS    6F                                                               
VHEXOUT  DS    A                                                                
TIMENOW  DS    F                                                                
QLEN     DS    F                                                                
TASKSTRT DS    X                   START TASK ID                                
FILTER   DS    X                   FILTER TYPE CHOSEN                           
LASTTASK DS    X                                                                
MINTASK  DS    X                                                                
WORK     DS    CL20                                                             
*                                                                               
WRKL     EQU   *-WRKD              SHOULD BE LESS THAN 256                      
         SPACE 3                                                                
SCDSECT  DSECT                     DEFINE SCREEN LINE                           
SCDHDR   DS    CL8                 FIELD HEADER                                 
SCDTID   DS    CL2                 TASK ID 'TN'                                 
         DS    C                                                                
SCDAPGM  DS    CL6                 A(PGM)                                       
         DS    C                                                                
SCDAWRK  DS    CL6                 A(WRK)                                       
         DS    C                                                                
SCDLINE  DS    CL4                 LINE ADDR                                    
SCDSL1   DS    C                   '/'                                          
SCDTRM   DS    CL4                 TERM ADDR                                    
         DS    C                                                                
SCDSYSPR DS    CL10                SYS/PROG                                     
         DS    C                                                                
SCDAUTL  DS    CL6                 A(UTL)                                       
         DS    C                                                                
SCDATWA  DS    CL6                 A(TWA)                                       
         DS    C                                                                
SCDIO    DS    CL5                 TOTAL I/O                                    
         DS    C                                                                
SCDCPU   DS    CL5                 TOTAL CPU                                    
         DS    C                                                                
SCDTIMER DS    CL5                 TIMER INTERRUPTS                             
         DS    C                                                                
SCDELAPS DS    CL6                 ELAPSED TIME                                 
         DS    CL1                                                              
SCDENQPQ DS    CL1                 PRTQ ENQUEUED                                
SCDLEN   EQU   *-SCDSECT           SHOULD BE 78 BYTES                           
         EJECT                                                                  
SRTSKFFD DSECT                                                                  
         DS    CL64                                                             
* SRTSKFFD                                                                      
       ++INCLUDE SRTSKFFD                                                       
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SRTSK00S  05/01/02'                                      
         END                                                                    
