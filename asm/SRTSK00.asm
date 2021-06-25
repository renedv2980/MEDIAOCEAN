*          DATA SET SRTSK00    AT LEVEL 003 AS OF 01/19/11                      
*PHASE T15200A                                                                  
         TITLE '$TASK - DISPLAY TASK LIST'                                      
TASK     CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,*$TASK**,CLEAR=YES,RR=RE                                   
         USING WORKD,RC            RC=A(W/S)                                    
         MVC   IPARMS,0(R1)                                                     
         ST    RD,SAVERD                                                        
         ST    RE,RELO                                                          
*                                                                               
         L     RA,ATWA                                                          
         USING SRTSKFFD,RA         RA=A(TWA)                                    
         L     R9,ASYSFACS                                                      
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         BRAS  RE,INIT                                                          
*                                                                               
         BRAS  RE,VALPARMS         VALIDATE PARAMETERS                          
         BRAS  RE,DSPSCRN          DISPLAY SCREEN                               
         B     XMOD                                                             
*                                                                               
***********************************************************************         
* DISPLAY SCREEN AS REQUESTED                                         *         
***********************************************************************         
         SPACE 1                                                                
DSPSCRN  NTR1  ,                                                                
         LA    R6,SRVL1H           FIRST SCREEN LINE                            
         L     R8,VTCB             POINT TO TCB TABLES                          
         LH    R2,0(R8)                                                         
         L     R3,2(R8)                                                         
         AHI   R8,6                                                             
         USING TCBD,R8                                                          
         CLC   TCBID+6(1),TASKSTRT REQUIRED START POINT ?                       
         BE    *+12                YES                                          
         BXLE  R8,R2,*-14                                                       
         B     ERR3                REQUESTED TCBID NOT FOUND                    
*                                                                               
DSPS02   CLI   TCBSYS,0            ANY PROGRAM ACTIVE ?                         
         BNE   *+12                                                             
         TM    FILTER,X'01'        DO WE WANT INACTIVE TASKS ?                  
         BZ    DSPS06              NO - IGNORE THIS ONE                         
*                                                                               
         TM    FILTER,X'02'        DO WE WANT ACTIVE TASKS ?                    
         BO    DSPS04              YES - PRINT THIS ONE                         
         TM    FILTER,X'04'        LOOKING FOR LONG RUNNERS ?                   
         BZ    DSPS06              NO - IGNORE THIS                             
*                                                                               
         L     R1,TIMENOW          CALCULATE DURATION SO FAR                    
         SL    R1,TCBSTTM                                                       
         C     R1,=A(38400*10)     DURATION MORE THAN 10 SECONDS ?              
         BL    DSPS06              NO - IGNORE IT                               
*                                                                               
DSPS04   BRAS  RE,DSPLINE                                                       
*                                                                               
         AHI   R6,SRVL2H-SRVL1H    POINT TO NEXT LINE                           
         LA    R0,SRVLAST                                                       
         CR    R6,R0               HAVE WE DONE THE LAST LINE ?                 
         BL    DSPS06              NO - KEEP GOING                              
*                                                                               
         CLC   SRVP2(8),FILTALL    IS IT REQUEST FOR ALL                        
         BNE   EXITNEXT            NO - DONT DO ANY PAGING                      
         MVC   SRVP1,SPACES                                                     
         MVC   SRVP1(1),TCBID+6    SAVE ID OF NEXT TCB FOR PAGEING              
         B     EXITPAGE            AND GET OUT WITH NEXT PAGE MESSAGE           
*                                                                               
DSPS06   BXLE  R8,R2,DSPS02        GO ROUND AGAIN IF ANOTHER TCB                
*                                                                               
         MVC   SRVL2-SRVL1(3,R6),=X'000100' SET TWA END                         
         CLC   SRVP2(8),FILTALL    IS IT REQUEST FOR ALL                        
         BNE   EXITNEXT            NO - DONT RESET START POINT                  
         MVI   SRVP1,C'1'          START AT FIRST TCB NEXT TIME                 
         B     EXITNEXT            EXIT WITH NEXT REQUEST MESSAGE               
         EJECT                                                                  
***********************************************************************         
* DISPLAY LINE DETAILS                                                *         
* NTRY: R8     = A(TCB ENTRY)                                         *         
*       R6     = A(DISPLAY LINE)                                      *         
* EXIT: CC EQ  = LINE USED FOR DISPLAY                                *         
*       CC NE  = LINE NOT USED (TCB IS INVALID FOR DISPLAY)           *         
***********************************************************************         
         SPACE 1                                                                
         USING TCBD,R8                                                          
         USING SCDSECT,R6                                                       
DSPLINE  NTR1  ,                                                                
         MVI   SCDTID,C'T'         TASK ID                                      
         MVC   SCDTID+1(1),TCBID+6                                              
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,7,TCBLOCK+1                                                   
         BZ    DSPL02                                                           
*                                                                               
         MVI   SCDTID,C'W'                                                      
         MVI   SCDTID+2,C'.'       SHOW TASK IN WAIT FLAG                       
         CLC   0(5,R1),=C'*TASK'                                                
         BNE   *+10                                                             
         MVC   SCDTID(1),6(R1)     SHOW WAIT TASK ID                            
*                                                                               
DSPL02   TM    TCBFLAG2,TCBNOPED                                                
         BZ    *+8                                                              
         MVI   SCDTID+2,C'X'       SHOW TASK NOPED POINT                        
*                                                                               
         MVC   SCDLUID,DOTS        PRIME LUID WITH DOTS                         
         CLI   TCBSYM,X'40'        IS LINE ADDRESS SET UP ?                     
         BNH   *+10                NO - SKIP                                    
         MVC   SCDLUID,TCBSYM      MOVE IN LUID                                 
*                                                                               
         MVC   SCDIO,DOTS                                                       
         MVC   SCDLIO,DOTS                                                      
         MVC   SCDTIMER,DOTS                                                    
         MVC   SCDSYSPR,DOTS                                                    
*                                                                               
         CLI   TCBSYS,0            TASK ACTIVE ?                                
         BE    EXIT                NO - NOTHING ESE TO DISPLAY                  
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,7,TCBIOCNT                                                    
         BZ    DSPL04                                                           
         EDIT  (R0),(6,SCDIO)                                                   
*                                                                               
DSPL04   XR    R0,R0                                                            
         ICM   R0,7,TCBLIOCT                                                    
         BZ    DSPL05                                                           
         EDIT  (R0),(6,SCDLIO)                                                  
*                                                                               
DSPL05   XR    R0,R0                                                            
         ICM   R0,3,TCBIRPTS       ANY TIMER INTERRUPTS ?                       
         BZ    DSPL06              NO DONT PRINT                                
         EDIT  (R0),(3,SCDTIMER)                                                
*                                                                               
DSPL06   XR    R0,R0                                                            
         L     R1,TCBCPUTM                                                      
         M     R0,=F'1000'                                                      
         D     R0,=F'38400'        R1=TIME IN MILLISECS                         
         EDIT  (R1),(6,SCDCPU),3                                                
         CLI   SCDCPU+1,C' '                                                    
         BH    *+8                                                              
         MVI   SCDCPU+1,C'0'                                                    
*                                                                               
         ICM   R0,15,TCBTKWT       GET START TIME                               
         EDIT  (R0),(6,SCDTKW)                                                  
*                                                                               
         ICM   R0,15,TCBSTTM       GET START TIME                               
         L     R1,TIMENOW          CALCULATE ELAPSED TIME                       
         SLR   R1,R0                                                            
         M     R0,=F'1000'         ROUND                                        
         D     R0,=F'38400'        R1=TIME IN MILLISESCS                        
         C     R1,=F'999999'                                                    
         BH    DSPL08                                                           
         EDIT  (R1),(7,SCDELAPS),3                                              
         CLI   SCDELAPS+2,C' '                                                  
         BH    *+8                                                              
         MVI   SCDELAPS+2,C'0'                                                  
         B     DSPL10                                                           
*                                                                               
DSPL08   XR    R0,R0                                                            
         D     R0,=F'1000'                                                      
         EDIT  (R1),(7,SCDELAPS),0                                              
*                                                                               
DSPL10   ICM   R0,15,TCBINTM                                                    
         ICM   R1,15,TCBSTTM       CALCULATE QUEUE TIME                         
         SLR   R1,R0                                                            
         M     R0,=F'1000'         ROUND                                        
         D     R0,=F'38400'        R1=TIME IN MILLISESCS                        
         EDIT  (R1),(6,SCDQUEUE),3                                              
         CLI   SCDQUEUE+1,C' '                                                  
         BH    *+8                                                              
         MVI   SCDQUEUE+1,C'0'                                                  
*                                                                               
         MVC   SCDSYSPR(1),TCBSYS                                               
         MVC   SPWORK+0(1),TCBSYS                                               
         CLI   TCBSYS,1                                                         
         BNH   *+10                                                             
         MVC   SPWORK+0(1),TCBSWSYS                                             
         MVC   SPWORK+1(1),TCBPRG                                               
         BRAS  RE,PGMCHASE         GO EXPAND IT                                 
         MVC   SCDSYSPR,WORK                                                    
*                                                                               
         TM    TCBFLAG1,TCBENQPQ   PQ ENQUEUED?                                 
         BZ    *+10                NO                                           
         MVC   SCDENQPQ,TCBPQCHR                                                
         TM    TCBFLAG1,TCBENQWK   WK ENQUEUED?                                 
         BZ    *+10                NO                                           
         MVC   SCDENQWK,TCBWKCHR                                                
         TM    TCBFLAG2,TCBENQFW   FW ENQUEUED?                                 
         BZ    *+10                NO                                           
         MVC   SCDENQFW,TCBFWCHR                                                
         TM    TCBFLAG2,TCBENQWF   WF ENQUEUED?                                 
         BZ    *+10                NO                                           
         MVC   SCDENQWF,TCBWFCHR                                                
         TM    TCBFLAG2,TCBENQCT   CT ENQUEUED?                                 
         BZ    *+8                 NO                                           
         MVI   SCDENQCT,C'Y'                                                    
         TM    TCBFLAG2,TCBENQEW   EZ ENQUEUED?                                 
         BZ    *+8                 NO                                           
         MVI   SCDENQEZ,C'Y'                                                    
         TM    TCBFLAG2,TCBENQMZ   MZ ENQUEUED?                                 
         BZ    *+8                 NO                                           
         MVI   SCDENQMZ,C'Y'                                                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OUTPUT SYSTEM/PROGRAM                                               *         
* NTRY: SPWORK  = SYSTEM/PROGRAM XL1,XL1                              *         
* EXIT: WORK    = SYSTEM/PROGRAM AS CHARACTER STRING                  *         
***********************************************************************         
         SPACE 1                                                                
PGMCHASE NTR1  ,                                                                
         MVC   WORK(10),DOTS       ELSE RETURN DOTS                             
         CLI   SPWORK,0            IS ANY SYSTEM ACTIVE ?                       
         BE    EXIT                YES - FIND WHICH                             
*                                                                               
         GOTO1 VHEXOUT,DMCB,SPWORK,FULL,2  EXPAND SYS/PROG                      
         MVC   WORK(40),SPACES                                                  
         MVI   WORK+7,C'/'                                                      
*                                                                               
         L     R5,VSELIST          POINT TO START OF SELIST                     
         LH    R2,0(R5)            PRIME INDICES                                
         L     R3,2(R5)                                                         
         LA    R5,6(,R5)                                                        
         USING SELISTD,R5                                                       
         CLC   SESYS,SPWORK        MATCH SYSTEM                                 
         BE    PC02                                                             
         BXLE  R5,R2,*-10                                                       
         MVC   WORK(2),FULL        MOVE X'SS' TO SSSSSSS AREA                   
         B     PC06                                                             
*                                                                               
PC02     LA    R2,SENAME+L'SENAME-1                                             
         CLI   0(R2),C' '                                                       
         BNE   *+8                 FIND FIRST NON-BLANK                         
         BCT   R2,*-8                                                           
*                                                                               
         CLI   0(R2),C'0'          IS LAST CHAR NUMERIC ?                       
         BL    *+14                YES - SKIP                                   
         MVC   WORK(7),SENAME      MOVE WHOLE SENAME                            
         B     PC04                                                             
*                                                                               
         MVC   WORK(4),SENAME      MOVE ONLY FIRST FOUR CHARS                   
*                                                                               
PC04     L     R4,SEPGMS           PROGRAM NAME LIST FOR THIS SYSTEM            
         USING PGMLSTD,R4                                                       
         LH    R2,0(R4)                                                         
         L     R3,2(R4)                                                         
         LA    R4,6(R4)                                                         
         CLC   PGMNUM,SPWORK+1     MATCH PROGRAM                                
         BE    PC08                                                             
         BXLE  R4,R2,*-10                                                       
*                                                                               
PC06     MVC   WORK+8(2),FULL+2    MOVE X'PP' TO PPPPPPP AREA                   
         B     PC10                                                             
*                                                                               
PC08     MVC   WORK+8(7),PGMNAME                                                
         DROP  R4,R5                                                            
*                                                                               
PC10     LA    RE,WORK             COMPRESS ALL BLANKS FROM WORK                
         LA    RF,WORK+1                                                        
         LHI   R0,15                                                            
*                                                                               
PC12     CLI   0(RE),C' '          SAVE WORK ADDRESS                            
         BNE   *+14                                                             
         MVC   0(20,RE),0(RF)                                                   
         B     PC14                                                             
*                                                                               
         AHI   RE,1                                                             
         AHI   RF,1                                                             
*                                                                               
PC14     BCT   R0,PC12                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETERS                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALPARMS NTR1  ,                                                                
         NI    SRVSRVH+6,X'BF'     SET OFF IC ORDER IN SRV INP FIELD            
         OI    SRVP1H+6,X'40'      PUT CURSOR UNDER TASK ID FIELD               
*                                                                               
         LA    R2,SRVP1H           STARTING TASK ID                             
         USING FHD,R2                                                           
         CLI   FHIL,0              INPUT?                                       
         BNE   VP102               YES                                          
         MVI   FHIL,1              SET DEFAULTS                                 
         MVI   FHDA,C'1'                                                        
         B     VP104                                                            
*                                                                               
VP102    CLI   FHIL,1              JUST TASK # INPUT?                           
         BE    VP104               YES                                          
*                                                                               
         CLI   FHIL,6              KILL,T                                       
         BNE   VALP1A                                                           
         CLC   FHDA(5),=C'KILL,'                                                
         BNE   VALP1A                                                           
         MVC   TASKSTRT,FHDA+5                                                  
         BAS   RE,KILLTASK         KILL THIS TASK                               
         MVC   FHDA(6),=C'1     '                                               
         B     VP104                                                            
*                                                                               
VALP1A   CLI   FHIL,8              MUST BE 7 OR 8                               
         BH    ERR1                                                             
         CLI   FHIL,7              TO BE TASKS=NN                               
         BL    ERR1                                                             
         CLC   =CL6'TASKS=',FHDA                                                
         BNE   ERR1                                                             
*                                                                               
         SR    R3,R3               GET LEN OF NUMBER INTO R3                    
         IC    R3,5(R2)                                                         
         SH    R3,=H'6'            LEN IS FIELD LEN-6                           
         LA    R4,14(R2)                                                        
         BAS   RE,VALNUM           VALIDATE TASK NUMBER                         
         CLI   DUB,X'FF'                                                        
         BE    ERR1                INVALID TASK NUMBER                          
         CVB   R1,DUB                                                           
         LA    R1,1(R1)                                                         
         STH   R1,LASTTASK                                                      
         BAS   RE,SETNOP           GO NOP THIS TASK                             
         MVC   FHDA,=CL8'1'                                                     
         MVI   FHIL,1                                                           
*                                                                               
VP104    MVC   TASKSTRT,FHDA       SAVE TASK ID                                 
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
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SETNOP - SET NOP POINT IN TCB OF LASTTASK                           *         
***********************************************************************         
         SPACE 1                                                                
SETNOP   NTR1                                                                   
         L     R8,VTCB             POINT TO TCB TABLES                          
         LH    R2,0(R8)            LENGTH TCB ENTRY                             
         L     R3,2(R8)            LAST ENTRY ADDRESS                           
         LA    R8,6(R8)            FIRST ENTRY ADDRESS                          
         LA    R1,1                START TASK 1                                 
         USING TCBD,R8                                                          
*                                                                               
SETNOP1  CH    R1,LASTTASK         IS THIS REQUIRED TASK                        
         BE    SETNOP2             YES - GO TO IT                               
         AHI   R1,1                                                             
         BXLE  R8,R2,SETNOP1       LOOK AT NEXT TCB                             
         CH    R1,LASTTASK         ARE WE RESTORING MAX TASKS                   
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
SETNOP4  CH    R1,LASTTASK             IS THIS REQUIRED TASK                    
         BE    *+8                                                              
         NI    TCBFLAG2,255-TCBNOPED   REMOVE ANY OTHER NOPS                    
         AHI   R1,1                                                             
         BXLE  R8,R2,SETNOP4           LOOK AT NEXT TCB                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SETNOP - SET NOP POINT IN TCB OF LASTTASK                           *         
***********************************************************************         
         SPACE 1                                                                
KILLTASK NTR1                                                                   
         L     R8,VTCB             POINT TO TCB TABLES                          
         LH    R2,0(R8)            LENGTH TCB ENTRY                             
         L     R3,2(R8)            LAST ENTRY ADDRESS                           
         LA    R8,6(R8)            FIRST ENTRY ADDRESS                          
         USING TCBD,R8                                                          
*                                                                               
KILLT1   CLC   TCBID+6(1),TASKSTRT                                              
         BE    KILLT2              YES - GO TO IT                               
         BXLE  R8,R2,KILLT1        LOOK AT NEXT TCB                             
         B     ERR5                REQUESTED TCBID NOT FOUND                    
*                                                                               
KILLT2   OI    TCBFLAG3,TCBKILL    FLAG THIS FOR FORCED ABEND                   
*                                                                               
*        REMOVE ANY OBSTACLES TO A CERTAIN FORCED ABEND                         
*                                                                               
         NI    TCBINDS1,255-TCBABPEN   CLEAR ABEND PENDING                      
         NI    TCBFLAG3,255-TCBDEBUG   CLEAR DEBUG MODE?                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         L     R2,ACOMFACS                                                      
         USING COMFACSD,R2         R2=A(COMFACS)                                
         MVC   VHEXOUT,CHEXOUT     SAVE USEFUL ENTRY POINTS                     
         DROP  R2                  FINISHED WITH COMFACS                        
*                                                                               
         TIME  TU                                                               
         ST    R0,TIMENOW          TIME IN MVS TU'S (1/38400 SEC)               
*                                                                               
         L     R2,VSSB             R2=A(SSB)                                    
         USING SSBD,R2                                                          
         MVC   SYSNAME(4),SSBSYSN4 ADV NAME                                     
         LLC   R0,SSBSYSIX         AORNUM/ADVNUM                                
         CLI   SYSNAME+3,C' '                                                   
         BNE   *+8                                                              
         MVI   SYSNAME+3,C'/'                                                   
         SRL   R0,4                                                             
         STC   R0,TORAOR                                                        
         LTR   R0,R0                                                            
         BNZ   *+12                                                             
         LHI   R0,X'A3'            SET TOR                                      
         B     *+8                                                              
         AHI   R0,X'80'            SET AOR LETTER                               
         STC   R0,SYSNAME+4                                                     
*                                                                               
         LA    R1,MINTAB                                                        
INIT01   CLC   SSBSYSNA,0(R1)      FIND MINIMUM TASK NUMBER                     
         BE    INIT02                                                           
         AHI   R1,5                                                             
         CLI   0(R1),C' '                                                       
         BNE   INIT01                                                           
*                                                                               
INIT02   MVC   MINTASK,3(R1)       SET MINIMUM NUMBER                           
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
ERR1     MVC   SRVMSG+14(L'MSG1),MSG1                                           
         B     ERRX                                                             
ERR2     MVC   SRVMSG+14(L'MSG2),MSG2                                           
         B     ERRX                                                             
ERR3     MVC   SRVMSG+14(L'MSG3),MSG3                                           
         B     ERRX                                                             
ERR4     MVC   SRVMSG+14(L'MSG4),MSG4                                           
         B     ERRX                                                             
ERR5     MVC   SRVMSG+14(L'MSG5),MSG5                                           
         B     ERRX                                                             
*                                                                               
ERRX     MVC   SRVMSG(14),=C'ED/9999 XXXXX '                                    
         MVC   SRVMSG+8(5),SYSNAME                                              
         B     XMOD                                                             
*                                                                               
       ++INCLUDE DDVALNUM                                                       
*                                                                               
EXITPAGE MVC   SRVMSG+L'MSG0(L'MSG0P),MSG0P                                     
         B     EXITCOMM                                                         
EXITNEXT MVC   SRVMSG+L'MSG0(L'MSG0N),MSG0N                                     
*                                                                               
EXITCOMM MVC   SRVMSG(L'MSG0),MSG0                                              
         MVC   SRVMSG(5),SYSNAME                                                
         CLI   TORAOR,0            GET INPUT QUEUE LENS IF TOR                  
         BNE   XMOD                                                             
         BRAS  RE,BLDQUE                                                        
         OC    HALF,HALF                                                        
         BZ    XMOD                                                             
         MVC   SRVMSG+6(50),WRK                                                 
*                                                                               
XMOD     L     RD,SAVERD                                                        
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* BUILD LIST OF SELIST ENTRIES WITH INPUT QUEUES                      *         
* EXIT: WRK    = LIST (SS=NN,SS=NN)                                   *         
*       HALF   = L'LIST IN WORK                                       *         
***********************************************************************         
BLDQUE   NTR1  ,                                                                
         XC    WRK,WRK                                                          
         XC    HALF,HALF                                                        
*                                                                               
         L     R2,VSELIST                                                       
         LH    R4,0(R2)                                                         
         L     R5,2(R2)                                                         
         AHI   R2,6                                                             
         USING SELISTD,R2                                                       
         LA    R3,WRK                                                           
*                                                                               
BQUE02   OC    SEQLEN,SEQLEN                                                    
         BZ    BQUE07                                                           
*                                                                               
         GOTO1 VHEXOUT,DMCB,SESYS,0(R3),1,0                                     
         MVI   2(R3),C'='                                                       
         AHI   R3,3                                                             
*                                                                               
         LH    R0,SEQLEN                                                        
         CHI   R0,99                                                            
         BNH   BQUE04                                                           
         MVC   0(2,R3),=CL3'??'                                                 
         AHI   R3,2                                                             
         B     BQUE06                                                           
*                                                                               
BQUE04   EDIT  (R0),(2,0(R3)),ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R3,R0                                                            
*                                                                               
BQUE06   MVI   0(R3),C','                                                       
         AHI   R3,1                                                             
*                                                                               
         LA    R0,WRK+L'WRK-1                                                   
         CR    R3,R0                                                            
         BNL   BQUE08                                                           
*                                                                               
BQUE07   BXLE  R2,R4,BQUE02                                                     
         LA    R0,WRK                                                           
         CR    R3,R0                                                            
         BE    EXIT                                                             
*                                                                               
BQUE08   BCTR  R3,0                                                             
         MVI   0(R3),C' '                                                       
         SR    R3,R0                                                            
         STH   R3,HALF                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
DOTS     DC    10C'.'                                                           
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
MSG0     DC    C'XXXXX TASK INFO DISPLAYED'                                     
MSG0P    DC    C' - ENTER TO PAGE'                                              
MSG0N    DC    C'             '                                                 
MSG1     DC    C'INVALID TASK ID'                                               
MSG2     DC    C'INVALID FILTER'                                                
MSG3     DC    C'START TASK NOT FOUND'                                          
MSG4     DC    C'NUMBER OF TASKS LESS THAN MINIMUM'                             
MSG5     DC    C'TASK NOT FOUND'                                                
*&&UK                                                                           
MINTAB   DC    C'AD1',AL2(16)      MINIMUM TASK VALUE                           
         DC    C'AD2',AL2(3)                                                    
         DC    C'AD3',AL2(3)                                                    
         DC    C'NEW',AL2(3)                                                    
         DC    C'TST',AL2(3)                                                    
         DC    C'TTS',AL2(3)                                                    
         DC    C'   ',AL2(16)      DEFAULT IF NOT IN TABLE                      
*&&                                                                             
*&&US                                                                           
MINTAB   DC    C'AD1',AL2(10)      MINIMUM TASK VALUE                           
         DC    C'AD2',AL2(10)                                                   
         DC    C'AD3',AL2(10)                                                   
         DC    C'AD4',AL2(10)                                                   
         DC    C'AD5',AL2(8)                                                    
         DC    C'AD6',AL2(8)                                                    
         DC    C'AD7',AL2(10)                                                   
         DC    C'REA',AL2(8)       FACREPA                                      
         DC    C'REB',AL2(8)       FACREPB                                      
         DC    C'REC',AL2(8)       FACREPC                                      
         DC    C'MEL',AL2(3)       FACMEL                                       
         DC    C'TST',AL2(3)       FACTEST                                      
         DC    C'DAE',AL2(3)       FACDARE                                      
         DC    C'FRD',AL2(3)       FACFRED                                      
         DC    C'Y2K',AL2(3)       FACY2K                                       
         DC    C'   ',AL2(10)      DEFAULT IF NOT IN TABLE                      
*&&                                                                             
SPACES   DC    80C' '                                                           
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
IPARMS   DS    0XL32                                                            
ASYSFACS DS    A                   A(SYSTEM FACILITIES TABLE)                   
ATIA     DS    A                   A(TIA)                                       
AUTL     DS    A                   A(UTL ENTRY)                                 
ACOMFACS DS    A                   A(COMMON FACILITIES TABLE)                   
ASELIST  DS    A                   A(SELIST ENTRY)                              
ATWA     DS    A                   A(TWA)                                       
AMAP     DS    A                   A(PHASE MAP)                                 
ATIOB    DS    A                   A(TRANSLATOR INPUT OUTPUT BLOCK)             
*                                                                               
DUB      DS    D                                                                
SAVERD   DS    A                                                                
SAVER1   DS    A                                                                
RELO     DS    A                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
SPWORK   DS    CL2                                                              
SAVERE   DS    F                                                                
DMCB     DS    6F                                                               
VHEXOUT  DS    A                                                                
TIMENOW  DS    F                                                                
QLEN     DS    F                                                                
SYSNAME  DS    CL5                                                              
TORAOR   DS    X                   ZERO IF TOR OR AOR NUMBER (1-7)              
TASKSTRT DS    X                   START TASK ID                                
FILTER   DS    X                   FILTER TYPE CHOSEN                           
LASTTASK DS    H                                                                
MINTASK  DS    H                                                                
WORK     DS    CL20                                                             
WRK      DS    CL256                                                            
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* SCREEN LINE DISPLAY                                                 *         
***********************************************************************         
         SPACE 1                                                                
SCDSECT  DSECT                     DEFINE SCREEN LINE                           
SCDHDR   DS    CL8                 FIELD HEADER                                 
SCDTID   DS    CL2                 TASK ID 'TN'                                 
         DS    C                                                                
SCDLUID  DS    CL8                 LUID                                         
         DS    C                                                                
SCDSYSPR DS    CL10                SYS/PROG                                     
         DS    C                                                                
SCDIO    DS    CL6                 TOTAL I/O                                    
         DS    C                                                                
SCDLIO   DS    CL6                 LOGICAL I/O                                  
         DS    C                                                                
SCDQUEUE DS    CL6                 TOTAL QUEUE TIME                             
         DS    C                                                                
SCDCPU   DS    CL6                 TOTAL CPU                                    
         DS    C                                                                
SCDTIMER DS    CL3                 TIMER INTERRUPTS                             
         DS    C                                                                
SCDTKW   DS    CL6                 TKWAIT POPS                                  
         DS    CL1                                                              
SCDELAPS DS    CL7                 ELAPSED TIME                                 
         DS    CL1                                                              
SCDENQPQ DS    CL1                 PRTQ ENQUEUED                                
SCDENQWK DS    CL1                 WRKR ENQUEUED                                
SCDENQFW DS    CL1                 FACW ENQUEUED                                
SCDENQWF DS    CL1                 WRKF ENQUEUED                                
SCDENQCT DS    CL1                 CNTL ENQUEUED                                
SCDENQEZ DS    CL1                 EAZI ENQUEUED                                
SCDENQMZ DS    CL1                 MEDZ ENQUEUED                                
SCDLEN   EQU   *-SCDSECT           SHOULD BE 78 BYTES                           
         EJECT                                                                  
***********************************************************************         
* SCREEN DSECT                                                        *         
***********************************************************************         
         SPACE 1                                                                
SRTSKFFD DSECT                                                                  
         DS    CL64                                                             
* SRTSKFFD                                                                      
       ++INCLUDE SRTSKFFD                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
* FAPGMLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAPGMLST                                                       
         PRINT ON                                                               
* FASRPARM                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASRPARM                                                       
         PRINT ON                                                               
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SRTSK00   01/19/11'                                      
         END                                                                    
