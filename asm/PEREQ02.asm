*          DATA SET PEREQ02    AT LEVEL 006 AS OF 06/19/12                      
*PHASE TE0402A                                                                  
*INCLUDE REQTWA                                                                 
         TITLE 'PEREQ02 - REQUEST - DISPLAY/UPDATE REQUEST FILE'                
         PRINT NOGEN                                                            
TE0402   CSECT                                                                  
         NMOD1 070,**RQ02**,RR=R7                                               
         USING WORKD,RC                                                         
         L     R9,0(R1)            R9=A(W/S)                                    
         USING REQTEMP,R9                                                       
         L     R3,ASAVE                                                         
         USING REQSAVE,R3          R3=A(TWA)                                    
         ST    R7,RELO                                                          
         SPACE 2                                                                
IOCTL    MVI   FERN,X'FF'                                                       
         LA    R6,BVRNAMEH                                                      
         ST    R6,FADR                                                          
         CLI   STATUS,3                                                         
         BNE   *+16                                                             
         CLI   REQOPTN,C'T'                                                     
         BNE   ENQREQ                                                           
         B     TOTREQ                                                           
         CLI   REQACTN,C'A'                                                     
         BE    CHKIT                                                            
         CLI   REQACTN,C'D'                                                     
         BE    CANREQ                                                           
         CLI   REQACTN,C'N'                                                     
         BE    CHKIT                                                            
         DC    H'0'                                                             
         SPACE 2                                                                
REQIOERR MVI   FERN,0                                                           
         SPACE 2                                                                
CLEARADR XC    LADR,LADR                                                        
         B     EXIT                                                             
         SPACE 2                                                                
SAVEADR  MVC   LADR,ADR                                                         
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*              ANY GLOBAL REQUEST POST VALIDATION GOES HERE                     
*                                                                               
CHKIT    EQU   *                                                                
         CLI   LREQMAP,126         CARD REQUEST                                 
         BE    CHKREQX             BYPASS VALIDATION                            
*        CHECK IF REQUEST FIELDS REQUIRE FURTHER VALIDATION                     
*                                                                               
CHKREQ   L     R7,AREQTBL          R7=A(REQTBL ENTRY)                           
         AH    R7,REQNDX                                                        
         SR    RF,RF               RF=FURTHER VAL ROUTINE NUM                   
         IC    RF,27(R7)                                                        
         LTR   RF,RF                                                            
         BZ    CHKREQX             NO VAL REQUIRED                              
         SLA   RF,2                                                             
         LA    RF,VALROUTS(RF)     RF=A(FURTHER VAL ROUTINE)                    
         L     RF,0(RF)                                                         
         A     RF,RELO                                                          
         BASR  RA,RF                                                            
         CLI   FERN,X'FF'                                                       
         BE    CHKREQX             ALL FIELDS OK                                
         SPACE 2                                                                
         LA    R0,24               SEARCH REQ MAP TABLE                         
         LA    R1,LREQMAP                                                       
CHKREQ1  CLI   0(R1),127                                                        
         BE    CHKREQ2                                                          
         CLC   ROUTNUM,0(R1)                                                    
         BE    CHKREQ3                                                          
         LA    R1,3(R1)                                                         
         BCT   R0,CHKREQ1                                                       
CHKREQ2  LA    R1,LREQMAP          NOT IN TBL POSN TO 1ST FLD                   
CHKREQ3  MVC   HALF,1(R1)                                                       
         LR    R6,R3                                                            
         AH    R6,HALF                                                          
CHKREQ4  ST    R6,FADR             POSN CURSOR TO ROUTNUM FLD                   
         B     EXIT                                                             
         SPACE 2                                                                
CHKREQX  CLI   REQACTN,C'N'                                                     
         BE    NEWREQ                                                           
         B     AMDREQ                                                           
         EJECT                                                                  
*        ROUTINES FOR FURTHER FIELD VALIDATION                                  
*                                                                               
VALR01   DS    0H                                                               
         BR    RA                                                               
         SPACE 2                                                                
*        THIS TABLE CONTAINS THE ADDRESSES OF VALIDATION ROUTINES               
*                                                                               
VALROUTS DC    A(0)                00                                           
         DC    A(VALR01)           01                                           
         EJECT                                                                  
*        READ LAST REQUEST RECORD AND UPDATE IT WITH AMENDED DATA               
*                                                                               
AMDREQ   MVC   ADR,LADR                                                         
         GOTO1 DATAMGR,DMCB,(X'80',DMRDIR),REQUEST,ADR,TEMP,,106                
         CLI   DMCB+8,0                                                         
         BNE   REQIOERR                                                         
         MVC   TEMP+26(80),REQREC+26                                            
         GOTO1 DATAMGR,DMCB,(X'00',DMWRT),REQUEST,ADR,TEMP,,106                 
         CLI   DMCB+8,0                                                         
         BNE   REQIOERR                                                         
         B     SAVEADR                                                          
         EJECT                                                                  
*        SEARCH REQUEST CHAIN AND COUNT/DISPLAY                                 
*                                                                               
ENQREQ   LA    R4,ENQFRSTH                                                      
         ST    R4,DISPADR          R4=A(NEXT TWA LINE NUM)                      
         USING DISPLD,R4                                                        
         XC    SKIPCTR(08),SKIPCTR SET COUNTERS                                 
         SPACE 2                                                                
         XC    ADR,ADR                                                          
         LA    R0,DMRDIR                                                        
         CLI   REQOPTN,C'N'                                                     
         BNE   ENQR1                                                            
         CLC   DISPFLDS(2),DISPMAX WAS THERE PREVIOUS                           
         BL    ENQRE2              NO                                           
         MVC   REQINCR,=H'2'       SET TO SKIP 1                                
         LH    R6,DISPFLDS+2       SEQ OF 1ST = LAST + 1                        
         AH    R6,DISPFLDS                                                      
         STH   R6,DISPFLDS+2                                                    
         LH    R6,DISPFLDS         SET ADR TO A(LAST)                           
         SLA   R6,2                                                             
         LA    R6,DISPFLDS(R6)                                                  
         MVC   ADR,0(R6)                                                        
         B     ENQR3                                                            
ENQR1    MVC   DISPFLDS+2(2),REQINCR         SEQ OF 1ST = INPUT VALUE           
         CLI   REQNUM,255                                                       
         BNE   *+12                                                             
         LA    R0,DMRSEQ                                                        
         B     ENQR3                                                            
         MVC   ADR,=X'000000FF'                                                 
         MVC   ADR(2),LREQREC+26                                                
         B     ENQR3                                                            
         SPACE 2                                                                
ENQR2    LA    R0,DMRSEQ                                                        
         CLI   REQNUM,255                                                       
         BE    ENQR3                                                            
         MVC   ADR,RHDR+16                                                      
         LA    R0,DMRDIR                                                        
ENQR3    GOTO1 DATAMGR,DMCB,(X'00',(R0)),REQUEST,ADR,REQREC,FILREC              
         CLI   DMCB+8,0                                                         
         BE    ENQR4                                                            
         TM    DMCB+8,X'80'                                                     
         BO    ENQREOF                                                          
         B     REQIOERR                                                         
         SPACE 2                                                                
ENQR4    CLI   REQNUM,255          FILTER OUT CANCELLED FROM ALL OPTION         
         BNE   ENQR4A                                                           
         CLC   RNUM,=X'FFFF'                                                    
         BE    ENQR2                                                            
         CLC   RNUM,=C'99'                                                      
         BE    ENQR2                                                            
         TM    REQFLAG,X'01'       FILTER OUT UNLINKED FROM ALL OPTION          
         BZ    ENQR2                                                            
         B     ENQR4B                                                           
ENQR4A   CLC   RNUM,LREQREC+26     FILTER OUT ANY SUNDRY REQUESTS               
         BE    *+14                                                             
         CLC   RNUM,=C'99'                                                      
         BNE   ENQR2                                                            
*                                                                               
ENQR4B   OC    LREQREC+33(2),LREQREC+33                                         
         BZ    ENQR4C                                                           
         CLC   RAGY,LREQREC+33     FILTER ON AGENCY                             
         BNE   ENQR2                                                            
*                                                                               
ENQR4C   OC    LREQREC(4),LREQREC                                               
         BZ    ENQR5                                                            
         CLC   REQOFFC,LREQREC     FILTER ON OFFICE CODE                        
         BNE   ENQR2                                                            
         SPACE 2                                                                
ENQR5    TM    REQFLTR,X'01'                                                    
         BZ    *+14                                                             
         CLC   RNAME,LREQREC+92    FILTER ON REQUESTOR NAME                     
         BNE   ENQR2                                                            
*                                                                               
ENQR5A   OC    LREQREC+4(6),LREQREC+4                                           
         BZ    ENQR5B                                                           
         CLC   REQOUT,LREQREC+4    FILTER ON OUTPUT TYPE                        
         BNE   ENQR2                                                            
*                                                                               
ENQR5B   OC    LREQREC+11(2),LREQREC+11                                         
         BZ    ENQR5C                                                           
         CLC   REQDEST,LREQREC+11  FILTER ON DESTINATION ID                     
         BNE   ENQR2                                                            
*                                                                               
ENQR5C   CLI   LST,0                                                            
         BZ    ENQR5D                                                           
         CLC   LSTNUM,RLIST        FILTER ON LIST NUMBER                        
         BNE   ENQR2                                                            
*                                                                               
ENQR5D   EQU   *                                                                
         SPACE 2                                                                
ENQR6    LH    R4,REQINCR          INGNORE (REQINCR-1) REQS                     
         BCTR  R4,0                                                             
         CH    R4,SKIPCTR                                                       
         BE    ENQR7                                                            
         LH    R4,SKIPCTR                                                       
         LA    R4,1(R4)                                                         
         STH   R4,SKIPCTR                                                       
         B     ENQR2                                                            
         SPACE 2                                                                
ENQR7    LH    R5,READCTR          UPDATE REQ READ COUNTER                      
         LA    R5,1(R5)                                                         
         STH   R5,READCTR                                                       
         MVC   TEMP(106),REQREC    SAVE REQ REC                                 
         CLC   RNUM,=C'99'                                                      
         BNE   *+16                                                             
         LH    R5,CANCCTR          UPDATE CANCELLED COUNTER                     
         LA    R5,1(R5)                                                         
         STH   R5,CANCCTR                                                       
         CLI   REQOPTN,C'L'        DISPLAY LAST OPTION                          
         BE    ENQR2               YES DO NOT DISPLAY                           
         SPACE 2                                                                
ENQR8    BAS   RE,ENQDISP          DISPLAY REQUEST                              
         CLC   DISPCTR,DISPMAX     END OF SCREEN                                
         BNE   ENQR2               NO BACK FOR NEXT                             
         B     ENQRE3                                                           
         SPACE 2                                                                
ENQREOF  OC    READCTR,READCTR                                                  
         BZ    ENQRE2              NO REQUESTS FOUND                            
         CLI   REQOPTN,C'L'                                                     
         BE    ENQRE1                                                           
         OC    DISPCTR,DISPCTR                                                  
         BZ    ENQRE2              NO REQUESTS DISPLAYED                        
         B     ENQRE3                                                           
         SPACE 2                                                                
ENQRE1   MVC   REQREC(106),TEMP    DISPLAY LAST REQUEST OPTION                  
         GOTO1 ENQDISP             DISPLAY THE LAST REQ                         
         XC    TEMP(60),TEMP                                                    
         MVC   TEMP(24),=C'REQUEST TOTAL = NNN LIVE'                            
         MVC   TEMP+24(33),=C' - LAST REQUEST NUM NNN DISPLAYED'                
         LH    R6,READCTR                                                       
         SH    R6,CANCCTR                                                       
         CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   TEMP+16(3),DUB                                                   
         LH    R6,READCTR                                                       
         CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   TEMP+44(3),DUB                                                   
         XC    DISPFLDS(2),DISPFLDS                                             
         B     ENQRX                                                            
         SPACE 2                                                                
ENQRE2   XC    TEMP(60),TEMP                                                    
         MVC   TEMP(18),=C'REQUESTS NOT FOUND'                                  
         XC    DISPFLDS(2),DISPFLDS                                             
         B     ENQRX                                                            
         SPACE 2                                                                
ENQRE3   XC    TEMP(60),TEMP                                                    
         MVC   TEMP(31),=C'REQUESTS NNN THRU NNN DISPLAYED'                     
         MVC   TEMP+31(25),=C' - CHANGE CANCEL STATUS ?'                        
         LH    R6,DISPFLDS+2       GET FIRST                                    
         CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   TEMP+09(3),DUB                                                   
         AH    R6,DISPFLDS         GET LAST = FIRST+TOTAL-1                     
         BCTR  R6,0                                                             
         CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   TEMP+18(3),DUB                                                   
         MVI   STATUS,1            SET STATUS FOR INPUT                         
         B     ENQRX                                                            
         SPACE 2                                                                
ENQRX    L     R4,DISPADR          CLEAR REST OF SCREEN                         
         LH    R6,DISPMAX                                                       
         SH    R6,DISPCTR                                                       
         BZ    ENQRX4                                                           
ENQRX1   XC    DLINE,DLINE         CLEAR & TRANSMIT NONE-EMPTY LINES            
         OI    DLHDR+6,X'80'                                                    
         B     ENQRX3                                                           
ENQRX2   NI    DLHDR+6,X'7F'       DONT TRANSMIT EMPTY LINES                    
ENQRX3   LA    R4,86(R4)                                                        
         BCT   R6,ENQRX1                                                        
         SPACE 2                                                                
ENQRX4   MVC   BVRHDR,TEMP         SET HDR MSG                                  
         MVI   FERN,X'FF'                                                       
         B     EXIT                                                             
         EJECT                                                                  
*        ROUTINE TO FORMAT REQUEST DATA IN SCREEN DISPLAY LINE                  
*                                                                               
ENQDISP  NTR1                                                                   
         L     R4,DISPADR          R4=A(NEXT SCR DISP LINE)                     
         SPACE 2                                                                
         XC    DLINE,DLINE         DISPLAY CANCELLED FLAG                       
         MVI   DLCANC,C' '                                                      
         MVC   DLNUM,RNUM                                                       
         CLC   RNUM,=C'99'                                                      
         BNE   ENQD0                                                            
         MVI   DLCANC,C'C'                                                      
         MVC   DUB(1),REQNUMB                                                   
         BAS   RE,GETREQID                                                      
         MVC   DLNUM,DUB+1                                                      
         SPACE 2                                                                
ENQD0    CLI   REQNUM,255          DONT DISPLAY REQ ID FOR SPECIFICS            
         BE    *+10                                                             
         MVC   DLNUM,=C'  '                                                     
         MVC   DLSORT,RSORT                                                     
         MVC   DLAGY(59),RAGY                                                   
         MVC   DLNAME,RNAME                                                     
         SPACE 2                                                                
         OI    DLHDR+6,X'80'                                                    
         CLI   STATUS,3                                                         
         BNE   ENQDISPX                                                         
         SPACE 2                                                                
ENQD1    LA    R4,86(R4)           UPDATE DISPLAY ADR                           
         ST    R4,DISPADR                                                       
         LH    R6,DISPCTR          UPDATE DISPLAY COUNTER                       
         LA    R6,1(R6)                                                         
         STH   R6,DISPCTR                                                       
         STH   R6,DISPFLDS                                                      
         SLA   R6,2                                                             
         LA    R6,DISPFLDS(R6)                                                  
         MVC   0(4,R6),ADR         SAVE DISK ADR                                
         B     ENQDISPX                                                         
         SPACE 2                                                                
ENQDISPX XIT1                                                                   
         SPACE 2                                                                
*        CONVERT BINARY REQNUM IN DUB(1) TO ALPHA IN DUB+1(2)                   
*                                                                               
GETREQID NTR1                                                                   
         SR    R0,R0                                                            
         IC    R0,DUB                                                           
         CVD   R0,DUB                                                           
         STC   R0,DUB                                                           
         UNPK  DUB+1(2),DUB+6(2)                                                
         OI    DUB+2,X'F0'                                                      
         MVI   DUB+3,0             SET NOT FOUND FLAG AND NUMBER VALUE          
         SR    R1,R1                                                            
         L     R7,AREQTBL                                                       
         SPACE 2                                                                
GETRID1  CLI   0(R7),0             SEARCH REQTBL FOR BINARY REQ NUM             
         BE    GETRIDX                                                          
         CLC   DUB(1),1(R7)                                                     
         BE    GETRID2                                                          
         IC    R1,0(R7)                                                         
         AR    R7,R1                                                            
         B     GETRID1                                                          
         SPACE 2                                                                
GETRID2  ST    R7,DUB+4            SET FOUND ADR AND FLAG                       
         MVI   DUB+3,1                                                          
         IC    R1,0(R7)            POINT TO LAST TWO BYTES OF ENTRY             
         AR    R7,R1                                                            
         SH    R7,=H'2'                                                         
         MVC   DUB+1(2),0(R7)      RETURN REQ ALPHA ID                          
         SPACE 2                                                                
GETRIDX  XIT1                                                                   
         EJECT                                                                  
*        CHANGE REQUEST CANCEL STATUS                                           
*                                                                               
CANREQ   LA    R4,ENQFRSTH         R4=A(NEXT TWA LINE NUM)                      
         USING DISPLD,R4                                                        
         ST    R4,DISPADR                                                       
         LH    R5,DISPFLDS         R5=NUM OF TWA LINES                          
         LA    R6,DISPFLDS+4       R6=A(DISK ADR)                               
         LTR   R5,R5                                                            
         BNZ   CANR1                                                            
         DC    H'0'                                                             
         SPACE 2                                                                
CANR1    TM    DLHDR+4,X'C0'       ANY INPUT IN CANCEL FIELD                    
         BZ    CANR6                                                            
         MVC   ADR,0(R6)                                                        
         GOTO1 DATAMGR,DMCB,(X'80',DMRDIR),REQUEST,ADR,REQREC                   
         CLI   DMCB+8,0                                                         
         BNE   CANR5                                                            
*                                                                               
         CLI   DLCANC,0                                                         
         BE    *+12                                                             
         CLI   DLCANC,C' '                                                      
         BNE   CANR1B                                                           
CANR1A   CLC   RNUM,=C'99'         UNCANCELL REQUIRED                           
         BNE   CANR6                                                            
         MVC   DUB(1),REQNUMB                                                   
         BAS   RE,GETREQID                                                      
         MVC   RNUM,DUB+1                                                       
         MVI   DLCANC,C' '                                                      
         B     CANR4                                                            
*                                                                               
CANR1B   CLI   DLCANC,C'U'                                                      
         BNE   *+18                                                             
         CLC   RNUM,=C'99'         ALLOW EXPLICIT UNCANCEL                      
         BE    CANR1A                                                           
         B     CANR1C                                                           
         CLI   DLCANC,C'A'                                                      
         BE    CANR3                                                            
         CLI   DLCANC,C'C'                                                      
         BNE   CANR1C                                                           
         CLC   RNUM,=C'99'         CANCELL REQUIRED                             
         BE    CANR6                                                            
         MVC   RNUM,=C'99'                                                      
         B     CANR4                                                            
*                                                                               
CANR1C   MVI   FERN,2              INVALID CODE                                 
         ST    R4,FADR                                                          
         B     EXIT                                                             
         SPACE 2                                                                
CANR3    CLI   DDS,1               ONLY DDS CAN AMEND                           
         BNE   CANR1C                                                           
         CLC   RNUM,=C'99'         CAN ONLY AMEND ACTIVE REQ                    
         BE    CANR1C                                                           
         CLI   5(R4),78                                                         
         BE    CANR3B                                                           
         SR    R1,R1               REDISPLAY IF TRUNC INPUT                     
         IC    R1,5(R4)                                                         
         SH    R1,=H'1'                                                         
         BM    CANR3A                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMP(0),8(R4)                                                    
CANR3A   BAS   RE,ENQDISP                                                       
         LTR   R1,R1                                                            
         BM    *+18                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),TEMP                                                     
CANR3B   MVC   RNUM+2(64),DLNUM+2                                               
         MVC   RNAME(L'DLNAME),DLNAME                                           
         B     CANR4                                                            
         SPACE 2                                                                
CANR4    GOTO1 DATAMGR,DMCB,(X'00',DMWRT),REQUEST,ADR,REQREC                    
         CLI   DMCB+8,0                                                         
         BNE   CANR5                                                            
         BAS   RE,ENQDISP                                                       
         B     CANR6                                                            
         SPACE 2                                                                
CANR5    MVI   FERN,0              DISK ERROR ON UPDATE                         
         ST    R4,FADR                                                          
         B     EXIT                                                             
         SPACE 2                                                                
CANR6    LA    R4,86(R4)           BUMP TO NEXT LINE                            
         ST    R4,DISPADR                                                       
         LA    R6,4(R6)            BUMP TO NEXT DISK ADR                        
         BCT   R5,CANR1                                                         
         SPACE 2                                                                
CANRX    XC    TEMP,TEMP                                                        
         MVC   TEMP(29),=C'REQUEST CANCEL STATUS AMENDED'                       
         B     ENQRX4                                                           
         EJECT                                                                  
*        DISPLAY TOTAL REQUEST COUNTS                                           
*                                                                               
TOTREQ   XC    TOTCTR(256),TOTCTR                                               
         XC    TOTCTR+256(256),TOTCTR+256                                       
         XC    ADR,ADR                                                          
         GOTO1 ,DMCB,(X'FF',DMRSEQ),REQUEST,ADR,REQREC,FILREC                   
         SPACE 2                                                                
TOTR1    GOTO1 DATAMGR,DMCB,,,,,FILREC        READ NEXT REQ REC                 
         CLI   DMCB+8,0                                                         
         BE    TOTR2                                                            
         TM    DMCB+8,X'80'                                                     
         BO    TOTR3                                                            
         B     REQIOERR                                                         
         SPACE 2                                                                
TOTR2    CLC   RNUM,=X'FFFF'       IGNORE DUMMYS                                
         BE    TOTR1                                                            
         CLC   RNUM,=C'99'         IGNORE CANCELLED                             
         BE    TOTR1                                                            
*                                                                               
         OC    LREQREC+33(2),LREQREC+33                                         
         BZ    *+14                                                             
         CLC   RAGY,LREQREC+33     FILTER ON AGENCY                             
         BNE   TOTR1                                                            
*                                                                               
         OC    LREQREC(4),LREQREC                                               
         BZ    *+14                                                             
         CLC   REQOFFC,LREQREC     FILTER ON OFFICE                             
         BNE   TOTR1                                                            
*                                                                               
         TM    REQFLTR,X'01'                                                    
         BZ    *+14                                                             
         CLC   RNAME,LREQREC+92    FILTER ON REQUESTOR                          
         BNE   TOTR1                                                            
*                                                                               
         OC    LREQREC+4(6),LREQREC+4                                           
         BZ    *+14                                                             
         CLC   REQOUT,LREQREC+4    FILTER ON OUTPUT TYPE                        
         BNE   TOTR1                                                            
*                                                                               
         OC    LREQREC+11(2),LREQREC+11                                         
         BZ    *+14                                                             
         CLC   REQDEST,LREQREC+11  FILTER ON DESTINATION                        
         BNE   TOTR1                                                            
*                                                                               
         CLI   LST,0                                                            
         BE    *+14                                                             
         CLC   LSTNUM,RLIST        FILTER ON LIST                               
         BNE   TOTR1                                                            
*                                                                               
         SR    RE,RE                                                            
         IC    RE,REQNUMB                                                       
         SLL   RE,1                                                             
         LA    RF,TOTCTR(RE)       POINT TO COUNTER AND BUMP                    
         LH    R1,0(RF)                                                         
         LA    R1,1(R1)                                                         
         STH   R1,0(RF)                                                         
         B     TOTR1                                                            
         SPACE 2                                                                
TOTR3    SR    R5,R5               DISPLAY COUNTERS ON MENU SCREEN              
         LA    R6,TOTCTR                                                        
TOTR4    CH    R5,=H'256'                                                       
         BE    TOTRX                                                            
         STC   R5,DUB                                                           
         BAS   RE,GETREQID         SEARCH REQTBL FOR BINARY REQ NUM             
         CLI   DUB+3,0                                                          
         BE    TOTR7               NOT IN REQTBL                                
         SPACE 2                                                                
         LA    RE,BVRTABH                                                       
         SR    RF,RF                                                            
TOTR5    CLI   0(RE),0             SEARCH SCREEN FOR ALPHA REQ ID               
         BE    TOTR7                                                            
         CLC   8(2,RE),DUB+1                                                    
         BNE   TOTR6                                                            
         LH    R0,0(R6)            MOVE COUNT TO SCREEN FIELD                   
         CVD   R0,DUB                                                           
         UNPK  10(4,RE),DUB+5(3)                                                
         OI    13(RE),X'F0'                                                     
         CLI   10(RE),C'0'                                                      
         BNE   *+8                                                              
         MVI   10(RE),C'='                                                      
         LTR   R0,R0                                                            
         BNZ   TOTR7                                                            
         MVC   11(3,RE),=C'...'                                                 
         B     TOTR7                                                            
TOTR6    IC    RF,0(RE)            BUMP SCREEN FIELD                            
         AR    RE,RF                                                            
         B     TOTR5                                                            
         SPACE 2                                                                
TOTR7    LA    R5,1(R5)            BUMP REQNUM                                  
         LA    R6,2(R6)            BUMP TABLE                                   
         B     TOTR4                                                            
         SPACE 2                                                                
TOTRX    XC    TEMP(60),TEMP                                                    
         MVC   TEMP(30),=C'TOTAL REQUEST COUNTS DISPLAYED'                      
         B     ENQRX4                                                           
         EJECT                                                                  
*        ADD A NEW REQUEST RECORD TO THE REQUEST CHAIN                          
*                                                                               
NEWREQ   TM    REQFLAG,X'02'       TEST SOON REQUEST                            
*NOP*    TM    REQFLAG,X'06'       TEST SOON/LATE REQUEST                       
         BNZ   SOON                                                             
         MVI   REQFLAG,X'01'       SET CHAIN FLAG                               
NEWADD   GOTO1 DATAMGR,DMCB,(000,DMADD),REQUEST,ADR,REQREC,,106                 
         CLI   DMCB+8,0                                                         
         BNE   REQIOERR                                                         
         B     SAVEADR                                                          
MULADD   GOTO1 DATAMGR,DMCB,(000,DMADD),REQUEST,ADR,REQREC,,106                 
         CLI   DMCB+8,0                                                         
         BNE   REQIOERR                                                         
         BR    RA                                                               
         SPACE 2                                                                
SOON     LA    R2,TEMP             BUILD SPOOK FOR SOON/LATE                    
         USING SPOOK,R2                                                         
         XC    TEMP(SPOOKL),TEMP                                                
         MVC   SPOOKUID,TWAUSRID                                                
         MVC   SPOOKTID,TWATRM                                                  
         MVI   SPOOKSEN,X'0E'                                                   
         MVI   SPOOKERN,X'0E'                                                   
         MVC   SPOOKAGY,TWAAGY                                                  
         MVC   SPOOKAGX,AGYB                                                    
         MVC   SPOOKDID,BVRNAME                                                 
         MVC   SPOOKSYS,=C'PE'                                                  
         MVI   SPOOKWEN,2          2 IS SOON REQUEST                            
*NOP*    TM    REQFLAG,X'04'                                                    
*NOP*    BZ    *+8                                                              
*NOP*    MVI   SPOOKWEN,7          7 IS LATE REQUEST                            
         L     RF,=V(REQTWA)                                                    
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(5,(R3)),REQREC,DATAMGR,ACOMFACS,(R2)                  
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(38),=C'Report XXX,9999 will be processed soon'            
*NOP*    TM    REQFLAG,X'04'                                                    
*NOP*    BZ    *+10                                                             
*NOP*    MVC   BVRHDR+34(4),=C'Late'                                            
         L     RE,8(R1)                                                         
         CLI   8(R1),X'FE'                                                      
         BE    ERRTQU              GETS MESSAGE AND RETURNS TO NEWREQX          
         CLI   8(R1),X'FF'                                                      
         BE    ERRPQU              GETS MESSAGE AND RETURNS TO NEWREQX          
         OC    0(7,RE),0(RE)                                                    
         BZ    ERRJCL              GETS MESSAGE AND RETURNS TO NEWREQX          
         MVC   BVRHDR+7(3),2(RE)                                                
         LH    RF,6(RE)                                                         
         LA    R4,BVRHDR+11                                                     
         EDIT  (RF),(4,(R4)),ALIGN=LEFT,WRK=TEMP+60                             
NEWREQX  MVI   FERN,X'FE'                                                       
         B     EXIT                                                             
*                                                                               
ERRTQU   MVC   BVRHDR(TQUMSGL),TQUMSG      TERMINAL QUEUE FULL                  
         XC    BVRHDR+TQUMSGL(L'BVRHDR-TQUMSGL),BVRHDR+TQUMSGL                  
         B     NEWREQX                                                          
ERRPQU   MVC   BVRHDR(PQUMSGL),PQUMSG      PRINT QUEUE FULL                     
         XC    BVRHDR+PQUMSGL(L'BVRHDR-PQUMSGL),BVRHDR+PQUMSGL                  
         B     NEWREQX                                                          
ERRJCL   MVC   BVRHDR(JCLMSGL),JCLMSG      JCL BOOK NOT FOUND                   
         XC    BVRHDR+JCLMSGL(L'BVRHDR-JCLMSGL),BVRHDR+JCLMSGL                  
         B     NEWREQX                                                          
         EJECT                                                                  
DISPMAX  DC    H'15'                                                            
REQUEST  DC    CL8'PERREQ'                                                      
DMRDIR   DC    CL8'DMRDIR'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMADD    DC    CL8'DMADD'                                                       
DMWRT    DC    CL8'DMWRT'                                                       
*                                                                               
FLDMIS   EQU   1                                                                
FLDINV   EQU   2           INVALID FIELD                                        
*                                                                               
TQUMSG   DC    C'** Terminal queue full - cannot process request **'            
TQUMSGL  EQU   *-TQUMSG                                                         
PQUMSG   DC    C'** print queue full - please contact DDS **'                   
PQUMSGL  EQU   *-PQUMSG                                                         
JCLMSG   DC    C'** JCL book not found - please contact DDS **'                 
JCLMSGL  EQU   *-JCLMSG                                                         
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DISPADR  DS    F                   A(NEXT DISP LINE ON SCR)                     
SKIPCTR  DS    H                   NUM OF RECS SKIPPED                          
READCTR  DS    H                   NUM OF RECS READ                             
CANCCTR  DS    H                   NUM OF RECS CANCELLED                        
DISPCTR  DS    H                   NUM OF RECS DISPLAYED                        
TOTCTR   DS    256H                                                             
         EJECT                                                                  
DISPLD   DSECT                                                                  
DLHDR    DS    CL8                                                              
DLINE    DS    0CL78                                                            
DLCANC   DS    CL1                                                              
DLNUM    DS    CL2                                                              
DLSORT   DS    CL5                                                              
DLAGY    DS    CL2                                                              
         DS    CL57                                                             
DLNAME   DS    CL11                                                             
         EJECT                                                                  
*PEREQSAVE                                                                      
       ++INCLUDE PEREQSAVE                                                      
         EJECT                                                                  
         ORG   REQSAVE+64                                                       
*PEREQFFD                                                                       
       ++INCLUDE PEREQFFD                                                       
         EJECT                                                                  
         ORG   BVRTABH                                                          
*PEREQFED                                                                       
       ++INCLUDE PEREQFED                                                       
         EJECT                                                                  
*PEREQTEMP                                                                      
       ++INCLUDE PEREQTEMP                                                      
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         SPACE 2                                                                
       ++INCLUDE DDSPOOK                                                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006PEREQ02   06/19/12'                                      
         END                                                                    
