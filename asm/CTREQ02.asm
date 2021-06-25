*          DATA SET CTREQ02    AT LEVEL 004 AS OF 03/20/17                      
*PHASE TA0402A                                                                  
         TITLE 'CTREQ02 - REQUEST - DISPLAY/UPDATE REQUEST FILE'                
         PRINT NOGEN                                                            
TA0402   CSECT                                                                  
         NMOD1 070,TA0402,RR=R9                                                 
         USING WORKD,RC                                                         
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     R9,0(R1)            R9=A(W/S)                                    
         USING REQTEMP,R9                                                       
         L     R3,ASAVE                                                         
         USING REQSAVE,R3          R3=A(TWA)                                    
         EJECT                                                                  
         LR    R2,R3                                                            
         USING TA04FFD,R2                                                       
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
*                                                                               
REQIOERR MVI   FERN,0                                                           
*                                                                               
CLEARADR XC    LADR,LADR                                                        
         B     EXIT                                                             
*                                                                               
SAVEADR  MVC   LADR,ADR                                                         
*                                                                               
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
*                                                                               
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
*                                                                               
CHKREQX  CLI   REQACTN,C'N'                                                     
         BE    NEWREQ                                                           
         B     AMDREQ                                                           
         EJECT                                                                  
*        ROUTINES FOR FURTHER FIELD VALIDATION                                  
*                                                                               
VALR01   DS    0H                                                               
         BR    RA                                                               
*                                                                               
*        THIS TABLE CONTAINS THE ADDRESSES OF VALIDATION ROUTINES               
*                                                                               
VALROUTS DC    A(0)                00                                           
         DC    A(VALR01)           01                                           
         EJECT                                                                  
*        READ LAST REQUEST RECORD AND UPDATE IT WITH AMENDED DATA               
*                                                                               
AMDREQ   MVC   ADR,LADR                                                         
         GOTO1 DATAMGR,DMCB,(X'A0',DMRDIR),REQUEST,ADR,FILREC                   
         CLI   DMCB+8,0                                                         
         BNE   REQIOERR                                                         
         MVC   FILREC+80(80),REQREC                                             
         GOTO1 DATAMGR,DMCB,(X'20',DMWRT),REQUEST,ADR,FILREC                    
         CLI   DMCB+8,0                                                         
         BNE   REQIOERR                                                         
         B     SAVEADR                                                          
         EJECT                                                                  
*        SEARCH REQUEST CHAIN AND COUNT/DISPLAY                                 
*                                                                               
ENQREQ   LA    R7,BVRFRSTH                                                      
         LA    R6,BVRHDRH                                                       
         SR    R7,R6                                                            
         AR    R2,R7                                                            
         USING TA04FED,R2          R2=A(1ST FLD IN ENQ)-64                      
         LA    R4,ENQFRSTH                                                      
         ST    R4,DISPADR          R4=A(NEXT TWA LINE NUM)                      
         USING DISPLD,R4                                                        
         XC    SKIPCTR(08),SKIPCTR SET COUNTERS                                 
*                                                                               
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
         MVC   ADR(2),LREQREC+80                                                
         B     ENQR3                                                            
*                                                                               
ENQR2    LA    R0,DMRSEQ                                                        
         CLI   REQNUM,255                                                       
         BE    ENQR3                                                            
         MVC   ADR,RQHLINK                                                      
         LA    R0,DMRDIR                                                        
ENQR3    GOTO1 DATAMGR,DMCB,(X'20',(R0)),REQUEST,ADR,FILREC                     
         MVC   REQREC,FILREC                                                    
         CLI   DMCB+8,0                                                         
         BE    ENQR4                                                            
         TM    DMCB+8,X'80'                                                     
         BO    ENQREOF                                                          
         B     REQIOERR                                                         
*                                                                               
ENQR4    CLI   REQNUM,255          FILTER OUT CANCELLED FROM ALL OPTION         
         BNE   ENQR4A                                                           
         CLC   RNUM,=X'FFFF'                                                    
         BE    ENQR2                                                            
         CLC   RNUM,=C'99'                                                      
         BE    ENQR2                                                            
         TM    REQFLAG,X'01'       FILTER OUT UNLINKED FROM ALL OPTION          
         BZ    ENQR2                                                            
         B     ENQR4B                                                           
ENQR4A   CLC   RNUM,LREQREC+80     FILTER OUT ANY SUNDRY REQUESTS               
         BE    *+14                                                             
         CLC   RNUM,=C'99'                                                      
         BNE   ENQR2                                                            
*                                                                               
ENQR4B   CLI   LREQREC+82,C'*'                                                  
         B     ENQR4C              **NOP**                                      
         CLC   RAGY,LREQREC+82     FILTER ON AGENCY                             
         BNE   ENQR2                                                            
*                                                                               
ENQR4C   CLI   LREQREC+RQHOFF-RQHHDR,0                                          
         BE    ENQR5                                                            
         CLC   RQHOFF,LREQREC+RQHOFF-RQHHDR  FILTER ON OFFICE CODE              
         BNE   ENQR2                                                            
*                                                                               
ENQR5    TM    REQFLTR,X'01'                                                    
         BZ    *+14                                                             
         CLC   RNAME,LREQREC+146   FILTER ON REQUESTOR NAME                     
         BNE   ENQR2                                                            
         TM    REQFLTR,X'02'                                                    
         B     *+14                **NOP**                                      
         CLC   RMED,LREQREC+84     FILTER ON MEDIA                              
         BNE   ENQR2                                                            
*                                                                               
ENQR5A   OC    LREQREC+RQHOUT-RQHHDR(6),LREQREC+RQHOUT-RQHHDR                   
         BZ    ENQR5B                                                           
         CLC   RQHOUT,LREQREC+RQHOUT-RQHHDR  FILTER ON OUTPUT TYPE              
         BNE   ENQR2                                                            
*                                                                               
ENQR5B   OC    LREQREC+RQHDEST-RQHHDR(2),LREQREC+RQHDEST-RQHHDR                 
         BZ    ENQR5C                                                           
         CLC   RQHDEST,LREQREC+RQHDEST-RQHHDR  FILTER ON DESTINATION ID         
         BNE   ENQR2                                                            
*                                                                               
ENQR5C   EQU   *                                                                
*                                                                               
ENQR6    LH    R4,REQINCR          INGNORE (REQINCR-1) REQS                     
         BCTR  R4,0                                                             
         CH    R4,SKIPCTR                                                       
         BE    ENQR7                                                            
         LH    R4,SKIPCTR                                                       
         LA    R4,1(R4)                                                         
         STH   R4,SKIPCTR                                                       
         B     ENQR2                                                            
*                                                                               
ENQR7    LH    R5,READCTR          UPDATE REQ READ COUNTER                      
         LA    R5,1(R5)                                                         
         STH   R5,READCTR                                                       
         MVC   TEMP(160),REQREC    SAVE REQ REC                                 
         CLC   RNUM,=C'99'                                                      
         BNE   *+16                                                             
         LH    R5,CANCCTR          UPDATE CANCELLED COUNTER                     
         LA    R5,1(R5)                                                         
         STH   R5,CANCCTR                                                       
         CLI   REQOPTN,C'L'        DISPLAY LAST OPTION                          
         BE    ENQR2               YES DO NOT DISPLAY                           
*                                                                               
ENQR8    BAS   RE,ENQDISP          DISPLAY REQUEST                              
         CLC   DISPCTR,DISPMAX     END OF SCREEN                                
         BNE   ENQR2               NO BACK FOR NEXT                             
         B     ENQRE3                                                           
*                                                                               
ENQREOF  OC    READCTR,READCTR                                                  
         BZ    ENQRE2              NO REQUESTS FOUND                            
         CLI   REQOPTN,C'L'                                                     
         BE    ENQRE1                                                           
         OC    DISPCTR,DISPCTR                                                  
         BZ    ENQRE2              NO REQUESTS DISPLAYED                        
         B     ENQRE3                                                           
*                                                                               
ENQRE1   MVC   REQREC(160),TEMP    DISPLAY LAST REQUEST OPTION                  
         GOTO1 ENQDISP             DISPLAY THE LAST REQ                         
         XC    TEMP(60),TEMP                                                    
         MVC   TEMP(24),=C'Request total = NNN live'                            
         MVC   TEMP+24(33),=C' - last request num NNN displayed'                
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
*                                                                               
ENQRE2   XC    TEMP(60),TEMP                                                    
         MVC   TEMP(18),=C'Requests not found'                                  
         XC    DISPFLDS(2),DISPFLDS                                             
         B     ENQRX                                                            
*                                                                               
ENQRE3   XC    TEMP(60),TEMP                                                    
         MVC   TEMP(31),=C'Requests NNN thru NNN displayed'                     
         MVC   TEMP+31(25),=C' - change cancel status ?'                        
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
*                                                                               
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
*                                                                               
ENQRX4   LR    R2,R3                                                            
         USING TA04FFD,R2                                                       
         MVC   BVRHDR,TEMP         SET HDR MSG                                  
         MVI   FERN,X'FF'                                                       
         B     EXIT                                                             
*                                                                               
*        ROUTINE TO FORMAT REQUEST DATA IN SCREEN DISPLAY LINE                  
*                                                                               
ENQDISP  NTR1                                                                   
         L     R4,DISPADR          R4=A(NEXT SCR DISP LINE)                     
*                                                                               
         XC    DLINE,DLINE         DISPLAY CANCELLED FLAG                       
         MVI   DLCANC,C' '                                                      
         MVC   DLNUM,RNUM                                                       
         CLC   RNUM,=C'99'                                                      
         BNE   ENQD0                                                            
         MVI   DLCANC,C'C'                                                      
         MVC   DUB(1),REQNUMB                                                   
         BAS   RE,GETREQID                                                      
         MVC   DLNUM,DUB+1                                                      
*                                                                               
ENQD0    CLI   REQNUM,255          DONT DISPLAY REQ ID FOR SPECIFICS            
         BE    *+10                                                             
         MVC   DLNUM,=C'  '                                                     
         MVC   DLNUM+2(64),RNUM+2                                               
         MVC   DLNAME,RNAME                                                     
*                                                                               
         OI    DLHDR+6,X'80'                                                    
         CLI   STATUS,3                                                         
         BNE   ENQDISPX                                                         
*                                                                               
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
*                                                                               
ENQDISPX XIT1                                                                   
*                                                                               
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
*                                                                               
GETRID1  CLI   0(R7),0             SEARCH REQTBL FOR BINARY REQ NUM             
         BE    GETRIDX                                                          
         CLC   DUB(1),1(R7)                                                     
         BE    GETRID2                                                          
         IC    R1,0(R7)                                                         
         AR    R7,R1                                                            
         B     GETRID1                                                          
*                                                                               
GETRID2  ST    R7,DUB+4            SET FOUND ADR AND FLAG                       
         MVI   DUB+3,1                                                          
         IC    R1,0(R7)            POINT TO LAST TWO BYTES OF ENTRY             
         AR    R7,R1                                                            
         SH    R7,=H'2'                                                         
         MVC   DUB+1(2),0(R7)      RETURN REQ ALPHA ID                          
*                                                                               
GETRIDX  XIT1                                                                   
         EJECT                                                                  
*        CHANGE REQUEST CANCEL STATUS                                           
*                                                                               
CANREQ   LA    R7,BVRFRSTH                                                      
         LA    R6,BVRHDRH                                                       
         SR    R7,R6                                                            
         AR    R2,R7                                                            
         USING TA04FED,R2                                                       
         LA    R4,ENQFRSTH         R4=A(NEXT TWA LINE NUM)                      
         USING DISPLD,R4                                                        
         ST    R4,DISPADR                                                       
         LH    R5,DISPFLDS         R5=NUM OF TWA LINES                          
         LA    R6,DISPFLDS+4       R6=A(DISK ADR)                               
         LTR   R5,R5                                                            
         BNZ   CANR1                                                            
         DC    H'0'                                                             
*                                                                               
CANR1    TM    DLHDR+4,X'C0'       ANY INPUT IN CANCEL FIELD                    
         BZ    CANR6                                                            
         MVC   ADR,0(R6)                                                        
         GOTO1 DATAMGR,DMCB,(X'A0',DMRDIR),REQUEST,ADR,REQREC                   
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
*                                                                               
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
*                                                                               
CANR4    GOTO1 DATAMGR,DMCB,(X'20',DMWRT),REQUEST,ADR,REQREC                    
         CLI   DMCB+8,0                                                         
         BNE   CANR5                                                            
         BAS   RE,ENQDISP                                                       
         B     CANR6                                                            
*                                                                               
CANR5    MVI   FERN,0              DISK ERROR ON UPDATE                         
         ST    R4,FADR                                                          
         B     EXIT                                                             
*                                                                               
CANR6    LA    R4,86(R4)           BUMP TO NEXT LINE                            
         ST    R4,DISPADR                                                       
         LA    R6,4(R6)            BUMP TO NEXT DISK ADR                        
         BCT   R5,CANR1                                                         
*                                                                               
CANRX    XC    TEMP,TEMP                                                        
         MVC   TEMP(29),=C'Request cancel status amended'                       
         B     ENQRX4                                                           
         EJECT                                                                  
*        DISPLAY TOTAL REQUEST COUNTS                                           
*                                                                               
TOTREQ   XC    TOTCTR(256),TOTCTR                                               
         XC    TOTCTR+256(256),TOTCTR+256                                       
         XC    ADR,ADR                                                          
         LR    R2,R3                                                            
         USING TA04FFD,R2                                                       
         GOTO1 ,DMCB,(X'20',DMRSEQ),REQUEST,ADR,FILREC                          
*                                                                               
TOTR1    GOTO1 DATAMGR,DMCB        READ NEXT REQ REC                            
         MVC   REQREC,FILREC                                                    
         CLI   DMCB+8,0                                                         
         BE    TOTR2                                                            
         TM    DMCB+8,X'80'                                                     
         BO    TOTR3                                                            
         B     REQIOERR                                                         
*                                                                               
TOTR2    CLC   RNUM,=X'FFFF'       IGNORE DUMMYS                                
         BE    TOTR1                                                            
         CLC   RNUM,=C'99'         IGNORE CANCELLED                             
         BE    TOTR1                                                            
*                                                                               
         CLI   LREQREC+82,C'*'                                                  
         B     *+14                **NOP**                                      
         CLC   RAGY,LREQREC+82     FILTER ON AGENCY                             
         BNE   TOTR1                                                            
*                                                                               
         CLI   LREQREC+RQHOFF-RQHHDR,0                                          
         BE    *+14                                                             
         CLC   RQHOFF,LREQREC+RQHOFF-RQHHDR  FILTER ON OFFICE                   
         BNE   TOTR1                                                            
*                                                                               
         TM    REQFLTR,X'01'                                                    
         BZ    *+14                                                             
         CLC   RNAME,LREQREC+148   FILTER ON REQUESTOR                          
         BNE   TOTR1                                                            
         TM    REQFLTR,X'02'                                                    
         B     *+14                **NOP**                                      
         CLC   RMED,LREQREC+84     FILTER ON MEDIA                              
         BNE   TOTR1                                                            
*                                                                               
         OC    LREQREC+RQHOUT-RQHHDR(6),LREQREC+RQHOUT-RQHHDR                   
         BZ    *+14                                                             
         CLC   RQHOUT,LREQREC+RQHOUT-RQHHDR  FILTER ON OUTPUT TYPE              
         BNE   TOTR1                                                            
*                                                                               
         OC    LREQREC+RQHDEST-RQHHDR(2),LREQREC+RQHDEST-RQHHDR                 
         BZ    *+14                                                             
         CLC   RQHDEST,LREQREC+RQHDEST-RQHHDR  FILTER ON DESTINATION ID         
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
*                                                                               
TOTR3    SR    R5,R5               DISPLAY COUNTERS ON MENU SCREEN              
         LA    R6,TOTCTR                                                        
TOTR4    CH    R5,=H'256'                                                       
         BE    TOTRX                                                            
         STC   R5,DUB                                                           
         BAS   RE,GETREQID         SEARCH REQTBL FOR BINARY REQ NUM             
         CLI   DUB+3,0                                                          
         BE    TOTR7               NOT IN REQTBL                                
*                                                                               
         LA    RE,BVRFRSTH                                                      
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
         B     TOTR7                                                            
TOTR6    IC    RF,0(RE)            BUMP SCREEN FIELD                            
         AR    RE,RF                                                            
         B     TOTR5                                                            
*                                                                               
TOTR7    LA    R5,1(R5)            BUMP REQNUM                                  
         LA    R6,2(R6)            BUMP TABLE                                   
         B     TOTR4                                                            
*                                                                               
TOTRX    XC    TEMP(60),TEMP                                                    
         MVC   TEMP(30),=C'Total request counts displayed'                      
         B     ENQRX4                                                           
         EJECT                                                                  
*        ADD A NEW REQUEST RECORD TO THE REQUEST CHAIN                          
*                                                                               
NEWREQ   MVI   RQHFLAG,RQHFLNK     SET CHAIN REQUEST FLAG                       
         MVC   ADR,=X'AAAAAAAA'                                                 
*                                                                               
NEWREQ1  CLC   RNUM,=C'40'         ERROR MSG LIST - BIG REQUEST TEST            
         BNE   NEWREQ1X                                                         
         CLC   RNAME(5),=C'CTRL#'  CTRL#CC TO PASS VALUE IN RQHCTRL             
         BE    NEWREQ1C                                                         
         CLC   RNAME(5),=C'CARD#'  CARD#NN TO CREATE NN CARD REQUEST            
         BNE   NEWREQ1X                                                         
         CLC   RNAME+5(2),=C'02'                                                
         BL    NEWREQ1X                                                         
         CLC   RNAME+5(2),=C'15'   15 IS MAX NUMBER OF CARDS                    
         BH    NEWREQ1X                                                         
         PACK  DUB,RNAME+5(2)                                                   
         CVB   R1,DUB              R1=NUMBER OF CARDS IN TEST REQUEST           
         LHI   R0,2                R0=REQCARD NUMBER                            
         LA    RE,REQREC2                                                       
*                                                                               
NEWREQ1A MVC   0(1,RE),=C' '                                                    
         MVC   1(79,RE),0(RE)                                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   00(2,RE),RNUM '                                                  
         MVI   02(RE),C'#'                                                      
         UNPK  03(2,RE),DUB+6(2)                                                
         MVC   66(7,RE),RNAME      REQUESTOR NAME                               
         MVI   73(RE),C'#'                                                      
         UNPK  74(2,RE),DUB+6(2)                                                
         MVC   76(2,RE),=C'XX'                                                  
         CR    R0,R1                                                            
         BE    NEWREQ1B                                                         
         AHI   R0,1                                                             
         LA    RE,80(RE)                                                        
         B     NEWREQ1A                                                         
NEWREQ1B AHI   R0,-1               REQFLAG HAS NUMCARDS-1                       
         SLL   R0,4                                                             
         STC   R0,DUB                                                           
         OC    REQFLAG,DUB         SET MAXCARDS-1                               
*                                                                               
         CLI   RNAME+7,C'#'        TEST CARD#NN#CC TO SET CTRL TO CC            
         BNE   NEWREQ1X                                                         
         CLC   RNAME+8(2),=C'01'   CC MUST BE 01-63                             
         BL    NEWREQ1X                                                         
         CLC   RNAME+8(2),=C'63'                                                
         BH    NEWREQ1X                                                         
         PACK  DUB,RNAME+8(2)                                                   
         CVB   R1,DUB              R1=VALUE TO BE PASSED IN RQHCTRL             
         STC   R1,RQHCTRL                                                       
         B     NEWREQ1X                                                         
*                                                                               
NEWREQ1C CLC   RNAME+5(2),=C'01'   CTRL#CC MUST BE 01-63                        
         BL    NEWREQ1X                                                         
         CLC   RNAME+5(2),=C'63'                                                
         BH    NEWREQ1X                                                         
         PACK  DUB,RNAME+5(2)                                                   
         CVB   R1,DUB              R1=VALUE TO BE PASSED IN RQHCTRL             
         STC   R1,RQHCTRL                                                       
NEWREQ1X EQU   *                                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'20',DMADD),REQUEST,ADR,REQREC                    
         CLI   DMCB+8,0                                                         
         BNE   REQIOERR                                                         
         CLI   BVRWHYH+5,0         TEST IF WHY/REASON TEXT WAS INPUT            
         BE    *+14                                                             
         MVC   BVRWHY(2),=C'??'    NOP WHY TEXT FOR NEXT ADD                    
         OI    BVRWHYH+6,X'80'                                                  
         B     SAVEADR                                                          
         EJECT                                                                  
DISPMAX  DC    H'15'                                                            
REQUEST  DC    CL8'CTREQ'                                                       
DMRDIR   DC    CL8'DMRDIR'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMADD    DC    CL8'DMADD'                                                       
DMWRT    DC    CL8'DMWRT'                                                       
*                                                                               
FLDMIS   EQU   1                                                                
FLDINV   EQU   2           INVALID FIELD                                        
*                                                                               
         LTORG                                                                  
*                                                                               
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
         DS    CL64                                                             
DLNAME   DS    CL11                                                             
         EJECT                                                                  
*CTREQSAVE                                                                      
       ++INCLUDE CTREQSAVE                                                      
         EJECT                                                                  
*CTREQTEMP                                                                      
       ++INCLUDE CTREQTEMP                                                      
         EJECT                                                                  
*CTREQFFD                                                                       
       ++INCLUDE CTREQFFD                                                       
         EJECT                                                                  
*CTREQFED                                                                       
       ++INCLUDE CTREQFED                                                       
         EJECT                                                                  
*DDFLDIND                                                                       
       ++INCLUDE DDFLDIND                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004CTREQ02   03/20/17'                                      
         END                                                                    
