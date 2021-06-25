*          DATA SET SPBUYHIST  AT LEVEL 021 AS OF 11/16/06                      
*PHASE BUYHISTA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRTREC                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE STXITER                                                                
BUYHIST  TITLE 'SPOT BUY HISTORY UPDATE'                                        
BUYHIST  CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
*                                                                               
QSPTFIL  EQU   X'21'                                                            
*                                                                               
QCOPY    EQU   1                                                                
QCHG     EQU   2                                                                
QADD     EQU   3                                                                
*                                                                               
         NBASE 0,BUYHIST,=V(REGSAVE)                                            
*                                                                               
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING BUYHIST+4096,RA                                                  
         ST    RD,BUYHRD                                                        
*                                                                               
         L     RC,=A(BUYHWK)                                                    
         USING BUYHWK,RC                                                        
*                                                                               
S        USING STAPACKD,STAWORK                                                 
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         ZAP   MAXLINE,=P'58'      SET MAXLINES                                 
*                                                                               
         XC    WORK,WORK                                                        
         ST    RB,WORK                                                          
         L     RE,=V(DUMMY)        DUMMY FOLLOWS REGSAVE                        
         ST    RE,WORK+4                                                        
         OI    WORK+4,X'80'                                                     
         GOTO1 =V(STXITER),DMCB,WORK                                            
*                                                                               
         XC    DMCB(12),DMCB                                                    
         L     RE,=A(SORTCARD)                                                  
         ST    RE,DMCB+0                                                        
         L     RE,=A(RECCARD)                                                   
         ST    RE,DMCB+4                                                        
         GOTO1 =V(SORTER),DMCB                                                  
*                                                                               
         BAS   RE,INIT             READ PARMS/LOAD STUFF/OPEN FILES             
*                                                                               
         BAS   RE,BLDAG            BUILD AGENCY LIST                            
         EJECT                                                                  
GET      L     R1,=A(RECVIN)                                                    
         L     R0,AIO1LN           READ TO IO1-4                                
         GET   (1),(0)                                                          
*                                                                               
GET2     L     RE,AIO1LN                                                        
         CLC   0(2,RE),=H'52'      MIN SPTFILE RECLEN                           
         BNH   GET                 MUST BE A BAD RECORD                         
*                                                                               
         L     R8,AIO1                                                          
         ST    R8,AIO              SET CURRENT IO AREA ADDRESS                  
         USING RCVRHDRD,R8                                                      
*                                                                               
         CLI   DM$RFILTY,QSPTFIL   TEST SPTFILE                                 
         BNE   GET                                                              
*                                                                               
         CLI   BUYKEY,X'10'        TEST BUYREC                                  
         BL    GET                                                              
* SET X'00' AT EOR                                                              
         L     R1,AIO1LN           POINT TO LEN FIELD                           
         LH    R0,0(R1)            GET REC LENGTH                               
         AR    R1,R0               POINT TO EOR                                 
         XC    0(2,R1),0(R1)       AND CLEAR                                    
*                                                                               
         CLC   SVSYS+6(2),SPACES                                                
         BE    GET10                                                            
         CLC   SVSYS+6(2),BUYALPHA  MATCH ALPHA AGY CODE                        
         BNE   GET                                                              
*                                                                               
GET10    CLI   DM$RRECTY,QADD                                                   
         BE    GET20                                                            
*                                                                               
         CLI   DM$RRECTY,QCOPY                                                  
         BE    GET40                                                            
*                                                                               
         MVC   P(23),=C'*BUYH* UNKNOWN REC TYPE'                                
*                                                                               
         CLI   DM$RRECTY,QCHG                                                   
         BNE   GET12                                                            
         MVC   P(26),=C'*BUYH* CHANGE WITHOUT COPY'                             
         OC    DM$RSIN,DM$RSIN     OFFLINE CHANGES MAY NOT HAVE COPIES          
         BNZ   GET12                                                            
         MVC   P,SPACES            SO SUPPRESS THE ERROR                        
         B     GET                                                              
*                                                                               
GET12    GOTO1 VPRINTER                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,BUYREC+13                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 =V(PRNTBL),DMCB,0,AIO1,C'DUMP',(R0),=C'1D'                       
         GOTO1 VPRINTER                                                         
         B     GET                                                              
*                                                                               
GET20    BAS   RE,CHKRSN           SEE IF REASON CODE IN ADD                    
         OC    A90EL,A90EL                                                      
         BNZ   GET22                                                            
         OC    A91EL,A91EL                                                      
         BZ    GET                                                              
*                                                                               
E        USING HISTEL,ELEM                                                      
*                                                                               
GET22    MVI   E.HISTEL,HISTELQ                                                 
         MVI   E.HISTELLN,HISTLENQ     SET MIN ELEM LEN                         
         MVC   E.HISTRSN(6),=C'NEWBUY'                                          
         ICM   RE,15,A90EL                                                      
         BZ    *+10                                                             
         MVC   E.HISTRSN(6),2(RE)                                               
* THERE MAY ALSO BE TEXT !                                                      
         XC    TEXTEL,TEXTEL                                                    
         L     R6,A90EL                                                         
         CLI   1(R6),RCELTXT-RCELEM   TEST ELEM LENGTH FOR TEXT                 
         BNH   GET23                  NO TEXT                                   
* SAVE REASON CODE TEXT FOR SORT                                                
         MVI   TEXTELCD,HTXTELQ                                                 
         SR    RE,RE                                                            
         IC    RE,1(R6)              GET LENGTH OF RCELEM                       
         AHI   RE,-(RCELTXT-RCELEM)  LESS FIXED LEN                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TEXTDATA(0),RCELTXT-RCELEM(R6)                                   
*                                                                               
         AHI   RE,3                ADD BACK 3 FOR ELCODE/LEN/EX                 
         STC   RE,TEXTELLN                                                      
*                                                                               
GET23    ICM   RE,15,A99EL                                                      
         BZ    GET24                                                            
         USING ACTVELEM,RE                                                      
         MVC   E.HISTPID,ACTVADD       SET PID OF CREATOR                       
         DROP  RE                                                               
*                                                                               
GET24    GOTO1 VDATCON,DMCB,(3,DM$RDATE),(2,E.HISTDATE)                         
         MVC   SVDATE,E.HISTDATE                                                
*                                                                               
         BAS   RE,GETTIME                                                       
         MVC   E.HISTTIME,TIME                                                  
*                                                                               
         MVI   E.HISTTYPE,RCID_NEW    SET NEW BUY CODE                          
         MVI   E.HISTDLEN,0           NO NEW/OLD FOR THIS TYPE                  
         BAS   RE,GENSORT                                                       
         B     GET                                                              
         DROP  E                                                                
         EJECT                                                                  
*===============================================================                
* PROCESS COPY - NEXT RECORD MUST BE ASSOCIATED CHANGE                          
*===============================================================                
         SPACE 1                                                                
GET40    L     R1,=A(RECVIN)                                                    
         L     R0,AIO2LN           READ TO IO2-4                                
         GET   (1),(0)                                                          
* SET X'00' AT EOR                                                              
         L     R1,AIO2LN           POINT TO LEN FIELD                           
         LH    R0,0(R1)            GET REC LENGTH                               
         AR    R1,R0               POINT TO EOR                                 
         XC    0(2,R1),0(R1)       AND CLEAR                                    
*                                                                               
         L     R8,AIO2                                                          
         ST    R8,AIO              SET CURRENT IO AREA ADDRESS                  
         USING RCVRHDRD,R8                                                      
*                                                                               
         CLI   DM$RFILTY,QSPTFIL   TEST SPTFILE                                 
         BNE   GETCHERR                                                         
*                                                                               
         CLI   DM$RRECTY,QCHG                                                   
         BNE   GETCHERR                                                         
*                                                                               
         CLI   BUYKEY,X'10'        TEST BUYREC                                  
         BL    GETCHERR                                                         
         TM    BUYKEY,X'08'        TEST COPIED BUY                              
         BO    GET20               YES - ONLY ADDS GET PROCESSED                
*                                                                               
         L     RE,AIO2LN                                                        
         CLC   0(2,RE),=H'52'      MIN SPTFILE RECLEN                           
         BNH   GETCHERR            MUST BE A BAD RECORD                         
*                                                                               
GET42    DS    0H                                                               
*                                                                               
         BAS   RE,CHKRSN                                                        
*                                                                               
         OC    A90EL,A90EL         TEST RSNEL IN CHANGE                         
         BZ    GET20               NO - SKIP THESE RECORDS                      
*                                                                               
         XC    TEXTEL,TEXTEL                                                    
         L     R6,A90EL                                                         
         CLI   1(R6),RCELTXT-RCELEM   TEST ELEM LENGTH FOR TEXT                 
         BNH   GET44                  NO TEXT                                   
* SAVE REASON CODE TEXT FOR SORT                                                
         MVI   TEXTELCD,HTXTELQ                                                 
         SR    RE,RE                                                            
         IC    RE,1(R6)              GET LENGTH OF RCELEM                       
         AHI   RE,-(RCELTXT-RCELEM)  LESS FIXED LEN                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TEXTDATA(0),RCELTXT-RCELEM(R6)                                   
*                                                                               
         AHI   RE,3                ADD BACK 3 FOR ELCODE/LEN/EX                 
         STC   RE,TEXTELLN                                                      
*                                                                               
GET44    L     R7,AIO1                                                          
*                                                                               
X        USING RCVRHDRD,R7                                                      
*                                                                               
         L     R8,AIO2                                                          
Y        USING RCVRHDRD,R8                                                      
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
E        USING HISTEL,ELEM                                                      
*                                                                               
         MVI   E.HISTEL,HISTELQ                                                 
         MVI   E.HISTELLN,HISTLENQ     SET MIN ELEM LEN                         
*                                                                               
         L     RE,A90EL                                                         
         USING RCELEM,RE                                                        
         MVC   E.HISTRSN(6),RCELRC     MOVE REASON CODE                         
*                                                                               
         CLI   RCFLDID,0               TEST CHG W/O REASON                      
         BE    GET                                                              
         DROP  RE                                                               
*                                                                               
         ICM   RE,15,A99EL                                                      
         BZ    GET46                                                            
         USING ACTVELEM,RE                                                      
         MVC   E.HISTPID,ACTVCHG       SET PID OF CREATOR                       
         DROP  RE                                                               
*                                                                               
GET46    GOTO1 VDATCON,DMCB,(3,X.DM$RDATE),(2,E.HISTDATE)                       
         MVC   SVDATE,E.HISTDATE                                                
*                                                                               
         BAS   RE,GETTIME          SET TIME IN HISTEL                           
         MVC   E.HISTTIME,TIME                                                  
*                                                                               
         MVI   CHGFLAG,0           RESET                                        
*                                                                               
         BAS   RE,CHKPER                                                        
         BE    *+8                                                              
         OI    CHGFLAG,X'80'       SET CHANGE OF PERIOD                         
*                                                                               
         BAS   RE,CHKTIM                                                        
*                                                                               
         BAS   RE,CHKSLN                                                        
*                                                                               
         BAS   RE,CHKCOS                                                        
*                                                                               
         BAS   RE,CHKNPW                                                        
         BE    *+8                                                              
         OI    CHGFLAG,X'40'       SET CHANGE OF NPW                            
*                                                                               
         TM    CHGFLAG,X'C0'       TEST CHANGE OF PERIOD OR NPW                 
         BNZ   *+8                 YES - DO NOT CHECK SKED                      
         BAS   RE,CHKSKD                                                        
*                                                                               
GET50    B     GET                                                              
         EJECT                                                                  
*============================================================                   
* COPY NOT FOLLOWED BY CHANGE - PRINT MESSAGE AND IGNORE                        
*============================================================                   
         SPACE 1                                                                
GETCHERR MVC   P(26),=C'*BUYH* COPY WITHOUT CHANGE'                             
         GOTO1 VHEXOUT,DMCB,AIO1,P+28,37,=C'TOG' KEY+RECOVERY HDR               
         GOTO1 VPRINTER                                                         
* MOVE RECORD TO IO1 AND CONTINUE                                               
         L     RE,AIO2LN           GET FROM ADDR                                
         LH    RF,0(RE)            GET FROM LEN                                 
         L     R0,AIO1LN           SET TO ADDR                                  
         LR    R1,RF               SET TO LEN                                   
         MVCL  R0,RE               MOVE TO AIO1LN                               
         B     GET2                AND CONTINUE                                 
         EJECT                                                                  
*===========================================================                    
* FOLLOWING ROUTINES EXTRACT DATA FROM COPY/CHANGE PAIR                         
* AND COMPARE THEM TO SEE IF THIS TYPE OF DATA CHANGED                          
* IF YES, A SORT RECORD IS GENERATED                                            
*===========================================================                    
         SPACE 1                                                                
CHKPER   MVI   E.HISTTYPE,RCID_PER SET CHANGE TYPE                              
         LA    R4,8                                                             
         STC   R4,E.HISTDLEN       SET DATA LEN                                 
*                                                                               
         LA    R5,E.HISTOLD                                                     
         MVC   0(3,R5),X.BDSTART   SET OLD START DATE                           
         MVC   3(3,R5),X.BDEND     OLD END DATE                                 
         MVC   6(1,R5),X.BDWKIND   OLD WEEKIND                                  
         MVC   7(1,R5),X.BDDAY     OLD DAY                                      
*                                                                               
         LA    R6,0(R4,R5)         POINT TO NEW DATA                            
         MVC   0(3,R6),Y.BDSTART   SET NEW START DATE                           
         MVC   3(3,R6),Y.BDEND     NEW END DATE                                 
         MVC   6(1,R6),Y.BDWKIND   NEW WEEKIND                                  
         MVC   7(1,R6),Y.BDDAY     NEW DAY                                      
         B      CHKALL                                                          
*                                                                               
CHKTIM   MVI   E.HISTTYPE,RCID_TIM                                              
         LA    R4,4                                                             
         STC   R4,E.HISTDLEN                                                    
*                                                                               
         LA    R5,E.HISTOLD                                                     
         MVC   0(4,R5),X.BDTIMST   MOVE START/END TIMES                         
*                                                                               
         LA    R6,0(R4,R5)                                                      
         MVC   0(4,R6),Y.BDTIMST                                                
         B     CHKALL                                                           
*                                                                               
CHKSLN   MVI   E.HISTTYPE,RCID_SLN                                              
         LA    R4,1                                                             
         STC   R4,E.HISTDLEN                                                    
*                                                                               
         LA    R5,E.HISTOLD                                                     
         MVC   0(1,R5),X.BDSEC     MOVE SLN                                     
*                                                                               
         LA    R6,0(R4,R5)                                                      
         MVC   0(4,R6),Y.BDSEC                                                  
         B     CHKALL                                                           
*                                                                               
CHKCOS   MVI   E.HISTTYPE,RCID_COS                                              
         LA    R4,5                                                             
         STC   R4,E.HISTDLEN                                                    
*                                                                               
         LA    R5,E.HISTOLD                                                     
         MVC   0(3,R5),X.BDCOST    MOVE OLD COST                                
         MVC   3(1,R5),X.BDCIND    AND INDS                                     
         MVC   4(1,R5),X.BDCIND2                                                
*                                                                               
         LA    R6,0(R4,R5)                                                      
         MVC   0(3,R6),Y.BDCOST    MOVE NEW COST                                
         MVC   3(1,R6),Y.BDCIND    AND INDS                                     
         MVC   4(1,R6),Y.BDCIND2                                                
         B     CHKALL                                                           
*                                                                               
CHKNPW   MVI   E.HISTTYPE,RCID_NPW                                              
         LA    R4,1                                                             
         STC   R4,E.HISTDLEN                                                    
*                                                                               
         LA    R5,E.HISTOLD                                                     
         MVC   0(1,R5),X.BDNOWK    MOVE OLD NPW                                 
*                                                                               
         LA    R6,0(R4,R5)                                                      
         MVC   0(1,R6),Y.BDNOWK                                                 
         B     CHKALL                                                           
         EJECT                                                                  
CHKALL   SR    R0,R0                                                            
         IC    R0,E.HISTELLN                                                    
         SR    R1,R1                                                            
         IC    R1,E.HISTDLEN                                                    
         AR    R1,R1               X 2                                          
         AR    R0,R1                                                            
         STC   R0,E.HISTELLN                                                    
*                                                                               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),0(R6)                                                    
         BNE   CHKALL2                                                          
         MVI   E.HISTELLN,HISTLENQ   RESET ELEMENT LENGTH                       
         CR    RE,RE               SET CC EQ                                    
         BR    RE                    RETURN WITH CC EQ                          
*                                                                               
* RECORD HAS CHANGED                                                            
*                                                                               
CHKALL2  LR    R0,RE                 SAVE RETURN REG                            
         BAS   RE,GENSORT            GENERATE SORT RECORD                       
         MVI   E.HISTELLN,HISTLENQ   RESET ELEMENT LENGTH                       
         LTR   RE,R0                 SET RETURN REG AND CC NEQ                  
         BR    RE                                                               
*                                                                               
GETTIME  SR    R0,R0                                                            
         ICM   R0,15,DM$RTIME      GET TIME AS 0HHMMSSX                         
         SRL   R0,12               DROP SSX                                     
         STH   R0,TIME                                                          
         BR    RE                                                               
         EJECT                                                                  
*=============================================================                  
* WORK OUT CHANGES IN NUMBER OF SPOTS/WEEK                                      
* BUILD A TABLE OF MAX 53 WEEKS START/END DATES FOR EACH REC                    
*=============================================================                  
         SPACE 1                                                                
CHKSKD   NTR1                                                                   
         XC    XWEEKS,XWEEKS       CLEAR COPY AREA                              
         XC    XCOUNTS,XCOUNTS                                                  
         GOTO1 VDATCON,DMCB,(3,X.BDSTART),WORK                                  
         GOTO1 (RF),(R1),(3,X.BDEND),WORK+6                                     
         GOTO1 VMOBILE,DMCB,(53,WORK),(5,XWEEKS),ADCONMBL,0                     
*                                                                               
         LA    R1,XWEEKS                                                        
         LA    R2,XCOUNTS                                                       
         MVC   ADBUY,AIO1                                                       
         BAS   RE,BLDSKED                                                       
*                                                                               
         XC    YWEEKS,YWEEKS       CLEAR CHANGE AREA                            
         XC    YCOUNTS,YCOUNTS                                                  
         GOTO1 VDATCON,DMCB,(3,Y.BDSTART),WORK                                  
         GOTO1 (RF),(R1),(3,Y.BDEND),WORK+6                                     
         GOTO1 VMOBILE,DMCB,(53,WORK),(5,YWEEKS),ADCONMBL,0                     
*                                                                               
         LA    R1,YWEEKS                                                        
         LA    R2,YCOUNTS                                                       
         MVC   ADBUY,AIO2                                                       
         BAS   RE,BLDSKED                                                       
*                                                                               
         CLC   XCOUNTS,YCOUNTS     TEST ANY CHANGE                              
         BE    EXIT                NO - RETURN                                  
* FIND FIRST WEEK THAT IS DIFFERENT                                             
         LA    R1,XWEEKS                                                        
         LA    R2,YWEEKS                                                        
         LHI   R3,53*4             SET LENGTH OF 53 COUNTERS                    
         XC    WORK,WORK           CLEAR BUILD AREA                             
         LA    R4,WORK+2                                                        
*                                                                               
CSK10    CLC   0(2,R1),0(R2)       SAME WEEK START DATE                         
         BE    CSK30                                                            
         BH    CSK20                                                            
* XWEEKS IS LOWER                                                               
         OC    WORK(2),WORK        TEST START DATE SAVED YET                    
         BNZ   *+10                YES                                          
         MVC   WORK(2),0(R1)       SET FIRST DIFFERENT WEEK                     
*                                                                               
         L     R0,256(R1)          GET COUNTER                                  
         STC   R0,0(R4)            SET 'OLD' VALUE (NEW VALUE IS 0)             
         LA    R4,2(R4)            NEXT COUNTER SLOT                            
         AHI   R3,-4               ADJUST REMAINING LENGTH                      
*                                                                               
         LA    R1,4(R1)            NEXT 'OLD' START DATE                        
         B     CSK10                                                            
*                                                                               
* YWEEKS IS LOWER                                                               
*                                                                               
CSK20    OC    WORK(2),WORK        TEST START DATE SAVED YET                    
         BNZ   *+10                YES                                          
         MVC   WORK(2),0(R2)       SET FIRST DIFFERENT WEEK                     
*                                                                               
         L     R0,256(R2)          GET COUNTER                                  
         STC   R0,1(R4)            SET 'NEW' VALUE (OLD VALUE IS 0)             
         LA    R4,2(R4)            NEXT COUNTER SLOT                            
         AHI   R3,-4               ADJUST REMAINING LENGTH                      
*                                                                               
         LA    R2,4(R2)            NEXT 'NEW' START DATE                        
         B     CSK10                                                            
*                                                                               
CSK30    OC    WORK(2),WORK        TEST DATE SET YET                            
         BNZ   CSK32                                                            
         CLC   256(4,R1),256(R2)   TEST THIS WEEK THE SAME                      
         BE    CSK38                                                            
*                                                                               
         MVC   WORK(2),0(R1)       SET WEEK START DATE                          
*                                                                               
CSK32    EX    R3,*+8              ALLOW 1 EXTRA BYTE FOR CLC                   
         B     *+10                                                             
         CLC   256(0,R1),256(R2)   TEST ALL REMAINING COUNTERS EQUAL            
         BE    CSK40               YES                                          
*                                                                               
         L     R0,256(R1)                                                       
         STC   R0,0(R4)            SET OLD VALUE                                
         L     R0,256(R2)                                                       
         STC   R0,1(R4)            SET NEW VALUE                                
*                                                                               
         LA    R4,2(R4)            NEXT ELEMENT SLOT                            
         AHI   R3,-4               ADJUST REMAINING LENGTH                      
*                                                                               
CSK38    CLI   0(R1),X'FF'         TEST END OF OLD WEEKS                        
         BE    *+8                                                              
         LA    R1,4(R1)            ELSE POINT TO NEXT OLD WEEK                  
*                                                                               
         CLI   0(R2),X'FF'         TEST END OF NEW WEEKS                        
         BE    *+8                                                              
         LA    R2,4(R2)            ELSE POINT TO NEXT NEW WEEK                  
         B     CSK30               CONTINUE UNTIL EQUAL                         
*                                                                               
CSK40    MVI   E.HISTTYPE,RCID_SKD                                              
         MVC   E.HISTOLD(108),WORK   MOVE DATE + 53 OLD/NEW COUNTERS            
*                                                                               
         LR    RE,R4                                                            
         LA    R0,WORK+2                                                        
         SR    RE,R0                                                            
         SRL   RE,1                GIVES NUMBER OF WEEKS                        
         STC   RE,E.HISTDLEN       SET DATALEN IN ELEM                          
* UPDATE ELEMENT LENGTH                                                         
         LA    R0,WORK                                                          
         SR    R4,R0                                                            
         AHI   R4,HISTLENQ                                                      
         STC   R4,E.HISTELLN                                                    
         BAS   RE,GENSORT                                                       
         B     EXIT                                                             
         EJECT                                                                  
*==============================================================                 
* BUILD TABLE OF WEEK END DATES AND NUMBER OF SPOTS                             
* R1 POINTS TO TABLE OF START/END DATES                                         
* COUNTERS ARE 256 BYTES FOLLOWING DATES                                        
*==============================================================                 
         SPACE 1                                                                
BLDSKED  NTR1                                                                   
         L     R6,ADBUY                                                         
         LA    R6,24(R6)            POINT TO BUYKEY                             
         LA    R6,BDELEM-BUYREC(R6) POINT TO BDELEM                             
*                                                                               
         MVI   ELCDLO,X'06'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
         USING REGELEM,R6                                                       
BLDSKD12 BRAS  RE,NEXTEL                                                        
         BNE   BLDSKDX                                                          
*                                                                               
         LR    R4,R1                                                            
*                                                                               
BLDSKD14 CLC   RDATE,2(R4)         SPOT PRIOR TO END OF THIS WEEK               
         BNH   BLDSKD16            YES                                          
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   BLDSKD14                                                         
         B     BLDSKD12            IGNORE SPOT NOT IN BUY DESC PERIOD           
*                                                                               
BLDSKD16 L     RE,ADBUY                                                         
         LA    RE,24(RE)           POINT TO BUYKEY                              
         CLI   3(RE),X'FF'         TEST POL                                     
         BNE   BLDSKD18                                                         
*                                                                               
         LHI   R0,1                                                             
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BZ    BLDSKD20            NO                                           
         ICM   R0,1,RPCOST         GET SPOTS FROM HOB OF RPCOST                 
         SRL   R0,2                USE 6 BITS ONLY                              
         B     BLDSKD20                                                         
*                                                                               
BLDSKD18 SR    R0,R0                                                            
         IC    R0,RNUM             GET NUMBER OF SPOTS                          
*                                                                               
BLDSKD20 TM    RSTATUS,X'80'       TEST MINUS                                   
         BZ    *+6                                                              
         LCR   R0,R0                                                            
*                                                                               
         A     R0,256(R4)          BUMP SPOT COUNT                              
         ST    R0,256(R4)                                                       
         B     BLDSKD12                                                         
*                                                                               
BLDSKDX  XIT1                                                                   
         EJECT                                                                  
*==============================================================                 
* RETURN ADDRESSES OF X'90',X'91',X'99' ELEMENTS FROM BUY                       
*==============================================================                 
         SPACE 1                                                                
CHKRSN   NTR1                                                                   
         XC    AELS,AELS                                                        
         L     R6,AIO                                                           
         LA    R6,24(R6)           POINT TO BUY KEY (AFTER RCVRHDR)             
         LA    R6,BDELEM-BUYKEY(R6)                                             
         SR    R0,R0                                                            
*                                                                               
CHKRSN2  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    EXIT                                                             
         CLI   0(R6),X'90'                                                      
         BNE   CHKRSN6                                                          
         CLI   DM$RRECTY,QADD      TEST ADD                                     
         BE    CHKRSN4                                                          
         USING RCELEM,R6                                                        
         CLI   RCFLDID,0           TEST CHANGE REQ'D RSNCODE                    
         BE    CHKRSN2             NO                                           
CHKRSN4  ST    R6,A90EL                                                         
         B     CHKRSN2                                                          
*                                                                               
CHKRSN6  CLI   0(R6),X'91'                                                      
         BNE   CHKRSN8                                                          
         ST    R6,A91EL                                                         
         B     CHKRSN2                                                          
*                                                                               
CHKRSN8  CLI   0(R6),X'99'                                                      
         BNE   CHKRSN2                                                          
         ST    R6,A99EL                                                         
         B     CHKRSN2                                                          
*                                                                               
CHKRSNX  B     EXIT                                                             
         EJECT                                                                  
*=================================================================              
* GENERATE A SORT RECORD                                                        
*=================================================================              
         SPACE 1                                                                
GENSORT  NTR1                                                                   
         MVC   SORTKEY,BUYKEY                                                   
         MVC   SORTKEY+11(1),SORTKEY+10  MOVE BUYLINE                           
         MVI   SORTKEY+10,0        SET SORTKEY TO DIRECTORY KEY                 
         NI    SORTKEY,X'F7'       DROP X'08' ORIG BUY FLAG                     
         MVC   SORTAGYA,BUYALPHA                                                
         MVC   SORTDATE,SVDATE     SET DATE IN KEY SO CAN PRINT IT              
*                                                                               
         L     R0,SEQNUM                                                        
         AHI   R0,1                                                             
         ST    R0,SEQNUM                                                        
         STCM  R0,15,SORTSEQ                                                    
*                                                                               
         MVC   SORTEL,ELEM                                                      
* CALCULATE LENGTH OF SORT RECORD                                               
         LHI   RE,SORTEL-SORTLEN   FIXED SORT LENGTH                            
         SR    R0,R0                                                            
         IC    R0,ELEM+1                                                        
         AR    R0,RE                                                            
         SLL   R0,16                                                            
         ST    R0,SORTLEN                                                       
*                                                                               
         CLI   TEXTEL,0            TEST ANY TEXT                                
         BE    GENSORT2                                                         
         LH    RE,SORTLEN                                                       
         LA    RE,SORTLEN(RE)      POINT TO END OF RECORD                       
         MVC   0(L'TEXTEL,RE),TEXTEL                                            
         SR    R0,R0                                                            
         IC    R0,TEXTELLN         GET LEN OF TEXTEL                            
         LH    RE,SORTLEN                                                       
         AR    R0,RE                                                            
         SLL   R0,16                                                            
         ST    R0,SORTLEN                                                       
*                                                                               
GENSORT2 GOTO1 =V(SORTER),DMCB,=C'PUT',SORTLEN                                  
         AP    SORTCNT,=P'1'                                                    
*                                                                               
         CLI   TRACESW,C'Y'                                                     
         BNE   EXIT                                                             
*                                                                               
         MVC   P(7),=C'TO SORT'                                                 
         GOTO1 VHEXOUT,DMCB,SORTKEY,P+10,13,=C'TOG'  KEY                        
         GOTO1 (RF),(R1),SORTKEY+13,P+38,4,=C'TOG'   SEQNUM                     
         GOTO1 VPRINTER                                                         
*                                                                               
H        USING HISTEL,SORTEL                                                    
* ELEM CODE/LEN                                                                 
         GOTO1 VHEXOUT,DMCB,H.HISTEL,P+10,2,=C'TOG'                             
*                                                                               
         MVC   P+14(6),H.HISTRSN    REASON CODE IS EBCDIC                       
*                                                                               
         LA    R0,HISTLENQ         STATIC ELEMENT LENGTH                        
         AHI   R0,-8               LESS 8 BYTES ALREADY DISPLAYED               
         GOTO1 (RF),(R1),SORTEL+8,P+21,(R0),=C'TOG'                             
*                                                                               
         GOTO1 VPRINTER                                                         
         CLI   H.HISTTYPE,RCID_SKD  TEST SKED CHANGE                            
         BNE   GENSORT4                                                         
         LA    R4,H.HISTOLD                                                     
         SR    R5,R5                                                            
         IC    R5,H.HISTDLEN       GET NUMBER OF WEEKS                          
         AR    R5,R5               X 2                                          
         AHI   R5,2                + 2 FOR DATE                                 
         GOTO1 VHEXOUT,DMCB,(R4),P+10,(R5),=C'TOG'                              
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
* OLD DATA                                                                      
GENSORT4 LA    R4,H.HISTOLD        POINT TO OLD DATA                            
         SR    R5,R5                                                            
         IC    R5,H.HISTDLEN       GET DATA LEN                                 
         GOTO1 VHEXOUT,DMCB,(R4),P+10,(R5),=C'TOG'                              
         GOTO1 VPRINTER                                                         
*                                                                               
         AR    R4,R5               POINT TO NEW DATA                            
         GOTO1 VHEXOUT,DMCB,(R4),P+10,(R5),=C'TOG'                              
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER            SKIP A LINE                                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
INPUTEOF DC    0H'0'                                                            
         CLOSE (RECVIN)                                                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO1            PRESET IO AREA ADDRESS                       
*                                                                               
         L     RE,=A(HEAD)                                                      
         MVC   MID1(L'HEAD),0(RE)                                               
         ZAP   LINE,=P'200'                                                     
         L     RE,AIO1                                                          
         XC    0(32,RE),0(RE)      CLEAR HISTKEY                                
         CP    SORTCNT,=P'0'                                                    
         BNE   OUT2                                                             
         MVC   P(16),=C'NO INPUT RECORDS'                                       
         GOTO1 VPRINTER                                                         
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         B     EOJ                                                              
*                                                                               
OUT2     GOTO1 =V(SORTER),DMCB,=C'GET'                                          
*                                                                               
         ICM   R2,15,4(R1)                                                      
         BNZ   OUT4                                                             
* REACHED EOF ON SORT FILE                                                      
         LA    R2,SORTREC                                                       
         MVI   0(R2),X'FF'                                                      
         MVC   1(31,R2),0(R2)      SET SVSORT KEY TO X'FF'S                     
         B     OUT12                                                            
*                                                                               
* MOVE SORT REC TO SVREC SO CAN SEE IT IN DUMPS                                 
*                                                                               
OUT4     LH    R3,0(R2)            GET 'FROM' REC LEN                           
         LA    RE,SORTLEN          SET 'TO' REC ADDR                            
         LA    RF,2(R3)            'TO'LEN = 'FROM' LEN+2 FOR X'0000'           
         MVCL  RE,R2                                                            
*                                                                               
         L     R6,AIO1             POINT TO PREVIOUS HISTREC                    
         USING HISTRECD,R6                                                      
*                                                                               
         CLI   HISTKEY,0           TEST FIRST TIME                              
         BNE   OUT6                                                             
         BAS   RE,PRTBUY                                                        
         MVC   SVDATE,SORTDATE                                                  
         B     OUT20               YES                                          
*                                                                               
OUT6     CLC   SORTKEY,HISTBUYK    SAME BUY RECORD KEY                          
         BE    OUT8                YES                                          
         BAS   RE,PRTBUY                                                        
         MVC   SVDATE,SORTDATE                                                  
         B     OUT12                                                            
*                                                                               
OUT8     CLC   SORTDATE,SVDATE     SAME DATE                                    
         BE    OUT40                                                            
         BAS   RE,PRTBUY                                                        
         MVC   SVDATE,SORTDATE                                                  
         B     OUT26               GO REMOVE ELEMENTS THIS DATE                 
*                                                                               
OUT12    LA    RF,PUTREC                                                        
         CLC   KEY(32),KEYSAVE     TEST REC ON FILE                             
         BE    *+8                                                              
         LA    RF,ADDREC           WRITE PREVIOUS HISTREC                       
         MVI   XSP,C'Y'                                                         
*                                                                               
         CLI   WRITESW,C'N'        TEST WRITE=NO                                
         BE    *+6                                                              
         BASR  RE,RF                                                            
*                                                                               
         CLI   TRACESW,C'Y'                                                     
         BNE   OUT14                                                            
         GOTO1 =V(PRTREC),DMCB,(C'E',AIO1),(42,32),VPRINT,VHEXOUT,0,0           
*                                                                               
OUT14    CLI   SORTKEY,X'FF'                                                    
         BE    EOJ                                                              
*                                                                               
K        USING HISTRECD,KEY                                                     
*                                                                               
OUT20    XC    KEY,KEY                                                          
         MVI   K.HISTTYP,HISTTYQ                                                
         MVI   K.HISTSTYP,HISTSTYQ                                              
         MVC   K.HISTBUYK,SORTKEY                                               
         DROP  K                                                                
*                                                                               
         MVI   XSP,C'Y'                                                         
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(32),KEYSAVE                                                  
         BE    OUT24                                                            
* FORMAT NEW HISTREC                                                            
         XC    HISTKEY(256),HISTKEY                                             
         MVC   HISTKEY(32),KEYSAVE                                              
         MVC   HISTAGYA,SORTAGYA                                                
         MVC   HISTLEN,=H'42'                                                   
         B     OUT40                                                            
*                                                                               
OUT24    MVI   XSP,C'Y'                                                         
         GOTO1 GETREC                                                           
         SPACE 1                                                                
* REMOVE ANY ELEMENTS WITH THIS DATE                                            
         SPACE 1                                                                
OUT26    L     R7,AIO1                                                          
         LA    R7,42(R7)           POINT TO FIRST ELEMENT                       
*                                                                               
OUT30    CLI   0(R7),HISTELQ                                                    
         BNE   OUT40                                                            
         CLC   HISTDATE-HISTEL(2,R7),SORTDATE                                   
         BNE   OUT32                                                            
         GOTO1 VRECUP,DMCB,(C'T',AIO1),(R7)                                     
         CLI   0(R7),HTXTELQ       TEXT ELEMENT MAY FOLLOW                      
         BNE   OUT30                                                            
         GOTO1 (RF),(R1),,(R7)                                                  
         B     OUT30                                                            
*                                                                               
OUT32    SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     OUT30                                                            
*                                                                               
OUT40    SR    RE,RE                                                            
         IC    RE,SORTEL+1                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SORTEL(0),42(R6)    SEE IF A DUP ELEM FOLLOWS                    
         BE    OUT2                YES - IGNORE                                 
*                                                                               
         GOTO1 VRECUP,DMCB,(C'T',AIO1),SORTEL,42(R6)                            
         LA    R7,SORTEL                                                        
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0               SEE IF THERE'S A TEXTEL                      
         CLI   0(R7),HTXTELQ                                                    
         BNE   OUT2                                                             
         LA    RE,42(R6)           FIRST ELEMENT IN HISTREC                     
         SR    R0,R0                                                            
         IC    R0,1(RE)                                                         
         AR    RE,R0               TEXT MUST GO AFTER IT                        
         ST    RE,8(R1)            SET AS INSERTION POINT                       
         GOTO1 (RF),(R1),,(R7)                                                  
         B     OUT2                                                             
*                                                                               
EOJ      L     RD,BUYHRD           SET TO EOJ EXIT                              
         XBASE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*==========================================================                     
* PROGRAM INITIALIZATION ROUTINES                                               
*==========================================================                     
         SPACE 1                                                                
INIT     NTR1                                                                   
         GOTO1 =V(CARDS),DMCB,ELEM,=C'RE00'                                     
         MVC   P(80),ELEM                                                       
         GOTO1 VPRINTER                                                         
*                                                                               
         CLC   =C'SPOT',ELEM                                                    
         BE    INIT2                                                            
INIT1    MVC   P(30),=C'** ERROR * SYSTEM NOT VALID **'                         
         GOTO1 =V(LOGIO),DMCB,1,(30,P)                                          
         GOTO1 VPRINTER                                                         
         B     CANCEL                                                           
         SPACE 1                                                                
INIT2    BAS   RE,GETSE            VALIDATE SYSTEM                              
         BE    INIT1               ERROR IF SE NUM IS ZERO                      
         MVC   SVSYS,ELEM          SPOTXYAA WHERE XY=SYS AND AA=AGY             
         EJECT                                                                  
*==========================================================                     
* VALIDATE PARMS                                                                
*==========================================================                     
         SPACE 1                                                                
INIT10   GOTO1 =V(CARDS),DMCB,ELEM,=C'RE00'                                     
         CLI   ELEM,C'*'           IGNORE COMMENTS                              
         BE    INIT10                                                           
         MVC   P(80),ELEM                                                       
         GOTO1 VPRINTER                                                         
         CLC   =C'/*',ELEM                                                      
         BE    INIT30                                                           
*                                                                               
         CLC   =C'WRITE=',ELEM                                                  
         BNE   INIT12                                                           
         MVC   WRITESW,ELEM+6                                                   
         MVI   FLSPTF,C' '         SUPPRESS EOF SEARCH ON SPTFILE               
         MVI   FLXSPF,C' '           AND XSPFILE                                
         B     INIT10                                                           
*                                                                               
INIT12   CLC   =C'TRACE',ELEM                                                   
         BNE   INIT14                                                           
         MVI   TRACESW,C'Y'                                                     
         B     INIT10                                                           
*                                                                               
INIT14   CLC   =C'DATE',ELEM                                                    
         BNE   INIT16                                                           
*                                                                               
         GOTO1 =V(DATVAL),DMCB,(0,ELEM+5),TODAYQ                                
         MVC   SPECDATE(5),=C'DATE='   OVERRIDE DATE FOR PRINTING               
         MVC   SPECDATE+5(6),TODAYQ                                             
         OC    0(4,R1),0(R1)           NOW TEST IT WAS VALID                    
         BNZ   INIT10                                                           
         MVC   P(28),=C'*BUYH* INVALID DATE FORMAT'                             
         B     INIT22                                                           
*                                                                               
INIT16   CLC   =C'UPDID=',ELEM                                                  
         BNE   INIT18                                                           
         GOTO1 =V(DATAMGR),DMCB,=C'UPDID'                                       
         L     R1,12(R1)                                                        
         MVC   0(2,R1),ELEM+6                                                   
         B     INIT10                                                           
*                                                                               
INIT18   CLC   =C'DSPACE=',ELEM                                                 
         BNE   INIT20                                                           
         L     RE,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RE),ELEM+7                                     
         B     INIT10                                                           
*                                                                               
INIT20   MVC   P(31),=C'*BUYH* UNKNOWN PARAMETER CARD'                          
*                                                                               
INIT22   GOTO1 =V(LOGIO),DMCB,1,(31,P)                                          
         GOTO1 VPRINTER                                                         
         B     CANCEL                                                           
         EJECT                                                                  
INIT30   DC    0H'0'                                                            
         OC    TODAYQ,TODAYQ                                                    
         BNZ   INIT32                                                           
         GOTO1 VDATCON,DMCB,(5,0),(0,TODAYQ)                                    
         SPACE 1                                                                
*===========================================================                    
* LOAD CORERES PHASES                                                           
*===========================================================                    
         SPACE 1                                                                
INIT32   LA    R4,ADCONS                                                        
         LA    R5,(ADCONX-ADCONS)/4                                             
*                                                                               
INIT34   MVC   DUB,SPACES                                                       
         MVC   DUB(4),=C'T00A'                                                  
         MVC   WORK(1),0(R4)          FIRST CHAR IS MODULE EQUATE               
         GOTO1 VHEXOUT,DMCB,WORK,DUB+4,1,0                                      
         GOTO1 VLOADER,DMCB,DUB,0                                               
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,0(R4)            OVERWRITE EQUATE WITH ADDRESS                
*                                                                               
         LA    R4,4(R4)                                                         
         BCT   R5,INIT34                                                        
* CALL STAPACK TO TELL IT WHERE CABLETAB IS                                     
         XC    STAWORK,STAWORK                                                  
         MVI   S.STAPACT,QSTP_T00A9E                                            
         MVC   S.STAPACOM,VCABLETB                                              
         GOTO1 VSTAPACK,STAWORK                                                 
                                                                                
         SPACE 1                                                                
*==============================================================                 
* OPEN FILES                                                                    
*==============================================================                 
         SPACE 1                                                                
INIT40   GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'SPOT',FLIST,AIO1                  
*                                                                               
         MVC   TITLE(30),=CL30'SPOTXY BUY HISTORY UPDATE'                       
         MVC   TITLE(6),SVSYS                                                   
*                                                                               
INITX    OPEN  (RECVIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
*                                                                               
FLIST    DS    0H                                                               
FLSPTF   DC    CL8'USPTFILE'                                                    
FLXSPF   DC    CL8'UXSPFILE'                                                    
         DC    CL8' SPTDIR '                                                    
         DC    CL8'UXSPDIR '                                                    
         DC    CL8' STAFIL '                                                    
         DC    CL8'X       '                                                    
         SPACE 1                                                                
*=====================================================================*         
* LOCATE SPOT SYSTEM AND GET SENUM. OPTIONAL AGENCY ALPHA ALSO        *         
* CONVERT TO USE TWO CHR SYSTEM NAME SPOTXYAA WHERE XY=SYS AND AA=AGY *         
*=====================================================================*         
         SPACE 1                                                                
GETSE    NTR1                                                                   
         CLI   ELEM+7,C' '         SPOTXAA > SPOTX AA                           
         BNE   GETSE1                                                           
         CLI   ELEM+6,C' '                                                      
         BE    GETSE1                                                           
         ICM   R0,3,ELEM+5         MOVE AGY CODE ONE BYTE TO RIGHT              
         MVI   ELEM+5,C' '                                                      
         STCM  R0,3,ELEM+6                                                      
*                                                                               
GETSE1   MVC   SESNAM+7(2),ELEM+4  EXTRACT ONE/TWO CHR SYSTEM ID                
*                                                                               
GETSEX   GOTO1 =V(DMDDNAME),DMCB,(X'24',=C'DDNAME'),SESNAM,0                    
         CLI   8(R1),0                                                          
         BNE   GETSEXX                                                          
         L     RF,8(R1)            GET A(FILE INFO LIST)                        
         MVC   SESNUM,1(RF)        EXTRACT SENUM                                
         L     RE,=V(UTL)          MOVE TO UTL                                  
         MVC   4(1,RE),SESNUM                                                   
*                                                                               
GETSEXX  CLI   SESNUM,0            EXIT WITH CC EQL IF ERROR                    
         XIT1                                                                   
*                                                                               
SESNAM   DC    C'SYS=SPT##'                                                     
SESNUM   DC    X'00'                                                            
*                                                                               
CANCEL   DC    0H'0'                                                            
         ABEND 999                                                              
         EJECT                                                                  
*=====================================================*                         
* READ THE AGENCY HEADERS AND BUILD A TABLE           *                         
* THAT IDENTIFIES AGENCY AS US OR CANADIAN            *                         
*=====================================================*                         
         SPACE 1                                                                
BLDAG    NTR1                                                                   
         XC    AGYTAB,AGYTAB                                                    
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         BAS   RE,HIGH                                                          
         B     BLDAG4                                                           
*                                                                               
BLDAG2   BAS   RE,SEQ                                                           
*                                                                               
BLDAG4   CLI   KEY,6                                                            
         BNE   BLDAGX                                                           
         L     R6,ADAGY                                                         
         ST    R6,AIO                                                           
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         LA    R6,24(R6)                                                        
         SR    R0,R0                                                            
*                                                                               
BLDAG6   IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    BLDAG2                                                           
         CLI   0(R6),2             FIND A MEDIA ELEMENT                         
         BNE   BLDAG6                                                           
         ZIC   RE,3(R6)            GET AG/MD BYTE                               
         SRL   RE,4                DROP MEDIA                                   
         SLL   RE,2                X4                                           
         LA    RE,AGYTAB(RE)                                                    
         CLI   0(RE),0             TEST SOMETHING THERE ALREADY                 
         BNE   BLDAG2              YES - SKIP                                   
*                                                                               
         L     RF,ADAGY                                                         
         MVC   0(2,RE),1(RF)       MOVE AGYALPHA TO TABLE                       
         LA    RF,AGYPROF+7-AGYHDR(RF)                                          
         MVC   2(1,RE),0(RF)       MOVE COUNTRY CODE TO TABLE                   
         B     BLDAG2                                                           
*                                                                               
BLDAGX   B     EXIT                                                             
         EJECT                                                                  
*===================================================================*           
* DIRECTORY AND FILE ROUTINES                                       *           
*===================================================================*           
         SPACE 1                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         B     GODIR                                                            
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     GODIR                                                            
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     GODIR                                                            
*                                                                               
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     GODIR                                                            
*                                                                               
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     GODIR                                                            
*                                                                               
GODIR    NTR1                                                                   
         MVC   DIRECTRY,=C'SPTDIR'                                              
         CLI   XSP,C'Y'                                                         
         BNE   *+10                                                             
         MVC   DIRECTRY,=C'XSPDIR'                                              
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),DIRECTRY,KEYSAVE,       X        
               KEY,0                                                            
         B     EXIT                                                             
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     GOFILE                                                           
*                                                                               
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     GOFILE                                                           
*                                                                               
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     GOFILE                                                           
*                                                                               
GOFILE   NTR1                                                                   
         LA    R4,KEY+14                                                        
         MVC   FILE(8),=CL8'SPTFILE'                                            
         CLI   XSP,C'Y'                                                         
         BNE   *+14                                                             
         LA    R4,KEY+36                                                        
         MVC   FILE(8),=CL8'XSPFILE'                                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),FILE,(R4),              X        
               AIO,(0,DMWORK)                                                   
         MVI   XSP,C'N'            *** ALWAYS CLEAR XSP                         
         B     EXIT                                                             
*                                                                               
SEQSTA   MVC   COMMAND,=C'DMRSEQ'                                               
         B     GOSTA                                                            
*                                                                               
RDSTA    MVC   COMMAND,=C'DMREAD'                                               
         B     GOSTA                                                            
*                                                                               
HISTA    MVC   COMMAND,=C'DMRDHI'                                               
         B     GOSTA                                                            
*                                                                               
GOSTA    GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'STATION',KEY,AIO              
         SPACE 1                                                                
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
XSP      DC    C'N'                                                             
DMINBTS  DC    X'00'                                                            
DMOUTBTS DC    X'00'                                                            
COMMAND  DC    CL8' '                                                           
DIRECTRY DC    CL8'SPTDIR'                                                      
FILE     DC    CL8'SPTFILE'                                                     
         EJECT                                                                  
*===========================================================                    
* PRINT A LINE TO IDENTIFY ACTIVITY                                             
*===========================================================                    
                                                                                
PRTBUY   NTR1                                                                   
         LA    R8,SORTKEY                                                       
         USING BUYREC,R8                                                        
*                                                                               
         CLC   SORTKEY(1),SVPRTKEY SAME A-M                                     
         BE    PRTB2                                                            
         ZAP   LINE,=P'200'                                                     
         MVC   SVPRTKEY,SORTKEY                                                 
*                                                                               
PRTB2    MVC   PAGY,SORTAGYA                                                    
*                                                                               
         SR    RE,RE                                                            
         IC    RE,BUYREC                                                        
         N     RE,=X'00000007'     DROP AGY                                     
         LA    RE,MDTAB-1(RE)                                                   
         MVC   PMED,0(RE)                                                       
*                                                                               
         GOTO1 VCLUNPK,DMCB,BUYREC+1,PCLT                                       
*                                                                               
         L     RE,ADCLT                                                         
         CLC   BUYREC(3),1(RE)     TEST HAVE CLTHDR ALREADY                     
         BE    PRTB5                                                            
         SPACE 1                                                                
*=============================================================                  
* NEED TO READ CLTHDR WITHOUT DESTROYING KEY AND I/O AREAS                      
* REMEMBER THAT SPOT HEADERS DO NOT REQUIRE DMWORK                              
*=============================================================                  
         SPACE 1                                                                
         XC    HDRKEY,HDRKEY                                                    
         MVC   HDRKEY+1(3),BUYREC                                               
         MVC   HDRKEYSV,HDRKEY                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',HDRKEYSV,HDRKEY              
         CLC   HDRKEY(13),HDRKEYSV                                              
         BE    *+6                                                              
         DCHO                                                                   
         GOTO1 VDATAMGR,DMCB,=C'DMRDIR',=C'SPTFILE',HDRKEY+14,ADCLT             
*                                                                               
PRTB5    LA    RE,=C'POL'                                                       
         CLI   BUYREC+3,X'FF'                                                   
         BE    PRTB12                                                           
         L     RE,ADCLT                                                         
         LA    RE,CLIST-CLTHDR(RE)                                              
*                                                                               
PRTB10   CLC   BUYKPRD,3(RE)                                                    
         BE    PRTB12                                                           
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   PRTB10                                                           
         LA    RE,=C'???'                                                       
PRTB12   MVC   PPRD,0(RE)                                                       
*                                                                               
         BAS   RE,GETSTA                                                        
         MVC   PSTA,S.STAPQSTA   MOVE PRINTABLE STATION                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
         MVI   PEST+3,C'-'                                                      
*                                                                               
         IC    R0,BUYKBUY+1                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(2,SORTDATE),(11,PDATE)                             
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
PRTBUYX  XIT1                                                                   
         EJECT                                                                  
GETSTA   NTR1                                                                   
         XC    STAWORK,STAWORK                                                  
         MVI   S.STAPACT,C'U'                                                   
         MVC   S.STAPAGY,SORTAGYA                                               
         SR    RE,RE                                                            
         IC    RE,SORTKEY          GET AGY/MED BYTE                             
         N     RE,=X'00000007'     DROP AGY                                     
         LA    RE,MDTAB-1(RE)                                                   
         MVC   S.STAPMED,0(RE)                                                  
         MVC   S.STAPACOM,=A(COMFACS)                                           
*                                                                               
GETSTA2  MVC   S.STAPMKST,BUYREC+4                                              
*                                                                               
         LA    RE,AGYTAB                                                        
         LA    RF,16                                                            
GETSTA4  CLC   0(2,RE),SORTAGYA                                                 
         BE    GETSTA10                                                         
         LA    RE,4(RE)                                                         
         BCT   RF,GETSTA4                                                       
         DC    H'0'                                                             
*                                                                               
GETSTA10 MVC   S.STAPCTRY,2(RE)    MOVE COUNTRY CODE                            
*                                                                               
         GOTO1 VSTAPACK,STAWORK                                                 
         CLI   S.STAPERR,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
MDTAB    DC    C'TRNX'                                                          
         LTORG                                                                  
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
BUYHWK   DS    0D                                                               
         DC    CL8'BUYHWK'                                                      
BUYHRD   DS    F                                                                
*                                                                               
DMCB     DS    0CL24                                                            
         ORG   DMCB                                                             
DM1      DS    F                                                                
DM2      DS    F                                                                
DM3      DS    F                                                                
DM4      DS    F                                                                
DM5      DS    F                                                                
DM6      DS    F                                                                
*                                                                               
DMWORK   DS    24F                                                              
STAWORK  DS    XL32                                                             
*                                                                               
P1       DS    A                                                                
P2       DS    A                                                                
P3       DS    A                                                                
P4       DS    A                                                                
P5       DS    A                                                                
P6       DS    A                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'**KEY***'                                                    
KEY      DS    XL48                                                             
         DC    CL8'*KEYSAVE'                                                    
KEYSAVE  DS    XL48                                                             
         DC    CL8'*HDRKEY*'                                                    
HDRKEY   DS    XL24                                                             
HDRKEYSV DS    XL24                                                             
*                                                                               
SEQNUM   DC    F'0'                                                             
TODAYQ   DC    XL6'00'            YYMMDD DATE                                   
SPOT     DS    C                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
SVSYS    DS    CL8                                                              
SVPRTKEY DS    XL13                                                             
WRITESW  DC    C'Y'                                                             
TRACESW  DS    C                                                                
CHGFLAG  DS    X                                                                
SVDATE   DS    XL2                                                              
         DS    0F                                                               
SORTCNT  DC    PL4'0'                                                           
*                                                                               
         DS    0D                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
TIME     DS    H                                                                
WORK     DS    XL128                                                            
*                                                                               
ELEM     DS    XL256                                                            
*                                                                               
TEXTEL   DS    0XL96                                                            
TEXTELCD DS    X                                                                
TEXTELLN DS    X                                                                
TEXTDATA DS    XL94                                                             
*                                                                               
*                                                                               
         DS    0A                                                               
AELS     DS    0XL12                                                            
A90EL    DS    A                   X'90' ELEM ADDR                              
A91EL    DS    A                   X'91' ELEM ADDR                              
A99EL    DS    A                   X'99' ELEM ADDR                              
*                                                                               
* THESE ELEMENTS ARE FROM COPY RECORD                                           
*                                                                               
AELSAVE  DS    0XL12                                                            
A90ELSV  DS    A                                                                
A91ELSV  DS    A                                                                
A99ELSV  DS    A                                                                
*                                                                               
ADCONS   DS    0A                                                               
VCLUNPK  DC    AL1(QCLUNPK),AL3(0)                                              
VMOBILE  DC    AL1(QMOBILE),AL3(0)                                              
VSTAPACK DC    AL1(QSTAPACK),AL3(0)                                             
VCABLETB DC    AL1(QCABLETB),AL3(0)  T00A9E                                     
ADCONX   EQU   *                                                                
*                                                                               
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHEXOUT  DC    V(HEXOUT)                                                        
VLOADER  DC    V(LOADER)                                                        
VRECUP   DC    V(RECUP)                                                         
VDATAMGR DC    V(DATAMGR)                                                       
*                                                                               
* WE WANT TO PASS THE ADDRESS OF ADCONMBL, NOT ITS VALUE.                       
* SO IT MUST NOT HAVE A TYPE ATTRIBUTE OF 'A'                                   
*                                                                               
ADCONMBL DS    0F                  ADCONLST FOR MOBILE                          
         DC    V(GETBROAD)                                                      
VADDAY   DC    V(ADDAY)                                                         
VGETDAY  DC    V(GETDAY)                                                        
VDATCON  DC    V(DATCON)                                                        
*                                                                               
AIO      DS    A                                                                
AIO1LN   DC    A(IO1LN)                                                         
AIO1     DC    A(IO1)                                                           
AIO2LN   DC    A(IO2LN)                                                         
AIO2     DC    A(IO2)                                                           
ADAGY    DC    A(AGYIOA)                                                        
ADCLT    DC    A(CLTIOA)                                                        
ADBUY    DC    A(0)                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'**XWEEKS'                                                    
XWEEKS   DS    XL256               53*4+X'FFFF'                                 
XCOUNTS  DS    XL256                                                            
         DS    0D                                                               
         DC    CL8'**YWEEKS'                                                    
YWEEKS   DS    XL256                                                            
YCOUNTS  DS    XL256                                                            
         DS    0D                                                               
         DC    CL8'*AGYTAB*'                                                    
AGYTAB   DS    XL64                                                             
         EJECT                                                                  
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=6024,             X        
               MACRF=GM,EODAD=INPUTEOF                                          
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,19,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=256'                                   
*                                                                               
HEAD     DC    CL32'AG M CLT PRD STATION EST-LIN '                              
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*SORTREC'                                                    
SORTLEN  DS    F                                                                
*                                                                               
SORTREC  DS    0F                                                               
SORTKEY  DS    XL13                BUY RECORD KEY                               
SORTSEQ  DS    XL4                 SEQUENCE NUMBER                              
SORTDATE DS    XL2                                                              
SORTAGYA DS    CL2                 AGENCY ALPHA                                 
         DS    XL3                 ALIGNMENT                                    
SORTEL   DS    XL256                                                            
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*AGYHDR*'                                                    
AGYIOA   DS    XL500                                                            
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*CLTHDR*'                                                    
CLTIOA   DS    2000C                                                            
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'**IO1***'                                                    
IO1LN    DS    F                                                                
IO1      DS    6100C                                                            
         DS    0D                                                               
         DC    CL8'**IO2***'                                                    
IO2LN    DS    F                                                                
IO2      DS    6100C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'*COMFACS'                                                    
COMFACS  DS    0D                                                               
         DC    V(DATAMGR)                                                       
         DC    70A(0)                                                           
         DS    0D                                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'**UTL **'                                                    
UTL      DC    64X'00'                                                          
*                                                                               
         DS    0D                                                               
         DC    CL8'**SSB **'                                                    
SSB      DC    256X'00'                                                         
         ORG   SSB                                                              
         DC    XL2'00',X'FF',X'02' NO RECOVERY                                  
         ORG                                                                    
         EJECT                                                                  
RCVRHDRD DSECT                                                                  
*PREFIX=DM$                                                                     
       ++INCLUDE DMRCVRHDR                                                      
*PREFIX=                                                                        
       ++INCLUDE SPGENBUY          NOTE BUYREC CONTINUES RCVRHDRD               
         EJECT                                                                  
         PRINT OFF                                                              
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENHIST                                                      
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
* DSECT FOR OUTPUT PRINT LINE                                                   
         ORG   P                                                                
PAGY     DS    CL2                                                              
         DS    C                                                                
PMED     DS    CL1                                                              
         DS    C                                                                
PCLT     DS    CL3                                                              
         DS    C                                                                
PPRD     DS    CL3                                                              
         DS    C                                                                
PSTA     DS    CL7                                                              
         DS    C                                                                
PEST     DS    CL3                                                              
         DS    C                                                                
PLIN     DS    CL3                                                              
         DS    C                                                                
PDATE    DS    CL7                                                              
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSYSELD                                                       
       ++INCLUDE FASSBOFF                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SPBUYHIST 11/16/06'                                      
         END                                                                    
