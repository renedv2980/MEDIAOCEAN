*          DATA SET SRSTX00    AT LEVEL 144 AS OF 05/21/07                      
*PHASE T17300A                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE UNBOOK                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE REPRPUPV                                                               
*INCLUDE REGENVER                                                               
*INCLUDE SQUASHER                                                               
                                                                                
         TITLE 'SRSTX00(T83D00) - REP STATION EXPRESS'                          
***********************************************************************         
*  HISTORY AS OF 07/19/2002                                           *         
*                                                                     *         
* 04/18/2006  BU     SERVICE REQUEST VERSION STX INTERFACE            *         
*                                                                     *         
* 05/04/2006  BU     ADD $$ VALUES TO PROCESSING                      *         
*                                                                     *         
* 05/26/2006  BU     NEW FIELDS PER C.CYGAN / J. YORK                 *         
*                                                                     *         
* 09/13/2006  BU     NEW FIELDS PER J. YORK (CO, CL, CTG, ETC)        *         
*                                                                     *         
* 09/26/2006  BU     REVERT LAST SENT BY TO ORIGINAL CODE, ADD        *         
*                    LAST CONTRACT ACTOR, ETC                         *         
*                                                                     *         
* 04/03/2007  BU     MASTER STATION RECORD PROCESSING: IGNORE         *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*---------------------------------------------------------------------*         
T17300   TITLE 'SRSTX00 ($STX) - STATION EXPRESS INTERFACE'                     
T17300   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL LENWORK,**$STX**,RR=R2,CLEAR=YES                                 
         LR    R7,RB                                                            
         AH    R7,=Y(COMMON-T17300)                                             
         USING COMMON,R7                                                        
*                                                                               
*---------------------*                                                         
* INITIALIZATION CODE *                                                         
*---------------------*                                                         
         USING WORKD,RC                                                         
         ST    RB,AT17300                                                       
*                                                                               
         ST    R2,BASERELO                                                      
         ST    RD,BASERD                                                        
         ST    R1,ASYSPARM                                                      
*                                                                               
*   ORIGINAL PROGRAMMING:  PARAMETER SAVES SUPERCEDED                           
*                                                                               
         MVC   SRPARMS(8*4),0(R1)  SAVE SERVICE REQUEST PARAMETER LIST          
*                                                                               
SRPARMSD USING SRPARMD,SRPARMS                                                  
*                                                                               
         MVC   ATIOB,SRPARMSD.SRQATIOB                                          
         MVC   ATWA,SRPARMSD.SRQATWA                                            
         MVC   ASYSFACS,SRPARMSD.SRQASYSF                                       
         MVC   ATIA,SRPARMSD.SRQATIA                                            
         MVC   ACOMFACS,SRPARMSD.SRQACOMF                                       
*                                                                               
         L     RF,ASYSFACS                                                      
         L     RF,VSSB-SYSFACD(RF)                                              
         USING SSBD,RF                                                          
         MVC   SSBSDATX,SSBSDATE                                                
         MVC   SSBSTIMX,SSBSTIME                                                
         MVC   SSBSYSNX,SSBSYSN4                                                
         XC    SSBSINX,SSBSINX     CLEAR STORAGE FOR RSIN                       
         XC    SSBSSEQ,SSBSSEQ     CLEAR STORAGE FOR SEQ #                      
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         MVC   REPALPHA,TWAAGY                                                  
         DROP  RA                                                               
         USING T173FFD,RA                                                       
MB       USING FAMSGD,FAMSGBLK                                                  
*                                                                               
         L     R9,SRPARMSD.SRQASYSF                                             
         USING SYSFACD,R9                                                       
*                                                                               
         MVC   AUTL,SRPARMSD.SRQAUTL                                            
         L     RF,AUTL                                                          
         MVC   ATBUFF,TBUFF-UTLD(RF)  GETTING OUR MSG FROM TSAR BUFFER          
         L     RF,ATBUFF                                                        
*                                                                               
MAIN0020 EQU   *                                                                
         BAS   RE,INITIAL          SET UP ALL VALUES                            
         BAS   RE,PROCREC          PROCESS RECORD                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*   INITIAL:  ESTABLISH ALL ROUTINES, WORKAREAS                                 
*                                                                               
INITIAL  NTR1                                                                   
         MVC   PGSIZE,=H'4096'     FORCE PAGE SIZE                              
*                                                                               
         LR    RE,RC                                                            
         AH    RE,=Y(IOAREA1-WORKD)                                             
         ST    RE,AIOREC                                                        
         ST    RE,AIO1                                                          
         AH    RE,=Y(LENIO)                                                     
         ST    RE,AIO2                                                          
         AH    RE,=Y(LENIO)                                                     
         ST    RE,AIO3                                                          
         AH    RE,=Y(LENIO)                                                     
         ST    RE,AIO4                                                          
*                                                                               
         L     RE,=V(TWABLD)                                                    
         A     RE,BASERELO                                                      
         ST    RE,VTWABLD                                                       
         L     RE,=V(UNBOOK)                                                    
         A     RE,BASERELO                                                      
         ST    RE,VUNBOOK                                                       
         L     RE,=V(RECUP)                                                     
         A     RE,BASERELO                                                      
         ST    RE,VRECUP                                                        
*                                                                               
         L     RE,=A(SETELEM)                                                   
         A     RE,BASERELO                                                      
         ST    RE,ASETELEM                                                      
         L     RE,=A(ADDDATA)                                                   
         A     RE,BASERELO                                                      
         ST    RE,AADDDATA                                                      
*                                                                               
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   VDMGR,CDATAMGR                                                   
         MVC   VCOLY,CCALLOV                                                    
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VSCANNER,CSCANNER                                                
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VCASHVAL,CCASHVAL                                                
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VADDAY,CADDAY                                                    
         MVC   VPERVERT,CPERVERT                                                
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VPERVAL,CPERVAL                                                  
         MVC   VGLOBBER,CGLOBBER                                                
         MVC   VDEMAND,CDEMAND                                                  
         MVC   VXTRAINF,CXTRAINF                                                
         MVC   VPROTOFF,CPROTOFF                                                
         MVC   VPROTON,CPROTON                                                  
         MVC   CUREDIT,CCUREDIT                                                 
         DROP  R1                                                               
*-------------------------------------------------------------*                 
* SET UP ADDRESSES FOR CORE-RESIDENT PHASES                                     
*-------------------------------------------------------------*                 
         L     R2,=A(PHASES)       R2=A(PHASE LIST)                             
         A     R2,BASERELO                                                      
         LA    R3,APHASES          R3=A(ADDRESS LIST)                           
         LA    R4,PHASESN          R4=MAX NUMBER OF PHASES (CORERES)            
         SR    R0,R0                                                            
         ICM   R0,14,=X'D9000A'                                                 
         LA    R1,DMCB                                                          
         L     RF,VCOLY                                                         
*                                                                               
         GOTO1 (RF),(R1),0,(R0)    <==  SPECIAL FOR BOOKVAL BECAUSE             
         MVC   VBOOKVAL,0(R1)             QBOOKVAL IS EQUATED TO 0              
*                                                                               
INIT0002 ICM   R0,1,0(R2)          ANY ENTRY HERE?                              
         BZ    INIT0004            NONE, SKIP TO THE NEXT ENTRY                 
*                                                                               
         GOTO1 (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
*                                                                               
INIT0004 LA    R2,1(R2)            BUMP TO THE NEXT ENTRY                       
         LA    R3,4(R3)                                                         
         BCT   R4,INIT0002                                                      
*                                                                               
         LR    RE,RB                                                            
         AH    RE,=Y(VROUTS-T17300)                                             
         LA    R0,NUMROUTS                                                      
         SR    RF,RF                                                            
*                                                                               
         LA    R1,VREADSTX                                                      
INIT0010 DS    0H                                                               
         ST    RE,0(R1)                                                         
         STC   RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,INIT0010                                                      
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         GOTO1 =A(INIFALNK),RR=Y   INITIALIZE FALINK BLOCK                      
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R9                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*------------------------------------------------------------------*            
*                                                                               
*------------------------------------------------------------------*            
COMMON   DS    0D                                                               
*        DC    CL8'**FAMAP*'                                                    
                                                                                
       ++INCLUDE SRSTXMAP                                                       
                                                                                
         EJECT                                                                  
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    DS    0H                  SET CC LOW & FAMSGNO                         
         MVC   MB.FAMSGNO,ERROR                                                 
         CLI   *,EFEF                                                           
         B     EXIT                                                             
EXITNO   LTR   RB,RB               SET CC NOT EQUAL                             
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
EXIT     DS    0H                  JUST EXIT                                    
         XIT1                                                                   
*                                                                               
ETOOBIG  MVC   ERROR,=Y(804)                                                    
         B     EXITL                                                            
*                                                                               
EPARMSEQ MVC   ERROR,=Y(210)                                                    
         B     EXITL                                                            
*                                                                               
EINVLEN  MVC   ERROR,=Y(85)                                                     
         B     EXITL                                                            
*                                                                               
EINVUPG  MVC   ERROR,=Y(235)                                                    
         B     EXITL                                                            
*                                                                               
EXITINV  MVC   ERROR,=Y(2)                                                      
         B     EXITL                                                            
*                                                                               
EBADSTA  DS    0H                                                               
         MVC   ERROR,=Y(150)                                                    
         B     ADDFLD                                                           
*                                                                               
EBADBOOK DS    0H                                                               
         MVC   ERROR,=Y(232)                                                    
         B     ADDFLD                                                           
*                                                                               
EBADDEMO DS    0H                                                               
         MVC   ERROR,=Y(233)                                                    
         B     ADDFLD                                                           
*                                                                               
EBADUPNM DS    0H                                                               
         MVC   ERROR,=Y(836)                                                    
         B     ADDFLD                                                           
*                                                                               
ADDFLD   DS    0H                  ADD FIELD IN WORK TO MESSAGE                 
         MVI   MB.FAMSGXTR,C'('                                                 
         ZIC   RE,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MB.FAMSGXTR+1(0),WORK+8                                          
         LA    RE,MB.FAMSGXTR+2(RE)                                             
         MVI   0(RE),C')'                                                       
         B     EXITL                                                            
         EJECT                                                                  
*                                                                               
         GETEL R8,=Y(RCONELEM-RCONREC),ELCODE                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*-------------------------------------------------------------------*           
* PROCREC - MAIN RECORD PROCESSING SECTION.  RECORD DELIVERED BY                
*           SERVICE REQUEST BRANCHES TO SPECIFIC HANDLING ROUTINE               
*-------------------------------------------------------------------*           
PROCREC  NTR1                                                                   
*                                                                               
*   COMPLETE THE MQ CONTROL INFORMATION SECTION                                 
*                                                                               
         XC    SRVFLD1H,SRVFLD1H   INITIALIZE FIELD FOR DUMP DISPLAY            
*                                                                               
         LA    R3,SRVFLD1          SET A(MQ CONTROL HEADER)                     
*                                                                               
         LA    R8,MQDSTIME(R3)                                                  
         GOTOR VHEXOUT,DMCB,SSBSTIMX,(R8),L'SSBSTIMX,=C'TOG'                    
*                                  INSERT TIME                                  
         MVC   MQDSYNAM(4,R3),SSBSYSNX                                          
*                                  INSERT SYSTEM NAME                           
         GOTO1 VDATCON,DMCB,(6,SSBSDATX),(5,MQDSDATE(R3))                       
*                                  INSERT DATE                                  
         L     R2,ATBUFF           SET A(TBUFF)                                 
         LA    RF,1                                                             
         CLC   =C'PASS',0(R2)      PASS INDICATOR?                              
         BE    PREC0060                                                         
         LA    RF,2                                                             
         CLC   =C'FAIL',0(R2)      FAIL INDICATOR?                              
         BE    PREC0060                                                         
         LA    RF,3                                                             
*                                                                               
*   NO I/O:  CAN'T ACCESS REP OR CONTROL RECORDS                                
*                                                                               
***>>>   GOTO1 =A(CTRLSET),DMCB,(R2),RR=Y                                       
*                                  ACCESS CONTROL RECORD                        
*                                                                               
****>>>  GOTO1 =A(READPROF),RR=Y                                                
*                                  ACCESS REP RECORD ON SYSTEM                  
         XC    SSBSINX,SSBSINX     CLEAR RSIN SAVE                              
         MVC   SSBSINX+1(3),RSIN+1-RECVHDR(R2)                                  
*                                  MOVE RSIN FROM RCVRY HDR AFTER               
*                                     STRIPPING TASKID                          
         L     RF,AUTL                                                          
         MVC   SSBSSEQ,TMOCNTS-UTLD(RF)   TRANSACTION SEQUENCE NUMBER           
*                                                                               
         LA    R2,24(R2)           POINT TO RECORD TYPE                         
*                                                                               
         CLI   0(R2),X'0C'         CONTRACT RECORD?                             
         BNE   PREC0020            NO                                           
         GOTO1 =A(DCONDWN),DMCB,(R2),RR=Y                                       
         B     PREC0040            SET UP MQ AND OUTPUT BUFFER                  
*                                                                               
PREC0020 EQU   *                                                                
         CLI   0(R2),X'32'         CONTRACT TYPE RECORD?                        
         BNE   PREC0021            NO                                           
         GOTO1 =A(DCTYDWN),DMCB,(R2),RR=Y                                       
         B     PREC0040            SET UP MQ AND OUTPUT BUFFER                  
*                                                                               
PREC0021 EQU   *                                                                
         CLI   0(R2),X'02'         STATION  RECORD?                             
         BNE   PREC0022            NO                                           
         CLC   =C'MR',RSTAKREP-RSTAKEY(R2)                                      
*                                  'MASTER' STATION RECORD (CONTROL)?           
         BE    PREC0900            YES - SKIP THIS RECORD                       
         GOTO1 =A(DSTADWN),DMCB,(R2),RR=Y                                       
         B     PREC0040            SET UP MQ AND OUTPUT BUFFER                  
*                                                                               
PREC0022 EQU   *                                                                
         CLI   0(R2),X'08'         ADVERT   RECORD?                             
         BNE   PREC0023            NO                                           
         GOTO1 =A(DADVDWN),DMCB,(R2),RR=Y                                       
         B     PREC0040            SET UP MQ AND OUTPUT BUFFER                  
*                                                                               
PREC0023 EQU   *                                                                
         CLI   0(R2),X'1A'         AGENCY   RECORD?                             
         BNE   PREC0024            NO                                           
         GOTO1 =A(DAGYDWN),DMCB,(R2),RR=Y                                       
         B     PREC0040            SET UP MQ AND OUTPUT BUFFER                  
*                                                                               
PREC0024 EQU   *                                                                
         CLI   0(R2),X'06'         CONTRACT RECORD?                             
         BNE   PREC0025            NO                                           
         GOTO1 =A(DSALDWN),DMCB,(R2),RR=Y                                       
         B     PREC0040            SET UP MQ AND OUTPUT BUFFER                  
*                                                                               
PREC0025 EQU   *                                                                
         CLI   0(R2),X'3A'         DEV S/P  RECORD?                             
         BNE   PREC0026            NO                                           
         GOTO1 =A(DDSPDWN),DMCB,(R2),RR=Y                                       
         B     PREC0040            SET UP MQ AND OUTPUT BUFFER                  
*                                                                               
PREC0026 EQU   *                                                                
         CLI   0(R2),X'3B'         DEV C/T  RECORD?                             
         BNE   PREC0027            NO                                           
         GOTO1 =A(DDCTDWN),DMCB,(R2),RR=Y                                       
         B     PREC0040            SET UP MQ AND OUTPUT BUFFER                  
*                                                                               
PREC0027 EQU   *                                                                
         CLI   0(R2),X'01'         REP      RECORD?                             
         BNE   PREC0028            NO                                           
         GOTO1 =A(DREPDWN),DMCB,(R2),RR=Y                                       
         B     PREC0040            SET UP MQ AND OUTPUT BUFFER                  
*                                                                               
PREC0028 EQU   *                                                                
         CLI   0(R2),X'0D'         CLASS    RECORD?                             
         BNE   PREC0029            NO                                           
         GOTO1 =A(DCLSDWN),DMCB,(R2),RR=Y                                       
         B     PREC0040            SET UP MQ AND OUTPUT BUFFER                  
*                                                                               
PREC0029 EQU   *                                                                
         CLI   0(R2),X'0F'         CATEGORY RECORD?                             
         BNE   PREC0030            NO                                           
         GOTO1 =A(DCTGDWN),DMCB,(R2),RR=Y                                       
         B     PREC0040            SET UP MQ AND OUTPUT BUFFER                  
*                                                                               
PREC0030 EQU   *                                                                
         CLI   0(R2),X'07'         GROUP    RECORD?                             
         BNE   PREC0031            NO                                           
         GOTO1 =A(DGRPDWN),DMCB,(R2),RR=Y                                       
         B     PREC0040            SET UP MQ AND OUTPUT BUFFER                  
*                                                                               
PREC0031 EQU   *                                                                
         CLI   0(R2),X'2B'         MARKET   RECORD?                             
         BNE   PREC0032            NO                                           
         GOTO1 =A(DMKTDWN),DMCB,(R2),RR=Y                                       
         B     PREC0040            SET UP MQ AND OUTPUT BUFFER                  
*                                                                               
PREC0032 EQU   *                                                                
         CLI   0(R2),X'04'         OFFICE   RECORD?                             
         BNE   PREC0033            NO                                           
         GOTO1 =A(DOFFDWN),DMCB,(R2),RR=Y                                       
         B     PREC0040            SET UP MQ AND OUTPUT BUFFER                  
*                                                                               
PREC0033 EQU   *                                                                
         CLI   0(R2),X'05'         TEAM     RECORD?                             
         BNE   PREC0034            NO                                           
         GOTO1 =A(DTEMDWN),DMCB,(R2),RR=Y                                       
         B     PREC0040            SET UP MQ AND OUTPUT BUFFER                  
*                                                                               
PREC0034 EQU   *                                                                
*                                                                               
*   CAN THIS MODULE EXIT CLEANLY IF AN UNRECOGNIZED RECORD TYPE                 
*        IS ENCOUNTERED?                                                        
*   TEST THIS OUT.  ELSE DIE.                                                   
*                                                                               
         B     PREC0900                                                         
*                                                                               
         DC    H'0'                UNRECOGNIZED RECORD TYPE                     
*                                                                               
PREC0040 EQU   *                                                                
*                                                                               
         LA    R4,SRVFLD1          SET A(MQ BUFFER)                             
*                                                                               
         L     RF,AFABLK                                                        
         USING FALINKD,RF                                                       
*                                                                               
         L     RE,FALABLD          SET A(1ST FIELD ON NEW SCREEN)               
         DROP  RF                                                               
*                                                                               
         L     RF,ANXTBUFF         END OF DATA                                  
*                                                                               
         SR    RF,RE               CALCULATE DATA LENGTH                        
*                                                                               
         B     PREC0100                                                         
*                                                                               
PREC0060 EQU   *                                                                
         L     RF,AFABLK                                                        
         USING FALINKD,RF                                                       
*                                                                               
         L     R4,FALABLD          SET A(DATA FIELD IN BUFFER)                  
         DROP  RF                                                               
*                                                                               
         MVC   0(4,R4),0(R2)       INSERT PASS / FAIL LITERAL                   
         MVC   SSBSINX,4(R2)       SAVE RSIN                                    
*                                                                               
         LA    RF,4                SET L(DATA: PASS / FAIL LITERAL)             
*                                                                               
         LA    R4,SRVFLD1          SET A(MQ BUFFER)                             
*                                                                               
*   PASS / FAIL:  ALTER Q-NAME                                                  
*                                                                               
         MVC   0(16,R4),=C'MOFAEOTS********'                                    
***      MVC   0(16,R4),=C'DDSTESTINGQUEUE*'                                    
*                                                                               
PREC0100 EQU   *                                                                
*                                                                               
         ST    RF,DUB              SAVE L(DATA) TEMPORARILY                     
         LA    R8,MQDRSIN(R4)                                                   
         GOTOR VHEXOUT,DMCB,SSBSINX,(R8),L'SSBSINX,=C'TOG'                      
*                                  INSERT RSIN FROM RCVRY HDR                   
*                                     OR PASS / FAIL FIELD                      
         LA    R8,MQDSSEQ(R4)                                                   
         EDIT  SSBSSEQ,(4,(R8)),FILL=0,DUB=MYDUB2                               
*                                                                               
         L     RF,DUB              RESET L(DATA)                                
         LA    RF,MQCTLEN(RF)      ADD MQ CONTROL DATA LEN TO TOTAL             
         LR    R5,RF                                                            
         XC    CORRELID,CORRELID   CLEAR CORRELATION ID FIELD                   
         MVC   CORRELID(4),SSBSINX                                              
*                                                                               
*   TEST TO PROVIDE FIXED RSIN TO MO                                            
****     MVC   CORRELID(4),=X'000000FF'                                         
*                                  INSERT TRANSACTION ID                        
         MVC   CORRELID+4(4),SSBSTIMX                                           
*                                  INSERT TIME                                  
         MVC   CORRELID+8(4),SSBSDATX                                           
*                                  INSERT DATE                                  
         MVC   CORRELID+12(4),SSBSYSNX                                          
*                                  INSERT SYSTEM NAME                           
                                                                                
*                                                                               
*   R4 IS USED TO PERMIT CHECKING THE SETUP DURING TESTING....                  
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CMQIO-COMFACSD(RF)                                            
         GOTOR (RF),DMCB,=CL8'PUT',(R4),(R5),0,0,DUB,C'CORL',CORRELID           
*                                                                               
PREC0900 EQU   *                                                                
         XIT1                                                                   
MYDUB2   DS    D                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*-------------------------------------------------------------------*           
* INITIALIZE FALINK                                                             
*-------------------------------------------------------------------*           
INIFALNK NTR1  BASE=*,LABEL=*                                                   
         LA    R0,FABLK                                                         
         LA    R1,FALINKDL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         OI    SRVSERVH+1,X'01'    SERVICE REQUEST ALWAYS MODIFIED              
         OI    SRVSERVH+6,X'80'                                                 
*                                                                               
         TM    MISCFLG1,MF1GLOB                                                 
         BNZ   *+10                                                             
         XC    VERSION,VERSION     CLEAR PC VERSION - UNLESS FROM GLOB          
*                                                                               
         LA    R2,FABLK                                                         
         ST    R2,AFABLK           FOR OTHER OVERLAYS                           
         USING FALINKD,R2                                                       
*                                                                               
         XCEFL SRVFLD1,3000        CLEAR PLENTY OF BUFFER SPACE                 
*                                                                               
         LA    R1,SRVFLD1          INITIALIZE MQ IN BUFFER SPACE                
*                                                                               
*   THIS IS THE TEST QUEUE ACCESSED BY JOHN'S DISPLAY FACILITY                  
*        AND MUST BE CHANGED TO THE 'PRODUCTION / TEST' MO Q'S                  
*        THIS QUEUE NAME WILL REQUIRE A CHANGE IF TRANSACTION                   
*        IS 'PASS' OR 'FAIL'                                                    
*                                                                               
         MVC   0(16,R1),=C'MOFARECS********'                                    
***      MVC   0(16,R1),=C'DDSTESTINGQUEUE*'                                    
*                                                                               
         LA    R8,MQCTLEN          SET LENGTH OF CONTROL                        
****     STCM  RF,15,16(R1)        SAVE LENGTH IN MQ HDR                        
         EDIT  (R8),(4,16(R1)),FILL=0                                           
*                                                                               
         LA    R1,SRVFLD1+MQCTLEN DISPLACE PAST MQ CONTROL                      
         ST    R1,FLNK             USE THIS AS A(BUFFER)                        
         ST    R1,ANXTBUFF         SET A(NEXT BUFFER) ALSO                      
*                                                                               
***                                                                             
***>>>   MVC   0(8,R1),=X'5FC20051C0240000'                                     
*                                  SET FIELD HEADER                             
*                                                                               
*   TO GET A FIELD BY FIELD DISPLAY, RATHER THAN THE FINAL FORMAT               
*        OF THE DATA, UNCOMMENT THE FOLLOWING INSTRUCTION AND                   
*        REASSEMBLE.  THIS WILL ACTIVATE THE OPTION CONTAINED IN                
*        'UEO'.                                                                 
*                                                                               
****>>>> MVC   8(36,R1),=C'UEO B=FDHA04000000B=04HA13870963%B%E'                
*                                                                               
         ST    R1,FALABLD          STORE A(1ST FIELD ON NEW SCREEN)             
*                                                                               
         MVI   EXPANDME,C'N'       TURN OFF EXPAND FLAG                         
         CLI   FALCONO-FALSCRD(R1),EXPANDED                                     
         BNE   INIF0020                                                         
         MVI   EXPANDME,C'Y'       TURN ON EXPAND  FLAG                         
INIF0020 EQU   *                                                                
         MVC   FALTBLD,VTWABLD         A(TWABLD)                                
         L     R1,ACOMFACS             A(SWITCH)                                
         L     R1,CSWITCH-COMFACSD(R1)                                          
         ST    R1,FALASWCH                                                      
         ST    R1,SWITCH                                                        
         LA    R1,*                                                             
         A     R1,=A(RECEIVE-(*-4))    A(MY RECEIVE ROUTINE)                    
         ST    R1,FALARCV                                                       
         LA    R1,*                                                             
         A     R1,=A(SEND-(*-4))       A(MY SEND ROUTINE)                       
         ST    R1,FALASND                                                       
***JRD   LA    R1,TRANSLATE            A(ELEMENT TRANSLATION ROUTINE)           
***JRD   ST    R1,FALATRN                                                       
         LA    R1,*                                                             
         A     R1,=A(BREAK-(*-4))      A(BREAK ROUTINE)                         
         ST    R1,FALASTP                                                       
         LA    R1,*                                                             
         A     R1,=A(RESUME-(*-4))     A(RESUME ROUTINE)                        
         ST    R1,FALARSM                                                       
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R1,FAMSGBLK             A(MESSAGE BLOCK)                         
         ST    R1,FALAMSG                                                       
         LA    R1,FACON                A(CONTROL FIELD BUFFER)                  
         ST    R1,FALACON                                                       
         L     R1,ATWA                                                          
         AH    R1,=Y(SVFALINK-T173FFD) A(FALINK SAVED STORAGE)                  
         ST    R1,FALASVE                                                       
         LA    R1,*                                                             
         A     R1,=A(FAMAP-(*-4))      A(MAP TABLE)                             
         ST    R1,FALAMAP                                                       
         ST    R1,AMAPTAB          FOR OTHER OVERLAYS                           
         MVC   FALAPGS,TWAPGS                                                   
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
TWAPGS   DC    AL4(FALATMS)                                                     
         LTORG                                                                  
         EJECT                                                                  
*------------------------------------------------------------------*            
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
*------------------------------------------------------------------*            
VROUTS   NTR1  BASE=*,LABEL=*                                                   
         SRL   RF,24                                                            
         B     ROUTTAB(RF)                                                      
*                                                                               
ROUTTAB  B     READ                                                             
         B     SEQ                                                              
         B     HIGH                                                             
         B     ADD                                                              
         B     WRITE                                                            
         B     GETREC                                                           
         B     PUTREC                                                           
         B     ADDREC                                                           
NUMROUTS EQU   (*-ROUTTAB)/4                                                    
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'                                               
         B     DIRCTRY                                                          
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         B     DIRCTRY                                                          
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
DIRCTRY  CLI   UPDATE,C'Y'                                                      
         BNE   *+8                                                              
         OI    DMINBTS,X'80'                                                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDMGR,DMCB,(DMINBTS,COMMAND),=C'REPDIR',KEYSAVE,KEY,0            
         B     DMCHECK                                                          
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
*-------------------------------------------------------------------*           
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
FILE     CLI   UPDATE,C'Y'                                                      
         BNE   *+8                                                              
         OI    DMINBTS,X'80'                                                    
         MVC   AIOREC,0(R1)                                                     
         LA    R0,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R0,KEY                                                           
         GOTO1 VDMGR,DMCB,(DMINBTS,COMMAND),=C'REPFILE',(R0),          X        
               AIOREC,(0,DMWORK)                                                
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                  DATA MANAGER ERRORS AND EXIT                                 
*-------------------------------------------------------------------*           
DMCHECK  DS    0H                                                               
         MVI   DMINBTS,X'00'                                                    
         MVI   UPDATE,C'N'                                                      
         MVC   DMBYTE,DMCB+8                                                    
*                                                                               
         NC    DMBYTE,DMOUTBTS                                                  
         BZ    DMEXITOK                                                         
         B     DMEXITOK                                                         
*                                                                               
DMEXITL  DS    0H                  SET CC LOW & FAMSGNO                         
         CLI   *,EFEF                                                           
         B     *+6                                                              
DMEXITOK DS    0H                                                               
         CR    RB,RB               SET CC EQUAL                                 
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
* XARSE- EXTENDED PARSE                                                         
*                                                                               
*   PARSE GENERIC RETURN FOR AND SEND TO PC BASED ON TABLE                      
*                                                                               
*    P1 - A(GENERIC OUTPUT STREAM)                                              
*    P2 - A(NULL TERMINATED TABLE)                                              
*                                                                               
*-------------------------------------------------------------------*           
XARSE    NTR1  BASE=*,LABEL=*                                                   
*&&DO                                                                           
         L     R8,0(R1)            A(OUTPUT STREAM)                             
         L     R6,4(R1)            A(TABLE)                                     
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,1,2(R8)                                                       
         BZ    EXITL               NO SUBDIVISIONS                              
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,1,2(R8)                                                       
         MHI   R2,2                                                             
         LA    R2,3(R2,R8)         INDEX TO DATA                                
*                                                                               
XARSE002 DS    0H                                                               
         LA    R5,3(R8)                                                         
         ICM   R4,1,2(R8)                                                       
         NI    MISCFLG1,EFEF-MF1TMPB2                                           
XARSE010 DS    0H                                                               
         LA    R3,4(R6)            CHECK FOR DATA IN TABLE                      
XARSE012 DS    0H                                                               
         CLI   0(R3),0             END OF TYPES?                                
         BE    XARSE100            YES                                          
         CLC   0(1,R3),1(R5)       TYPE MATCH?                                  
         BE    XARSE020            YES                                          
         LA    R3,XTABLQ(R3)                                                    
         B     XARSE012                                                         
*                                                                               
XARSE020 DS    0H                                                               
         CLI   1(R3),2             GENERIC ELEMENT ADD?                         
         BH    XARSE100            NO                                           
*                                                                               
         CLI   1(R3),1             SPACES CHECK?                                
         BNE   XARSE022            NO                                           
*                                                                               
         ZIC   RE,0(R5)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SPACES                                                   
         BNH   XARSE034                                                         
*                                                                               
XARSE022 DS    0H                                                               
         TM    MISCFLG1,MF1TMPB2   HEADER SENT?                                 
         BO    XARSE030            YES                                          
*                                                                               
         ICM   RF,15,0(R6)         GET ELEMENT HEADER ADDRESS                   
         BZ    XARSE028                                                         
*                                                                               
         A     RF,BASERELO                                                      
         GOTO1 ASETELEM,DMCB,AFABLK,(RF),0                                      
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
XARSE028 DS    0H                                                               
         OI    MISCFLG1,MF1TMPB2   HEADER SENT                                  
*                                                                               
XARSE030 DS    0H                                                               
         ZIC   R0,0(R5)            GET DATA LENGTH                              
         ICM   RF,15,4(R3)         GET MAPTABLE ADDRESS                         
         A     RF,BASERELO                                                      
*                                                                               
         CLI   1(R3),2             MAPCODE ONLY?                                
         BNE   XARSE032            NO                                           
*                                                                               
         CLI   0(R2),C'Y'                                                       
         BNE   XARSE034                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,(RF),0,0                                    
         B     XARSE034                                                         
*                                                                               
XARSE032 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,(RF),(R2),(R0)                              
*                                                                               
XARSE034 DS    0H                                                               
*                                                                               
XARSE100 DS    0H                                                               
         ZIC   R0,0(R5)            BUMP DISPLACEMENT INTO DATA                  
         AR    R2,R0                                                            
*                                                                               
         LA    R5,2(R5)            NEXT ENTRY                                   
         BCT   R4,XARSE010                                                      
*                                                                               
         CLI   0(R2),0                                                          
         BNE   XARSE002                                                         
*                                                                               
XARSEX   DS    0H                                                               
*&&                                                                             
         B     EXITOK                                                           
         EJECT                                                                  
*-------------------------------------------------------------------*           
* LITERALS AND CONSTANTS                                                        
*-------------------------------------------------------------------*           
EFEF     EQU   X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
BREAK    NTR1  BASE=*,LABEL=*                                                   
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
RESUME   NTR1  BASE=*,LABEL=*                                                   
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
SEND     NTR1  BASE=*,LABEL=*                                                   
         OC    SENDROUT,SENDROUT                                                
         BZ    SENDX                                                            
*                                                                               
         MVC   ASETELEM,0(R1)      SAVE FALINK ROUTINE ADDRESSES                
         MVC   AADDDATA,4(R1)                                                   
*                                                                               
         L     RF,SENDROUT                                                      
         BASR  RE,RF                                                            
         BL    EXITL                                                            
*                                                                               
         TM    MISCFLG1,MF1DATA    ANY DATA IN BUFFER?                          
         BZ    SENDX               NO - DON'T CLOSE                             
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,FALADNE,FALADNE,0                           
SENDX    DS    0H                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
*                                                                               
*-------------------------------------------------------------------*           
RECEIVE  NTR1  BASE=*,LABEL=*                                                   
         B     EXITOK                                                           
*-------------------------------------------------------------------*           
* VERSION INFO - VERSION FIELD                                                  
*-------------------------------------------------------------------*           
         DS    0D                                                               
         USING *,RB                                                             
FVERVER  LR    RB,RF                                                            
         CLC   FPARMS+8,=F'4'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         L     RF,FPARMS+4                                                      
         MVC   VERSION,0(RF)        COPY VERSION                                
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* STAWEB REQUEST - SET STATION WEB MODE STATUS                                  
*-------------------------------------------------------------------*           
         DS    0D                                                               
         USING *,RB                                                             
STAWEB   LR    RB,RF                                                            
*                                                                               
         OI    MISCFLG1,MF1STWEB                                                
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* CONTRACT DETAIL IN CONTRACT LIST                                              
*-------------------------------------------------------------------*           
         DS    0D                                                               
         USING *,RB                                                             
CONDET   LR    RB,RF                                                            
*                                                                               
         OI    MISCFLG2,BCDETAL                                                 
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* CONTRACT NUMBERS ONLY IN CONTRACT LIST                                        
*-------------------------------------------------------------------*           
         DS    0D                                                               
         USING *,RB                                                             
CONONLY  LR    RB,RF                                                            
*                                                                               
         OI    MISCFLG2,BC#ONLY                                                 
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* NO DETAIL CONTRACT DETAIL IN STATION WEB DOWNLOAD                             
*-------------------------------------------------------------------*           
         DS    0D                                                               
         USING *,RB                                                             
NODETAIL LR    RB,RF                                                            
*                                                                               
         OI    MISCFLG2,DCNODET                                                 
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* NO ORDER COMMENTS ON CONTRACT                                                 
*-------------------------------------------------------------------*           
         DS    0D                                                               
         USING *,RB                                                             
NOORDCOM LR    RB,RF                                                            
*                                                                               
         OI    MISCFLG2,DCNOCOM                                                 
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   NO I/O PERMITTED:  NO ACCESS TO CONTROL FILE                                
*                                                                               
*&&DO                                                                           
*                                                                               
*  CTRLSET:  SETS REP SE # BY LOOKING IN CONTROL FILE.                          
*                                                                               
CTRLSET  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,0(R1)            SET A(RECOVERY RECORD HEADER)                
CTRL0040 EQU   *                                                                
         L     R3,AUTL                                                          
         MVI   4(R3),X'0A'         SET UTL SE TO CTFILE                         
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'           FIND CONTROL FILE ID RECORD                  
*                                                                               
         USING RECVHDR,R2                                                       
*                                                                               
*   COULD HAVE TAKEN SYSTEM NUMBER DIRECTLY FROM HEADER.  THEN IT               
*        WOULD HAVE BEEN NECESSARY TO DRAG THE POWER CODE OUT OF                
*        EACH AND EVERY RECORD TYPE BEING PROCESSED.  THIS CENTRAL-             
*        IZES THE PROCESS.                                                      
*                                                                               
         MVC   WORK+23(2),RUSER    INSERT ID NUMBER FROM RECOVERY HDR           
*                                                                               
         GOTO1 VDMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',WORK,AIOREC                 
*                                  RETRIEVE CONTROL FILE RECORD                 
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         L     R1,AIOREC                                                        
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                                                             
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
CTRL0120 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   CTRL0160            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    CTRL0200            YES                                          
CTRL0160 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BNE   CTRL0120            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
CTRL0200 EQU   *                                                                
         ST    R1,FULL             SAVE A(X'21' ELEMENT)                        
         L     R4,FULL             RESET A(X'21' ELEMENT)                       
*                                     WITH REP UTL CODE                         
         GOTO1 SWITCH,DMCB,(3(R4),X'FFFFFFFF'),0                                
         MVC   4(1,R3),3(R4)       OVERRIDE CONTROL FILE UTL                    
         CLI   4(R1),0             SWITCHED OKAY?                               
         BE    CTRL0280            YES - NOW FIND POWER CODE                    
         CLI   4(R1),2             SYSTEM NOT OPENED?                           
         BE    *+6                                                              
         DC    H'0'                OTHERWISE DEATH                              
CTRL0250 GOTO1 SWITCH,DMCB,(X'0A',X'FFFFFFFF'),0                                
         B     CTRLNO                                                           
*                                                                               
CTRL0280 EQU   *                                                                
         L     R1,AIOREC                                                        
         LA    R1,28(R1)           FIND X'06' AGENCY ID ELEMENT                 
CTRL0320 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                YES - NOT FOUND????!!!                       
         CLI   0(R1),X'06'         AGENCY ID ELEMENT?                           
         BE    CTRL0360            YES                                          
         ZIC   RF,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R1,RF                                                            
         B     CTRL0320            GO BACK FOR NEXT                             
CTRL0360 EQU   *                                                                
         MVC   POWERCDE,2(R1)      SAVE POWER CODE                              
CTRL0400 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
         B     CTRL0600            EXIT OKAY                                    
CTRLNO   LTR   RB,RB                                                            
CTRL0600 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*&&DO                                                                           
*  READPROF:  RETRIEVES PROFILES FROM REP SYSTEM SWITCHED TO.                   
*                                                                               
READPROF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
K        USING RREPKEY,KEY         GET PARENT REP CODE                          
         XC    K.RREPKEY,K.RREPKEY                                              
         MVI   K.RREPKTYP,X'01'                                                 
         MVC   K.RREPKREP,POWERCDE                                              
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RREPKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                NO REP RECORD                                
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R6,AIOREC                                                        
         LA    R6,RREPELEM-RREPREC(R6)                                          
         CLI   0(R6),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                LOST REP ELEMENT                             
*                                                                               
         USING RREPELEM,R6                                                      
         MVC   PARALPHA,RREPPAR                                                 
         MVC   REPNAME,RREPNAME                                                 
         MVC   REPADDR,RREPADDR                                                 
*---------------------------------------------------------*                     
* SAVE THE MASTER REP TEMPORARILY FOR THE INITIAL DOWNLOAD                      
* IN INVSEQ                                                                     
*---------------------------------------------------------*                     
         MVC   INVSEQ,POWERCDE     REP COMPANY                                  
         CLC   RREPMAST,=C'  '     NO MASTER CONTROL                            
         BNH   INIT0012                                                         
         CLC   RREPMAST,=X'FFFF'   THIS IS THE MASTER                           
         BE    INIT0012                                                         
*                                                                               
         MVC   INVSEQ,RREPMAST     SET THE MASTER                               
*                                                                               
INIT0012 DS    0H                                                               
         MVC   INVSEQ+2(1),RREPPROF+11                                          
         DROP  R6                                                               
*                                                                               
         GOTO1 GETPROF,DMCB,('RREPQCNT',CONPROFS)                               
         GOTO1 GETPROF,DMCB,('RREPQSOM',SOMPROFS)                               
*                                                                               
         XIT1                                                                   
*&&                                                                             
         EJECT                                                                  
*-------------------------------------------------------------------*           
* GETPROF - GET THE PROGRAM PROFILES                                            
*   INPUT:   P1  BYTE 1      PROGRAM #                                          
*                BYTE 2-4    A(PROFILE AREA) CL10                               
*                                                                               
*   OUPUT:   PROFILES IN PROFILE AREA                                           
*                                                                               
*-------------------------------------------------------------------*           
GETPROF  NTR1                                                                   
         ZIC   R3,0(R1)                                                         
         L     R2,0(R1)                                                         
         XC    0(10,R2),0(R2)                                                   
*                                                                               
         L     R6,AIOREC                                                        
         LA    R6,RREPELEM-RREPREC(R6)                                          
GPROF02  CLI   0(R6),0                                                          
         BE    GETPROFX            NO PROFILE ELEMENT                           
         CLI   0(R6),X'04'                                                      
         BE    GPROF04                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GPROF02                                                          
*                                                                               
GPROF04  DS    0H                                                               
         USING RREPPGMP,R6                                                      
         ZIC   RF,RREPPGM#         # OF PROGRAM UNITS (LOOP COUNTER)            
         LA    R6,RREPPGM1                                                      
         USING RREPPGM1,R6                                                      
GPROF10  CLM   R3,1,RREPPGM1       CORRECT PROGRAM?                             
         BE    GPROF20             YES                                          
         LA    R6,RREPPGML(R6)                                                  
         BCT   RF,GPROF10                                                       
         B     GETPROFX            NOT FOUND. USE DEFAULTS.                     
*                                                                               
GPROF20  MVC   0(10,R2),RREPPGM1   SAVE PROGRAM PROFILES UNIT                   
         DROP  R6                                                               
GETPROFX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* CONTRACT TYPE DOWNLOAD                                                        
*-------------------------------------------------------------------*           
DCTYDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            SET A(INP REC IN TBUFF)                      
         ST    R8,AIOREC           SAVE A(INPUT RECORD)                         
*                                                                               
         USING RCTYREC,R8                                                       
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,CNTDATA,0                                   
         GOTO1 AADDDATA,DMCB,AFABLK,CNTCTYEL,RCTYKCTY,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CNTDSCEL,RCTYDESC,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CNTREPEL,RCTYKREP,2                         
         GOTO1 =A(ADDDUMMY),RR=Y                                                
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* STATION  DOWNLOAD                                                             
*-------------------------------------------------------------------*           
DSTADWN  NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            SET A(INP REC IN TBUFF)                      
         ST    R8,AIOREC           SAVE A(INPUT RECORD)                         
*                                                                               
         USING RSTAREC,R8                                                       
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,STADATA,0                                   
         GOTO1 AADDDATA,DMCB,AFABLK,STAREPEL,RSTAKREP,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,STASTAEL,RSTAKSTA,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,STAMKTEL,RSTAMKT,20                         
         GOTO1 AADDDATA,DMCB,AFABLK,STAAFFEL,RSTAAFFL,3                         
         EDIT  RSTACHAN,(3,WORK+20),ALIGN=LEFT                                  
         GOTO1 AADDDATA,DMCB,AFABLK,STACHNEL,WORK+20,(R0)                       
         CLI   RSTATRAF,C' '                                                    
         BNH   DSTA0020                                                         
         MVC   TRAFSYS,RSTATRAF                                                 
         GOTO1 AADDDATA,DMCB,AFABLK,STATRAF,RSTATRAF,0                          
*&&DO                                                                           
*                                                                               
*   USE OF THIS STATION RECORD IS NOT ASSOCIATED WITH A CONTRACT.  AS           
*        A RESULT, EOP DATA IS NOT RELEVANT HERE.                               
*                                                                               
         MVI   EOPREQ,C'N'                                                      
         CLI   RSTATRAF,C'B'                                                    
         BNE   *+8                                                              
         MVI   EOPREQ,C'Y'                                                      
         CLI   RSTATRAF,C'W'                                                    
         BNE   *+8                                                              
         MVI   EOPREQ,C'Y'                                                      
         CLI   RSTATRAF,C'J'                                                    
         BNE   *+8                                                              
         MVI   EOPREQ,C'Y'                                                      
         CLI   RSTATRAF,C'C'                                                    
         BNE   *+8                                                              
         MVI   EOPREQ,C'Y'                                                      
         DROP  R8                                                               
*&&                                                                             
DSTA0020 EQU   *                                                                
         MVI   ECYN,0                                                           
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'08'        EXTRA DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSTAXXEL,R8                                                      
*              IS THIS AN EC STATION (Y/N)                                      
         TM    RSTAXOPT,X'80'                                                   
         BZ    *+8                                                              
         MVI   ECYN,1                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,STAECYN,ECYN,0                              
                                                                                
*              EOP REQUIRED (Y/N)                                               
         MVI   BYTE2,0                                                          
         CLI   RSTAOPT9,C'Y'                                                    
         BNE   *+8                                                              
         MVI   BYTE2,1                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,STAEOPRQ,BYTE2,0                            
*                                                                               
*&&DO                                                                           
*   PROFILES FROM THE REP RECORD ARE NOT AVAILABLE.  AS SUCH, THIS              
*        TEST IS MEANINGLESS, AND NO VALUE WILL BE SENT                         
*                                                                               
         MVI   SOMTEMP,C'N'                                                     
         TM    SOMPROF,X'80'                                                    
         BZ    *+8                                                              
         MVI   SOMTEMP,C'Y'                                                     
         GOTO1 AADDDATA,DMCB,AFABLK,STAEMLYN,SOMTEMP,0                          
*&&                                                                             
*                                                                               
*   LOAD AN ARRAY OF PROFILE VALUES                                             
*                                                                               
         MVI   WORK2,C'N'          SET FIRST TO 'N'                             
         MVC   WORK2+1(39),WORK2   SET REMAINDER OF STRING                      
         MVC   WORK2(9),RSTAOPTS   MOVE FIRST 9 OPTS - THESE ARE ALL            
*                                     ACTUAL VALUES                             
         MVC   BYTE2,RSTAOPTA      UNLOAD OPTIONS BYTE                          
         LA    R1,WORK2+9          SET A(NEXT ARRAY POSITION)                   
         BAS   RE,SETARRAY         PROCESS OPTION BYTE                          
         MVC   BYTE2,RSTAOPTB      UNLOAD OPTIONS BYTE                          
         LA    R1,WORK2+17         SET A(NEXT ARRAY POSITION)                   
         BAS   RE,SETARRAY         PROCESS OPTION BYTE                          
         MVC   BYTE2,RSTAOPTC      UNLOAD OPTIONS BYTE                          
         LA    R1,WORK2+25         SET A(NEXT ARRAY POSITION)                   
         BAS   RE,SETARRAY         PROCESS OPTION BYTE                          
*                                                                               
         DROP  R8                                                               
*                                                                               
         LA    R0,40               SET LENGTH                                   
         GOTO1 AADDDATA,DMCB,AFABLK,STAPROFS,WORK2,(R0)                         
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'25'        SALES ASSISTANT ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   DSTA0060                                                         
*                                                                               
DSTA0040 DS    0H                                                               
         ZIC   R0,1(R8)            LENGTH                                       
         SHI   R0,RSTAADD-RSTAEML                                               
         USING RSTAEML,R8                                                       
         GOTO1 AADDDATA,DMCB,AFABLK,STAEMLEL,RSTAADD,(R0)                       
         DROP  R8                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DSTA0040                                                         
*                                                                               
DSTA0060 DS    0H                                                               
         GOTO1 =A(ADDDUMMY),RR=Y                                                
*                                                                               
*   TEST                                                                        
***      LA    RF,SRVFLD1                                                       
***      L     RE,AIOREC                                                        
***      LA    R1,1                                                             
***      DC    H'0'                                                             
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* SETARRAY:  UNWIND OPTION BITS, AND INSERT INTO PROFILE ARRAY                  
*        PRIOR TO ADDING DATA TO FALINK                                         
*        BYTE2 =  PROFILE BYTE                                                  
*        R1    -> NEXT ARRAY SLOT                                               
*-------------------------------------------------------------------*           
SETARRAY NTR1                                                                   
         LA    R0,7                LOOP 7 TIMES AFTER FIRST FLAG                
SARR0020 EQU   *                                                                
         TM    BYTE2,X'80'         BIT TURNED ON?                               
         BNO   SARR0040            NO                                           
         MVI   0(R1),C'Y'          YES - TURN ON OUTPUT                         
SARR0040 EQU   *                                                                
         LA    R1,1(R1)            BUMP TO NEXT OUTPUT SLOT                     
         ZIC   RF,BYTE2            RESET BYTE                                   
         SLL   RF,1                SLIDE BYTE UP 1 BIT                          
         STC   RF,BYTE2            RETURN BYTE                                  
         BCT   R0,SARR0020         CHECK NEXT 7 BITS                            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* ADVERT   DOWNLOAD                                                             
*-------------------------------------------------------------------*           
DADVDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            SET A(INP REC IN TBUFF)                      
         ST    R8,AIOREC           SAVE A(INPUT RECORD)                         
*                                                                               
         USING RADVREC,R8                                                       
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,ADVDATA,0                                   
         GOTO1 AADDDATA,DMCB,AFABLK,ADVCODEL,RADVKADV,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,ADVREPEL,RADVKREP,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,ADVNAMEL,RADVNAME,0                         
         GOTO1 =A(ADDDUMMY),RR=Y                                                
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* AGENCY   DOWNLOAD                                                             
*-------------------------------------------------------------------*           
DAGYDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            SET A(INP REC IN TBUFF)                      
         ST    R8,AIOREC           SAVE A(INPUT RECORD)                         
*                                                                               
         USING RAGY2REC,R8                                                      
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,AGYDATA,0                                   
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,AGYCODEL,RAGK2AGY,0                         
         CLC   RAGK2AOF,SPACES                                                  
         BNH   DAGY0010            NO AGENCY OFFICE                             
         GOTO1 AADDDATA,DMCB,AFABLK,AGYOFFEL,RAGK2AOF,0                         
DAGY0010 EQU   *                                                                
         GOTO1 AADDDATA,DMCB,AFABLK,AGYREPEL,RAGK2REP,0                         
*                                                                               
         MVI   ELCODE,X'1F'        AGENCY DETAIL ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   DAGY0020            NOT FOUND                                    
         USING RAG2ELEM,R8                                                      
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,AGYNAMEL,RAG2NAM2,0                         
         DROP  R8                                                               
DAGY0020 EQU   *                                                                
         L     R8,AIOREC                                                        
*                                                                               
         L     R3,AIOREC           SET R3 FOR LOOKUP                            
         LA    R3,RAGY2FXE-RAGY2REC(R3)                                         
*                                  SET A(FAX ELEMENT)                           
DAGY0040 DS    0H                                                               
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0             EOR?                                         
         BE    DAGY0900            YES                                          
         CLI   0(R3),X'20'         EXPANDED ADDRESS ELEMENT?                    
         BNE   DAGY0040            NO                                           
*                                                                               
         USING RAGY2AE1,R3                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,AGYAD1EL,RAGY2AD1,0                         
         MVI   BYTE2,0             SET 'USE AGYAD2EL'                           
         CLC   RAGY2AD2,SPACES     ANY ADDRESS LINE2?                           
         BNH   DAGY0060            NO  -                                        
         GOTO1 AADDDATA,DMCB,AFABLK,AGYAD2EL,RAGY2AD2,0                         
         MVI   BYTE2,1             SET 'USE AGYAD3EL'                           
DAGY0060 EQU   *                                                                
         XC    WORK,WORK           CLEAR A WORKSPACE                            
         MVC   WORK,RAGY2CTY       INSERT CITY INTO WORK                        
*                                                                               
         LA    RE,WORK             FLOAT STATE AND ZIP                          
         LA    RF,36-1(RE)         CALCULATE A(LAST CHARACTER)                  
DAGY0080 DS    0H                                                               
         OI    0(RF),X'40'         SET BIN ZERO TO SPACE                        
         CLI   0(RF),X'40'         FIELD = SPACE?                               
         BH    *+12                NO  - LAST CHARACTER FOUND                   
         BCTR  RF,0                YES - BACK UP 1 POSITION                     
         CR    RF,RE               CHECK START = END                            
         BH    DAGY0080            NOT AT START YET                             
*                                                                               
         MVC   3(2,RF),RAGY2STE    INSERT STATE                                 
         MVC   7(10,RF),RAGY2ZIP   INSERT ZIP CODE                              
*                                                                               
         DROP  R3                                                               
         CLI   BYTE2,0                                                          
         BNE   DAGY0100            NO  - USE AGY LINE 3                         
         GOTO1 AADDDATA,DMCB,AFABLK,AGYAD2EL,WORK,0                             
         B     DAGY0120                                                         
DAGY0100 EQU   *                                                                
         GOTO1 AADDDATA,DMCB,AFABLK,AGYAD3EL,WORK,0                             
DAGY0120 EQU   *                                                                
*&&DO                                                                           
*      - NO LOOKUP FOR THIS DATA - IN RESTX00, THE CONTRACT ITSELF              
*        PROVIDES SEVERAL ITEMS OF DATA TO SATISFY THESE LOOKUPS.               
*        THIS CANNOT BE DONE HERE, AS THE CONTRACT RECORD IS NOT                
*        THE FEED.                                                              
*                                                                               
*  LOOKUP CONTYPE RECORD TO GET FORMAT INFO                                     
*                                                                               
         XC    KEY,KEY                                                          
K        USING RCTYREC,KEY                                                      
         MVI   K.RCTYKTYP,RCTYKTYQ     REC TYPE                                 
         MVC   K.RCTYKREP,REPALPHA     REP CODE                                 
         MVC   K.RCTYKCTY,0(R5)        CON TYPE                                 
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCTYKEY),KEYSAVE                                           
         BNE   DAGY0900                                                         
         GOTO1 VGETREC,AIO2                                                     
*                                                                               
         L     R8,AIO2                                                          
         LA    R8,RCTYELEM-RCTYREC(R8)                                          
AGADD030 DS    0H                                                               
         ZIC   R0,1(R8)                                                         
         AR    R8,R0                                                            
         CLI   0(R8),0             EOR?                                         
         BE    DAGY0900            YES                                          
         CLI   0(R8),X'10'         FORMAT ELEMENT?                              
         BNE   AGADD030            NO                                           
*                                                                               
         USING RCTYFEL,R8                                                       
         TM    RCTYFPRA,X'08'           CARE OF AGENCY OVERRIDE?                
         BNZ   AGADD040                 YES                                     
*                                                                               
         L     RE,AIO1                                                          
R        USING RAGYREC,RE                                                       
         TM    R.RAGYFLAG,X'20'    CARE OF AGENCY?                              
         BZ    AGADD040            NO                                           
         DROP  R                                                                
*                                                                               
         XC    KEY,KEY                                                          
K        USING RADVKEY,KEY                                                      
         MVI   K.RADVKTYP,X'08'                                                 
         MVC   K.RADVKREP,REPALPHA                                              
         MVC   K.RADVKADV,0(R4)                                                 
         OC    K.RADVKADV,SPACES                                                
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RADVKEY),KEYSAVE                                           
         BNE   AGADD040                                                         
*                                                                               
         GOTO1 VGETREC,AIO1                                                     
         L     RE,AIO1                                                          
         USING RADVREC,RE                                                       
         MVC   W.ADDRLN0(L'RADVNAME),RADVNAME                                   
         DROP  RE                                                               
*                                                                               
         OC    W.ADDRLN0,SPACES                                                 
         CLC   W.ADDRLN0,SPACES                                                 
         BNH   AGADD080                                                         
*                                                                               
         LA    RE,W.ADDRLN0+L'RADVNAME-1                                        
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
*                                                                               
         MVC   2(9,RE),=C'-CARE OF:'                                            
         B     AGADD080                                                         
*                                                                               
AGADD040 DS    0H                                                               
         TM    RCTYFA1S,X'80'           REPLACE AGY ADDRESS 1?                  
         BNO   AGADD050                 NO - NEXT FIELD                         
*                                                                               
         XC    W.ADDRLN1,W.ADDRLN1                                              
         MVI   BYTE,C'G'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   AGADD050                                                         
*                                                                               
         L     RE,FULL                                                          
         ZIC   R1,1(RE)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   W.ADDRLN1(0),3(RE)                                               
*                                                                               
AGADD050 DS    0H                                                               
         TM    RCTYFA2S,X'80'           REPLACE AGY ADDRESS 2?                  
         BNO   AGADD060                 NO - NEXT FIELD                         
*                                                                               
         XC    W.ADDRLN2,W.ADDRLN2                                              
         MVI   BYTE,C'H'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   AGADD060                                                         
*                                                                               
         L     RE,FULL                                                          
         ZIC   R1,1(RE)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   W.ADDRLN2(0),3(RE)                                               
*                                                                               
AGADD060 DS    0H                                                               
         TM    RCTYFA3S,X'80'           REPLACE AGY ADDRESS 3?                  
         BNO   AGADD070                 NO - NEXT FIELD                         
*                                                                               
         XC    W.ADDRLN3,W.ADDRLN3                                              
         MVI   BYTE,C'I'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   AGADD070                                                         
*                                                                               
         L     RE,FULL                                                          
         ZIC   R1,1(RE)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   W.ADDRLN3(0),3(RE)                                               
*                                                                               
AGADD070 DS    0H                                                               
         TM    RCTYFANS,X'80'           REPLACE AGY NAME?                       
         BNO   AGADD080                 NO - NEXT FIELD                         
*                                                                               
         XC    W.ADDRNAM,W.ADDRNAM                                              
         MVI   BYTE,C'E'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   AGADD080                                                         
*                                                                               
         L     RE,FULL                                                          
         ZIC   R1,1(RE)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   W.ADDRNAM(0),3(RE)                                               
*                                                                               
AGADD080 DS    0H                                                               
*                                                                               
*&&                                                                             
DAGY0900 DS    0H                                                               
         GOTO1 =A(ADDDUMMY),RR=Y                                                
         B     EXITOK                                                           
*----------------------------                                                   
*&&DO                                                                           
TXTSEEK  NTR1                                                                   
         L     R8,AIO2                                                          
         LA    R8,RCTYELEM-RCTYREC(R8)                                          
TS010    DS    0H                                                               
         ZIC   R0,1(R8)                                                         
         AR    R8,R0                                                            
         CLI   0(R8),0             EOR?                                         
         BE    EXITL               YES                                          
         CLI   0(R8),X'12'         FORMAT ELEMENT?                              
         BNE   TS010               NO                                           
         CLC   BYTE,2(R8)                                                       
         BNE   TS010                                                            
*                                                                               
         ST    R8,FULL                                                          
         B     EXITOK                                                           
*                                                                               
         DROP  R8                                                               
*----------------------------                                                   
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
****>>>> AGENCY CODE                                                            
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* S/P      DOWNLOAD                                                             
*-------------------------------------------------------------------*           
DSALDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            SET A(INP REC IN TBUFF)                      
         ST    R8,AIOREC           SAVE A(INPUT RECORD)                         
*                                                                               
         USING RSALREC,R8                                                       
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,SALDATA,0                                   
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,SALCODEL,RSALKSAL,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,SALREPEL,RSALKREP,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,SALNAMEL,RSALNAME,0                         
         CLC   RSALTEL,SPACES      ANY TELEPHONE NUMBER?                        
         BNH   DSAL0020            NO                                           
         GOTO1 AADDDATA,DMCB,AFABLK,SALTELEL,RSALTEL,0                          
DSAL0020 EQU   *                                                                
         CLC   RSALFAX,SPACES      ANY FAX NUMBER?                              
         BNH   DSAL0040            NO                                           
         GOTO1 AADDDATA,DMCB,AFABLK,SALFAXEL,RSALFAX,0                          
DSAL0040 EQU   *                                                                
         GOTO1 AADDDATA,DMCB,AFABLK,SALTEMEL,RSALTEAM,0                         
*                                                                               
*   CAN'T GIVE DIVISION OR TEAM NAME                                            
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,SALOFFEL,RSALOFF,0                          
         MVI   BYTE2,C'N'                                                       
         CLI   RSALMGR,C'Y'        MANAGER?                                     
         BNE   DSAL0060            NO                                           
         MVI   BYTE2,C'Y'                                                       
DSAL0060 EQU   *                                                                
         GOTO1 AADDDATA,DMCB,AFABLK,SALMGREL,BYTE2,0                            
         GOTO1 =A(ADDDUMMY),RR=Y                                                
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* DEV S/P  DOWNLOAD                                                             
*-------------------------------------------------------------------*           
DDSPDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            SET A(INP REC IN TBUFF)                      
         ST    R8,AIOREC           SAVE A(INPUT RECORD)                         
*                                                                               
         USING RDSPREC,R8                                                       
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,DSPDATA,0                                   
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,DSPCODEL,RDSPKSAL,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,DSPREPEL,RDSPKREP,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,DSPNAMEL,RDSPNAME,0                         
         CLC   RDSPTEL,SPACES      ANY TELEPHONE NUMBER?                        
         BNH   DDSP0020            NO                                           
         GOTO1 AADDDATA,DMCB,AFABLK,DSPTELEL,RDSPTEL,0                          
DDSP0020 EQU   *                                                                
         CLC   RDSPFAX,SPACES      ANY FAX NUMBER?                              
         BNH   DDSP0040            NO                                           
         GOTO1 AADDDATA,DMCB,AFABLK,DSPFAXEL,RDSPFAX,0                          
DDSP0040 EQU   *                                                                
         GOTO1 =A(ADDDUMMY),RR=Y                                                
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* DEV C/T  DOWNLOAD                                                             
*-------------------------------------------------------------------*           
DDCTDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            SET A(INP REC IN TBUFF)                      
         ST    R8,AIOREC           SAVE A(INPUT RECORD)                         
*                                                                               
         USING RDCTREC,R8                                                       
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,DCTDATA,0                                   
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,DCTCTYEL,RDCTKCTY,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,DCTREPEL,RDCTKREP,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,DSPNAMEL,RDCTDESC,0                         
         GOTO1 =A(ADDDUMMY),RR=Y                                                
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* REP / COMPANY DOWNLOAD                                                        
*-------------------------------------------------------------------*           
DREPDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            SET A(INP REC IN TBUFF)                      
         ST    R8,AIOREC           SAVE A(INPUT RECORD)                         
*                                                                               
         USING RREPREC,R8                                                       
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,QRPDATA,0                                   
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,QRPCODEL,RREPKREP,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,QRPNAMEL,RREPNAME,0                         
         CLC   =X'FFFF',RREPMAST   MASTER REP?                                  
         BE    DREP0040            YES - NO 'MASTER CODE'                       
         CLC   =C'  ',RREPMAST     MASTER/ SUB?                                 
         BNH   DREP0040            NO  - NOT A MASTER CODE                      
         GOTO1 AADDDATA,DMCB,AFABLK,QRPMASEL,RREPMAST,0                         
*                                  NO  - ADD MASTER CODE                        
DREP0040 EQU   *                                                                
         GOTO1 =A(ADDDUMMY),RR=Y                                                
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* CLASS    DOWNLOAD                                                             
*-------------------------------------------------------------------*           
DCLSDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            SET A(INP REC IN TBUFF)                      
         ST    R8,AIOREC           SAVE A(INPUT RECORD)                         
*                                                                               
         USING RCLSREC,R8                                                       
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,QCLDATA,0                                   
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,QCLCODEL,RCLSKCLS,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,QCLNAMEL,RCLSNAME,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,QCLREPEL,RCLSKREP,0                         
         GOTO1 =A(ADDDUMMY),RR=Y                                                
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* CATEGORY DOWNLOAD                                                             
*-------------------------------------------------------------------*           
DCTGDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            SET A(INP REC IN TBUFF)                      
         ST    R8,AIOREC           SAVE A(INPUT RECORD)                         
*                                                                               
         USING RCTGREC,R8                                                       
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,QCTDATA,0                                   
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,QCTCODEL,RCTGKCTG,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,QCTNAMEL,RCTGNAME,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,QCTREPEL,RCTGKREP,0                         
         GOTO1 =A(ADDDUMMY),RR=Y                                                
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* GROUP    DOWNLOAD                                                             
*-------------------------------------------------------------------*           
DGRPDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            SET A(INP REC IN TBUFF)                      
         ST    R8,AIOREC           SAVE A(INPUT RECORD)                         
*                                                                               
         USING RGRPREC,R8                                                       
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,QGPDATA,0                                   
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,QGPCODEL,RGRPKGRP,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,QGPNAMEL,RGRPNAME,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,QGPSGPEL,RGRPSBNM,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,QGPREPEL,RGRPKREP,0                         
         GOTO1 =A(ADDDUMMY),RR=Y                                                
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* MARKET   DOWNLOAD                                                             
*-------------------------------------------------------------------*           
DMKTDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            SET A(INP REC IN TBUFF)                      
         ST    R8,AIOREC           SAVE A(INPUT RECORD)                         
*                                                                               
         USING RMKTREC,R8                                                       
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,QMKDATA,0                                   
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,QMKCODEL,RMKTKMKT,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,QMKNAMEL,RMKTNAME,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,QMKREPEL,RMKTKREP,0                         
         GOTO1 =A(ADDDUMMY),RR=Y                                                
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* OFFICE   DOWNLOAD                                                             
*-------------------------------------------------------------------*           
DOFFDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            SET A(INP REC IN TBUFF)                      
         ST    R8,AIOREC           SAVE A(INPUT RECORD)                         
*                                                                               
         USING ROFFREC,R8                                                       
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,QOFDATA,0                                   
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,QOFCODEL,ROFFKOFF,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,QOFNAMEL,ROFFNAME,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,QOFREPEL,ROFFKREP,0                         
         GOTO1 =A(ADDDUMMY),RR=Y                                                
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* TEAM     DOWNLOAD                                                             
*-------------------------------------------------------------------*           
DTEMDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            SET A(INP REC IN TBUFF)                      
         ST    R8,AIOREC           SAVE A(INPUT RECORD)                         
*                                                                               
         USING RTEMREC,R8                                                       
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,QTMDATA,0                                   
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,QTMCODEL,RTEMKTEM,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,QTMDNMEL,RTEMDVNM,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,QTMTNMEL,RTEMNAME,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,QTMREPEL,RTEMKREP,0                         
         GOTO1 =A(ADDDUMMY),RR=Y                                                
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* LIST CONTRACT DOWNLOAD                                                        
*-------------------------------------------------------------------*           
LCONDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            SET A(CON REC IN TBUFF)                      
         ST    R8,ACONREC          SAVE A(RCONREC)                              
*                                                                               
         USING RCONREC,R8                                                       
*                                                                               
         GOTO1         ,DMCB,AFABLK,CONDATA,0                                   
         GOTO1 ASETELEM                                                         
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONNUMEL,WORK,0                             
*                                                                               
         GOTO1 VDATCON,DMCB,(3,RCONDATE),(19,WORK)                              
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLSEL,WORK,0                             
         GOTO1 VDATCON,DMCB,(3,RCONDATE+3),(19,WORK)                            
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLEEL,WORK,0                             
         GOTO1 AADDDATA,DMCB,AFABLK,CONBYREL,RCONBUYR,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONOFFEL,RCONKOFF,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONAGYEL,RCONKAGY,0                         
         CLC   RCONKAOF,SPACES                                                  
         BNH   LCD0022                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,CONAOFEL,RCONKAOF,0                         
*                                                                               
LCD0022  DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONADVEL,RCONKADV,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSTAEL,RCONKSTA,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSALEL,RCONSAL,0                          
         GOTO1 AADDDATA,DMCB,AFABLK,CONCTYEL,RCONTYPE,0                         
*                                                                               
         CLC   RCONPRD,SPACES                                                   
         BNH   LCD0030                                                          
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONPRDEL,RCONPRD,L'RCONPRD                  
         B     LCD0032                                                          
*                                                                               
LCD0030  DS    0H                                                               
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'05'        GET PRODUCT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   LCD0032                                                          
*                                                                               
         USING RCONEXEL,R8                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONPRDEL,RCONEXPR,L'RCONEXPR                
         DROP  R8                                                               
*                                                                               
LCD0032  DS    0H                                                               
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'18'        DEVELOPMENT INFO ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   LCD0040                                                          
*                                                                               
         USING RCONDVEL,R8                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONDCTEL,RCONDVCT,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONDSPEL,RCONDVSP,0                         
         DROP  R8                                                               
*                                                                               
LCD0040  DS    0H                                                               
*                                                                               
         GOTO1 =A(CKSIDE),RR=Y     CHECK REP/STATION SIDE                       
         GOTO1 AADDDATA,DMCB,AFABLK,CONSIDEL,BYTE,0                             
*                                                                               
         GOTO1 =A(CKSEND),RR=Y     GET REP/STATION LAST SENT/CF                 
*                                                                               
         GOTO1 =A(CKVER),RR=Y      GET LAST VERSION                             
         MVC   CONVERSV,BYTE                                                    
         GOTO1 AADDDATA,DMCB,AFABLK,CONVEREL,BYTE,0                             
*                                                                               
         GOTO1 =A(CKMOD),RR=Y      GET LAST MOD                                 
         GOTO1 AADDDATA,DMCB,AFABLK,CONMODEL,BYTE,0                             
*                                                                               
         GOTO1 =A(CKWIP),RR=Y      GET WIP STATUS                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONWIPEL,BYTE,0                             
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONREPEL,REPALPHA,0                         
*                                                                               
         GOTO1 =A(BOOKS),RR=Y      PUT OUT BOOKS AND COMMENTS S                 
*                                                                               
         XC    WORK,WORK                                                        
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   LCD0042                                                          
         USING RCONIEL,R8                                                       
                                                                                
         CLC   RCONIADV,SPACES                                                  
         BNH   LCD0041A                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONADV,RCONIADV,0                           
LCD0041A EQU   *                                                                
         CLC   RCONIPRD,SPACES                                                  
         BNH   LCD0041B                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONPRD1,RCONIPRD,0                          
LCD0041B EQU   *                                                                
         CLC   RCONIPR2,SPACES                                                  
         BNH   LCD0041C                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONPRD2,RCONIPR2,0                          
LCD0041C EQU   *                                                                
         MVC   WORK(10),RCONXEST                                                
         OC    WORK(10),SPACES                                                  
         CLC   WORK(10),SPACES                                                  
         BNE   *+10                                                             
         MVC   WORK(4),RCONIEST                                                 
         GOTO1 AADDDATA,DMCB,AFABLK,CONEST,WORK,0                               
         DROP  R8                                                               
                                                                                
LCD0042  EQU   *                                                                
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   LCD0042A                                                         
         USING RCONXEL,R8                                                       
         GOTO1 AADDDATA,DMCB,AFABLK,CONTRAF,RCONTRF,0                           
         DROP  R8                                                               
                                                                                
LCD0042A EQU   *     HAS THIS CONTRACT EVER BEEN EC'D (Y/N)                     
         MVI   BYTE2,0                                                          
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'15'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   BYTE2,1                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,CONECFLG,BYTE2,0                            
                                                                                
LCD0045  DS    0H                                                               
*                                                                               
*&&DO                                                                           
*     NO ADDITIONAL RECORD READS ALLOWED.                                       
*                                                                               
         MVI   K.RCON8EID,X'03'    READ DEMO KEY                                
         XC    K.RCON8EAG(L'KEY-(RCON8EAG-RCON8ETP)),K.RCON8EAG                 
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(RCON8EAG-RCON8ETP),KEYSAVE                                   
         BNE   LCD0050                                                          
*                                                                               
         L     RE,AIO2                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'INV'                                                   
         DROP  RE                                                               
*                                                                               
         XC    WORK+60(50),WORK+60                                              
         MVC   WORK+60(3),K.RCON8EMO                                            
         CLI   WORK+60+1,C'T'      FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   WORK+60+1,C'I'                                                   
*                                                                               
         GOTO1 VDEMOCON,DMCB,(1,WORK+60),(9,WORK),(0,AIO2)                      
         ZIC   R0,0(R1)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONDEMEL,WORK,(R0)                          
*                                                                               
LCD0050  DS    0H                                                               
         MVC   KEY,KEYSAVE         RESET KEY                                    
*                                                                               
LCD0100  DS    0H                                                               
*                                                                               
         MVI   K.RCON8EID,X'FF'    READ NEXT RECORD SET                         
         XC    K.RCON8EAG(L'KEY-(RCON8EAG-RCON8ETP)),K.RCON8EAG                 
         GOTO1 VHIGH                                                            
         B     LCD0010                                                          
         DROP  K,KS,R5                                                          
*&&                                                                             
LCDX     DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* BOOKS - GET COMMENTS AND BOOK DATE AND TYPE INFO                              
*-------------------------------------------------------------------*           
BOOKS    NTR1  BASE=*,LABEL=*                                                   
         USING RCONREC,R8                                                       
                                                                                
         XC    WORK,WORK                                                        
         XC    WORK2,WORK2                                                      
* --------------- COMMENTS ---------------------*                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   BOOK20                                                           
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BNE   BOOK30                                                           
                                                                                
BOOK10   DS    0H                                                               
         ZIC   R2,1(R8)                                                         
         AHI   R2,-3                                                            
         LTR   R2,R2                                                            
         BM    BOOK15                                                           
         EX    R2,*+8                                                           
         B     *+10                                                             
         OC    2(0,R8),SPACES                                                   
         LA    R2,1(R2)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONCOMNT,2(R8),(R2)                         
BOOK15   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    BOOK10                                                           
         B     BOOK30                                                           
                                                                                
BOOK20   L     R8,AIOREC                                                        
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   BOOK30                                                           
                                                                                
BOOK23   DS    0H                                                               
         ZIC   R2,1(R8)                                                         
         AHI   R2,-3                                                            
         LTR   R2,R2                                                            
         BM    BOOK25                                                           
         EX    R2,*+8                                                           
         B     *+10                                                             
         OC    2(0,R8),SPACES                                                   
         LA    R2,1(R2)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONCOMNT,2(R8),(R2)                         
BOOK25   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    BOOK23                                                           
* --------------- BOOK'S ------------------------*                              
BOOK30   DS    0H                                                               
         MVC   SAVLBLS,SPACES                                                   
         USING RCONREC,R8                                                       
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'0B'                                                     
         BAS   RE,GETEL                                                         
         BNE   BOOK40              NO BOOK RENAMES                              
         ZIC   RE,1(R8)                                                         
         SH    RE,=H'3'            OVEHEAD AND 1 FOR EX                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SAVLBLS(0),2(R8)                                                 
                                                                                
BOOK40   L     R8,AIOREC                                                        
         MVI   ELCODE,X'12'        SAR ELEMENT                                  
         BAS   RE,GETEL                                                         
         BNE   BOOK90                                                           
                                                                                
         XC    WORK2,WORK2                                                      
                                                                                
         USING RSARXEL,R8                                                       
         LA    R0,6                NUMBER OF BOOKS TO COPY                      
         LA    RE,RSARXBKS                                                      
         LA    RF,SAVBOOKS                                                      
         MVC   2(3,RF),0(RE)       MOVE BOOK                                    
         LA    RF,5(RF)                                                         
         LA    RE,3(RE)                                                         
         BCT   R0,*-14                                                          
                                                                                
DSARBK   CLC   RSARXBKS(2),=C'DR'                                               
         BNE   DSARBK05                                                         
         MVC   WORK,SPACES                                                      
         MVC   WORK+8(2),=C'DR'                                                 
         B     DSARBKX                                                          
                                                                                
DSARBK05 CLC   RSARXBKS(2),=C'PP'                                               
         BNE   DSARBK10                                                         
         MVC   WORK,SPACES                                                      
         MVC   WORK+8(2),=C'PP'                                                 
         B     DSARBKX                                                          
                                                                                
DSARBK10 LA    R2,WORK+8                                                        
         LA    R3,SAVBOOKS                                                      
         LA    R4,SAVLBLS                                                       
                                                                                
DSARBK20 CLI   0(R3),0                USER DEFINED BOOK?                        
         BNE   DSARBK30               YES                                       
                                                                                
         OC    0(L'SAVBOOK,R3),0(R3)  ANY BOOK?                                 
         BZ    DSARBK60               NO MORE BOOKS                             
                                                                                
         CLI   1(R3),0             SPECIAL BOOK TYPE USED FOR THIS BOOK         
         BE    DSARBK21                                                         
         XC    0(5,R4),0(R4)                                                    
         MVC   0(1,R4),1(R3)                                                    
                                                                                
DSARBK21 DS    0H                                                               
         MVI   WORK,16+8                                                        
         XC    WORK2,WORK2                                                      
         MVC   WORK2(8),WORK                                                    
         GOTO1 VUNBOOK,DMCB,(1,2(R3)),WORK2,0,(C'+',=CL6' ')                    
*TEST                                                                           
*        CLC   2(3,R3),=XL3'606807'                                             
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*TEST                                                                           
         ZIC   RE,WORK2                                                         
         LA    RE,WORK2(RE)                                                     
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
                                                                                
         CLI   0(RE),C')'                                                       
         BNE   DSARBK26                                                         
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     DSARBK26                                                         
                                                                                
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
                                                                                
DSARBK26 LA    RF,WORK2+8                                                       
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WORK2+8                                                  
         LA    R2,1(RE,R2)                                                      
         B     DSARBK50                                                         
                                                                                
DSARBK30 MVC   0(L'SAVBOOK,R2),0(R3) SHOW THE USER-DEFINED LABEL                
                                                                                
         LA    R2,L'SAVBOOK-1(R2)    REMOVE AS MUCH SPACES AS WE CAN            
DSARBK33 CLI   0(R2),C' '                                                       
         BH    DSARBK36                                                         
         BCTR  R2,0                                                             
         B     DSARBK33                                                         
DSARBK36 LA    R2,1(R2)                                                         
                                                                                
DSARBK50 EQU   *                                                                
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)            BUMP TO THE NEXT BOOK                        
                                                                                
*TEST                                                                           
*        CLC   2(3,R3),=XL3'606807'                                             
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*TEST                                                                           
         LA    R3,L'SAVBOOK(R3)                                                 
         LA    R4,L'SAVLBL(R4)                                                  
         LA    RE,SAVBOOKS+L'SAVBOOKS                                           
         CR    R3,RE                                                            
         BL    DSARBK20                                                         
                                                                                
DSARBK60 BCTR  R2,0                REMOVE THE LAST COMMA                        
         MVI   0(R2),C' '                                                       
                                                                                
*        TRY AND SEPARATE OUT BOOKS                                             
         LA    R2,WORK+8                                                        
         LA    R3,7                                                             
                                                                                
DSARBK65 MVC   MYDUB,SPACES                                                     
         MVC   MYDUB(6),0(R2)                                                   
         CLI   MYDUB+5,C','                                                     
         BNE   *+8                                                              
         MVI   MYDUB+5,C' '                                                     
         CLI   MYDUB+6,C','                                                     
         BNE   *+8                                                              
         MVI   MYDUB+6,C' '                                                     
         GOTO1 AADDDATA,DMCB,AFABLK,CONBOOKS,MYDUB,0                            
                                                                                
         LA    R2,5(R2)                                                         
         CLI   0(R2),C','                                                       
         BNE   *+12                                                             
         LA    R2,1(R2)                                                         
         B     DSARBK67                                                         
                                                                                
         CLI   1(R2),C','                                                       
         BNE   BOOK90                                                           
         LA    R2,2(R2)                                                         
DSARBK67 BCT   R3,DSARBK65                                                      
                                                                                
DSARBKX  DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONBOOKS,WORK+8,0                           
         DROP  R8                                                               
                                                                                
BOOK90   DS    0H                                                               
BOOKXIT  B     EXIT                                                             
                                                                                
SAVBOOKS DS    0CL(7*5)                                                         
SAVBOOK  DS    7CL5                                                             
                                                                                
SAVLBLS  DS    0CL(7*5)                                                         
SAVLBL   DS    7CL5                                                             
                                                                                
MYDUB    DS    D                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* DEMOS - GET AND OUTPUT DEMOS FROM SAR OR BOP                                  
*-------------------------------------------------------------------*           
DEMOS    NTR1  BASE=*,LABEL=*                                                   
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'12'        SAR ELEMENT                                  
         BAS   RE,GETEL                                                         
         BNE   DEMO0020                                                         
*                                                                               
         LA    R2,7                NUMBER OF DEMOS                              
         LA    R8,RSARXDEM-RSARXEL(R8)                                          
*                                                                               
DEMO0010 DS    0H                                                               
         OC    0(3,R8),0(R8)                                                    
         BZ    DEMO0040                                                         
*                                                                               
         L     RE,AIO2                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'INV'                                                   
         DROP  RE                                                               
*                                                                               
         XC    WORK+60(50),WORK+60                                              
         MVC   WORK+60(3),0(R8)                                                 
         CLI   WORK+60+1,C'T'      FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   WORK+60+1,C'I'                                                   
         MVI   WORK+60,0           NO PRIMARY                                   
*                                                                               
         GOTO1 VDEMOCON,DMCB,(1,WORK+60),(9,WORK),(0,AIO2)                      
         ZIC   R0,0(R1)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONDEMEL,WORK,(R0)                          
*                                                                               
         LA    R8,3(R8)                                                         
         BCT   R2,DEMO0010                                                      
*                                                                               
DEMO0020 DS    0H                                                               
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'10'        BOP ELEMENT                                  
         BAS   RE,GETEL                                                         
         BNE   DEMO0040                                                         
*                                                                               
         USING RCONBPEL,R8                                                      
         CLI   RCONBPDM,X'FF'      SKIP OLD CONTRACTS                           
         BNE   DEMO0040                                                         
*                                                                               
         LA    R2,(L'RCONBPDM-1)/3 NUMBER OF DEMOS                              
         LA    R8,RCONBPDM+1                                                    
         DROP  R8                                                               
*                                                                               
DEMO0030 DS    0H                                                               
         OC    0(3,R8),0(R8)                                                    
         BZ    DEMO0040                                                         
*                                                                               
         L     RE,AIO2                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'INV'                                                   
         DROP  RE                                                               
*                                                                               
         XC    WORK+60(50),WORK+60                                              
         MVC   WORK+60(3),0(R8)                                                 
         CLI   WORK+60+1,C'T'      FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   WORK+60+1,C'I'                                                   
         MVI   WORK+60,0           NO PRIMARY                                   
*                                                                               
         GOTO1 VDEMOCON,DMCB,(1,WORK+60),(9,WORK),(0,AIO2)                      
         ZIC   R0,0(R1)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONDEMEL,WORK,(R0)                          
*                                                                               
         LA    R8,3(R8)                                                         
         BCT   R2,DEMO0030                                                      
*                                                                               
DEMO0040 DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        DOWNLOAD BUYLINE DEMOS                                                 
*-------------------------------------------------------------------*           
DBUYDEM  NTR1  BASE=*,LABEL=*                                                   
*&&DO                                                                           
         XC    WORK2(64),WORK2     CLEAR A BUILD AREA                           
         L     R8,AIOREC                                                        
         SR    R3,R3               USE R3 AS STORAGE AND FLAG                   
*                                                                               
         MVI   ELCODE,X'0D'        AGENCY DEMO ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DBDM0020            NO 0D ELEMENT: ONLY REP?                     
*                                                                               
         CLI   1(R8),X'12'         0D ELEMENT FOUND: OLD?                       
         BE    EXITOK              YES - OLD ELEMENT - SKIP                     
*                                                                               
         CLI   1(R8),X'22'                                                      
         BE    EXITOK              YES - OLD ELEMENT - SKIP                     
*                                                                               
*   0D FOUND                                                                    
*                                                                               
         LR    R3,R8               YES - SAVE A(X'0D' ELEMENT)                  
DBDM0020 EQU   *                                                                
         L     R8,AIOREC           LOOK FOR AN X'0E' ELEMENT                    
         MVI   ELCODE,X'0E'        REP    DEMO ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    DBDM0040            0E FOUND:  PROCESS AS REP                    
*                                                                               
         LTR   R3,R3               ANY X'0D' FOUND?                             
         BZ    EXITOK              NO  - NO 0D OR 0E IN BUYLINE                 
*                                                                               
         LR    R8,R3               NO 0E FOUND: RESET A(0D)                     
DBDM0040 EQU   *                                                                
         MVC   SAVELTID,0(R8)      SAVE ELEMENT CODE BEING PROCESSED            
*                                                                               
         ZIC   R6,1(R8)            ELEMENT LENGTH                               
         AR    R6,R8               END OF ELEMENT                               
*                                                                               
*   ONLY THE FIRST DEMO WILL BE PROCESSED                                       
*                                                                               
         AHI   R8,2                FIRST DEMO                                   
         USING RBUYDMCV,R8                                                      
*                                                                               
*   X'0D' AND X'0E' HAVE SAME LAYOUT: USING X'0D' LABELS                        
*                                                                               
DBDM0060 DS    0H                                                               
         CR    R8,R6               PAST END OF ELEMENT?                         
         BNL   DBDM0180            YES - FINISHED                               
*                                                                               
         OC    RBUYDMCT,RBUYDMCT   NO DEMO CATEGORY                             
         BZ    DBDM0180            SKIP                                         
*                                                                               
         CLI   RBUYDMCT,C'('       USER DEFINED DEMO CATEGORY?                  
         BE    DBDM0180            SKIP                                         
*                                                                               
         CLC   RBUYDMDM,=X'FFFFFFFF'                                            
         BE    DBDM0180            NO VALUE SKIP                                
*                                                                               
         BRAS  RE,DBUNDEM                                                       
*                                                                               
         L     RF,FULL                                                          
         BCTR  RF,0                SET FOR EX (MOVE BY LENGTH)                  
         EX    RF,DBDM0080         MOVE BY LENGTH                               
         B     DBDM0100                                                         
DBDM0080 MVC   WORK2(0),WORK       MOVE BY LENGTH                               
DBDM0100 EQU   *                                                                
         LA    R4,WORK2                                                         
         AR    R4,RF               SET A(LAST CHARACTER)                        
         LA    R4,2(R4)            SET A(NEXT FIELD)                            
         MVC   0(3,R4),=C'AGY'     INSERT 'AGENCY DEMO'                         
         CLI   SAVELTID,X'0D'      AGENCY ELEMENT?                              
         BNE   DBDM0120            NO  - REP ELEMENT                            
*                                  YES - CHECK IN ELT FOR PREV VALUE            
         CLC   RBUYDM2M,=X'FFFFFFFF'                                            
*                                  ANY PREVIOUS VALUE?                          
         BE    DBDM0160            NO                                           
         B     DBDM0140            YES - ADD '*' & ADJUST LENGTH                
DBDM0120 EQU   *                                                                
         MVC   0(3,R4),=C'REP'     INSERT 'REP    DEMO'                         
         LTR   R3,R3               IS THERE AN X'0D'?                           
         BNZ   DBDM0140            YES - THIS IS AN OVERRIDE                    
         CLC   RBUYDM2M,=X'FFFFFFFF'                                            
*                                  NO  - ANY PREVIOUS VALUE?                    
         BE    DBDM0160            NO  - THIS IS NOT AN OVERRIDE                
*                                  YES - THIS IS AN OVERRIDE                    
DBDM0140 EQU   *                                                                
         MVI   3(R4),C'*'          YES - INDICATE VALUE IS OVERRIDE             
         LA    R4,1(R4)            ADD 1 FOR '*'                                
DBDM0160 EQU   *                                                                
         LA    R4,4(R4)            SET A(NEXT FIELD FOR DEMO VALUE)             
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RBUYDMDM    LOAD DEMO VALUE AS HEXOUT                    
         EDIT  (P5,WORK),(6,0(R4)),1,ALIGN=LEFT,ZERO=NOBLANK                    
*                                                                               
*   THIS MAY BE FINE-TUNED TO SEPARATE IMPS FROM RATINGS, TO ELIMINATE          
*        THE DECIMAL POINT FOR IMPS                                             
*   THERE PROBABLY ARE NO IMPS BROUGHT IN HERE ANYWAY.                          
*                                                                               
         AR    R4,R0               ADD LENGTH OF EDITED FIELD                   
*                                                                               
         LA    RF,WORK2                                                         
         SR    R4,RF               CALCULATE FIELD LENGTH                       
*                                                                               
*   ADD THIS FIELD AS A MAP ELEMENT                                             
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYADEMO,WORK2,(R4)                         
*                                                                               
*                                                                               
*   EXIT FROM PROCESSING AN AGENCY DEMO                                         
*                                                                               
DBDM0180 DS    0H                                                               
         AHI   R8,L'RBUYDMCV                                                    
         B     EXITOK              ONLY PROCESS A SINGLE ITEM                   
**                                                                              
***>>>   B     DBDM0060            THIS IS LOOP TRANSFER                        
         DROP  R8                                                               
*                                                                               
*-------------------------------------------------------------------*           
DBUNDEM  NTR1                                                                   
         L     RE,AIO4                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'INV'                                                   
         DROP  RE                                                               
*                                                                               
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVC   WORK+60(3),0(R8)                                                 
         CLI   WORK+60+1,C'T'      FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   WORK+60+1,C'I'                                                   
*                                                                               
         GOTO1 VDEMOCON,DMCB,(1,WORK+60),(9,WORK),(0,AIO4)                      
*                                                                               
         XC    FULL,FULL           STORE LENGTH OF OUTPUT SINCE IT IS           
         MVC   FULL+3(1),0(R1)     VARIABLE                                     
*                                                                               
*&&                                                                             
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* BUCKET - ADD BUCKETS AND OUTPUT ORDER TOTAL ELEMENT                           
*-------------------------------------------------------------------*           
BUCKET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZAP   WORK(10),=PL1'0'                                                 
*                                                                               
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'03'        SEE IF THERE IS MONEY                        
         BAS   RE,GETEL                                                         
         BNE   BUCK020                                                          
*                                                                               
BUCK010  DS    0H                                                               
         SR    RE,RE                                                            
         ICM   RE,15,RCONBKAM-RCONBKEL(R8)                                      
         CVD   RE,DUB                                                           
         AP    WORK(10),DUB                                                     
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    BUCK010                                                          
*                                                                               
BUCK020  DS    0H                                                               
         ZAP   WORK+10(10),=PL1'0'                                              
*                                                                               
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'63'        SEE IF THERE IS TRADE                        
         BAS   RE,GETEL                                                         
         BNE   BUCK040                                                          
*                                                                               
BUCK030  DS    0H                                                               
         SR    RE,RE                                                            
         ICM   RE,15,RCONBKAM-RCONBKEL(R8)                                      
         CVD   RE,DUB                                                           
         AP    WORK+10(10),DUB                                                  
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    BUCK030                                                          
*                                                                               
BUCK040  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* CKSIDE - CHECK IF CONTRACT IS ON REP/STATION/BOTH SIDE                        
*          RETURNS R/S/B IN 'BYTE'                                              
*-------------------------------------------------------------------*           
CKSIDE   NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,C'R'           DEAFULT TO REP SIDE                          
         L     R8,ACONREC          CHECK SEND STATUS                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKSIDEX             NO 20 ELEM, ON REP SIDE                      
         USING RCONSEND,R8                                                      
*                                                                               
         TM    RCONSENF,X'80'      LAST SENT BY REP?                            
         BZ    CKSIDE20            NO                                           
         MVI   BYTE,C'S'           STATION SIDE                                 
         TM    RCONSENF,X'10'      STA VERSION NOT ADVANCED?                    
         BZ    CKSIDEX             NO                                           
         MVI   BYTE,C'B'           REP COULD TOUCH IT AGAIN                     
CKSIDE20 DS    0H                                                               
         TM    RCONSENF,X'20'      REP VERSION NOT ADVANCED?                    
         BZ    CKSIDEX                                                          
         MVI   BYTE,C'B'           STATION COULD TOUCH IT AGAIN                 
CKSIDEX  B     EXITOK                                                           
         DROP  R8                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
* CKSEND - CHECK IF CONTRACT LAST SEND BY REP/STA OR IS CONFIRMED               
*          RETURNS R/S/C IN 'BYTE'                                              
*-------------------------------------------------------------------*           
CKSEND   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BAS   RE,CKLSTSND         SET CONSNDEL FIELD                           
*                                                                               
         MVI   BYTE,C'N'           SET 'NOT CONFIRMED'                          
         L     R8,ACONREC          CHECK SEND STATUS                            
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKSEND05            NO ELEMENT: PUT OUT 'NOT CONFIRMED'          
*                                                                               
         USING RCONXEL,R8                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BNO   CKSEND05            NO                                           
         DROP  R8                                                               
*                                                                               
         MVC   WORK2+100(L'KEY+L'KEYSAVE),KEY                                   
         MVI   BYTE,C'C'                                                        
*                                                                               
         USING RCONREC,R8                                                       
         L     R8,ACONREC          CHECK SEND STATUS                            
*                                                                               
CKSEND05 EQU   *                                                                
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONCNFEL,BYTE,0                             
*                                                                               
*&&DO                                                                           
*                                                                               
*   DATE/TIME NOT NEEDED:  GENERATED IN CKLSTSND RTN.                           
*                                                                               
         CLI   BYTE,C'N'           NOT CONFIRMED?                               
         BE    CKSEND10            YES - NO DATE/TIME CONFIRMED NEEDED          
*                                                                               
         L     R8,ACONREC          GET CF DATE/TIME                             
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKSEND10            NO DATE/TIME ELEMENT                         
*                                                                               
         USING RMODELEM,R8                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONSDTEL,RMODEL1D,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSTMEL,RMODEL1T,0                         
*                                                                               
         DROP  R8                                                               
*&&                                                                             
CKSEND10 DS    0H                                                               
         L     R8,ACONREC          CHECK SEND STATUS                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKSEND30            NO 20 ELEM, ON REP SEND                      
*                                                                               
         USING RCONSEND,R8                                                      
         TM    RCONSENF,X'80'      LAST SENT BY REP?                            
         BNO   CKSEND20            NO                                           
*                                                                               
         MVI   BYTE,C'R'                                                        
         GOTO1 AADDDATA,DMCB,AFABLK,CONLCAEL,BYTE,0                             
*                                                                               
*&&DO                                                                           
*                                                                               
*   DATE/TIME NOT NEEDED:  GENERATED IN CKLSTSND RTN.                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONSDTEL,RCONSRDT,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSTMEL,RCONSRTI,0                         
*&&                                                                             
         B     CKSEND30                                                         
*                                                                               
CKSEND20 DS    0H                                                               
         TM    RCONSENF,X'40'      LAST SENT BY STATION?                        
         BNO   CKSEND30                                                         
*                                                                               
         MVI   BYTE,C'S'           STATION COULD TOUCH IT AGAIN                 
         GOTO1 AADDDATA,DMCB,AFABLK,CONLCAEL,BYTE,0                             
*&&DO                                                                           
*                                                                               
*   DATE/TIME NOT NEEDED:  GENERATED IN CKLSTSND RTN.                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONSDTEL,RCONSSDT,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSTMEL,RCONSSTI,0                         
*&&                                                                             
CKSEND30 DS    0H                                                               
*                                                                               
CKSEND50 DS    0H                                                               
         B     EXITOK                                                           
*-------------------------------------------------------------------*           
* CKLSTSND - SET SINGLE FIELD: REVERTING TO PRIOR CODING, THIS                  
*            PROCESSING IS LIFTED FROM CKSEND                                   
*-------------------------------------------------------------------*           
CKLSTSND NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,C'N'           SET 'NOT CONFIRMED'                          
         L     R8,ACONREC          CHECK SEND STATUS                            
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKLSTS10            NO ELEMENT: PUT OUT 'NOT CONFIRMED'          
*                                                                               
         USING RCONXEL,R8                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BNO   CKLSTS10            NO                                           
         DROP  R8                                                               
*                                                                               
         MVC   WORK2+100(L'KEY+L'KEYSAVE),KEY                                   
         MVI   BYTE,C'C'                                                        
*                                                                               
         USING RCONREC,R8                                                       
         L     R8,ACONREC          CHECK SEND STATUS                            
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONSNDEL,BYTE,0                             
*                                                                               
         L     R8,ACONREC          GET CF DATE/TIME                             
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKLSTS30            NO DATE/TIME ELEMENT                         
*                                                                               
         USING RMODELEM,R8                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONSDTEL,RMODEL1D,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSTMEL,RMODEL1T,0                         
*                                                                               
         B     CKLSTS30            FINISHED                                     
         DROP  R8                                                               
*                                                                               
CKLSTS10 DS    0H                                                               
         L     R8,ACONREC          CHECK SEND STATUS                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKLSTS30            NO 20 ELEM, ON REP SEND                      
*                                                                               
         USING RCONSEND,R8                                                      
         TM    RCONSENF,X'80'      LAST SENT BY REP?                            
         BNO   CKLSTS20            NO                                           
*                                                                               
         MVI   BYTE,C'R'                                                        
         GOTO1 AADDDATA,DMCB,AFABLK,CONSNDEL,BYTE,0                             
         GOTO1 AADDDATA,DMCB,AFABLK,CONSDTEL,RCONSRDT,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSTMEL,RCONSRTI,0                         
*                                                                               
         B     CKLSTS30                                                         
*                                                                               
CKLSTS20 DS    0H                                                               
         TM    RCONSENF,X'40'      LAST SENT BY STATION?                        
         BNO   CKLSTS30                                                         
*                                                                               
         MVI   BYTE,C'S'           STATION COULD TOUCH IT AGAIN                 
         GOTO1 AADDDATA,DMCB,AFABLK,CONSNDEL,BYTE,0                             
         GOTO1 AADDDATA,DMCB,AFABLK,CONSDTEL,RCONSSDT,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSTMEL,RCONSSTI,0                         
*                                                                               
CKLSTS30 DS    0H                                                               
         CLI   BYTE,C'N'                                                        
         BNE   CKLSTS50                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONSNDEL,BYTE,0                             
*                                                                               
CKLSTS50 DS    0H                                                               
         B     EXITOK                                                           
*-------------------------------------------------------------------*           
* CKVER - GET CURRENT VERSION NUMBER OF HEADER                                  
*-------------------------------------------------------------------*           
CKVER    NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,0                                                           
         L     R8,ACONREC          CHECK SEND STATUS                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKVERX                                                           
         USING RCONSEND,R8                                                      
         MVC   BYTE,RCONSRV        REP VER#                                     
         CLC   BYTE,RCONSSV        VS. STATION VER#                             
         BH    *+10                                                             
         MVC   BYTE,RCONSSV        STA VER#                                     
CKVERX   B     EXITOK                                                           
         DROP  R8                                                               
*-------------------------------------------------------------------*           
* CKMOD - GET CURRENT MOD NUMBER OF HEADER                                      
*-------------------------------------------------------------------*           
CKMOD    NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,X'FF'          SET DEFAULT VALUE                            
         L     R8,ACONREC          CHECK SEND STATUS                            
*                                                                               
*   WHY LOOK IN THE HISTORY ELEMENT WHEN YOU CAN LOOK IN THE                    
*        HEADER ITSELF?                                                         
*                                                                               
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKMODX                                                           
         USING RMODELEM,R8                                                      
         MVC   BYTE,RMODEL1M       MOD#                                         
CKMODX   B     EXITOK                                                           
         DROP  R8                                                               
*-------------------------------------------------------------------*           
* CKWIP - GET CURRENT WIP STATUS                                                
*-------------------------------------------------------------------*           
CKWIP    NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,0                                                           
         L     R8,ACONREC          CHECK WIP STATUS                             
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKWIP10                                                          
         USING RCONXEL,R8                                                       
*                                                                               
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BO    CKWIPX              YES - NOT WIP                                
*                                                                               
CKWIP10  L     R8,ACONREC          CHECK WIP STATUS                             
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKWIPX                                                           
         USING RCONSEND,R8                                                      
*                                                                               
         TM    RCONSENF,X'30'      CHECK BOTH VERSION ADVANCED FLAGS            
         BO    CKWIPX                                                           
         MVI   BYTE,1                                                           
CKWIPX   B     EXITOK                                                           
         DROP  R8                                                               
*-------------------------------------------------------------------*           
* REFRESH CONTRACT DOWNLOAD                                                     
*-------------------------------------------------------------------*           
DCONDWN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ATWA                                                          
         AHI   R6,(SVPARMBF-T173FFD)                                            
*                                                                               
*                                                                               
         USING VHPARMD,R6                                                       
DCDWN010 DS    0H                                                               
         XC    VHPARMD(VHPARMLQ),VHPARMD                                        
*                                                                               
         L     R8,0(R1)            SET A(CON REC IN TBUFF)                      
         ST    R8,ACONREC                                                       
*                                                                               
         BAS   RE,DCONFOO                                                       
*                                                                               
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
*-------------------------------------------------------------------*           
DCONFOO  NTR1  BASE=*,LABEL=*                                                   
         L     R8,ACONREC                                                       
         USING RCONREC,R8                                                       
*                                                                               
         MVI   ELCODE,X'17'        GET COMBO ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   *+14                NOT FOUND - OK TO CONTINUE                   
         MVC   ERROR,=Y(920)                                                    
         B     EXITL                                                            
*                                                                               
         L     R8,ACONREC                                                       
         USING RCONREC,R8                                                       
*                                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
*                                                                               
         MVC   VHPCON,WORK                                                      
*                                                                               
         MVC   VHPSTA,RCONKSTA                                                  
*                                                                               
         MVC   VHPADV,RCONKADV                                                  
         MVC   VHPPRD,RCONPRD                                                   
         MVC   VHPAGY,RCONKAGY                                                  
         MVC   VHPAOF,RCONKAOF                                                  
         GOTO1 VDATCON,DMCB,(3,RCONDATE),(19,VHPFLS)                            
         GOTO1 VDATCON,DMCB,(3,RCONDATE+3),(19,VHPFLE)                          
         MVC   VHPSAL,RCONSAL                                                   
         MVC   VHPCTY,RCONTYPE                                                  
         MVC   VHPBUYER,RCONBUYR                                                
*                                                                               
*                                                                               
*                                  ADD NEW CONTRACT ELEMENT                     
         GOTO1 ASETELEM,DMCB,AFABLK,CONDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         MVI   BYTE2,C'F'          SET 'FORECAST' FLAG                          
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'12'        RETRIEVE PENDING ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   DFOO0008            NO PENDING ELEMENT                           
         GOTO1 =A(MKT$$),DMCB,(R8),RR=Y                                         
         USING RSARXEL,R8                                                       
         TM    RSARXFLG,X'08'      FORECAST ORDER?                              
         BO    DFOO0009            YES -                                        
         DROP  R8                                                               
DFOO0008 DS    0H                                                               
         L     R8,ACONREC                                                       
         USING RCONREC,R8                                                       
*                                                                               
         MVI   BYTE2,C'P'          NO  - SET 'PENDING ' FLAG                    
         TM    RCONMODR,X'10'      BUYLINE ADDED?                               
         BO    DFOO0010            YES - NOT PENDING                            
         MVI   ELCODE,X'06'        NO  - SPL ADDED?                             
         BAS   RE,GETEL                                                         
         BE    DFOO0010            YES  - NOT PENDING                           
DFOO0009 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONPENDG,BYTE2,0                            
DFOO0010 DS    0H                                                               
*                                                                               
*   GET SPL ELEMENT (POSSIBLY AGAIN) TO SATISFY STA SHR REQUIREMENT             
*                                                                               
         L     R8,ACONREC                                                       
         USING RCONREC,R8                                                       
*                                                                               
         MVI   ELCODE,X'06'        GET SPL ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   DFOO0015            NO SPL ELEMENT                               
         USING RCONSPEL,R8                                                      
         CLI   RCONSPNU,0          ANY STATION ENTRIES?                         
         BE    DFOO0015            NO                                           
         ZICM  RF,RCONSPAM,4       YES - GET STATION PERCENT OF 1ST             
*                                     (REP'D) STATION                           
         CVD   RF,DUB                                                           
         ZAP   WORK(05),=PL1'0'                                                 
         AP    WORK(5),DUB                                                      
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONSTSHR,WORK,0                             
         DROP  R8                                                               
DFOO0015 DS    0H                                                               
         L     R8,ACONREC                                                       
         USING RCONREC,R8                                                       
*                                                                               
         TM    RCONCNTL,X'80'      CONTRACT MARKED FOR DELETION?                
         BNO   DFOO0020            NO                                           
         MVI   BYTE2,C'D'          YES - SET DELETE ELEMENT                     
         GOTO1 AADDDATA,DMCB,AFABLK,CONDEFLG,BYTE2,0                            
DFOO0020 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONNUMEL,VHPCON,0                           
         GOTO1 AADDDATA,DMCB,AFABLK,CONREPEL,RCONKREP,0                         
*                                                                               
* CREATE AND FIRST BUY DATES                                                    
         GOTO1 VDATCON,DMCB,(3,RCONHDRD),(19,WORK+36)                           
         GOTO1 AADDDATA,DMCB,AFABLK,CONHDRDT,WORK+36,0                          
         GOTO1 VDATCON,DMCB,(3,RCONCREA),(19,WORK+36)                           
         GOTO1 AADDDATA,DMCB,AFABLK,CONBUYDT,WORK+36,0                          
         TM    RCONMODR,X'10'      BUYS ADDED?                                  
         BNO   DFOO0030            NO                                           
         MVI   WORK+36,C'Y'                                                     
         GOTO1 AADDDATA,DMCB,AFABLK,CONBYFLG,WORK+36,0                          
*                                  SET 'BUYS ADDED'                             
DFOO0030 EQU   *                                                                
* FLIGHT                                                                        
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLSEL,VHPFLS,0                           
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLEEL,VHPFLE,0                           
* BUYER                                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONBYREL,RCONBUYR,0                         
* GROUP                                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONGRSUB,RCONKGRP,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONOFFEL,RCONKOFF,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONAGYEL,RCONKAGY,0                         
         CLC   RCONKAOF,SPACES                                                  
         BNH   DFOO0040                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONAOFEL,RCONKAOF,0                         
*                                                                               
DFOO0040 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONADVEL,RCONKADV,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSTAEL,RCONKSTA,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSALEL,RCONSAL,0                          
         GOTO1 AADDDATA,DMCB,AFABLK,CONCTYEL,RCONTYPE,0                         
*                                                                               
         CLC   RCONPRD,SPACES                                                   
         BNH   DFOO0060                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONPRDEL,RCONPRD,L'RCONPRD                  
         B     DFOO0080                                                         
*                                                                               
DFOO0060 DS    0H                                                               
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'05'        GET PRODUCT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DFOO0080                                                         
*                                                                               
         USING RCONEXEL,R8                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONPRDEL,RCONEXPR,L'RCONEXPR                
         DROP  R8                                                               
*                                                                               
DFOO0080 EQU   *                                                                
         MVI   BYTE2,C'M'          SET ORDER TYPE TO 'MANUAL'                   
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'1D'        DARE ELEMENT                                 
         BAS   RE,GETEL                                                         
         JNE   DFOO0090            NOT FOUND                                    
*                                                                               
         MVI   BYTE2,C'D'          SET ORDER TYPE TO 'DARE'                     
         USING RCONDREL,R8                                                      
         TM    RCONDRF2,X'01'      LINKED TO XML ORDER?                         
         BNO   DFOO0090            NO  - LEAVE AS 'DARE'                        
         MVI   BYTE2,C'X'          SET ORDER TYPE TO 'XML'                      
DFOO0090 EQU   *                                                                
         GOTO1 AADDDATA,DMCB,AFABLK,CONAGCON,BYTE2,0                            
         DROP  R8                                                               
*                                                                               
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'18'        DEVELOPMENT INFO ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   DFOO0100                                                         
*                                                                               
         USING RCONDVEL,R8                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONDCTEL,RCONDVCT,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONDSPEL,RCONDVSP,0                         
         DROP  R8                                                               
DFOO0100 DS    0H                                                               
* DEMOS                                                                         
         LA    RF,*                                                             
         AHI   RF,DEMOS-(*-4)                                                   
         BASR  RE,RF               GET DEMOS                                    
* SIDE                                                                          
         LA    RF,*                                                             
         AHI   RF,CKSIDE-(*-4)                                                  
         BASR  RE,RF               CHECK REP/STATION SIDE                       
         GOTO1 AADDDATA,DMCB,AFABLK,CONSIDEL,BYTE,0                             
* SEND                                                                          
         LA    RF,*                                                             
         AHI   RF,CKSEND-(*-4)                                                  
         BASR  RE,RF               GET REP/STATION LAST SENT/CF                 
* VER                                                                           
         LA    RF,*                                                             
         AHI   RF,CKVER-(*-4)                                                   
         BASR  RE,RF               GET LAST VERSION                             
         MVC   CONVERSV,BYTE                                                    
         GOTO1 AADDDATA,DMCB,AFABLK,CONVEREL,BYTE,0                             
* MOD                                                                           
         LA    RF,*                                                             
         AHI   RF,CKMOD-(*-4)                                                   
         BASR  RE,RF               GET LAST MOD                                 
         GOTO1 AADDDATA,DMCB,AFABLK,CONMODEL,BYTE,0                             
* WIP                                                                           
         LA    RF,*                                                             
         AHI   RF,CKWIP-(*-4)                                                   
         BASR  RE,RF               GET WIP                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONWIPEL,BYTE,0                             
         GOTO1 =A(BOOKS),RR=Y                                                   
*                                                                               
         XC    WORK,WORK                                                        
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   DFOO0180                                                         
         USING RCONIEL,R8                                                       
         CLC   RCONIADV,SPACES                                                  
         BNH   DFOO0120                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONADV,RCONIADV,0                           
DFOO0120 EQU   *                                                                
         CLC   RCONIPRD,SPACES                                                  
         BNH   DFOO0140                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONPRD1,RCONIPRD,0                          
DFOO0140 EQU   *                                                                
         CLC   RCONIPR2,SPACES                                                  
         BNH   DFOO0160                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONPRD2,RCONIPR2,0                          
DFOO0160 EQU   *                                                                
         MVC   WORK(10),RCONXEST                                                
         OC    WORK(10),SPACES                                                  
         CLC   WORK(10),SPACES                                                  
         BNE   *+10                                                             
         MVC   WORK(4),RCONIEST                                                 
         GOTO1 AADDDATA,DMCB,AFABLK,CONEST,WORK,0                               
         DROP  R8                                                               
                                                                                
DFOO0180 DS    0H                                                               
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DFOO0200                                                         
         USING RCONXEL,R8                                                       
         GOTO1 AADDDATA,DMCB,AFABLK,CONTRAF,RCONTRF,0                           
         DROP  R8                                                               
DFOO0200 DS    0H                                                               
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DFOO0220                                                         
         USING RCONXXEL,R8                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONTRAGY,RCONXAGY,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONTRADV,RCONXADV,0                         
                                                                                
DFOO0220 DS    0H                                                               
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'A0'                                                     
         BAS   RE,GETEL                                                         
         BNE   DFOO0260                                                         
         USING RCONOREL,R8                                                      
         MVC   WORK,SPACES                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONTRSOF,RCONOOFF,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONTRSLS,RCONOSAL,0                         
         B     DFOO0260                                                         
         DROP  R8                                                               
                                                                                
*        EOP CODES REQUIRED                                                     
DFOO0260 DS    0H                                                               
         MVI   BYTE2,0                                                          
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'15'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   BYTE2,1                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,CONECFLG,BYTE2,0                            
         CLI   BYTE2,0             ANY 15 ELEMENT?                              
         BE    DFOO0280            NO                                           
*                                                                               
         USING RCONECEL,R8                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONECDAT,RCONECDT,0                         
         GOTO1 VHEXOUT,DMCB,RCONECTM,WORK,2,=C'TOG'                             
         GOTO1 AADDDATA,DMCB,AFABLK,CONECTIM,WORK,0                             
         DROP  R8                                                               
*                                                                               
DFOO0280 DS    0H                                                               
                                                                                
* TOTAL$                                                                        
         LA    RF,*                                                             
         AHI   RF,BUCKET-(*-4)                                                  
         BASR  RE,RF               BUCKET CONTRACT                              
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONTOTEL,WORK,0                             
         GOTO1 AADDDATA,DMCB,AFABLK,CONXOTEL,WORK+10,0                          
DFOO0320 DS    0H                                                               
*                                                                               
         L     R8,ACONREC                                                       
         USING RCONREC,R8                                                       
*                                                                               
*&&DO                                                                           
*                                                                               
*   NONE OF THESE I/O-CENTERED ROUTINES CAN BE EXECUTED.                        
*                                                                               
         GOTO1 =A(ADDSALNM),RR=Y                                                
         GOTO1 =A(ADDOFFNM),RR=Y                                                
         GOTO1 =A(ADDDSPNM),RR=Y                                                
         GOTO1 =A(ADDADVNM),RR=Y                                                
         GOTO1 =A(ADDAGYNM),RR=Y                                                
*&&                                                                             
*                                                                               
* MON DOLLARS                                                                   
         GOTO1 =A(DCONDMON),RR=Y                                                
         SR    R2,R2               CLEAR INDICATOR                              
*                                                                               
         GOTO1 =A(ADDDUMMY),RR=Y                                                
*                                                                               
*                                                                               
DFOO0360 DS    0H                                                               
         NI    BYTE3,X'FF'-FLUSHIT TURN OFF 'FLUSH'                             
*                                                                               
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'82'        REP ORDER COMMENT                            
         BAS   RE,GETEL                                                         
         BNE   DFOO0420                                                         
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,OCMDATA,0                                   
*                                                                               
DFOO0380 DS    0H                                                               
         ZIC   R2,1(R8)                                                         
         AHI   R2,-3                                                            
*                                                                               
         LTR   R2,R2                                                            
         BM    DFOO0400                                                         
* REP                                                                           
*                                                                               
*   DON'T "OR SPACES" - CLIENT ENTERING TILDA, WHICH IS BEING                   
*        CORRUPTED                                                              
*&&DO                                                                           
         EX    R2,*+8                                                           
         B     *+10                                                             
         OC    2(0,R8),SPACES                                                   
*&&                                                                             
         LA    R2,1(R2)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,OCMREPEL,2(R8),(R2)                         
*                                                                               
DFOO0400 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    DFOO0380                                                         
*                                                                               
DFOO0420 DS    0H                                                               
         ZIC   R2,0(R8)            SAVE THE LAST ELCODE                         
*                                                                               
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'92'        STA ORDER COMMENT                            
         BAS   RE,GETEL                                                         
         BNE   DFOO0480                                                         
*                                                                               
         CHI   R2,X'82'            IF THE LAST ELCODE WAS REP ORDER             
         BE    DFOO0440             COMMENT ALREADY HAVE HEADER                 
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,OCMDATA,0                                   
*                                                                               
DFOO0440 DS    0H                                                               
         ZIC   R2,1(R8)                                                         
         AHI   R2,-3                                                            
*                                                                               
         LTR   R2,R2                                                            
         BM    DFOO0460                                                         
* STA                                                                           
*                                                                               
*   DON'T "OR SPACES" - CLIENT ENTERING TILDA, WHICH IS BEING                   
*        CORRUPTED                                                              
*&&DO                                                                           
         EX    R2,*+8                                                           
         B     *+10                                                             
         OC    2(0,R8),SPACES                                                   
*&&                                                                             
         LA    R2,1(R2)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,OCMSTAEL,2(R8),(R2)                         
         OI    BYTE3,FLUSHIT                                                    
*                                                                               
DFOO0460 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    DFOO0440                                                         
*                                                                               
DFOO0480 DS    0H                                                               
         TM    BYTE3,FLUSHIT       NEED TO FLUSH LAST?                          
         BNO   DFOO0500            NO                                           
*                                                                               
         GOTO1 =A(ADDDUMMY),RR=Y                                                
DFOO0500 DS    0H                                                               
*&&DO                                                                           
*   TEST                                                                        
         LA    R0,1                                                             
         LA    RF,SRVFLD1                                                       
         DC    H'0'                                                             
*   TEST END                                                                    
*&&                                                                             
         XIT1                                                                   
         EJECT                                                                  
         DROP  R6                                                               
*                                                                               
DCONERR  DS    0H                                                               
         L     RE,ADDR                                                          
         MVC   ERROR,0(RE)                                                      
         B     EXITL                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ADDDUMMY:  FLUSH OUT THE LAST ITEM GENERATED.                               
*                                                                               
ADDDUMMY NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(5),=C'DUMMY'                                                
         GOTO1 AADDDATA,DMCB,AFABLK,CONDUMMY,WORK,0                             
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   NONE OF THESE I/O-CENTERED ROUTINES CAN BE DONE                             
*                                                                               
*&&DO                                                                           
*   ADDADVNM:  ADD ADVERTISER NAME TO THE OUTPUT.                               
*        ALSO ADD ADVERTISER CATEGORY, CLASS                                    
*                                                                               
ADDADVNM NTR1  LABEL=*,BASE=*                                                   
         L     R8,ACONREC                                                       
         USING RCONREC,R8                                                       
*                                                                               
         XC    KEY,KEY                                                          
K        USING RADVKEY,KEY                                                      
         MVI   K.RADVKTYP,X'08'                                                 
         MVC   K.RADVKREP,RCONKREP                                              
         MVC   K.RADVKADV,RCONKADV                                              
         OC    K.RADVKADV,SPACES                                                
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(15),=C'ADV NOT ON FILE'                                     
*                                  JIC                                          
         CLC   KEY(L'RADVKEY),KEYSAVE                                           
         BNE   ADAD0040                                                         
*                                                                               
         GOTO1 VGETREC,AIO1                                                     
         L     R3,AIO1                                                          
         USING RADVREC,R3                                                       
         MVC   WORK(L'RADVNAME),RADVNAME                                        
         GOTO1 AADDDATA,DMCB,AFABLK,CONCLASS,RADVCLSS,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONCATGY,RADVCATG,0                         
*                                                                               
         DROP  R3,R8                                                            
ADAD0040 EQU   *                                                                
         GOTO1 AADDDATA,DMCB,AFABLK,CONADVNM,WORK,0                             
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ADDAGYNM:  ADD AGENCY     NAME TO THE OUTPUT.                               
*                                                                               
ADDAGYNM NTR1  LABEL=*,BASE=*                                                   
         L     R8,ACONREC                                                       
         USING RCONREC,R8                                                       
*                                                                               
         XC    KEY,KEY                                                          
K        USING RAGYKEY,KEY                                                      
         MVI   K.RAGYKTYP,X'0A'                                                 
         MVC   K.RAGYKREP,RCONKREP                                              
         MVC   K.RAGYKAGY(6),RCONKAGY                                           
         OC    K.RAGYKAGY(6),SPACES                                             
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(15),=C'AGY NOT ON FILE'                                     
*                                  JIC                                          
         CLC   KEY(L'RAGYKEY),KEYSAVE                                           
         BNE   ADAG0040                                                         
*                                                                               
         GOTO1 VGETREC,AIO1                                                     
         L     R3,AIO1                                                          
         USING RAGYREC,R3                                                       
         MVC   WORK(L'RAGYNAM1),RAGYNAM1                                        
*                                                                               
         DROP  R3,R8                                                            
ADAG0040 EQU   *                                                                
*                                                                               
*   TEST                                                                        
         MVC   HALF,=C'XX'                                                      
*   TEST END                                                                    
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONAGYNM,WORK,0                             
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ADDSALNM:  ADD SALESPERSON NAME TO THE OUTPUT.                              
*        ALSO ADD DIV/TEAM DATA                                                 
*                                                                               
ADDSALNM NTR1  LABEL=*,BASE=*                                                   
         L     R8,ACONREC                                                       
         USING RCONREC,R8                                                       
*                                                                               
         XC    KEY,KEY                                                          
K        USING RSALKEY,KEY                                                      
         MVI   K.RSALKTYP,X'06'                                                 
         MVC   K.RSALKREP,RCONKREP                                              
         MVC   K.RSALKSAL,RCONSAL                                               
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(15),=C'S/P NOT ON FILE'                                     
*                                  JIC                                          
         CLC   KEY(L'RSALKEY),KEYSAVE                                           
         BNE   ADSP0040                                                         
*                                                                               
         GOTO1 VGETREC,AIO1                                                     
         L     R3,AIO1                                                          
         USING RSALREC,R3                                                       
         MVC   WORK(L'RSALNAME),RSALNAME                                        
         GOTO1 AADDDATA,DMCB,AFABLK,CONDIVTM,RSALTEAM,0                         
*                                                                               
         DROP  R3,R8                                                            
ADSP0040 EQU   *                                                                
         GOTO1 AADDDATA,DMCB,AFABLK,CONSALNM,WORK,0                             
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ADDOFFNM:  ADD OFFICE NAME TO THE OUTPUT.                                   
*                                                                               
ADDOFFNM NTR1  LABEL=*,BASE=*                                                   
         L     R8,ACONREC                                                       
         USING RCONREC,R8                                                       
*                                                                               
         XC    KEY,KEY                                                          
K        USING ROFFKEY,KEY                                                      
         MVI   K.ROFFKTYP,X'04'                                                 
         MVC   K.ROFFKREP,RCONKREP                                              
         MVC   K.ROFFKOFF,RCONKOFF                                              
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(15),=C'OFC NOT ON FILE'                                     
*                                  JIC                                          
         CLC   KEY(L'ROFFKEY),KEYSAVE                                           
         BNE   ADOF0040                                                         
*                                                                               
         GOTO1 VGETREC,AIO1                                                     
         L     R3,AIO1                                                          
         USING ROFFREC,R3                                                       
         MVC   WORK(L'ROFFNAME),ROFFNAME                                        
*                                                                               
         DROP  R3,R8                                                            
ADOF0040 EQU   *                                                                
         GOTO1 AADDDATA,DMCB,AFABLK,CONOFFNM,WORK,0                             
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ADDDSPNM:  ADD DEV S/P NAME TO THE OUTPUT.                                  
*                                                                               
ADDDSPNM NTR1  LABEL=*,BASE=*                                                   
         L     R8,ACONREC                                                       
         USING RCONREC,R8                                                       
         MVC   HALF,RCONKREP                                                    
*                                  SAVE POWER CODE                              
         XC    WORK,WORK                                                        
         MVC   WORK(15),=C'DSP NOT ON FILE'                                     
*                                  JIC                                          
         MVI   ELCODE,X'18'        GET DEV ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   ADDS0040            NOT FOUND                                    
*                                                                               
         XC    KEY,KEY                                                          
K        USING RDSPKEY,KEY                                                      
         MVI   K.RDSPKTYP,X'3A'                                                 
         MVC   K.RDSPKREP,HALF                                                  
         MVC   K.RDSPKSAL,RCONDVSP-RCONDVEL(R8)                                 
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RDSPKEY),KEYSAVE                                           
         BNE   ADDS0040                                                         
*                                                                               
         GOTO1 VGETREC,AIO1                                                     
         L     R3,AIO1                                                          
         USING RDSPREC,R3                                                       
         MVC   WORK(L'RDSPNAME),RDSPNAME                                        
*                                                                               
         DROP  R3                                                               
ADDS0040 EQU   *                                                                
         GOTO1 AADDDATA,DMCB,AFABLK,CONDSPNM,WORK,0                             
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*                                                                               
* PROCESS PENDING ELEMENT FOR MARKET DOLLARS, SHARE GOAL                        
*                                                                               
*    R8 -> RCONREC'S X'12' ELEMENT                                              
*                                                                               
*-------------------------------------------------------------------*           
MKT$$    NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            RESET A(X'12' ELEMENT)                       
*                                                                               
         USING RSARXEL,R8                                                       
*                                                                               
         TM    RSARXFL2,X'80'      'BONUS' BUDGET?                              
         BO    MKT$0080            YES - DON'T USE FIGURES                      
         TM    RSARXFL2,X'40'      'TRADE' BUDGET?                              
         BO    MKT$0080            YES - DON'T USE FIGURES                      
         TM    RSARXFL2,X'20'      'DR' BUDGET?                                 
         BO    MKT$0080            YES - DON'T USE FIGURES                      
         TM    RSARXFL2,X'10'      'ORDER' BUDGET?                              
         BO    MKT$0080            YES - DON'T USE FIGURES                      
         TM    RSARXFL2,X'08'      'PP' BUDGET?                                 
         BO    MKT$0080            YES - DON'T USE FIGURES                      
         TM    RSARXFL2,X'04'      'GEN AVAIL' BUDGET?                          
         BO    MKT$0080            YES - DON'T USE FIGURES                      
*                                                                               
         MVC   FULL,RSARXBGT       DISPLAY BUDGET, IF ANY                       
         L     RF,FULL                                                          
         LTR   RF,RF               ANY VALUE HERE?                              
         BNZ   MKT$0010            YES                                          
         TM    RSARXFLG,X'40'      BUDGET ENTERED AS ZERO?                      
         BNO   MKT$0040            NO  - SKIP BUDGET                            
*                                  YES - SEND ZEROS                             
MKT$0010 EQU   *                                                                
         ZAP   WORK(12),=PL1'0'                                                 
         CVD   RF,DUB                                                           
         AP    WORK(12),DUB                                                     
*                                                                               
*   MARKET $$                                                                   
         GOTO1 AADDDATA,DMCB,AFABLK,CONMKT$$,WORK,0                             
*                                  INSERT MARKET BUDGET                         
MKT$0040 EQU   *                                                                
         CLI   RSARXLEN,120        OLD OR NEW ELEMENT?                          
         BNH   MKT$0120            OLD - NEXT FIELD DOESN'T EXIST               
         ZIC   RF,RSARXSHG         DISPLAY SHARE GOAL, IF ANY                   
         LTR   RF,RF               ANY VALUE HERE?                              
         BNZ   MKT$0050            YES                                          
         TM    RSARXFLG,X'20'      NO  - SHARE GOAL ENTERED AS ZERO?            
         BNO   MKT$0120            NO  - DON'T SEND SHARE GOAL                  
*                                                                               
MKT$0050 EQU   *                                                                
         ZAP   WORK(05),=PL1'0'                                                 
         CVD   RF,DUB                                                           
         AP    WORK(05),DUB                                                     
*                                                                               
*   SHARE GOAL                                                                  
         GOTO1 AADDDATA,DMCB,AFABLK,CONSHGOL,WORK,0                             
*                                  INSERT SHARE GOAL                            
         B     MKT$0120                                                         
*                                                                               
*&&DO                                                                           
* STATION BUDGET HAS BEEN REMOVED AS A FIELD.  CODE IS                          
*        COMMENTED OUT RATHER THAN REMOVED, ON THE OFF-CHANCE                   
*        THEY AGAIN CHANGE THEIR MINDS.                                         
*                                                                               
* STATION BUDGET = MARKET BUDGET * SHARE GOAL                                   
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,FULL                                                       
         XC    WORK,WORK                                                        
         MVC   WORK+1(1),RSARXSHG                                               
         MH    RF,WORK                                                          
         XC    WORK,WORK                                                        
         MVI   WORK+3,100                                                       
         D     RE,WORK                                                          
         LR    R4,RF                                                            
         EDIT  (R4),(10,RECONSBD),FILL=0                                        
         B     MKT$0120                                                         
*&&                                                                             
*                                                                               
MKT$0080 EQU   *                                                                
         MVC   WORK(8),=C'BONUS   '                                             
         TM    RSARXFL2,X'80'      'BONUS' BUDGET?                              
         BO    MKT$0090            YES - DON'T USE FIGURES                      
         MVC   WORK(8),=C'TRADE   '                                             
         TM    RSARXFL2,X'40'      'TRADE' BUDGET?                              
         BO    MKT$0090            YES - DON'T USE FIGURES                      
         MVC   WORK(8),=C'DR      '                                             
         TM    RSARXFL2,X'20'      'DR' BUDGET?                                 
         BO    MKT$0090            YES - DON'T USE FIGURES                      
         MVC   WORK(8),=C'ORDER   '                                             
         TM    RSARXFL2,X'10'      'ORDER' BUDGET?                              
         BO    MKT$0090            YES - DON'T USE FIGURES                      
         MVC   WORK(8),=C'PP      '                                             
         TM    RSARXFL2,X'08'      'PP' BUDGET?                                 
         BO    MKT$0090            YES - DON'T USE FIGURES                      
         MVC   WORK(8),=C'GEN AVL '                                             
         TM    RSARXFL2,X'04'      'GEN AVAIL' BUDGET?                          
         BO    MKT$0090            YES - DON'T USE FIGURES                      
         B     MKT$0120            NO VALUE / NO ELEMENT                        
MKT$0090 EQU   *                                                                
*                                                                               
*   ORDER TEXT                                                                  
         GOTO1 AADDDATA,DMCB,AFABLK,CONCNTXT,WORK,0                             
*                                  INSERT ORDER TEXT                            
         B     MKT$0120                                                         
*                                                                               
MKT$0120 EQU   *                                                                
         XIT1                                                                   
         DROP  R8                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* PROCESS MON DOLLARS                                                           
*                                                                               
*    ACONREC -> RCONREC                                                         
*                                                                               
*-------------------------------------------------------------------*           
DCONDMON NTR1  BASE=*,LABEL=*                                                   
         L     R8,ACONREC                                                       
         USING RCONREC,R8                                                       
*                                                                               
         TM    RCONMODR+1,X'20'    MON DOLLARS?                                 
         BZ    DMON030             NO                                           
         DROP  R8                                                               
*                                                                               
         SR    R6,R6                                                            
         L     R2,ACONREC                                                       
*                                                                               
         L     R8,ACONREC                                                       
         MVI   ELCODE,X'03'        SEE IF THERE IS MONEY                        
         BAS   RE,GETEL                                                         
         BNE   DMON030                                                          
*                                                                               
         USING RCONBKEL,R8                                                      
DMON010  DS    0H                                                               
         CLC   0(RCONBKWK-RCONBKEL,R2),0(R8)                                    
         BE    DMON020                                                          
*                                                                               
         LTR   R6,R6               ANY MONEY?                                   
         BZ    DMON012             NO                                           
*                                                                               
X        USING RCONBKEL,R2                                                      
         XC    FULL,FULL                                                        
         MVC   FULL(1),X.RCONBKYR                                               
         MVC   FULL+1(1),X.RCONBKMN                                             
         MVI   FULL+2,1                                                         
         DROP  X                                                                
* MONTH                                                                         
         GOTO1 VDATCON,DMCB,(3,FULL),(19,WORK)                                  
         GOTO1 AADDDATA,DMCB,AFABLK,CONMONEL,WORK,0                             
* DOLLARS                                                                       
         ST    R6,FULL                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,CONDOLEL,FULL,0                             
*                                                                               
DMON012  DS    0H                                                               
         CLI   0(R8),X'03'         ELEMENT A BUCKET?                            
         BNE   DMON030             NO                                           
*                                                                               
         SR    R6,R6               RESET FOR NEXT BUCKET                        
         LR    R2,R8                                                            
*                                                                               
DMON020  DS    0H                                                               
         ICM   RE,15,RCONBKAM                                                   
         AR    R6,RE               ADD DOLLARS                                  
*                                                                               
         BAS   RE,NEXTEL                                                        
         B     DMON010                                                          
         DROP  R8                                                               
*                                                                               
DMON030  DS    0H                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
                                                                                
                                                                                
*-------------------------------------------------------------------*           
*                                                                               
* INTERESTING DATA STRINGS FOR XARSE                                            
*                                                                               
*  AL4(HEADER ELEMENT)                                                          
*    AL1(DATA EQUATE), XARSE MODE  0 - DEFAULT                                  
*                                  1 - DON'T SEND UNLESS > SPACE                
*                                  2 - MAPCODE ONLY                             
*    AL4(MAPTABLE ENTRY FOR DATA)                                               
*                                                                               
*-------------------------------------------------------------------*           
SALTBL   DC    A(SALDATA)                                                       
FOO      DC    AL1(QSALKSAL,0),A(SALCODEL)                                      
XTABLQ   EQU   *-FOO                                                            
         DC    AL1(QSALNAME,0),A(SALNAMEL)                                      
         DC    AL1(QSALTEL,0),A(SALTELEL)                                       
         DC    AL1(QSALFAX,0),A(SALFAXEL)                                       
         DC    AL1(QSALTEAM,0),A(SALTEMEL)                                      
         DC    AL1(QTEMDVNM,0),A(SALDIVEL)                                      
         DC    AL1(QTEMNAME,0),A(SALTNMEL)                                      
         DC    AL1(QSALEMAL,0),A(SALEMLEL)                                      
         DC    AL1(QSALOFF,0),A(SALOFFEL)                                       
         DC    AL1(QSALOFFN,0),A(SALOFNEL)                                      
         DC    AL1(QSALMGR,2),A(SALMGREL)                                       
         DC    AL1(QSALADD0,0),A(SALADDEL)                                      
         DC    AL1(QSALADD1,0),A(SALADDEL)                                      
         DC    AL1(0)                                                           
CNTTBL   DC    A(CNTDATA)                                                       
         DC    AL1(QCTYKCTY,0),A(CNTCTYEL)                                      
         DC    AL1(QCTYDESC,0),A(CNTDSCEL)                                      
         DC    AL1(0)                                                           
DCTTBL   DC    A(DCTDATA)                                                       
         DC    AL1(QDCTKCTY,0),A(DCTCTYEL)                                      
         DC    AL1(QDCTNAME,0),A(DCTNAMEL)                                      
         DC    AL1(0)                                                           
DSPTBL   DC    A(DSPDATA)                                                       
         DC    AL1(QDSPKSAL,0),A(DSPCODEL)                                      
         DC    AL1(QDSPNAME,0),A(DSPNAMEL)                                      
         DC    AL1(QDSPTEL,0),A(DSPTELEL)                                       
         DC    AL1(QDSPFAX,0),A(DSPFAXEL)                                       
         DC    AL1(0)                                                           
STATBL   DC    A(STADATA)                                                       
         DC    AL1(QSTAKSTA,0),A(STASTAEL)                                      
         DC    AL1(QSTAMKT,0),A(STAMKTEL)                                       
         DC    AL1(QSTAAFFL,0),A(STAAFFEL)                                      
         DC    AL1(QSTACHAN,0),A(STACHNEL)                                      
         DC    AL1(0)                                                           
ADVTBL   DC    A(ADVDATA)                                                       
         DC    AL1(QADVKADV,0),A(ADVCODEL)                                      
         DC    AL1(QADVNAME,0),A(ADVNAMEL)                                      
         DC    AL1(0)                                                           
AGYTBL   DC    A(AGYDATA)                                                       
         DC    AL1(QAGYKAGY,0),A(AGYCODEL)                                      
         DC    AL1(QAGYKAOF,1),A(AGYOFFEL)                                      
         DC    AL1(QAGYNAME,0),A(AGYNAMEL)                                      
         DC    AL1(0)                                                           
AGADTBL  DC    A(0)                                                             
         DC    AL1(QAGADLN0,0),A(AGYAD0EL)                                      
         DC    AL1(QAGADLN1,0),A(AGYAD1EL)                                      
         DC    AL1(QAGADLN2,0),A(AGYAD2EL)                                      
         DC    AL1(QAGADLN3,0),A(AGYAD3EL)                                      
         DC    AL1(QAGADNAM,0),A(AGYONMEL)                                      
         DC    AL1(0)                                                           
PRDTBL   DC    A(PRDDATA)                                                       
         DC    AL1(QPRDKPRD,1),A(PRDCODEL)                                      
         DC    AL1(QPRDNAME,0),A(PRDNAMEL)                                      
         DC    AL1(0)                                                           
DPTTBL   DC    A(DPTDATA)                                                       
         DC    AL1(QDPTCODE,0),A(DPTCODEL)                                      
         DC    AL1(QDPTSNAM,0),A(DPTSNMEL)                                      
         DC    AL1(QDPTLNAM,0),A(DPTLNMEL)                                      
         DC    AL1(0)                                                           
                                                                                
*-------------------------------------------------------------------*           
* THESE ARE USED DURING BCONDET                                                 
*-------------------------------------------------------------------*           
SALBCD   DC    A(SALDATA)                                                       
         DC    AL1(QSALNAME,0),A(SALNAMEL)                                      
         DC    AL1(QSALTEL,0),A(SALTELEL)                                       
         DC    AL1(QSALFAX,0),A(SALFAXEL)                                       
         DC    AL1(QSALEMAL,0),A(SALEMLEL)                                      
         DC    AL1(0)                                                           
ADVBCD   DC    A(ADVDATA)                                                       
         DC    AL1(QADVNAME,0),A(ADVNAMEL)                                      
         DC    AL1(0)                                                           
AGYBCD   DC    A(AGYDATA)                                                       
         DC    AL1(QAGYKAGY,0),A(AGYCODEL)                                      
         DC    AL1(QAGYKAOF,1),A(AGYOFFEL)                                      
         DC    AL1(QAGYNAME,0),A(AGYNAMEL)                                      
         DC    AL1(0)                                                           
PRDBCD   DC    A(PRDDATA)                                                       
         DC    AL1(QPRDNAME,0),A(PRDNAMEL)                                      
         DC    AL1(0)                                                           
                                                                                
*-------------------------------------------------------------------*           
*                                                                               
* ROUTINE LIST FOR REQUEST HEADERS                                              
*                                                                               
*   AL2 - HEADER EQUATE NUMBER                                                  
*   AL4 - ADDRESS OF DOWNLOAD ROUTINE                                           
*   AL4 - ADDRESS OF FIELD LIST FOR REQUEST                                     
*   AL4 - ADDRESS OF OPTIONAL EXTRA SETUP ROUTINE                               
*                                                                               
*                                                                               
*-------------------------------------------------------------------*           
REQHDRS  DS    0A                                                               
         DC    AL2(INITHDRQ),A(0),A(0),A(0)                                     
*                                                                               
*   TABLE WILL NOT BE USED.  ABOVE CHANGE MADE JUST FOR A CLEAN                 
*        ASSEMBLY.                                                              
*                                                                               
ROUTABLQ EQU   *-REQHDRS                                                        
         DC    AL2(BCONHDRQ),A(0),A(BCONFLDS),A(0)                              
         DC    AL2(LCONHDRQ),A(LCONDWN),A(LCONFLDS),A(0)                        
         DC    AL2(DCONHDRQ),A(DCONDWN),A(DCONFLDS),A(0)                        
***      DC    AL2(VHDRHDRQ),A(VHDRDWN),A(VHDRFLDS),A(0)                        
**       DC    AL2(VBKSHDRQ),A(VBKSDWN),A(VBKSFLDS),A(0)                        
         DC    AL2(0000)                                                        
*-------------------------------------------------------------------*           
*                                                                               
* FIELD ROUTINE LIST FOR VERSION INFO                                           
*                                                                               
*-------------------------------------------------------------------*           
FVERFLDS DS    0A                                                               
         DC    AL2(FVERVERQ),A(FVERVER)                                         
FLDTABLQ EQU   *-FVERFLDS                                                       
         DC    AL2(0000)                                                        
*-------------------------------------------------------------------*           
* FIELD ROUTINE LIST FOR LIST CONTRACT REQUEST                                  
*-------------------------------------------------------------------*           
LCONFLDS DS    0A                                                               
         DC    AL2(0000)                                                        
*-------------------------------------------------------------------*           
* FIELD ROUTINE LIST FOR STATION CONTRACT REQUEST                               
*-------------------------------------------------------------------*           
BCONFLDS DS    0A                                                               
         DC    AL2(0000)                                                        
*-------------------------------------------------------------------*           
* FIELD ROUTINE LIST FOR RATING DOWNLOAD REQUEST                                
*-------------------------------------------------------------------*           
RTGSFLDS DS    0A                                                               
         DC    AL2(0000)                                                        
*-------------------------------------------------------------------*           
* FIELD ROUTINE LIST FOR VALIDATE HEADER                                        
*   AND BOOKS/UPGRADES/DEMOS                                                    
*-------------------------------------------------------------------*           
VHDRFLDS DS    0A                                                               
         DC    AL2(0000)                                                        
*-------------------------------------------------------------------*           
* FIELD ROUTINE LIST FOR DOWNLOAD CONTRACT HEADER FIELDS                        
*-------------------------------------------------------------------*           
DCONFLDS DS    0A                                                               
         DC    AL2(0000)                                                        
*-------------------------------------------------------------------*           
*        ** LOADED PHASE LIST **                                                
*-------------------------------------------------------------------*           
PHASES   DS    0X                  ** LOADED PHASE LIST **                      
         DC    AL1(0)              <=== TWABLD                                  
         DC    AL1(0)              <=== UNBOOK                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QDEMOVAL)                                                    
         DC    AL1(QUPVAL)                                                      
         DC    AL1(QBOOKVAL)       <=== BUT THIS EQUATE IS ALSO 0               
         DC    AL1(QDAYVAL)                                                     
         DC    AL1(QDAYUNPK)                                                    
         DC    AL1(QTIMVAL)                                                     
         DC    AL1(QUNTIME)                                                     
         DC    AL1(QREFETCH)                                                    
         DC    AL1(QGETBROD)                                                    
         DC    AL1(QFALINK)                                                     
         DC    AL1(QREPFACS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
PHASESN  EQU   *-PHASES                                                         
         EJECT                                                                  
*****>>>>>   FALINK ROUTINES                                                    
***********************************************************************         
* SET NEW ELEMENT CODE AND ADD IT TO BUFFER IN XA                     *         
*                                                                     *         
* NTRY: AMODE=24                                                      *         
*       P1: B 0-3: A(FALINKBLK)                                       *         
*       P2: B 0-3: A(ELEMENT HEADER FIELD)                            *         
*       P3: B 0-3: 0  (GIVES DEFAULT ELEMENT CODE)                    *         
*             OR                                                      *         
*       P3: B 0  : LENGTH OF OVERRIDE STRING (MUST BE <60 BYTES)      *         
*           B 1-3: A(OVERRIDE STRING)                                 *         
*                                                                     *         
* EXIT: AMODE=24                                                      *         
*       CC NOT SET                                                    *         
***********************************************************************         
                                                                                
**SETELEM CODE                                                                  
SETELEM  NTR1  BASE=*,LABEL=*                                                   
         GOTOR SETGLB              SET A(GLOBALS)                               
         USING GLOBALS,RA                                                       
         ICM   R9,15,0(R1)         A(FALINKBLK IS 1ST PARAMETER)                
         ICM   R9,15,FALASVE-FALINKD(R9)                                        
                                                                                
         L     R2,4(R1)            R2=A(ELEMENT HEADER ENTRY)                   
         USING MHELD,R2                                                         
         XC    SAVE,SAVE                                                        
         XC    SAVELEN,SAVELEN                                                  
                                                                                
SETEL02  SR    RF,RF                                                            
         ICM   RF,1,8(R1)          OVERRIDING CODE?                             
         BZ    SETEL04             NO                                           
         BCTR  RF,0                RF=L'-1 OF OVERRIDE CODE                     
         SR    RE,RE                                                            
         ICM   RE,7,9(R1)                                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT+2(0),0(RE)  COPY IN OVERRIDE STRING                      
         AHI   RF,1                                                             
         B     SETEL10                                                          
                                                                                
SETEL04  OC    MHCODE,MHCODE                                                    
         BNZ   *+6                                                              
         DC    H'0'                ELEMENT CODE CANNOT BE ZERO                  
                                                                                
         GOTOR VHEXOUT,DMCB,MHCODE,WORK,L'MHCODE,0                              
*                                                                               
*   I HAVE NO IDEA WHAT THIS CODE DOES IN FALINK, SO I'VE COMMENTED             
*        IT OUT HERE.  IT SEEMS TO HAVE NO EFFECT LOCALLY.                      
*                                                                               
**       ORG   *-2                                                              
**       BASSM RE,RF                                                            
**       SAM31 ,                                                                
                                                                                
         OC    16(4,R1),16(R1)                                                  
         BNZ   *+6                                                              
         DC    H'0'                INVALID HEXOUT                               
                                                                                
         LHI   RF,L'MHCODE*2       HEXOUT LENGTH DOUBLES                        
                                                                                
SETEL06  CLC   =C'00',WORK         STRIP LEADING ZERO PAIR(S)                   
         BNE   SETEL08                                                          
         AHI   RF,-3               RF=RF-1                                      
         EX    RF,SETELMVE                                                      
         AHI   RF,1                RF=RF-2                                      
         B     SETEL06                                                          
                                                                                
SETELMVE MVC   WORK(0),WORK+2                                                   
                                                                                
SETEL08  BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT+2(0),WORK   COPY IN ELEMENT CODE                         
         AHI   RF,1                                                             
                                                                                
SETEL10  STC   RF,ELEMENT          SET ELEMENT CODE LENGTH                      
         AHI   RF,2                                                             
         STC   RF,ELEMLEN          SET LENGTH OF ELEMENT STRING                 
         MVI   ELEMENT+1,ELEMIDQ   SET ELEMENT IDENTIFIER                       
         TR    ELEMENT(1),LENGTHS  TRANSLATE LENGTH                             
                                                                                
         GOTOR EWRITE              WRITE PENDING ELEMENT                        
                                                                                
SETELEMX EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* WRITE PENDING ELEMENT TO BUFFER                                     *         
*                                                                     *         
* NTRY: AMODE    = 31                                                 *         
*       R9       = A(SAVED STORAGE COVERED BY SAVED)                  *         
*       RC       = A(WORKING STORAGE COVERED BY WORKD)                *         
*       ELEMLEN  = LENGTH OF ELEMENT PENDING                          *         
*       ELEMENT  = ELEMENT TO WRITE                                   *         
*                                                                     *         
* EXIT: AMODE    = 31                                                 *         
*       ANXTBUFF = UPDATED                                            *         
*       CC NOT SET                                                    *         
***********************************************************************         
                                                                                
EWRITE   NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2                                                            
         ICM   R2,1,ELEMLEN        LENGTH OF ELEMENT TO WRITE                   
         BNZ   *+6                                                              
         DC    H'0'                WHERE DID IT GO!!!                           
         GOTOR VPROTOFF                                                         
                                                                                
         ICM   R3,15,ANXTBUFF      NEXT AVAILABLE SLOT                          
         ICM   RE,15,FLNK          START OF BUFFER                              
         AH    RE,PGSIZE           END OF BUFFER (ALLOW TERMINATOR)             
         BCTR  RE,0                                                             
         SR    RE,R3               RE=LENGTH REMAINING IN BUFFER                
         CR    RE,R2                                                            
         BH    EWRITE02            ROOM FOR THIS ELEMENT IN BUFFER              
                                                                                
         DC    H'0'                                                             
*                                                                               
*   SHOULD NEVER GO TO ANOTHER BUFFER                                           
****>>>  GOTOR NEXTBUF                                                          
                                                                                
         L     R3,ANXTBUFF         THIS GETS RESET TO POINT TO START            
         LR    R0,R3                                                            
         LH    R1,PGSIZE                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR BUFFER AREA DOWN                       
                                                                                
EWRITE02 BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),ELEMENT     MOVE IN ELEMENT INFORMATION                  
                                                                                
         LA    R3,1(R2,R3)                                                      
         ST    R3,ANXTBUFF         UPDATE POINTER                               
         MVI   0(R3),EOB           SET TERMINATOR                               
                                                                                
EWRITEX  GOTOR VPROTON                                                          
         J     EXITY                                                            
         LTORG                                                                  
         DROP  R2,RA,RB                                                         
         EJECT                                                                  
***********************************************************************         
* ADD A PIECE OF DATA TO THE BUFFER                                   *         
*                                                                     *         
* NTRY: AMODE=24                                                      *         
*       P1: A(FALINKBLK)                                              *         
*       P2: A(ELEMENT DATA ENTRY)                                     *         
*       P3: A(DATA)                                                   *         
*       P4: 0 FOR NORMAL, NZ HOLDS FIELD LENGTH OVERRIDE              *         
*                                                                     *         
* EXIT: AMODE=24                                                      *         
*       CC NOT SET                                                    *         
***********************************************************************         
                                                                                
ADDDATA  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTOR SETGLB              SET A(GLOBALS)                               
         USING GLOBALS,RA                                                       
         ICM   R9,15,0(R1)         A(FALINKBLK IS 1ST PARAMETER)                
         L     R9,FALASVE-FALINKD(R9)                                           
*                                                                               
         L     RF,0(R1)                                                         
         MVC   FAPARMS(FALINKDL),0(RF)                                          
*                                                                               
         XC    FWORK,FWORK         RESET TEMPORARY AREAS                        
         XC    WORKLEN,WORKLEN                                                  
         XC    WORKMAPT,WORKMAPT                                                
         XC    TEMP,TEMP                                                        
         XC    TEMPLEN,TEMPLEN                                                  
                                                                                
ADDD10   ICM   R3,15,4(R1)         RE=A(MDELD FOR THIS DATA)                    
         USING MDELD,R3                                                         
         LA    RE,CNVRTAB          RE=A(CONVERSION TABLE)                       
         USING CNVRTABD,RE                                                      
ADDD12   CLI   CNVRTYPE,EOT                                                     
         BNE   *+6                                                              
         DC    H'0'                UNKNOWN DATA TYPE                            
         CLC   CNVRTYPE,MDTYPE                                                  
         BE    *+12                                                             
         AHI   RE,CNVRTABL                                                      
         B     ADDD12                                                           
         SR    RF,RF                                                            
         ICM   RF,3,CNVRUPL        GET A(UPLOAD CONVERSION ROUTINE)             
         A     RF,AT17300          SET ADDRESSABILITY                           
*                                                                               
         DROP  RE                                                               
*                                                                               
         GOTO1 (RF)                DO CONVERSION                                
*                                                                               
         OC    WORKLEN,WORKLEN     ANY CURRENT ITEM?                            
         BNZ   *+6                 YES                                          
         DC    H'0'                                                             
                                                                                
         OC    SAVELEN,SAVELEN     PREVIOUS ITEM SAVED?                         
         BZ    ADDD18              NO - SAVE THIS ONE AND QUIT                  
                                                                                
         CLC   MDCODE,SAVEMAP      CURRENT DATA SAME CODE AS BEFORE?            
         BNE   ADDD16              NO                                           
                                                                                
         TM    SFLAG1,SFBOS        ALREADY IN SERIES?                           
         BO    *+8                 YES                                          
         OI    SFLAG1,SFSCHAR      ADD START CHARACTER TO SAVE ITEM             
         GOTOR DWRITE              WRITE PENDING DATA                           
                                                                                
         NI    SFLAG1,EFEF-(SFSCHAR+SFECHAR+SFSDPEND)                           
         OI    SFLAG1,SFBOS                                                     
         B     ADDD18              MOVE CURRENT ITEM TO SAVE                    
                                                                                
ADDD16   TM    SFLAG1,SFBOS        WITHIN SERIES                                
         BZ    *+8                                                              
         OI    SFLAG1,SFECHAR      ADD END CHARACTER                            
         GOTOR DWRITE              WRITE PENDING DATA                           
         NI    SFLAG1,EFEF-(SFBOS+SFSCHAR+SFECHAR+SFSDPEND)                     
                                                                                
ADDD18   MVC   SAVEMAP,MDCODE      SAVE MAPCODE                                 
         MVC   SAVEMTXT,MDTEXT     SAVE MAPTEXT                                 
         MVC   SAVEMAPT,WORKMAPT                                                
         XC    SAVE,SAVE           MOVE CURRENT ITEM TO SAVE                    
         XC    SAVELEN,SAVELEN                                                  
         SR    RF,RF                                                            
         IC    RF,WORKLEN                                                       
         STC   RF,SAVELEN          SAVE DATA LENGTH                             
         CHI   RF,DONLY            SPECIAL - ELEMENT ONLY                       
         BE    ADDD20                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     ADDD20                                                           
         MVC   SAVE(0),FWORK       SAVE DATA                                    
                                                                                
ADDD20   OI    SFLAG1,SFSDPEND     SET WRITE PENDING                            
*                                                                               
         J     EXITY                                                            
         DROP  R3,RB                                                            
****>>>> SUPPORT ROUTINES LIKE ADDBIN                                           
***********************************************************************         
* ADD TO BUFFER ROUTINES                                              *         
* ------------------------------------------------------------------- *         
* NTRY: 04(R1)  = A(MAP DATA ELEMENT)                                 *         
*       08(R1)  = A(INPUT DATA)                                       *         
*       12(R1)  = OVERRIDE DATA LENGTH (IF REQUIRED)                  *         
* EXIT: FWORK   = DATA                                                *         
*       WORKLEN = DATA LENGTH                                         *         
*                                                                     *         
* GET FROM BUFFER ROUTINES                                            *         
* ------------------------------------------------------------------- *         
* NTRY: 00(R1)  = A(MAP DATA ELEMENT)                                 *         
*       04(R1)  = A(INPUT DATA)                                       *         
*       08(R1)  = DATA LENGTH INPUT                                   *         
* EXIT: FWORK   = DATA                                                *         
*       WORKLEN = DATA LENGTH TO PASS BACK                            *         
***********************************************************************         
                                                                                
***********************************************************************         
* ADD BINARY DATA TO BUFFER                                           *         
***********************************************************************         
                                                                                
ADDBIN   NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0                                                            
         LM    R2,R4,4(R1)                                                      
         USING MDELD,R2                                                         
         LTR   R4,R4               OVERRIDE DATA LENGTH                         
         BNZ   *+8                                                              
         ICM   R4,1,MDDLEN                                                      
                                                                                
         MVI   WORKMAPT,C'F'       DEFAULT TYPE IS 32-BIT INTEGER               
         CHI   R4,1                                                             
         BNE   *+8                                                              
         MVI   WORKMAPT,C'T'       LENGTH 1 IS TINY INT                         
         CHI   R4,2                                                             
         BNE   *+8                                                              
         MVI   WORKMAPT,C'S'       LENGTH 2 IS SHORT INT                        
                                                                                
         LHI   RF,1                                                             
         SLL   RF,0(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,ADDBICM                                                       
         CURED (R0),(14,FWORK),0,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                
         STC   R0,WORKLEN                                                       
ADDBINX  J     EXIT                                                             
                                                                                
ADDBICM  ICM   R0,0,0(R3)                                                       
         EJECT                                                                  
***********************************************************************         
* GET BINARY DATA FROM BUFFER                                         *         
***********************************************************************         
                                                                                
GETBIN   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,0(R1)                                                      
         BCTR  R4,0                                                             
         EX    R4,GETBPCK          PACK CHARACTER NUMBER                        
         CVB   R0,DUB                                                           
         ICM   R4,1,MDDLEN                                                      
         STC   R4,WORKLEN          SET REQUIRED LENGTH                          
         LHI   RF,1                                                             
         SLL   RF,0(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,GETBSTCM         STORE REQUIRED NUMBER OF CHARACTERS          
GETBINX  J     EXIT                                                             
                                                                                
GETBPCK  PACK  DUB,0(0,R3)                                                      
GETBSTCM STCM  R0,0,FWORK                                                       
         EJECT                                                                  
***********************************************************************         
* ADD UNSIGNED BINARY DATA TO BUFFER                                  *         
***********************************************************************         
                                                                                
ADDUSB   NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0                                                            
         LM    R2,R4,4(R1)                                                      
         LTR   R4,R4               OVERRIDE DATA LENGTH                         
         BNZ   *+8                                                              
         ICM   R4,1,MDDLEN                                                      
                                                                                
         MVI   WORKMAPT,C'F'       DEFAULT TYPE IS 32-BIT INTEGER               
         CHI   R4,1                                                             
         BNE   *+8                                                              
         MVI   WORKMAPT,C'T'       LENGTH 1 IS TINY INT                         
         CHI   R4,2                                                             
         BNE   *+8                                                              
         MVI   WORKMAPT,C'S'       LENGTH 2 IS SHORT INT                        
                                                                                
         LHI   RF,1                                                             
         SLL   RF,0(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,ADDUICM                                                       
         TMH   R0,X'8000'          BFN?                                         
         BO    ADDU02              YES                                          
         CURED (R0),(14,FWORK),0,ALIGN=LEFT,ZERO=NOBLANK                        
         STC   R0,WORKLEN                                                       
         B     ADDUSBX                                                          
                                                                                
ADDU02   N     R0,HOBOFF                                                        
         CVD   R0,DUB               NUMBER NOW IN DUB                           
         OI    DUB+L'DUB-1,X'0F'                                                
         AP    DUB,=PL8'2147483648' ADD THE MISSING HOB                         
         CURED (P8,DUB),(15,FWORK),0,ALIGN=LEFT,ZERO=NOBLANK                    
         STC   R0,WORKLEN                                                       
ADDUSBX  J     EXIT                                                             
                                                                                
ADDUICM  ICM   R0,0,0(R3)                                                       
         EJECT                                                                  
***********************************************************************         
* GET UNSIGNED BINARY DATA FROM BUFFER                                *         
***********************************************************************         
                                                                                
GETUSB   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,0(R1)                                                      
         BCTR  R4,0                                                             
         EX    R4,GETUPCK          PACK CHARACTER NUMBER                        
         CVB   R0,DUB                                                           
         ICM   R4,1,MDDLEN                                                      
         STC   R4,WORKLEN          SET REQUIRED LENGTH                          
         LHI   RF,1                                                             
         SLL   RF,0(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,GETUSTCM         STORE REQUIRED NUMBER OF CHARACTERS          
         J     EXIT                                                             
                                                                                
GETUPCK  PACK  DUB,0(0,R3)                                                      
GETUSTCM STCM  R0,0,FWORK                                                       
         EJECT                                                                  
***********************************************************************         
* ADD PACKED DATA TO BUFFER (OUTPUT FORMAT: MINUS=YES)                *         
***********************************************************************         
                                                                                
ADDPCK   NTR1  BASE=*,LABEL=*                                                   
         MVI   WORKMAPT,C'L'       PACKED ARE ALL TYPE LONG                     
         LM    R2,R4,4(R1)                                                      
         LTR   R4,R4               OVERRIDE DATA LENGTH                         
         BNZ   *+8                                                              
         ICM   R4,1,MDDLEN                                                      
         BCTR  R4,0                                                             
         EX    R4,ADDPZAP                                                       
         LNR   R0,R0                                                            
         CURED (P8,DUB),(14,FWORK),0,ALIGN=LEFT,FLOAT=-,ZERO=NOBLANK            
         STC   R0,WORKLEN                                                       
ADDPCKX  J     EXIT                                                             
                                                                                
ADDPZAP  ZAP   DUB,0(0,R3)                                                      
         EJECT                                                                  
***********************************************************************         
* GET PACKED DATA FROM BUFFER                                         *         
***********************************************************************         
                                                                                
GETPCK   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,0(R1)                                                      
         GOTOR VCASHVAL,DMCB,(C'0',(R3)),(R4)                                   
         CLI   0(R1),EFEF                                                       
         BNE   *+6                                                              
         DC    H'0'                INVALID VALUE                                
                                                                                
         SR    RF,RF                                                            
         IC    RF,MDDLEN                                                        
         STC   RF,WORKLEN          SET REQUIRED LENGTH                          
         BCTR  RF,0                                                             
         SLL   RF,4                                                             
         EX    RF,GETPZAP          ZAP IN DATA                                  
GETPCKX  J     EXIT                                                             
                                                                                
GETPZAP  ZAP   FWORK(0),DMCB+4(8)                                               
         EJECT                                                                  
***********************************************************************         
* ADD CHARACTER DATA TO BUFFER                                        *         
***********************************************************************         
                                                                                
ADDCHR   NTR1  BASE=*,LABEL=*                                                   
         MVI   WORKMAPT,C'C'       TYPE CHAR                                    
         LM    R2,R4,4(R1)                                                      
         LTR   R4,R4               OVERRIDE DATA LENGTH                         
         BNZ   *+8                                                              
         ICM   R4,1,MDDLEN                                                      
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   FWORK(0),0(R3)                                                   
         AHI   R4,1                R4 = LENGTH OF DATA                          
                                                                                
         LA    RF,FWORK(R4)                                                     
         BCTR  RF,0                RF = LAST CHARACTER                          
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R4,*-10             FIND FIRST NON-SPACE                         
                                                                                
         LTR   R4,R4                                                            
         BNZ   *+8                                                              
         LHI   R4,DONLY                                                         
         STC   R4,WORKLEN          SET LENGTH MOVED                             
ADDCHRX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* USER QUALIFIED DATA                                                 *         
***********************************************************************         
                                                                                
ADDUSR   NTR1  BASE=*,LABEL=*                                                   
         MVI   WORKMAPT,0          NO TYPE FOR USER FIELDS                      
         LM    R2,R4,4(R1)                                                      
         LTR   R4,R4               OVERRIDE DATA LENGTH                         
         BNZ   *+8                                                              
         ICM   R4,1,MDDLEN                                                      
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   FWORK(0),0(R3)                                                   
         AHI   R4,1                R4 = LENGTH OF DATA                          
                                                                                
         LA    RF,FWORK(R4)                                                     
         BCTR  RF,0                RF = LAST CHARACTER                          
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R4,*-10             FIND FIRST NON-SPACE                         
                                                                                
         LTR   R4,R4                                                            
         BNZ   *+8                                                              
         LHI   R4,DONLY                                                         
         STC   R4,WORKLEN          SET LENGTH MOVED                             
ADDUSRX  J     EXIT                                                             
                                                                                
***********************************************************************         
* GET PRINTABLE (CHARACTER) DATA FROM BUFFER                          *         
***********************************************************************         
                                                                                
GETCHR   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,0(R1)                                                      
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   FWORK(0),0(R3)      MOVE DATA INTO FWORK                         
                                                                                
         SR    RF,RF               DATA LENGTH                                  
         ICM   RF,1,MDDLEN                                                      
         BNZ   *+8                 ZERO MEANS VARIABLE LENGTH                   
         LA    RF,1(R4)                                                         
         STC   RF,WORKLEN                                                       
GETCHRX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD PWOS DATE TO BUFFER (OUTPUT FORMAT YYMMDD)                      *         
***********************************************************************         
                                                                                
ADDDTE   NTR1  BASE=*,LABEL=*                                                   
         MVI   WORKMAPT,C'D'       DATE                                         
         LM    R2,R4,4(R1)                                                      
         GOTOR VDATCON,DMCB,(1,(R3)),(20,FWORK)                                 
         MVI   WORKLEN,8                                                        
         CLC   THISYEAR,FWORK                                                   
         BNE   ADDDTEX                                                          
         MVI   WORKLEN,4                                                        
         MVC   FWORK(4),FWORK+4                                                 
ADDDTEX  J     EXIT                                                             
                                                                                
***********************************************************************         
* GET DATE INFORMATION FROM BUFFER (OUTPUT IS YMD)                    *         
***********************************************************************         
                                                                                
GETDTE   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,0(R1)                                                      
         GOTOR VDATCON,DMCB,(9,(R3)),(1,FWORK)                                  
         MVI   WORKLEN,3                                                        
GETDTEX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD PWOS JULIAN DATE TO BUFFER (OUTPUT FORMAT YYMMDD)               *         
***********************************************************************         
                                                                                
ADDJDT   NTR1  BASE=*,LABEL=*                                                   
         MVI   WORKMAPT,C'D'       DATE                                         
         LM    R2,R4,4(R1)                                                      
         GOTOR VDATCON,DMCB,(8,(R3)),(20,FWORK)                                 
         MVI   WORKLEN,8                                                        
         CLC   THISYEAR,FWORK                                                   
         BNE   ADDJDTX                                                          
         MVI   WORKLEN,4                                                        
         MVC   FWORK(4),FWORK+4                                                 
ADDJDTX  J     EXIT                                                             
                                                                                
***********************************************************************         
* GET PWOS JULIAN DATE FROM BUFFER                                    *         
***********************************************************************         
                                                                                
GETJDT   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,0(R1)                                                      
         GOTOR VDATCON,DMCB,(9,(R3)),(19,FWORK)                                 
         MVI   WORKLEN,3                                                        
GETJDTX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD BINARY DATE TO BUFFER (OUTPUT FORMAT YYMMDD)                    *         
***********************************************************************         
                                                                                
ADDBDT   NTR1  BASE=*,LABEL=*                                                   
         MVI   WORKMAPT,C'D'       DATE                                         
         LM    R2,R4,4(R1)                                                      
         GOTOR VDATCON,DMCB,(3,(R3)),(20,FWORK)                                 
         MVI   WORKLEN,8                                                        
         CLC   THISYEAR,FWORK                                                   
         BNE   ADDBDTX                                                          
         MVI   WORKLEN,4                                                        
         MVC   FWORK(4),FWORK+4                                                 
ADDBDTX  J     EXIT                                                             
                                                                                
***********************************************************************         
* GET BINARY DATE FROM BUFFER                                         *         
***********************************************************************         
                                                                                
GETBDT   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,0(R1)                                                      
         GOTOR VDATCON,DMCB,(9,(R3)),(3,FWORK)                                  
         MVI   WORKLEN,3                                                        
GETBDTX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD COMPRESSED DATE TO BUFFER (OUTPUT FORMAT YYMMDD)                *         
***********************************************************************         
                                                                                
ADDCDT   NTR1  BASE=*,LABEL=*                                                   
         MVI   WORKMAPT,C'D'       DATE                                         
         LM    R2,R4,4(R1)                                                      
         GOTOR VDATCON,DMCB,(2,(R3)),(20,FWORK)                                 
         MVI   WORKLEN,8                                                        
         CLC   THISYEAR,FWORK                                                   
         BNE   ADDCDTX                                                          
         MVI   WORKLEN,4                                                        
         MVC   FWORK(4),FWORK+4                                                 
ADDCDTX  J     EXIT                                                             
                                                                                
***********************************************************************         
* GET COMPRESSED DATE FROM BUFFER                                     *         
***********************************************************************         
                                                                                
GETCDT   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,0(R1)                                                      
         GOTOR VDATCON,DMCB,(9,(R3)),(2,FWORK)                                  
         MVI   WORKLEN,2                                                        
GETCDTX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD CASH AMOUNT TO BUFFER (OUTPUT FORMAT: 2DP,MINUS=YES)            *         
***********************************************************************         
                                                                                
ADDCSH   NTR1  BASE=*,LABEL=*                                                   
         MVI   WORKMAPT,C'L'       PACKED ARE ALL TYPE LONG                     
         LM    R2,R4,4(R1)                                                      
         LTR   R4,R4               OVERRIDE DATA LENGTH                         
         BNZ   *+8                                                              
         ICM   R4,1,MDDLEN                                                      
         BCTR  R4,0                                                             
         EX    R4,ADDCSZAP                                                      
         CURED (P8,DUB),(14,FWORK),0,ALIGN=LEFT,FLOAT=-,ZERO=NOBLANK            
         STC   R0,WORKLEN                                                       
ADDCSHX  J     EXIT                                                             
                                                                                
ADDCSZAP ZAP   DUB,0(0,R3)                                                      
         EJECT                                                                  
***********************************************************************         
* GET PACKED CASH VALUE FROM BUFFER                                   *         
***********************************************************************         
                                                                                
GETCSH   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,0(R1)                                                      
         GOTOR VCASHVAL,DMCB,(C'0',(R3)),(R4)                                   
         CLI   0(R1),EFEF                                                       
         BNE   *+6                                                              
         DC    H'0'                INVALID VALUE                                
                                                                                
         SR    RF,RF                                                            
         IC    RF,MDDLEN                                                        
         STC   RF,WORKLEN          SET REQUIRED LENGTH                          
         BCTR  RF,0                                                             
         SLL   RF,4                                                             
         EX    RF,GETCZAP          ZAP IN DATA                                  
GETCSHX  J     EXIT                                                             
                                                                                
GETCZAP  ZAP   FWORK(0),DMCB+4(8)                                               
         EJECT                                                                  
***********************************************************************         
* ADD BINARY CASH DATA TO BUFFER                                      *         
***********************************************************************         
                                                                                
ADDBCA   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,4(R1)                                                      
         LTR   R4,R4               OVERRIDE DATA LENGTH                         
         BNZ   *+8                                                              
         ICM   R4,1,MDDLEN                                                      
                                                                                
         MVI   WORKMAPT,C'F'       DEFAULT TYPE IS 32-BIT INTEGER               
         CHI   R4,1                                                             
         BNE   *+8                                                              
         MVI   WORKMAPT,C'T'       LENGTH 1 IS TINY INT                         
         CHI   R4,2                                                             
         BNE   *+8                                                              
         MVI   WORKMAPT,C'S'       LENGTH 2 IS SHORT INT                        
                                                                                
         LHI   RF,1                                                             
         SLL   RF,0(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,ADDBCICM                                                      
         CURED (R0),(14,FWORK),0,ALIGN=LEFT,FLOAT=-                             
         STC   R0,WORKLEN                                                       
         J     EXIT                                                             
                                                                                
ADDBCICM ICM   R0,0,0(R3)                                                       
         EJECT                                                                  
***********************************************************************         
* GET BINARY CASH DATA FROM BUFFER                                    *         
***********************************************************************         
                                                                                
GETBCA   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,0(R1)                                                      
         GOTOR VCASHVAL,DMCB,(C'N',(R3)),(R4)                                   
         CLI   0(R1),EFEF                                                       
         BNE   *+6                                                              
         DC    H'0'                INVALID VALUE                                
                                                                                
         L     R0,DMCB+4           BINARY AMOUNT RETURNED HERE                  
         ICM   R4,1,MDDLEN                                                      
         STC   R4,WORKLEN          SET REQUIRED LENGTH                          
         LHI   RF,1                                                             
         SLL   RF,0(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,GETCSTCM         STORE REQUIRED NUMBER OF DIGITS              
GETBCAX  J     EXIT                                                             
                                                                                
GETCSTCM STCM  R0,0,FWORK                                                       
         EJECT                                                                  
***********************************************************************         
* ADD HEXADECIMAL DATA TO BUFFER                                      *         
***********************************************************************         
                                                                                
ADDHEX   NTR1  BASE=*,LABEL=*                                                   
         MVI   WORKMAPT,C'X'       TYPE HEX                                     
         LM    R2,R4,4(R1)                                                      
         LTR   R4,R4               OVERRIDE DATA LENGTH                         
         BNZ   *+8                                                              
         ICM   R4,1,MDDLEN                                                      
         GOTOR VHEXOUT,DMCB,(R3),FWORK,(R4),0                                   
         SLL   R4,1                                                             
                                                                                
ADDHX02  CHI   R4,1                MUST SHOW AT LEAST 1 BYTE                    
         BNH   ADDHEXX                                                          
         CLI   FWORK,C'0'          STRIP LEADING ZERO(S)                        
         BNE   ADDHEXX                                                          
         BCTR  R4,0                R4=R4-1                                      
         EX    R4,*+8                                                           
         B     ADDHX02                                                          
         MVC   FWORK(0),FWORK+1                                                 
                                                                                
ADDHEXX  STC   R4,WORKLEN                                                       
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET HEXADECIMAL DATA FROM BUFFER                                    *         
***********************************************************************         
                                                                                
GETHEX   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,0(R1)                                                      
                                                                                
         MVI   WORK2,C'0'           ZERO FILL WORK2                             
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
                                                                                
         LTR   R4,R4               OVERRIDE DATA LENGTH SET?                    
         BNZ   *+12                YES                                          
         ICM   R4,1,MDDLEN                                                      
         SLL   R4,1                                                             
                                                                                
         SR    RF,RF                                                            
         ICM   RF,1,MDDLEN                                                      
         SLL   RF,1                                                             
         SR    RF,R4               RF=AMOUNT OF ALIGNMENT REQUIRED              
         LA    RF,WORK2(RF)                                                     
                                                                                
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R3)                                                    
         AHI   R4,1                                                             
                                                                                
         SR    RF,RF                                                            
         ICM   RF,1,MDDLEN                                                      
         SLL   RF,1                                                             
         GOTOR VHEXIN,DMCB,WORK2,FWORK,(RF),0                                   
         L     R4,DMCB+12                                                       
         STC   R4,WORKLEN                                                       
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD DATA CODE ONLY (ZERO LENGTH) TO BUFFER                          *         
***********************************************************************         
                                                                                
ADDMDE   MVI   WORKMAPT,C'H'                                                    
         MVI   WORKLEN,DONLY                                                    
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET DATA CODE ONLY FROM BUFFER                                      *         
***********************************************************************         
                                                                                
GETMDE   BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
****>>>> SUPPORT ROUTINES LIKE ADDBIN                                           
*                                                                               
***  >>>>>   FALINK DWRITE ROUTINE                                              
***********************************************************************         
* WRITE PENDING DATA TO BUFFER                                        *         
* NTRY: AMODE    = 31                                                 *         
*       R9       = A(SAVED STORAGE COVERED BY SAVED)                  *         
*       RC       = A(WORKING STORAGE COVERED BY WORKD)                *         
*       SAVELEN  = LENGTH OF DATA PENDING                             *         
*       SAVE     = DATA TO WRITE                                      *         
*                                                                     *         
* EXIT: AMODE    = 31                                                 *         
*       ANXTBUFF = UPDATED                                            *         
*       CC NOT SET                                                    *         
***********************************************************************         
                                                                                
DWRITE   NTR1  BASE=*,LABEL=*                                                   
         LA    R8,FAPARMS                                                       
         USING FALINKD,R8                                                       
*                                                                               
         CLI   SAVELEN,DONLY       MDCODE ONLY?                                 
         BE    DWRITE02                                                         
         TM    FALAINDS,FALAINDC   TEST OUTPUT COMPRESSION OFF                  
         BNZ   DWRITE02                                                         
         GOTOR REPCHR              REPEATING CHARACTER OPTIMIZATION             
                                                                                
DWRITE02 GOTOR VPROTOFF                                                         
         SR    R2,R2                                                            
         CLI   SAVELEN,DONLY       MAPCODE ONLY - SET LENGTH ZERO               
         BE    *+8                                                              
         IC    R2,SAVELEN                                                       
*&&DO                                                                           
         TM    SFLAG2,SFLSTQ       LAST PAGE?                                   
         BZ    DWRITE04            NO                                           
         L     R6,ANXTBUFF                                                      
         LA    R6,256(R6)          BUFFER SPARE WE WILL ALLOW                   
         L     RE,FLNK                                                          
         AH    RE,PGSIZE                                                        
         CR    R6,RE                                                            
         L     R6,ANXTBUFF                                                      
         BL    DWRITE06                                                         
         OI    SFLAG2,SFULBQ       FLAG BREAK REQUIRED                          
         B     DWRITE06                                                         
*&&                                                                             
DWRITE04 L     R6,ANXTBUFF         GET NEXT AVAILABLE SLOT                      
         L     RE,FLNK             START OF BUFFER                              
         AH    RE,PGSIZE           END OF BUFFER LESS MAX XTRA REQUIRED         
*&&US*&& AHI   RE,-12                                                           
*&&UK*&& AHI   RE,-24                                                           
         SR    RE,R6               REMAINING SPACE                              
         CR    RE,R2               WILL THIS DATA FIT?                          
         BH    DWRITE06            YES                                          
*                                                                               
         DC    H'0'                SHOULD NEVER NEED ANOTHER BUFFER             
DWRITE06 TM    SFLAG1,SFSCHAR      START OF SERIES REQUIRED?                    
         BZ    *+12                NO                                           
         MVI   0(R6),SCHARQ                                                     
         AHI   R6,1                                                             
         CLI   SAVEMAPT,0          TEST DATA TYPE IS SET                        
         BNE   DWRITE08                                                         
         TM    FALAINDS,FALAINDT   TEST DON'T SEND DATA TYPE                    
         BNZ   DWRITE08                                                         
         LTR   R2,R2                                                            
         BZ    DWRITE08                                                         
         BCTR  R2,0                LENGTH IS OFF BY ONE                         
                                                                                
DWRITE08 CHI   R2,LCHARL           STRING IS LONGER THAN 63 CHARACTERS?         
         BL    DWRITE10            NO                                           
         MVI   0(R6),LCHAR                                                      
         AHI   R6,1                                                             
         AHI   R2,-(LCHARL)                                                     
         B     DWRITE08                                                         
                                                                                
DWRITE10 CLI   SAVELEN,DONLY                                                    
         BNE   DWRITE12                                                         
         MVI   0(R6),MONLY                                                      
         XC    SAVELEN,SAVELEN                                                  
         B     DWRITE14                                                         
                                                                                
DWRITE12 LA    R1,LENGTHS(R2)      MOVE IN LENGTH CODE FOR DATA                 
         MVC   0(1,R6),0(R1)                                                    
DWRITE14 AHI   R6,1                                                             
         SR    RF,RF                                                            
         ICM   RF,3,SAVEMAP                                                     
                                                                                
         L     R1,FALABLD          EXPANDED DOWNLOAD?                           
         CLI   EXPANDME,C'Y'                                                    
         BNE   DWRITE16                                                         
*                                                                               
         MVC   0(L'MDTEXT,R6),SAVEMTXT                                          
         MVI   L'MDTEXT(R6),C'='                                                
         AHI   R6,L'MDTEXT+1                                                    
         B     DWRITE26                                                         
                                                                                
DWRITE16 TM    SFLAG1,SFBOS        CURRENTLY IN SERIES?                         
         BZ    DWRITE18            NO - OUTPUT MAPCODE                          
         TM    FALAINDS,FALAINDD   TEST TYPE EMBEDDED IN DATA                   
         BZ    DWRITE26                                                         
         SR    R1,R1               YES - DROP TYPE FOR DUPLICATES               
         ICM   R1,1,SAVELEN                                                     
         BZ    DWRITE26                                                         
         BCTR  R1,0                                                             
         STC   R1,SAVELEN                                                       
         MVC   SAVE(L'SAVE-1),SAVE+1                                            
         B     DWRITE26                                                         
DWRITE18 EQU   *                                                                
**       CLI   VRSN,V6Q            TEST VERSION 6 OR GREATER                    
**       BL    DWRITE20                                                         
         CHI   RF,XBASIS           TEST MAP CODE GREATER THAN 189               
         BL    DWRITE20                                                         
         MVI   0(R6),XCHAR         EXPANDED MAP CHARACTER                       
         AHI   R6,1                                                             
         SHI   RF,XBASIS                                                        
         SR    RE,RE                                                            
         LHI   R0,LCHARL-1                                                      
         DR    RE,R0                                                            
         CHI   RF,LCHARL-1                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         LA    RF,LENGTHS(RF)                                                   
         MVC   0(1,R6),0(RF)                                                    
         AHI   R6,1                                                             
         LA    RE,LENGTHS(RE)                                                   
         MVC   0(1,R6),0(RE)                                                    
         AHI   R6,1                                                             
         B     DWRITE24                                                         
                                                                                
DWRITE20 CHI   RF,LCHARL           MAPCODE IS GREATER THAN 63?                  
         BL    DWRITE22            NO                                           
         MVI   0(R6),LCHAR                                                      
         AHI   R6,1                                                             
         AHI   RF,-(LCHARL)                                                     
         B     DWRITE20                                                         
                                                                                
DWRITE22 LA    R1,LENGTHS(RF)      MOVE IN MAPCODE LENGTH                       
         MVC   0(1,R6),0(R1)                                                    
         AHI   R6,1                                                             
                                                                                
DWRITE24 OC    SAVEMAPT,SAVEMAPT                                                
         BZ    DWRITE26                                                         
         MVC   0(L'SAVEMAPT,R6),SAVEMAPT                                        
         AHI   R6,L'SAVEMAPT                                                    
                                                                                
DWRITE26 SR    R2,R2               MOVE IN DATA                                 
         ICM   R2,1,SAVELEN                                                     
         BZ    DWRITE28                                                         
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),SAVE                                                     
         AHI   R2,1                                                             
                                                                                
DWRITE28 AR    R6,R2                                                            
                                                                                
DWRITE30 TM    SFLAG1,SFECHAR      END OF SERIES REQUIRED?                      
         BZ    *+16                NO                                           
         MVI   0(R6),ECHARQ                                                     
         AHI   R6,1                                                             
         NI    SFLAG1,FF-(SFECHAR)                                              
                                                                                
         MVI   0(R6),EOB           SET E-O-BUFFER MARKER                        
         ST    R6,ANXTBUFF                                                      
                                                                                
         OC    SAVEELEM,SAVEELEM   TEST ELEMENT PENDING                         
         BZ    DWRITEX                                                          
                                                                                
         LA    R2,SAVEELEM+L'SAVEELEM-1                                         
         CLI   0(R2),0                                                          
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         LA    RE,SAVEELEM                                                      
         SR    R2,RE                                                            
         L     R6,ANXTBUFF         GET NEXT AVAILABLE SLOT                      
         L     RE,FLNK             START OF BUFFER                              
         AH    RE,PGSIZE           END OF BUFFER                                
         SR    RE,R6               REMAINING SPACE                              
         BCTR  RE,0                                                             
         CR    R2,RE                                                            
         BNH   DWRITE32                                                         
         DC    H'0'                                                             
*                                                                               
*   SHOULD NEVER GO TO NEXTBUF                                                  
*                                                                               
***>>>   GOTOR NEXTBUF                                                          
                                                                                
DWRITE32 L     R6,ANXTBUFF                                                      
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),SAVEELEM                                                 
         LA    R6,1(R2,R6)                                                      
         ST    R6,ANXTBUFF                                                      
         XC    SAVEELEM,SAVEELEM                                                
                                                                                
DWRITEX  GOTOR VPROTON                                                          
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* REPEATING CHARACTER OPTIMIZATION (CALLED FROM DWRITE)               *         
***********************************************************************         
                                                                                
REPCHR   NTR1  ,                                                                
         SR    R0,R0                                                            
         IC    R0,SAVELEN                                                       
         CHI   R0,3                POSSIBLY ANYTHING TO OPTIMISE?               
         BH    REPCHR08            YES                                          
                                                                                
         XC    WORK2,WORK2         LOOK FOR REPEAT CHARACTERS                   
         LA    R2,WORK2                                                         
         LA    R1,SAVE                                                          
                                                                                
REPCHR02 CLI   0(R1),REPEATQ                                                    
         BNE   REPCHR04                                                         
         MVC   0(L'REALREP,R2),REALREP                                          
         AHI   R2,L'REALREP        REPEAT CHARACTERS MUST BE DUPLICATED         
         B     REPCHR06                                                         
                                                                                
REPCHR04 MVC   0(1,R2),0(R1)                                                    
         CLI   0(R2),C' '          FIX BAD DATA                                 
         BNL   *+8                                                              
         MVI   0(R2),C' '                                                       
         AHI   R2,1                                                             
                                                                                
REPCHR06 AHI   R1,1                                                             
         BCT   R0,REPCHR02                                                      
                                                                                
         LA    R1,WORK2                                                         
         SR    R2,R1               R2=LENGTH OF OPTIMIZED DATA                  
         STC   R2,SAVELEN                                                       
         MVC   SAVE,WORK2                                                       
         B     REPCHRX                                                          
                                                                                
REPCHR08 LA    R1,SAVE                                                          
         LA    R2,WORK2                                                         
         XC    WORK2,WORK2                                                      
                                                                                
REPCHR10 LR    RE,R1               SAVE A(FIRST CHARACTER)                      
         CLI   0(R1),REPEATQ                                                    
         BNE   REPCHR12                                                         
         MVC   0(L'REALREP,R2),REALREP                                          
         AHI   R2,L'REALREP        REPEAT CHARACTERS MUST BE DUPLICATED         
         B     REPCHR14                                                         
                                                                                
REPCHR12 CLI   0(R1),C' '          FIX BAD DATA                                 
         BNL   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLC   1(2,R1),0(R1)       TEST IF NEXT 2 CHARACTERS SAME               
         BE    REPCHR16                                                         
         MVC   0(1,R2),0(R1)       COPY IN THIS CHARACTER AND ADVANCE           
         AHI   R2,1                                                             
                                                                                
REPCHR14 AHI   R1,1                                                             
         BCT   R0,REPCHR10                                                      
                                                                                
         LA    R1,WORK2                                                         
         SR    R2,R1               R2=LENGTH OF OPTIMIZED DATA                  
         STC   R2,SAVELEN                                                       
         MVC   SAVE,WORK2                                                       
         B     REPCHRX                                                          
                                                                                
REPCHR16 AHI   R1,2                ADVANCE REPEAT POINTER                       
         SHI   R0,2                REDUCE REMAINING LENGTH                      
         BNM   REPCHR18                                                         
         DC    H'0'                OH DEAR                                      
                                                                                
REPCHR18 CLC   0(1,R1),1(R1)       STILL REPEATING?                             
         BNE   *+12                NO                                           
         AHI   R1,1                                                             
         BCT   R0,REPCHR18                                                      
                                                                                
         AHI   R1,1                                                             
         SR    R1,RE               R1=NUM OF REPETITIONS                        
         MVI   0(R2),REPEATQ       INSERT REPEATING INDICATOR                   
         AHI   R2,1                                                             
         LR    R3,R1               SAVE ACTUAL LENGTH                           
                                                                                
REPCHR20 CHI   R1,LCHARL           MULTIPLE CHARACTER LENGTH?                   
         BL    REPCHR22            NO                                           
         MVI   0(R2),LCHAR                                                      
         AHI   R2,1                                                             
         SHI   R1,LCHARL                                                        
         B     REPCHR20                                                         
                                                                                
REPCHR22 STC   R1,BYTE             TRANSLATE LENGTH                             
         TR    BYTE,LENGTHS                                                     
                                                                                
         MVC   0(1,R2),BYTE                                                     
         MVC   1(1,R2),0(RE)                                                    
         AHI   R2,2                                                             
         LR    R1,R3               RESTORE ACTUAL LENGTH                        
         AR    R1,RE               RESET R1 TO LAST REPETITION                  
         BCTR  R1,0                                                             
         B     REPCHR14                                                         
                                                                                
REPCHRX  J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* GET NEXT AVAILABLE PAGE (TWA/TEMPEST) FOR BUFFERING                 *         
*                                                                     *         
* NTRY: AMODE    = 31                                                 *         
*       R9       = A(SAVED STORAGE COVERED BY SAVED)                  *         
*       RC       = A(WORKING STORAGE COVERED BY WORKD)                *         
*                                                                     *         
* EXIT: AMODE    = 31                                                 *         
*       CC NOT SET                                                    *         
***********************************************************************         
                                                                                
NEXTBUF  NTR1  LABEL=NO                                                         
         J     EXIT                                                             
         EJECT                                                                  
SETGLB   BASR  RA,0                POINTS RA TO GLOBAL LITERALS                 
         AHI   RA,GLOBALS-*                                                     
         BR    RE                                                               
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERAL VALUES **                  
                                                                                
EXITLL   CLI   *,EFEF                                                           
         J     EXITT                                                            
                                                                                
EXITY    CR    RE,RE                                                            
                                                                                
EXITT    XIT1  ,                                                                
                                                                                
FALXMODA DC    H'0',C'$ABEND'                                                   
                                                                                
         LTORG                                                                  
                                                                                
HEADELS  DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(02),AL1(LINLEN),X'C2'             
         DC    AL1(CONFLDQ),X'00'                                               
                                                                                
DATAELS  DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(02),AL1(LINLEN),X'C0'             
         DC    AL1(0),X'00'                                                     
                                                                                
         DS    0F                                                               
HOBOFF   DC    X'7FFFFFFF'                                                      
EFFS     DC    X'FFFFFFFF'                                                      
ERRF     DC    C'ERRF'                                                          
ERRC     DC    C'ERRC'                                                          
DMRSRV   DC    CL8'DMRSRV'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
TEMPSTR  DC    CL8'TEMPSTR'                                                     
TEMPEST  DC    CL8'TEMPEST'                                                     
SPACESX  DC    CL80' '                                                          
IDENTIFY DC    C'<FALINK>'         IDENTIFIER IN TWAMSG TO SHOW ACTIVE          
MOREMSG  DC    C'ENCORE'           MORE PROCESSING TO DO                        
REALREP  DC    C'??'               '?' IS REPEAT CHAR '?' BECOMES '??'          
*&&US                                                                           
MAXTRACK EQU   8                   NUMBER OF TRACKS TEMPEST ALLOWS              
MAXPAGES EQU   12                  NUMBER OF PAGES                              
*&&                                                                             
*&&UK                                                                           
MAXTRACK EQU   16                  NUMBER OF TRACKS TEMPEST ALLOWS              
MAXPAGES EQU   24                  NUMBER OF PAGES                              
*&&                                                                             
                                                                                
LENGTHS  DC    C'%ABCDEFGHIJKLMNO'                 00-0F                        
         DC    C'PQRSTUVWXYZ01234'                 10-1F                        
         DC    C'56789abcdefghijk'                 20-2F                        
         DC    C'lmnopqrstuvwxyz+'                 30-3F                        
         DC    C'                '                 40-4F                        
         DC    C'                '                 50-5F                        
         DC    C'                '                 60-6F                        
         DC    C'                '                 70-7F                        
         DC    C'                '                 80-8F                        
         DC    C'                '                 90-9F                        
         DC    C'                '                 A0-AF                        
         DC    C'                '                 B0-BF                        
         DC    C'                '                 C0-CF                        
         DC    C'                '                 D0-DF                        
         DC    C'                '                 E0-EF                        
         DC    C'                '                 F0-FF                        
                                                                                
CNVRTAB  DS    0XL(CNVRTABL)       ** DATA TYPE CONVERSION TABLE **             
         DC    AL1(MDTBIQ)         BINARY (EDIT)                                
         DC    AL2(ADDBIN-T17300,GETBIN-T17300)                                 
         DC    AL1(MDTUSQ)         UNSIGNED BINARY (EDIT)                       
         DC    AL2(ADDUSB-T17300,GETUSB-T17300)                                 
         DC    AL1(MDTPKQ)         PACKED                                       
         DC    AL2(ADDPCK-T17300,GETPCK-T17300)                                 
         DC    AL1(MDTCHQ)         CHARACTER                                    
         DC    AL2(ADDCHR-T17300,GETCHR-T17300)                                 
         DC    AL1(MDTDTQ)         PWOS DATE                                    
         DC    AL2(ADDDTE-T17300,GETDTE-T17300)                                 
         DC    AL1(MDTCDQ)         COMPRESSED DATE                              
         DC    AL2(ADDCDT-T17300,GETCDT-T17300)                                 
         DC    AL1(MDTCAQ)         PACKED CASH AMOUNT                           
         DC    AL2(ADDCSH-T17300,GETCSH-T17300)                                 
         DC    AL1(MDTBCQ)         BINARY CASH AMOUNT                           
         DC    AL2(ADDBCA-T17300,GETBCA-T17300)                                 
         DC    AL1(MDTHXQ)         BINARY (HEXOUT)                              
         DC    AL2(ADDHEX-T17300,GETHEX-T17300)                                 
         DC    AL1(MDTMDQ)         HEADER ONLY                                  
         DC    AL2(ADDMDE-T17300,GETMDE-T17300)                                 
         DC    AL1(MDTJDQ)         JULIAN DATE                                  
         DC    AL2(ADDJDT-T17300,GETJDT-T17300)                                 
         DC    AL1(MDTBDQ)         BINARY DATE                                  
         DC    AL2(ADDBDT-T17300,GETBDT-T17300)                                 
         DC    AL1(MDTUQQ)         DDLINK ONLY                                  
         DC    AL2(ADDUSR-T17300,GETCHR-T17300)                                 
CNVRTABX DC    AL1(EOT)                                                         
                                                                                
*                                                                               
*NVRTABD DSECT                     ** DSECT FOR CONVERSION TABLE **             
*NVRTYPE DS    AL1                 TYPE (FROM MDEQUS)                           
*NVRUPL  DS    AL2                 UPLOAD CONVERSION ROUTINE                    
*NVRDWN  DS    AL2                 DOWNLOAD CONVERSION ROUTINE                  
*NVRTABL EQU   *-CNVRTABD                                                       
*                                                                               
         EJECT                                                                  
***********************************************************************         
* EQUATES                                                             *         
***********************************************************************         
                                                                                
GE$INACT EQU   0011                INVALID ACTION                               
GE$FLERR EQU   0075                FALINK ERROR - CODE &T                       
GE$MISIF EQU   0200                MISSING INPUT FIELD                          
GE$NOTN  EQU   0204                NOT NUMERIC                                  
GE$ISEQ  EQU   0212                INVALID ACTION SEQUENCE                      
GE$YRF   EQU   0088                UPGRADE NOW                                  
*&&UK                                                                           
GE$USN   EQU   0089                UPGRADE SOON                                 
*&&                                                                             
*&&US                                                                           
GE$USN   EQU   0117                UPGRADE SOON                                 
*&&                                                                             
                                                                                
TWMXDSP  EQU   3000                MAX SCREEN SIZE ALLOWED                      
CONFLDQ  EQU   99                  FIELD NUMBER OF CONTROL FIELD                
LINLEN   EQU   79                  WIDTH OF DATA LINE                           
FIRSTLEN EQU   LINLEN-L'FALCON                                                  
                                                                                
EOT      EQU   0                   END OF TABLE                                 
EOB      EQU   0                   END OF BUFFER                                
SCHARQ   EQU   C'('                START OF SERIES CHARACTER                    
ECHARQ   EQU   C')'                END OF SERIES CHARACTER                      
MONLY    EQU   C'%'                MAPCODE ONLY                                 
DONLY    EQU   FF                  ONLY ELEMENT CODE REQUIRED (L'=0)            
REPEATQ  EQU   C'?'                REPEAT CHARACTER                             
ELEMIDQ  EQU   C'='                ELEMENT IDENTIFIER                           
LCHARL   EQU   63                  NORMAL MULTIPLE CHARACTER LENGTH             
LCHAR    EQU   C'+'                NORMAL MULTIPLE CHARACTER                    
XBASIS   EQU   LCHARL*3            EXTENDED MAPCODE CHARACTER LENGTH            
XCHAR    EQU   C'-'                EXTENDED MAPCODE CHARACTER                   
                                                                                
YES      EQU   C'Y'                                                             
MORE     EQU   C'C'                MORE FOR THIS DOWNLOAD                       
FF       EQU   X'FF'                                                            
                                                                                
V1Q      EQU   1                                                                
V2Q      EQU   2                                                                
V3Q      EQU   3                                                                
V4Q      EQU   4                   RUNNER SERVER SUPPORT                        
V5Q      EQU   5                   ASYNCHRONOUS DOWNLOAD SUPPORT                
V6Q      EQU   6                   EXPANDED MAP CODE SUPPORT                    
V7Q      EQU   7                                                                
V8Q      EQU   8                                                                
V9Q      EQU   9                                                                
         EJECT                                                                  
***  >>>>>   FALINK DWRITE ROUTINE                                              
*****>>>>>   FALINK ROUTINES END                                                
****>>>>>       FALINK STRIPPED ROUTINES END                                    
*------------------------------------------------------------------*            
*        THE FOLLOWING BOOKS ARE INCLUDED WITH PRINTING SUPPRESSED              
*                                                                               
*        INCLUDE DDCOREQUS                                                      
*        INCLUDE RECNTAUTOD                                                     
*        INCLUDE SRSTXWRK                                                       
*        INCLUDE DEDEMFILE                                                      
*        INCLUDE REGENSAL2                                                      
*        INCLUDE REGENDAR                                                       
*        INCLUDE REGENCFC                                                       
*        INCLUDE FAFACTS                                                        
*        INCLUDE FAXTRAINF                                                      
*        INCLUDE CTGENFILE                                                      
*        INCLUDE SEACSFILE                                                      
*------------------------------------------------------------------*            
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE RECNTAUTOD                                                     
       ++INCLUDE SRSTXWRK                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE REGENSAL2                                                      
       ++INCLUDE REGENDAR                                                       
       ++INCLUDE REGENCFC                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE REGENEOP                                                       
*                                                                               
       ++INCLUDE DDTWABLDD                                                      
*                                                                               
         EJECT                                                                  
                                                                                
CNVRTABD DSECT                     ** DSECT FOR CONVERSION TABLE **             
CNVRTYPE DS    AL1                 TYPE (FROM MDEQUS)                           
CNVRUPL  DS    AL2                 UPLOAD CONVERSION ROUTINE                    
CNVRDWN  DS    AL2                 DOWNLOAD CONVERSION ROUTINE                  
CNVRTABL EQU   *-CNVRTABD                                                       
*                                                                               
BROADTBL DSECT                                                                  
BRDTABLE DS    0CL7                                                             
         ORG   BRDTABLE                                                         
BRDSTART DS    XL3                 BINARY MONTH START DATE                      
BRDEND   DS    XL3                 BINARY MONTH END   DATE                      
BRDWEEKS DS    XL1                 NUM WEEKS IN PERIOD                          
BRDLEN   EQU   *-BRDSTART          LENGTH OF ENTRY                              
*                                                                               
DCWORKD  DSECT                                                                  
DARCOM   DS    CL(10*60)                                                        
DCWORKQ  EQU   *-DCWORKD                                                        
       ++INCLUDE RECNTPROF                                                      
       ++INCLUDE FAXTRAINF                                                      
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FAADRREC                                                       
       ++INCLUDE FAATC                                                          
       ++INCLUDE FACTRY                                                         
       ++INCLUDE FADECB                                                         
       ++INCLUDE FADMPHDR                                                       
       ++INCLUDE FAJOBTAB                                                       
       ++INCLUDE FALANG                                                         
       ++INCLUDE FALERB                                                         
       ++INCLUDE FAPGMLST                                                       
****      NCLUDE FAPHLIST                                                       
****      NCLUDE FASCREQUS                                                      
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FASRPARM                                                       
       ++INCLUDE FASRS                                                          
       ++INCLUDE FASSB                                                          
       ++INCLUDE FATCB                                                          
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FATSTTAB                                                       
       ++INCLUDE FASCTTAB                                                       
       ++INCLUDE FATWA                                                          
       ++INCLUDE FAUPDTAB                                                       
       ++INCLUDE FAUSRTAB                                                       
       ++INCLUDE FAUTL                                                          
       ++INCLUDE FAVRSNTAB                                                      
*********INCLUDE FADSECTS                                                       
DRECVHDR DSECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
*   PARKING SPACE FOR CODE I MIGHT USE LATER                                    
*                                                                               
*   THERE IS NO OVERLAY 10, SO ALL THIS STUFF WON'T WORK                        
*                                                                               
*&&DO                                                                           
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VPRDQ,ADDR2),DMCB,(RC),(0,VHPPRD),(0,VHPADV),ADDR               
         BNE   DCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,PRDTBL-(*-4)                                                  
         GOTO1 =A(XARSE),DMCB,ADDR,(R0),RR=Y                                    
*                                                                               
NOGO0022 DS    0H                                                               
         CLC   VHPCTY,SPACES                                                    
         BNH   NOGO0030                                                         
* CTY                                                                           
         GOTO1 ASETELEM,DMCB,AFABLK,CNTDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R0,L'VHPCTY                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CNTCTYEL,VHPCTY,(R0)                        
*                                                                               
NOGO0030 DS    0H                                                               
         GOTOX (VSTAQ,ADDR2),DMCB,(RC),(0,VHPSTA),ADDR                          
         BNE   DCONERR                                                          
* STA                                                                           
         LA    R0,*                                                             
         AHI   R0,STATBL-(*-4)                                                  
         GOTO1 =A(XARSE),DMCB,ADDR,(R0),RR=Y                                    
*        STAREC IS STILL IN AIOREC FROM VSTA CALL.                              
         MVI   TRAFSYS,0                                                        
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSTAELEM,R8                                                      
*              TRAFFIC SYSTEM                                                   
         CLC   RSTATRAF,C' '                                                    
         BNH   DSTA0020                                                         
         MVC   TRAFSYS,RSTATRAF                                                 
         GOTO1 AADDDATA,DMCB,AFABLK,STATRAF,RSTATRAF,0                          
                                                                                
         MVI   EOPREQ,C'N'                                                      
         CLI   RSTATRAF,C'B'                                                    
         BNE   *+8                                                              
         MVI   EOPREQ,C'Y'                                                      
         CLI   RSTATRAF,C'W'                                                    
         BNE   *+8                                                              
         MVI   EOPREQ,C'Y'                                                      
         CLI   RSTATRAF,C'J'                                                    
         BNE   *+8                                                              
         MVI   EOPREQ,C'Y'                                                      
         CLI   RSTATRAF,C'C'                                                    
         BNE   *+8                                                              
         MVI   EOPREQ,C'Y'                                                      
         DROP  R8                                                               
                                                                                
DSTA0020 EQU   *                                                                
         MVI   ECYN,0                                                           
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'08'        EXTRA DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSTAXXEL,R8                                                      
*              IS THIS AN EC STATION (Y/N)                                      
         TM    RSTAXOPT,X'80'                                                   
         BZ    *+8                                                              
         MVI   ECYN,1                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,STAECYN,ECYN,0                              
                                                                                
*              EOP REQUIRED (Y/N)                                               
         MVI   BYTE2,0                                                          
         CLI   RSTAOPT9,C'Y'                                                    
         BNE   *+8                                                              
         MVI   BYTE2,1                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,STAEOPRQ,BYTE2,0                            
*&&DO                                                                           
         MVI   SOMTEMP,C'N'                                                     
         TM    SOMPROF,X'80'                                                    
         BZ    *+8                                                              
         MVI   SOMTEMP,C'Y'                                                     
         GOTO1 AADDDATA,DMCB,AFABLK,STAEMLYN,SOMTEMP,0                          
*&&                                                                             
*   LOAD AN ARRAY OF PROFILE VALUES                                             
*                                                                               
         MVI   WORK2,C'N'          SET FIRST TO 'N'                             
         MVC   WORK2+1(39),WORK2   SET REMAINDER OF STRING                      
         MVC   WORK2(9),RSTAOPTS   MOVE FIRST 9 OPTS - THESE ARE ALL            
*                                     ACTUAL VALUES                             
         MVC   BYTE2,RSTAOPTA      UNLOAD OPTIONS BYTE                          
         LA    R1,WORK2+9          SET A(NEXT ARRAY POSITION)                   
         BAS   RE,SETARRAY         PROCESS OPTION BYTE                          
         MVC   BYTE2,RSTAOPTB      UNLOAD OPTIONS BYTE                          
         LA    R1,WORK2+17         SET A(NEXT ARRAY POSITION)                   
         BAS   RE,SETARRAY         PROCESS OPTION BYTE                          
         MVC   BYTE2,RSTAOPTC      UNLOAD OPTIONS BYTE                          
         LA    R1,WORK2+25         SET A(NEXT ARRAY POSITION)                   
         BAS   RE,SETARRAY         PROCESS OPTION BYTE                          
*                                                                               
         DROP  R8                                                               
*                                                                               
         LA    R0,40               SET LENGTH                                   
         GOTO1 AADDDATA,DMCB,AFABLK,STAPROFS,WORK2,(R0)                         
*                                                                               
* STAREC IS STILL IN AIOREC FROM VSTA CALL.                                     
*  GET THE STATION EMAIL ADDRESS FROM IT.                                       
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'25'        SALES ASSISTANT ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   DSTA0060                                                         
*                                                                               
DSTA0040 DS    0H                                                               
         ZIC   R0,1(R8)            LENGTH                                       
         SHI   R0,RSTAADD-RSTAEML                                               
         USING RSTAEML,R8                                                       
         GOTO1 AADDDATA,DMCB,AFABLK,STAEMLEL,RSTAADD,(R0)                       
         DROP  R8                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DSTA0040                                                         
*                                                                               
DSTA0060 DS    0H                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
* ADV                                                                           
         GOTOX (VADVQ,ADDR2),DMCB,(RC),(0,VHPADV),ADDR                          
         BNE   DCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,ADVTBL-(*-4)                                                  
         GOTO1 =A(XARSE),DMCB,ADDR,(R0),RR=Y                                    
*                                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
* AGY                                                                           
         GOTOX (VAGYQ,ADDR2),DMCB,(RC),(0,VHPAGY),(0,VHPAOF),ADDR               
         BNE   DCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,AGYTBL-(*-4)                                                  
         GOTO1 =A(XARSE),DMCB,ADDR,(R0),RR=Y                                    
*                                                                               
         GOTOX (AGYADDRQ,ADDR2),DMCB,(RC),(0,VHPAGY),(0,VHPAOF),       +        
               (0,VHPADV),(0,VHPCTY),ADDR                                       
         BNE   NOGO0040                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,AGADTBL-(*-4)                                                 
         GOTO1 =A(XARSE),DMCB,ADDR,(R0),RR=Y                                    
*                                                                               
NOGO0040 DS    0H                                                               
         GOTOX (VSALQ,ADDR2),DMCB,(RC),(0,VHPSAL),ADDR                          
         BE    *+14                                                             
         MVC   ERROR,=Y(154)                                                    
         B     EXITL                                                            
* SAL                                                                           
         LA    R0,*                                                             
         AHI   R0,SALTBL-(*-4)                                                  
         GOTO1 =A(XARSE),DMCB,ADDR,(R0),RR=Y                                    
* SAA                                                                           
*                                                                               
K        USING RSA2KEY,KEY                                                      
         XC    K.RSA2KEY,K.RSA2KEY                                              
         MVI   K.RSA2KTYP,X'46'                                                 
         MVC   K.RSA2KREP,REPALPHA                                              
         MVC   K.RSA2KSAL,VHPSAL                                                
         OC    K.RSA2KSAL,SPACES                                                
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RSA2KEY),KEYSAVE                                           
         BNE   NOGO0044            MISSING PERSON 2 RECORD                      
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'21'        SALES ASSISTANT ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   NOGO0044                                                         
*                                                                               
         USING RSASEMEM,R8                                                      
         GOTO1 ASETELEM,DMCB,AFABLK,SAADATA,0                                   
         GOTO1 AADDDATA,DMCB,AFABLK,SAANAMEL,RSASEMNM,0                         
         TM    RSASEMFL,X'80'                                                   
         BZ    NOGO0042                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,SAAEFLEL,0,0                                
*                                                                               
NOGO0042 DS    0H                                                               
         ZIC   R0,1(R8)            LENGTH                                       
         SHI   R0,RSASEAML-RSASEMEM                                             
         GOTO1 AADDDATA,DMCB,AFABLK,SAAEMLEL,RSASEAML,(R0)                      
         DROP  R8                                                               
*                                                                               
NOGO0044 DS    0H                                                               
         CLC   VHPDSP,SPACES                                                    
         BNH   NOGO0050                                                         
* DSP                                                                           
         GOTOX (VDEVSALQ,ADDR2),DMCB,(RC),(0,VHPDSP),ADDR                       
         BNE   DCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,DSPTBL-(*-4)                                                  
         GOTO1 =A(XARSE),DMCB,ADDR,(R0),RR=Y                                    
*                                                                               
NOGO0050 DS    0H                                                               
         CLC   VHPDCT,SPACES                                                    
         BNH   NOGO0060                                                         
* DCT                                                                           
         GOTOX (VDEVTYPQ,ADDR2),DMCB,(RC),(0,VHPDCT),ADDR                       
         BNE   DCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,DCTTBL-(*-4)                                                  
         GOTO1 =A(XARSE),DMCB,ADDR,(R0),RR=Y                                    
*                                                                               
NOGO0060 DS    0H                                                               
         TM    MISCFLG2,DCNOCOM    NO COMMENTS?                                 
         BNZ   NOGO0100                                                         
* COMMENTS                                                                      
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),VHPKDA    HEADER D/A                                   
         GOTO1 VGETREC,AIOREC      REREAD HEADER REC                            
*&&                                                                             
*-------------------------------------------------------------------*           
* READ BUYLINES FOR PC                                                          
*-------------------------------------------------------------------*           
*&&DO                                                                           
NOGO0100 DS    0H                                                               
         TM    MISCFLG2,DCBUYS                                                  
         BZ    NOGO0110                                                         
*                                                                               
* BUYS                                                                          
***      BAS   RE,DCONDBUY                                                      
*                                                                               
NOGO0110 DS    0H                  GENERATE WORKSHEET DOWNLOAD                  
         GOTO1 VCOLY,DMCB,(X'11',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
* WKS                                                                           
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,VHPCON,(RC),FAMAP                                      
         B     EXITOK                                                           
*&&                                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'144SRSTX00   05/21/07'                                      
         END                                                                    
