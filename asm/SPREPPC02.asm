*          DATA SET SPREPPC02  AT LEVEL 003 AS OF 02/28/05                      
*PHASE SPPC02C                                                                  
***********************************************************************         
* ORIGINALLY SPREPUP02P, CONVERTED TO SPREPPC02 TO BE A REAL REPORT             
***********************************************************************         
SPPC02   TITLE 'SPREPPC02 - PRINT CABLE AND NETWORKS FOR AN AGENCY'             
SPPC02   CSECT                                                                  
         DS    8192C                                                            
         ORG   *-8192                                                           
         PRINT NOGEN                                                            
*                                  GOT 3 BASE REGISTERS                         
         NMOD1 0,SPPC02,R8,R7                                                   
*                                                                               
         LR    RC,RB               SET UP MY WORK AREA                          
         AH    RC,=Y(SPPCWRKD-SPPC02)                                           
         USING SPPCWRKD,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    MAIN                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* MAIN                                                                          
***************                                                                 
*     MAIN SECTION OF THE PROGRAM                                               
***********************************************************************         
*                                                                               
MAIN     DS    0H                                                               
         LR    R6,RB               SET ADDRESS OF RECORD FOR SPONSOR            
         A     R6,=A(SPOTREC-SPPC02)                                            
         ST    R6,AREC1                                                         
         LR    R6,RB               SET ADDRESS OF RECORD #2 FOR SPONSOR         
         A     R6,=A(SPOTREC2-SPPC02)                                           
         ST    R6,AREC2                                                         
         ST    R6,ADSTAT                                                        
         ST    R6,ADSTATAD                                                      
*                                                                               
         MVC   AREC,AREC1          DEFAULT RECORD AREA                          
*                                                                               
         ZAP   COUNTER1,=P'0'      # OF RECORDS THAT HAVE PROBLEMS              
         XC    SVDHDEND,SVDHDEND                                                
*                                                                               
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QCABLETB                                                  
         L     RF,ACOMFACS                                                      
         L     RF,(CCALLOV-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         ST    RF,ACABLTAB                                                      
         SR    R0,R0                                                            
         IC    R0,5(RF)            GET ENTRY LENGTH                             
         STH   R0,CBLTABLN                                                      
*                                                                               
MAIN10   BAS   RE,READRECS                                                      
*                                                                               
NOMORE   MVC   P(43),=CL43'# OF RECS WITH PROBLEMS ON THIS SPOT FILE: '         
         EDIT  (P8,COUNTER1),(17,P+43),ALIGN=LEFT,ZERO=NOBLANK                  
         GOTO1 REPORT                                                           
*                                                                               
NOMOREX  MVI   MODE,REQLAST                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS THROUGH ALL THE RECORDS ON THE SPOT FILE                   
***********************************************************************         
*                                                                               
READRECS NTR1                                                                   
*                                                                               
         XC    KEY,KEY             SET THE STATION RECORD KEY                   
         LA    R4,KEY                                                           
         USING STAKEY,R4                                                        
         MVC   STAKTYPE(2),=C'ST'                                               
         MVI   STAKCALL,C'0'                                                    
         DROP  R4                                                               
*                                                                               
         BAS   RE,STAHIGH                                                       
RREC10   CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AREC                                                          
         USING STAREC,R6                                                        
         CLC   =C'ST',STAKTYPE                                                  
         BNE   RRECX                                                            
*****                                                                           
* SHOW CABLE STATION AND NAME                                                   
*****                                                                           
         CLI   QOPT3,C'Y'          DON'T SHOW DEACTIVATED STATIONS?             
         BNE   RREC15                                                           
         CLI   SSYSDACT,X'FF'      THIS STATION DEACTIVATED?                    
         BE    RREC200             YES, SKIP THIS RECORD                        
*                                                                               
RREC15   DS    0H                                                               
         OC    SSYSNAME,SSYSNAME                                                
         BZ    RREC20                                                           
         ZICM  R3,STAKLEN,2                                                     
         LA    RE,SCBLSQNQ                                                      
         CR    R3,RE               RECORD LENGTH =  SCBLSQNQ?                   
         BNE   RREC20                                                           
         OC    SCBL24,SCBL24                                                    
         BNZ   *+14                                                             
         OC    SCBLSEQ,SCBLSEQ                                                  
         BZ    RREC20                                                           
         XC    WKTOP24,WKTOP24                                                  
         XC    WKCBLSEQ,WKCBLSEQ                                                
         MVC   WKTOP24,SCBL24                                                   
         MVC   WKCBLSEQ,SCBLSEQ                                                 
*                                                                               
         CLI   QOPT2,C'Y'          LIST ALL STATIONS & THEIR NETWORKS?          
         BNE   RREC200                                                          
         B     RREC30              YES                                          
*                                                                               
RREC20   AP    COUNTER1,=P'1'                                                   
         MVC   P(46),=C'**************  PROBLEM RECORD  **************'         
         GOTO1 REPORT                                                           
*                                                                               
RREC30   MVC   P(8),=C'AGENCY: '                                                
         MVC   P+8(L'STAKAGY),STAKAGY                                           
*                                                                               
         CLC   =C'000',STAKCLT                                                  
         BE    RREC35                                                           
         MVC   P(10),=C'EXCEPTION:'                                             
         MVC   P+70(8),=C'CLIENT: '                                             
         MVC   P+78(L'STAKCLT),STAKCLT                                          
*                                                                               
RREC35   MVC   P+15(15),=CL15'CABLE STATION: '                                  
         MVC   P+30(5),STAKCALL                                                 
*                                                                               
         MVC   P+40(L'SSYSNAME),SSYSNAME                                        
         OC    SSYSNAME,SSYSNAME                                                
         BNZ   *+10                                                             
         MVC   P+40(24),=CL24'---> NO NAME'                                     
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,STAKLEN                                                     
         CHI   R0,SCBLSQNQ                                                      
         BNE   RREC191                                                          
*                                                                               
***********************************************************************         
* SHOW NETWORKS                                                                 
***********************************************************************         
*                                                                               
RREC100  MVC   P(10),=C'NETWORKS: '                                             
*                                                                               
         OC    WKTOP24,WKTOP24     ANY TOP 24 TO PRINT?                         
         BNZ   RREC110             YES                                          
         OC    WKCBLSEQ,WKCBLSEQ   ANY CABLE SEQ TO PRINT?                      
         BNZ   RREC150             YES                                          
*                                                                               
         MVC   P+10(16),=CL16'---> NO NETWORKS'                                 
         GOTO1 REPORT                                                           
         B     RREC200                                                          
*                                                                               
RREC110  MVC   SVTOP24,WKTOP24                                                  
         LA    R1,P+10                                                          
         SR    R2,R2               NUMBER OF CABLE NETWORK PER LINE             
         LA    R3,SVTOP24          USING SAVED VERSION OF TOP 24                
         L     R4,ACABLTAB                                                      
*                                                                               
RREC120  TM    6(R4),X'40'         TOP 24 CABLE NETWORK?                        
         BZ    RREC130             NO, GET NEXT ITEM IN TABLE                   
*                                                                               
         TM    0(R3),X'80'         CURRENT TOP 24 BIT IS ON?                    
         BZ    RREC140             NO                                           
         MVC   0(3,R1),0(R4)                                                    
         MVI   3(R1),C' '                                                       
         LA    R1,4(R1)            NEXT POSITION ON PRINT LINE                  
         LA    R2,1(R2)            NUMBER OF NETWORK PER LINE COUNTER           
*                                                                               
RREC140  ZICM  R5,SVTOP24,3                                                     
         SLL   R5,1                1 BIT LESS TO TEST                           
         STCM  R5,7,SVTOP24                                                     
         LA    R3,SVTOP24                                                       
*                                                                               
RREC130  AH    R4,CBLTABLN                                                      
         OC    SVTOP24,SVTOP24     STILL MORE TOP 24 BITS ARE ON?               
         BNZ   RREC120             YES, TEST SOME MORE                          
         OC    WKCBLSEQ,WKCBLSEQ   ANY CABLE SEQ TO PRINT?                      
         BNZ   RREC150             YES                                          
         GOTO1 REPORT              JUST PRINT OUT TOP 24 AND DONE               
         B     RREC200                                                          
*                                                                               
*REC150  LA    R0,127              SET LOOP COUNT                               
RREC150  DS    0H                                                               
         LA    R3,WKCBLSEQ                                                      
         L     R4,ACABLTAB                                                      
         LA    RE,WKCBLSEQ+L'WKCBLSEQ    END OF WKCBLSEQ                        
         ST    RE,ADDRSAVE         SAVE END OF WKCBLSEQ                         
*                                                                               
         OC    WKTOP24,WKTOP24                                                  
         BNZ   RREC160                                                          
         MVC   P(10),=CL10'NETWORKS: '                                          
         LA    R1,P+10                                                          
         SR    R2,R2               NUMBER OF CABLE NETWORK PER LINE             
*                                                                               
RREC160  DS    0H                                                               
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    RREC191                                                          
         TM    6(R4),X'40'         TOP 24 CABLE NETWORK?                        
         BNZ   RREC190             YES, GET NEXT ITEM IN CABLE TABLE            
*                                                                               
RREC170  DS    0H                                                               
         C     R3,ADDRSAVE                                                      
         BNL   RREC190                                                          
         CLC   0(2,R3),=XL2'0'                                                  
         BE    RREC190                                                          
         CLC   0(2,R3),3(R4)                                                    
         BNE   RREC180                                                          
         CH    R2,=H'30'                                                        
         BL    RREC175                                                          
         GOTO1 REPORT              CURRENT LINE IS FULL, PRINT IT               
         SR    R2,R2               RESET COUNTER                                
         LA    R1,P+10             RESET PRINT LINE POINTER                     
*                                                                               
RREC175  MVC   0(3,R1),0(R4)                                                    
         MVI   3(R1),C' '                                                       
         LA    R1,4(R1)            NEXT POSITION ON PRINT LINE                  
         LA    R2,1(R2)            NUMBER OF NETWORK PER LINE COUNTER           
         B     RREC190                                                          
*                                                                               
RREC180  LA    R3,2(R3)            NEXT ITEM IN CABLE SEQ                       
         B     RREC170                                                          
*                                                                               
RREC190  AH    R4,CBLTABLN                                                      
         LA    R3,WKCBLSEQ         REPOINT TO CABLE SEQ                         
*        BCT   R0,RREC160                                                       
         B     RREC160                                                          
*                                                                               
RREC191  DS    0H                                                               
*                                                                               
         GOTO1 REPORT                                                           
         XC    WKTOP24,WKTOP24                                                  
         XC    SVTOP24,SVTOP24                                                  
         XC    WKCBLSEQ,WKCBLSEQ                                                
*        MVC   P(35),=C'*** RECORD LENGTH IS HEX         ***'                   
*        EDIT  (R2),(9,P+25),ALIGN=LEFT,ZERO=BLANK                              
*        GOTO1 HEXOUT,DMCB,STAKLEN,P+25,2,0                                     
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
******** DC    H'0'                DEATH                                        
*                                                                               
***********************************************************************         
*                                                                               
RREC200  BAS   RE,STASEQ                                                        
         TM    DMCB+8,X'80'                                                     
         BNZ   RRECX                                                            
         B     RREC10                                                           
*                                                                               
RRECX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CTFILE CALLS                                                                  
***********************************************************************         
CTLHIGH  MVC   COMMAND,DMRDHI                                                   
         B     CTLFILE                                                          
*                                                                               
CTLSEQ   MVC   COMMAND,DMRSEQ                                                   
*                                                                               
CTLFILE  MVC   DATADISP,=Y(CT5DATA-CT5REC)                                      
         ST    RE,SVDREGRE                                                      
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'CTFILE',KEY,AREC               
         L     RE,SVDREGRE                                                      
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* STATION CALLS                                                                 
***********************************************************************         
STAHIGHD OI    DMINBTS,X'08'                                                    
STAHIGH  MVC   COMMAND,DMRDHI                                                   
         B     STAFILE                                                          
*                                                                               
STASEQ   MVC   COMMAND,DMRSEQ                                                   
         B     STAFILE                                                          
*                                                                               
STAADD   CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,DMADD                                                    
         B     STAFILE                                                          
*                                                                               
STAWRT   CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,DMWRT                                                    
*                                                                               
STAFILE  ST    RE,SVDREGRE                                                      
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),STATION,KEY,AREC                  
         MVI   DMINBTS,0           ALWAYS CLEAR OUT DMINBTS                     
         L     RE,SVDREGRE                                                      
         BR    RE                                                               
*                                                                               
***********************************************************************         
* SPTDIR CALLS                                                                  
***********************************************************************         
SPHIGHD  OI    DMINBTS,X'08'                                                    
SPHIGH   MVC   COMMAND,DMRDHI                                                   
         MVC   KEYSAVE,KEY                                                      
         B     SPDIR                                                            
*                                                                               
SPADD    CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,DMADD                                                    
         B     SPDIR                                                            
*                                                                               
SPWRT    CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,DMWRT                                                    
*                                                                               
SPDIR    ST    RE,SVDREGRE                                                      
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),SPTDIR,KEY,KEY                    
         MVI   DMINBTS,0           ALWAYS CLEAR OUT DMINBTS                     
         L     RE,SVDREGRE                                                      
         BR    RE                                                               
*                                                                               
***********************************************************************         
* SPTFILE CALLS                                                                 
***********************************************************************         
SPFGETR  MVC   COMMAND,GETREC                                                   
         B     SPFILE                                                           
*                                                                               
SPFADDR  CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,ADDREC                                                   
         B     SPFILE                                                           
*                                                                               
SPFPUTR  CLI   RCWRITE,C'Y'                                                     
         BNE   DONTWRIT                                                         
         MVC   COMMAND,PUTREC                                                   
*                                                                               
SPFILE   MVC   DATADISP,=Y(STADTAEL-STADDKEY)                                   
         ST    RE,SVDREGRE                                                      
         GOTO1 DATAMGR,DMCB,COMMAND,SPTFILE,KEY,AREC,DMWORK                     
         L     RE,SVDREGRE                                                      
         BR    RE                                                               
*                                                                               
***********************************************************************         
* IN CASE RCWRITE=C'N', THEN ASSUME NO ERROR FROM DATAMGR                       
***********************************************************************         
DONTWRIT MVI   DMCB+8,0            MAKE IT SEEM LIKE WE HAVE NO ERROR           
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* TRACE DATA BLOCK                                                              
*                                                                               
*        PARAMETER 1 - A(DATA)                                                  
*        PARAMETER 2 - L(DATA)  OR  ZERO FOR ELEMENTAL RECORD                   
*        PARAMETER 3 - A(LABEL) OR  ZERO FOR NO LABEL                           
*        PARAMETER 4 - L(LABEL) IF PARM 3 IS NOT ZERO                           
*                                                                               
*=================================================================*             
MYTRACE  NTR1                                                                   
         CLI   QOPT1,C'Y'          OPTION SET TO DISPLAY TRACE?                 
         BNE   TRX                 NO                                           
*                                                                               
         LM    R2,R5,0(R1)         R2 = A(DATA)                                 
*                                  R3 = L(DATA)                                 
*                                  R4 = A(LABEL)                                
*                                  R5 = L(LABEL)                                
*                                                                               
         LTR   R4,R4               IF CALLER SUPPLIED A LABEL                   
         BZ    TR10                                                             
*                                                                               
         MVI   P,C'-'              THEN FILL PRINT LINE WITH '-'S               
         MVC   P+1(131),P                                                       
*                                                                               
         LR    RE,R5               RF = A(PLACE TO CENTER LABEL)                
         SRL   RE,1                                                             
         LA    RF,66                                                            
         SR    RF,RE                                                            
         LA    RF,P(RF)                                                         
*                                                                               
         BCTR  R5,0                MOVE LABEL TO CENTER OF PRINT LINE           
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)                                                    
*                                                                               
         GOTO1 REPORT              PRINT LABEL LINE                             
*                                                                               
TR10     LTR   R3,R3               IF DATA IS A RECORD                          
         BNZ   TR50                                                             
         OC    DATADISP,DATADISP   IF THERE IS A KEY                            
         BZ    TR15                                                             
*                                  PRINT OUT ITS KEY                            
         LH    R3,DATADISP                                                      
         GOTO1 PRNTBL,DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                         
*                                                                               
TR15     LR    R6,R2               A(RECORD)                                    
         AH    R6,DATADISP         + DISPLACEMENT TO FIRST ELEMENT              
         MVI   ELCODE,0                                                         
         BAS   RE,FIRSTEL                                                       
         BNE   TR100                                                            
*                                                                               
TR20     ZIC   R4,1(R6)            PRINT ELEMENT                                
         GOTO1 PRNTBL,DMCB,0,(R6),C'DUMP',(R4),=X'01C4'                         
*                                                                               
         BAS   RE,NEXTEL           REPEAT UNTIL NO MORE ELEMENTS                
         BE    TR20                                                             
         B     TR100                                                            
*                                  ELSE PRINT ENTIRE DATA BLOCK                 
TR50     GOTO1 PRNTBL,DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                         
*                                                                               
TR100    DS    0H                                                               
*                                                                               
TRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VARIABLES AND STUFF                                                           
***********************************************************************         
CODTABLE DS    0XL2                                                             
         DC    C'D',AL1(CDNEWSYS)      NEW SYSTEM                               
         DC    C'E',AL1(CDPAYADR)      PAYABLE ADDRESS CHANGE                   
         DC    C'F',AL1(CDNETCHG)      NETWORK CHANGE                           
         DC    C'G',AL1(CDEXCLUS)      EXCLUSIVITY CHANGE                       
         DC    C'H',AL1(CDSYSNAM)      SYSTEM NAME CHANGE                       
         DC    C'I',AL1(CDMSOINT)      MSO AND INTERCONNECT CHANGE              
         DC    C'J',AL1(CDTPEADR)      TAPE ADDRESS CHANGE                      
         DC    C'X',AL1(CDNTACTV)      SYSTEM DEACTIVATED                       
         DC    X'00'                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
*                                                                               
       ++INCLUDE SPCBLLST                                                       
         EJECT                                                                  
SPPCWRKD DS    0A                                                               
AREC1    DS    A                   A(FIRST RECORD AREA)                         
AREC2    DS    A                   A(SECOND RECORD AREA)                        
ACABLTAB DS    A                   A(CABLE NETWORK TABLE)                       
CBLTABLN DS    H                                                                
         DS    H                                                                
*                                                                               
SVDREGRE DS    A                   SAVED REGISTER RE                            
ADDRSAVE DS    F                                                                
*                                                                               
COUNTER1 DS    PL8                 COUNTERS                                     
COUNTER2 DS    PL8                                                              
COUNTER3 DS    PL8                                                              
COUNTER4 DS    PL8                                                              
COUNTER5 DS    PL8                                                              
*                                                                               
BITFLAG1 DS    XL1                 FIRST SET OF BIT FLAGS                       
B1DBLQTE EQU   X'80'                   WE FOUND A DOUBLE QUOTE ALREADY          
B1COMMA  EQU   X'40'                   WE FOUND A COMMA ALREADY                 
B1NETWRK EQU   X'20'                   GOT NETWORK DATA                         
B1ACTION EQU   X'10'                   GOT ACTION CODE DATA                     
*                                                                               
BITFLAG2 DS    XL1                 SECOND SET OF BIT FLAGS                      
B2ADDING EQU   X'80'                   ADD RECORD (0=WRITE RECORD)              
*                                                                               
ACTNFLAG DS    XL1                 SAME BIT DEFINITION AS CODEFLAG              
CODEFLAG DS    XL1                 ACTION CODE BIT FLAG                         
CDNEWSYS EQU   X'80'                   'D' - NEW SYSTEM                         
CDPAYADR EQU   X'40'                   'E' - PAYABLE ADDRESS CHANGE             
CDNETCHG EQU   X'20'                   'F' - NETWORK CHANGE                     
CDEXCLUS EQU   X'10'                   'G' - EXCLUSIVITY CHANGE                 
CDSYSNAM EQU   X'08'                   'H' - SYSTEM NAME CHANGE                 
CDMSOINT EQU   X'04'                   'I' - MSO/INTERCONNECT CHANGE            
CDTPEADR EQU   X'02'                   'J' - TAPE ADDRESS CHANGE                
CDNTACTV EQU   X'01'                   'X' - SYSTEM DEACTIVATED                 
*                                                                               
ELCODE   DS    XL1                                                              
*                                                                               
ACTHDEND DS    CL5                 ACTION CABLE HEADEND (LAST USED)             
SYSHDEND DS    CL5                 SYSTEM CABLE HEADEND (LAST USED)             
NETHDEND DS    CL5                 SYSTEM CABLE HEADEND (LAST USED)             
CBLHDEND DS    CL5                 CABLE HEADEND (CURRENT)                      
SVDHDEND DS    CL5                 STATION CALL LETTERS                         
NUMMRKT  DS    CL4                 MKT NUM EBCDIC (BASED ON ALPHA MKT)          
PRVMRKT  DS    CL4                 PREVIOUS MARKET OF THE STATION               
TODAYDTE DS    CL6                 TODAY'S DATE EBCDIC YY/MM/DD                 
*                                                                               
SPTSENUM DS    XL1                 SPOT SENUM                                   
XCPTCLT  DS    CL3                 EXCEPTION CLIENT RECORD, 000 = REG.          
*                                                                               
*                                                                               
WKTOP24  DC    XL3'00'             WORKING FIELD FOR TOP 24 NETWORKS            
SVTOP24  DC    XL3'00'             SAVED WORK FIELD OF TOP 24 NETWORKS          
WKCBLSEQ DC    XL206'00'           WORKING FIELD FOR CABLE SEQ NETWORKS         
*                                                                               
SPOTREC  DS    CL4000                                                           
SPOTREC2 DS    CL4000                                                           
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
GENSTAD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
GENADDD  DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
         EJECT                                                                  
GENCBLD  DSECT                                                                  
       ++INCLUDE SPGENCBL                                                       
         EJECT                                                                  
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
       ++INCLUDE SPTRSTA                                                        
STARECX  EQU   *                   END OF TRAFFIC ADDRESS RECORD                
         EJECT                                                                  
*SPGENAGY                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
*SPGENANMK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENANMK                                                      
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPPC02 02/28/05'                                      
         END                                                                    
