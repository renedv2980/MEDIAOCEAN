*          DATA SET SPREPMS02  AT LEVEL 046 AS OF 05/01/02                      
*PHASE SPMS02A                                                                  
*INCLUDE SPLDCPTR                                                               
         SPACE                                                                  
SPMS02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPMS02,R7,R8                                                   
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
* CONTROL SECTION *                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    SP000               GET THE NEW STATION CODE                     
         CLI   MODE,REQLAST                                                     
         BE    SP400               UPDATE THE INVOICE RECORDS                   
         CLI   MODE,RUNLAST                                                     
         BE    PRTOUT                                                           
         CLI   MODE,RUNFRST                                                     
         BNE   EXIT                                                             
         L     RE,SSB                                                           
         OI    3(RE),X'08'         NEED TO RECOVER COPIES AND CHANGES           
         STM   R7,RB,SPSCR7                                                     
         B     EXIT                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
* MAKE SURE THIS IS A CANADIAN AGENCY                                           
*                                                                               
SP000    L     R1,ADAGY                                                         
         USING AGYHDRD,R1                                                       
         CLI   AGYPROF+7,C'C'                                                   
         BE    SP010                                                            
         GOTO1 AENDREQ                                                          
         DROP  R1                                                               
*                                                                               
SP010    BAS   RE,WWSTAFIX                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(1),SVAGYMD      A-M                                          
         GOTO1 HIGH                                                             
         BE    SP030                                                            
*                                                                               
SP020    GOTO1 SEQ                                                              
*                                                                               
SP030    CLC   KEY(1),KEYSAVE                                                   
         BNE   SPBUYX                                                           
*                                                                               
         MVC   DUB(1),KEY                                                       
         NI    DUB,X'0F'                                                        
         CLI   DUB,1               TV                                           
         BE    SP040                                                            
         CLI   DUB,3               NETWORK                                      
         BE    SP040                                                            
         CLI   DUB,8               IS THIS A COMBINED BUY?                      
         BNE   SP020               NO SKIP                                      
         BH    SPBUYX              DONE                                         
*                                                                               
         CLI   KEY+11,0                                                         
         BNE   SP040               ACTIVE POINTER                               
         MVC   SAVEKEY2(17),KEY                                                 
         MVI   TRACECDE,C'X'                                                    
         BAS   RE,SPDEL            DELETE THE PASSIVE POINTERS                  
*                                                                               
         MVC   DUB(1),KEY          RESTORE ORIGINAL MEDIA IN KEY                
         NI    DUB,X'F0'                                                        
         OC    DUB(1),KEY+10                                                    
         MVC   KEY(1),DUB                                                       
         MVI   KEY+10,X'FF'                                                     
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRACECDE,C'B'                                                    
         BAS   RE,SPDEL            DELETE THE PASSIVE POINTERS                  
*                                                                               
         MVC   KEY(13),SAVEKEY2                                                 
         OI    DMINBTS,X'08'       PASS BACK THE DELETED RECORDS                
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(13),KEY                                                  
         BE    SP020                                                            
         DC    H'0'                                                             
         EJECT                                                                  
* THIS SECTION REPLACES THE ACTIVE POINTERS *                                   
*                                                                               
SP040    CLI   KEY+10,0                                                         
         BE    SP050               ACTIVE POINTER                               
         MVI   TRACECDE,C'C'                                                    
         BAS   RE,SPDEL            DELETE THE POINTER                           
         B     SP020                                                            
*                                                                               
SP050    MVC   SAVEKEY(13),KEY                                                  
         MVI   TRACECDE,C'D'                                                    
         BAS   RE,SPTRACE          *** TRACE ***                                
*                                                                               
         GOTO1 GETBUY                                                           
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         XC    TMKST,TMKST                                                      
         MVC   TMKST,4(R6)         UNPK OLD STATION                             
         BAS   RE,SETSTPCK                                                      
         BNE   SP110                                                            
         BAS   RE,DELDR            DELETE OLD REC & PTR                         
*                                                                               
         MVC   6(3,R6),NEWBSTA      SET THE NEW STATION CODE                    
         MVC   KEY+6(3),NEWBSTA                                                 
         MVI   TRACECDE,C'E'                                                    
         BAS   RE,ADDDR                                                         
         AP    BUYCHG,=P'1'                                                     
*                                                                               
         GOTO1 =V(LDCPTR),DMCB,ADBUY,A(SPBUFF)                                  
         L     R5,=A(SPBUFF)                                                    
         BAS   RE,PRTBUY                                                        
         MVI   USRSW2,C'Y'                                                      
         LA    R5,18(R5)                                                        
         CLI   0(R5),0                                                          
         BE    SP110               DONE                                         
         CLI   BUYKEY+3,X'FF'                                                   
         BE    SP060                                                            
         CLI   BDTIME,0            TEST FOR PIGGYBACK                           
         BE    SP110                NO.                                         
         LA    R5,18(R5)           A PASSIVE POINTER WAS ALREADY ADDED          
         B     SP070                                                            
*                                                                               
SP060    CLI   BDMASPRD,0          IS THERE A POL MASTER PRODUCT CODE           
         BE    SP070                NO.                                         
         CLC   3(1,R5),BDMASPRD    IS THIS THE POOL MASTER PRD CODE?            
         BNE   SP070                NO.                                         
         LA    R5,18(R5)                                                        
         CLI   0(R5),0                                                          
         BE    SP110                                                            
         CLI   BDMASPRD+1,0        ARE THERE 2 POOL MASTER PRD CODES?           
         BE    SP070                NO.                                         
         CLC   3(1,R5),BDMASPRD+1  IS THIS THE 2ND POOL MASTER PRD CODE         
         BNE   SP070                NO.                                         
         LA    R5,18(R5)                                                        
*                                                                               
SP070    CLI   0(R5),0                                                          
         BE    SP110                                                            
*                                                                               
SP080    MVC   KEY(13),0(R5)                                                    
         MVI   TRACECDE,C'F'                                                    
         BAS   RE,SPTRACE          *** TRACE ***                                
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   SP100                                                            
         CLC   =X'FF00',KEY+10                                                  
         BNE   SP090                                                            
         CLC   KEY+3(1),BDMASPRD                                                
         BE    SP100                                                            
         CLC   KEY+3(1),BDMASPRD+1                                              
         BE    SP100                                                            
*                                                                               
SP090    GOTO1 DATAMGR,DMCB,DMADD,SPTDIR,KEY,KEY                                
         TM    DM3,X'DD'          ALLOW DUP KEY ON ADD OR DELETED REC           
         BZ    SP100                                                            
         DC    H'0'                ERROR                                        
*                                                                               
SP100    LA    R5,18(R5)                                                        
         CLI   0(R5),0             IS THIS THE END OF THE SPBUFF?               
         BNE   SP080                NO.                                         
*                                                                               
SP110    MVC   KEY(13),SAVEKEY                                                  
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),SPTDIR,KEY,KEY                       
         CLC   KEY(13),SAVEKEY                                                  
         BE    SP020                                                            
         DC    H'0'                                                             
*                                                                               
SPBUYX   B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* SP400 - THIS SECTION UPDATES THE INVOICE RECORDS *                            
*                                                                               
SP400    MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
         MVC   AREC,ADBUY                                                       
         L     R6,AREC                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'           INVOICE RECORD                               
         MVC   KEY+1(1),SVAGYMD    A-M                                          
         GOTO1 HIGH                                                             
         B     SP440                                                            
*                                                                               
SP430    GOTO1 SEQ                                                              
*                                                                               
SP440    CLC   KEY(2),KEYSAVE      SAME 0B/A-M/STA                              
         BNE   SP500               GO CHECK NSID RECORDS                        
         MVI   TRACECDE,C'G'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
         GOTO1 GET                                                              
         XC    TMKST,TMKST                                                      
         MVC   TMKST+2(3),KEY+2    SET UP OLD STATION TO UNPK                   
         BAS   RE,SETSTPCK         GET NEW STATION                              
         BNE   SP430                                                            
         BAS   RE,DELDR            DELETE OLD PTR & RECORD                      
*                                                                               
         MVC   2(3,R6),NEWBSTA     UPDATE CALL LETTERS                          
         MVC   KEY+2(3),NEWBSTA                                                 
         MVI   TRACECDE,C'H'                                                    
         BAS   RE,ADDDR                                                         
         AP    INVCHG,=P'1'                                                     
*                                                                               
         GOTO1 CLUNPK,DMCB,KEY+5,P+10                                           
         GOTO1 DATCON,(R1),(2,KEY+7),(5,P+20)                                   
         GOTO1 HEXOUT,(R1),KEY+9,P+38,1,=C'MIX'                                 
         GOTO1 REPORT                                                           
         B     SP430                                                            
         EJECT                                                                  
*                                                                               
* SP500 - THIS SECTION UPDATES NSID RECORDS *                                   
*                                                                               
SP500    MVI   FORCEHED,C'Y'                                                    
         MVC   AREC,ADBUY                                                       
         L     R6,AREC                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,SIRKTYPQ        NEW SID RECORD                               
         MVC   KEY+1(1),SVAGYMD    A-M                                          
         GOTO1 HIGH                                                             
         B     SP530                                                            
*                                                                               
SP520    GOTO1 SEQ                                                              
*                                                                               
SP530    CLC   KEY(2),KEYSAVE      TEST 0C/A-M                                  
         BNE   SP600                NO. CHECK OLD SID RECORDS                   
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
         GOTO1 GET                                                              
         XC    TMKST,TMKST                                                      
         MVC   TMKST,KEY+2         UNPK OLD STATION                             
         BAS   RE,SETSTPCK         GET NEW STATION                              
         BNE   SP520                                                            
         BAS   RE,DELDR            DELETE DIRECTORY & RECORD                    
*                                                                               
         MVC   6(3,6),NEWBSTA      UPDATE CALL LETTERS IN RECORD                
         MVC   KEY+6(3),NEWBSTA    IN KEY AS WELL                               
         MVI   TRACECDE,C'J'                                                    
         BAS   RE,ADDDR                                                         
*                                                                               
         AP    NSICHG,=P'1'                                                     
         B     SP520                                                            
*                                                                               
SP600    DS    0H                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* SP600 - THIS SECTION UPDATES SID RECORDS *                                    
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   AREC,ADBUY                                                       
         L     R6,AREC                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D59'     SID RECORD                                   
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         GOTO1 HIGH                                                             
         B     SP620                                                            
*                                                                               
SP610    GOTO1 SEQ                                                              
*                                                                               
SP620    CLC   KEY(3),KEYSAVE      SAME 0D59/A-M                                
         BNE   SP690                YES, GO ON                                  
         MVI   TRACECDE,C'K'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
         GOTO1 GET                                                              
         XC    TMKST,TMKST                                                      
         MVC   TMKST,3(R6)         UNPK OLD STATION                             
         BAS   RE,SETSTPCK         GET NEW STATION                              
         BNE   SP610                                                            
         BAS   RE,DELDR                                                         
*                                                                               
         MVC   5(3,R6),NEWBSTA      UPDATE CALL LETTERS                         
         MVC   KEY+5(3),NEWBSTA                                                 
         MVI   TRACECDE,C'L'                                                    
         BAS   RE,ADDDR                                                         
*                                                                               
         AP    SIDCHG,=P'1'                                                     
         B     SP610                                                            
*                                                                               
SP690    DS    0H                                                               
         EJECT                                                                  
*                                                                               
* DB100 - THIS SECTION UPDATES DOUBLE BOOKING RECORDS *                         
*                                                                               
DB100    MVC   AREC,ADBUY                                                       
         L     R6,AREC                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D7B'     DOUBLE BOOKING RECORD                        
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         GOTO1 HIGH                                                             
         B     DB120                                                            
*                                                                               
DB110    GOTO1 SEQ                                                              
*                                                                               
DB120    CLC   KEY(3),KEYSAVE      SAME 0D7B/A-M                                
         BNE   DB190                NO. END OF THIS REC TYPE/AM                 
         MVI   TRACECDE,C'M'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
         GOTO1 GET                                                              
         XC    TMKST,TMKST                                                      
         MVC   TMKST,5(R6)         UNPK OLD STATION                             
         BAS   RE,SETSTPCK                                                      
         BNE   DB110                                                            
         BAS   RE,DELDR                                                         
*                                                                               
         MVC   5(3,R6),NEWBSTA     UPDATE CALL LETTERS                          
         MVC   KEY+5(3),NEWBSTA                                                 
         MVI   TRACECDE,C'N'                                                    
         BAS   RE,ADDDR                                                         
*                                                                               
         AP    DBLCHG,=P'1'                                                     
         B     DB110                                                            
*                                                                               
DB190    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* SP700 - THIS SECTION UPDATES TRAFFIC INSTRUCTION RECAP RECORDS *              
*                                                                               
         L     RE,UTL                                                           
         CLI   FCUPTRF,C'Y'        TEST TRAFFIC SYSTEM OP                       
         BNE   *+10                                                             
         MVC   4(1,RE),RCUTLTRF    SET TRAFFIC SYSTEM NUMBER                    
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   AREC,ADBUY                                                       
         L     R6,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A24'     INST RECAP RECORD                            
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         GOTO1 HIGH                                                             
         B     SP720                                                            
*                                                                               
SP710    GOTO1 SEQ                                                              
*                                                                               
SP720    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SP800               NO,THEN TRY NEXT                             
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
         GOTO1 GET                                                              
         XC    TMKST,TMKST                                                      
         MVC   TMKST,8(R6)         UNPK OLD STATION                             
         BAS   RE,SETSTPCK         GET NEW STATION                              
         BNE   SP710                                                            
         BAS   RE,DELTR                                                         
*                                                                               
         MVC   8(3,R6),NEWBSTA      UPDATE CALL LETTERS                         
         MVC   KEY+8(3),NEWBSTA                                                 
         BAS   RE,ADDTR                                                         
         AP    INRCHG,=P'1'                                                     
         B     SP710                                                            
*                                                                               
SP800    DS    0H                                                               
         EJECT                                                                  
*                                                                               
* SP800 - THIS SECTION UPDATES TRAFFIC SHIPPING RECAP RECORDS *                 
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   AREC,ADBUY                                                       
         L     R6,AREC                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A25'     SHIP RECAP RECORD                            
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         GOTO1 HIGH                                                             
         B     SP820                                                            
*                                                                               
SP810    GOTO1 SEQ                                                              
*                                                                               
SP820    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SP900               NO,THEN TRY NEXT                             
         MVI   TRACECDE,C'P'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
         GOTO1 GET                                                              
         XC    TMKST,TMKST                                                      
         MVC   TMKST,7(R6)         UNPK OLD STATION                             
         BAS   RE,SETSTPCK         GET NEW STATION                              
         BNE   SP810                                                            
         BAS   RE,DELTR                                                         
*                                                                               
         MVC   7(3,R6),NEWBSTA      UPDATE CALL LETTERS                         
         MVC   KEY+7(3),NEWBSTA                                                 
         BAS   RE,ADDTR                                                         
*                                                                               
         AP    SHPCHG,=P'1'                                                     
         B     SP810                                                            
*                                                                               
SP900    DS   0H                                                                
         EJECT                                                                  
*                                                                               
* SP900 - THIS SECTION UPDATES TRAFFIC BUY ACTIVITY RECORDS *                   
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   AREC,ADBUY                                                       
         L     R6,AREC                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2E'     BUY ACT/ESTM RECORD                          
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         GOTO1 HIGH                                                             
         B     SP920                                                            
*                                                                               
SP910    GOTO1 SEQ                                                              
*                                                                               
SP920    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SPA00               NO,THEN TRY NEXT                             
         MVI   TRACECDE,C'Q'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
         GOTO1 GET                                                              
         XC    TMKST,TMKST                                                      
         MVC   TMKST,7(R6)         UNPK OLD STATION                             
         BAS   RE,SETSTPCK         GET NEW STATION                              
         BNE   SP910                                                            
         BAS   RE,DELTR                                                         
*                                                                               
         MVC   7(3,R6),NEWBSTA      UPDATE CALL LETTERS                         
         MVC   KEY+7(3),NEWBSTA                                                 
         BAS   RE,ADDTR                                                         
*                                                                               
         AP    BUYACHG,=P'1'                                                    
         B     SP910                                                            
*                                                                               
SPA00    DS   0H                                                                
         EJECT                                                                  
*                                                                               
* SPA00 - THIS SECTION UPDATES TRAFFIC BUY RECORDS *                            
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   AREC,ADBUY                                                       
         L     R6,AREC                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A32'     TRAFFIC BUY RECORD                           
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         GOTO1 HIGH                                                             
         B     SPA20                                                            
*                                                                               
SPA10    GOTO1 SEQ                                                              
*                                                                               
SPA20    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SPB00               NO,THEN TRY NEXT                             
         MVI   TRACECDE,C'R'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
         GOTO1 GET                                                              
         XC    TMKST,TMKST                                                      
         MVC   TMKST,8(R6)         UNPK OLD STATION                             
         BAS   RE,SETSTPCK         GET NEW STATION                              
         BNE   SPA10                                                            
         BAS   RE,DELTR                                                         
*                                                                               
         MVC   8(3,R6),NEWBSTA      UPDATE CALL LETTERS                         
         MVC   KEY+8(3),NEWBSTA                                                 
         BAS   RE,ADDTR                                                         
*                                                                               
         AP    TBUYCHG,=P'1'                                                    
         B     SPA10                                                            
*                                                                               
SPB00    DS   0H                                                                
         EJECT                                                                  
*                                                                               
* SPB00 - THIS SECTION UPDATES TRAFFIC STATION LABEL LIST RECORDS *             
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   AREC,ADBUY                                                       
         L     R6,AREC                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2F'     LABEL LIST RECORD                            
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         GOTO1 HIGH                                                             
         B     SPB20                                                            
*                                                                               
SPB10    GOTO1 SEQ                                                              
*                                                                               
SPB20    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SPC00               NO,THEN TRY NEXT                             
         MVI   TRACECDE,C'S'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
         GOTO1 GET                                                              
*                                                                               
         L     R6,ADBUY            POINT TO RECORD                              
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   SPB10                                                            
         MVI   CHGEL,C'N'          SET OFF ELEM CHANGED                         
         B     *+12                                                             
*                                                                               
SPB30    BAS   RE,NEXTEL                                                        
         BNE   SPB40                                                            
         XC    TMKST,TMKST                                                      
         MVC   TMKST,2(R6)         UNPK OLD STATION                             
         BAS   RE,SETSTPCK         GET NEW STATION                              
         BNE   SPB30                                                            
         MVC   2(3,R6),NEWBSTA      INSERT NEW STATION                          
         MVI   CHGEL,C'Y'          CHANGED ELEMENT                              
         B     SPB30                                                            
*                                                                               
SPB40    CLI   CHGEL,C'Y'          TEST ELEMENTS CHANGED                        
         BNE   SPB10               NO, THEN NEXT RECORD                         
         BAS   RE,PUTTR                                                         
*                                                                               
         AP    LABLCHG,=P'1'                                                    
         B     SPB10                                                            
*                                                                               
SPC00    DS    0H                                                               
         EJECT                                                                  
* SPC00 - THIS SECTION UPDATES TRAFFIC MARKET STATION LIST RECORDS *            
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   AREC,ADBUY                                                       
         L     R6,AREC                                                          
         MVI   CHGEL,0                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A31'     STATION LIST RECORD                          
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         GOTO1 HIGH                                                             
         B     SPC20                                                            
*                                                                               
SPC10    GOTO1 SEQ                                                              
*                                                                               
SPC20    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SPD00               NO,THEN TRY NEXT                             
         GOTO1 GET                                                              
*                                                                               
         L     R6,ADBUY            POINT TO RECORD                              
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   SPC10                                                            
         MVI   CHGEL,C'N'                                                       
         B     *+12                                                             
*                                                                               
SPC30    BAS   RE,NEXTEL                                                        
         BNE   SPC40                                                            
         XC    TMKST,TMKST                                                      
         MVC   TMKST,2(R6)         UNPK OLD STATION                             
         BAS   RE,SETSTPCK         GET NEW STATION                              
         BNE   SPC30                                                            
         MVC   2(3,R6),NEWBSTA      INSERT NEW STATION                          
         MVI   CHGEL,C'Y'                                                       
         B     SPC30                                                            
*                                                                               
SPC40    CLI   CHGEL,C'Y'                                                       
         BNE   SPC10                                                            
         BAS   RE,PUTTR                                                         
         AP    STALCHG,=P'1'                                                    
         B     SPC10                                                            
*                                                                               
SPD00    DS    0H                                                               
         EJECT                                                                  
*                                                                               
* SPD00 - THIS SECTION UPDATES TRAFFIC PATTERN RECORDS *                        
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   AREC,ADBUY                                                       
         L     R6,AREC                                                          
         MVI   CHGEL,0                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A22'     PATTERN RECORD                               
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         GOTO1 HIGH                                                             
         B     SPD20                                                            
*                                                                               
SPD10    GOTO1 SEQ                                                              
*                                                                               
SPD20    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SPE00               NO,THEN TRY NEXT                             
         GOTO1 GET                                                              
*                                                                               
         L     R6,ADBUY                                                         
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   CHGEL,C'N'                                                       
*                                                                               
         CLI   2(R6),C'S'          TEST STATION LIST                            
         BNE   SPD10                                                            
         ZIC   R0,1(R6)            #STATIONS IN LIST                            
         SH    R0,=H'3'                                                         
         SRDL  R0,32                                                            
         D     R0,=F'5'                                                         
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                DIE IF ISN'T EVEN                            
*                                                                               
         LR    R0,R1                                                            
         LA    R1,3(R6)                                                         
SPD40    CLC   0(5,R1),QSTA        TEST OLD STATION                             
         BE    SPD46                                                            
         OC    0(2,R1),0(R1)                                                    
         BNZ   SPD46                                                            
         MVC   DUB(3),2(R1)                                                     
         NI    DUB+2,X'80'                                                      
         XC    TMKST,TMKST                                                      
         MVC   TMKST,2(R1)         UNPK OLD STATION                             
         BAS   RE,SETSTPCK         GET NEW STATION                              
         BNE   SPD46                                                            
         MVC   2(3,R1),NEWBSTA      INSERT NEW STATION                          
         MVI   CHGEL,C'Y'                                                       
*                                                                               
SPD46    LA    R1,5(R1)                                                         
         BCT   R0,SPD40                                                         
*                                                                               
         CLI   CHGEL,C'Y'          TEST CHANGED ELEMENT                         
         BNE   SPD10                                                            
         BAS   RE,PUTTR                                                         
*                                                                               
         AP    PATCHG,=P'1'                                                     
         B     SPD10                                                            
*                                                                               
SPE00    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         L     RE,UTL                                                           
         MVC   4(1,RE),SPOTSE      RESTORE SPOT SYSTEM NUMBER                   
         EJECT                                                                  
*                                                                               
* SPDEL - THIS SECTION DELETES THE PASSIVE POINTERS *                           
*                                                                               
SPDEL    NTR1                                                                   
         MVI   KEY+13,C'S'         PASSIVE POINTER - DELETE RECORD              
         BAS   RE,SPTRACE          *** TRACE ***                                
*                                                                               
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   SPDEL20                                                          
         GOTO1 DATAMGR,DMCB,DMWRT,SPTDIR,KEY,KEY                                
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SPDEL20  DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* THIS ROUTINE PRINTS THE BUY LINE NUMBERS FOR EACH PRODUCT *                   
*                                                                               
PRTBUY   NTR1                                                                   
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         LA    R2,P+2                                                           
         USING PLINED,R2                                                        
*                                                                               
         MVC   BYTE,BUYKAM                                                      
         NI    BYTE,X'0F'                                                       
         MVI   PMED,C'T'                                                        
         CLI   BYTE,X'01'                                                       
         BE    PB10                                                             
         MVI   PMED,C'R'                                                        
         CLI   BYTE,X'02'                                                       
         BE    PB10                                                             
         MVI   PMED,C'N'                                                        
         CLI   BYTE,X'03'                                                       
         BE    PB10                                                             
         MVI   PMED,C'C'                                                        
         CLI   BYTE,X'08'                                                       
         BE    PB10                                                             
         MVI   PMED,C' '                                                        
*                                                                               
PB10     MVC   PAGY(2),BUYALPHA                                                 
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING STAPACKD,R4                                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,QAGY                                                     
         MVC   STAPMED,QMED                                                     
         MVC   STAPMKST,BUYMSTA                                                 
         MVI   STAPCTRY,C'C'       WE'RE ONLY DOING CANADA                      
         MVC   STAPACOM,ACOMFACS                                                
         GOTO1 VSTAPACK,DMCB,WORK                                               
         MVC   PMKT,STAPQMKT                                                    
         MVC   PSTA,STAPQSTA                                                    
         DROP  R4                                                               
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PEST,WORK+7                                                      
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PLIN,WORK+7                                                      
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY+14,PLDA,4,=C'N'                                  
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* THIS CODE READS ALL WUNDERMAN RECORDS AND FIXES THE STATION                   
* CALL LETTERS AS NECESSARY.                                                    
* THE RECORDS AFFECTED - REGULAR CONTRACT RECORDS       X'0E14'                 
*                        GRADE ASSIGNMENT RECORDS       X'0E16'                 
*                        LOG ENTRY + PASSIVE POINTERS   X'0E18' X'0E98'         
*                        INVOICE + PASSIVE POINTERS     X'0E19' X'0E99'         
*                        ORDER + PASSIVE POINTERS       X'0E1A' X'0E9A'         
*                                                                               
*                                                                               
WWSTAFIX NTR1                                                                   
         MVC   PRNTREC,=C'REGULAR CONTRACT'                                     
         XC    KEY,KEY             REGULAR CONTRACT RECORDS                     
         LA    R6,KEY                                                           
         USING REGRECD,R6                                                       
*                                                                               
         MVI   REGKTYPE,REGKTYPQ                                                
         MVI   REGKSTYP,REGKSTPQ                                                
         MVC   REGKAM,BAGYMD                                                    
         GOTO1 HIGH                                                             
         B     WW20                                                             
*                                                                               
WW10     GOTO1 SEQ                                                              
*                                                                               
WW20     CLC   KEY(3),KEYSAVE      COMPLETED REG CONTRACTS                      
         BNE   WW100                                                            
*                                  UNPACK OLD WAY                               
         L     R6,=A(SPBUFF)       GET CONTRACT RECORD                          
         STCM  R6,15,AREC                                                       
         GOTO1 GET                                                              
         MVC   TMKST,REGKMKT                                                    
         BAS   RE,SETSTPCK         GET                                          
         BNE   WW10                                                             
*                                                                               
         BAS   RE,DELDR            MARK DIRECTORY & RECORD DELETED              
         MVC   REGKSTA,NEWBSTA                                                  
         MVC   KEY+9(3),NEWBSTA                                                 
         AP    REGCHAN,=P'1'                                                    
         BAS   RE,ADDDR                                                         
*                                                                               
WW60     MVI   TRACECDE,C'T'                                                    
         BAS   RE,SPTRACE                                                       
         BAS   RE,PRNT             PRINT A LINE OF REC INFO                     
         NI    DMINBTS,X'F7'                                                    
         B     WW10                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
WW100    DS    0H                                                               
         XC    KEY,KEY             GRADE ASSIGNMENT RECORDS                     
         LA    R6,KEY                                                           
         USING GASRECD,R6                                                       
         MVC   PRNTREC,=C'GRADE ASSIGNMENT'                                     
*                                                                               
         MVI   GASKTYPE,GASKTYPQ                                                
         MVI   GASKSTYP,GASKSTPQ                                                
         MVC   GASKAM,BAGYMD                                                    
         GOTO1 HIGH                                                             
         B     WW120                                                            
*                                                                               
WW110    GOTO1 SEQ                                                              
*                                                                               
WW120    CLC   KEY(3),KEYSAVE      COMPLETED GRADE ASSIGNMENTS                  
         BNE   WW200                                                            
         L     R6,=A(SPBUFF)       GET GRADE ASSIGNMENT RECORD                  
         STCM  R6,15,AREC                                                       
         GOTO1 GET                                                              
         XC    TMKST,TMKST                                                      
         MVC   TMKST+2(3),GASKSTA                                               
         BAS   RE,SETSTPCK         GET                                          
         BNE   WW110                                                            
*                                                                               
         BAS   RE,DELDR            MARK DIRECTORY & RECORD DELETED              
         MVC   GASKSTA,NEWBSTA                                                  
         MVC   KEY+6(3),NEWBSTA                                                 
         AP    GASCHAN,=P'1'                                                    
         BAS   RE,ADDDR                                                         
*                                                                               
WW140    MVI   TRACECDE,C'U'                                                    
         BAS   RE,SPTRACE                                                       
         BAS   RE,PRNT             PRINT A LINE OF REC INFO                     
         NI    DMINBTS,X'F7'                                                    
         B     WW110                                                            
         DROP  R6                                                               
*                                                                               
WW200    MVI   RECSTYPE,X'98'      LOG RECORDS                                  
         MVI   TRREC,C'8'          TO TRACK TRACE                               
         MVC   PRNTREC,=C'LOG ENTRY RECORD'                                     
         BAS   RE,SPL00                                                         
*                                                                               
         MVI   RECSTYPE,X'99'      INVOICE RECORDS                              
         MVI   TRREC,C'9'          TO TRACK TRACE                               
         MVC   PRNTREC,=C'INVOICE   RECORD'                                     
         BAS   RE,SPL00                                                         
*                                                                               
         MVI   RECSTYPE,X'9A'      ORDER RECORDS                                
         MVI   TRREC,C'A'          TO TRACK TRACE                               
         MVC   PRNTREC,=C'ORDER ENTRY REC '                                     
         BAS   RE,SPL00                                                         
*                                                                               
WWX      BAS   RE,SUMMARY                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        MARK DIRECTORY & RECORD DELETED                                        
*                                                                               
DELDR    NTR1                                                                   
         NI    DMINBTS,X'F7'                                                    
         OI    KEY+13,X'80'        MARK FOR DELETION                            
         OI    15(R6),X'80'                                                     
         CLI   RCWRITE,C'Y'                                                     
         BNE   DDX                                                              
         GOTO1 WRITE                                                            
         GOTO1 PUT                                                              
*                                                                               
DDX      B     EXIT                                                             
*                                                                               
*        DELETE A RECORD FROM TRFFILE                                           
*                                                                               
DELTR    NTR1                                                                   
         CLI   RCWRITE,C'Y'                                                     
         BNE   EXIT                                                             
         GOTO1 DATAMGR,DMCB,=C'DMDEL',=C'TRFFILE',KEY,AREC                      
         TM    DM3,X'FD'                                                        
         BZ    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
*        PUT A RECORD TO TRFFILE                                                
*                                                                               
PUTTR    NTR1                                                                   
         CLI   TRACECDE,0                                                       
         BNE   *+8                                                              
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
         CLI   RCWRITE,C'Y'                                                     
         BNE   EXIT                                                             
         GOTO1 DATAMGR,DMCB,PUTREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         BE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
*        ADD A NEW RECORD TO SPTFILE                                            
*                                                                               
ADDDR    NTR1                                                                   
         NI    KEY+13,X'7F'        TURN OFF DELETED FLAG (DIR)                  
         NI    15(R6),X'7F'        TURN OFF DELETED FLAG (RECORD)               
         CLI   TRACECDE,0                                                       
         BNE   *+8                                                              
         MVI   TRACECDE,C'Y'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
ADDD10   CLI   RCWRITE,C'Y'                                                     
         BNE   EXIT                                                             
         GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,ADBUY,DMWORK                  
         CLI   DM3,0                                                            
         BE    EXIT                                                             
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
*        ADD A NEW RECORD TO TRFFILE                                            
*                                                                               
ADDTR    NTR1                                                                   
         NI    KEY+13,X'7F'        TURN OFF DELETED FLAG (DIR)                  
         NI    15(R6),X'7F'        TURN OFF DELETED FLAG (RECORD)               
         MVI   TRACECDE,C'Z'                                                    
         CLI   RCWRITE,C'Y'                                                     
         BNE   EXIT                                                             
         GOTO1 DATAMGR,DMCB,ADDREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         BE    EXIT                                                             
         DC    H'0'                DATAMGR ERROR                                
         EJECT                                                                  
*                                                                               
* SUBROUTINE TO CHANGE SPLIT (MINIO) RECORDS' STATION                           
* CALL LETTERS. RECSTYPE = SUBTYPE OF RECORD                                    
*                                                                               
SPL00    NTR1                                                                   
         XC    KEY,KEY             READ PASSIVE POINTER                         
         MVI   KEY,X'0E'                                                        
         MVC   KEY+1(1),RECSTYPE                                                
         MVC   KEY+2(1),BAGYMD                                                  
         GOTO1 HIGH                                                             
         B     SPL20                                                            
*                                                                               
SPL10    GOTO1 SEQ                                                              
*                                                                               
SPL20    MVC   SAVEKEY(13),KEY                                                  
         CLC   KEY(3),KEYSAVE      ALL CLIENTS                                  
         BNE   SPLX                COMPLETED SET OF RECORDS                     
*                                                                               
SPL30    L     R6,=A(SPBUFF)                                                    
         STCM  R6,15,AREC                                                       
         GOTO1 GET                                                              
*                                                                               
         NI    DMINBTS,X'F7'                                                    
         MVI   ELCODE,X'80'        FIND PASSIVE ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    SPL50                                                            
         MVC   TRACECDE,TRREC                                                   
         BAS   RE,PASSERR                                                       
         B     SPL70                                                            
*                                                                               
SPL50    XC    TMKST,TMKST                                                      
         MVC   TMKST+2(3),9(R6)    UNPK OLD STATION                             
         BAS   RE,SETSTPCK         GET                                          
         BNE   SPL10                                                            
         MVC   9(3,R6),NEWBSTA     MOVE NEW STATION TO X'80' ELEM               
         CLI   RCWRITE,C'Y'                                                     
         BNE   SPL60                                                            
         GOTO1 PUT                                                              
         OI    KEY+13,X'80'        MARK FOR DELETION                            
         GOTO1 WRITE                                                            
*                                                                               
SPL60    MVC   KEY+7(3),NEWBSTA                                                 
         CLI   RCWRITE,C'Y'                                                     
         BNE   SPL70                                                            
         NI    KEY+13,X'7F'                                                     
         GOTO1 ADDIR                                                            
*                                                                               
SPL70    MVC   TRACECDE,TRREC                                                   
         CLI   TRACECDE,C'8'                                                    
         BNE   SPL80                                                            
         AP    LOGCHAN,=P'1'                                                    
         B     SPL100                                                           
*                                                                               
SPL80    CLI   TRACECDE,C'9'                                                    
         BNE   SPL90                                                            
         AP    INVCHAN,=P'1'                                                    
         B     SPL100                                                           
*                                                                               
SPL90    AP    ORDCHAN,=P'1'                                                    
*                                                                               
SPL100   BAS   RE,SPTRACE                                                       
         NI    DMINBTS,X'F7'                                                    
         B     SPL10                                                            
*                                                                               
SPLX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* NO PASSIVE POINTER EXITS FOR THIS RECORD                                      
*                                                                               
PASSERR  NTR1                                                                   
         XC    P,P                                                              
         GOTO1 REPORT                                                           
         MVC   P+10(30),=CL30' *  ERROR * NO PASSIVE POINTER'                   
         GOTO1 REPORT                                                           
         MVC   P(1),TRACECDE                                                    
         GOTO1 HEXOUT,DMCB,KEY,P+10,20,=C'TOG',0                                
         GOTO1 REPORT                                                           
*                                                                               
PASSX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
SUMMARY  NTR1                                                                   
         XC    P,P                                                              
         PRINT GEN                                                              
         GOTO1 REPORT                                                           
         PRINT NOGEN                                                            
         MVC   P+10(16),=C'WUNDERMAN TOTALS'                                    
         GOTO1 REPORT                                                           
         MVC   P+10(16),=C'--------- ------'                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         LA    R4,COUNTS                                                        
         LA    R5,NCOUNTS                                                       
*                                                                               
SUM10    OI    3(R4),X'0F'                                                      
         UNPK  P(7),0(4,R4)                                                     
         MVC   P+9(16),4(R4)                                                    
         GOTO1 REPORT                                                           
         LA    R4,L'COUNTS(R4)                                                  
         BCT   R5,SUM10                                                         
*                                                                               
SUMX     XC    P,P                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
PRNT     NTR1                                                                   
         XC    P,P                                                              
         OC    P,SPACES                                                         
         MVC   MYCLT,KEY+3                                                      
         MVC   MYPRD,KEY+5                                                      
         GOTO1 CLUNPK,DMCB,KEY+3,P+5                                            
         MVC   P+80(16),PRNTREC    RECORD TYPE                                  
         CLI   TRACECDE,C'6'                                                    
         BE    PRNT05                                                           
         ZIC   R1,KEY+6            CAMPAIGN                                     
         EDIT  (R1),(3,P+64),ZERO=BLANK                                         
*                                                                               
PRNT05   MVC   KEY2,KEY            SAVE KEY                                     
         MVC   MYAREC,AREC         SAVE ADDRESS OF REC                          
         XC    KEY,KEY                                                          
         USING CLTHDRD,R5                                                       
         LA    R5,KEY                                                           
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,MYCLT                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,=A(SPBUFF+2000)                                               
         STCM  R5,15,AREC                                                       
         GOTO1 GET                                                              
         CLI   DM3,0                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P+10(L'CNAME),CNAME                                              
         LA    R5,60(R5)           GET PRD CODE MNEMONIC FROM CLIST             
*                                                                               
PRNT10   CLI   3(R5),0                                                          
         BNE   PRNT15                                                           
         MVC   P+47(18),=C'PRD CODE NOT FOUND'                                  
         B     PRNT30                                                           
*                                                                               
PRNT15   CLC   MYPRD,3(R5)                                                      
         BE    PRNT20                                                           
         LA    R5,4(R5)            BUMP TO NEXT PRD CODE                        
         B     PRNT10                                                           
*                                                                               
PRNT20   MVC   P+47(3),0(R5)                                                    
*                                                                               
PRNT30   GOTO1 REPORT                                                           
*                                                                               
PRNTX    MVC   KEY,KEY2            RESTORE KEY                                  
         MVC   AREC,MYAREC                                                      
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
PRTOUT   DS    0H                                                               
         LA    R4,RECTAB                                                        
         LA    R5,NRECTAB                                                       
*                                                                               
PO10     OI    3(R4),X'0F'                                                      
         UNPK  P(7),0(4,R4)                                                     
         MVC   P+9(16),4(R4)                                                    
         GOTO1 REPORT                                                           
         LA    R4,L'RECTAB(R4)                                                  
         BCT   R5,PO10                                                          
*                                                                               
         XC    P,P                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
SETSTPCK NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING STAPACKD,R4                                                      
         GOTO1 MSUNPK,DMCB,TMKST,STAPQMKT,STAPQSTA                              
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,QAGY                                                     
         MVC   STAPMED,QMED                                                     
         MVI   STAPCTRY,C'C'       WE'RE ONLY DOING CANADA                      
         MVC   STAPACOM,ACOMFACS                                                
         GOTO1 VSTAPACK,DMCB,WORK                                               
         CLI   STAPERR,0                                                        
         BNE   SETSERR                                                          
         MVC   NEWBMKST,STAPMKST                                                
         B     YES                                                              
*                                                                               
SETSERR  CLC   LASTMKST,STAPMKST                                                
         BE    SETSERR1                                                         
         MVC   LASTMKST,STAPMKST                                                
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         GOTO1 HEXOUT,DMCB,KEY,P,13,=C'TOG'                                     
         MVC   P+20(9),STAPQMKT                                                 
         MVC   P+30(22),=CL22'NOT FOUND IN NEW FILE'                            
         GOTO1 REPORT                                                           
         MVC   P,SAVEPRT                                                        
*                                                                               
SETSERR1 AP    NFOUND,=P'1'                                                     
         B     NO                                                               
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
* THIS SECTION PRINTS A TRACE RECORD OF THE KEY *                               
*                                                                               
SPTRACE  NTR1                                                                   
         CLI   QOPT5,C'Y'                                                       
         BNE   EXIT                                                             
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVC   P(1),TRACECDE                                                    
         GOTO1 HEXOUT,DMCB,KEY,P+10,18,=C'MIX',0                                
         CLI   TRACECDE,C'8'                                                    
         BNE   SPTRACE2                                                         
         MVC   P(7),=C'80 ELEM'                                                 
         GOTO1 HEXOUT,DMCB,(R6),P+10,15,=C'TOG',0                               
         GOTO1 REPORT                                                           
*                                                                               
SPTRACE2 DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         MVI   TRACECDE,0                                                       
         B     EXIT                                                             
         EJECT                                                                  
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
COUNTS   DS    0CL20                                                            
REGCHAN  DC    PL4'0',CL16'REG CONTACTS CHG'                                    
GASCHAN  DC    PL4'0',CL16'GRADE ASSIGN CHG'                                    
LOGCHAN  DC    PL4'0',CL16'LOG ENTRIES CHG'                                     
INVCHAN  DC    PL4'0',CL16'INVOICES CHG'                                        
ORDCHAN  DC    PL4'0',CL16'ORD ENTRIES CHG'                                     
DUPLREC  DC    PL4'0',CL16'DUP RECORDS'                                         
NCOUNTS  EQU   ((*-COUNTS)/L'COUNTS)                                            
*                                                                               
RECTAB   DS    0CL20                                                            
BUYCHG   DC    PL4'0',CL16'BUY '                                                
INVCHG   DC    PL4'0',CL16'INVOICE'                                             
NSICHG   DC    PL4'0',CL16'NSID '                                               
SIDCHG   DC    PL4'0',CL16'SID '                                                
DBLCHG   DC    PL4'0',CL16'DOUBLE BOOKING'                                      
PATCHG   DC    PL4'0',CL16'PATTERN '                                            
INRCHG   DC    PL4'0',CL16'INST RECAP '                                         
SHPCHG   DC    PL4'0',CL16'SHIP RECAP '                                         
STADCHG  DC    PL4'0',CL16'STAT ADDR '                                          
BUYACHG  DC    PL4'0',CL16'BUY ACT/ESTM'                                        
LABLCHG  DC    PL4'0',CL16'LABEL LIST '                                         
STALCHG  DC    PL4'0',CL16'STATION LIST '                                       
TBUYCHG  DC    PL4'0',CL16'TRAFFIC BUY '                                        
TSTACHG  DC    PL4'0',CL16'STAT ADDRESS'                                        
NFOUND   DC    PL4'0',CL16'STAT NOT FOUND'                                      
NRECTAB  EQU   ((*-RECTAB)/L'RECTAB)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* DATA                                                                          
*                                                                               
SPSCR7   DC    5F'0'               SAVE AREA FOR R7                             
MYAREC   DS    A                                                                
*                                                                               
         DS    0H                                                               
TMKST    DS    0CL5                                                             
TMKT     DS    H                   PACKED MKT/STA                               
TSTA     DS    XL3                                                              
*                                                                               
ELCODE   DS    CL1                                                              
CHGEL    DS    CL1                                                              
M        DS    CL1                                                              
*                                                                               
SAVEKEY  DC    XL20'00'                                                         
SAVEKEY2 DC    XL20'00'                                                         
*                                                                               
NEWBMKST DS    0CL5                                                             
NEWBMKT  DS    H                   PACKED MKT/STA                               
NEWBSTA  DS    XL3                                                              
*                                                                               
RECSTYPE DS    CL1                 WW RECORD SUB TYPE                           
LASTMKST DS    XL5                                                              
*                                                                               
SAVEPRT  DS    CL132                                                            
*                                                                               
TRACECDE DS    CL1                 TRACE CODE                                   
TRREC    DS    CL1                 CODE TO TRACK TRACE                          
MYCLT    DS    XL2                                                              
MYPRD    DS    XL1                                                              
PRNTREC  DS    CL16                                                             
*                                                                               
         DS    0D                                                               
SPBUFF   DS    4000C                                                            
         EJECT                                                                  
PLINED   DSECT                                                                  
PMED     DS    CL1                                                              
         DS    CL2                                                              
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL2                                                              
PLDA     DS    CL8                                                              
         DS    CL1                                                              
PLELDSP  DS    CL10                                                             
         EJECT                                                                  
         PRINT OFF                                                              
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
STAHDRD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENWBS                                                       
         EJECT                                                                  
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046SPREPMS02 05/01/02'                                      
         END                                                                    
