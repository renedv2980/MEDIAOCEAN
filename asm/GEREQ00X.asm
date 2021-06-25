*          DATA SET GEREQ00X   AT LEVEL 034 AS OF 08/29/00                      
*PHASE TFD800A                                                                  
         TITLE 'TFD800 - GENCON REQUEST DISPLAY/DELETE'                         
TFD800   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         NMOD1 0,TFD800,RR=R8                                                   
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING TFD800+4096,RA                                                   
         ST    R8,RELO                                                          
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R3,ATWA                                                          
         USING CONHEADH-64,R3                                                   
         OI    CONHEADH+6,X'80'    SET TO XMT MESSAGE FIELD                     
         MVC   CONHEAD,SPACES      CLEAR THE MESSAGE AREA                       
*                                                                               
         L     R1,EFHACT           R2=A(CONACTH)                                
         OI    1(R1),X'01'         FORCE ACTION FIELD TO MODIFIED               
         OI    6(R1),X'80'         AND THEREFORE MUST TRANSMIT                  
*                                                                               
         B     QDIS                                                             
*                                                                               
RELO     DS    A                                                                
*                                                                               
EQXIT    CR    RE,RE                                                            
         B     *+6                                                              
NEQXIT   LTR   RE,RE                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
REQERR   GOTO1 ERREX                                                            
         EJECT                                                                  
* TEST FOR ANY CHANGES IN HEADLINE FIELDS *                                     
         SPACE 1                                                                
QDIS     DS    0H                                                               
         L     R6,EFHTAG                                                        
         USING REQSCRD,R6                                                       
         CLI   TWASCR,X'FF'        TEST REQUEST SCREEN LOADED                   
         BE    QDIS2                                                            
         ST    R6,DMCB                                                          
         GOTO1 CALLOV,DMCB,,X'D90FD8FD'                                         
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TWASCR,X'FF'        INDICATE REQUEST SCREEN LOADED               
*&&UK                                                                           
         LA    R2,CONHEADH                                                      
         XR    R0,R0                                                            
TRANS    OI    6(R2),X'80'         TURN ON TRANSMIT BITS                        
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CR    R2,R6                                                            
         BL    TRANS                                                            
*&&                                                                             
         EJECT                                                                  
*                                                                               
QDIS2    L     R1,EFHREC           R1=A(CONRECH)                                
         TM    4(R1),X'20'         TEST RECORD TYPE CHANGED                     
         BO    *+8                 NO                                           
         NI    REQFILTH+4,X'DF'    INVALIDATE FILTER                            
         OI    4(R1),X'20'         SET RECORD VALIDATED                         
*                                                                               
         L     R1,EFHACT           R1=A(CONACTH)                                
         TM    4(R1),X'20'         TEST ACTION CHANGED                          
         BO    *+8                 NO                                           
         NI    REQFILTH+4,X'DF'    INVALIDATE FILTER                            
         OI    4(R1),X'20'         SET ACTION VALIDATED                         
*                                                                               
         MVC   QREQFILE,REQFILE    SET REQUEST FILE NAME                        
         OC    QREQFILE,QREQFILE                                                
         BNZ   *+10                                                             
         MVC   QREQFILE,=C'REQUEST'                                             
*                                                                               
         LA    R2,REQFILTH                                                      
         TM    4(R2),X'20'         TEST FILTERS CHANGED                         
         BZ    QDIS20              YES                                          
*                                                                               
         OC    LISTDIR(6),LISTDIR  TEST RECORD CHANGED                          
         BZ    QDIS20              YES                                          
         SPACE 1                                                                
* FILTERS UNCHANGED - SCAN SELECT FIELDS *                                      
         SPACE 1                                                                
         LA    R2,REQSEL1H                                                      
         LA    R4,LISTDIR                                                       
*                                                                               
QDIS4    CLI   5(R2),0             TEST INPUT                                   
         BE    QDIS8               NO                                           
         CLI   8(R2),C'D'          TEST 'DELETE'                                
         BE    QDIS6                                                            
         CLI   8(R2),C'C'          TEST 'CANCEL'                                
         BE    QDIS6                                                            
         MVI   ERROR,INVALID                                                    
         B     REQERR                                                           
*                                                                               
QDIS6    MVC   0(1,R4),8(R2)       SAVE INPUT CHARACTER IN DIR                  
*                                                                               
QDIS8    LA    R4,6(R4)            NEXT DIR                                     
         OC    2(3,R4),2(R4)       TEST MORE DISPLAYED                          
         BZ    QDIS10                                                           
         LA    R2,NEXTLINE(R2)     ADVANCE TO NEXT SCREEN LINE                  
         CLI   0(R2),0             TEST MORE LINES ON SCREEN                    
         BNE   QDIS4                                                            
         EJECT                                                                  
* SELECT FIELDS VALIDATED - PROCESS THEM *                                      
         SPACE 1                                                                
QDIS10   LA    R4,LISTDIR                                                       
         LA    R5,15                                                            
*                                                                               
QDIS12   CLI   0(R4),0                                                          
         BE    QDIS18                                                           
         CLI   0(R4),C'D'                                                       
         BE    QDEL                                                             
         CLI   0(R4),C'C'                                                       
         BE    QDEL                                                             
         DC    H'0'                                                             
QDIS18   LA    R4,6(R4)            NEXT DIR                                     
         BCT   R5,QDIS12                                                        
         B     QDIS20                                                           
         SPACE 2                                                                
* DELETE REQUEST *                                                              
         SPACE 1                                                                
QDEL     DS    0H                                                               
         XC    QADDR,QADDR                                                      
         MVC   QADDR(3),2(R4)      MOVE DISK ADDRESS                            
         GOTO1 DATAMGR,DMCB,=C'DMRDIR',QREQFILE,QADDR,AIO                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R7,AIO                                                           
         ZIC   RE,15(R7)           GET NUMBER OF CARDS - 1                      
         SRL   RE,4                                                             
         LA    RE,1(RE)                                                         
         LA    R7,26(R7)           POINT TO START OF REQUEST                    
*                                                                               
         MVC   0(2,R7),=C'99'      SET DELETED REQUEST CODE                     
         LA    R7,80(R7)                                                        
         BCT   RE,*-10                                                          
*                                                                               
         GOTO1 (RF),(R1),=C'DMWRT'                                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     QDIS18                                                           
         EJECT                                                                  
* CLEAR THE SCREEN *                                                            
         SPACE 1                                                                
QDIS20   LA    R2,REQSEL1H                                                      
*                                                                               
QDIS22   ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,QDISOC                                                        
         BZ    QDIS24                                                           
         EX    RE,QDISSPC                                                       
         BE    QDIS24                                                           
         EX    RE,QDISXC           CLEAR THE FIELD                              
         OI    6(R2),X'80'         SET XMT                                      
QDIS24   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   QDIS22                                                           
         B     QDIS30                                                           
*                                                                               
QDISOC   OC    8(0,R2),8(R2)                                                    
QDISSPC  CLC   8(0,R2),SPACES                                                   
QDISXC   XC    8(0,R2),8(R2)                                                    
         SPACE 2                                                                
* BUILD LIST OF FILTERS *                                                       
         SPACE 1                                                                
QDIS30   LA    R2,REQFILTH                                                      
         TM    4(R2),X'20'         TEST FILTERS CHANGED                         
         BO    QDIS32              NO                                           
         MVC   LASTLIST(2),QCRDCODE YES - START AT FIRST REQ                    
         MVI   LASTLIST+2,0                                                     
         MVI   LASTLIST+3,X'FF'                                                 
         XC    VERYFRST,VERYFRST   RESET DATA DISPLAYED SWITCH                  
*                                                                               
QDIS32   XC    BLOCK(200),BLOCK    CLEAR FILTER AREA                            
         XC    BLOCK+200(100),BLOCK+200                                         
         LA    R4,BLOCK                                                         
         LA    R2,REQFILTH                                                      
         CLI   5(R2),0             TEST ANY FILTERS                             
         BE    QDIS34              NO                                           
         GOTO1 SCANNER,DMCB,(R2),(8,(R4))                                       
         B     QDIS34                                                           
         SPACE 2                                                                
QDIS34   OI    4(R2),X'20'         SET FILTERS 'EDITED'                         
         XC    LISTDIR(90),LISTDIR                                              
         LA    R0,LISTDIR                                                       
         ST    R0,QNEXTDIR                                                      
         EJECT                                                                  
*** READ REQUESTS ***                                                           
         SPACE 1                                                                
         LA    R2,REQSEL1H                                                      
         MVI   QAGYDATA,0          RESET SWITCH                                 
*                                                                               
QDIS36   DS    0H                                                               
         OC    LASTLIST(2),LASTLIST TEST REACHED EOF                            
         BZ    QDIS70                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDIR',QREQFILE,LASTLIST,AIO                    
         TM    8(R1),X'80'         TEST EOF                                     
         BO    QDIS70              YES - MUST BE NO REQUESTS                    
*                                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
*** FILTER REQUEST ***                                                          
         SPACE 1                                                                
         L     R1,AIO                                                           
         USING REQHDRD,R1                                                       
         SPACE 1                                                                
         MVC   QADDR,LASTLIST      SET DISK ADDRESS OF CURRENT REQ              
         XC    LASTLIST,LASTLIST                                                
         MVC   LASTLIST(3),REQLINK SAVE LINK FIELD                              
*                                                                               
         CLC   REQCODE,QCRDCODE    MATCH THIS REQUEST TYPE                      
         BNE   QDIS36              NO- IGNORE                                   
         SPACE 1                                                                
* ALLOW DDS TERMINAL TO FILTER ON AGENCY *                                      
         SPACE 1                                                                
         CLI   TWAOFFC,C'*'        TEST DDS TERMINAL                            
         BNE   QDIS37                                                           
         CLI   BLOCK,1             TEST FOR 'U=XXX'                             
         BNE   QDIS37                                                           
         CLI   BLOCK+12,C'U'                                                    
         BNE   QDIS37                                                           
*                                                                               
         MVI   QAGYDATA,C'Y'       SET FLAG AGENCY SPECIFIED                    
         LA    R4,BLOCK            SET TO SKIP FILTER 1                         
         CLC   =C'ALL',BLOCK+22    TEST 'ALL' AGENCIES                          
         BE    QDIS48                                                           
         CLC   REQAGY,BLOCK+22     ELSE MATCH AGENCY                            
         BE    QDIS48                                                           
         B     QDIS36                                                           
*                                                                               
QDIS37   DS    0H                  MATCH AGENCY CODE IN REQUEST                 
         CLI   REQAGY+1,C'*'       SPECIAL FOR ACCPAK                           
         BNE   QDIS37B                                                          
         CLC   REQORIG,TWAORIG     CHECK AGAINST ORIGIN ID NUMBER               
         BNE   QDIS36                                                           
         B     *+14                                                             
QDIS37B  CLC   TWAAGY,REQAGY                                                    
         BNE   QDIS36                                                           
*                                                                               
         LA    R4,BLOCK-32         POINT TO FIRST ENTRY - 1                     
         B     QDIS48                                                           
         SPACE 1                                                                
* MATCH FILTER TO EACH FIELD IN REQUEST *                                       
         SPACE 1                                                                
QDIS38   ZIC   R1,QCTR                                                          
         MH    R1,=H'80'                                                        
         A     R1,AIO                                                           
         LA    R1,38(R1)           POINT TO FIRST FIELD ON CARD                 
         LA    R0,67(R1)           POINT TO LAST COL                            
         ST    R0,QEND             AND SET ITS ADDRESS                          
*                                                                               
QDIS40   PACK  DUB,2(2,R1)         PACK LENGTH                                  
         CVB   RF,DUB                                                           
         BCTR  RF,0                                                             
         LA    RE,4(R1)            POINT TO DATA                                
*                                                                               
         MVC   WORK,SPACES         WORK WILL CONTAIN FIELD CONTENTS             
         L     R5,QALSTFLD         A(LAST FIELD FOUND)                          
         CLC   0(2,R5),0(R1)       IS THIS A CONTINUATION?                      
         BNE   QDIS41              NO                                           
*                                                                               
         PACK  DUB,2(2,R5)         PACK PREVIOUS FIELD LENGTH                   
         CVB   RF,DUB                                                           
         LA    RE,WORK(RF)                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),4(R5)       MOVE IN FIRST PORTION OF DATA                
         PACK  DUB,2(2,R1)         PACK NEW LENGTH                              
         CVB   RF,DUB                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),4(R1)       MOVE SECOND PORTION OF DATA                  
         B     QDIS45                                                           
*                                                                               
QDIS41   CLC   =C'DDS,',0(RE)                                                   
         BNE   QDIS42                                                           
         SH    RF,=H'4'                                                         
         LA    RE,4(RE)                                                         
         B     QDIS44                                                           
*                                                                               
QDIS42   CLC   =C'OV,',0(RE)                                                    
         BE    *+14                                                             
         CLC   =C'ON,',0(RE)                                                    
         BNE   QDIS44                                                           
         SH    RF,=H'3'                                                         
         LA    RE,3(RE)                                                         
*                                                                               
QDIS44   EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)       MOVE FILTER INTO WORK (BLANK PADDED)         
*                                                                               
QDIS45   ST    R1,QALSTFLD         SAVE A(THIS FIELD)                           
         ZIC   RE,0(R4)            GET FILTER LENGTH                            
         CLI   0(R4),3             ALWAYS MATCH FOR AT LEAST 3 CHARS            
         BH    *+8                                                              
         LA    RE,3                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),WORK                                                    
         BE    QDIS48                                                           
         SPACE 1                                                                
* THIS FIELD DOESN'T MATCH  - TRY NEXT FIELD *                                  
         SPACE 1                                                                
         PACK  DUB,2(2,R1)         GET FIELD LENGTH                             
         CVB   RF,DUB                                                           
         LA    R1,4(R1,RF)         POINT BEYOND FIELD                           
*                                                                               
         CLI   0(R1),C'*'          TEST END OF REQUEST                          
         BE    QDIS36              YES - READ NEXT                              
*                                                                               
         LA    R1,1(R1)                                                         
         C     R1,QEND             TEST PAST END OF CARD                        
         BNL   QDIS46                                                           
*                                                                               
         CLI   0(R1),C' '          TEST END OF CARD                             
         BNE   QDIS40              NO - CONTINUE                                
         SPACE 1                                                                
* ADVANCE TO NEXT CARD *                                                        
         SPACE 1                                                                
QDIS46   ZIC   R1,QCTR                                                          
         LA    R1,1(R1)                                                         
         STC   R1,QCTR                                                          
         B     QDIS38                                                           
*                                                                               
QDIS48   LA    R4,32(R4)           NEXT FILTER                                  
*                                                                               
         CLI   0(R4),0             ANY MORE                                     
         BE    QDIS50              NO - GO DISPLAY REQUEST                      
*                                                                               
         MVI   QCTR,0              RESET CARD COUNTER                           
         XC    QALSTFLD,QALSTFLD   NO FIELDS FOUND YET                          
         B     QDIS38                                                           
         EJECT                                                                  
*** DISPLAY REQUEST ***                                                         
         SPACE 1                                                                
QDIS50   DS    0H                                                               
         L     R1,AIO                                                           
         LA    R1,38(R1)           POINT TO FIRST REQUEST FIELD                 
         LA    R0,67(R1)                                                        
         ST    R0,QEND                                                          
         MVI   QCTR,0              RESET CARD COUNTER                           
         LA    R4,ELEM-1           OUTPUT BUILD AREA                            
         MVC   ELEM,SPACES                                                      
         XC    QALSTFLD,QALSTFLD   NO FIELDS FOUND YET                          
*                                                                               
         CLI   QAGYDATA,C'Y'       TEST AGENCY SPECIFIED                        
         BNE   QDIS52                                                           
         L     RE,AIO                                                           
         USING REQHDRD,RE                                                       
         MVC   1(2,R4),REQAGY      DISPLAY AGENCY                               
         CLI   REQAGY+1,C'*'       SPECIAL FOR ACCPAK                           
         BNE   QDIS51                                                           
         EDIT  (2,REQORIG),(4,1(R4))                                            
         DROP  RE                                                               
         LA    R4,2(R4)                                                         
QDIS51   LA    R4,3(R4)                                                         
*                                                                               
QDIS52   PACK  DUB,2(2,R1)         FIELD LENGTH                                 
         CVB   RE,DUB                                                           
         BCTR  RE,0                                                             
*                                                                               
         CLI   ONLYSPUL,C'Y'       TEST SPOOL ONLY SYSTEM                       
         BE    QDIS54              YES                                          
*                                                                               
         CLC   =C'02',0(R1)        NOT SPOOL ONLY - TEST RECORD FIELD           
         BNE   QDIS53                                                           
         SPACE 1                                                                
* SINCE EOD CODES ARE NOT UNIQUE, MATCH ON RECORD NAME AND ACTION *             
         SPACE 1                                                                
         L     RF,EFHREC                                                        
         CLC   8(3,RF),4(R1)       MATCH 3 CHARACTERS                           
         BNE   QDIS36              NO - SKIP REQUEST                            
         B     QDIS58              AND SINCE IT MATCHED, NO DISPLAY             
*                                                                               
QDIS53   CLC   =C'03',0(R1)        NOT SPOOL ONLY - TEST ACTION FIELD           
         BNE   QDIS56                                                           
         SPACE 1                                                                
         L     RF,EFHACT                                                        
         CLC   8(3,RF),4(R1)       MATCH 3 CHARACTERS                           
         BNE   QDIS36              NO - SKIP REQUEST                            
         B     QDIS58              AND SINCE IT MATCHED, NO DISPLAY             
*                                                                               
QDIS54   CLC   =C'03',0(R1)        SPOOL ONLY - TEST REPORT FIELD               
         BE    QDIS58              YES SKIP                                     
*                                                                               
QDIS56   L     R5,QALSTFLD         A(LAST FIELD FOUND)                          
         CLC   0(2,R5),0(R1)       IS THIS A CONTINUATION?                      
         BNE   *+6                 NO                                           
         BCTR  R4,0                YES -- APPEND THIS TO PREVIOUS FIELD         
*                                                                               
         ST    R1,QALSTFLD         SAVE A(THIS FIELD)                           
         EX    RE,*+8              MOVE FIELD TO DISPLAY AREA                   
         B     *+10                                                             
         MVC   1(0,R4),4(R1) *EXECUTED*                                         
*                                                                               
         LA    R4,2(R4,RE)         POINT TO NEXT OUTPUT POSITION-1              
         MVI   0(R4),C'-'          KEY FIELD SEPERATOR                          
         CLC   =C'08',0(R1)        WAS THAT A 'KEY' FIELD                       
         BNL   *+8                 YES                                          
         MVI   0(R4),C','          DATA FIELD SEPERATOR                         
         LA    R0,ELEM+80                                                       
         CR    R4,R0               TEST ANY MORE DISPLAY ROOM                   
         BNL   QDIS62              NO                                           
*                                                                               
QDIS58   LA    R1,5(RE,R1)         POINT BEYOND CURRENT FIELD                   
         CLI   0(R1),C'*'          TEST END OF REQ                              
         BE    QDIS62                                                           
         LA    R1,1(R1)                                                         
         C     R1,QEND             TEST PAST END OF CARD                        
         BNL   QDIS60                                                           
         CLI   0(R1),C' '          TEST END OF CARD                             
         BNE   QDIS52                                                           
         SPACE 1                                                                
* ADVANCE TO NEXT CARD *                                                        
         SPACE 1                                                                
QDIS60   ZIC   R1,QCTR                                                          
         LA    R1,1(R1)                                                         
         STC   R1,QCTR                                                          
         MH    R1,=H'80'                                                        
         LA    R1,38(R1)                                                        
         A     R1,AIO                                                           
         LA    R0,67(R1)                                                        
         ST    R0,QEND             SET END CARD ADDRESS                         
         B     QDIS52                                                           
*                                                                               
QDIS62   MVI   0(R4),C'*'          SHOW END OF REQUEST                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               ADVANCE TO DISPLAY FIELD                     
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'            SET FOR EX                                   
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),ELEM *EXECUTED*                                          
         OI    6(R2),X'80'         SET XMT BIT                                  
*                                                                               
         L     RE,QNEXTDIR         GET DIRECTORY POINTER                        
         MVC   2(3,RE),QADDR       MOVE DISK ADDRESS TO DIRECTORY               
*                                                                               
         OC    VERYFRST,VERYFRST   TEST ANY DISPLAYED YET                       
         BNZ   *+10                                                             
         MVC   VERYFRST(3),QADDR   NO - SET SWITCH                              
*                                                                               
         LA    RE,6(RE)            ADVANCE DIRECTORY POINTER                    
         ST    RE,QNEXTDIR         AND SAVE                                     
         LA    R0,LISTDIR+89                                                    
         CR    RE,R0               TEST MORE ROOM IN LIST                       
         BH    QDIS70              NO - DONE                                    
*                                                                               
         ZIC   R0,0(R2)            ADVANCE TO NEXT SELECT FIELD                 
         AR    R2,R0                                                            
         CLI   0(R2),0             TEST EOS                                     
         BNE   QDIS36                                                           
         SPACE 1                                                                
* REACHED END OF DATA OR END OF SCREEN - SELECT A MESSAGE *                     
         SPACE 1                                                                
QDIS70   OC    LISTDIR(6),LISTDIR  ANY DATA IN DIRECTORY                        
         BZ    QDIS80              NO                                           
         MVC   CONHEAD(L'SELMSG),SELMSG                                         
         LA    R2,REQSEL1H                                                      
         B     QDIS90                                                           
*                                                                               
SELMSG   DC    C'DATA DISPLAYED. SELECT OR HIT ENTER FOR NEXT.'                 
*                                                                               
QDIS80   LA    R2,REQFILTH         POINT TO FILTER FIELD                        
         NI    4(R2),X'DF'         RESET 'EDITED' BIT                           
*                                                                               
         OC    VERYFRST,VERYFRST   TEST ANY DATA FOR THIS REQUEST               
         BNZ   QDIS82              YES - GO SAY NO MORE                         
         MVC   CONHEAD(18),=C'NO DATA TO DISPLAY'                               
         L     R2,EFHREC                                                        
         B     QDIS90                                                           
*                                                                               
QDIS82   MVC   CONHEAD(39),=C'NO MORE TO DISPLAY. HIT ENTER FOR FIRST'          
         L     R2,EFHREC                                                        
*                                                                               
QDIS90   OI    6(R2),X'40'         POSITION CURSOR                              
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         ORG   BLOCK+300                                                        
QADDR    DS    F                                                                
QNEXTDIR DS    A                                                                
QEND     DS    A                                                                
QALSTFLD DS    A                                                                
QCTR     DS    C                                                                
QAGYDATA DS    C                                                                
QREQFILE DS    CL7                                                              
         SPACE 2                                                                
REQHDRD  DSECT                                                                  
       ++INCLUDE DMREQHDR                                                       
REQCODE  DS    CL2                                                              
REQAGY   DS    CL2                                                              
         EJECT                                                                  
       ++INCLUDE DDGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
REQSCRD  DSECT                                                                  
       ++INCLUDE GEREQFDD                                                       
*                                                                               
NEXTLINE EQU   REQSEL2H-REQSEL1H   DSPL TO NEXT LINE                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034GEREQ00X  08/29/00'                                      
         END                                                                    
