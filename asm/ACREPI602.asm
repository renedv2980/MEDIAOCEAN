*          DATA SET ACREPI602  AT LEVEL 069 AS OF 03/25/15                      
*PHASE ACI602A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'ACI602 - JOB NUMBERS TAPE FOR OGILVY'                           
ACI602   CSECT                                                                  
         DS    CL4000                                                           
         ORG   ACI602                                                           
         PRINT NOGEN                                                            
         NMOD1 0,**ACI6**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACI6D,RC                                                         
         LA    R9,OUTAREA                                                       
         USING RECD,R9                                                          
         LA    R8,P                                                             
         USING PFIELDSD,R8                                                      
         EJECT                                                                  
*              ROUTINES EXECUTED AT FIRST FOR RUN                               
         SPACE 2                                                                
         USING ACKEYD,R3                                                        
         CLI   MODE,RUNFRST                                                     
         BNE   I610                                                             
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         ZAP   COUNT,=P'0'           INIT # OF TAPES REQUESTED                  
         MVC   MYIO(1),RCSVCOMP    COMPANY CODE                                 
         MVC   MYIO+1(2),=C'SJ'                                                 
         MVI   ELCODE,X'30'        GET 30 ELEMENT                               
         LA    R3,MYIO                                                          
         B     I604                                                             
I602     LA    R3,MYIO                                                          
         ZIC   R4,ACKEYACC+14                                                   
         AH    R4,=H'1'                                                         
         STC   R4,ACKEYACC+14                                                   
I604     GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',(R3),(R3)                        
         CLC   ACKEYACC+1(2),=C'SJ'                                             
         BNE   I605                END OF SJ RECORDS                            
         CLC   ACKEYACC+9(6),SPACES                                             
         BE    I602                IF NO JOB LEVEL, GET NEXT RECORD             
         BAS   RE,GETEL            IF 30 ELEMENT PRESENT                        
         BNE   I604A                                                            
         USING ACSTATD,R3                                                       
         CLI   ACSTFILT,C'X'       AND IF FILTER 1 = X IN 30 ELEMENT            
         BE    I602                SKIP TO NEXT RECORD                          
I604A    GOTO1 =V(SORTER),DMCB,=C'PUT',MYIO                                     
         B     I602                                                             
         SPACE                                                                  
         DROP  R3                                                               
         SPACE                                                                  
*              START BUILDING TABLE OF DUPLICATES                               
I605     LA    R4,DUPTAB           R4 = POINTER TO CURRENT DUPTAB ENTRY         
         LA    R3,PRETAB           R3 = POINTER TO PREV                         
         LH    R0,=Y(NDUPS)                                                     
I606     GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     R2,4(R1)            GET A(RECORD) INTO R2                        
         LTR   R2,R2               SET COND CODE                                
         BZ    I609                XIT IF R2 = 0, END OF DATA                   
         CLC   9(6,R2),9(R4)       COMPARE JOB NUMBERS                          
         BNE   I608                BRANCH IF NOT SAME                           
         LA    R4,15(R4)           DUPLICATION, CURRENT = NEXT ENTRY            
         BCTR  R0,0                DECRIMENT TABLE COUNTER                      
         LTR   R0,R0               IS THERE ENOUGH ROOM FOR THE NEXT            
         BZ    I6DUMP              NO                                           
         MVC   0(15,R4),0(R2)      PUT THIS SORT REC INTO CURRENT ENTRY         
         LR    R3,R4               UPDATE PREV ENTRY TO THIS ENTRY              
         LA    R4,15(R4)           DUPLICATION, CURRENT = NEXT ENTRY            
         B     I608A                                                            
I608     MVC   0(15,R4),0(R2)      PUT THIS SORT REC INTO CURRENT ENTRY         
         CLC   9(6,R2),9(R3)       COMPARE WITH PREV ENTRY'S JOB NUMBER         
         BNE   I606                NO DUPLICATION YET, KEEP CURRENT             
         LR    R3,R4               UPDATE PREV ENTRY TO THIS ENTRY              
         LA    R4,15(R4)           DUPLICATION, CURRENT = NEXT ENTRY            
I608A    BCT   R0,I606                                                          
I6DUMP   DC    H'0'                DUMP IF RUN OUT OF ROOM IN DUPTAB            
I609     MVI   0(R4),X'00'         SET END OF TABLE, CURRENT NOT DUP            
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES EXECUTED AT FIRST FOR REQUEST                           
         SPACE 2                                                                
I610     CLI   MODE,REQFRST                                                     
         BNE   I620                                                             
         SPACE                                                                  
         MVI   RCSUBPRG,0                                                       
         ZAP   NUMREC,=P'0'        INIT NUMBER OF RECORDS PROCESSED             
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT1,C'Y'                                                       
         BNE   I611                                                             
         AP    COUNT,=P'1'                                                      
         CVB   R4,COUNT                                                         
         MVC   DSNAME+13(2),ALPHAID                                             
         GOTO1 DYNALLOC,DMCB,(0,=CL8'TAPE'),((R4),DSNAME)                       
         OPEN  (TAPE,OUTPUT)                                                    
         SPACE                                                                  
*              CHANGE QSTART AND QEND TO PACKED UNSIGNED                        
I611     CLC   QSTART,SPACES                                                    
         BE    I615                                                             
         GOTO1 DATCON,DMCB,(0,QSTART),(1,QSTPK)                                 
         CLC   QEND,SPACES                                                      
         BNE   *+14                                                             
         MVC   QENDPK,QSTPK        IF QEND IS BLANK, SET QEND = QSTART          
         B     XIT                                                              
         GOTO1 DATCON,DMCB,(0,QEND),(1,QENDPK)                                  
         B     XIT                                                              
         SPACE                                                                  
*              IF QSTART IS BLANK, SET QSTART AND QEND TO TODAY'S DATE          
I615     GOTO1 DATCON,DMCB,(5,QSTART),(1,QSTPK)                                 
         MVC   QENDPK,QSTPK                                                     
         SPACE                                                                  
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
*              ROUTINES EXECUTED AT PROCESS AN ACCOUNT RECORD                   
         SPACE 2                                                                
I620     CLI   MODE,PROCACC                                                     
         BNE   I630                                                             
         USING ACSTATD,R4                                                       
         L     R4,ADACCSTA         GET ADDRESS OF 30 ELEMENT                    
         CLC   ACSTBFDT,QSTPK                                                   
         BL    XIT                 DATE BALANCE B/FRWD BEFORE QSTART            
         CLC   ACSTBFDT,QENDPK                                                  
         BH    XIT                 DATE BALANCE B/FRWD AFTER QEND               
         LH    R0,=Y(NDUPS)                                                     
         L     R3,ADACC                                                         
         LA    R2,DUPTAB           R2 = CURRENT DUPLICATE ENTRY                 
I622     CLC   9(6,R2),9(R3)                                                    
         BE    XIT                 DUPLICATE, SKIP TO NEXT ACCOUNT              
         BH    I623                JOB NUMBER NOT A DUPLICATE, PROCESS          
         CLI   0(R2),X'00'                                                      
         BE    I623                END OF TABLE, NOT A DUPLICATE                
         LA    R2,15(R2)           GET NEXT ENTRY                               
         BCT   R0,I622             BRANCH IF NOT END OF TABLE                   
I623     MVI   SPACING,X'02'       SET DOUBLE SPACING                           
         MVI   OUTAREA,X'40'       SET OUTAREA TO BLANKS                        
         MOVE  (OUTAREA+1,279),OUTAREA                                          
         MVC   P,OUTAREA           SET P TO BLANKS IN CASE QOPT2 NOT Y          
         AP    NUMREC,=P'1'        INCREMENT RECORD COUNTER                     
         GOTO1 DATCON,DMCB,(1,ACSTBFDT),(8,PFDATE) MOVE DATE TO PRINT           
         MVI   RCSTAT,C'A'         INIT JOB STATUS TO A ON TAPE                 
         TM    ACSTSTAT,X'40'+X'20'  ACCOUNT IS CLOSED/LOCKED                   
         BZ    *+8                                                              
         MVI   RCSTAT,C'I'         SET JOB STATUS TO I ON TAPE                  
         L     R4,ADTRANS          GET ADDRESS OF 24 ELEMENT                    
         USING TRANSD,R4                                                        
         MVC   PFOFFCOD,TRNSANAL   DONOVAN OFFICE CODE                          
         LR    R4,R3                                                            
         USING ACKEYD,R4                                                        
         MVC   PFCLIENT,ACKEYACC+3 CLIENT                                       
         MVC   PFPROD,ACKEYACC+6   PRODUCT                                      
         MVI   ELCODE,X'99'                                                     
         BAS   RE,GETEL                                                         
         BE    *+14                                                             
         MVC   PFPRIMJB,ACKEYACC+9 PRIMARY JOB # = DDS JOB #                    
         B     I625                                                             
         USING ACJVD,R3                                                         
         MVC   PFPRIMJB,ACJVJOB    PRIMARY JOB # = JV JOB #                     
         MVC   PFSECJOB,ACKEYACC+9 SECONDARY JOB # = DDS JOB #                  
I625     L     R3,ADHEIRB          SET R3 TO BEGINNING OF RECORD                
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL                                                         
         BNE   I625A                                                            
         USING ACNOD,R3                                                         
         ZIC   R5,ACNOLEN                                                       
         SH    R5,=H'3'                                                         
         CLI   ACNOLEN,X'06'       SEE IF L'ACNO > 4                            
         BNH   *+8                                                              
         LA    R5,3                IF YES, JUST MOVE 4                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   PFACCODE(0),ACNO                                                 
I625A    L     R3,ADACCNAM                                                      
         USING ACNAMED,R3                                                       
         CLI   ACNMLEN,X'26'                                                    
         BNL   I626                LENGTH = 36                                  
         ZIC   R4,ACNMLEN          LENGTH < 36                                  
         SH    R4,=H'3'                                                         
         B     *+8                                                              
I626     LA    R4,34               LENGTH = 36, JUST MOVE 35                    
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   PFJOBDES(0),ACNMNAME JOB DESCRIPTION                             
         CLI   RCSTAT,C'I'         IF CLOSED/INACTIVE                           
         BNE   I627                                                             
         L     R3,ADHEIRC          SET R3 TO BEGINNING OF JOB RECORD            
         USING ACJOBD,R3                                                        
         MVI   ELCODE,X'26'                                                     
         BAS   RE,GETEL                                                         
         BNE   I627                                                             
         GOTO1 DATCON,DMCB,(1,ACJBCLOS),(8,PFCLOSED) PRINT CLOSED DATE          
         GOTO1 (RF),(R1),,(0,RCCLOSED) MOVE CLOSED DATE TO TAPE                 
I627     CLI   QOPT1,C'Y'                                                       
         BNE   I628                                                             
         MVC   RCJOBDES,PFJOBDES                                                
         MVC   RCOFFCOD,PFOFFCOD                                                
         MVC   RCCLIENT,PFCLIENT                                                
         MVC   RCPROD,PFPROD                                                    
         MVC   RCPRIMJB,PFPRIMJB                                                
         MVC   RCSECJOB,PFSECJOB                                                
         MVC   RCACCODE,PFACCODE                                                
         PUT   TAPE,OUTAREA                                                     
I628     CLI   QOPT2,C'Y'          IF QOPT2=Y                                   
         BNE   XIT                                                              
         GOTO1 ACREPORT            WANT DETAILS                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES EXECUTED AT LAST FOR REQUEST                            
         SPACE 2                                                                
I630     CLI   MODE,REQLAST                                                     
         BNE   I640                                                             
         CLI   QOPT2,C'Y'          IF WANT DETAILS                              
         BNE   I634                                                             
         MVC   P+1(L'ENDING),ENDING                                             
         LA    R7,P+L'ENDING+2                                                  
         EDIT  NUMREC,(7,(R7)),COMMAS=YES,ALIGN=LEFT                            
         MVI   SPACING,X'02'                                                    
         GOTO1 ACREPORT                                                         
I634     CLI   QOPT1,C'Y'                                                       
         BNE   XIT                                                              
         CLOSE (TAPE)                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES EXECUTED AT LAST FOR RUN                                
         SPACE 2                                                                
I640     CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         MVI   P,X'40'             SET TO BLANKS IN CASE QOPT2 NOT Y            
         MVC   P+1(131),P                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,X'01'                                                   
         LA    R3,DUPTAB                                                        
         LH    R0,=Y(NDUPS)                                                     
I642     CLI   0(R3),X'00'                                                      
         BE    XIT                 END OF TABLE                                 
         MVC   PFCLIENT,3(R3)                                                   
         MVC   PFPROD,6(R3)                                                     
         MVC   PFPRIMJB,9(R3)                                                   
         MVI   SPACING,X'02'                                                    
         GOTO1 ACREPORT                                                         
         LA    R3,15(R3)           GET NEXT ENTRY                               
         BCT   R0,I642                                                          
         B     XIT                                                              
         EJECT                                                                  
*              CONSTANTS                                                        
         SPACE                                                                  
TAPE     DCB   DSORG=PS,MACRF=PM,DDNAME=TAPE,BLKSIZE=2800,LRECL=280             
         LTORG                                                                  
         SPACE 2                                                                
ENDING   DC    C'TOTAL NUMBER OF RECORDS ='                                     
DSNAME   DC    CL20'ACCTAPE.AC0I6  1'                                           
SORTCARD DC    CL80'SORT FIELDS=(10,6,A),FORMAT=BI,WORK=1' KEY=JOB NUM          
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(15)'                                  
PRETAB   DC    XL15'00'            MUST BE RIGHT BEFORE DUPTAB                  
DUPTAB   DC    (NDUPS)XL15'00'     TABLE OF DUPLICATE DDS JOB NUMBERS           
NDUPS    EQU   32767               NUMBER OF DUPLICATE ENTRIES                  
         EJECT                                                                  
*              DSECT FOR PROGRAM                                                
         SPACE                                                                  
ACI6D    DSECT                                                                  
OUTAREA  DS    CL280                                                            
MYIO     DS    CL1000                                                           
NUMREC   DS    PL4                 NUMBER OF RECORDS PROCESSED                  
ELCODE   DS    XL1                 FOR GETELS, ETC.                             
QSTPK    DS    CL3                 QSTART IN PACKED UNSIGNED                    
QENDPK   DS    CL3                 QEND IN PACKED UNSIGNED                      
         DS    0D                                                               
COUNT    DS    PL8                 # OF TAPES REQUESTED                         
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         SPACE                                                                  
PFIELDSD DSECT                                                                  
         DS    C                                                                
PFCLIENT DS    CL3                                                              
         DS    CL2                                                              
PFPROD   DS    CL3                                                              
         DS    CL3                                                              
PFPRIMJB DS    CL6                                                              
         DS    CL6                                                              
PFJOBDES DS    CL35                                                             
         DS    CL2                                                              
PFOFFCOD DS    CL2                                                              
         DS    CL3                                                              
PFACCODE DS    CL4                                                              
         DS    CL7                                                              
PFSECJOB DS    CL6                                                              
         DS    CL9                                                              
PFDATE   DS    CL8                                                              
         DS    CL2                                                              
PFCLOSED DS    CL8                                                              
         SPACE 3                                                                
*              DSECT TO COVER OUTPUT RECORDS                                    
         SPACE                                                                  
RECD     DSECT                                                                  
RCOFFCOD DS    CL2                                                              
RCPRIMJB DS    CL6                                                              
RCSTAT   DS    CL1                                                              
         DS    CL1                                                              
RCCLOSED DS    CL6                                                              
         DS    CL52                                                             
RCJOBDES DS    CL35                                                             
RCCLIENT DS    CL3                                                              
RCPROD   DS    CL3                                                              
         DS    CL158                                                            
RCACCODE DS    CL4                                                              
RCSECJOB DS    CL6                                                              
         DS    CL3                                                              
         EJECT                                                                  
*        ACGENBOTH                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'069ACREPI602 03/25/15'                                      
         END                                                                    
