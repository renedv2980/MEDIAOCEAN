*          DATA SET DMEXTENDO  AT LEVEL 083 AS OF 05/01/02                      
*PHASE DMEXTFWR  <=== NOTE FRED IS RESPONSIBLE IF THIS GOES WRONG !             
*INCLUDE CARDS                                                                  
*INCLUDE PRINT110                                                               
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DMDMGRL                                                                
       ++INCLUDE DMGREQUS                                                       
         TITLE 'DMEXTEND - EXTEND AND ERASE A DIRECT ACCESS FILE'               
         PRINT NOGEN                                                            
DMEXTEND CSECT                                                                  
         NBASE 0,DMEXTEND,WORK=A(DMEXWORK)                                      
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         MVC   TITLE(8),=CL8'DMEXTEND'                                          
*                                                                               
DME10    DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
         CLI   CARD,C'*'           TEST FOR A COMMENT                           
         BE    DME10                                                            
         CLC   =C'FIND',CARD                                                    
         BE    DMEFIND                                                          
         CLC   =C'DISPLAY',CARD                                                 
         BE    DMEFIND                                                          
         CLC   =C'FIXOLD',CARD                                                  
         BE    DMEFIX                                                           
         CLC   =C'FIXNEW',CARD                                                  
         BE    DMEFIX                                                           
         CLC   =C'DDSIO=',CARD                                                  
         BNE   DME12                                                            
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6                                                   
         B     DME10                                                            
         SPACE 1                                                                
DME12    LA    RE,CARD             VALIDATE ACTION                              
         LA    R0,8                MAX ACTION LEN IS 8                          
DME14    CLI   0(RE),C' '                                                       
         BE    DME16                                                            
         CLI   0(RE),C'='                                                       
         BE    DME16                                                            
         LA    RE,1(RE)                                                         
         BCT   R0,DME14                                                         
         B     ACTNERR                                                          
*                                                                               
DME16    ST    RE,FULL             SAVE TERMINATOR ADDRESS                      
*                                                                               
         LA    R4,ACTTAB                                                        
         LA    R0,CARD                                                          
         SR    RE,R0               GIVES LENGTH                                 
         STC   RE,FULL             AND DATA LENGTH                              
*                                                                               
DME18    ZIC   RE,FULL             RESTORE DATA LENGTH                          
         CLM   RE,1,0(R4)          TEST EQUAL NUMBER OF CHARACTERS              
         BNE   DME20                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   CARD(0),2(R4) *EXECUTED*                                         
         BE    DME30                                                            
*                                                                               
DME20    LA    R4,L'ACTTAB(R4)     POINT TO NEXT ENTRY                          
         CLI   0(R4),X'FF'         TEST EOL                                     
         BNE   DME18                                                            
         B     ACTNERR                                                          
         EJECT                                                                  
****************************************************************                
* TABLE ENTRIES ARE AS FOLLOWS -                               *                
* LENGTH FOR ACTION NAME COMPARE                               *                
* ACTION DEFINITION BITS  ====  X'80' = NO OLD FILE            *                
*                         ====  X'40' = EXTEND ON SAME VOLUME  *                
* ACTION NAME IN CHARACTERS                                    *                
         SPACE 1                                               *                
ACTTAB   DS    0XL10                                           *                
         DC    AL1(6),X'80'                                    *                
ACT1NAME DC    CL8'DEFINE'                                     *                
         DC    AL1(6),X'C0'                                    *                
ACT2NAME DC    CL8'DEFVOL'                                     *                
         DC    AL1(6),X'00'                                    *                
ACT3NAME DC    CL8'EXTEND'                                     *                
         DC    AL1(6),X'40'                                    *                
ACT4NAME DC    CL8'EXTVOL'                                     *                
         DC    X'FF'                                           *                
****************************************************************                
         EJECT                                                                  
DME30    DS    0H                                                               
         MVI   NEWXTNTS,1          SET DEFAULT NUMBER                           
         L     RE,FULL                                                          
         CLI   0(RE),C'='          TEST XTNTS SPECIFIED                         
         BNE   DME32               NO                                           
         MVC   NEWXTNTS,1(RE)                                                   
         NI    NEWXTNTS,X'0F'      MAKE HEX                                     
*                                                                               
         CLI   1(RE),C'1'                                                       
         BL    XTNTERR                                                          
         CLI   1(RE),C'9'                                                       
         BH    XTNTERR                                                          
*                                                                               
DME32    DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
         CLI   CARD,C'*'                                                        
         BE    DME32                                                            
*                                                                               
         CLC   CARD(9),=C'ERASE=ALL'                                            
         BE    DME32X                                                           
         CLC   CARD(9),=C'ERASE=NEW'                                            
         BE    DME32X                                                           
         CLC   CARD(8),=C'ERASE=NO'                                             
         BE    DME32X                                                           
         B     ERSERR                                                           
*                                                                               
DME32X   LA    R3,OLDFILE          POINT TO OLD FILE                            
         USING DTFPHD,R3                                                        
         XC    P1(24),P1           SET DADDS PARAM LIST                         
         L     RE,=A(DMEXBUFF)                                                  
         ST    RE,P2                                                            
         MVI   P2,X'FF'                                                         
         ST    R3,P4                                                            
         LA    RE,P6                                                            
         ST    RE,P5                                                            
         EJECT                                                                  
DME34    TM    1(R4),X'80'         TEST OLD EXTENT EXISTS                       
         BO    DME40               NO                                           
         SPACE 1                                                                
* PROCESS OLD EXTENT *                                                          
         SPACE 1                                                                
         GOTO1 =V(DADDS),P1,A(DAOPEN)                                           
         SR    R1,R1                                                            
         LA    RE,DMTX                                                          
*                                                                               
DME36    CLI   0(RE),X'FF'         COUNT NUMBER OF EXISTING EXTENTS             
         BE    DME38                                                            
         LA    R1,1(R1)                                                         
         LA    RE,14(RE)                                                        
         B     DME36                                                            
*                                                                               
DME38    STC   R1,OLDXTNTS                                                      
         GOTO1 =V(DADDS),P1,A(DACLOSE)                                          
         SPACE 2                                                                
DME40    LA    R2,NEWFILE          OPEN NEW FILE AS MVS SEQUENTIAL              
*NOP     OPEN  ((2),EXTEND)                                                     
         OPEN  ((2),OUTPUT)                                                     
         TM    48(R2),X'10'        TEST OPEN SUCCEEDED                          
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DME44    TM    1(R4),X'40'         ARE NEW EXTENTS ON SAME VOL                  
         BO    DME50               YES                                          
         SPACE 1                                                                
* NEW EXTENTS ON NEW VOLUME - WRITE ONE RECORD FOR EACH EXTENT *                
         SPACE 1                                                                
         ZIC   R5,NEWXTNTS                                                      
         TM    1(R4),X'80'         TEST OLD EXTENT EXISTS                       
         BO    DME47               NO - SKIP FIRST FEOV                         
DME46    DS    0H                                                               
         MVC   P(12),=C'BEFORE FEOV'                                            
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,PRTDEB                                                        
         FEOV  (2)                 FORCE VOLUME SWITCH                          
         MVC   P(12),=C'AFTER  FEOV'                                            
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,PRTDEB                                                        
*                                                                               
DME47    PUT   (2),NEWREC                                                       
*                                                                               
         L     RE,NEWFILE+44       GET DEB ADDRESS                              
         SR    R0,R0                                                            
         ICM   R0,1,16(RE)         NUMBER OF EXTENTS IN DEB                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    RF,32(RE)           POINT TO FIRST EXTENT                        
         B     DME47B                                                           
*                                                                               
DME47A   LA    RF,16(RF)           POINT TO NEXT                                
*                                                                               
DME47B   BCT   R0,DME47A                                                        
         SPACE 1                                                                
* FORCE END OF FILE TO END CCHH ON LAST EXTENT *                                
         SPACE 1                                                                
         MVC   NEWFILE+8(4),10(RF) MOVE END CCHH TO DCB                         
         MVI   NEWFILE+12,99       SET HIGH RECORD NUM                          
         MVI   NEWFILE+18,X'00'    SET TRACK BALANCE TO 0                       
         MVI   NEWFILE+19,X'00'    SET TRACK BALANCE TO 0                       
*                                                                               
         BCT   R5,DME46                                                         
*                                                                               
         MVC   P(21),=C'BEFORE F...ING CLOSE'                                   
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,PRTDEB                                                        
*                                                                               
         CLOSE ((2),)                                                           
*                                                                               
         B     DME60                                                            
         EJECT                                                                  
* NEW EXTENTS ON SAME VOLUME *                                                  
         SPACE 1                                                                
DME50    ZIC   R5,NEWXTNTS                                                      
         B     DME52PUT            SKIP OPEN FIRST TIME ONLY                    
*                                                                               
DME52    OPEN  ((2),OUTPUT)                                                     
         TM    48(R2),X'10'        TEST OPEN SUCCEEDED                          
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DME52PUT PUT   (2),NEWREC          WRITE A SINGLE RECORD                        
*                                                                               
         MVC   P(21),=C'AFTER  F...ING PUT'                                     
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,PRTDEB                                                        
*                                                                               
         CLOSE ((2),)                                                           
*                                                                               
         OPEN  ((2),OUTPUT)        REOPEN DCB                                   
         TM    48(R2),X'10'        TEST OPEN SUCCEEDED                          
         BO    *+6                                                              
         DC    H'0'                                                             
         MVC   8(4,R2),LASTXTNT+10 MOVE LAST CCHH TO DCB                        
         MVI   12(R2),99           FORCE HIGH REC                               
         XC    18(2,R2),18(R2)     CLEAR TRACK BALANCE                          
         CLOSE ((2),)                                                           
*                                                                               
         BCT   R5,DME52                                                         
         EJECT                                                                  
* ERASE FILE IF NECESSARY *                                                     
         SPACE 1                                                                
DME60    CLC   CARD+6(2),=C'NO'    TEST DO NOT ERASE                            
         BE    DMEOJ                                                            
         SPACE 1                                                                
* OPEN THE NEW FILE AS A DDS FILE *                                             
         SPACE 1                                                                
         LA    R3,ERSFILE                                                       
         ST    R3,P4                  SET DTF ADDRESS                           
         MVC   22(7,R3),=C'NEWFILE'   OVERRIDE FILE NAME                        
         GOTO1 =V(DADDS),P1,A(DAOPEN)                                           
         SPACE 1                                                                
* CALL DADDS TO ERASE *                                                         
         SPACE 1                                                                
         XC    P6,P6               SET TO ERASE WHOLE FILE                      
         CLC   CARD+6(3),=C'NEW'   TEST ERASE NEW EXTENT(S) ONLY                
         BNE   DME64               NO - GO ERASE ALL                            
* NEED TO CALL DADDS TO FORCE IT TO DO HITRK CALCULATION                        
         MVC   P6(4),=X'00010100'                                               
         GOTO1 =V(DADDS),P1,A(DATRNS)                                           
*                                                                               
         ZIC   R5,OLDXTNTS         GET NUMBER OF OLD EXTENTS                    
         LA    RE,DMTX-14                                                       
*                                                                               
DME62    LA    RE,14(RE)                                                        
         MVC   P6(2),12(RE)        MOVE HIGH TT                                 
         BCT   R5,DME62                                                         
*                                                                               
DME64    GOTO1 =V(DADDS),P1,A(WTERASE)                                          
*                                                                               
         GOTO1 (RF),(R1),A(DACLOSE)                                             
         SPACE 2                                                                
DMEOJ    XBASE                                                                  
         EJECT                                                                  
DMEFIND  BAS   RE,PRTDEB                                                        
         B     DMEOJ                                                            
         EJECT                                                                  
* THIS LOGIC TO SET HIGH WATER MARK FOR EXISTING FILES *                        
         SPACE 1                                                                
DMEFIX   BAS   RE,PRTDEB           SETS LASTXTNT                                
*                                                                               
         LA    R2,NEWFILE                                                       
         OPEN  ((R2),OUTPUT)                                                    
         TM    48(R2),X'10'                                                     
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DMEFIX1  L     R3,44(R2)           POINT TO DEB                                 
         ZIC   R0,16(R3)           GET NUMBER OF EXTENTS FROM DEB               
         LA    R3,32(R3)           POINT TO FIRST XTNT ENTRY                    
         B     DMEFIX1B                                                         
*                                                                               
DMEFIX1A LA    R3,16(R3)           POSITION TO LAST                             
*                                                                               
DMEFIX1B BCT   R0,DMEFIX1A                                                      
*                                                                               
DMEFIX2  CLC   1(3,R3),LASTXTNT+1  SAME VOLUME (UCB ADDRESS)                    
         BE    DMEFIX4                                                          
         FEOV  (2)                 TRY NEXT VOLUME                              
         B     DMEFIX1                                                          
*                                                                               
DMEFIX4  ZIC   R0,SAVEDEB+16       GET NUMBER OF EXTENTS                        
         SR    R1,R1               CLEAR COUNTER                                
         LA    RE,SAVEDEB+32       POINT TO FIRST EXTENT                        
*                                                                               
DMEFIX6  CLC   1(3,RE),17(RE)      MATCH NEXT UCB ADDRESS                       
         BE    *+8                 YES - DONT BUMP                              
         LA    R1,1(R1)            NO - BUMP COUNTER                            
         LA    RE,16(RE)           NEXT EXTENT ENTRY                            
         BCT   R0,DMEFIX6                                                       
*                                                                               
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,44(R2)   ****    POINT TO DEB                                 
         IC    R1,16(R1)   ****    GET NUMBER OF EXTENTS FROM DEB               
         BCTR  R1,0                                                             
         STC   R1,5(R2)            SET EXTENT SEQUENCE NUM                      
         MVC   8(4,R2),LASTXTNT+10  MOVE END CCHH TO DCB                        
         MVI   12(R2),99            SET HIGH RECORD NUM                         
         XC    18(2,R2),18(R2)      SET TRACK BALANCE TO 0                      
*                                                                               
         CLOSE ((2),)                                                           
         B     DMEOJ                                                            
         EJECT                                                                  
* SUBROUTINE TO PRINT DEB *                                                     
         SPACE 1                                                                
PRTDEB   NTR1                                                                   
*                                                                               
* FORMAT BASIC SECTION *                                                        
*                                                                               
*        LA    R3,DEBFILE                                                       
*        MVC   22(7,R3),=C'NEWFILE'  OVERRIDE FILE NAME                         
*        XC    WORK(16),WORK         USE WORK SO PARAM LIST UNTOUCHED           
*        GOTO1 =V(DADDS),WORK,A(DAOPEN),,,(R3)                                  
*                                                                               
*        L     R4,DTFADCB          GET DCB ADDRESS                              
         XC    DSPL,DSPL                                                        
         LA    R4,NEWFILE                                                       
         L     R4,44(R4)           GET DEB ADDRESS                              
         ZIC   RE,16(R4)           GET NUMBER OF EXTENTS                        
         SLL   RE,4                X 16                                         
         LA    RE,32(RE)                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SAVEDEB(0),0(R4)    SAVE ENTIRE DEB                              
*                                                                               
         LA    R0,2                                                             
*                                                                               
PRTDEB2  BAS   RE,HEXPRT                                                        
         LA    R4,16(R4)                                                        
         LH    RE,DSPL                                                          
         LA    RE,16(RE)                                                        
         STH   RE,DSPL                                                          
         BCT   R0,PRTDEB2                                                       
*                                                                               
*        L     R4,DTFADCB          GET DCB ADDRESS                              
         LA    R4,NEWFILE                                                       
         L     RE,44(R4)           GET DEB ADDRESS                              
         ZIC   R0,16(RE)           GET NUMBER OF EXTENTS                        
         LTR   R0,R0                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    R4,32(RE)           POINT TO FIRST EXTENT                        
*                                                                               
PRTDEB4  BAS   RE,HEXPRT                                                        
         MVC   LASTXTNT,0(R4)      SAVE FOR POSTERITY                           
*                                                                               
         LA    R4,16(R4)           NEXT EXTENT                                  
         LH    RE,DSPL                                                          
         LA    RE,16(RE)                                                        
         STH   RE,DSPL                                                          
         BCT   R0,PRTDEB4                                                       
*                                                                               
*        LA    R3,DEBFILE                                                       
*        XC    WORK(16),WORK       USE WORK SO PARAM LIST UNTOUCHED             
*        GOTO1 =V(DADDS),WORK,A(DACLOSE),,,(R3)                                 
*                                                                               
         GOTO1 =V(PRINTER)         SKIP A LINE                                  
         B     EXIT                                                             
*                                                                               
HEXPRT   NTR1                                                                   
         MVI   P,C'+'                                                           
         GOTO1 =V(HEXOUT),DMCB,DSPL+1,P+1,1,=C'MIX'                             
         GOTO1 =V(HEXOUT),DMCB,(R4),WORK,16,=C'MIX'                             
         MVC   P+4(8),WORK                                                      
         MVC   P+13(8),WORK+8                                                   
         MVC   P+22(8),WORK+16                                                  
         MVC   P+31(8),WORK+24                                                  
         GOTO1 =V(PRINTER)                                                      
EXIT     XIT1                                                                   
*                                                                               
DSPL     DC    H'0'                                                             
         EJECT                                                                  
* ERROR ROUTINES *                                                              
         SPACE 1                                                                
ACTNERR  MVC   ACTNMSG+L'ACTNMSG(12),CARD                                       
         LA    RE,ACTNMSGL                                                      
         LA    RF,ACTNMSG                                                       
         B     ERRX                                                             
*                                                                               
XTNTERR  MVC   XTNTMSG+L'XTNTMSG(12),CARD                                       
         LA    RE,XTNTMSGL                                                      
         LA    RF,XTNTMSG                                                       
         B     ERRX                                                             
*                                                                               
ERSERR   MVC   ERSMSG+L'ERSMSG(12),CARD                                         
         LA    RE,ERSMSGL                                                       
         LA    RF,ERSMSG                                                        
*                                                                               
ERRX     BCTR  RE,0                ADJUST FOR EX                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(RF)                                                       
         GOTO1 =V(PRINTER)                                                      
         B     DMEOJ                                                            
         SPACE 2                                                                
ACTNMSG  DC    C'** ERROR ** INVALID ACTION  '                                  
         DC    CL12' '                                                          
ACTNMSGL EQU   *-ACTNMSG                                                        
*                                                                               
XTNTMSG  DC    C'** ERROR ** INVALID NUMBER OF EXTENTS'                         
         DC    CL12' '                                                          
XTNTMSGL EQU   *-XTNTMSG                                                        
*                                                                               
ERSMSG   DC    C'** ERROR ** INVALID ERASE OPTION -'                            
         DC    CL12' '                                                          
ERSMSGL  EQU   *-ERSMSG                                                         
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*OLDFIL*'                                                      
OLDFILE  DMDA  DSKXTNT=16                                                       
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'*NEWFIL*'                                                      
NEWFILE  DCB   DDNAME=NEWFILE,DSORG=PS,MACRF=(PM),                     X        
               RECFM=FU,BLKSIZE=9500                                            
         SPACE 2                                                                
DEBFILE  DMDA  DSKXTNT=16                                                       
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'*ERSFIL*'                                                      
ERSFILE  DMDA  DSKXTNT=16                                                       
         SPACE 2                                                                
BLDXTNT  DS    0C                                                               
CARD     DC    CL80' '                                                          
ANSWER   DC    CL8' '                                                           
UPSI     DC    X'00'                                                            
TYPE     DC    X'00'                                                            
OLDXTNTS DC    X'00'                                                            
NEWXTNTS DC    X'00'                                                            
DEBXTNTS DC    X'00'                                                            
LASTXTNT DC    XL16'00'                                                         
         SPACE 2                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
ACMRG    DS    A                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
         DC    C'**PARM**'                                                      
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
SAVEDEB  DC    XL256'00'                                                        
         DC    16X'00'             PRECAUTION                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
NEWREC   DC    950CL10'THUNDERBAY'     1 REC/TRK ON 3350                        
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'**BUFF**'                                                      
DMEXBUFF DS    200D                                                             
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'**WORK**'                                                      
DMEXWORK DS    200D                                                             
         SPACE 2                                                                
DMEXLAST DS    D                                                                
         EJECT                                                                  
*DMDTFPH                                                                        
       ++INCLUDE DMDTFPH                                                        
         SPACE 2                                                                
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'083DMEXTENDO 05/01/02'                                      
         END                                                                    
