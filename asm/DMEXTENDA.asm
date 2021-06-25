*          DATA SET DMEXTENDA  AT LEVEL 039 AS OF 05/01/02                      
*PHASE DMEXTENA <<< AS OF 9/2/99 THIS IS THE VERSION US & UK                    
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
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         USING DMEXTEND+4096,R8                                                 
*                                                                               
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         MVC   TITLE(8),=CL8'DMEXTEND'                                          
*                                                                               
         BAS   RE,DFPLVL                                                        
*                                                                               
DME10    DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
         CLI   CARD,C'*'           TEST FOR A COMMENT                           
         BE    DME10                                                            
*                                                                               
         CLC   =C'DDSIO=',CARD     USE ALTERNATE DDSIO?                         
         BNE   *+18                                                             
         L     RF,=V(DDSIO)        YES                                          
         MVC   0(8,RF),CARD+6                                                   
         B     DME10                                                            
*                                                                               
         CLC   =C'FIND',CARD                                                    
         BE    DMEFIND                                                          
         CLC   =C'DISPLAY',CARD                                                 
         BE    DMEFIND                                                          
         CLC   =C'FIXOLD',CARD                                                  
         BE    DMEFIX                                                           
         CLC   =C'FIXNEW',CARD                                                  
         BE    DMEFIX                                                           
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
         GOTO1 =V(DATAMGR),P0,,DAOPEN                                           
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
         GOTO1 =V(DATAMGR),P0,,DACLOSE                                          
         SPACE 2                                                                
DME40    L     R5,AJFCBA                                                        
         ST    R5,EXITLST                                                       
         MVI   EXITLST,HIGHBIT+JFCBEXIT                                         
         LA    R2,NEWFILE                                                       
         PRINT GEN                                                              
         RDJFCB NEWFILE            GET JFCB DATA BEFORE OPEN (I.E. JCL)         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         PRINT NOGEN                                                            
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'JFCBMSG1),JFCBMSG1                                           
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,DISPJFCB         GO DISPLAY THE JFCB                          
         GOTO1 =V(PRINTER)         BLANK LINE                                   
*                                                                               
         OPEN  ((2),EXTEND)        OPEN NEW FILE AS MVS SEQUENTIAL              
         TM    48(R2),X'10'        TEST OPEN SUCCEEDED                          
         BO    *+6                                                              
         DC    H'0'                                                             
         CLI   SMSFLAG,C'A'        IS SMS ACTIVE ON THIS SYSTEM?                
         BNE   DME44               NO, SKIP GETTING INFO ON THE FILE            
         USING INFMJFCB,R5                                                      
         LA    R6,JFCBDSNM         GET ADDRESS OF DATASET NAME                  
         BAS   RE,DSNSMS                                                        
*                                                                               
DME44    TM    1(R4),X'40'         ARE NEW EXTENTS ON SAME VOL                  
         BO    DME50               YES                                          
         SPACE 1                                                                
* NEW EXTENTS ON NEW VOLUME - WRITE ONE RECORD FOR EACH EXTENT *                
         SPACE 1                                                                
         ZIC   R5,NEWXTNTS                                                      
         TM    1(R4),X'80'         TEST OLD EXTENT EXISTS                       
         BO    DME47               NO - SKIP FIRST FEOV                         
*                                                                               
DME46    FEOV  (2)                 FORCE VOLUME SWITCH                          
         BAS   RE,PRTDEB                                                        
*                                                                               
DME47    L     RF,ANEWREC                                                       
         PRINT GEN                                                              
         PUT   (2),(15)                                                         
         PRINT NOGEN                                                            
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
         CLOSE ((2),)                                                           
*                                                                               
         BAS   RE,PRTDEB                                                        
*                                                                               
         B     DME60                                                            
JFCBMSG1 DC    C'INFO FROM NEWFILE DD (BEFORE OPEN) FOLLOWS:'                   
JFCBMSG2 DC    C'INFO FROM NEWFILE DD (AFTER OPEN) FOLLOWS:'                    
         EJECT                                                                  
* NEW EXTENTS ON SAME VOLUME *                                                  
         SPACE 1                                                                
DME50    TM    JFCBIND2,JFCMOD     DOES JCL INDICATE DISP=MOD?                  
         BO    DME51               YES, GOOD                                    
         ABEND 21                  NO, TOO BAD WE'LL GET 614                    
*                                  IF WE CONTINUE                               
         DROP  R5                  DONE WITH JFCB                               
DME51    ZIC   R5,NEWXTNTS                                                      
         B     DME52PUT            SKIP OPEN FIRST TIME ONLY                    
*                                                                               
DME52    OPEN  ((2),OUTPUT)                                                     
         TM    48(R2),X'10'        TEST OPEN SUCCEEDED                          
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DME52PUT L     RF,ANEWREC          WRITE A SINGLE RECORD                        
         PUT   (2),(15)            WRITE A SINGLE RECORD                        
*                                                                               
         CLOSE ((2),)                                                           
*                                                                               
         BAS   RE,PRTDEB                                                        
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
         L     R3,AERSFILE                                                      
         ST    R3,P4                  SET DTF ADDRESS                           
         MVC   22(7,R3),=C'NEWFILE'   OVERRIDE FILE NAME                        
         GOTO1 =V(DATAMGR),P0,,DAOPEN                                           
         SPACE 1                                                                
* CALL DADDS TO ERASE *                                                         
         SPACE 1                                                                
         XC    P6,P6               SET TO ERASE WHOLE FILE                      
         CLC   CARD+6(3),=C'NEW'   TEST ERASE NEW EXTENT(S) ONLY                
         BNE   DME64               NO - GO ERASE ALL                            
* NEED TO CALL DADDS TO FORCE IT TO DO HITRK CALCULATION                        
         MVC   P6(4),=X'00010100'                                               
         GOTO1 =V(DATAMGR),P0,,DATRNS                                           
*                                                                               
         ZIC   R5,OLDXTNTS         GET NUMBER OF OLD EXTENTS                    
         MVC   P+1(28),=C'ERASE=NEW, NO OF OLD XTNTS= '                         
         STCM  R5,15,P+29                                                       
         GOTO1 =V(PRINTER)                                                      
         XC    P6(4),P6                                                         
         LA    RE,DMTX-14                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
*                                                                               
DME62    LA    RE,14(RE)                                                        
         ICM   R0,3,12(RE)                                                      
         AR    R1,R0                                                            
         BCT   R5,DME62                                                         
         STCM  R1,3,P6                                                          
         NI    DTFTYPE,X'FF'-DTFTBIG  UNSET BIG FILE FLAG                       
*                                                                               
         C     R1,=F'65535'        TEST BIG FILE                                
         BNH   DME63                                                            
         SLL   R1,32-18            SAVE 18 BITS OF ADDRESS                      
*                                                                               
         LR    R5,R1               SAVE P6                                      
         OI    DTFTYPE,DTFTBIG     AND INDICATE BIG FILE                        
*                                                                               
         MVC   P6(4),=X'00010100'  HAVE DADDS RECALCULATE                       
         GOTO1 =V(DATAMGR),P0,,DATRNS                                           
*                                                                               
         ST    R5,P6               AND SET IN P6                                
*                                                                               
DME63    MVC   P+1(24),=C'ERASE=NEW, HIGH TRACK = '                             
         MVC   P+25(2),P6                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
DME64    GOTO1 =V(DATAMGR),P0,,WTERASE                                          
*                                                                               
         GOTO1 =V(DATAMGR),P0,,DACLOSE                                          
         SPACE 2                                                                
DMEOJ    XBASE                                                                  
LINKERR  EQU   *                                                                
         GOTO1 =V(HEXOUT),DMCB,SMSLIST,WORK,16,=C'MIX'                          
         MVC   P+1(08),WORK                                                     
         MVC   P+10(08),WORK+8                                                  
         GOTO1 =V(PRINTER)                                                      
         ABEND 20                                                               
         EJECT                                                                  
DMEFIND  L     R5,AJFCBA                                                        
         ST    R5,EXITLST                                                       
         MVI   EXITLST,HIGHBIT+JFCBEXIT                                         
         LA    R2,NEWFILE                                                       
*                                                                               
*        OPEN  ((2),EXTEND)        OPEN NEW FILE AS MVS SEQUENTIAL              
*        TM    48(R2),X'10'        TEST OPEN SUCCEEDED                          
*        BO    *+6                                                              
*        DC    H'0'                                                             
         PRINT GEN                                                              
         RDJFCB NEWFILE            GET JFCB DATA AFTER OPEN                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         PRINT NOGEN                                                            
         GOTO1 =V(PRINTER)                                                      
         MVC   P(L'JFCBMSG2),JFCBMSG2                                           
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,DISPJFCB         GO DISPLAY THE JFCB                          
         GOTO1 =V(PRINTER)         BLANK LINE                                   
         CLI   SMSFLAG,C'A'        IS SMS ACTIVE ON THIS SYSTEM?                
         BNE   DME44               NO, SKIP GETTING INFO ON THE FILE            
         USING INFMJFCB,R5                                                      
         LA    R6,JFCBDSNM         GET ADDRESS OF DATASET NAME                  
         BAS   RE,DSNSMS                                                        
         DROP  R5                  DONE WITH JFCB                               
         BAS   RE,PRTDEB                                                        
         B     DMEOJ                                                            
         LTORG                                                                  
         EJECT                                                                  
* THIS LOGIC TO SET HIGH WATER MARK FOR EXISTING FILES *                        
         SPACE 1                                                                
DMEFIX   ABEND  21               NO-LONGER SUPPORTED                            
         EJECT                                                                  
**  SUBROUTINE TO DETERMINE THE DFP LEVEL OF THE SYSTEM AND IF                  
**  THE SMS SUBSYSTEM IS ACTIVE                                                 
DFPLVL   ST    RE,RETSAVE                                                       
         LA    R6,DFP                                                           
         ST    R6,LEVEL                                                         
         LINK  EP=IGWASYS,MF=(E,SYSLIST)                                        
         XC    RETCDE(8),RETCDE                                                 
         BNZ   LINKERR                                                          
         L     R7,SPECIND                                                       
         CH    R7,=H'1'            ARE WE ON A HIGHER LEVEL?                    
         BNE   DISPLVL             NO - SKIP GETTING IT'S LEVEL                 
         LA    R6,DFSMS                                                         
         ST    R6,LEVEL                                                         
         LINK  EP=IGWASYS,MF=(E,SYSLIST)                                        
         XC    RETCDE(8),RETCDE                                                 
         BNZ   LINKERR                                                          
         MVC   PRODNAME(09),=C'DFSMS/MVS'                                       
*                                                                               
DISPLVL  MVC   P+2(44),=C'THE DATA MANAGEMENT LEVEL OF THIS SYSTEM IS:'         
         MVC   P+47(10),PRODNAME                                                
         GOTO1 =V(HEXOUT),DMCB,SYSLVL+3,P+58,1,=C'MIX'                          
         GOTO1 =V(HEXOUT),DMCB,SYSLVL+7,P+60,1,=C'MIX'                          
         GOTO1 =V(HEXOUT),DMCB,SYSLVL+11,P+62,1,=C'MIX'                         
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)         BLANK LINE                                   
*                                                                               
DISPSMS  L     R7,SYSATTR                                                       
         MVC   P+2(06),=C'SMS IS'                                               
         CH    R7,=H'1'            IS SMS ACTIVE?                               
         BNE   NOSMS                                                            
         MVC   P+9(06),=C'ACTIVE'                                               
         MVI   SMSFLAG,C'A'                                                     
         B     PRTSMS                                                           
NOSMS    MVC   P+9(10),=C'NOT ACTIVE'                                           
         MVI   SMSFLAG,C'N'                                                     
PRTSMS   GOTO1 =V(PRINTER)                                                      
         L     RE,RETSAVE                                                       
         BR    RE                                                               
         EJECT                                                                  
**  SUBROUTINE TO GET SMS INFO FOR A SPECIFIC DATASET NAME, THE ADDRESS         
**  OF WHICH IS PASSED IN REG 6                                                 
DSNSMS   ST    RE,RETSAVE                                                       
         ST    R6,DSNADDR                                                       
         LINK  EP=IGWASMS,MF=(E,SMSLIST)                                        
         LTR   RF,RF                                                            
         BZ    DSNSMS1                                                          
         LA    RE,12                                                            
         CR    RF,RE                                                            
         BNE   LINKERR                                                          
         MVC   P(L'SMSMSG1),SMSMSG1                                             
         GOTO1 =V(PRINTER)                                                      
         B     DSNSMSE                                                          
DSNSMS1  MVC   P(L'SMSMSG2),SMSMSG2                                             
         GOTO1 =V(PRINTER)                                                      
         MVC   P+3(16),=C'STORAGE CLASS =>'                                     
         MVC   P+20(30),SMSDATA                                                 
         GOTO1 =V(PRINTER)                                                      
         MVC   P(19),=C'MANAGEMENT CLASS =>'                                    
         MVC   P+20(30),SMSDATA+30                                              
         GOTO1 =V(PRINTER)                                                      
         MVC   P+6(13),=C'DATA CLASS =>'                                        
         MVC   P+20(30),SMSDATA+60                                              
         GOTO1 =V(PRINTER)         BLANK LINE                                   
DSNSMSE  L     RE,RETSAVE                                                       
         BR    RE                                                               
         EJECT                                                                  
**  SUBROUTINE TO DISPLAY PERTINENT FIELDS FROM THE JFCB                        
**  ADDRESS OF JFCB IS PASSED IN R5                                             
DISPJFCB ST    RE,RETSAVE                                                       
         USING INFMJFCB,R5                                                      
         IC    R6,JFCBNVOL         GET NUMBER OF VOLUMES                        
***                                ONLY 5 VOL SERS CAN BE IN JFCB               
***                                REMAINDER MUST BE IN EXTENSIONS              
         MVC   P+2(15),=C'DATASET NAME =>'                                      
         MVC   P+18(44),JFCBDSNM    GET DATASET NAME                            
         GOTO1 =V(PRINTER)                                                      
         MVC   P+3(14),=C'STATUS BITS =>'                                       
         GOTO1 =V(HEXOUT),DMCB,JFCBIND2,P+18,1,=C'MIX'                          
         TM    JFCBIND2,JFCNEW                                                  
         BNO   TESTMOD                                                          
         MVC   P+25(3),=C'NEW'                                                  
         B     PRTSTAT                                                          
TESTMOD  TM    JFCBIND2,JFCMOD                                                  
         BNO   TESTOLD                                                          
         MVC   P+25(3),=C'MOD'                                                  
         B     PRTSTAT                                                          
TESTOLD  TM    JFCBIND2,JFCOLD                                                  
         MVC   P+25(3),=C'OLD'              WHAT ELSE COULD IT BE?              
PRTSTAT  GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P+5(12),=C'VOLSER(S) =>'                                         
         MVC   P+18(30),JFCBVOLS   5 VOL SERS                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P+5(12),=C'VOL COUNT =>'                                         
         GOTO1 =V(HEXOUT),DMCB,JFCBVLCT,P+18,1,=C'MIX'                          
         GOTO1 =V(PRINTER)                                                      
         MVC   P+5(12),=C'SPACE VAL =>'                                         
         GOTO1 =V(HEXOUT),DMCB,JFCBPQTY,P+18,3,=C'MIX'                          
         MVI   P+24,C','                                                        
         GOTO1 =V(HEXOUT),DMCB,JFCBSQTY,P+26,3,=C'MIX'                          
         MVC   P+35(13),=C'VALUES IN HEX'                                       
         GOTO1 =V(PRINTER)                                                      
         L     RE,RETSAVE                                                       
         BR    RE                                                               
         DROP  R5                  DONE WITH JFCB                               
         EJECT                                                                  
* SUBROUTINE TO PRINT DEB *                                                     
         SPACE 1                                                                
PRTDEB   NTR1                                                                   
*                                                                               
* FORMAT BASIC SECTION *                                                        
*                                                                               
         LA    R3,DEBFILE                                                       
         MVC   22(7,R3),=C'NEWFILE'  OVERRIDE FILE NAME                         
         XC    WORK(20),WORK         USE WORK SO PARAM LIST UNTOUCHED           
         GOTO1 =V(DATAMGR),WORK,DADDS,DAOPEN,,,(R3)                             
*                                                                               
ENDIN    XC    DSPL,DSPL                                                        
         L     R4,DTFADCB          GET DCB ADDRESS                              
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
         GOTO1 =V(PRINTER)         SKIP A LINE                                  
         MVC   P(26),=C'DEB AS USED BY DATAMANAGER'                             
         GOTO1 =V(PRINTER)         SKIP A LINE                                  
PRTDEB2  BAS   RE,HEXPRT           PRINT BEGINNING PART OF DEB                  
         LA    R4,16(R4)                                                        
         LH    RE,DSPL                                                          
         LA    RE,16(RE)                                                        
         STH   RE,DSPL                                                          
         BCT   R0,PRTDEB2                                                       
         GOTO1 =V(PRINTER)         SKIP A LINE                                  
         MVC   P+10(L'DEBMSG1),DEBMSG1                                          
         GOTO1 =V(PRINTER)         PRINT A LINE                                 
         MVC   P+10(L'DEBMSG2),DEBMSG2                                          
         GOTO1 =V(PRINTER)         PRINT A LINE                                 
*                                                                               
         L     R4,DTFADCB          GET DCB ADDRESS                              
         L     RE,44(R4)           GET DEB ADDRESS                              
         USING DEBBASIC,R4                                                      
         ZIC   R0,16(RE)           GET NUMBER OF EXTENTS                        
         LTR   R0,R0                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1               CLEAR COUNTER                                
         LA    R4,32(RE)           POINT TO FIRST EXTENT                        
         USING DEBDASD,R4          MAP THE EXTENT AREA                          
*        DEBSTRCC                  STARTING CYL  (H)                            
*        DEBSTRHH                                                               
*        DEBENDCC                  ENDING CYL (H)                               
*        DEBENDHH                                                               
*        DEBNMTRK                  H   NUMBER OF TRACKS ALLOCATED               
*                                                                               
PRTDEB4  SR    R9,R9                                                            
         ICM   R9,7,DEBUCBA        GET THE UCB ADDRESS                          
**                 UCBNAME         DEVICE NUMBER                                
         BAS   RE,HEXPRTX          PRINT AN EXTENT                              
         MVC   LASTXTNT,0(R4)      SAVE FOR POSTERITY                           
         AH    R1,DEBNMTRK         ADD UP THE NUMBER OF TRACKS                  
*                                                                               
         LA    R4,16(R4)           NEXT EXTENT                                  
         LH    RE,DSPL                                                          
         LA    RE,16(RE)                                                        
         STH   RE,DSPL                                                          
         BCT   R0,PRTDEB4                                                       
         DROP  R4                                                               
         ST    R1,TRKSUSED                                                      
         EDIT  (R1),(10,SUMTRK),0,COMMAS=YES                                    
         MVC   P+39(7),=C'======='                                              
         GOTO1 =V(PRINTER)                                                      
*NOP     GOTO1 =V(HEXOUT),DMCB,TRKSUSED,P+38,4,=C'MIX'                          
         MVC   P+36(10),SUMTRK     DECIMAL TOTAL                                
         GOTO1 =V(PRINTER)         OUTPUT TOTAL TRACKS                          
         C     R1,LIMIT                                                         
         BNH   HEXDONE                                                          
         MVC   P+5(35),=C'****** 65K TRACK LIMIT EXCEEDED ***'                  
         GOTO1 =V(PRINTER)         OUTPUT TOTAL TRACKS                          
*                                                                               
HEXDONE  LA    R3,DEBFILE                                                       
         XC    WORK(20),WORK       USE WORK SO PARAM LIST UNTOUCHED             
         GOTO1 =V(DATAMGR),WORK,DADDS,DACLOSE,,,(R3)                            
*                                                                               
         GOTO1 =V(PRINTER)         SKIP A LINE                                  
         B     EXIT                                                             
TRKSUSED DS    F                                                                
LIMIT    DC    F'65520'            MAX SIZE OF HALF WORD W/O SIGN               
*                                  NOW MINUS 1 CYLINDER                         
SUMTRK   DS    CL10                                                             
DEBMSG1  DC    C'UCB        START    END         TRK '                          
DEBMSG2  DC    C'ADDR       CC/HH    CC/HH      COUNT'                          
SMSMSG1  DC    C'DATASET NOT FOUND IN CATALOG; NOT SMS MANAGED'                 
SMSMSG2  DC    C'ASSOCIATED SMS INFO FOR DSN:'                                  
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
HEXPRTX  NTR1                                                                   
         MVI   P,C'+'                                                           
         GOTO1 =V(HEXOUT),DMCB,DSPL+1,P+1,1,=C'MIX'                             
         GOTO1 =V(HEXOUT),DMCB,(R4),WORK,16,=C'MIX'                             
         MVC   P+4(2),WORK                                                      
         MVC   P+8(7),WORK+2           UCB ADDRESS                              
         MVC   P+16(4),WORK+8          ZERO AREA                                
         MVC   P+21(8),WORK+12         STARTING CC/HH                           
         MVC   P+30(8),WORK+20         ENDING CC/HH                             
         MVC   P+42(4),WORK+28         TOTAL TRACKS IN EXTENT(HEX)              
         USING UCBOB,R9                                                         
         PRINT GEN                                                              
         UCBDEVN DEVN=CHAN                                                      
         PRINT NOGEN                                                            
         MVC   P+50(4),CHAN            DEVICE NUMBER                            
         MVC   P+56(6),UCBVOLI         VOL SER                                  
         GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
         DROP  R9                                                               
*                                                                               
DSPL     DC    H'0'                                                             
CHAN     DS    F                                                                
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
NEWFILE  DCB   DDNAME=NEWFILE,DSORG=PS,MACRF=(PM),EXLST=(EXITLST),     X        
               RECFM=FU,BLKSIZE=9500                                            
EXITLST  DS    2F                                                               
AJFCBA   DC    A(JFCBAREA)                                                      
ANEWREC  DC    A(NEWREC)                                                        
AERSFILE DC    A(ERSFILE)                                                       
SYSLIST  DC    A(RETCDE)                                                        
         DC    A(REASCDE)                                                       
         DC    A(LEVEL)                                                         
         DC    A(SYSLVL)                                                        
         DC    A(SYSATTR)                                                       
LEVEL    DS    F                                                                
DFSMS    EQU   2                   LEVEL INDICATOR FOR DFSMS PRODUCT            
DFP      EQU   1                                                                
SYSLVL   DS    4F                                                               
         ORG   SYSLVL                                                           
SMSVER   DS    1F                                                               
SMSREL   DS    1F                                                               
SMSMOD   DS    1F                                                               
SPECIND  DS    1F                                                               
*                                                                               
SYSATTR  DS    4F                                                               
PRODNAME DC    CL10'MVS/DFP'                                                    
*                                                                               
SMSLIST  DC    A(RETCDE)                                                        
         DC    A(REASCDE)                                                       
         DC    A(PROB1)                                                         
         DC    A(DSNLEN)                                                        
DSNADDR  DS    A                                                                
         DC    A(SMSDATA)                                                       
         DC    A(DSTYPE)                                                        
RETCDE   DC    F'0'                                                             
REASCDE  DC    F'0'                                                             
PROB1    DC    2F'0'                                                            
DSNLEN   DC    F'44'                                                            
DSTYPE   DC    F'0'                                                             
SMSDATA  DC    3CL30' '                                                         
         DC    C'*JFCB***'                                                      
HIGHBIT  EQU   X'80'                                                            
JFCBEXIT EQU   X'07'               DCB EXITLIST CODE VALUE (ORIGINAL)           
         SPACE 2                                                                
DEBFILE  DMDA  DSKXTNT=16                                                       
         SPACE 2                                                                
BLDXTNT  DS    0C                                                               
CARD     DC    CL80' '                                                          
ANSWER   DC    CL8' '                                                           
SMSFLAG  DC    X'00'                                                            
UPSI     DC    X'00'                                                            
TYPE     DC    X'00'                                                            
OLDXTNTS DC    X'00'                                                            
NEWXTNTS DC    X'00'                                                            
DEBXTNTS DC    X'00'                                                            
LASTXTNT DC    XL16'00'                                                         
         SPACE 2                                                                
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         SPACE 3                                                                
UTL      DC    F'0',X'0A'          FOR DATAMGR (CONTROL SYSTEM)                 
         SPACE 2                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
RETSAVE  DS    F                                                                
DADDS    DC    C'DADDS'                                                         
         DC    C'**PARM**'                                                      
P0       DC    A(DADDS)            P1 TO DATAMGR IS =C'DADDS'                   
P1       DS    F                   P1 TO DADDS, P2 TO DATAMGR                   
P2       DS    F                   P2 TO DADDS, P3 TO DATAMGR                   
P3       DS    F                   P3 TO DADDS, P4 TO DATAMGR                   
P4       DS    F                   P4 TO DADDS, P5 TO DATAMGR                   
P5       DS    F                   P5 TO DADDS, P6 TO DATAMGR                   
P6       DS    F                   P6 TO DADDS, P7 TO DATAMGR                   
         LTORG                                                                  
SAVEDEB  DC    XL256'00'                                                        
         DC    16X'00'             PRECAUTION                                   
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    C'*ERSFIL*'                                                      
ERSFILE  DMDA  DSKXTNT=16                                                       
         SPACE 2                                                                
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
JFCBAREA DS    176X                                                             
         EJECT                                                                  
*DMDTFPH                                                                        
       ++INCLUDE DMDTFPH                                                        
         SPACE 2                                                                
       ++INCLUDE DDDPRINT                                                       
         SPACE 1                                                                
         PRINT   GEN                                                            
         IEFJFCBN   LIST=YES                                                    
         IEZDEB     LIST=NO                                                     
         DSECT                                                                  
         IEFUCBOB   DEVCLAS=DA                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039DMEXTENDA 05/01/02'                                      
         END                                                                    
