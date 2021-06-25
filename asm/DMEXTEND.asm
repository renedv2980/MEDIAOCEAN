*          DATA SET DMEXTEND   AT LEVEL 003 AS OF 12/03/13                      
*PROCESS USING(WARN(15))                                                        
*PHASE DMEXTENA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE SCANNER                                                                
         TITLE 'DMEXTEND - EXTEND AND ERASE A DIRECT ACCESS FILE'               
         PRINT NOGEN                                                            
DMEXTEND CSECT                                                                  
         NBASE 0,DMEXTEND,R8,WORK=A(DMEXWORK)                                   
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
         MVC   TITLE(8),=CL8'DMEXTEND'                                          
*                                                                               
         BAS   RE,DFPLVL                                                        
*                                                                               
DME02    GOTO1 VCARDS,DMCB,CARD,=C'RE00'                                        
         MVC   P(80),CARD                                                       
         GOTO1 VPRINTER                                                         
         CLI   CARD,C'*'           TEST FOR A COMMENT                           
         BE    DME02                                                            
*                                                                               
DME04    CLC   CARD(6),=C'DDSIO='  TEST IF DDSIO SPECIFIED                      
         BNE   DME06                                                            
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6                                                   
         B     DME02                                                            
*                                                                               
DME06    CLC   CARD(7),=C'DSPACE=' TEST IF DSPACE SPECIFIED                     
         BNE   DME08                                                            
         L     RF,=V(SSB)                                                       
         MVC   SSODSPAC-SSBOFFD(1,RF),CARD+7                                    
         B     DME02                                                            
*                                                                               
DME08    CLC   CARD(4),=C'FIND'                                                 
         BE    DMEFIND                                                          
         CLC   CARD(7),=C'DISPLAY'                                              
         BE    DMEFIND                                                          
         CLC   CARD(6),=C'FIXOLD'                                               
         BE    DMEFIX                                                           
         CLC   CARD(6),=C'FIXNEW'                                               
         BE    DMEFIX                                                           
*                                                                               
         GOTO1 VSCANNER,PLIST,(C'C',CARD),WORK,0,0                              
         CLI   4(R1),1                                                          
         BNE   ACTNERR                                                          
         LA    RF,WORK                                                          
         USING SCANBLKD,RF                                                      
*                                                                               
         XR    RE,RE                                                            
         LA    R4,ACTTAB                                                        
DME12    IC    RE,SC1STLEN                                                      
         CLM   RE,1,0(R4)          TEST EQUAL NUMBER OF CHARACTERS              
         BNE   DME20                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BE    DME30                                                            
         CLC   SC1STFLD(0),2(R4)   EXECUTED                                     
*                                                                               
DME20    AHI   R4,L'ACTTAB         POINT TO NEXT ENTRY                          
         CLI   0(R4),X'FF'         TEST EOL                                     
         BNE   DME12                                                            
         B     ACTNERR                                                          
         EJECT                                                                  
***********************************************************************         
* TABLE ENTRIES ARE AS FOLLOWS -                                      *         
* LENGTH FOR ACTION NAME COMPARE                                      *         
* ACTION DEFINITION BITS  X'80' = NO OLD FILE                         *         
*                         X'40' = EXTEND ON SAME VOLUME               *         
* ACTION NAME IN CHARACTERS                                           *         
***********************************************************************         
ACTTAB   DS    0XL10                                                            
         DC    AL1(6),X'80'                                                     
ACT1NAME DC    CL8'DEFINE'                                                      
         DC    AL1(6),X'C0'                                                     
ACT2NAME DC    CL8'DEFVOL'                                                      
         DC    AL1(6),X'00'                                                     
ACT3NAME DC    CL8'EXTEND'                                                      
         DC    AL1(6),X'40'                                                     
ACT4NAME DC    CL8'EXTVOL'                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS KEYWORD= PARAMETERS                                         *         
***********************************************************************         
DME30    MVI   NEWXTNTS,1          SET DEFAULT NUMBER                           
         CLI   SC2NDLEN,0                                                       
         BE    DME32                                                            
         TM    SC2NDVAL,SCNUMQ                                                  
         BZ    XTNTERR                                                          
*                                                                               
         L     RE,SC2NDNUM                                                      
         CHI   RE,1                                                             
         BL    XTNTERR                                                          
         CHI   RE,48                                                            
         BH    XTNTERR                                                          
         STC   RE,NEWXTNTS                                                      
*                                                                               
DME32    DS    0H                                                               
         GOTO1 VCARDS,DMCB,CARD,=C'RE00'                                        
         MVC   P(80),CARD                                                       
         GOTO1 VPRINTER                                                         
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
DME32X   LA    R3,OLDFILE          POINT TO OLD FILE DTF                        
         USING DTFPHD,R3                                                        
         XC    P1(24),P1           SET DADDS PARAM LIST                         
         L     RE,=A(DMEXBUFF)                                                  
         ST    RE,P2                                                            
         MVI   P2,X'FF'                                                         
         ST    R3,P4                                                            
         LA    RE,DISKADR                                                       
         ST    RE,P5                                                            
         LA    RE,DUB                                                           
         ST    RE,P6                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS OLD EXTENT                                                  *         
***********************************************************************         
DME34    TM    1(R4),X'80'         TEST OLD EXTENT EXISTS                       
         BO    DME40               NO                                           
         GOTO1 VDMGR,P0,,DAOPEN                                                 
*                                                                               
         SR    R1,R1                                                            
         LA    RE,DMTX                                                          
         USING EXTENTD,RE                                                       
         TM    DIND,DINDXAM        HIGH CORE EXTENT                             
         BZ    *+8                                                              
         ICM   RE,15,DMTX                                                       
         SAM31                                                                  
DME36    CLI   EXTENTD,X'FF'       COUNT NUMBER OF EXISTING EXTENTS             
         BE    DME38                                                            
         LA    R1,1(R1)                                                         
         LA    RE,EXTLNQ(RE)                                                    
         B     DME36                                                            
         DROP  RE                                                               
*                                                                               
DME38    SAM24                                                                  
         STC   R1,OLDXTNTS                                                      
         GOTO1 VDMGR,P0,,DACLOSE                                                
*                                                                               
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
         GOTO1 VPRINTER                                                         
         MVC   P(L'JFCBMSG1),JFCBMSG1                                           
         GOTO1 VPRINTER                                                         
         BAS   RE,DISPJFCB         GO DISPLAY THE JFCB                          
         GOTO1 VPRINTER                                                         
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
                                                                                
***********************************************************************         
* NEW EXTENTS ON NEW VOLUME - WRITE ONE RECORD FOR EACH EXTENT        *         
***********************************************************************         
         LLC   R5,NEWXTNTS                                                      
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
                                                                                
***********************************************************************         
* FORCE END OF FILE TO END CCHH ON LAST EXTENT                        *         
***********************************************************************         
         MVC   NEWFILE+8(4),10(RF) MOVE END CCHH TO DCB                         
         MVI   NEWFILE+12,99       SET HIGH RECORD NUM                          
         MVI   NEWFILE+18,X'00'    SET TRACK BALANCE TO 0                       
         MVI   NEWFILE+19,X'00'    SET TRACK BALANCE TO 0                       
         BCT   R5,DME46                                                         
*                                                                               
         CLOSE ((2),)                                                           
         BAS   RE,PRTDEB                                                        
         B     DME60                                                            
*                                                                               
JFCBMSG1 DC    C'INFO FROM NEWFILE DD (BEFORE OPEN) FOLLOWS:'                   
JFCBMSG2 DC    C'INFO FROM NEWFILE DD (AFTER OPEN) FOLLOWS:'                    
         EJECT                                                                  
***********************************************************************         
* NEW EXTENTS ON SAME VOLUME                                          *         
***********************************************************************         
DME50    TM    JFCBIND2,JFCMOD     DOES JCL INDICATE DISP=MOD?                  
         BO    DME51               YES, GOOD                                    
         ABEND 21                  NO, TOO BAD WE'LL GET 614 ABEND              
         DROP  R5                  DONE WITH JFCB                               
DME51    LLC   R5,NEWXTNTS                                                      
         B     DME52PUT            SKIP OPEN FIRST TIME ONLY                    
*                                                                               
DME52    OPEN  ((2),OUTPUT)                                                     
         TM    48(R2),X'10'        TEST OPEN SUCCEEDED                          
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DME52PUT L     RF,ANEWREC          WRITE A SINGLE RECORD                        
         PUT   (2),(15)                                                         
         CLOSE ((2),)                                                           
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
***********************************************************************         
* ERASE FILE IF NECESSARY                                             *         
***********************************************************************         
DME60    CLC   CARD+6(2),=C'NO'    TEST DO NOT ERASE                            
         BE    DMEOJ                                                            
         L     R3,AERSFILE         OPEN THE NEW FILE AS A DDS FILE              
         ST    R3,P4               SET DTF ADDRESS                              
         MVC   22(8,R3),=C'NEWFILE '  OVERRIDE FILE NAME                        
         GOTO1 VDMGR,P0,,DAOPEN                                                 
*                                  CALL DADDS TO ERASE                          
         SR    R1,R1               SET TO ERASE WHOLE FILE                      
         CLC   CARD+6(3),=C'NEW'   TEST ERASE NEW EXTENT(S) ONLY                
         BNE   DME64               NO - GO ERASE ALL                            
         MVC   DISKADR,FRSTADR     SET 22-BIT ADDRESS OF FIRST RECORD           
*                                                                               
         GOTO1 VDMGR,P0,,DATRNS    CALLS DADDS TO DO HITRK CALCULATION          
*                                                                               
         LLC   R1,OLDXTNTS         GET NUMBER OF OLD EXTENTS                    
         MVC   P+1(28),=C'ERASE=NEW, NO OF OLD XTNTS= '                         
         EDIT  (R1),(3,P+28),ALIGN=LEFT                                         
         GOTO1 VPRINTER                                                         
                                                                                
         XC    DISKADR,DISKADR                                                  
         LLC   R5,OLDXTNTS         NUMBER OF EXTENTS ON OLD FILE                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         LA    RE,DMTX                                                          
         USING EXTENTD,RE                                                       
         TM    DIND,DINDXAM        TEST HIGH CORE EXTENT                        
         BZ    *+8                                                              
         ICM   RE,15,DMTX                                                       
         SAM31                                                                  
DME62    ICM   R0,3,EXT#TRKS                                                    
         LA    RE,EXTLNQ(RE)                                                    
         AR    R1,R0               ADD THEM UP FOR TOTAL TRACKS                 
         BCT   R5,DME62                                                         
         SAM24                                                                  
         DROP  RE                                                               
*                                                                               
DME64    LR    R0,R1               CONVERT TO 22-BIT DISK ADDRESS               
         SLL   R0,32-22                                                         
         ST    R0,DISKADR          P5=A(DISKADR)                                
*                                                                               
DME66    MVC   P+1(23),=C'ERASE=NEW, HIGH TRACK= '                              
         CLC   CARD+6(3),=C'NEW'   TEST ERASE NEW EXTENT(S) ONLY                
         BE    *+10                                                             
         MVC   P+1(9),=C'ERASE=ALL'                                             
         EDIT  (R1),(8,P+23),ALIGN=LEFT                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 VDMGR,P0,,WTERASE                                                
*                                                                               
         GOTO1 VDMGR,P0,,DACLOSE                                                
         B     DMEOJ                                                            
         EJECT                                                                  
***********************************************************************         
* LINK ERROR                                                          *         
***********************************************************************         
LINKERR  EQU   *                                                                
         GOTO1 VHEXOUT,DMCB,SMSLIST,WORK,16,=C'MIX'                             
         MVC   P+1(08),WORK                                                     
         MVC   P+10(08),WORK+8                                                  
         GOTO1 VPRINTER                                                         
         ABEND 20                                                               
         EJECT                                                                  
***********************************************************************         
* FIND AND DISPLAY ACTIONS                                            *         
***********************************************************************         
DMEFIND  L     R5,AJFCBA                                                        
         ST    R5,EXITLST                                                       
         MVI   EXITLST,HIGHBIT+JFCBEXIT                                         
         LA    R2,NEWFILE                                                       
*&&DO                                                                           
         OPEN  ((2),EXTEND)        OPEN NEW FILE AS MVS SEQUENTIAL              
         TM    48(R2),X'10'        TEST OPEN SUCCEEDED                          
         BO    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         PRINT GEN                                                              
         RDJFCB NEWFILE            GET JFCB DATA AFTER OPEN                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         PRINT NOGEN                                                            
         GOTO1 VPRINTER                                                         
         MVC   P(L'JFCBMSG2),JFCBMSG2                                           
         GOTO1 VPRINTER                                                         
         BAS   RE,DISPJFCB         GO DISPLAY THE JFCB                          
         GOTO1 VPRINTER                                                         
         CLI   SMSFLAG,C'A'        IS SMS ACTIVE ON THIS SYSTEM?                
         BNE   DME44               NO, SKIP GETTING INFO ON THE FILE            
         USING INFMJFCB,R5                                                      
         LA    R6,JFCBDSNM         GET ADDRESS OF DATASET NAME                  
         BAS   RE,DSNSMS                                                        
         DROP  R5                  DONE WITH JFCB                               
         BAS   RE,PRTDEB                                                        
         B     DMEOJ                                                            
         EJECT                                                                  
***********************************************************************         
* FIXOLD AND FIXNEW ACTIONS                                           *         
***********************************************************************         
DMEFIX   ABEND  21               NO-LONGER SUPPORTED                            
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO DETERMINE THE DFP LEVEL OF THE SYSTEM AND IF          *         
* THE SMS SUBSYSTEM IS ACTIVE                                         *         
***********************************************************************         
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
         GOTO1 VHEXOUT,DMCB,SYSLVL+3,P+58,1,=C'MIX'                             
         GOTO1 VHEXOUT,DMCB,SYSLVL+7,P+60,1,=C'MIX'                             
         GOTO1 VHEXOUT,DMCB,SYSLVL+11,P+62,1,=C'MIX'                            
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
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
PRTSMS   GOTO1 VPRINTER                                                         
         L     RE,RETSAVE                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO GET SMS INFO FOR A SPECIFIC DATASET NAME, THE ADDRESS *         
* OF WHICH IS PASSED IN REG 6                                         *         
***********************************************************************         
DSNSMS   ST    RE,RETSAVE                                                       
         ST    R6,DSNADDR                                                       
         LINK  EP=IGWASMS,MF=(E,SMSLIST)                                        
         LTR   RF,RF                                                            
         BZ    DSNSMS1         BRANCH ON GOOD RESULTS                           
         LA    RE,12                                                            
         CR    RF,RE                                                            
         BE    DSNSMS1A        IF RF IS X'C' DSN NOT FOUND                      
         LA    RE,16                                                            
         CR    RF,RE           IF RF IS X'10' DSN ALSO LIKELY NOT FOUND         
         BNE   LINKERR                                                          
DSNSMS1A MVC   P(L'SMSMSG1),SMSMSG1                                             
         GOTO1 VPRINTER                                                         
         B     DSNSMSE                                                          
DSNSMS1  MVC   P(L'SMSMSG2),SMSMSG2                                             
         GOTO1 VPRINTER                                                         
         MVC   P+3(16),=C'STORAGE CLASS =>'                                     
         MVC   P+20(30),SMSDATA                                                 
         GOTO1 VPRINTER                                                         
         MVC   P(19),=C'MANAGEMENT CLASS =>'                                    
         MVC   P+20(30),SMSDATA+30                                              
         GOTO1 VPRINTER                                                         
         MVC   P+6(13),=C'DATA CLASS =>'                                        
         MVC   P+20(30),SMSDATA+60                                              
         GOTO1 VPRINTER                                                         
DSNSMSE  L     RE,RETSAVE                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO DISPLAY PERTINENT FIELDS FROM THE JFCB                *         
* ADDRESS OF JFCB IS PASSED IN R5                                     *         
* ONLY 5 VOLSERS CAN BE IN JFCB - REST MUST BE IN EXTENSIONS          *         
***********************************************************************         
DISPJFCB ST    RE,RETSAVE                                                       
         USING INFMJFCB,R5                                                      
         IC    R6,JFCBNVOL         GET NUMBER OF VOLUMES                        
         MVC   P+2(15),=C'DATASET NAME =>'                                      
         MVC   P+18(44),JFCBDSNM   GET DATASET NAME                             
         GOTO1 VPRINTER                                                         
         MVC   P+3(14),=C'STATUS BITS =>'                                       
         GOTO1 VHEXOUT,DMCB,JFCBIND2,P+18,1,=C'MIX'                             
         TM    JFCBIND2,JFCNEW                                                  
         BNO   TESTMOD                                                          
         MVC   P+25(3),=C'NEW'                                                  
         B     PRTSTAT                                                          
TESTMOD  TM    JFCBIND2,JFCMOD                                                  
         BNO   TESTOLD                                                          
         MVC   P+25(3),=C'MOD'                                                  
         B     PRTSTAT                                                          
TESTOLD  TM    JFCBIND2,JFCOLD                                                  
         MVC   P+25(3),=C'OLD'     WHAT ELSE COULD IT BE?                       
PRTSTAT  GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+5(12),=C'VOLSER(S) =>'                                         
         MVC   P+18(30),JFCBVOLS   5 VOL SERS                                   
         GOTO1 VPRINTER                                                         
         MVC   P+5(12),=C'VOL COUNT =>'                                         
         GOTO1 VHEXOUT,DMCB,JFCBVLCT,P+18,1,=C'MIX'                             
         GOTO1 VPRINTER                                                         
         MVC   P+5(12),=C'SPACE VAL =>'                                         
         GOTO1 VHEXOUT,DMCB,JFCBPQTY,P+18,3,=C'MIX'                             
         MVI   P+24,C','                                                        
         GOTO1 VHEXOUT,DMCB,JFCBSQTY,P+26,3,=C'MIX'                             
         MVC   P+35(13),=C'VALUES IN HEX'                                       
         GOTO1 VPRINTER                                                         
         L     RE,RETSAVE                                                       
         BR    RE                                                               
         DROP  R5                  DONE WITH JFCB                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO PRINT DEB                                             *         
***********************************************************************         
PRTDEB   NTR1                                                                   
         LA    R3,DEBFILE                                                       
         MVC   22(8,R3),=C'NEWFILE '  OVERRIDE FILE NAME                        
         XC    WORK(20),WORK          USE WORK SO PARAM LIST UNTOUCHED          
         GOTO1 VDMGR,WORK,DADDS,DAOPEN,,,(R3)                                   
*                                                                               
ENDIN    XC    DSPL,DSPL                                                        
         L     R4,DTFADCB          GET DCB ADDRESS                              
         L     R4,44(R4)           GET DEB ADDRESS                              
         LLC   RE,16(R4)           GET NUMBER OF EXTENTS                        
         SLL   RE,4                X 16                                         
         LA    RE,32(RE)                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SAVEDEB(0),0(R4)    SAVE ENTIRE DEB                              
*                                                                               
         LA    R0,2                                                             
*                                                                               
         GOTO1 VPRINTER                                                         
         MVC   P(26),=C'DEB AS USED BY DATAMANAGER'                             
         GOTO1 VPRINTER                                                         
PRTDEB2  BAS   RE,HEXPRT           PRINT BEGINNING PART OF DEB                  
         LA    R4,16(R4)                                                        
         LH    RE,DSPL                                                          
         LA    RE,16(RE)                                                        
         STH   RE,DSPL                                                          
         BCT   R0,PRTDEB2                                                       
         GOTO1 VPRINTER                                                         
         MVC   P+10(L'DEBMSG1),DEBMSG1                                          
         GOTO1 VPRINTER                                                         
         MVC   P+10(L'DEBMSG2),DEBMSG2                                          
         GOTO1 VPRINTER                                                         
*                                                                               
         L     R4,DTFADCB          GET DCB ADDRESS                              
         L     RE,44(R4)           GET DEB ADDRESS                              
         USING DEBBASIC,R4                                                      
         LLC   R0,16(RE)           GET NUMBER OF EXTENTS                        
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
*                                  UCBNAME DEVICE NUMBER                        
         BAS   RE,HEXPRTX          PRINT AN EXTENT                              
         MVC   LASTXTNT,0(R4)      SAVE FOR POSTERITY                           
         SR    RF,RF                                                            
         ICM   RF,3,DEBNMTRK       ADD UP THE NUMBER OF TRACKS                  
         AR    R1,RF                                                            
*                                                                               
         LA    R4,16(R4)           NEXT EXTENT                                  
         LH    RE,DSPL                                                          
         LA    RE,16(RE)                                                        
         STH   RE,DSPL                                                          
         BCT   R0,PRTDEB4                                                       
         DROP  R4                                                               
*                                                                               
         ST    R1,TRKSUSED                                                      
         EDIT  (R1),(7,SUMTRK)                                                  
         MVC   P+40(7),=C'======='                                              
         GOTO1 VPRINTER                                                         
         MVC   P+40(7),SUMTRK      DECIMAL TOTAL                                
         GOTO1 VPRINTER                                                         
         C     R1,LIMIT                                                         
         BNH   HEXDONE                                                          
         MVC   P+5(34),=C'** 1048560 TRACK LIMIT EXCEEDED **'                   
         GOTO1 VPRINTER                                                         
*                                                                               
HEXDONE  LA    R3,DEBFILE                                                       
         XC    WORK(20),WORK       USE WORK SO PARAM LIST UNTOUCHED             
         GOTO1 VDMGR,WORK,DADDS,DACLOSE,,,(R3)                                  
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         EJECT                                                                  
TRKSUSED DC    F'0'                                                             
LIMIT    DC    F'1048560'          MAX SIZE OF 20-BIT TRACK FILE                
*                                  LESS ONE CYLINDER                            
SUMTRK   DS    CL7                                                              
DEBMSG1  DC    C'UCB        START    END         TRK '                          
DEBMSG2  DC    C'ADDR       CC/HH    CC/HH      COUNT'                          
SMSMSG1  DC    C'DATASET NOT FOUND IN CATALOG; NOT SMS MANAGED'                 
SMSMSG2  DC    C'ASSOCIATED SMS INFO FOR DSN:'                                  
*                                                                               
                                                                                
HEXPRT   NTR1                                                                   
         MVI   P,C'+'                                                           
         GOTO1 VHEXOUT,DMCB,DSPL+1,P+1,1,=C'MIX'                                
         GOTO1 VHEXOUT,DMCB,(R4),WORK,16,=C'MIX'                                
         MVC   P+4(8),WORK                                                      
         MVC   P+13(8),WORK+8                                                   
         MVC   P+22(8),WORK+16                                                  
         MVC   P+31(8),WORK+24                                                  
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
                                                                                
HEXPRTX  NTR1                                                                   
         MVI   P,C'+'                                                           
         GOTO1 VHEXOUT,DMCB,DSPL+1,P+1,1,=C'MIX'                                
         GOTO1 VHEXOUT,DMCB,(R4),WORK,16,=C'MIX'                                
         MVC   P+4(2),WORK                                                      
         MVC   P+8(7),WORK+2           UCB ADDRESS                              
         MVC   P+16(4),WORK+8          ZERO AREA                                
         MVC   P+21(8),WORK+12         STARTING CC/HH                           
         MVC   P+30(8),WORK+20         ENDING CC/HH                             
         MVC   P+42(4),WORK+28         TOTAL TRACKS IN EXTENT(HEX)              
*                                                                               
         USING UCBOB,R9                                                         
         PRINT GEN                                                              
         UCBDEVN DEVN=CHAN                                                      
         PRINT NOGEN                                                            
         MVC   P+50(4),CHAN            DEVICE NUMBER                            
         MVC   P+56(6),UCBVOLI         VOL SER                                  
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         DROP  R9                                                               
*                                                                               
DSPL     DC    H'0'                                                             
CHAN     DS    F                                                                
         EJECT                                                                  
***********************************************************************         
* ERROR ROUTINES                                                      *         
***********************************************************************         
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
ERRX     DC    H'0'                                                             
         BCTR  RE,0                ADJUST FOR EX                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(RF)                                                       
         GOTO1 VPRINTER                                                         
         B     DMEOJ                                                            
*                                                                               
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
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
DMEOJ    XBASE                                                                  
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*OLDFIL*'                                                      
OLDFILE  DMDA  DSKXTNT=0,BIG=22BIT                                              
*                                                                               
         DS    0D                                                               
         DC    C'*NEWFIL*'                                                      
NEWFILE  DCB   DDNAME=NEWFILE,DSORG=PS,MACRF=(PM),EXLST=(EXITLST),     X        
               RECFM=FU,BLKSIZE=9500                                            
EXITLST  DS    2F                                                               
*                                                                               
VPRINTER DC    V(PRINTER)                                                       
VCARDS   DC    V(CARDS)                                                         
VDMGR    DC    V(DATAMGR)                                                       
VHEXOUT  DC    V(HEXOUT)                                                        
VCPRINT  DC    V(CPRINT)                                                        
VSCANNER DC    V(SCANNER)                                                       
*                                                                               
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
*                                                                               
DEBFILE  DMDA  DSKXTNT=0,BIG=22BIT                                              
*                                                                               
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
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
PLIST    DS    6F                                                               
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
*                                                                               
DISKADR  DC    X'00000401'         22-BIT DISK ADDRESS                          
FRSTADR  DC    X'00000401'         22-BIT TRACK 1/BLOCK 0/RECORD 1              
         LTORG                                                                  
*                                                                               
SAVEDEB  DC    XL256'00'                                                        
         DC    16X'00'             PRECAUTION                                   
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    C'*ERSFIL*'                                                      
ERSFILE  DMDA  DSKXTNT=0,BIG=22BIT                                              
*                                                                               
NEWREC   DC    950CL10'THUNDERBAY'                                              
*                                                                               
         DS    0D                                                               
         DC    C'**BUFF**'                                                      
DMEXBUFF DS    200D                                                             
*                                                                               
         DC    CL8'*SSBSSB*'                                                    
SSB      DC    X'0000FF',X'40',4X'00',CL8' ',32X'00',A(0),204X'00'              
*                                                                               
         DS    0D                                                               
         DC    CL8'*UTLUTL*'                                                    
UTL      DC    F'0',X'00',XL251'00'                                             
*                                                                               
         DS    0D                                                               
         DC    C'**WORK**'                                                      
DMEXWORK DS    2000D                                                            
*                                                                               
DMEXLAST DS    D                                                                
JFCBAREA DS    176X                                                             
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
*DMDTFPH                                                                        
       ++INCLUDE DMDTFPH                                                        
         EJECT                                                                  
*DMXTNTD                                                                        
       ++INCLUDE DMXTNTD                                                        
         EJECT                                                                  
* FASSBOFF                                                                      
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
*DDSCANBLKD                                                                     
       ++INCLUDE DDSCANBLKD                                                     
         EJECT                                                                  
*DMGREQUS                                                                       
       ++INCLUDE DMGREQUS                                                       
         EJECT                                                                  
         PRINT   GEN                                                            
         DSECT                                                                  
         IEFJFCBN   LIST=YES                                                    
         IEZDEB     LIST=YES                                                    
*                                                                               
         DSECT                                                                  
         IEFUCBOB   DEVCLAS=DA                                                  
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DMEXTEND  12/03/13'                                      
         END                                                                    
