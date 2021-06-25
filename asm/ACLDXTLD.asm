*          DATA SET ACLDXTLD   AT LEVEL 193 AS OF 06/08/00                      
*PHASE ACLDXTLD,*                                                               
*INCLUDE ACRECTYP                                                               
*INCLUDE DATCON                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'GENERAL ACLD EXTERNAL FOR CHECKING TIMSHT LOCK DATE'            
*   CHECK TIME SHEET LOCK DATE IN LOCELD READING PERSON RECORD                  
*   IF TIMESHEET LOCK DATE (LOCLOCK) EXIST PRINT PERSON AND SOME INFO           
*   FROM LOCELD                                                                 
*--------------------------------------------------------------------*          
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*--------------------------------------------------------------------*          
         PRINT NOGEN                                                            
DMLDELS  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDELS                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
*--------------------------------------------------------------------*          
* CONTROL FLOW LOGIC                                                            
*--------------------------------------------------------------------*          
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
                                                                                
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
                                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
                                                                                
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
                                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
                                                                                
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
                                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
                                                                                
DMXIT    XMOD1 1                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*----------------------------------------------------------------*              
DMXINIT  DS    0H                                                               
         ZAP   PKCNT,=P'0'         INITIALIZE RECD TOTAL COUNTER                
         MVI   FLAG,0              FIRST TIME IN FLAG                           
         MVI   SVCPY,0             INIT COMPANY WITH ARBITRARY VALUE            
         MVI   CPYTBL,X'FF'        END OF TABL MARKER                           
         BAS   RE,PHEADER                                                       
         B     DMXIT                                                            
         EJECT                                                                  
*----------------------------------------------------------------*              
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*----------------------------------------------------------------*              
         USING PERRECD,R4                                                       
         USING CPYTBLD,R6                                                       
DMXREC   L     R4,AREC                                                          
         LA    R6,CPYTBL           POINT TO CODE/TOTAL TABL                     
         GOTO1 =V(ACRECTYP),DMCB,(C'D',PERRECD)                                 
         CLI   0(R1),ACRTPER       ONLY WANT PERSON RECORDS                     
         BNE   DMXKEEP                                                          
*                                                                               
         USING PLINED,R7                                                        
         LA    R7,P                                                             
         USING LOCELD,R2           STAFF LOCATION ELEMENT                       
         LA    R2,PERRFST                                                       
DMXREC10 CLI   0(R2),0                                                          
         BE    DMXKEEP                                                          
         CLI   0(R2),LOCELQ        X'83' ELEMENT                                
         BE    DMXREC30                                                         
DMXREC20 ZIC   R1,1(R2)                                                         
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         B     DMXREC10                                                         
*                                                                               
DMXREC30 DS    0H                                                               
         OC    LOCLOCK,LOCLOCK     IS THERE A LOCK DATE IN X'83' ELEM           
         BZ    DMXREC20            NO CHECK NEXT X'83' IF EXIST                 
         GOTO1 HEXOUT,DMCB,PERKCPY,PCPY,L'PERKCPY  PRINT COMP CODE              
         MVC   PPERSON,PERKCODE    FROM PERSON RECD KEY                         
         MVC   POFFICE,LOCOFF                                                   
         MVC   PDEPT,LOCDEPT                                                    
         MVC   PSUBDEPT,LOCSUB     MOVE IN SUB DEPT TO PLINE                    
         GOTO1 VDATCON,DMCB,(1,LOCSTART),(8,PSTARTDT)                           
         GOTO1 VDATCON,DMCB,(1,LOCEND),(8,PENDDT)                               
         GOTO1 VDATCON,DMCB,(1,LOCLOCK),(8,PLOCDATE)                            
         GOTO1 VPRINTER                                                         
         BAS   RE,NEWPGHDR                                                      
*                                                                               
DMXREC40 CLI   0(R6),X'FF'         ADD CPY/TOT HERE IF CPY CHANGES              
         BE    *+12                                                             
         LA    R6,CPYTBLQ(R6)      LOOK FOR NXT AVAILABLE AREA IN TBL           
         B     DMXREC40                                                         
         CLI   FLAG,1              IF NOT FIRST TIME THEN SUBTRACT              
         BNE   DMXREC50                                                         
         SHI   R6,CPYTBLQ          POINT TO WHERE CPYCODE/TOTAL IS              
*                                                                               
DMXREC50 CLC   PERKCPY,SVCPY       DID COMPANY CHANGE                           
         BE    DMXREC60                                                         
         CLI   FLAG,1              IS IT FIRST TIME IN                          
         BNE   *+8                                                              
         LA    R6,CPYTBLQ(R6)      DIFF CPY FND POINT TO NXT AVL SPCE           
         ZAP   CPYTOT,=P'0'        CLEAR TOTAL FIELD                            
         MVI   FLAG,1              MARK FIRST TIME IN ALREADY                   
         MVI   CPYTBLQ(R6),X'FF'   MARK END OF TABLE                            
DMXREC60 DS    0H                                                               
         MVC   CPYCDE,PERKCPY      SAVE CPY CODE TO TABLE                       
         AP    CPYTOT,=P'1'        INC COMPANY TOTAL                            
         DROP  R6                                                               
*                                                                               
         AP    PKCNT,=P'1'         INC RECORD COUNT                             
         MVC   SVCPY,PERKCPY       SAVE CURRENT COMPANY                         
         B     DMXREC20            CHECK NEXT X'83' ELEM IF EXIST               
DMXRECX  DS    0H                                                               
         BAS   RE,NEWPGHDR         PRINT HEADER IF NEW PAGE                     
         B     DMXKEEP                                                          
*        SR    R9,R9                                                            
*        ICM   R9,3,MPRRLEN        GET RECORD LENGTH FOR PRINTABLES             
*                                                                               
*        MVC   MSG,=CL10'TRNS RECIN'                                            
*        LA    R0,L'MSG                                                         
*        LA    R5,=C'2D'                                                        
*        GOTO1 PRNTBL,DMCB,((R0),MSG),(R4),C'DUMP',(R9),(R5),          X        
               (C'P',PRINT)                                                     
*        MVC   P(15),=CL15'OLD VALUE'                                           
*        EDIT  (B4,MTPFDATA),PRTPCT,2                                           
*        GOTO1 VPRINTER            PRINT OLD VALUE AND DUMP                     
*                                                                               
*        MHI   R8,10                                                            
*        STCM  R8,15,MTPFDATA      PUT BACK NEW VALUE                           
*                                                                               
*        MVC   MSG,=CL10'TRNS OUT'                                              
*        LA    R0,L'MSG                                                         
*        LA    R5,=C'2D'                                                        
*        GOTO1 PRNTBL,DMCB,((R0),MSG),(R4),C'DUMP',(R9),(R5),          X        
               (C'P',PRINT)                                                     
*        MVC   P(15),=CL15'NEW VALUE'                                           
*        EDIT  (R8),PRTPCT,3                                                    
*        GOTO1 VPRINTER            PRINT NEW VALUE AND DUMP                     
*        MVC   SVCPY,MPRKCPY       SAVE CURRENT COMPANY                         
* DMXRECX  B     DMXKEEP                                                        
         DROP  R2,R4                                                            
         EJECT                                                                  
*****************************************************************               
* SUBROUTINE TO PRINT HEADER OF THE REPORT                                      
*****************************************************************               
         USING PLINED,R7                                                        
PHEADER  NTR1                                                                   
         LA    R7,P                                                             
         ZAP   PKLNCNT,=P'0'       RESET NUMBER OF LINES COUNTER                
         MVC   PCPY(7),=C'COMPANY'                                              
         MVC   PPERSON(6),=C'PERSON'                                            
         MVC   PSTARTDT(8),=C'START DT'                                         
         MVC   PENDDT(6),=C'END DT'                                             
         MVC   POFFICE(6),=C'OFFICE'                                            
         MVC   PDEPT(4),=C'DEPT'                                                
         MVC   PSUBDEPT(8),=C'SUB-DEPT'                                         
         MVC   PLOCDATE(9),=C'LOCK-DATE'                                        
         GOTO1 VPRINTER                                                         
         MVC   PCPY(7),=C'-------'                                              
         MVC   PPERSON(6),=C'------'                                            
         MVC   PSTARTDT(8),=C'--------'                                         
         MVC   PENDDT(6),=C'------'                                             
         MVC   POFFICE(6),=C'------'                                            
         MVC   PDEPT(4),=C'----'                                                
         MVC   PSUBDEPT(8),=C'--------'                                         
         MVC   PLOCDATE(9),=C'---------'                                        
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         EJECT                                                                  
         DROP  R7                                                               
********************************************************************            
* FORCE NEW PAGE IF PAGE HAS 35 LINES AND PRINT HEADER AT THE      *            
* BEGINING OF EVERY PAGE                                           *            
********************************************************************            
NEWPGHDR NTR1                                                                   
         AP    PKLNCNT,=P'1'             INC NUM OF LINES COUNTER               
         CP    PKLNCNT,MAXLN                                                    
         BNH   NEWPGX                    ONLY PRINT 26 LINES A PAGE             
*                                                                               
         AP    LINE,MAXLINE              FORCE NEW PAGE                         
         BAS   RE,PHEADER                                                       
NEWPGX   XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*----------------------------------------------------------------*              
DMXRET   DS    0H                                                               
                                                                                
*----------------------------------------------------------------*              
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*----------------------------------------------------------------*              
DMXEOF   DS    0H                                                               
         USING CPYTBLD,R6                                                       
         LA    R6,CPYTBL           POINT TO COMPANY CODE/TOTAL RECD             
         GOTO1 VPRINTER                                                         
DMXEOF10 CLI   0(R6),X'FF'         IS IT END OF TABLE                           
         BE    DMXEOF20                                                         
         MVC   P(16),=CL16'TOTALS FOR     ='                                    
         GOTO1 HEXOUT,DMCB,CPYCDE,P+12,L'CPYCDE                                 
         EDIT  (P4,CPYTOT),(8,P+17)       COMPANY TOTAL                         
         GOTO1 VPRINTER                                                         
         LA    R6,CPYTBLQ(R6)                                                   
         B     DMXEOF10                                                         
DMXEOF20 DS    0H                                                               
         GOTO1 VPRINTER                                                         
         MVC   P(15),=CL15'NUM OF RECDS = '                                     
         EDIT  (P4,PKCNT),(8,P+17)       RECD/COMPANY TOTAL                     
         GOTO1 VPRINTER                                                         
*                                                                               
DMXEOFX  B     EXIT                                                             
*        DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*  DUMP   RECORDS                                                     *         
***********************************************************************         
*        SPACE 1                                                                
*DUMP     NMOD1 0,**DMP**                                                       
*         L     RC,0(R1)                                                        
*         LA    R0,L'MSG                                                        
*         LA    R2,MSG                                                          
*         L     R3,4(R1)                                                        
*         L     R4,8(R1)                                                        
*                                                                               
*         LA    R5,=C'2D'                                                       
*        GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),        X         
*              (C'P',PRINT)                                                     
*                                                                               
*DUMPX    XIT1                                                                  
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
         SPACE 1                                                                
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE AND CONSTANTS                                      *          
**********************************************************************          
         SPACE 1                                                                
*                                                                               
PRNTBL   DC    V(PRNTBL)                                                        
PRINT    DC    V(PRINT)                                                         
HEXOUT   DC    V(HEXOUT)                                                        
PKCNT    DS    PL4                 RECORD COUNTER                               
FLAG     DS    X                                                                
SVCPY    DS    X                   SAVE COMPANY CODE                            
CPYTBL   DS    XL500               COMAPNY CODE AND TOTAL TABLE                 
MAXLN    DC    PL4'35'                                                          
PKLNCNT  DC    PL4'0'                                                           
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
VDATCON  DC    V(DATCON)                                                        
         EJECT                                                                  
**********************************************************************          
* PRINT LINE DSECT                                                   *          
**********************************************************************          
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL2                                                              
PCPY     DS    CL2                       PERSON                                 
         DS    CL7                                                              
PPERSON  DS    CL8                       PERSON                                 
         DS    CL2                                                              
PSTARTDT DS    CL8                 LOCATION START DATE                          
         DS    CL2                                                              
PENDDT   DS    CL8                 LOCATION END DATE                            
         DS    CL2                                                              
POFFICE  DS    CL2                 OFFICE                                       
         DS    CL6                                                              
PDEPT    DS    CL6                 DEPARTMENT                                   
         DS    CL2                                                              
PSUBDEPT DS    CL6                 SUB-DEPARTMENT                               
         DS    CL4                                                              
PLOCDATE DS    CL8                 TIMESHEET LOCK DATE                          
*                                                                               
         EJECT                                                                  
**********************************************************************          
* OTHER DSECTS                                                       *          
**********************************************************************          
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
HALF     DS    H                                                                
WORK     DS    CL64                                                             
                                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
ELEMENT  DS    XL255                                                            
RECTYP   DS    CL1                                                              
*                                                                               
MSG      DS    CL10                                                             
WORKX    EQU   *                                                                
*                                                                               
CPYTBLD  DSECT                     DSECT TO COVER CPYTBL                        
CPYCDE   DS    XL1                 COMPANY CODE                                 
CPYTOT   DS    PL4                 TOT # OF RECDS IN COMPANY                    
CPYTBLQ  EQU   *-CPYTBLD                                                        
         EJECT                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'193ACLDXTLD  06/08/00'                                      
         END                                                                    
