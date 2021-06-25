*          DATA SET ACLDXWCT   AT LEVEL 028 AS OF 09/25/00                      
*PHASE ACLDXWCT,*                                                               
*INCLUDE ACRECTYP                                                               
*INCLUDE DATCON                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'GENERAL ACLD EXTERNAL'                                          
*   FIX WORK CODES ** FOR AGENCIES WHO SHOULD NOT HAVE ** WORK CODES            
*                                                                               
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
         BAS   RE,PRTHEAD          PRINTHEADER                                  
*                                                                               
         ZAP   PKCNT,=P'0'         INITIALIZE RECD TOTAL COUNTER                
         B     DMXIT                                                            
         EJECT                                                                  
*----------------------------------------------------------------*              
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*----------------------------------------------------------------*              
         USING PLINED,R7                                                        
         USING TIMRECD,R4                                                       
DMXREC   L     R4,AREC                                                          
         GOTO1 =V(ACRECTYP),DMCB,(C'D',TIMRECD)                                 
         CLI   0(R1),ACRTTIM       ONLY WANT TRANSACTIONS                       
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   TIMKREF,=C'*TIME*'                                               
         BNE   DMXKEEP                                                          
*                                                                               
         USING TIMELD,R2                                                        
         LA    R2,TIMRFST                                                       
DMXREC10 CLI   0(R2),0                                                          
         BE    DMXRECX                                                          
         CLI   0(R2),TIMELQ        X'8B'                                        
         BE    DMXREC20                                                         
DMXREC15 ZIC   R1,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,R1                                                            
         B     DMXREC10                                                         
*                                                                               
DMXREC20 DS    0H                                                               
         CLI   TIMETYP,TIMEINP                                                  
         BNE   DMXREC15                                                         
         CLC   TIMTSK,=C'**'                                                    
         BNE   DMXREC15            DON'T NEED IT                                
*                                                                               
         LA    R7,P                POINT TO PRINT LINE                          
         CP    PKLNCNT,=P'0'       IS IT FIRST TIME IN                          
         BNE   DMXREC30                                                         
         GOTO1 HEXOUT,DMCB,TIMKCPY,PCPY,L'TIMKCPY                               
         MVC   PUL,TIMKUNT         PRINT UNIT/LEDGER                            
*                                                                               
DMXREC30 DS    0H                                                               
         LA    R3,TIMKACT-TIMKEY(R4)                                            
         MVC   POFF(LEV1),0(R3)    OFFICE                                       
         LA    R3,LEV1(R3)                                                      
         MVC   PDPT(LEV2),0(R3)    DEPARTMENT                                   
         LA    R3,LEV2(R3)                                                      
         MVC   PSBDPT(LEV3),0(R3)  SUB-DEPARTMENT                               
         LA    R3,LEV3(R3)                                                      
         MVC   PPER(LEV4),0(R3)    PERSON                                       
         GOTO1 VDATCON,DMCB,(1,TIMKPEDT),(8,PEDATE)                             
*                                                                               
         MVC   PTASK,TIMTSK                                                     
         EDIT  (P3,TIMHRS),(8,PHOURS)                                           
*                                                                               
         MVI   PTYPE,C'B'          ASSUME BILLABLE TIME                         
         CLI   TIMTTYP,TIMTCR      CLIENT REALIZATION                           
         BNE   *+12                                                             
         MVI   PTYPE,C'R'                                                       
         B     DMXREC40                                                         
*                                                                               
         CLI   TIMTTYP,TIMTCN       NON-BILLABLE                                
         BNE   *+12                                                             
         MVI   PTYPE,C'N'                                                       
         B     DMXREC40                                                         
*                                                                               
         CLI   TIMTTYP,TIMTNC       NON-CLIENT                                  
         BNE   *+8                                                              
         MVC   PTYPE(2),=C'NC'                                                  
         DROP  R2                                                               
*                                                                               
DMXREC40 AP    PKCNT,=P'1'         INC NO. OF RECORDS                           
         BAS   RE,PBOX                                                          
         BAS   RE,NEWPGHDR                                                      
         B     DMXREC15                                                         
*                                                                               
DMXRECX  B     DMXKEEP                                                          
         DROP  R4,R7                                                            
         EJECT                                                                  
*----------------------------------------------------------------*              
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*----------------------------------------------------------------*              
DMXRET   DS    0H                                                               
                                                                                
*----------------------------------------------------------------*              
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*----------------------------------------------------------------*              
DMXEOF   DS    0H                                                               
*        GOTO1 VPRINTER                                                         
*        MVC   P(16),=CL16'TOTALS FOR     ='                                    
*        GOTO1 HEXOUT,DMCB,CPYCDE,P+12,L'CPYCDE                                 
*        EDIT  (P4,CPYTOT),(8,P+17)       COMPANY TOTAL                         
*        GOTO1 VPRINTER                                                         
*        LA    R6,CPYTBLQ(R6)                                                   
*        B     DMXEOF10                                                         
* DMXEOF20 DS    0H                                                             
         BAS   RE,PRNTLINE                                                      
         GOTO1 VPRINTER                                                         
         MVC   P+2(15),=CL15'NUM OF LINES = '                                   
         EDIT  (P4,PKCNT),(8,P+17)       RECD/COMPANY TOTAL                     
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
DMXEOFX  B     EXIT                                                             
*        DROP  R7                                                               
         EJECT                                                                  
********************************************************************            
* FORCE NEW PAGE IF PAGE HAS 25 LINES AND PRINT HEADER AT THE      *            
* BEGINING OF EVERY PAGE                                           *            
********************************************************************            
NEWPGHDR NTR1                                                                   
         AP    PKLNCNT,=P'1'             INC NUM OF LINES COUNTER               
         CP    PKLNCNT,MAXLN                                                    
         BNH   NEWPGX                    ONLY PRINT 26 LINES A PAGE             
*                                                                               
         BAS   RE,PRNTLINE               CLOSE BOX                              
*                                                                               
         AP    LINE,MAXLINE              FORCE NEW PAGE                         
         BAS   RE,PRTHEAD                                                       
NEWPGX   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  PRINT HEADER                                                       *         
***********************************************************************         
         USING PLINED,R7                                                        
PRTHEAD  NTR1                                                                   
         ZAP   PKLNCNT,=P'0'                                                    
*                                                                               
         BAS   RE,PRNTLINE               CLOSE BOX                              
         LA    R7,P                                                             
         MVC   PCPY(3),=C'CPY'                                                  
         MVC   PUL(3),=C'U/L'                                                   
         MVC   PPER(6),=C'PERSON'                                               
         MVC   POFF(3),=C'OFF'                                                  
         MVC   PDPT(3),=C'DPT'                                                  
         MVC   PSBDPT(7),=C'SUB-DPT'      SUB DEPARTMENT                        
         MVC   PTASK(4),=C'TASK'                                                
         MVC   PTYPE(4),=C'TYPE'            TYPE OF TIME                        
         MVC   PEDATE(8),=C'END-DATE'       END DATE                            
         MVC   PHOURS(5),=C'HOURS'                                              
*                                                                               
         BAS   RE,PBOX                                                          
         BAS   RE,PRNTLINE               CLOSE BOX                              
PRTHDX   DS    0H                                                               
         XIT1                                                                   
         DROP  R7                                                               
**********************************************************************          
* PRINT HORIZONTAL LINE                                              *          
**********************************************************************          
PRNTLINE NTR1                                                                   
         MVI   P+1,C'-'                    DRAW LINE                            
         MVC   P+2(PLINLNQ),P                                                   
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* PRINT BOX VERTICAL LINES                                           *          
**********************************************************************          
PBOX     NTR1                                                                   
         USING PLINED,R7                                                        
         MVI   P+(PCPY-PLINED-1),BAR                                            
         MVI   P+(PUL-PLINED-1),BAR                                             
         MVI   P+(PPER-PLINED-1),BAR                                            
         MVI   P+(POFF-PLINED-1),BAR                                            
         MVI   P+(PDPT-PLINED-1),BAR                                            
         MVI   P+(PSBDPT-PLINED-1),BAR                                          
         MVI   P+(PTASK-PLINED-1),BAR                                           
         MVI   P+(PTYPE-PLINED-1),BAR                                           
         MVI   P+(PEDATE-PLINED-1),BAR                                          
         MVI   P+(PHOURS-PLINED-1),BAR                                          
         MVI   P+PLINLNQ,BAR                                                    
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         DROP  R7                                                               
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
MAXLN    DC    PL4'35'                                                          
PKCNT    DS    PL4                 RECORD COUNTER                               
PKLNCNT  DS    PL4'0'              NO. OF LINES                                 
BAR      EQU   C'|'                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
VDATCON  DC    V(DATCON)                                                        
**********************************************************************          
* PRINT LINE DSECT                                                   *          
**********************************************************************          
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL2                                                              
PCPY     DS    CL2                 COMPANY CODE                                 
         DS    CL3                                                              
PUL      DS    CL2                 UNIT/LEDGER                                  
         DS    CL2                                                              
PPER     DS    CL6                 PERSON                                       
         DS    CL2                                                              
POFF     DS    CL2                 OFFICE                                       
         DS    CL2                                                              
PDPT     DS    CL2                 DEPARTMENT                                   
         DS    CL2                                                              
PSBDPT   DS    CL2                 SUB DEPARTMENT                               
         DS    CL7                                                              
PTASK    DS    CL2                 TIME TASK                                    
         DS    CL3                                                              
PTYPE    DS    CL1                 TYPE OF TIME R,N,B                           
         DS    CL5                                                              
PEDATE   DS    CL8                 END      DATE                                
         DS    CL2                                                              
PHOURS   DS    CL8                 HOURS                                        
         DS    CL2                                                              
PLINLNQ  EQU   *-PLINED                                                         
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
PERKEYD  DSECT                     DSECT TO COVER CPYTBL                        
         DS    CL3                 CPY/UNIT/LEDGER                              
PEROFF   DS    CL2                 PERSON OFFICE                                
PERDPT   DS    CL2                 PERSON DPT                                   
PERSBDPT DS    CL2                 PERSON SUB-DPT                               
PERPER   DS    CL6                                                              
PERKEYLQ EQU   *-PERKEYD                                                        
         EJECT                                                                  
**********************************************************************          
* EQUATES                                                            *          
**********************************************************************          
         SPACE 1                                                                
LEV1     EQU   2                                                                
LEV2     EQU   2                                                                
LEV3     EQU   2                                                                
LEV4     EQU   6                                                                
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
**PAN#1  DC    CL21'028ACLDXWCT  09/25/00'                                      
         END                                                                    
