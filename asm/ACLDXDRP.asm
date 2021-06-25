*          DATA SET ACLDXDRP   AT LEVEL 019 AS OF 03/14/00                      
*PHASE ACLDXDRP                                                                 
*INCLUDE ACRECTYP                                                               
*INCLUDE DATCON                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'CHECK TRNS FOR 8B ELEMENTS'                                     
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
         BAS   RE,PRNTHDR                                                       
         B     DMXIT                                                            
         EJECT                                                                  
*----------------------------------------------------------------*              
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*----------------------------------------------------------------*              
         USING CHDRECD,R4                                                       
DMXREC   L     R4,AREC                                                          
         GOTO1 =V(ACRECTYP),DMCB,(C'D',CHDRECD)                                 
         CLI   0(R1),ACRTCHDH      ONLY WANT OFFICE/CNTRA                       
         BNE   DMXKEEP                                                          
         CLI   CHDKCPY,X'CE'       WANT ONLY ZENY                               
         BNE   DMXKEEP                                                          
*                                                                               
         LA    R1,PAYTAB                 PAYABLE LEDG TABLE                     
DMXREC1  CLI   0(R1),X'FF'                                                      
         BE    DMXKEEP                                                          
         CLC   CHDKUNT(2),0(R1)                                                 
         BE    *+12                                                             
         LA    R1,L'PAYTAB(R1)                                                  
         B     DMXREC1                                                          
*                                                                               
         CLC   CHDKOFF,SPACES            GET ONLY CONTRA/OFFICE                 
         BNH   DMXKEEP                                                          
*        CLC   CHDKULA,=CL14'SPM0124'                                           
*        BNE   DMXKEEP                                                          
*                                                                               
         USING PLINED,R7                                                        
         LA    R7,P                                                             
*                                                                               
         MVC   PRTKULC,CHDKULA           UNIT,LEDGER,ACCOUNT                    
         MVC   PRTOFF,CHDKOFF                                                   
         MVC   PRTKCULC,CHDKCULC         CONTRA UNIT,LEDGER,ACCOUNT             
         XC    FLAG,FLAG                                                        
*                                                                               
         USING BUKELD,R2                                                        
         LA    R2,CHDRFST                                                       
DMXREC2  CLI   0(R2),0                                                          
         BNE   DMXREC5                                                          
         TM    FLAG,FLGBUK                                                      
         BO    DMXRECX                                                          
         MVC   PRTNARR,=CL25'BUCKET DOES NOT EXIST'                             
         BAS   RE,PBOX                   PRNT C'|' AFTR EVRY DATA FLD           
         GOTO1 VPRINTER                  PRINT LINE OF REPORT                   
         B     DMXRECX                                                          
*                                                                               
DMXREC5  CLI   0(R2),BUKELQ              IS IT X'45'                            
         BNE   DMXREC10                  NO EXIT                                
*                                                                               
         OI    FLAG,FLGBUK                                                      
         MVC   WORK(2),BUKMOS                                                   
         MVI   WORK+2,X'01'                                                     
         GOTO1 =V(DATCON),DMCB,(1,WORK),(0,WORK+6)                              
         MVC   PRTMOS,WORK+6                                                    
*                                                                               
         EDIT  (P6,BUKDR),PRTDAMNT,2,ZERO=NOBLANK,MINUS=YES                     
*                                                                               
         EDIT  (P6,BUKCR),PRTCAMNT,2,ZERO=NOBLANK,MINUS=YES                     
*                                                                               
         BAS   RE,PBOX                   PRNT C'|' AFTR EVRY DATA FLD           
         GOTO1 VPRINTER                  PRINT LINE OF REPORT                   
*                                                                               
DMXREC10 DS    0H                        CHECK FOR MULTIPLE BUCKETS             
         ZIC   R1,1(R2)                  GET LENGTH                             
         AR    R2,R1                     BUMP TO X'60' ELEMENT                  
         B     DMXREC2                                                          
*                                                                               
DMXRECX  BAS   RE,NEWPGHDR               PRINT HEADER IF IT'S NEW PAGE          
         B     DMXKEEP                                                          
         DROP  R2,R4,R7                                                         
         EJECT                                                                  
*----------------------------------------------------------------*              
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*----------------------------------------------------------------*              
DMXRET   DS    0H                                                               
                                                                                
*----------------------------------------------------------------*              
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*----------------------------------------------------------------*              
DMXEOF   DS    0H                                                               
         USING PLINED,R7                                                        
         LA    R7,P                                                             
*                                                                               
         BAS   RE,PRNTLINE               CLOSE LAST BOX                         
*                                                                               
DMXEOFX  B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
********************************************************************            
* FORCE NEW PAGE IF PAGE HAS 35 LINES AND PRINT HEADER AT THE      *            
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
         BAS   RE,PRNTHDR                                                       
NEWPGX   XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* PRINT HEADER OF THE REPORT                                         *          
**********************************************************************          
PRNTHDR  NTR1                                                                   
         USING PHEADERD,R7                                                      
         LA    R7,P                                                             
         ZAP   PKLNCNT,=P'0'             RESET NUM OF LINES COUNTER             
*                                                                               
         BAS   RE,PRNTLINE                                                      
*                                                                               
         MVC   PKULC,=CL14'   U/L/ACC'                                          
         MVC   POFF(3),=C'OFF'                                                  
         MVC   PKCULC,=CL15'CONTRA U/L/ACC'                                     
         MVC   PMOS,=CL4'MOS'                                                   
         MVC   PDAMNT+8(5),=C'DEBIT'                                            
         MVC   PCAMNT+8(6),=C'CREDIT'                                           
         MVC   PNARR,=CL25'  BUCKET EXIST OR NOT '                              
         BAS   RE,PBOX                                                          
         GOTO1 VPRINTER                                                         
*                                                                               
         BAS   RE,PRNTLINE                                                      
*                                                                               
         XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT HORIZONTAL LINE                                              *          
**********************************************************************          
PRNTLINE NTR1                                                                   
         MVI   P,C'-'                    DRAW LINE                              
         MVC   P+1(LINELNQ),P                                                   
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* PRINT BOX VERTICAL LINES                                           *          
**********************************************************************          
PBOX     NTR1                                                                   
         USING PLINED,R7                                                        
         MVI   B1,BAR                                                           
         MVI   B2,BAR                                                           
         MVI   B3,BAR                                                           
         MVI   B4,BAR                                                           
         MVI   B5,BAR                                                           
         MVI   B6,BAR                                                           
         MVI   B7,BAR                                                           
         MVI   B8,BAR                                                           
         XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*  DUMP   RECORDS                                                     *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NTR1                                                                   
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,0(R1)                                                         
         L     R4,4(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    B     EXIT                                                             
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
         SPACE 1                                                                
PAYTAB   DS    0CL2                                                             
         DC    C'SP'                                                            
         DC    C'SV'                                                            
         DC    C'SX'                                                            
         DC    C'SS'                                                            
         DC    C'ST'                                                            
         DC    C'SU'                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE AND CONSTANTS                                      *          
**********************************************************************          
         SPACE 1                                                                
BAR      EQU   C'|'                                                             
MAXLN    DC    PL4'35'                   MAXIMUM LINES PER PAGE                 
PKLNCNT  DC    PL4'0'                                                           
*                                                                               
PRNTBL   DC    V(PRNTBL)                                                        
PRINT    DS    V(PRINT)                                                         
FLAG     DS    XL1                                                              
FLGBUK   EQU   X'80'                                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* PRINT HEADER DSECT                                                 *          
**********************************************************************          
         SPACE 1                                                                
PHEADERD DSECT                                                                  
         DS    CL1                                                              
PKULC    DS    CL14                      UNIT                                   
         DS    CL2                       C'|' FOR BOX                           
POFF     DS    CL2                                                              
         DS    CL2                                                              
PKCULC   DS    CL15                      CONTRA UNIT,LEDGER,ACCOUNT             
         DS    CL2                                                              
PMOS     DS    CL4                                                              
         DS    CL2                                                              
PDAMNT   DS    CL16                      PRINT DEBIT AMOUNT                     
         DS    CL2                                                              
PCAMNT   DS    CL16                      PRINT CREDIT AMOUNT                    
         DS    CL2                                                              
PNARR    DS    CL25                                                             
         DS    CL1                                                              
LINELNQ  EQU   *-PHEADERD-1              UNDERLINE HEADERS  EQU                 
         EJECT                                                                  
**********************************************************************          
* PRINT LINE DSECT                                                   *          
**********************************************************************          
         SPACE 1                                                                
PLINED   DSECT                                                                  
B1       DS    CL1                       C'|' FOR A BOX                         
PRTKULC  DS    CL14                      UNIT,LEDGER,ACC                        
B2       DS    CL2                       C'|' FOR BOX                           
PRTOFF   DS    CL2                                                              
B3       DS    CL2                                                              
PRTKCULC DS    CL15                      CONTRA UNIT,LEDGER,ACCOUNT             
B4       DS    CL2                                                              
PRTMOS   DS    CL4                                                              
B5       DS    CL2                                                              
PRTDAMNT DS    CL16                      PRINT DEBIT AMOUNT                     
B6       DS    CL2                                                              
PRTCAMNT DS    CL16                      PRINT CREDIT AMOUNT                    
B7       DS    CL2                                                              
PRTNARR  DS    CL25                                                             
B8       DS    CL1                                                              
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
**PAN#1  DC    CL21'019ACLDXDRP  03/14/00'                                      
         END                                                                    
