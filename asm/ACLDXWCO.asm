*          DATA SET ACLDXWCO   AT LEVEL 019 AS OF 01/11/01                      
*PHASE ACLDXWCO,*                                                               
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
         USING TRNRECD,R4                                                       
DMXREC   L     R4,AREC                                                          
         GOTO1 =V(ACRECTYP),DMCB,(C'D',TRNRECD)                                 
         CLI   0(R1),ACRTTRN       ONLY WANT TRANSACTIONS                       
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   TRNKUNT(2),=C'SJ'                                                
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   TRNKWORK,=C'99'                                                  
         BNE   DMXKEEP                                                          
*                                                                               
         USING TRNELD,R2                                                        
         LA    R2,TRNRFST                                                       
         CLI   0(R2),TRNELQ        X'44'                                        
         BE    *+6                                                              
         DC    H'00'               JUST CHECKING                                
*                                                                               
         CLI   TRNTYPE,TRNTBILL    IS IT TYPE 6                                 
         BE    DMXKEEP             DON'T NEED IT                                
*                                                                               
         CLI   TRNTYPE,TRNTCLBL    IS IT TYPE 7                                 
         BE    DMXKEEP             DON'T NEED IT                                
*                                                                               
         LA    R7,P                POINT TO PRINT LINE                          
*        CP    PKLNCNT,=P'0'       IS IT FIRST TIME IN                          
*        BNE   DMXREC10                                                         
         GOTO1 HEXOUT,DMCB,TRNKCPY,PRTCPY,L'TRNKCPY                             
DMXREC10 MVC   PRTULA,TRNKULA                                                   
         MVC   PRTWCD,TRNKWORK                                                  
         GOTO1 VDATCON,DMCB,(1,TRNDATE),(8,PRTDATE)                             
         MVC   PRTREF,TRNREF                         REFERENCE                  
         GOTO1 HEXOUT,DMCB,TRNSUB,PRTSBR,L'TRNSUB    SUB REFERENCE              
         MVC   PRTBTCH,TRNBTCH                       BATCH                      
         EDIT  TRNTYPE,PRTTYPE                                                  
         DROP  R2                                                               
*                                                                               
         AP    PKCNT,=P'1'         INC NO. OF RECORDS                           
         BAS   RE,PBOX                                                          
         BAS   RE,NEWPGHDR                                                      
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
         BAS   RE,PRNTLINE                                                      
         GOTO1 VPRINTER                                                         
         MVC   P+2(15),=CL15'NUM OF RECDS = '                                   
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
         MVC   PRTCPY(3),=C'CPY'                                                
         MVC   PRTULA(11),=C'U/L/ACCOUNT'                                       
         MVC   PRTWCD(3),=C'WCD'                                                
         MVC   PRTDATE(4),=C'DATE'                                              
         MVC   PRTREF(3),=C'REF'                                                
         MVC   PRTSBR(7),=C'SUB-REF'        SUB REFERENCE                       
         MVC   PRTBTCH(5),=C'BATCH'         BATCH                               
         MVC   PRTTYPE(4),=C'TYPE'          TYPE OF TIME                        
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
         MVI   P+(PRTCPY-PLINED-1),BAR                                          
         MVI   P+(PRTULA-PLINED-1),BAR                                          
         MVI   P+(PRTWCD-PLINED-1),BAR                                          
         MVI   P+(PRTDATE-PLINED-1),BAR                                         
         MVI   P+(PRTREF-PLINED-1),BAR                                          
         MVI   P+(PRTSBR-PLINED-1),BAR                                          
         MVI   P+(PRTBTCH-PLINED-1),BAR                                         
         MVI   P+(PRTTYPE-PLINED-1),BAR                                         
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
PRTCPY   DS    CL2                 COMPANY CODE                                 
         DS    CL3                                                              
PRTULA   DS    CL14                UNIT/LEDGER                                  
         DS    CL2                                                              
PRTWCD   DS    CL2                 WORK CODE                                    
         DS    CL3                                                              
PRTDATE  DS    CL8                 TRANSACTION DATE                             
         DS    CL2                                                              
PRTREF   DS    CL6                 TRANSACTION REFERENCE                        
         DS    CL2                                                              
PRTSBR   DS    CL2                 TRANSACTION SUB REFERENCE                    
         DS    CL7                                                              
PRTBTCH  DS    CL6                 TRANSACTION BATCH REFERENCE                  
         DS    CL2                                                              
PRTTYPE  DS    CL4                 TYPE OF TRANSACTION                          
         DS    CL5                                                              
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
**PAN#1  DC    CL21'019ACLDXWCO  01/11/01'                                      
         END                                                                    
