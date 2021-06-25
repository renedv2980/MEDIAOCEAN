*          DATA SET ACLDXMTP   AT LEVEL 178 AS OF 02/17/00                      
*PHASE ACLDXMTP,*                                                               
*INCLUDE ACRECTYP                                                               
*INCLUDE DATCON                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'GENERAL ACLD EXTERNAL'                                          
*   FIX MEDIA TRANSFER PROFILE'S INTERNAL PERCENT VALUE FROM 2 DECIMAL          
*   TO THREE DECIMAL PLACES                                                     
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
         B     DMXIT                                                            
         EJECT                                                                  
*----------------------------------------------------------------*              
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*----------------------------------------------------------------*              
         USING MPRRECD,R4                                                       
         USING CPYTBLD,R6                                                       
DMXREC   L     R4,AREC                                                          
         LA    R6,CPYTBL           POINT TO CODE/TOTAL TABL                     
         GOTO1 =V(ACRECTYP),DMCB,(C'D',MPRRECD)                                 
         CLI   0(R1),ACRTMPR       ONLY WANT MEDIA POSTING RULES                
         BNE   DMXKEEP                                                          
         CLI   MPRKSUB,MPRKSUBQ    X'01' ONLY WANT RECD SUB TYPE                
         BNE   DMXKEEP                                                          
*                                                                               
         USING PLINED,R7                                                        
         LA    R7,P                                                             
         USING MTPELD,R2           MEDIA TRANSFER PROFILE                       
         LA    R2,MPRRFST                                                       
DMXREC10 CLI   0(R2),0                                                          
         BE    DMXKEEP                                                          
         CLI   0(R2),MTPELQ        X'2F'                                        
         BNE   *+12                                                             
         CLI   MTPFNUM,MTPFIPT2    X'1B' PERCENTAGE FOR INTRNL (AOR)            
         BE    DMXREC20                                                         
         ZIC   R1,1(R2)                                                         
         AR    R2,R1               BUMP TO NEXT ELEMENT                         
         B     DMXREC10                                                         
*                                                                               
DMXREC20 DS    0H                                                               
         ICM   R8,15,MTPFDATA      GET OLD PCT VALUE                            
         BZ    DMXKEEP                                                          
*                                                                               
DMXREC30 CLI   0(R6),X'FF'         ADD CPY/TOT HERE IF CPY CHANGES              
         BE    *+12                                                             
         LA    R6,CPYTBLQ(R6)      LOOK FOR NXT AVAILABLE AREA IN TBL           
         B     DMXREC30                                                         
         CLI   FLAG,1              IF NOT FIRST TIME THEN SUBTRACT              
         BNE   DMXREC40                                                         
         SHI   R6,CPYTBLQ          POINT TO WHERE CPYCODE/TOTAL IS              
*                                                                               
DMXREC40 CLC   MPRKCPY,SVCPY       DID COMPANY CHANGE                           
         BE    DMXREC50                                                         
         CLI   FLAG,1              IS IT FIRST TIME IN                          
         BNE   *+8                                                              
         LA    R6,CPYTBLQ(R6)      DIFF CPY FND POINT TO NXT AVL SPCE           
         ZAP   CPYTOT,=P'0'        CLEAR TOTAL FIELD                            
         MVI   FLAG,1              MARK FIRST TIME IN ALREADY                   
         MVI   CPYTBLQ(R6),X'FF'   MARK END OF TABLE                            
DMXREC50 DS    0H                                                               
         MVC   CPYCDE,MPRKCPY      SAVE CPY CODE TO TABLE                       
         AP    CPYTOT,=P'1'        INC COMPANY TOTAL                            
         DROP  R6                                                               
*                                                                               
         AP    PKCNT,=P'1'         INC RECORD COUNT                             
         SR    R9,R9                                                            
         ICM   R9,3,MPRRLEN        GET RECORD LENGTH FOR PRINTABLES             
*                                                                               
         MVC   MSG,=CL10'TRNS RECIN'                                            
         LA    R0,L'MSG                                                         
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),MSG),(R4),C'DUMP',(R9),(R5),          X        
               (C'P',PRINT)                                                     
         MVC   P(15),=CL15'OLD VALUE'                                           
         EDIT  (B4,MTPFDATA),PRTPCT,2                                           
         GOTO1 VPRINTER            PRINT OLD VALUE AND DUMP                     
*                                                                               
         MHI   R8,10                                                            
         STCM  R8,15,MTPFDATA      PUT BACK NEW VALUE                           
*                                                                               
         MVC   MSG,=CL10'TRNS OUT'                                              
         LA    R0,L'MSG                                                         
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),MSG),(R4),C'DUMP',(R9),(R5),          X        
               (C'P',PRINT)                                                     
         MVC   P(15),=CL15'NEW VALUE'                                           
         EDIT  (R8),PRTPCT,3                                                    
         GOTO1 VPRINTER            PRINT NEW VALUE AND DUMP                     
         MVC   SVCPY,MPRKCPY       SAVE CURRENT COMPANY                         
*                                                                               
DMXRECX  B     DMXKEEP                                                          
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
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* PRINT LINE DSECT                                                   *          
**********************************************************************          
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL17                                                             
PRTPCT   DS    CL10                      PRINT AMOUNT                           
         DS    CL2                                                              
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
**PAN#1  DC    CL21'178ACLDXMTP  02/17/00'                                      
         END                                                                    
