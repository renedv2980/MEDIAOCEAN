*          DATA SET UNLDEX     AT LEVEL 141 AS OF 05/01/02                      
*PHASE UNXUNLDA                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE CLUNPK                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'UNLDEX - LOAD/DUMP MODEL EXTERNAL ROUTINE'                      
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
UNLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,*UNLDEXT,RR=R5                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
UNXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         ST    R5,RELO                                                          
         L     RE,=V(DATCON)                                                    
         AR    RE,R5                                                            
         ST    RE,VDATCON                                                       
         L     RE,=V(GETDAY)                                                    
         AR    RE,R5                                                            
         ST    RE,VGETDAY                                                       
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,VPRNTBL                                                       
         L     RE,=V(HEXOUT)                                                    
         AR    RE,R5                                                            
         ST    RE,VHEXOUT                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    UNINIT              INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    UNXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    UNXEOF              END-OF-FILE                                  
         B     UNXIT                                                            
         SPACE 2                                                                
UNXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
***      GOTO1 VHEXOUT,DMCB,(R3),P,50                                           
****     GOTO1 VPRINTER                                                         
         B     UNXIT                                                            
         SPACE 2                                                                
UNXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     UNXIT                                                            
         SPACE 2                                                                
UNXIT    XMOD1 1                                                                
         EJECT                                                                  
*************************************************************                   
* INITIALIZE LOGIC                                          *                   
*************************************************************                   
         SPACE                                                                  
UNINIT   XC    CLTSV,CLTSV                                                      
         ZAP   CLTGRS,=P'0'                                                     
         ZAP   CLTNET,=P'0'                                                     
         ZAP   CLTTOT,=P'0'                                                     
         ZAP   ALLTOT,=P'0'                                                     
         B     UNXIT                                                            
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                                          
****************************************************************                
UNXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         CLC   =X'0E06',0(R3)      IS IT NEWBILLING RECORD                      
         BNE   UNXKEEP                                                          
         USING NUBRECD,R3                                                       
         TM    NUBSTAT,X'80'       SKIP DELETED RECS                            
         BO    UNXOK                                                            
         GOTO1 =V(CLUNPK),DMCB,NUBKCLI,WORK+60                                  
***      CLC   =C'BB',WORK+60                                                   
***      BNE   UNXOK                                                            
***      CLC   =C'MTVT',NUBKNET      **************88                           
***      BNE   UNXOK                 *******************OOPOWER                 
***      CLI   NUBKEST,6           6 ONLY                                       
***      BNE   UNXOK                                                            
***      GOTO1 =V(PRNTBL),DMCB,=C'RECS',0(R3),C'DUMP',180,=C'1D'                
         CLI   CLTSV,0             FIRST TIME?                                  
         BE    UNX13               YES                                          
         CLC   WORK+60(3),CLTSV    CLIENT CHANGED?                              
         BE    UNX08                                                            
         OI    FLAG,X'01'          CLIENT CHANGE                                
         B     UNX10                                                            
UNX08    CLC   NETSV,NUBKNET       NET CHANGED                                  
         BE    UNX17                                                            
UNX10    BAS   RE,FLUSHIT          YES/PRINT OUT CLIENT TOTALS                  
*                                                                               
UNX13    MVC   CLTSV,WORK+60       YES/SET CLIENT,NETWORK                       
         MVC   NETSV,NUBKNET                                                    
*                                                                               
UNX17    LA    R3,NUBELDQ(R3)      POINT TO FIRST ELEMENT                       
         CLI   0(R3),X'10'                                                      
         BNE   UNXOK                                                            
         USING NBILD,R3                                                         
UNX20    TM    NBILST,X'20'        TEST UNBILLED                                
         BO    UNX30               SKIP                                         
* CHECKING                                                                      
****     L     R5,AREC                                                          
****     GOTO1 =V(PRNTBL),DMCB,=C'RECS',0(R5),C'DUMP',40,=C'1D'                 
****     EDIT  (B4,NBILGRS),(10,P+1)                                            
****     GOTO1 VPRINTER                                                         
*                                                                               
         L     R1,NBILGRS                                                       
         CVD   R1,DUB                                                           
         AP    CLTGRS,DUB                                                       
         L     R1,NBILNET                                                       
         CVD   R1,DUB                                                           
         AP    CLTNET,DUB                                                       
UNX30    ZIC   R4,1(R3)                                                         
         LTR   R4,R4                                                            
         BZ    UNXOK                                                            
         AR    R3,R4                                                            
         CLI   0(R3),X'10'                                                      
         BNE   UNXOK                                                            
         B     UNX20                                                            
*                                                                               
         B     UNXOK                                                            
         DROP  R3                                                               
FLUSHIT  NTR1                                                                   
         MVC   P+1(3),CLTSV                                                     
         MVC   P+6(4),NETSV                                                     
         ZAP   PAKWORK,CLTGRS                                                   
         BAS   RE,EDITIT                                                        
         AP    ALLTOT,CLTGRS       ROLL OVER                                    
         AP    CLTTOT,CLTGRS       ROLL OVER                                    
         GOTO1 VPRINTER                                                         
         TM    FLAG,X'01'          CLIENT CHANGE?                               
         BNO   FLSH10                                                           
         NI    FLAG,X'FF'-X'01'                                                 
         MVC   P+1(10),=C'CLIENT TOT'                                           
         ZAP   PAKWORK,CLTTOT                                                   
         BAS   RE,EDITIT                                                        
         ZAP   CLTTOT,=P'0'                                                     
         GOTO1 VPRINTER                                                         
         TM    FLAG,X'02'           EOF                                         
         BNO   FLSH10                                                           
         MVC   P+1(10),=C'AGENCY TOT'                                           
         ZAP   PAKWORK,ALLTOT                                                   
         BAS   RE,EDITIT                                                        
         GOTO1 VPRINTER                                                         
FLSH10   ZAP   CLTGRS,=P'0'                                                     
         ZAP   CLTNET,=P'0'                                                     
         XIT1                                                                   
*                                                                               
EDITIT   NTR1                                                                   
         LA    R4,P+13                                                          
         UNPK  WORK(15),PAKWORK                                                 
         MVC   BYTE,WORK+14        SAVE SIGN                                    
         OI    WORK+14,X'F0'                                                    
         MVC   15(2,R4),WORK+13    MOVE IN DECIMALS                             
         MVI   14(R4),C'.'         AND DECIMAL POINT                            
         MVC   1(13,R4),WORK                                                    
         LA    R1,12               DROP LEADING ZEROS                           
         LR    RE,R4               SAVE START OF PRINT AREA                     
         LA    R4,1(R4)            POINT TO START OF NUMBER                     
EDI10    CLI   0(R4),X'F0'                                                      
         BNE   EDI12                                                            
         MVI   0(R4),X'40'                                                      
         LA    R4,1(R4)                                                         
         BCT   R1,EDI10                                                         
EDI12    TM    BYTE,X'D0'          IS IT NEGATIVE                               
         BNO   EDIX                                                             
         CLI   1(RE),X'40'         YES/SET NEG SIGN BEFORE 1ST                  
         BH    *+12                    SIGNIFICANT DIGIT                        
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
EDIX     XIT1                                                                   
***************************************************************                 
                                                                                
* UNIT                                                                          
UNXR10   CLI   1(R3),X'63'         YNR ONLY                                     
         BNE   UNXPURGE                                                         
         USING NURECD,R3                                                        
         TM    NURSTAT,X'C0'       CLOSED OUT ONLY                              
         BNO   UNXPURGE                                                         
         CLC   NUKCLT,=X'8961'     CLB                                          
         BE    UNXR15                                                           
         CLC   NUKCLT,=X'896E'     CLO                                          
         BE    UNXR15                                                           
         CLC   NUKCLT,=X'8970'     CLQ                                          
         BE    UNXR15                                                           
         CLC   NUKCLT,=X'8977'     CLX                                          
         BE    UNXR15                                                           
         CLC   NUKCLT,=X'897C'     CL3                                          
         BNE   UNXPURGE                                                         
                                                                                
UNXR15   DS    0H                                                               
         NI    NURSTAT,X'FF'-X'C0' UNCLOSE                                      
*                                                                               
UNXOK    DS    0H                                                               
**       GOTO1 =V(PRNTBL),DMCB,=C'RECS',AREC,C'DUMP',40,=C'1D'                  
         B     UNXKEEP                                                          
***********************************************8                                
         CLI   NUPACK,1            PACKAGE = 1                                  
         BNE   UNXKEEP                                                          
         TM    NUPACKST,X'20'      LOCKED ONLY                                  
         BZ    UNXKEEP                                                          
         NI    NUPACKST,X'FF'-X'01'   CLEAR NOSHOW DELETE                       
         NI    NURSTAT,X'FF'-X'80'    UNDELETE                                  
                                                                                
                                                                                
*                                                                               
UNX200   CLI   0(R3),X'02'         IS IT PACKAGE                                
         BNE   UNXPURGE                                                         
         USING NPRECD,R3                                                        
         CLI   NPKAM,X'43'         BDPE                                         
         BNE   UNXPURGE                                                         
         CLC   NPKCLT,=X'BC8D'     CLIENT PEN                                   
         BNE   UNXPURGE                                                         
         CLI   NPKEST,152          EST=152                                      
         BNE   UNXPURGE                                                         
         B     UNXKEEP                                                          
                                                                                
***********************************************                                 
         CLI   NPKPACK,1           PACKAGE = 1                                  
         BNE   UNXKEEP                                                          
         TM    NPAKSTAT,X'20'      LOCKED ONLY                                  
         BZ    UNXKEEP                                                          
         NI    NPAKSTAT,X'FF'-X'01'   CLEAR NO-SHOW DELETE                      
         NI    NPKRSTAT,X'FF'-X'80'   UNDELTE                                   
         L     R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
         C     R1,=F'20'                                                        
****     BH    UNXKEEP                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'PKG00',AREC,C'DUMP',50,=C'1D'                 
         B     UNXKEEP                                                          
                                                                                
*                                                                               
* END-OF-FILE PROCESSING                                                        
*                                                                               
UNXEOF   DS    0H                                                               
**       MVC   P(12),=C'RECS CHANGED'                                           
**       EDIT  (B4,COUNT),(8,P+13),ALIGN=LEFT                                   
**       GOTO1 VPRINTER                                                         
         OI    FLAG,X'03'          CLIENT CHANGE/EOF                            
         BAS   RE,FLUSHIT                                                       
         B     UNXIT                                                            
         EJECT                                                                  
* GETEL MACRO                                                                   
*                                                                               
         GETEL R3,27,ELCODE                                                     
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
PKGCNT   DC    F'0'                                                             
UNTCNT   DC    F'0'                                                             
COUNT    DC    F'0'                                                             
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
RELO     DS    A                                                                
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         SPACE 1                                                                
VDATCON  DS    A                                                                
VGETDAY  DS    A                                                                
VPRNTBL  DS    A                                                                
VHEXOUT  DS    A                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
STDATE   DS    XL2                                                              
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
CLTSV    DS    XL3'0'                                                           
NETSV    DS    CL4                                                              
CLTGRS   DS    PL8'0'                                                           
CLTNET   DS    PL8'0'                                                           
CLTTOT   DS    PL8'0'                                                           
ALLTOT   DS    PL8'0'                                                           
PAKWORK  DS    PL8'0'                                                           
BYTE     DS    CL1                                                              
FLAG     DS    CL1                                                              
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE NEGENPACK                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPUA                                                       
       ++INCLUDE NEGENUBILL                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'141UNLDEX    05/01/02'                                      
         END                                                                    
