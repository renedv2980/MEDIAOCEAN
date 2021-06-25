*          DATA SET ACLDXINT   AT LEVEL 039 AS OF 08/20/98                      
*PHASE ACLDXINT                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE ACRECTYP                                                               
         TITLE 'FIX INTERQAGENCY ADJUST YR 2000 BUG'                            
***********************************************************************         
*                                                                               
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
*                                                                               
***********************************************************************         
ACLDXCOP CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**COP**,R7,R8                                                  
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
         ST    R1,APARM                                                         
         MVC   PLIST,0(R1)                                                      
         B     DMXCTL                                                           
*                                                                               
DMXCTL   CLI   PLIST,X'00'                                                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'                                                      
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF                                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              INITIALIZE                                             *         
***********************************************************************         
*                                                                               
DMXINIT  DS    0H                                                               
         LA    R5,ACCUMTAB                                                      
         USING ACUMD,R5                                                         
         LA    R1,ACBUKS                                                        
         LA    R0,ACBKCNT                                                       
         ZAP   0(ACBUKLN,R1),=P'0'                                              
         LA    R1,ACBUKLN(R1)                                                   
         BCT   R0,*-10                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PROCESS RECORD                                         *         
*              NOTE: RETURNED FROM ACRECTYP   0(R1) RECTYPE           *         
*                                             1(R1) COMPANY CODE      *         
*                                             2(R1) DISP TO COMP CODE *         
***********************************************************************         
*                                                                               
         USING TRNRECD,R4                                                       
DMXREC   DS    0H                                                               
         L     R4,AREC             IDENTIFY RECORD TYPE                         
         GOTO1 RECTYP,DMCB,(C'D',TRNRECD)                                       
         CLI   1(R1),X'B3'         OACO                                         
         BNE   DMXKEEP                                                          
         MVC   COMPDSP,2(R1)       SAVE DISP TO COMPANY                         
         MVC   RECDTYP,0(R1)       AND RECORD TYPE                              
         CLI   RECDTYP,ACRTNBT                                                  
         BNE   DMXKEEP                                                          
DMXR02   L     R4,AREC                                                          
         USING TBARECD,R4                                                       
         CLI   TBAKBTYP,X'01'                                                   
         BNE   DMXKEEP                                                          
         CLC   TBAKBMOS,=X'9808'                                                
         BNE   DMXKEEP                                                          
         CLC   TBAKBREF,=C'GF47'                                                
         BNE   DMXKEEP                                                          
         BAS   RE,DMPGET                      PRINT IT                          
         B     DMXKEEP                                                          
*        LA    R5,ACCUMTAB                                                      
*        USING ACUMD,R5                                                         
*MXR02A  CLI   ACUMLDG,X'FF'       EOT                                          
*        BE    DMXKEEP                                                          
*        CLC   ACUMLDG,TRNKULA     WHAT THIS LEDGER?                            
*        BE    DMXR04                                                           
*        LA    R5,ACULEN(R5)                                                    
*        B     DMXR02A                                                          
*                                                                               
*&&UK                                                                           
DMXR04   CLI   RECDTYP,ACRTTRN                TRANSACTIONS                      
         BNE   DMXR20                                                           
         CLI   TRNRSTYP,X'3A'                 TYPE 58                           
         BNE   DMXKEEP                                                          
         CLC   TRNRSMOS,=X'9912'                                                
         BNH   DMXKEEP                                                          
         BAS   RE,DMPGET                      PRINT IT                          
*****    MVI   TRNRSMOS,X'97'                                                   
         MVC   TRNRSMOS,=X'9707'                                                
         LA    R2,TRNRFST                                                       
DMXR06   CLI   0(R2),0                                                          
         BE    DMXRXX                                                           
         USING TRNELD,R2                                                        
         CLI   0(R2),X'44'                                                      
         BNE   DMXR08                                                           
******   MVI   TRNMOS,C'7'                                                      
         MVC   TRNMOS,=C'77'                                                    
         LA    R1,ACUTDR                                                        
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R1,ACUTCR                                                        
         AP    0(ACBUKLN,R1),TRNAMNT                                            
         B     DMXR10                                                           
*                                                                               
         USING TRSELD,R2                                                        
DMXR08   CLI   0(R2),X'60'                                                      
         BNE   DMXR10                                                           
*******  MVI   TRSPMOS,X'97'                                                    
         MVC   TRSPMOS,=X'9707'                                                 
*                                                                               
DMXR10   SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     DMXR06                                                           
*                                                                               
DMXR20   CLI   RECDTYP,ACRTCAC                BUCKETS                           
         BNE   DMXKEEP                                                          
         MVI   HITSW,C'N'                                                       
         CLC   TRNKULA(2),=CL2'1C'                                              
         BNE   DMXR21                                                           
         CLC   TRNKCUNT(2),=CL2'11'                                             
         BE    DMXR21                                                           
         CLC   TRNKCUNT(2),=CL2'12'                                             
         BNE   DMXKEEP                                                          
*                                                                               
DMXR21   LA    R2,TRNRFST                                                       
*        CLI   0(R2),X'45'                                                      
*        BE    *+6                                                              
*        DC    H'0'                                                             
DMXR22   CLI   0(R2),0                                                          
         BE    DMXR32                                                           
         USING BUKELD,R2                                                        
         CLI   0(R2),X'45'                                                      
         BNE   DMXR30                                                           
         CLC   BUKMOS,=X'9912'                                                  
         BNH   DMXR30                                                           
         CLI   HITSW,C'Y'                                                       
         MVI   HITSW,C'Y'                                                       
         BE    *+8                                                              
         BAS   RE,DMPGET                                                        
         MVC   BUKELEM,BUKEL       SAVE ELEMENT                                 
         MVI   BUKEL,DELELQ        MARK FOR DELETION                            
         GOTO1 HELLO,ELIST,(C'D',=C'ACCMST'),('DELELQ',TRNRECD),0               
         LA    R2,BUKELEM                                                       
******   MVI   BUKMOS,X'97'                                                     
         MVC   BUKMOS,=X'9707'                                                  
         CLI   CACKBTYP-CACRECD(R4),X'40'                                       
         BNE   *+20                                                             
         AP    ACUBDR,BUKDR           REGULAR BUCKET                            
         AP    ACUBCR,BUKCR                                                     
         B     DMXR23                                                           
         CLI   CACKBTYP-CACRECD(R4),C'G'                                        
         BNE   *+20                                                             
         AP    ACUGBDR,BUKDR          GROSS BUCKET                              
         AP    ACUGBCR,BUKCR                                                    
         B     DMXR23                                                           
         AP    ACUUBDR,BUKDR           UNKNOWN BUCKET                           
         AP    ACUUBCR,BUKCR                                                    
DMXR23   DS    0H                                                               
         GOTO1 HELLO,ELIST,(C'G',=C'ACCMST'),('BUKELQ',TRNRECD),       +        
               (L'BUKMOS,BUKMOS)                                                
         CLI   ELERR,0                                                          
         BNE   DMXR24              NO MATCHING EL                               
         L     R1,ELADDR                                                        
         AP    BUKDR-BUKELD(L'BUKDR,R1),BUKDR                                   
         AP    BUKCR-BUKELD(L'BUKCR,R1),BUKCR                                   
         B     DMXR21                                                           
*                                                                               
DMXR24   DS    0H                                                               
         GOTO1 HELLO,ELIST,(C'P',=C'ACCMST'),TRNRECD,BUKELEM,0                  
         CLI   ELERR,0                                                          
         BE    DMXR21              MUST FIT BECAUSE I JUST DELETED 1            
         DC    H'0'                CAN'T ADD THE ELEMENT                        
*                                                                               
DMXR30   SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     DMXR22                                                           
*                                                                               
DMXR32   CLI   HITSW,C'Y'                                                       
         BNE   DMXKEEP                                                          
*&&                                                                             
*                                                                               
DMXRXX   BAS   RE,DMPPUT                                                        
         B     DMXKEEP                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*              PRINT TOTALS                                           *         
***********************************************************************         
DMXEOF   DS    0H                                                               
         ZAP   LINE,=P'99' '       PRINT RECORD COUNTS                          
         GOTO1 VPRINTER                                                         
         LA    R5,ACCUMTAB                                                      
DMXEOF2  CLI   0(R5),X'FF'                                                      
         BE    DMXEOFX                                                          
         MVC   P(L'ACUMLDG),ACUMLDG                                             
         MVC   P+3(12),=C'TRANS DEBITS'                                         
         EDIT  ACUTDR,(14,P+14),2,MINUS=YES                                     
         GOTO1 VPRINTER                                                         
         MVC   P(L'ACUMLDG),ACUMLDG                                             
         MVC   P+3(12),=C'TRANS CREDIT'                                         
         EDIT  ACUTCR,(14,P+14),2,MINUS=YES                                     
         GOTO1 VPRINTER                                                         
         MVC   P(L'ACUMLDG),ACUMLDG                                             
         MVC   P+3(12),=C'BUCKS DEBITS'                                         
         EDIT  ACUBDR,(14,P+14),2,MINUS=YES                                     
         GOTO1 VPRINTER                                                         
         MVC   P(L'ACUMLDG),ACUMLDG                                             
         MVC   P+3(12),=C'BUCKS CREDIT'                                         
         EDIT  ACUBCR,(14,P+14),2,MINUS=YES                                     
         GOTO1 VPRINTER                                                         
         MVC   P(L'ACUMLDG),ACUMLDG                                             
         MVC   P+3(12),=C'B (G) DEBITS'                                         
         EDIT  ACUGBDR,(14,P+14),2,MINUS=YES                                    
         GOTO1 VPRINTER                                                         
         MVC   P(L'ACUMLDG),ACUMLDG                                             
         MVC   P+3(12),=C'B (G) CREDIT'                                         
         EDIT  ACUGBCR,(14,P+14),2,MINUS=YES                                    
         GOTO1 VPRINTER                                                         
         MVC   P(L'ACUMLDG),ACUMLDG                                             
         MVC   P+3(12),=C'B (U) DEBITS'                                         
         EDIT  ACUUBDR,(14,P+14),2,MINUS=YES                                    
         GOTO1 VPRINTER                                                         
         MVC   P(L'ACUMLDG),ACUMLDG                                             
         MVC   P+3(12),=C'B (U) CREDIT'                                         
         EDIT  ACUUBCR,(14,P+14),2,MINUS=YES                                    
         GOTO1 VPRINTER                                                         
         LA    R5,ACULEN(R5)                                                    
         B     DMXEOF2                                                          
DMXEOFX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              DITTO OUTPUT RECORDS                                   *         
***********************************************************************         
*                                                                               
DMPGET   NTR1  ,                                                                
         L     R9,AREC                                                          
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    DUMPX                                                            
         LA    R7,=C'GET'                                                       
         MVC   HALF,ACCRLEN-ACCRECD(R9)                                         
         B     DUMP                                                             
*                                                                               
DMPPUT   NTR1  ,                                                                
         L     R9,AREC                                                          
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    DUMPX                                                            
         LA    R7,=C'PUT'                                                       
         MVC   HALF,ACCRLEN-ACCRECD(R9)                                         
*                                                                               
DUMP     LH    R8,HALF                                                          
         GOTO1 PRNTBL,DMCB,(3,(R7)),AREC,C'DUMP',(R8),=C'2D'                    
                                                                                
DUMPX    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
*        EXIT CONDITIONS                                              *         
***********************************************************************         
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     EXIT                                                             
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     EXIT                                                             
*                                                                               
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     EXIT                                                             
*                                                                               
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     EXIT                                                             
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        ACCUM TABLE                                                            
***********************************************************************         
*                                                                               
ACCUMTAB DC     CL2'1C',(ACBKCNT)PL(ACBUKLN)'0'                                 
         DC     CL2'11',(ACBKCNT)PL(ACBUKLN)'0'                                 
         DC     CL2'12',(ACBKCNT)PL(ACBUKLN)'0'                                 
         DC     CL2'SR',(ACBKCNT)PL(ACBUKLN)'0'                                 
         DC     CL2'SI',(ACBKCNT)PL(ACBUKLN)'0'                                 
         DC     X'FF'                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
***********************************************************************         
*              EQUATES                                                *         
***********************************************************************         
*                                                                               
SPACE    EQU   X'40'                                                            
DELELQ   EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              WORK AREA                                              *         
***********************************************************************         
*                                                                               
PRNTBL   DC    V(PRNTBL)                                                        
PRINT    DC    V(PRINT)                                                         
HELLO    DC    V(HELLO)                                                         
RECTYP   DC    V(ACRECTYP)                                                      
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                   VPRINTER                                     
VCPRINT  DS    A                   VCPRINT                                      
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
         DS    CL8                                                              
WORK     DS    CL20                                                             
         DS    CL8                                                              
BYTE     DS    C                                                                
HALF     DS    H                                                                
HITSW    DS    X                                                                
BUKELEM  DS    XL(BUKLNQ)                                                       
RECDTYP  DS    XL1                 RECORD TYPE (SEE EQUATES)                    
RECCOMP  DS    XL1                 COMPANY CODE                                 
COMPDSP  DS    XL1                 DISPLACEMENT TO COMPANY CODE                 
ELIST    DS    3A                  HELLO PARAMETER LIST                         
ELERR    DS    0XL1                HELLO ERROR RETURN BYTE                      
ELADDR   DS    A                   HELLO ELEMENT ADDRESS (GET)                  
ELADDR2  DS    A                   HELLO ELEMENT ADDRESS (ADD)                  
ELADDR3  DS    A                                                                
ELADDR4  DS    A                                                                
*                                                                               
*                                                                               
DATADISP DC    H'56'                                                            
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'1'                                                           
MAXDUMP  DC    PL4'5000'                                                        
ELM1     DS    CL255                                                            
         EJECT                                                                  
*              DSECT FOR ACCUMS                                                 
ACUMD    DSECT                                                                  
ACUMLDG  DS    CL2                 LEDGER                                       
ACUMKLEN EQU   *-ACUMD             KEY LENGTH                                   
ACBUKS   DS    PL8                                                              
ACBUKLN  EQU   *-ACBUKS                                                         
         ORG   ACBUKS                                                           
ACUTDR   DS    PL(ACBUKLN)         TRANS DR                                     
ACUTCR   DS    PL(ACBUKLN)         TRANS CR                                     
ACUBDR   DS    PL(ACBUKLN)         BUCK  DR                                     
ACUBCR   DS    PL(ACBUKLN)         BUCK  CR                                     
ACUGBDR  DS    PL(ACBUKLN)         GROSS BUCK  DR                               
ACUGBCR  DS    PL(ACBUKLN)         GROSS BUCK  CR                               
ACUUBDR  DS    PL(ACBUKLN)         UNKNOWN BUCK  DR                             
ACUUBCR  DS    PL(ACBUKLN)         UNKNOWN BUCK  CR                             
ACBKCNT  EQU   (*-ACBUKS)/ACBUKLN  NUMBER OF BUCKETS                            
ACULEN   EQU   *-ACUMD             RECORD LENGTH                                
         EJECT                                                                  
***********************************************************************         
*        OTHER INCLUDES                                               *         
***********************************************************************         
*                                                                               
* DDDPRINT                                                                      
* ACGENFILE                                                                     
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039ACLDXINT  08/20/98'                                      
         END                                                                    
