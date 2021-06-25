*          DATA SET SPREPFXNE  AT LEVEL 027 AS OF 08/29/97                      
*PHASE SPFX02N                                                                  
         TITLE 'SPFX02 - FIX MISSED 0B ELEMS FROM POL CONV PROGRAM'             
*---------------------------------------------------------------*               
* FIX BUY RECORDS CONVERTED FROM NON-POL TO POL FOR DNCH        *               
* THE MISSED 0B ELEMENTS DO NOT HAVE THE X'40' BITS ON          *               
* AND THE AFFIDAVIT ELEMENTS (X'10') ARE UNHUNG                 *               
*---------------------------------------------------------------*               
         SPACE 1                                                                
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
FX02     CLI   MODE,CLTFRST                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
                                                                                
* CLTFRST                                                                       
FX10     DS    0H                                                               
         XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVC   KEY(3),1(R6)       MOVE A-M/CLT                                  
         MVI   KEY+3,X'FF'                                                      
         GOTO1 HIGH                                                             
         B     FX30                                                             
*                                                                               
FX25     GOTO1 SEQ                                                              
*                                                                               
FX30     CLC   KEY(3),KEYSAVE      TEST SAME A/M/CLT                            
         BNE   FX100                                                            
         TM    KEY+13,X'80'        TEST DELETED                                 
         BO    FX25                                                             
*                                                                               
         MVI   DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 GETBUY                                                           
         MVI   DMINBTS,0                                                        
*                                                                               
         L     R6,ADBUY            POINT TO RECORD                              
         TM    15(R6),X'80'        TEST DELETED                                 
         BO    FX25                                                             
         CLC   KEY+4(2),4(R6)      TEST SPILL                                   
         BNE   FX25                YES- IGNORE                                  
*                                                                               
         BAS   RE,CHKOTO                                                        
*                                                                               
         MVI   FIXED,C'N'                                                       
         LA    R6,24(R6)                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
FX32     BAS   RE,NEXTEL                                                        
         BNE   FX50                                                             
*                                                                               
FX34     LR    R7,R6                                                            
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),X'0C'         NEXT ELEM AN OTO                             
         BNE   FX32                NO                                           
* NEXT IS AN OTO - TEST IF IT IS MINUS                                          
         TM    6(R7),X'80'         TEST OTO IS MINUS                            
         BZ    FX32                NO - SKIP                                    
         TM    6(R6),X'40'         TEST PREV ELEM MISSED                        
         BO    FX32                                                             
*                                                                               
         CLI   FIXED,C'Y'          TEST FIRST FIX THIS BUY                      
         BE    FX38                NO                                           
         BAS   RE,PRTBUY                                                        
         BAS   RE,PRTELS                                                        
*                                                                               
FX38     MVI   FIXED,C'Y'                                                       
         OI    6(R6),X'40'                                                      
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),X'10'                                                      
         BL    FX32                                                             
         CLI   0(R7),X'1F'                                                      
         BH    FX32                                                             
         GOTO1 RECUP,DMCB,ADBUY,(R7)                                            
         B     FX32                                                             
*                                                                               
FX50     CLI   FIXED,C'Y'          TEST RECORD CHANGED                          
         BNE   FX25                                                             
*                                                                               
         BAS   RE,PRTELS                                                        
*                                                                               
FX42     GOTO1 PUTBUY                                                           
         B     FX25                                                             
*                                                                               
FX100    MVI   MODE,CLTLAST                                                     
         B     EXIT                                                             
         EJECT                                                                  
*==============================================================*                
* CHECK FOR CONSECUTIVE -OTO'S IN RECORD                       *                
*==============================================================*                
         SPACE 1                                                                
CHKOTO   NTR1                                                                   
         MVI   FIXED,C'N'                                                       
*                                                                               
CHK0     MVI   ELCDLO,X'0C'                                                     
         MVI   ELCDHI,X'0C'                                                     
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
*                                                                               
CHK2     BAS   RE,NEXTEL                                                        
         BNE   CHK20                                                            
*                                                                               
         TM    6(R6),X'80'         TEST MINUS                                   
         BZ    CHK2                NO                                           
         SR    R0,R0               NOW LOOK AT NEXT ELEMENT                     
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'0C'         TEST IT IS ALSO AN OTO                       
         BNE   CHK2                NO - IGNORE                                  
         TM    6(R6),X'80'         YES - TEST MINUS                             
         BZ    CHK2                NO - IGNORE                                  
*                                                                               
         MVC   ELEM(14),0(R6)      SAVE -OTO                                    
         CLI   FIXED,C'Y'                                                       
         BE    CHK4                                                             
         BAS   RE,PRTBUY                                                        
         BAS   RE,PRTELS                                                        
         MVI   FIXED,C'Y'                                                       
*                                                                               
CHK4     DS    0H                  DELETE -OTO FROM RECORD                      
         GOTO1 RECUP,DMCB,ADBUY,(R6),0                                          
* FIND 0B/0C ELEMENT THIS DATE THAT ISN'T MISSED YET                            
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
*                                                                               
CHK10    BAS   RE,NEXTEL                                                        
         BE    CHK12                                                            
         OC    ELEM+4(2),ELEM+4    TEST SPOT PAID                               
         BE    CHK0                NO - JUST FORGET ABOUT IT                    
         MVC   P+1(23),=C'** PAID OTO MISMATCH **'                              
         GOTO1 REPORT                                                           
         B     EXIT                                                             
CHK12    TM    6(R6),X'80'         TEST MINUS                                   
         BO    CHK10                                                            
         CLC   2(2,R6),ELEM+2      SAME ELEM DATES                              
         BNE   CHK10                                                            
         OC    ELEM+4(2),ELEM+4    IS THE OTO PAID                              
         BZ    CHK12X                                                           
         CLC   4(2,R6),ELEM+4      MATCH PAID DATES AS WELL                     
         BNE   CHK10                                                            
*                                                                               
CHK12X   TM    6(R6),X'40'         TEST MISSED                                  
         BO    CHK10                                                            
         OI    6(R6),X'40'         SET MISSED FLAG                              
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)            PUT THE -OTO AFTER IT                        
         AR    R6,R0                                                            
CHK14    CLI   0(R6),X'10'         TEST TO REMOVE ODD ELEMENTS                  
         BL    CHK16                                                            
         CLI   0(R6),X'1F'                                                      
         BH    CHK16                                                            
         GOTO1 RECUP,DMCB,ADBUY,(R6)                                            
         B     CHK14                                                            
*                                                                               
CHK16    GOTO1 RECUP,DMCB,ADBUY,ELEM,(R6)                                       
         B     CHK0                AND DO IT ALL AGAIN                          
* ONE MO' TIME TO FIND ANY UNHUNG AFFIDS IN THE RECORD                          
CHK20    MVI   ELCDLO,X'0C'                                                     
         MVI   ELCDHI,X'0C'                                                     
         L     R6,ADBUY                                                         
         SR    R7,R7                                                            
         LA    R6,24(R6)                                                        
CHK22    BAS   RE,NEXTEL                                                        
         BNE   CHK24                                                            
         LR    R7,R6               SAVE LAST OTO ADDRESS                        
         B     CHK22                                                            
*                                                                               
CHK24    LTR   R6,R7               POINT BACK TO LAST OTO                       
         BZ    CHKX                                                             
         TM    6(R6),X'80'         TEST MINUS OTO                               
         BZ    CHKX                                                             
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
CHK26    CLI   0(R6),X'10'                                                      
         BL    CHKX                                                             
         CLI   0(R6),X'1F'                                                      
         BH    CHKX                                                             
         CLI   FIXED,C'Y'                                                       
         BE    CHK28                                                            
         BAS   RE,PRTBUY                                                        
         BAS   RE,PRTELS                                                        
         MVI   FIXED,C'Y'                                                       
*                                                                               
CHK28    GOTO1 RECUP,DMCB,ADBUY,(R6)                                            
         B     CHK26                                                            
*                                                                               
CHKX     CLI   FIXED,C'Y'          TEST ANYTHING HAPPENED                       
         BNE   EXIT                                                             
         GOTO1 PUTBUY                                                           
         BAS   RE,PRTELS                                                        
         B     EXIT                                                             
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTBUY   NTR1                                                                   
         CLC   LASTKEY(13),KEY     TEST SAME AS BEFORE                          
         BE    EXIT                                                             
         MVC   LASTKEY(13),KEY                                                  
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
PRTB2    LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   PAGY(2),BUYALPHA                                                 
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(1),WORK+4                                                 
         CLI   WORK+4,C' '                                                      
         BH    *+8                                                              
         MVI   PSTA+5,C'T'                                                      
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,KEY+11           GET LINE NUMBER                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
         CLC   BDCHG(3),CNVDATE                                                 
         BL    PRTB4                                                            
         GOTO1 DATCON,DMCB,(3,BDCHG),(11,PCML)                                  
*                                                                               
PRTB4    GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
LASTKEY  DS    XL13                                                             
CNVDATE  DC    AL1(97,08,22)                                                    
         EJECT                                                                  
         L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'BUYREC',(R6),C'DUMP',(R0),=C'1D00'                
         B     EXIT                                                             
         EJECT                                                                  
*===============================================================*               
* SUBROUTINE PRINTS BUY ELEMENTS                                *               
*===============================================================*               
         SPACE 1                                                                
PRTELS   NTR1                                                                   
         CLI   QOPT1,C'Y'          SUPPRESS ELEMENTS                            
         BE    PRTELX                                                           
*                                                                               
PRTEL0   MVC   P(5),=C'AFTER'                                                   
         CLI   FIXED,C'Y'                                                       
         BE    *+10                                                             
         MVC   P(6),=C'BEFORE'                                                  
* PRINT KEY AND BDELEM                                                          
         L     R6,ADBUY                                                         
         GOTO1 HEXOUT,DMCB,(R6),P+8,24,=C'TOG'                                  
         LA    R6,24(R6)                                                        
         GOTO1 (RF),(R1),(R6),P2+8,40                                           
         GOTO1 (RF),(R1),40(R6),P3+8,30                                         
         GOTO1 REPORT                                                           
*                                                                               
         SR    R0,R0                                                            
PRTEL2   IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    PRTELX                                                           
         CLI   0(R6),X'0B'         FIND FIRST OB ELEM                           
         BNE   PRTEL2                                                           
*                                                                               
PRTEL4   LH    R4,MAXCHARS         SET MAX CHARS TO PRINT                       
         LA    R5,P+8              SET FIRST PRINT POSITION                     
*                                                                               
PRTEL6   IC    R0,1(R6)            GET ELEMENT LENGTH                           
         SR    R4,R0                                                            
         BP    PRTEL10                                                          
         AR    R4,R0               RESTORE R4 FOR NEXT TEST                     
         CH    R4,MAXCHARS         TEST ELEM WON'T EVER FIT                     
         BNE   PRTEL8              IT MIGHT, BUT THERE'S STUFF HERE             
         LH    R0,MAXCHARS         ELSE JUST PRINT MAXCHARS                     
         B     PRTEL10                                                          
*                                                                               
PRTEL8   GOTO1 REPORT                                                           
         B     PRTEL4                                                           
*                                                                               
PRTEL10  GOTO1 HEXOUT,DMCB,(R6),(R5),(R0),=C'TOG'                               
         A     R5,DMCB+16          ADD OUTPUT LENGTH                            
         LA    R5,1(R5)                                                         
*                                                                               
PRTEL12  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'66'                                                      
         BE    PRTEL12             SKIP COMMENTS                                
         CLI   0(R6),0                                                          
         BNE   PRTEL6                                                           
*                                                                               
         CH    R4,MAXCHARS         TEST ANYTHING MORE TO PRINT                  
         BE    PRTELX                                                           
         GOTO1 REPORT                                                           
*                                                                               
PRTELX   XIT1                                                                   
PRTCOUNT DC    PL4'0'                                                           
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
         DS    0D                                                               
ELEM     DS    XL256                                                            
ELADDR   DS    A                                                                
COUNT    DS    F                                                                
SPOTCNT  DS    F                                                                
MAXCHARS DC    H'50'               MAX INPUT CHARS PER LINE                     
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
SPTLEN   DS    X                                                                
PROD     DS    X                                                                
FIXED    DS    C                                                                
*                                                                               
* TABLE OF RECORD COUNT BUCKETS                                                 
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL24                                                            
INRECS   DC    F'0',CL20'RECORDS IN'                                            
OUTRECS  DC    F'0',CL20'RECORDS OUT'                                           
BUYFIX   DC    F'0',CL20'BUYS FIXED'                                            
EBUYFIX  DC    F'0',CL20'EXPLODED BUYS FIXED'                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
*                                                                               
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL2                                                              
PCML     DS    CL4                                                              
         DS    CL1                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027SPREPFXNE 08/29/97'                                      
         END                                                                    
